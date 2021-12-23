use crate::leema::code::Code;
use crate::leema::failure::{Failure, Lresult};
use crate::leema::fiber::Fiber;
use crate::leema::frame::{Event, Frame, FrameTrace, Parent};
use crate::leema::msg::{AppMsg, IoMsg, MsgItem, WorkerMsg};
use crate::leema::reg::Reg;
use crate::leema::rsrc;
use crate::leema::stack;
use crate::leema::struple::{Struple2, StrupleItem};
use crate::leema::val::{Fref, MsgVal, Val};

use crate::leema::lib_core;

use std::cmp::min;
use std::collections::{HashMap, LinkedList};
use std::pin::Pin;
use std::rc::Rc;
use std::sync::{
    mpsc::{channel, Receiver, Sender},
    Arc,
};
use std::thread;
use std::time::Duration;

use futures::{Async, Poll};


const DEFAULT_STACK_SIZE: usize = 50;

#[derive(Debug)]
enum ReadyFiber
{
    New(Fiber),
    Ready(Fiber, Rc<Code>),
}

#[derive(Debug)]
enum FiberWait
{
    Code(Fiber),
    Io(Fiber),
    Future(Fiber, Rc<Code>),
}


pub struct RustFuncContext<'a>
{
    worker: &'a Worker,
    task: &'a mut Fiber,
}

impl<'a> RustFuncContext<'a>
{
    pub fn new(w: &'a Worker, t: &'a mut Fiber) -> RustFuncContext<'a>
    {
        RustFuncContext { worker: w, task: t }
    }

    pub fn current_fref(&self) -> &Fref
    {
        &self.task.head.function
    }

    pub fn pc(&self) -> i32
    {
        self.task.head.pc
    }

    pub fn get_param(&self, i: i8) -> Lresult<&Val>
    {
        self.task.head.e.get_param(i)
    }

    pub fn get_reg(&self, r: Reg) -> Lresult<&Val>
    {
        Ok(ltry!(self.task.head.e.get_reg(r)))
    }

    pub fn fail_here(&self) -> Arc<FrameTrace>
    {
        self.task.head.trace.fail_here()
    }

    pub fn set_result(&mut self, r: Val)
    {
        self.task.head.parent.set_result(r);
    }

    pub fn new_call(
        &mut self,
        return_pc: i32,
        f: Fref,
        args: Struple2<Val>,
    ) -> Lresult<Event>
    {
        let start_pc = self.task.head.pc;
        self.task.head.pc = return_pc;
        Ok(Event::Call(Reg::stack(0), start_pc as i16, f, args))
    }

    pub fn new_task(&self, f: Fref, args: Struple2<Val>)
    {
        let (send, _) = channel();
        let spawn_msg = AppMsg::Spawn(send, f, args);
        self.worker
            .app_tx
            .send(spawn_msg)
            .expect("failed sending new_task msg");
    }

    pub fn new_fork(&self, f: Fref, args: Struple2<Val>) -> Receiver<Val>
    {
        let (send, recv) = channel();
        let spawn_msg = AppMsg::Spawn(send, f, args);
        self.worker
            .app_tx
            .send(spawn_msg)
            .expect("failed sending new_fork msg");
        recv
    }
}


pub struct Worker
{
    fresh: LinkedList<ReadyFiber>,
    waiting: HashMap<i64, FiberWait>,
    code: HashMap<Fref, Rc<Code>>,
    app_tx: Sender<AppMsg>,
    io_tx: Sender<IoMsg>,
    msg_rx: Receiver<WorkerMsg>,
    id: i64,
    next_fiber_id: i64,
    did_nothing: i32,
    done: bool,
}

/**
 * main_loop
 *   get fresh/active frame
 *     iterate until !active
 *   push frame
 *   rotate
 */
impl Worker
{
    pub fn init(
        wid: i64,
        send: Sender<AppMsg>,
        io: Sender<IoMsg>,
        recv: Receiver<WorkerMsg>,
    ) -> Worker
    {
        Worker {
            fresh: LinkedList::new(),
            waiting: HashMap::new(),
            code: HashMap::new(),
            app_tx: send,
            io_tx: io,
            msg_rx: recv,
            id: wid,
            next_fiber_id: 0,
            did_nothing: 0,
            done: false,
        }
    }

    pub fn run(mut self)
    {
        let mut did_nothing = 0;
        while !self.done {
            let did_something = self.run_once().unwrap();
            if did_something {
                did_nothing = 0;
            } else {
                did_nothing = min(did_nothing + 1, 100_000);
            }
            if did_nothing > 1000 {
                thread::sleep(Duration::from_micros(did_nothing));
            }
        }
    }

    pub fn run_once(&mut self) -> Lresult<bool>
    {
        let mut did_something = false;
        while let Result::Ok(msg) = self.msg_rx.try_recv() {
            did_something = true;
            self.process_msg(msg)?;
        }

        match self.pop_fresh() {
            Some(ReadyFiber::New(f)) => {
                did_something = true;
                self.load_code(f)?;
            }
            Some(ReadyFiber::Ready(mut f, code)) => {
                did_something = true;
                let ev = self.execute_frame(&mut f, &*code);
                self.handle_event(f, ev, code)?;
            }
            None => {}
        }
        Ok(did_something)
    }

    fn find_code<'a>(&'a self, fref: &Fref) -> Option<Rc<Code>>
    {
        self.code
            .get(fref)
            .map(|func: &'a Rc<Code>| (*func).clone())
    }

    fn load_code(&mut self, curf: Fiber) -> Lresult<()>
    {
        let opt_code = self.find_code(&curf.head.function);
        if let Some(func) = opt_code {
            self.push_coded_fiber(curf, func)
        } else {
            let args = Val::Tuple(vec![StrupleItem::new_v(Val::Call(
                curf.head.function.clone(),
                vec![],
            ))]);
            let msg = IoMsg::Iop {
                worker_id: self.id,
                fiber_id: curf.fiber_id,
                action: lib_core::load_code,
                rsrc_id: Some(rsrc::ID_PROGLIB),
                params: MsgItem::new(&args),
            };
            self.io_tx
                .send(msg)
                .expect("failure sending load code message to app");
            let fiber_id = curf.fiber_id;
            let fw = FiberWait::Code(curf);
            self.waiting.insert(fiber_id, fw);
            Ok(())
        }
    }

    pub fn execute_frame(&self, f: &mut Fiber, code: &Code) -> Lresult<Event>
    {
        match code {
            &Code::Leema(ref ops) => f.execute_leema_frame(ops),
            &Code::Rust(ref rf) => {
                vout!("execute rust code\n");
                rf(f)
            }
            &Code::Rust2(ref rf) => {
                vout!("execute rust code2\n");
                let ctx = RustFuncContext::new(self, f);
                rf(ctx)
            }
            &Code::Iop(_, _) => {
                Err(rustfail!(
                    "leema_failure",
                    "cannot execute iop in a worker\n",
                ))
            }
        }
    }

    pub fn handle_event(
        &mut self,
        mut fbr: Fiber,
        evr: Lresult<Event>,
        code: Rc<Code>,
    ) -> Poll<Val, Failure>
    {
        let e = match evr {
            Ok(ev) => ev,
            Err(failure) => {
                fbr.head.parent.set_result(Val::Failure2(Box::new(failure)));
                self.return_from_call(fbr);
                return Ok(Async::NotReady);
            }
        };
        match e {
            Event::Success => {
                vout!("function call success\n");
                self.return_from_call(fbr);
                Ok(Async::NotReady)
            }
            Event::Call(dst, line, func, args) => {
                vout!("push_call({} {}, {:?})\n", line, func, args);
                fbr.push_call(code.clone(), dst, line, func, args);
                self.load_code(fbr)?;
                Result::Ok(Async::NotReady)
            }
            Event::TailCall(func, args) => {
                vout!("push_tailcall({}, {:?})\n", func, args);
                fbr.push_tailcall(func, args);
                self.load_code(fbr)?;
                Result::Ok(Async::NotReady)
            }
            Event::NewTask(fref, callargs) => {
                let (sender, _receiver) = channel();
                let msg = AppMsg::Spawn(sender, fref, callargs);
                self.app_tx.send(msg).expect("new task msg send failure");
                let (new_child, new_parent) = fbr.new_task_key();
                let new_task_key = Val::Tuple(vec![
                    StrupleItem::new(None, Val::Int(new_child)),
                    StrupleItem::new(None, Val::Int(new_parent)),
                ]);
                fbr.head.parent.set_result(new_task_key);
                self.return_from_call(fbr);
                Result::Ok(Async::NotReady)
            }
            Event::FutureWait(reg) => {
                println!("wait for future {:?}", reg);
                Result::Ok(Async::NotReady)
            }
            Event::Iop((rsrc_worker_id, rsrc_id), _iopf, _iopargs) => {
                if self.id == rsrc_worker_id {
                    println!(
                        "Run Iop on worker with resource: {}/{}",
                        rsrc_worker_id, rsrc_id
                    );
                    /*
                    let opt_ioq = self.resource.get_mut(&rsrc_id);
                    if opt_ioq.is_none() {
                        panic!("Iop resource not found: {}", rsrc_id);
                    }
                    let mut ioq = opt_ioq.unwrap();
                    match ioq.checkout(self.id, iopf, iopargs) {
                        // Some((iopf2, rsrc, iopargs2)) => {
                        Some((rsrc, iop2)) => {
                            match (iop2.action)(resp, rsrc, iop2.params) {
                                Event::IoFuture(fut) => {
                                    self.handle.spawn(fut);
                                }
                                _ => {
                                    panic!("not a future");
                                }
                            }
                        }
                        None => {
                            // resource is busy, will push it later
                        }
                    }
                    */
                    Ok(Async::NotReady)
                } else {
                    Err(rustfail!(
                        "leema_failure",
                        "cannot send iop from worker({}) to worker({})",
                        self.id,
                        rsrc_worker_id,
                    ))
                }
            }
            Event::Uneventful => {
                vout!("shouldn't be here with uneventful");
                Err(rustfail!(
                    "leema_failure",
                    "code: {:?}, pc: {:?}",
                    code,
                    fbr.head.pc,
                ))
            }
        }
    }

    pub fn return_from_call(&mut self, mut fbr: Fiber)
    {
        let parent = fbr.head.take_parent();
        match parent {
            Parent::Caller(old_code, mut pf, dst) => {
                pf.pc += 1;
                fbr.head = *pf;
                vout!("return to caller: {}()\n", fbr.head.function);
                vout!(" result: {}\n", dst);
                self.push_fresh(ReadyFiber::Ready(fbr, old_code));
            }
            Parent::Repl(_) => {}
            Parent::Main(res) => {
                vout!("finished main func\n");
                let msg = AppMsg::MainResult(MsgVal::new(&res));
                self.done = true;
                self.app_tx.send(msg).expect("app message send failure");
            }
            Parent::Fork(_) => {
                // this should have already happened
            }
            Parent::Task => {} // nothing to do
            Parent::Null => {
                // this shouldn't have happened
                vout!("Parent::Null but how?\n");
            }
        }
    }

    pub fn process_msg(&mut self, msg: WorkerMsg) -> Lresult<()>
    {
        match msg {
            WorkerMsg::Spawn(result_dst, func, args) => {
                vout!("worker spawn2 {}\n", func);
                let parent = Parent::new_fork(result_dst);
                let (stack, e) =
                    stack::Buffer::new(DEFAULT_STACK_SIZE, func.clone(), args);
                let root = Frame::new_root(e, parent, func);
                self.spawn_fiber(stack, root);
            }
            WorkerMsg::FoundCode(fiber_id, fref, code) => {
                let newf = fref.take();
                let rc_code = Rc::new(code);
                self.code.insert(newf, rc_code.clone());
                let opt_fiber = self.waiting.remove(&fiber_id);
                if let Some(FiberWait::Code(fib)) = opt_fiber {
                    self.push_coded_fiber(fib, rc_code)?;
                } else {
                    return Err(rustfail!(
                        "leema_failure",
                        "Cannot find waiting fiber: {}",
                        fiber_id,
                    ));
                }
            }
            WorkerMsg::IopResult(fiber_id, result_msg) => {
                vout!("iop_result({}, {:?})\n", fiber_id, result_msg);
                let result_val = result_msg.take();
                let wait = self.waiting.remove(&fiber_id).unwrap();
                let mut fib = match wait {
                    FiberWait::Io(f) => f,
                    FiberWait::Code(f) => f,
                    FiberWait::Future(_, _) => {
                        panic!("cannot handle a future now");
                    }
                };
                fib.head.parent.set_result(result_val);
                self.return_from_call(fib);
            }
            WorkerMsg::Done => {
                self.done = true;
            }
        }
        Ok(())
    }

    fn push_coded_fiber(&mut self, fib: Fiber, code: Rc<Code>) -> Lresult<()>
    {
        if let Some((iopf, rsrc_idx)) = code.get_iop() {
            let fiber_id = fib.fiber_id;
            let rsrc_id = match rsrc_idx {
                Some(idx) => {
                    if let &Val::ResourceRef(rid) = fib.head.e.get_param(idx)? {
                        Some(rid)
                    } else {
                        None
                    }
                }
                None => None,
            };

            let args = Val::Tuple(fib.head.e.get_params().to_vec());
            let msg_vals = MsgVal::new(&args);
            self.io_tx
                .send(IoMsg::Iop {
                    worker_id: self.id,
                    fiber_id,
                    rsrc_id,
                    action: iopf,
                    params: msg_vals,
                })
                .map_err(|e| rustfail!("io_failure", "{}", e))?;
            self.waiting.insert(fiber_id, FiberWait::Io(fib));
            Ok(())
        } else {
            self.push_fresh(ReadyFiber::Ready(fib, code));
            Ok(())
        }
    }

    pub fn spawn_fiber(&mut self, stack: Pin<Box<stack::Buffer>>, frame: Frame)
    {
        vout!("spawn_fiber({})\n", frame.function);
        let id = self.next_fiber_id;
        self.next_fiber_id += 1;
        let fib = Fiber::spawn(id, stack, frame);
        self.fresh.push_back(ReadyFiber::New(fib));
    }

    fn pop_fresh(&mut self) -> Option<ReadyFiber>
    {
        self.fresh.pop_front()
    }

    fn push_fresh(&mut self, f: ReadyFiber)
    {
        self.fresh.push_back(f)
    }
}
