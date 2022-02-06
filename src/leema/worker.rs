use crate::leema::code::Code;
use crate::leema::failure::Lresult;
use crate::leema::fiber::Fiber;
use crate::leema::frame::{Event, Frame, FrameTrace};
use crate::leema::msg::{self, IoMsg, MsgItem, WorkerMsg};
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
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::mpsc::{self, sync_channel, Receiver, SyncSender};
use std::sync::Arc;
use std::thread;
use std::time::Duration;

use futures::task::Poll;
use tokio::task;



const DEFAULT_STACK_SIZE: usize = 50;
static NEXT_FIBER_ID: AtomicI64 = AtomicI64::new(0);

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
        self.task.head.e.fref().unwrap()
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
        self.task.head.e.set_result(r);
    }

    pub fn new_call(
        &mut self,
        return_pc: i32,
        f: Fref,
        args: Struple2<Val>,
    ) -> Lresult<Event>
    {
        let start_pc = self.task.head.pc;
        // pc will be incremented by one upon return
        self.task.head.pc = return_pc - 1;
        // push result
        self.task.head.e.stack_push(Val::VOID);
        // push func
        self.task.head.e.stack_push(Val::Func(f));
        // push subject
        self.task.head.e.stack_push(Val::VOID);
        let argc = args.len() as i16;
        for a in args {
            // push the key too maybe?
            self.task.head.e.stack_push(a.v);
        }
        Ok(Event::PushCall {
            argc,
            line: start_pc as i16,
        })
    }

    pub fn new_task(&self, f: Fref, args: Struple2<Val>)
    {
        eprintln!("new_task {} {:?}", f, args);
        /*
        let (send, _) = channel(1);
        let spawn_msg = AppMsg::Spawn(send, f, args);
        self.worker
            .app_tx
            .blocking_send(spawn_msg)
            .expect("failed sending new_task msg");
            */
    }

    pub fn new_fork(&self, f: Fref, args: Struple2<Val>) -> Receiver<Val>
    {
        eprintln!("new_fork {} {:?}", f, args);
        let (_send, recv) = sync_channel(1);
        /*
        let spawn_msg = AppMsg::Spawn(send, f, args);
        self.worker
            .app_tx
            .blocking_send(spawn_msg)
            .expect("failed sending new_fork msg");
            */
        recv
    }
}


pub struct Worker
{
    fresh: LinkedList<ReadyFiber>,
    waiting: HashMap<i64, FiberWait>,
    code: HashMap<Fref, Rc<Code>>,
    io_tx: SyncSender<IoMsg>,
    msg_rx: Receiver<WorkerMsg>,
    spawn_rx: msg::SpawnReceiver,
    id: i64,
    next_fiber_id: i64,
    did_nothing: i32,
    done: bool,
}

pub struct WorkerSeed
{
    pub wid: i64,
    pub io_send: SyncSender<IoMsg>,
    pub worker_recv: Receiver<WorkerMsg>,
    pub spawn: msg::SpawnReceiver,
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
    pub fn init(seed: WorkerSeed) -> Worker
    {
        Worker {
            fresh: LinkedList::new(),
            waiting: HashMap::new(),
            code: HashMap::new(),
            io_tx: seed.io_send,
            msg_rx: seed.worker_recv,
            spawn_rx: seed.spawn,
            id: seed.wid,
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

        match self.spawn_rx.try_recv() {
            Ok(spawn) => ltry!(self.process_spawn(spawn)),
            Err(mpsc::TryRecvError::Empty) => {} // do nothing
            Err(mpsc::TryRecvError::Disconnected) => {
                eprintln!("spawn channel disconnected");
            }
        }

        match self.pop_fresh() {
            Some(ReadyFiber::New(f)) => {
                did_something = true;
                ltry!(self.load_code(f));
            }
            Some(ReadyFiber::Ready(mut f, code)) => {
                did_something = true;
                let _ev = self.execute_frame(&mut f, &*code);
                // self.handle_event(f, ev, code);
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
        let fref = ltry!(curf.head.function());
        let opt_code = self.find_code(fref);
        if let Some(func) = opt_code {
            self.push_coded_fiber(curf, func)
        } else {
            vout!("load code for {}\n", fref);
            let args = Val::Tuple(vec![
                StrupleItem::new_v(Val::ResourceRef(rsrc::ID_PROGLIB)),
                StrupleItem::new_v(Val::Func(fref.clone())),
            ]);
            let msg = IoMsg::Iop {
                worker_id: self.id,
                fiber_id: curf.fiber_id,
                action: lib_core::load_code,
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
    ) -> Poll<Val>
    {
        let e = match evr {
            Ok(ev) => ev,
            Err(failure) => {
                fbr.head.e.set_result(Val::Failure2(Box::new(failure)));
                self.return_from_call(fbr);
                return Poll::Pending;
            }
        };
        match e {
            Event::Success => {
                vout!("function call success\n");
                self.return_from_call(fbr);
                Poll::Pending
            }
            Event::PushCall { argc, line } => {
                vout!("push_call({} @{})\n", argc, line);
                fbr.head =
                    fbr.head.push_call(code.clone(), argc, line).unwrap();
                self.load_code(fbr).unwrap();
                Poll::Pending
            }
            Event::TailCall(func, args) => {
                vout!("push_tailcall({}, {:?})\n", func, args);
                fbr.push_tailcall(func, args);
                self.load_code(fbr).unwrap();
                Poll::Pending
            }
            Event::NewTask(_fref, _callargs) => Poll::Pending,
            /*
            Event::NewTask(fref, callargs) => {
                let (sender, _receiver) = channel(1);
                let msg = AppMsg::Spawn(sender, fref, callargs);
                self.app_tx
                    .blocking_send(msg)
                    .expect("new task msg send failure");
                let (new_child, new_parent) = fbr.new_task_key();
                let new_task_key = Val::Tuple(vec![
                    StrupleItem::new(None, Val::Int(new_child)),
                    StrupleItem::new(None, Val::Int(new_parent)),
                ]);
                fbr.head.e.set_result(new_task_key);
                self.return_from_call(fbr);
                Poll::Pending
            }
            */
            Event::FutureWait(reg) => {
                println!("wait for future {:?}", reg);
                Poll::Pending
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
                    Poll::Pending
                } else {
                    Poll::Ready(Val::Failure2(Box::new(rustfail!(
                        "leema_failure",
                        "cannot send iop from worker({}) to worker({})",
                        self.id,
                        rsrc_worker_id,
                    ))))
                }
            }
            Event::Uneventful => {
                vout!("shouldn't be here with uneventful");
                Poll::Ready(Val::Failure2(Box::new(rustfail!(
                    "leema_failure",
                    "code: {:?}, pc: {:?}",
                    code,
                    fbr.head.pc,
                ))))
            }
        }
    }

    pub fn return_from_call(&mut self, mut fbr: Fiber)
    {
        if let Some(caller_code) = fbr.head.pop_call() {
            vout!("return to caller: {}()\n", fbr.head.function().unwrap().f);
            self.push_fresh(ReadyFiber::Ready(fbr, caller_code));
            return;
        }

        // should write some metrics about being done

        // fiber is done, how to finish it?
        fbr.send_result();
    }

    pub fn process_msg(&mut self, msg: WorkerMsg) -> Lresult<()>
    {
        match msg {
            WorkerMsg::FoundCode(fiber_id, fref, code) => {
                let newf = fref.take();
                vout!("worker found code {}\n", newf);
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
                fib.head.e.stack_push(result_val);
                self.return_from_call(fib);
            }
            WorkerMsg::Done => {
                self.done = true;
            }
        }
        Ok(())
    }

    pub fn process_spawn(&mut self, msg: msg::SpawnMsg) -> Lresult<()>
    {
        match msg {
            msg::SpawnMsg::Spawn(result_dst, func, args) => {
                vout!("worker spawn {}\n", func);
                let (stack, e) =
                    stack::Buffer::new(DEFAULT_STACK_SIZE, func.clone(), args);
                let root = Frame::new_root(e);
                self.spawn_fiber(stack, root, Some(result_dst));
            }
        }
        Ok(())
    }

    fn push_coded_fiber(&mut self, fib: Fiber, code: Rc<Code>) -> Lresult<()>
    {
        // remove the rsrc_idx config from the iops
        if let Some((iopf, _rsrc_idx)) = code.get_iop() {
            let fiber_id = fib.fiber_id;
            let args = Val::Tuple(fib.head.e.get_params().to_vec());
            let msg_vals = MsgVal::new(&args);
            self.io_tx
                .send(IoMsg::Iop {
                    worker_id: self.id,
                    fiber_id,
                    action: iopf,
                    params: msg_vals,
                })
                .unwrap();
            self.waiting.insert(fiber_id, FiberWait::Io(fib));
            Ok(())
        } else {
            self.push_fresh(ReadyFiber::Ready(fib, code));
            Ok(())
        }
    }

    pub fn spawn_fiber(
        &mut self,
        stack: Pin<Box<stack::Buffer>>,
        frame: Frame,
        result: Option<SyncSender<Val>>,
    )
    {
        vout!("spawn_fiber({})\n", frame.function().unwrap().f);
        let id = self.next_fiber_id;
        self.next_fiber_id += 1;
        let fib = Fiber::spawn(id, stack, frame, result);
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

async fn run_fiber_loop(_f: Fiber)
{
    loop {
        task::yield_now().await;
    }
}

fn new_fiber(
    f: Fref,
    args: Struple2<Val>,
    result: Option<SyncSender<Val>>,
) -> Fiber
{
    vout!("spawn fiber {}\n", f);
    let (stack, e) = stack::Buffer::new(DEFAULT_STACK_SIZE, f, args);
    let root = Frame::new_root(e);

    let id = NEXT_FIBER_ID.fetch_add(1, Ordering::SeqCst);
    Fiber::spawn(id, stack, root, result)
}
