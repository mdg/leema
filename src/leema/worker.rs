use crate::leema::code::Code;
use crate::leema::fiber::Fiber;
use crate::leema::frame::{Event, Frame, Parent};
use crate::leema::lri::Lri;
use crate::leema::lstr::Lstr;
use crate::leema::msg::{AppMsg, IoMsg, MsgItem, WorkerMsg};
use crate::leema::reg::Reg;
use crate::leema::struple::Struple;
use crate::leema::val::{MsgVal, Val};

use std::cmp::min;
use std::collections::{HashMap, LinkedList};
use std::rc::Rc;
use std::sync::mpsc::{channel, Receiver, Sender};
use std::thread;
use std::time::Duration;

use futures::{Async, Poll};


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

    pub fn current_fri(&self) -> &Lri
    {
        &self.task.head.function
    }

    pub fn get_param(&self, i: i8) -> &Val
    {
        self.task.head.e.get_param(i)
    }

    pub fn get_reg(&self, r: &Reg) -> &Val
    {
        self.task.head.e.get_reg(r)
    }

    pub fn set_result(&mut self, r: Val)
    {
        self.task.head.parent.set_result(r);
    }

    pub fn new_task(&self, fri: Lri, args: Struple<Val>)
    {
        let (send, _) = channel();
        let spawn_msg = AppMsg::Spawn(send, fri, args);
        self.worker
            .app_tx
            .send(spawn_msg)
            .expect("failed sending new_task msg");
    }

    pub fn new_fork(&self, fri: Lri, args: Struple<Val>) -> Receiver<Val>
    {
        let (send, recv) = channel();
        let spawn_msg = AppMsg::Spawn(send, fri, args);
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
    code: HashMap<Lstr, HashMap<Lstr, Rc<Code>>>,
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
            let did_something = self.run_once();
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

    pub fn run_once(&mut self) -> bool
    {
        let mut did_something = false;
        while let Result::Ok(msg) = self.msg_rx.try_recv() {
            did_something = true;
            self.process_msg(msg);
        }

        match self.pop_fresh() {
            Some(ReadyFiber::New(f)) => {
                did_something = true;
                self.load_code(f);
            }
            Some(ReadyFiber::Ready(mut f, code)) => {
                did_something = true;
                let ev = self.execute_frame(&mut f, &*code);
                // .expect("failure executing frame");
                self.handle_event(f, ev, code)
                    .expect("failure handling event");
            }
            None => {}
        }
        did_something
    }

    fn find_code<'a>(
        &'a self,
        modname: &str,
        funcname: &str,
    ) -> Option<Rc<Code>>
    {
        self.code
            .get(modname)
            .and_then(|module: &'a HashMap<Lstr, Rc<Code>>| {
                module.get(funcname)
            })
            .map(|func: &'a Rc<Code>| (*func).clone())
    }

    fn load_code(&mut self, curf: Fiber)
    {
        let opt_code = self.find_code(curf.module_name(), curf.function_name());
        if let Some(func) = opt_code {
            self.push_coded_fiber(curf, func);
        } else {
            let msg = AppMsg::RequestCode(
                self.id,
                curf.fiber_id,
                MsgItem::new(curf.module_name()),
                MsgItem::new(curf.function_name()),
            );
            self.app_tx
                .send(msg)
                .expect("failure sending load code message to app");
            let fiber_id = curf.fiber_id;
            let fw = FiberWait::Code(curf);
            self.waiting.insert(fiber_id, fw);
        }
    }

    pub fn execute_frame(&self, f: &mut Fiber, code: &Code) -> Event
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
                panic!("cannot execute iop in a worker\n");
            }
        }
    }

    pub fn handle_event(
        &mut self,
        mut fbr: Fiber,
        e: Event,
        code: Rc<Code>,
    ) -> Poll<Val, Val>
    {
        match e {
            Event::Complete(success) => {
                if success {
                    // analyze successful function run
                } else {
                    vout!("function call failed\n");
                }
                self.return_from_call(fbr);
                Result::Ok(Async::NotReady)
            }
            Event::Success => {
                vout!("function call success\n");
                Result::Ok(Async::NotReady)
            }
            Event::Call(dst, line, func, args) => {
                vout!("push_call({} {}, {:?})\n", line, func, args);
                fbr.push_call(code.clone(), dst, line, func, args);
                self.load_code(fbr);
                Result::Ok(Async::NotReady)
            }
            Event::NewTask(Val::FuncRef(callri, callargs, _)) => {
                let (sender, _receiver) = channel();
                let msg = AppMsg::Spawn(sender, callri, callargs);
                self.app_tx.send(msg).expect("new task msg send failure");
                let (new_child, new_parent) = fbr.new_task_key();
                let new_task_key = Val::Tuple(Struple(vec![
                    (None, Val::Int(new_child)),
                    (None, Val::Int(new_parent)),
                ]));
                fbr.head.parent.set_result(new_task_key);
                self.return_from_call(fbr);
                Result::Ok(Async::NotReady)
            }
            Event::NewTask(p) => {
                panic!("Event::NewTask parameter must be a FuncRef: {:?}", p);
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
                } else {
                    panic!(
                        "cannot send iop from worker({}) to worker({})",
                        self.id, rsrc_worker_id
                    );
                }
                Result::Ok(Async::NotReady)
            }
            Event::Uneventful => {
                println!("We shouldn't be here with uneventful");
                panic!("code: {:?}, pc: {:?}", code, fbr.head.pc);
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
            }
        }
    }

    pub fn process_msg(&mut self, msg: WorkerMsg)
    {
        match msg {
            WorkerMsg::Spawn(result_dst, func, args) => {
                vout!("worker spawn2 {}\n", func);
                let parent = Parent::new_fork(result_dst);
                let root = Frame::new_root(parent, func, args);
                self.spawn_fiber(root);
            }
            WorkerMsg::FoundCode(fiber_id, module, func, code) => {
                let rc_code = Rc::new(code);
                let mut new_mod = HashMap::new();
                new_mod.insert(func.take(), rc_code.clone());
                self.code.insert(module.take(), new_mod);
                let opt_fiber = self.waiting.remove(&fiber_id);
                if let Some(FiberWait::Code(fib)) = opt_fiber {
                    self.push_coded_fiber(fib, rc_code);
                } else {
                    panic!("Cannot find waiting fiber: {}", fiber_id);
                }
            }
            WorkerMsg::IopResult(fiber_id, result_msg) => {
                vout!("iop_result({}, {:?})\n", fiber_id, result_msg);
                let result_val = result_msg.take();
                let wait = self.waiting.remove(&fiber_id).unwrap();
                if let FiberWait::Io(mut fib) = wait {
                    fib.head.parent.set_result(result_val);
                    self.return_from_call(fib);
                }
            }
            WorkerMsg::Done => {
                self.done = true;
            }
        }
    }

    fn push_coded_fiber(&mut self, fib: Fiber, code: Rc<Code>)
    {
        if code.is_leema() {
            self.push_fresh(ReadyFiber::Ready(fib, code));
        } else if code.is_rust() {
            self.push_fresh(ReadyFiber::Ready(fib, code));
        } else if let Some((iopf, rsrc_idx)) = code.get_iop() {
            let fiber_id = fib.fiber_id;
            let rsrc_id = rsrc_idx.and_then(|idx| {
                if let &Val::ResourceRef(rid) = fib.head.e.get_param(idx) {
                    Some(rid)
                } else {
                    None
                }
            });
            let msg_val = MsgVal::new(fib.head.e.get_params());
            self.io_tx
                .send(IoMsg::Iop {
                    worker_id: self.id,
                    fiber_id,
                    rsrc_id,
                    action: iopf,
                    params: msg_val,
                })
                .expect("io send failure");
            self.waiting.insert(fiber_id, FiberWait::Io(fib));
        } else {
            panic!("code is what type? {:?}", *code);
        }
    }

    pub fn spawn_fiber(&mut self, frame: Frame)
    {
        vout!("spawn_fiber({})\n", frame.function);
        let id = self.next_fiber_id;
        self.next_fiber_id += 1;
        let fib = Fiber::spawn(id, frame);
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
