
use leema::code::{self, CodeKey, Code, Op, OpVec, ModSym, RustFunc};
use leema::fiber::{Fiber};
use leema::frame::{self, Event, Frame, Parent, Resource, Iop, Ioq};
use leema::log;
use leema::reg::{Reg};
use leema::val::{Env, Val, MsgVal, Type};

use std::cell::{RefCell, RefMut, Ref};
use std::collections::{HashMap, LinkedList};
use std::fmt;
use std::io::{stderr, Write};
use std::mem;
use std::rc::{Rc};
use std::sync::atomic::{AtomicBool, AtomicIsize, Ordering};
use std::sync::mpsc::{channel, Sender, Receiver};
use std::thread;
use std::time::{Duration};

use futures::{Poll, Async, Stream, Sink};
use futures::future::{Future};
use futures::task;
use tokio_core::reactor;


#[derive(Debug)]
pub enum Msg
{
    // Spawn(module, function)
    Spawn(String, String),
    // RequestCode(worker_id, frame_id, module, function)
    RequestCode(i64, i64, String, String),
    // FoundCode(frame_id, module, function, code)
    FoundCode(i64, String, String, Code),
    MainResult(MsgVal),
    IOReady(i64),
}

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
    Io(Fiber, Rc<Code>),
    Future(Fiber, Rc<Code>),
}


pub struct Worker
{
    fresh: LinkedList<ReadyFiber>,
    waiting: HashMap<i64, FiberWait>,
    handle: reactor::Handle,
    code: HashMap<String, HashMap<String, Rc<Code>>>,
    resource: HashMap<i64, Ioq>,
    app_tx: Sender<Msg>,
    app_rx: Receiver<Msg>,
    //io: IOQueue,
    id: i64,
    next_fiber_id: i64,
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
    pub fn run(wid: i64, send: Sender<Msg>, recv: Receiver<Msg>)
    {
        let mut core = reactor::Core::new().unwrap();
        let h = core.handle();
        let w = Worker{
            fresh: LinkedList::new(),
            waiting: HashMap::new(),
            handle: h.clone(),
            resource: HashMap::new(),
            code: HashMap::new(),
            app_tx: send,
            app_rx: recv,
            id: wid,
            next_fiber_id: 0,
            done: false,
        };
        let wexec = WorkerExec{
            w: Rc::new(RefCell::new(w)),
            h: h,
        };
        let result = core.run(wexec).unwrap();
        println!("worker {} done with: {:?}", wid, result);
    }

    fn find_code<'a>(&'a self, modname: &str, funcname: &str)
        -> Option<Rc<Code>>
    {
        self.code.get(modname)
        .and_then(|module: &'a HashMap<String, Rc<Code>>| {
            module.get(funcname)
        })
        .map(|func: &'a Rc<Code>| {
            (*func).clone()
        })
    }

    fn load_code(&mut self, curf: Fiber)
    {
        let opt_code = self.find_code(curf.module_name(), curf.function_name());
        if let Some(func) = opt_code {
            self.push_fresh(ReadyFiber::Ready(curf, func));
        } else {
            let msg = Msg::RequestCode(self.id, curf.fiber_id
                , curf.module_name().to_string()
                , curf.function_name().to_string());
            self.app_tx.send(msg);
            let fiber_id = curf.fiber_id;
            let fw = FiberWait::Code(curf);
            self.waiting.insert(fiber_id, fw);
        }
    }

    pub fn handle_event(&mut self, e: Event, code: Rc<Code>)
        -> Poll<Val, Val>
    {
        match e {
            Event::Complete(mut f, success) => {
                if success {
                    // analyze successful function run
                } else {
                    vout!("function call failed\n");
                }
                vout!("function is complete\n");
                let parent = f.head.take_parent();
                match parent {
                    Parent::Caller(old_code, mut pf, dst) => {
                        pf.pc += 1;
                        f.head = *pf;
                        vout!("return to caller: {}.{}()\n"
                            , f.head.module_name()
                            , f.head.function_name()
                            );
                        self.push_fresh(ReadyFiber::Ready(f, old_code));
                    }
                    Parent::Repl(res) => {
                    }
                    Parent::Main(res) => {
                        vout!("finished main func\n");
                        let msg = Msg::MainResult(res.to_msg());
                        self.done = true;
                        self.app_tx.send(msg);
                    }
                    Parent::Null => {
                        // this shouldn't have happened
                    }
                }
                Result::Ok(Async::NotReady)
            }
            Event::Success => {
                Result::Ok(Async::NotReady)
            }
            Event::Failure => {
                Result::Ok(Async::NotReady)
            }
            Event::Call(mut fiber, dst, module, func, args) => {
                vout!("push_call({}.{})\n", module, func);
                fiber.push_call(code.clone(), dst, module, func, args);
                self.load_code(fiber);
                Result::Ok(Async::NotReady)
            }
            Event::FutureWait(reg) => {
                println!("wait for future {:?}", reg);
                Result::Ok(Async::NotReady)
            }
            Event::Iop((rsrc_worker_id, rsrc_id), iopf, iopargs) => {
                if self.id == rsrc_worker_id {
                    let resp = self.create_iop_response();
                    let opt_ioq = self.resource.get_mut(&rsrc_id);
                    if opt_ioq.is_none() {
                        panic!("Iop resource not found: {}", rsrc_id);
                    }
                    let mut ioq = opt_ioq.unwrap();
                    match ioq.checkout(self.id, iopf, iopargs) {
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
                } else {
                    panic!("cannot send iop from worker({}) to worker({})",
                        self.id, rsrc_worker_id);
                }
                Result::Ok(Async::NotReady)
            }
            Event::IoFuture(fut) => {
                self.handle.spawn(fut);
                Result::Ok(Async::NotReady)
            }
            Event::Iop2(_) => {
                Result::Ok(Async::NotReady)
            }
            Event::IOWait => {
                println!("do I/O");
                Result::Ok(Async::NotReady)
            }
            Event::Fork => {
                // self.fresh.push_back((code, curf));
                // end this iteration,
                Result::Ok(Async::NotReady)
            }
            Event::Uneventful(nextf) => {
                println!("We shouldn't be here with uneventful");
                panic!("code: {:?}, pc: {:?}", code, nextf.head.pc);
            }
            Event::None => {
                panic!("Event::None? wtf!");
            }
        }
    }

    pub fn process_msg(&mut self, msg: Msg)
    {
        match msg {
            Msg::Spawn(module, call) => {
                vout!("worker call {}.{}()\n", module, call);
                self.spawn_fiber(module, call);
            }
            Msg::FoundCode(fiber_id, module, func, code) => {
                let rc_code = Rc::new(code);
                let mut new_mod = HashMap::new();
                new_mod.insert(func, rc_code.clone());
                self.code.insert(module, new_mod);
                let opt_fiber = self.waiting.remove(&fiber_id);
                if let Some(FiberWait::Code(fib)) = opt_fiber {
                    self.push_fresh(ReadyFiber::Ready(fib, rc_code));
                } else {
                    panic!("Cannot find waiting fiber: {}", fiber_id);
                }
            }
            _ => {
                panic!("Must be a message for the app: {:?}", msg);
            }
        }
    }

    pub fn spawn_fiber(&mut self, module: String, func: String)
    {
        vout!("spawn_fiber({}::{})\n", module, func);
        let id = self.next_fiber_id;
        self.next_fiber_id += 1;
        let frame = Frame::new_root(module, func);
        let fib = Fiber::spawn(id, frame, &self.handle); //self.app_rx.clone());
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

    fn add_fork(&mut self, key: &CodeKey, newf: Frame)
    {
vout!("lock app, add_fork\n");
    }

    fn create_iop_response(&self) -> Box<Fn(Val, Box<Resource>)>
    {
        let h = self.handle.clone();
        Box::new(|result, rsrc| {
        })
    }
}


struct WorkerExec
{
    w: Rc<RefCell<Worker>>,
    h: reactor::Handle,
}

impl WorkerExec
{
    pub fn run_once(&mut self) -> Poll<Val, Val>
    {
        RefMut::map(self.w.borrow_mut(), |wref| {
            while let Result::Ok(msg) = wref.app_rx.try_recv() {
                wref.process_msg(msg);
            }
            wref
        });

        let opt_ev = {
            let wref: &mut Worker = &mut *(self.w.borrow_mut());
            match wref.pop_fresh() {
                Some(ReadyFiber::New(f)) => {
                    wref.load_code(f);
                    None
                }
                Some(ReadyFiber::Ready(mut f, code)) => {
                    Some(WorkerExec::execute_frame(f, code))
                }
                None => None,
            }
        };
        if let Some((ev, code)) = opt_ev {
            self.w.borrow_mut().handle_event(ev, code)
        } else {
            Result::Ok(Async::NotReady)
        }
    }

    pub fn execute_frame(mut f: Fiber, code: Rc<Code>
        ) -> (Event, Rc<Code>)
    {
        let ev = match &*code {
            &Code::Leema(ref ops) => {
                f.execute_leema_frame(ops)
            }
            &Code::Rust(ref rf) => {
                vout!("execute rust code\n");
                rf(f)
            }
        };
        (ev, code)
    }
}

impl Future for WorkerExec
{
    type Item = Val;
    type Error = Val;

    fn poll(&mut self) -> Poll<Val, Val>
    {
        task::park().unpark();
        let poll_result = self.run_once();
        thread::yield_now();
        poll_result
    }
}
