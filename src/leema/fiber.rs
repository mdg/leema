#[macro_use]
use leema::log;
use leema::frame::{Frame, Event, Parent, FrameTrace};
use leema::val::{Val, Env, FutureVal, Type, MsgVal};
use leema::reg::{Reg, Ireg};
use leema::code::{self, CodeKey, Code, Op, OpVec, ModSym, RustFunc};
use leema::list;

use std::cell::{RefCell, RefMut};
use std::collections::{HashMap, LinkedList};
use std::collections::hash_map;
use std::ops::{DerefMut};
use std::rc::{Rc};
use std::mem;
use std::fmt::{self, Debug};
use std::time::{Duration};
use std::thread;
use std::io::{stderr, Write};

use futures::{Poll, Async, Sink, Stream};
use futures::future::{Future};
use futures::task;
use futures::unsync::mpsc::{Sender, Receiver};
use tokio_core::reactor;


#[derive(Debug)]
pub enum FiberToWorkerMsg
{
    // RequestCode(fiber_id, module, function)
    RequestCode(i64, String, String),
    MainResult(MsgVal),
}

#[derive(Debug)]
pub enum WorkerToFiberMsg
{
    FoundCode(Rc<Code>),
}

#[derive(Debug)]
pub struct Fiber
{
    fiber_id: i64,
    handle: reactor::Handle,
    to_worker: Sender<FiberToWorkerMsg>,
    from_worker: Receiver<WorkerToFiberMsg>,
    head: Frame,
}

#[derive(Debug)]
pub enum FiberState
{
    New(Rc<RefCell<Fiber>>),
    CodeWait(Rc<RefCell<Fiber>>),
    IoWait(Rc<RefCell<Fiber>>, Rc<Code>),
    Ready(Rc<RefCell<Fiber>>, Rc<Code>),
    Complete(Rc<RefCell<Fiber>>),
}

impl Fiber
{
    pub fn spawn(h: &reactor::Handle, id: i64, root: Frame
            , tx: Sender<FiberToWorkerMsg>, rx: Receiver<WorkerToFiberMsg>)
    {
        let f = Fiber{
            fiber_id: id,
            handle: h.clone(),
            to_worker: tx,
            from_worker: rx,
            head: root,
        };
        let fref = Rc::new(RefCell::new(f));
        Fiber::request_code(fref.clone());
    }

    pub fn id(&self) -> i64
    {
        self.fiber_id
    }

    pub fn module_name(&self) -> &str
    {
        self.head.module_name()
    }

    pub fn function_name(&self) -> &str
    {
        self.head.function_name()
    }

    pub fn request_code(fref: Rc<RefCell<Fiber>>)
    {
println!("request_code()");
        let fut =
            SentMessage{f: fref.clone()}
            .and_then(|f2| {
println!("message sent for fiber");
                ReceivedMessage{f: f2}
            })
            .map(|i| {
println!("received message on fiber");
            });

        let f: &mut Fiber = &mut *(fref.borrow_mut());
        let msg = FiberToWorkerMsg::RequestCode(
            f.fiber_id,
            f.module_name().to_string(),
            f.function_name().to_string(),
        );
        f.to_worker.start_send(msg);
        let h = f.handle.clone();

        h.spawn(fut);
println!("code requested");
        // Result::Ok(Async::NotReady)
    }

    pub fn push_call(&mut self, code: Rc<Code>, dst: Reg
            , module: Rc<String>, func: Rc<String>, args: Val
    ) {
        let trace = self.head.trace.clone();
        let mut newf = Frame{
            parent: Parent::Null,
            module: module.clone(),
            function: func.clone(),
            trace: FrameTrace::push_call(&trace, &(*func)),
            e: Env::with_args(args),
            pc: 0,
        };
        mem::swap(&mut self.head, &mut newf);
        let parent = Parent::Caller(code, Box::new(newf), dst);
        self.head.set_parent(parent);
    }

    pub fn handle_msg(f: &Rc<RefCell<Fiber>>, msg: WorkerToFiberMsg)
    {
        match msg {
            WorkerToFiberMsg::FoundCode(code) => {
                println!("set code from msg");
                let h = f.borrow().handle.clone();
                let state = FiberState::Ready(f.clone(), code);
                h.spawn(FiberExec{fs: state});
            }
        }
    }

    fn execute_leema_frame(curf: &mut Frame, ops: &OpVec) -> Event
    {
        let mut e = Event::Uneventful;
        while Event::Uneventful == e {
            e = Frame::execute_leema_op(curf, ops);
        }
        e
    }

    pub fn handle_event(f: &mut Rc<RefCell<Fiber>>, e: Event, code: &Rc<Code>)
    {
        match e {
            Event::Complete(success) => {
                if success {
                    // analyze successful function run
                } else {
                    vout!("function call failed\n");
                }
                let parent = f.borrow_mut().head.take_parent();
                vout!("function is complete: {:?}\n", parent);
                match parent {
                    Parent::Caller(old_code, mut pf, dst) => {
                        RefMut::map(f.borrow_mut(), move |fref| {
                            pf.pc += 1;
                            fref.head = *pf;
                            fref
                        });
                    }
                    Parent::Repl(res) => {
                        /*
vout!("lock app, repl done in iterate\n");
                        let mut _app = self.app.lock().unwrap();
                        _app.set_result(res);
                        */
                    }
                    Parent::Main(res) => {
vout!("finished main func\n");
                        let msg = FiberToWorkerMsg::MainResult(res.to_msg());
                        // let fclone = f.clone();
                        RefMut::map(f.borrow_mut(), move |fref2| {
                            /*
                            (*fref2).to_worker.send(msg);
                            let h = fref2.handle.clone();
                            let fut =
                                SentMessage{f: fclone}
                                .map(|_| {
                                    ()
                                });
                                */
                            // h.spawn(fut);
                            fref2
                        });
                    }
                    /*
                    Parent::Fork(mut ready, mut tx) => {
                        println!("finished a fork!");
                        // still need to pass the
                        // result from the fork
                        // to the parent
                        let r = curf.e.takeResult();
                        println!("send({:?})", r);
                        tx.send(Msg::MainResult(r.to_msg()));
                        ready.store(
                            true,
                            Ordering::Relaxed,
                        );
                    }
                    */
                    Parent::Null => {
                        // this shouldn't have happened
                    }
                }
            }
            Event::Call(dst, module, func, args) => {
                RefMut::map(f.borrow_mut(), |fref| {
                    fref.push_call(code.clone(), dst, module, func, args);
                    fref
                });
                // self.load_frame(newf);
            }
            Event::FutureWait(reg) => {
                println!("wait for future {:?}", reg);
            }
            Event::IOWait => {
                println!("do I/O");
            }
            Event::Fork => {
                // self.fresh.push_back((code, curf));
                // end this iteration,
            }
            Event::Uneventful => {
                println!("We shouldn't be here with uneventful");
                panic!("code: {:?}, pc: {:?}", code, f.borrow_mut().head.pc);
            }
        }
    }
}

struct FiberExec
{
    fs: FiberState,
}

impl FiberExec
{
    fn run(f: &mut Rc<RefCell<Fiber>>, code: &Rc<Code>)
    // fn run<F>(f: Rc<RefCell<Fiber>>, code: Rc<Code>) // -> F
        // where F: Future<Item=(), Error=()>
    {
        let ev = match **code {
            Code::Leema(ref ops) => {
println!("run leema code");
                // let fref: &mut Fiber = f.borrow;
                Fiber::execute_leema_frame(&mut f.borrow_mut().head, ops);
                Event::Uneventful
            }
            Code::Rust(ref rf) => {
                // rf(&mut rf);
                // self.event = Event::Complete(true);
                Event::Uneventful
            }
            Code::RustIo(ref rf) => {
                // rf(&mut f, &self.handle);
                // self.event = Event::Complete(true);
                Event::Uneventful
            }
        };
        Fiber::handle_event(f, ev, code);
    }
}

impl Future for FiberExec
{
    type Item = ();
    type Error = ();

    fn poll(&mut self) -> Poll<(), ()>
    {
        task::park().unpark();
        match &mut self.fs {
            &mut FiberState::New(_) => {
                println!("brand new fiber");
            }
            &mut FiberState::CodeWait(_) => {
                thread::yield_now();
            }
            &mut FiberState::IoWait(_, _) => {
                println!("fiber io wait");
                thread::yield_now();
            }
            &mut FiberState::Ready(ref mut f, ref code) => {
                // let fut = f.run(code);
                FiberExec::run(f, code);
            }
            &mut FiberState::Complete(_) => {
                println!("fiber complete");
            }
        }
        Result::Ok(Async::NotReady)
    }
}

struct SentMessage
{
    f: Rc<RefCell<Fiber>>,
}

impl Future for SentMessage
{
    type Item = Rc<RefCell<Fiber>>;
    type Error = ();

    fn poll(&mut self) -> Poll<Rc<RefCell<Fiber>>, ()>
    {
println!("SentMessage poll");
        let result = self.f.borrow_mut().to_worker.poll_complete();
        result.map(|a| {
                Async::Ready(self.f.clone())
            })
            .map_err(|_| { () })
    }
}

struct ReceivedMessage
{
    f: Rc<RefCell<Fiber>>,
}

impl Future for ReceivedMessage
{
    type Item = Rc<RefCell<Fiber>>;
    type Error = ();

    fn poll(&mut self) -> Poll<Rc<RefCell<Fiber>>, ()>
    {
println!("ReceivedMessage::poll()");
        let presult = self.f.borrow_mut().from_worker.poll();
        match presult {
            Ok(Async::NotReady) => {
println!("poll returned not ready");
                let tp = task::park();
                tp.unpark();
                Ok(Async::NotReady)
            }
            Ok(Async::Ready(Some(msg))) => {
println!("poll received a msg: {:?}", msg);
                Fiber::handle_msg(&self.f, msg);
                Ok(Async::Ready(self.f.clone()))
            }
            Ok(Async::Ready(None)) => {
                println!("end of queue");
                Ok(Async::Ready(self.f.clone()))
            }
            Err(e) => {
                panic!("poll error {:?}", e);
            }
        }
    }
}
