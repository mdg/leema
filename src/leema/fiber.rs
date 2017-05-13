#[macro_use]
use leema::log;
use leema::frame::{Frame, Event};
use leema::val::{Val, Env, FutureVal, Type};
use leema::reg::{Reg, Ireg};
use leema::code::{self, CodeKey, Code, Op, OpVec, ModSym, RustFunc};
use leema::list;

use std::cell::{RefCell};
use std::collections::{HashMap, LinkedList};
use std::collections::hash_map;
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
        let fut =
            SentMessage{f: fref.clone()}
            .and_then(|f2| {
                ReceivedMessage{f: f2}
            })
            .map(|i| {
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
        // Result::Ok(Async::NotReady)
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

    fn execute_leema_frame(curf: &mut Frame, ops: &OpVec)
    {
        let mut e = Event::Uneventful;
        while Event::Uneventful == e {
            e = Frame::execute_leema_op(curf, ops);
        }
    }
}

struct FiberExec
{
    fs: FiberState,
}

impl FiberExec
{
    fn run(f: &Rc<RefCell<Fiber>>, code: &Rc<Code>)
    // fn run<F>(f: Rc<RefCell<Fiber>>, code: Rc<Code>) // -> F
        // where F: Future<Item=(), Error=()>
    {
        match **code {
            Code::Leema(ref ops) => {
                Fiber::execute_leema_frame(&mut f.borrow_mut().head, ops);
            }
            Code::Rust(ref rf) => {
                // rf(&mut rf);
            }
            Code::RustIo(ref rf) => {
                // rf(&mut f, &self.handle);
                // self.event = Event::Complete(true);
            }
        }
        // self.handle_event(code, curf);
    }
}

impl Future for FiberExec
{
    type Item = ();
    type Error = ();

    fn poll(&mut self) -> Poll<(), ()>
    {
        task::park().unpark();
        match &self.fs {
            &FiberState::New(_) => {
                println!("brand new fiber");
            }
            &FiberState::CodeWait(_) => {
                thread::yield_now();
            }
            &FiberState::IoWait(_, _) => {
                println!("fiber io wait");
                thread::yield_now();
            }
            &FiberState::Ready(ref f, ref code) => {
                // let fut = f.run(code);
                FiberExec::run(f, code);
            }
            &FiberState::Complete(_) => {
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
        let presult = self.f.borrow_mut().from_worker.poll();
        match presult {
            Ok(Async::NotReady) => {
                let tp = task::park();
                tp.unpark();
                Ok(Async::NotReady)
            }
            Ok(Async::Ready(Some(msg))) => {
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
