#[macro_use]
use leema::log;
use leema::frame::{Frame};
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
pub enum FiberState
{
    New,
    CodeWait,
    IoWait,
    Complete,
}

#[derive(Debug)]
pub struct Fiber
{
    fiber_id: i64,
    state: FiberState,
    handle: reactor::Handle,
    to_worker: Sender<FiberToWorkerMsg>,
    from_worker: Receiver<WorkerToFiberMsg>,
    head: Frame,
    code: Option<Rc<Code>>,
}

impl Fiber
{
    pub fn spawn(h: &reactor::Handle, id: i64, root: Frame
            , tx: Sender<FiberToWorkerMsg>, rx: Receiver<WorkerToFiberMsg>)
    {
        let f = Fiber{
            fiber_id: id,
            state: FiberState::New,
            handle: h.clone(),
            to_worker: tx,
            from_worker: rx,
            head: root,
            code: None,
        };
        let fref = Rc::new(RefCell::new(f));
        Fiber::request_code(fref);
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
        let f: &mut Fiber = &mut *(fref.borrow_mut());
        let msg = FiberToWorkerMsg::RequestCode(
            f.fiber_id,
            f.module_name().to_string(),
            f.function_name().to_string(),
        );
        f.state = FiberState::CodeWait;
        f.to_worker.start_send(msg);
        let h = f.handle.clone();

        let fut = SentMessage{f: fref.clone()}
            .and_then(|fref2: Rc<RefCell<Fiber>>| {
                let f2: &mut Fiber = &mut *(fref2.borrow_mut());
                ReceivedMessage{f: fref2.clone()}
                    .map(|i| {
                })
            });
        h.spawn(fut);
    }

    pub fn receive_msg(&mut self, msg: WorkerToFiberMsg)
    {
        match msg {
            WorkerToFiberMsg::FoundCode(code) => {
                self.code = Some(code);
            }
        }
    }
}

impl Future for Fiber
{
    type Item = ();
    type Error = ();

    fn poll(&mut self) -> Poll<(), ()>
    {
        let tp = task::park();
        let d = Duration::new(0, 100000);
        let t = reactor::Timeout::new(d, &self.handle)
            .unwrap()
            .map(move |fut| {
println!("fiber timed out: {:?}", fut);
                tp.unpark();
            })
            .map_err(|_| {
                () // Val::new_str("timeout error".to_string())
            });
        self.handle.spawn(t);
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
            _ => {
                presult.map(|r| {
                    Async::Ready(self.f.clone())
                })
            }
        }
    }
}
