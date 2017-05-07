#[macro_use]
use leema::log;
use leema::frame::{Frame};
use leema::val::{Val, Env, FutureVal, Type};
use leema::reg::{Reg, Ireg};
use leema::code::{self, CodeKey, Code, Op, OpVec, ModSym, RustFunc};
use leema::list;

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
pub struct Fiber
{
    fiber_id: i64,
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
            handle: h.clone(),
            to_worker: tx,
            from_worker: rx,
            head: root,
            code: None,
        };
        f.request_code();
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

    pub fn request_code(mut self)
    {
        let msg = FiberToWorkerMsg::RequestCode(
            self.fiber_id,
            self.module_name().to_string(),
            self.function_name().to_string(),
        );
        self.to_worker.start_send(msg);
        /*
        self.from_worker.and_then(|msg| {
            println!("fiber.from_worker({:?})", msg);
            self.receive_msg(msg);
            let d = Duration::new(0, 100000);
            reactor::Timeout::new(d, &self.handle).map_err(|_| {()})
        });
        */
        let h = self.handle.clone();
        let f = SentMessage{f: Some(self)}
            .and_then(|f: Fiber| {
                println!("message sent for fiber: {:?}", f.fiber_id);
                ReceivedMessage{f: f}
                    .map(|i| {
                    ()
                })
            });
        h.spawn(f);
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
    f: Option<Fiber>,
}

impl Future for SentMessage
{
    type Item = Fiber;
    type Error = ();

    fn poll(&mut self) -> Poll<Fiber, ()>
    {
        let mut f = self.f.take().unwrap();
        let result = f.to_worker.poll_complete();
        result.map(|a| {
                Async::Ready(f)
            })
            .map_err(|_| { () })
    }
}

struct ReceivedMessage
{
    f: Fiber,
}

impl Future for ReceivedMessage
{
    type Item = Fiber;
    type Error = ();

    fn poll(&mut self) -> Poll<Fiber, ()>
    {
        let fut = self.f.from_worker.poll();
        Ok(Async::NotReady)
    }
}
