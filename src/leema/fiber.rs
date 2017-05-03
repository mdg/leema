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

use futures::{Poll, Async};
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
    FoundCode(Code),
}

#[derive(Debug)]
pub struct Fiber
{
    fiber_id: i64,
    handle: reactor::Handle,
    to_worker: Sender<FiberToWorkerMsg>,
    from_worker: Receiver<WorkerToFiberMsg>,
    head: Frame,
    code: Option<Code>,
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
        h.spawn(f);
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

    pub fn request_code(&self)
    {
    }
}

impl Future for Fiber
{
    type Item = ();
    type Error = ();

    fn poll(&mut self) -> Poll<(), ()>
    {
        if self.code.is_none() {
            self.request_code();
        }
        let d = Duration::new(0, 100000);
        let tp = task::park();
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
