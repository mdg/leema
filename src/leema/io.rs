
use leema::frame::{self, Event};
use leema::msg::{WorkerMsg, AppMsg, IoMsg};
use leema::rsrc::{self, Rsrc};
use leema::val::{Val, MsgVal};
use leema::worker;

use std;
use std::cell::{RefCell, RefMut};
use std::fmt;
use std::rc::{Rc};
use std::sync::mpsc::{channel, Sender, Receiver};
use std::collections::{HashMap, LinkedList};
use std::thread;

use futures::{Poll, Async};
use futures::future::{Future};
use futures::task;
use tokio_core::reactor;

/*
Rsrc
Resource

RsrcOp
RsrcAction
ResourceAction
IoAction

Iop
IoResult

Ioq
ResourceQueue
IoEvent
*/

#[derive(Debug)]
pub struct Iop
{
    pub action: Box<rsrc::Action>,
    pub params: Vec<Val>,
    pub src_worker_id: i64,
}

pub struct Ioq
{
    rsrc: Option<Box<Rsrc>>,
    queue: LinkedList<Iop>,
}

impl Ioq
{
    pub fn new(resource: Box<Rsrc>) -> Ioq
    {
        Ioq{
            rsrc: Some(resource),
            queue: LinkedList::new(),
        }
    }

    /**
     * Checkout a resource object and return it w/ the Iop
     *
     * If the resource is already in used, then return None
     */
    pub fn checkout(&mut self, worker_id: i64, iopf: Box<rsrc::Action>
        , args: Vec<Val>) -> Option<(Box<Rsrc>, Iop)>
    {
        let iop = Iop{
            action: iopf,
            params: args,
            src_worker_id: worker_id,
        };

        match self.rsrc.take() {
            Some(r) => {
                Some((r, iop))
            }
            None => {
                self.queue.push_back(iop);
                None
            }
        }
    }

    /**
     * Add the resource back to the Ioq to be used later
     */
    pub fn checkin(&mut self, r: Box<Rsrc>) -> Option<(Box<Rsrc>, Iop)>
    {
        match self.queue.pop_front() {
            Some(iop) => {
                Some((r, iop))
            }
            None => {
                self.rsrc = Some(r);
                None
            }
        }
    }
}

pub struct Io
{
    resource: HashMap<i64, Ioq>,
    pub handle: reactor::Handle,
    msg_rx: std::sync::mpsc::Receiver<IoMsg>,
    app_tx: std::sync::mpsc::Sender<AppMsg>,
    worker_tx: HashMap<i64, std::sync::mpsc::Sender<WorkerMsg>>,
    next_rsrc_id: i64,
    io: Option<Rc<RefCell<Io>>>,
}

impl Io
{
    pub fn new(app_tx: Sender<AppMsg>, msg_rx: Receiver<IoMsg>)
        -> (Rc<RefCell<Io>>, reactor::Core)
    {
        let mut core = reactor::Core::new().unwrap();
        let h = core.handle();
        let io = Io{
            resource: HashMap::new(),
            handle: h,
            msg_rx: msg_rx,
            app_tx: app_tx,
            worker_tx: HashMap::new(),
            next_rsrc_id: 1,
            io: None,
        };
        let rcio = Rc::new(RefCell::new(io));
        let rcio2 = rcio.clone();
        rcio.borrow_mut().io = Some(rcio2);
        (rcio, core)
    }

    pub fn run_once(&mut self) -> Poll<Val, Val>
    {
        let rsrc_id = 0;
        let resp = self.create_iop_response(rsrc_id, 0, 0);
        Ok(Async::NotReady)
    }

    fn create_iop_response(&self, rsrc_id: i64, src_worker_id: i64
        , src_fiber_id: i64)
        -> Box<Fn(Val, Box<Rsrc>)>
    {
        let h = self.handle.clone();
        let rcio = self.io.clone().unwrap();
        // let tx = self.worker_tx.clone();
        Box::new(move |result, rsrc| {
            RefMut::map(rcio.borrow_mut(), |ioref| {
                // put resource back w/ resource_id
                // same thread
                ioref.return_rsrc(rsrc_id, rsrc);

                // send result value back to original worker and fiber
                // different thread, convert to message and send
                let result_msg = result.to_msg();
                {
                    let src_send = ioref.worker_tx.get(&src_worker_id).unwrap();
                    src_send.send(
                        WorkerMsg::IopResult(src_fiber_id, result_msg));
                }
                ioref
            });
        })
    }

    pub fn new_rsrc(&mut self, rsrc: Box<Rsrc>) -> i64
    {
        let rsrc_id = self.next_rsrc_id;
        self.next_rsrc_id += 1;
        self.resource.insert(rsrc_id, Ioq::new(rsrc));
        rsrc_id
    }

    pub fn return_rsrc(&mut self, rsrc_id: i64, rsrc: Box<Rsrc>)
    {
        let ioq = self.resource.get_mut(&rsrc_id).unwrap();
        if let Some((next_rsrc, iop)) = ioq.checkin(rsrc) {
        }
    }
}

struct IoLoop
{
    handle: reactor::Handle,
    io: Rc<RefCell<Io>>,
}

impl IoLoop
{
    pub fn run(mut core: reactor::Core, rcio: Rc<RefCell<Io>>)
    {
        let my_handle = rcio.borrow().handle.clone();
        let my_loop = IoLoop{
            io: rcio,
            handle: my_handle,
        };

        let result = core.run(my_loop).unwrap();
        println!("io is done with: {:?}", result);
    }
}

impl Future for IoLoop
{
    type Item = Val;
    type Error = Val;

    fn poll(&mut self) -> Poll<Val, Val>
    {
        task::park().unpark();
        let poll_result = self.io.borrow_mut().run_once();
        thread::yield_now();
        poll_result
    }
}
