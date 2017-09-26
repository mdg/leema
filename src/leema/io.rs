
use leema::frame::{self, Event};
use leema::msg::{WorkerMsg, AppMsg, IoMsg};
use leema::rsrc::{self, Rsrc, IopCtx, RsrcAction, IopAction};
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

pub struct RsrcOp
{
    action: rsrc::RsrcAction,
    params: Vec<Val>,
    src_worker_id: i64,
    src_fiber_id: i64,
}

pub struct RsrcQueue
{
    rsrc_id: i64,
    rsrc: Option<Box<Rsrc>>,
    queue: LinkedList<RsrcOp>,
}

impl RsrcQueue
{
    pub fn new(rsrc_id: i64, resource: Box<Rsrc>) -> Ioq
    {
        Ioq{
            rsrc_id: rsrc_id,
            rsrc: Some(resource),
            queue: LinkedList::new(),
        }
    }

    /**
     * Push a RsrcOp onto the queue
     *
     * If the resource is already in used, then return None
     */
    pub fn push(&mut self, worker_id: i64, fiber_id: i64, iopf: RsrcAction
        , args: Vec<Val>) -> Option<(Box<Rsrc>, RsrcOp)>
    {
        let iop = RsrcOp{
            action: iopf,
            params: args,
            src_worker_id: worker_id,
            src_fiber_id: fiber_id,
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

pub struct Iop
{
    action: rsrc::IopAction,
    params: Vec<Val>,
    src_worker_id: i64,
    src_fiber_id: i64,
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

    pub fn add_worker(worker_id: i64)
    {
    }

    pub fn run_once(&mut self) -> Poll<Val, Val>
    {
        if let Ok(incoming) = self.msg_rx.try_recv() {
            self.handle_incoming(incoming);
        }
        let rsrc_id = 0;
        let resp = self.create_iop_ctx(0, 0, rsrc_id);
        Ok(Async::NotReady)
    }

    pub fn handle_incoming(&mut self, incoming: IoMsg)
    {
        match incoming {
            IoMsg::Iop{
                worker_id: wid,
                frame_id: fid,
                action,
                params,
            } => {
                println!("handle incoming Iop");
                let param_vals = params.into_iter().map(|mv| {
                    Val::from_msg(mv)
                }).collect();
                self.handle_iop_action(wid, fid, action, param_vals);
            }
            IoMsg::RsrcOp{
                worker_id: wid,
                frame_id: fid,
                rsrc_id,
                action,
                params,
            } => {
                println!("handle incoming RsrcOp");
                let param_vals = params.into_iter().map(|mv| {
                    Val::from_msg(mv)
                }).collect();
                self.handle_rsrc_action(wid, fid
                    , action, rsrc_id, param_vals);
            }
            IoMsg::NewWorker(worker_id, worker_tx) => {
                self.worker_tx.insert(worker_id, worker_tx);
            }
        }
    }

    fn handle_iop_action(&mut self, worker_id: i64, frame_id: i64
        , action: IopAction, params: Vec<Val>)
    {
    }

    fn handle_rsrc_action(&mut self, worker_id: i64, frame_id: i64
        , action: RsrcAction, rsrc_id: i64
        , params: Vec<Val>)
    {
    }

    fn handle_event(&mut self, ev: Event)
    {
    }

    fn create_iop_ctx<'a>(&'a mut self, src_worker_id: i64, src_fiber_id: i64
        , rsrc_id: i64)
        -> IopCtx<'a>
    {
        let h = self.handle.clone();
        let rcio = self.io.clone().unwrap();
        // let tx = self.worker_tx.clone();
        IopCtx::new(self, src_worker_id, src_fiber_id, rsrc_id)
        /*
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
        */
    }

    pub fn new_rsrc(&mut self, rsrc: Box<Rsrc>) -> i64
    {
        let rsrc_id = self.next_rsrc_id;
        self.next_rsrc_id += 1;
        self.resource.insert(rsrc_id, Ioq::new(rsrc_id, rsrc));
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


#[cfg(test)]
mod tests
{
    use leema::io::{self, Io};
    use leema::msg;

    use std::sync::mpsc;
    use std::collections::{HashMap};

fn mock_iop_action(ctx: &mut IoContext, params: Vec<Val>) -> Event
{
    ctx.set_result(Val::Int(7));
}

#[test]
fn test_io_constructor()
{
    let (_, msg_rx) = mpsc::channel::<msg::IoMsg>();
    let (app_tx, _) = mpsc::channel::<msg::AppMsg>();
    // let worker_tx = HashMap::new();

    let (io, core) = Io::new(app_tx, msg_rx);
}

#[test]
fn test_iop_action_flow()
{
    let (msg_tx, msg_rx) = mpsc::channel::<msg::IoMsg>();
    let (app_tx, _) = mpsc::channel::<msg::AppMsg>();
    // let worker_tx = HashMap::new();

    let (io, core) = Io::new(app_tx, msg_rx);
}

}
