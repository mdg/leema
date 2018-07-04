
use leema::log;
use leema::lstr::{Lstr};
use leema::msg::{WorkerMsg, AppMsg, IoMsg};
use leema::rsrc::{self, Rsrc, Event, IopCtx};
use leema::struple::{Struple};
use leema::val::{Val};

use std;
use std::cell::{RefCell};
use std::io::{Write};
use std::rc::{Rc};
use std::sync::mpsc::{Sender, Receiver};
use std::collections::{HashMap, LinkedList};
use std::thread;

use futures::{Poll, Async};
use futures::future::{Future};
use futures::stream::{Stream};
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

pub struct Iop
{
    action: rsrc::IopAction,
    ctx: IopCtx,
    src_worker_id: i64,
    src_fiber_id: i64,
    rsrc_id: Option<i64>,
}

pub struct RsrcQueue
{
    rsrc_id: i64,
    rsrc: Option<Box<Rsrc>>,
    queue: LinkedList<Iop>,
}

impl RsrcQueue
{
    pub fn new(rsrc_id: i64, resource: Box<Rsrc>) -> RsrcQueue
    {
        RsrcQueue{
            rsrc_id: rsrc_id,
            rsrc: Some(resource),
            queue: LinkedList::new(),
        }
    }

    /**
     * Push an Iop onto the queue
     *
     * If the resource is already in used, then return None
     */
    pub fn push_iop(&mut self, mut iop: Iop) -> Option<Iop>
    {
        match self.rsrc.take() {
            Some(r) => {
                iop.ctx.init_rsrc(r);
                Some(iop)
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
    pub fn checkin(&mut self, r: Box<Rsrc>) -> Option<(Iop, Box<Rsrc>)>
    {
        match self.queue.pop_front() {
            Some(iop) => {
                Some((iop, r))
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
    resource: HashMap<i64, RsrcQueue>,
    pub handle: reactor::Handle,
    next: LinkedList<Iop>,
    msg_rx: std::sync::mpsc::Receiver<IoMsg>,
    app_tx: std::sync::mpsc::Sender<AppMsg>,
    worker_tx: HashMap<i64, std::sync::mpsc::Sender<WorkerMsg>>,
    next_rsrc_id: i64,
    io: Option<Rc<RefCell<Io>>>,
    done: bool
}

impl Io
{
    pub fn new(app_tx: Sender<AppMsg>, msg_rx: Receiver<IoMsg>)
        -> (Rc<RefCell<Io>>, reactor::Core)
    {
        let core = reactor::Core::new().unwrap();
        let h = core.handle();
        let io = Io{
            resource: HashMap::new(),
            handle: h,
            next: LinkedList::new(),
            msg_rx: msg_rx,
            app_tx: app_tx,
            worker_tx: HashMap::new(),
            next_rsrc_id: 1,
            io: None,
            done: false,
        };
        let rcio = Rc::new(RefCell::new(io));
        let rcio2 = rcio.clone();
        rcio.borrow_mut().io = Some(rcio2);
        (rcio, core)
    }

    pub fn run_once(&mut self) -> Poll<Val, Val>
    {
        if let Ok(incoming) = self.msg_rx.try_recv() {
            self.handle_incoming(incoming);
        }
        if self.done {
            Ok(Async::Ready(Val::Int(1)))
        } else {
            Ok(Async::NotReady)
        }
    }

    pub fn handle_incoming(&mut self, incoming: IoMsg)
    {
        match incoming {
            IoMsg::Iop{
                worker_id: wid,
                fiber_id: fid,
                action,
                rsrc_id,
                params,
            } => {
                vout!("iop incoming: {:?}:{:?}:{:?} {:?}\n",
                    wid, fid, rsrc_id, params);
                let param_vals = Val::from_msg(params);
                self.handle_iop_action(wid, fid, action, rsrc_id, param_vals);
            }
            IoMsg::NewWorker(worker_id, worker_tx) => {
                self.worker_tx.insert(worker_id, worker_tx);
            }
            IoMsg::Done => {
                self.done = true;
            }
        }
    }

    fn handle_iop_action(&mut self, worker_id: i64, fiber_id: i64
        , action: rsrc::IopAction, opt_rsrc_id: Option<i64>, params: Val)
    {
        vout!("handle_iop_action({},{},{:?},{:?})\n",
            worker_id, fiber_id, opt_rsrc_id, params);
        let ctx = self.create_iop_ctx(worker_id, fiber_id
            , opt_rsrc_id.clone(), None, params);
        let iop = Iop{
            ctx: ctx,
            action: action,
            src_worker_id: worker_id,
            src_fiber_id: fiber_id,
            rsrc_id: opt_rsrc_id.clone(),
        };
        match opt_rsrc_id {
            None => {
                self.next.push_back(iop);
            }
            Some(rsrc_id) => {
                let opt_rsrc_op = {
                    let opt_rsrcq = self.resource.get_mut(&rsrc_id);
                    if opt_rsrcq.is_none() {
                        panic!("missing queue for rsrc: {}", rsrc_id);
                    }
                    let mut rsrcq = opt_rsrcq.unwrap();
                    rsrcq.push_iop(iop)
                };
                if let Some(rsrc_op) = opt_rsrc_op {
                    self.next.push_back(rsrc_op);
                }
            }
        }
    }

    fn run_iop(&mut self, rsrc_op: Option<(Iop, Box<Rsrc>)>)
    {
        if let Some((mut iop, rsrc)) = rsrc_op {
            vout!("run_iop\n");
            iop.ctx.init_rsrc(rsrc);
            self.next.push_back(iop);
        }
    }

    pub fn take_next_iop(&mut self) -> Option<Iop>
    {
        self.next.pop_front()
    }

    pub fn handle_event(&mut self, worker_id: i64, fiber_id: i64
        , rsrc_id: Option<i64>, ev: Event)
    {
        match ev {
            Event::NewRsrc(rsrc, prev_rsrc) => {
                vout!("handle Event::NewRsrc\n");
                self.return_rsrc(rsrc_id, prev_rsrc);
                let new_rsrc_id = self.new_rsrc(rsrc);
                let result = Val::ResourceRef(new_rsrc_id);
                self.send_result(worker_id, fiber_id, result);
            }
            Event::Result(result, rsrc) => {
                vout!("handle Event::Result\n");
                self.return_rsrc(rsrc_id, rsrc);
                self.send_result(worker_id, fiber_id, result);
            }
            Event::Future(libfut) => {
                vout!("handle Event::Future\n");
                let rcio: Rc<RefCell<Io>> = self.io.clone().unwrap();
                let rcio_err = rcio.clone();
                let iofut = libfut
                    .map(move |ev2| {
                        vout!("handle Event::Future ok\n");
                        let mut bio = rcio.borrow_mut();
                        bio.handle_event(worker_id, fiber_id, rsrc_id, ev2);
                        ()
                    }).map_err(move |ev2| {
                        vout!("handle Event::Future map_err\n");
                        let mut bio = rcio_err.borrow_mut();
                        bio.handle_event(worker_id, fiber_id, rsrc_id, ev2);
                        ()
                    });
                vout!("spawn new future\n");
                self.handle.spawn(iofut);
            }
            Event::Stream(libstream) => {
                vout!("handle Event::Stream\n");
                let rcio: Rc<RefCell<Io>> = self.io.clone().unwrap();
                let rcio_err = rcio.clone();
                let iostream = libstream
                    .into_future()
                    .map(move |(ev2, str2)| {
                        let mut bio = rcio.borrow_mut();
                        bio.handle_event(
                            worker_id, fiber_id, rsrc_id, ev2.unwrap()
                        );
                        ()
                    })
                    .map_err(move |(ev2, str2)| {
                        let mut bio = rcio_err.borrow_mut();
                        bio.handle_event(worker_id, fiber_id, rsrc_id, ev2);
                        ()
                    });
                self.handle.spawn(iostream);
            }
        }
    }

    fn create_iop_ctx<'a>(&'a mut self, src_worker_id: i64, src_fiber_id: i64
        , rsrc_id: Option<i64>, rsrc: Option<Box<Rsrc>>, param_val: Val)
        -> IopCtx
    {
        let h = self.handle.clone();
        let rcio = self.io.clone().unwrap();
        // let tx = self.worker_tx.clone();
        IopCtx::new(rcio, src_worker_id, src_fiber_id, rsrc_id, rsrc, param_val)
    }

    pub fn new_rsrc(&mut self, rsrc: Box<Rsrc>) -> i64
    {
        let rsrc_id = self.next_rsrc_id;
        self.next_rsrc_id += 1;
        self.resource.insert(rsrc_id, RsrcQueue::new(rsrc_id, rsrc));
        rsrc_id
    }

    pub fn send_result(&mut self, worker_id: i64, fiber_id: i64, result: Val)
    {
        vout!("send_result({},{},{:?})\n", worker_id, fiber_id, result);
        let tx = self.worker_tx.get(&worker_id).unwrap();
        tx.send(WorkerMsg::IopResult(fiber_id, result.to_msg()));
    }

    pub fn return_rsrc(&mut self, rsrc_id: Option<i64>, rsrc: Option<Box<Rsrc>>)
    {
        vout!("return_rsrc({:?})\n", rsrc_id);
        match (rsrc_id, rsrc) {
            (Some(irsrc_id), Some(real_rsrc)) => {
                let next_op = {
                    let ioq = self.resource.get_mut(&irsrc_id).unwrap();
                    ioq.checkin(real_rsrc)
                };
                self.run_iop(next_op);
            }
            (Some(irsrc_id), None) => {
                vout!("rsrc was not returned for {}\n", irsrc_id);
                // TODO: maybe should clear the ioq?
            }
            (None, None) => {
                // TODO: maybe should clear the ioq?
            }
            (None, _) => {
                panic!("cannot return resource without id");
                // TODO: maybe should clear the ioq?
            }
        }
    }
}

pub struct IoLoop
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
        task::current().notify();
        let poll_result = self.io.borrow_mut().run_once();
        let opt_iop = self.io.borrow_mut().take_next_iop();
        if let Some(iop) = opt_iop {
            let ev = (iop.action)(iop.ctx);
            self.io.borrow_mut().handle_event(iop.src_worker_id
                , iop.src_fiber_id, iop.rsrc_id, ev);
        }
        thread::yield_now();
        poll_result
    }
}


#[cfg(test)]
pub mod tests
{
    use leema::io::{Io, IoLoop};
    use leema::lstr::{Lstr};
    use leema::msg;
    use leema::rsrc::{self, Rsrc};
    use leema::struple::{Struple};
    use leema::val::{Val, Type};

    use std::rc::{Rc};
    use std::sync::mpsc;

#[derive(Debug)]
struct MockRsrc {}

impl Rsrc for MockRsrc
{
    fn get_type(&self) -> Type
    {
        Type::Resource(Rc::new("MockRsrc".to_string()))
    }
}

fn mock_iop_action(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    rsrc::Event::Result(Val::Int(8), None)
}

fn mock_rsrc_action(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    let rsrc: MockRsrc = ctx.take_rsrc();
    rsrc::Event::Result(Val::Int(18), Some(Box::new(rsrc)))
}

pub fn exercise_iop_action(action: rsrc::IopAction
    , params: Vec<(Option<Lstr>, Val)>
    ) -> Result<(i64, Val), mpsc::TryRecvError>
{
    let (msg_tx, msg_rx) = mpsc::channel::<msg::IoMsg>();
    let (app_tx, _) = mpsc::channel::<msg::AppMsg>();
    let (worker_tx, worker_rx) = mpsc::channel::<msg::WorkerMsg>();

    let (io, core) = Io::new(app_tx, msg_rx);

    let msg_params = Val::Struple(None, Struple(params)).to_msg();
    msg_tx.send(msg::IoMsg::NewWorker(11, worker_tx));
    msg_tx.send(msg::IoMsg::Iop{
        worker_id: 11,
        fiber_id: 21,
        action: action,
        rsrc_id: None,
        params: msg_params,
    });
    msg_tx.send(msg::IoMsg::Done);

    IoLoop::run(core, io);

    worker_rx.try_recv()
        .map(|result_msg| {
            match result_msg {
                msg::WorkerMsg::IopResult(fiber_id, msg_val) => {
                    (fiber_id, Val::from_msg(msg_val))
                }
                _ => {
                    (0, Val::new_str("that didn't work".to_string()))
                }
            }
        })
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
    let resp = exercise_iop_action(mock_iop_action, vec![]);
    assert!(resp.is_ok());
}

#[test]
fn test_rsrc_action_flow()
{
    let (msg_tx, msg_rx) = mpsc::channel::<msg::IoMsg>();
    let (app_tx, _) = mpsc::channel::<msg::AppMsg>();
    let (worker_tx, worker_rx) = mpsc::channel::<msg::WorkerMsg>();

    let (mut io, core) = Io::new(app_tx, msg_rx);
    let rsrc_id = io.borrow_mut().new_rsrc(Box::new(MockRsrc{}));

    msg_tx.send(msg::IoMsg::NewWorker(8, worker_tx));
    msg_tx.send(msg::IoMsg::Iop{
        worker_id: 8,
        fiber_id: 7,
        action: mock_rsrc_action,
        rsrc_id: Some(rsrc_id),
        params: Val::empty_tuple().to_msg(),
    });
    msg_tx.send(msg::IoMsg::Done);
    IoLoop::run(core, io);

    let resp = worker_rx.try_recv();
    assert!(resp.is_ok());
}

}
