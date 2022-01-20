use crate::leema::failure::{self, Lresult};
use crate::leema::lstr::Lstr;
use crate::leema::msg::{AppMsg, IoMsg, MsgItem, WorkerMsg};
use crate::leema::program;
use crate::leema::rsrc::{self, Event, IopCtx, Rsrc};
use crate::leema::struple::Struple2;
use crate::leema::val::{Fref, MsgVal, Val};

use std;
use std::cell::RefCell;
use std::cmp::min;
use std::collections::{HashMap, LinkedList};
use std::rc::Rc;
use std::sync::mpsc::{channel, Receiver, Sender, TryRecvError};
use std::thread;
use std::time::Duration;

use futures::future::Future;
use futures::stream::Stream;
use futures::task;
use futures::{Async, Poll};
use tokio::runtime::current_thread;
use tokio_current_thread::TaskExecutor;

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

const DEFAULT_NEXT_CAPACITY: usize = 16;

#[derive(Debug)]
pub struct Iop
{
    src_worker_id: i64,
    src_fiber_id: i64,
    action: rsrc::IopAction,
    params: Struple2<Val>,
    result: Val,
    rsrc_ids: Vec<i64>,
    rsrc_val: HashMap<i64, Box<dyn Rsrc>>,
}

impl Iop
{
    pub fn new(
        worker_id: i64,
        fiber_id: i64,
        action: rsrc::IopAction,
        params: Struple2<Val>,
    ) -> Iop
    {
        let mut rsrc_ids: Vec<i64> = params
            .iter()
            .filter_map(|p| {
                if let Val::ResourceRef(rsrc_id) = &p.v {
                    Some(*rsrc_id)
                } else {
                    None
                }
            })
            .collect();
        rsrc_ids.sort();
        let rsrc_val = HashMap::with_capacity(rsrc_ids.len());
        Iop {
            src_worker_id: worker_id,
            src_fiber_id: fiber_id,
            action,
            params,
            result: Val::VOID,
            rsrc_ids,
            rsrc_val,
        }
    }

    pub fn pop_rsrc_id(&mut self) -> Option<i64>
    {
        self.rsrc_ids.pop()
    }

    pub fn set_result(&mut self, r: Val)
    {
        self.result = r;
    }

    pub fn add_rsrc(&mut self, id: i64, rsrc: Box<dyn Rsrc>) -> Lresult<()>
    {
        if self.rsrc_val.contains_key(&id) {
            return Err(lfail!(
                failure::Mode::CodeFailure,
                "cannot reinitialize rsrc",
                "rsrc_id": ldisplay!(id),
            ));
        }
        self.rsrc_val.insert(id, rsrc);
        Ok(())
    }

    pub fn init_rsrc(&mut self, _id: i64, _rsrc: Box<dyn Rsrc>)
    {
        unimplemented!();
    }

    pub fn get_rsrc<T>(&self, p: i8) -> Lresult<&T>
    where
        T: Rsrc,
    {
        let r = ltry!(self.params.get(p as usize).ok_or_else(|| {
            lfail!(
                failure::Mode::CodeFailure,
                "invalid parameter",
                "param": ldisplay!(p),
            )
        }));
        if let Val::ResourceRef(id) = r.v {
            self.rsrc_val
                .get(&id)
                .map(|v| {
                    let result = v.downcast_ref::<T>();
                    &*(result.unwrap())
                })
                .ok_or_else(|| {
                    lfail!(
                        failure::Mode::RuntimeLeemaFailure,
                        "resource missing",
                        "rsrc_id": ldisplay!(id),
                    )
                })
        } else {
            Err(lfail!(
                failure::Mode::CodeFailure,
                "ref parameter is not a resource",
                "param": ldisplay!(p),
                "value": ldebug!(r.v),
            ))
        }
    }

    pub fn mut_rsrc<T>(&mut self, p: i8) -> Lresult<&mut T>
    where
        T: Rsrc,
    {
        let r = ltry!(self.params.get(p as usize).ok_or_else(|| {
            lfail!(
                failure::Mode::CodeFailure,
                "invalid parameter",
                "param": ldisplay!(p),
            )
        }));
        if let Val::ResourceRef(id) = r.v {
            self.rsrc_val
                .get_mut(&id)
                .map(|v| {
                    let result = v.downcast_mut::<T>();
                    &mut *(result.unwrap())
                })
                .ok_or_else(|| {
                    lfail!(
                        failure::Mode::RuntimeLeemaFailure,
                        "resource missing",
                        "rsrc_id": ldisplay!(id),
                    )
                })
        } else {
            Err(lfail!(
                failure::Mode::CodeFailure,
                "mut parameter is not a resource",
                "param": ldisplay!(p),
                "value": ldebug!(r.v),
            ))
        }
    }

    pub fn take_rsrc<T>(&mut self, p: i8) -> Lresult<T>
    where
        T: Rsrc,
    {
        let r = ltry!(self.params.get(p as usize).ok_or_else(|| {
            lfail!(
                failure::Mode::CodeFailure,
                "invalid parameter",
                "param": ldisplay!(p),
            )
        }));
        if let Val::ResourceRef(id) = r.v {
            self.rsrc_val
                .remove(&id)
                .map(|v| {
                    let result = v.downcast::<T>();
                    *(result.unwrap())
                })
                .ok_or_else(|| {
                    lfail!(
                        failure::Mode::RuntimeLeemaFailure,
                        "resource missing",
                        "rsrc_id": ldisplay!(id),
                    )
                })
        } else {
            Err(lfail!(
                failure::Mode::CodeFailure,
                "take parameter is not a resource",
                "param": ldisplay!(p),
                "value": ldebug!(r.v),
                "params": ldebug!(self.params),
            ))
        }
    }

    pub fn get_param(&self, p: i8) -> Lresult<&Val>
    {
        self.params.get(p as usize).map(|i| &i.v).ok_or_else(|| {
            lfail!(
                failure::Mode::CodeFailure,
                "invalid param index",
                "param": ldisplay!(p),
            )
        })
    }
}

pub struct RsrcQueue
{
    rsrc_id: i64,
    rsrc: Option<Box<dyn Rsrc>>,
    queue: LinkedList<Iop>,
}

impl RsrcQueue
{
    pub fn new(rsrc_id: i64, resource: Box<dyn Rsrc>) -> RsrcQueue
    {
        RsrcQueue {
            rsrc_id,
            rsrc: Some(resource),
            queue: LinkedList::new(),
        }
    }

    /**
     * Push an Iop onto the queue
     *
     * If the resource is available, add the resource to the iop and return it
     * If the resource is already in use, then queue the iop and return None
     */
    pub fn push_iop(&mut self, mut iop: Iop) -> Option<Iop>
    {
        match self.rsrc.take() {
            Some(r) => {
                iop.add_rsrc(self.rsrc_id, r).unwrap();
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
    pub fn checkin(&mut self, r: Box<dyn Rsrc>)
        -> Option<(Iop, Box<dyn Rsrc>)>
    {
        match self.queue.pop_front() {
            Some(iop) => Some((iop, r)),
            None => {
                self.rsrc = Some(r);
                None
            }
        }
    }
}


#[derive(Clone)]
pub struct RunQueue
{
    app_send: Sender<AppMsg>,
}

#[derive(Debug)]
pub struct RunQueueReceiver(Receiver<Val>);

impl RunQueue
{
    pub fn spawn(&self, func: Fref, args: Struple2<Val>) -> RunQueueReceiver
    {
        let (result_send, result_recv) = channel();
        self.app_send
            .send(AppMsg::Spawn(result_send, func, args))
            .unwrap();
        RunQueueReceiver(result_recv)
    }
}

impl Future for RunQueueReceiver
{
    type Item = Val;
    type Error = Val;

    fn poll(&mut self) -> Poll<Val, Val>
    {
        match self.0.try_recv() {
            Ok(result) => Ok(Async::Ready(result)),
            Err(TryRecvError::Empty) => {
                task::current().notify();
                Ok(Async::NotReady)
            }
            Err(TryRecvError::Disconnected) => {
                println!("RunQueueReceiver disconnected");
                Ok(Async::Ready(Val::Str(Lstr::Sref(
                    "RunQueueReceiver disconnected",
                ))))
            }
        }
    }
}


pub struct Io
{
    resource: HashMap<i64, RsrcQueue>,
    next: LinkedList<Iop>,
    msg_rx: std::sync::mpsc::Receiver<IoMsg>,
    app_tx: std::sync::mpsc::Sender<AppMsg>,
    worker_tx: HashMap<i64, std::sync::mpsc::Sender<WorkerMsg>>,
    next_rsrc_id: i64,
    io: Option<Rc<RefCell<Io>>>,
    done: bool,
}

impl Io
{
    pub fn new(
        app_tx: Sender<AppMsg>,
        msg_rx: Receiver<IoMsg>,
        prog: program::Lib,
    ) -> Rc<RefCell<Io>>
    {
        let mut io = Io {
            resource: HashMap::new(),
            next: LinkedList::new(),
            msg_rx,
            app_tx,
            worker_tx: HashMap::new(),
            next_rsrc_id: rsrc::ID_INITIAL,
            io: None,
            done: false,
        };
        let libq = RsrcQueue::new(rsrc::ID_PROGLIB, Box::new(prog));
        io.resource.insert(rsrc::ID_PROGLIB, libq);
        let rcio = Rc::new(RefCell::new(io));
        let rcio2 = rcio.clone();
        rcio.borrow_mut().io = Some(rcio2);
        rcio
    }

    pub fn run_once(&mut self) -> Poll<MsgVal, MsgVal>
    {
        if let Ok(incoming) = self.msg_rx.try_recv() {
            self.handle_incoming(incoming);
        }
        if self.done {
            Ok(Async::Ready(MsgVal::new(&Val::Int(0))))
        } else {
            Ok(Async::NotReady)
        }
    }

    pub fn handle_incoming(&mut self, incoming: IoMsg)
    {
        match incoming {
            IoMsg::Iop {
                worker_id: wid,
                fiber_id: fid,
                action,
                params,
            } => {
                vout!("iop incoming: {:?}:{:?}: {:?}\n", wid, fid, params);
                let pvals: Struple2<Val> =
                    params.into_iter().map(|p| p.map_v(|v| v.take())).collect();
                self.handle_iop_action(wid, fid, action, pvals);
            }
            IoMsg::NewWorker(worker_id, worker_tx) => {
                self.worker_tx.insert(worker_id, worker_tx);
            }
            IoMsg::Done => {
                self.done = true;
            }
        }
    }

    fn handle_iop_action(
        &mut self,
        worker_id: i64,
        fiber_id: i64,
        action: rsrc::IopAction,
        params: Struple2<Val>,
    )
    {
        vout!(
            "handle_iop_action({},{},{:?})\n",
            worker_id,
            fiber_id,
            params
        );
        let iop = Iop::new(worker_id, fiber_id, action, params);
        self.push_iop(iop).unwrap();
    }

    fn push_iop(&mut self, mut iop: Iop) -> Lresult<()>
    {
        if let Some(rsrc_id) = iop.pop_rsrc_id() {
            if let Some(q) = self.resource.get_mut(&rsrc_id) {
                if let Some(iop2) = q.push_iop(iop) {
                    ltry!(self.push_iop(iop2));
                }
            } else {
                return Err(lfail!(
                    failure::Mode::RuntimeLeemaFailure,
                    "resourced does not exist",
                    "rsrc_id": ldisplay!(rsrc_id),
                ));
            }
        } else {
            self.next.push_back(iop);
        }
        Ok(())
    }

    fn run_iop(&mut self, rsrc_op: Option<(Iop, Box<dyn Rsrc>)>)
    {
        if let Some((iop, _rsrc)) = rsrc_op {
            vout!("run_iop\n");
            // TODO add the rsrc back to the iop?
            self.next.push_back(iop);
        }
    }

    pub fn take_next_iop(&mut self) -> Option<Iop>
    {
        self.next.pop_front()
    }

    pub fn handle_event(&mut self, worker_id: i64, fiber_id: i64, ev: Event)
    {
        match ev {
            Event::NewRsrc(rsrc) => {
                vout!("handle Event::NewRsrc\n");
                let new_rsrc_id = self.new_rsrc(rsrc);
                let result = Val::ResourceRef(new_rsrc_id);
                self.send_result(worker_id, fiber_id, result);
            }
            Event::ReturnRsrc(_rsrc) => {
                // delete this event? or make it return iop?
                vout!("handle Event::ReturnRsrc\n");
            }
            Event::Complete(_iop) => {}
            Event::DropRsrc => {
                // delete this event? should be able to derive it
                vout!("handle Event::DropRsrc\n");
                panic!("cannot drop rsrc with no id");
            }
            Event::Result(result) => {
                vout!("handle Event::Result\n");
                self.send_result(worker_id, fiber_id, result);
            }
            Event::FoundCode(fref, code) => {
                vout!("handle Event::FoundCode\n");
                let tx = self.worker_tx.get(&worker_id).unwrap();
                let msg =
                    WorkerMsg::FoundCode(fiber_id, MsgItem::new(&fref), code);
                tx.send(msg).expect("failed sending found code to worker");
            }
            Event::Future(libfut) => {
                vout!("handle Event::Future\n");
                let rcio: Rc<RefCell<Io>> = self.io.clone().unwrap();
                let rcio_err = rcio.clone();
                let iofut = libfut
                    .map(move |ev2| {
                        vout!("handle Event::Future ok\n");
                        let mut bio = rcio.borrow_mut();
                        bio.handle_event(worker_id, fiber_id, ev2);
                        ()
                    })
                    .map_err(move |ev2| {
                        vout!("handle Event::Future map_err\n");
                        let mut bio = rcio_err.borrow_mut();
                        bio.handle_event(worker_id, fiber_id, ev2);
                        ()
                    });
                vout!("spawn new future\n");
                TaskExecutor::current()
                    .spawn_local(Box::new(iofut))
                    .expect("spawn local failure");
            }
            Event::Stream(libstream) => {
                vout!("handle Event::Stream\n");
                let rcio: Rc<RefCell<Io>> = self.io.clone().unwrap();
                let rcio_err = rcio.clone();
                let iostream = libstream
                    .into_future()
                    .map(move |(ev2, _str2)| {
                        let mut bio = rcio.borrow_mut();
                        bio.handle_event(worker_id, fiber_id, ev2.unwrap());
                        ()
                    })
                    .map_err(move |(ev2, _str2)| {
                        let mut bio = rcio_err.borrow_mut();
                        bio.handle_event(worker_id, fiber_id, ev2);
                        ()
                    });
                vout!("spawn new stream\n");
                TaskExecutor::current()
                    .spawn_local(Box::new(iostream))
                    .expect("spawn local failure");
            }
            Event::Sequence(first, second) => {
                vout!("handle Event::Sequence\n");
                self.handle_event(worker_id, fiber_id, *first);
                self.handle_event(worker_id, fiber_id, *second);
            }
        }
    }

    fn create_iop_ctx<'a>(&'a mut self, iop: Iop) -> IopCtx
    {
        let rcio = self.io.clone().unwrap();
        let run_queue = RunQueue {
            app_send: self.app_tx.clone(),
        };
        IopCtx::new(rcio, iop, run_queue, self.next_rsrc_id)
    }

    pub fn new_rsrc(&mut self, rsrc: Box<dyn Rsrc>) -> i64
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
        tx.send(WorkerMsg::IopResult(fiber_id, MsgVal::new(&result)))
            .expect("failed sending iop result to worker");
    }

    pub fn return_rsrc(&mut self, rsrc_id: Option<i64>, rsrc: Box<dyn Rsrc>)
    {
        vout!("return_rsrc({:?})\n", rsrc_id);
        if rsrc_id.is_none() {
            panic!("cannot return resource without id");
        }
        let next_op = {
            let ioq = self.resource.get_mut(&rsrc_id.unwrap()).unwrap();
            ioq.checkin(rsrc)
        };
        self.run_iop(next_op);
    }
}

pub struct IoLoop
{
    io: Rc<RefCell<Io>>,
    did_nothing: u64,
}

impl IoLoop
{
    pub fn run(rcio: Rc<RefCell<Io>>)
    {
        let my_loop = IoLoop {
            io: rcio,
            did_nothing: 0,
        };

        let mut rt = current_thread::Runtime::new().unwrap();
        let result = rt.block_on(my_loop);
        println!("io is done: {:?}", result);
    }
}

impl Future for IoLoop
{
    type Item = MsgVal;
    type Error = MsgVal;

    fn poll(&mut self) -> Poll<MsgVal, MsgVal>
    {
        task::current().notify();
        let poll_result = self.io.borrow_mut().run_once();
        let opt_iop = self.io.borrow_mut().take_next_iop();
        if let Some(iop) = opt_iop {
            let worker_id = iop.src_worker_id;
            let fiber_id = iop.src_fiber_id;
            let action = iop.action;
            let ctx = self.io.borrow_mut().create_iop_ctx(iop);
            let ev = action(ctx);
            self.io.borrow_mut().handle_event(worker_id, fiber_id, ev);
            self.did_nothing = 0;
        } else {
            self.did_nothing = min(self.did_nothing + 1, 100_000);
            if self.did_nothing > 1000 {
                thread::sleep(Duration::from_micros(self.did_nothing));
            }
        }
        poll_result
    }
}


#[cfg(test)]
pub mod tests
{
    use crate::leema::io::{Io, IoLoop};
    use crate::leema::loader::Interloader;
    use crate::leema::lstr::Lstr;
    use crate::leema::msg;
    use crate::leema::program;
    use crate::leema::rsrc::{self, Rsrc};
    use crate::leema::struple::{Struple2, StrupleItem};
    use crate::leema::val::{MsgVal, Type, Val};

    use std::sync::mpsc;

    #[derive(Debug)]
    struct MockRsrc {}

    impl Rsrc for MockRsrc
    {
        fn get_type(&self) -> Type
        {
            user_type!("/foo/MockRsrc")
        }
    }

    fn empty_program() -> program::Lib
    {
        program::Lib::new(Interloader::default())
    }

    fn mock_iop_action(_ctx: rsrc::IopCtx) -> rsrc::Event
    {
        rsrc::Event::Result(Val::Int(8))
    }

    fn mock_rsrc_action(mut ctx: rsrc::IopCtx) -> rsrc::Event
    {
        let rsrc: MockRsrc = ctx.take_rsrc();
        rsrc::Event::seq(
            rsrc::Event::ReturnRsrc(Box::new(rsrc)),
            rsrc::Event::Result(Val::Int(18)),
        )
    }

    pub fn exercise_iop_action(
        action: rsrc::IopAction,
        params: Struple2<Val>,
    ) -> Result<(i64, Val), mpsc::TryRecvError>
    {
        let (msg_tx, msg_rx) = mpsc::channel::<msg::IoMsg>();
        let (app_tx, _) = mpsc::channel::<msg::AppMsg>();
        let (worker_tx, worker_rx) = mpsc::channel::<msg::WorkerMsg>();

        let rcio = Io::new(app_tx, msg_rx, empty_program());

        let msg_params = params
            .into_iter()
            .map(|p| p.map_v(|v| MsgVal::new(&v)))
            .collect();
        msg_tx.send(msg::IoMsg::NewWorker(11, worker_tx)).unwrap();
        msg_tx
            .send(msg::IoMsg::Iop {
                worker_id: 11,
                fiber_id: 21,
                action,
                params: msg_params,
            })
            .unwrap();
        msg_tx.send(msg::IoMsg::Done).unwrap();

        IoLoop::run(rcio);

        worker_rx.try_recv().map(|result_msg| {
            match result_msg {
                msg::WorkerMsg::IopResult(fiber_id, msg_val) => {
                    (fiber_id, msg_val.take())
                }
                _ => (0, Val::Str(Lstr::Sref("that didn't work"))),
            }
        })
    }

    #[test]
    fn test_io_constructor()
    {
        let (_, msg_rx) = mpsc::channel::<msg::IoMsg>();
        let (app_tx, _) = mpsc::channel::<msg::AppMsg>();
        // let worker_tx = HashMap::new();

        Io::new(app_tx, msg_rx, empty_program());
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

        let io = Io::new(app_tx, msg_rx, empty_program());
        let rsrc_id = io.borrow_mut().new_rsrc(Box::new(MockRsrc {}));
        let params =
            vec![StrupleItem::new_v(MsgVal::new(&Val::ResourceRef(rsrc_id)))];

        msg_tx.send(msg::IoMsg::NewWorker(8, worker_tx)).unwrap();
        msg_tx
            .send(msg::IoMsg::Iop {
                worker_id: 8,
                fiber_id: 7,
                action: mock_rsrc_action,
                params,
            })
            .unwrap();
        msg_tx.send(msg::IoMsg::Done).unwrap();
        IoLoop::run(io);

        let resp = worker_rx.try_recv();
        assert!(resp.is_ok());
    }
}
