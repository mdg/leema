// use crate::leema::lstr::Lstr;
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
use std::sync::atomic::AtomicI64;
use std::sync::mpsc::{channel, Receiver, SyncSender};

use tokio::runtime;

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

/// Maybe just make this a UUID?
static NEXT_RSRC_ID: AtomicI64 = AtomicI64::new(0);

pub struct Iop
{
    action: rsrc::IopAction,
    ctx: IopCtx,
    src_worker_id: i64,
    src_fiber_id: i64,
    rsrc_id: Option<i64>,
    required_rsrc_ids: Vec<i64>,
    rsrc: HashMap<i64, Box<dyn Rsrc>>,
    result: Val,
}

impl Iop
{
    /// take the params, extract the rsrc_ids, sort them into required_rsrc_ids
    pub fn new() {}

    pub fn next_required_rsrc(&mut self) -> Option<i64>
    {
        self.required_rsrc_ids.pop()
    }

    pub fn push_rsrc(&mut self, rsrc_id: i64, rsrc: Box<dyn Rsrc>)
    {
        self.rsrc.insert(rsrc_id, rsrc);
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
    pub fn checkin(&mut self, r: Box<dyn Rsrc>) -> Option<Iop>
    {
        match self.queue.pop_front() {
            Some(mut iop) => {
                iop.push_rsrc(self.rsrc_id, r);
                Some(iop)
            }
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
    app_send: SyncSender<AppMsg>,
}

/// why is this in the io code?
#[derive(Debug)]
pub struct RunQueueReceiver(Receiver<Val>);

impl RunQueue
{
    pub fn spawn(&self, _func: Fref, _args: Struple2<Val>) -> RunQueueReceiver
    {
        let (_result_send, result_recv) = channel();
        /*
        self.app_send
            .send(AppMsg::Spawn(result_send, func, args))
            .unwrap();
            */
        RunQueueReceiver(result_recv)
    }
}

/*
impl Future for RunQueueReceiver
{
    type Output = Val;

    fn poll(&mut self, _ctx: &mut Context<'_>) -> Poll<Val>
    {
        match self.0.try_recv() {
            Ok(result) => Poll::Ready(result),
            Err(TryRecvError::Empty) => {
                Poll::Pending
            }
            Err(TryRecvError::Disconnected) => {
                println!("RunQueueReceiver disconnected");
                Poll::Ready(Val::Str(Lstr::Sref(
                    "RunQueueReceiver disconnected",
                )))
            }
        }
    }
}
*/


pub struct Io
{
    resource: HashMap<i64, RsrcQueue>,
    next: LinkedList<Iop>,
    msg_rx: std::sync::mpsc::Receiver<IoMsg>,
    app_tx: std::sync::mpsc::SyncSender<AppMsg>,
    worker_tx: HashMap<i64, std::sync::mpsc::SyncSender<WorkerMsg>>,
    next_rsrc_id: i64,
    io: Option<Rc<RefCell<Io>>>,
    done: bool,
}

impl Io
{
    pub fn new(
        app_tx: SyncSender<AppMsg>,
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

    pub async fn run_once(&mut self)
    {
        if let Ok(incoming) = self.msg_rx.try_recv() {
            self.handle_incoming(incoming);
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
                vout!("iop incoming: {:?}:{:?}:{:?}\n", wid, fid, params);
                let param_vals = params.take();
                self.handle_iop_action(wid, fid, action, param_vals);
            }
            IoMsg::Call {
                worker_id,
                fiber_id,
                f,
                params,
            } => {
                vout!(
                    "io call: {}:{}:{:?} {:?}\n",
                    worker_id,
                    fiber_id,
                    f,
                    params
                );
            }
            IoMsg::NewWorker(worker_id, worker_tx) => {
                vout!("add worker send channel {}\n", worker_id);
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
        params: Val,
    )
    {
        vout!(
            "handle_iop_action({},{},{:?})\n",
            worker_id,
            fiber_id,
            params
        );
        let ctx = self.create_iop_ctx(worker_id, fiber_id, None, params);
        let iop = Iop {
            ctx,
            action,
            src_worker_id: worker_id,
            src_fiber_id: fiber_id,
            rsrc_id: None,
            required_rsrc_ids: vec![],
            result: Val::VOID,
            rsrc: HashMap::new(),
        };
        self.push_iop(iop);
    }

    fn push_iop(&mut self, mut iop: Iop)
    {
        match iop.next_required_rsrc() {
            None => {
                self.next.push_back(iop);
            }
            Some(rsrc_id) => {
                let opt_rsrc_op = {
                    let opt_rsrcq = self.resource.get_mut(&rsrc_id);
                    if opt_rsrcq.is_none() {
                        panic!("missing queue for rsrc: {}", rsrc_id);
                    }
                    let rsrcq = opt_rsrcq.unwrap();
                    rsrcq.push_iop(iop)
                };
                if let Some(rsrc_op) = opt_rsrc_op {
                    // recurse in case rsrc_op needs more resources
                    self.push_iop(rsrc_op);
                }
            }
        }
    }

    pub fn take_next_iop(&mut self) -> Option<Iop>
    {
        self.next.pop_front()
    }

    pub fn handle_event(
        &mut self,
        worker_id: i64,
        fiber_id: i64,
        rsrc_id: Option<i64>,
        ev: Event,
    )
    {
        match ev {
            Event::Complete(ctx) => {
                self.send_result(worker_id, fiber_id, ctx.get_result().clone());
            }
            Event::NewRsrc(rsrc) => {
                vout!("handle Event::NewRsrc\n");
                let new_rsrc_id = self.new_rsrc(rsrc);
                let result = Val::ResourceRef(new_rsrc_id);
                self.send_result(worker_id, fiber_id, result);
            }
            Event::ReturnRsrc(rsrc) => {
                vout!("handle Event::ReturnRsrc\n");
                self.return_rsrc(rsrc_id, rsrc);
            }
            Event::DropRsrc => {
                vout!("handle Event::DropRsrc\n");
                if rsrc_id.is_none() {
                    panic!("cannot drop rsrc with no id");
                }
                self.resource.remove(&rsrc_id.unwrap());
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
            Event::Future(_libfut) => {
                vout!("handle Event::Future\n");
                /*
                let rcio: Rc<RefCell<Io>> = self.io.clone().unwrap();
                let rcio_err = rcio.clone();
                let iofut = libfut
                    .map(move |ev2| {
                        vout!("handle Event::Future ok\n");
                        let mut bio = rcio.borrow_mut();
                        bio.handle_event(worker_id, fiber_id, rsrc_id, ev2);
                        ()
                    })
                    .map_err(move |ev2| {
                        vout!("handle Event::Future map_err\n");
                        let mut bio = rcio_err.borrow_mut();
                        bio.handle_event(worker_id, fiber_id, rsrc_id, ev2);
                        ()
                    });
                vout!("spawn new future\n");
                TaskExecutor::current()
                    .spawn_local(Box::new(iofut))
                    .expect("spawn local failure");
                    */
            }
            /*
            Event::Stream(libstream) => {
                vout!("handle Event::Stream\n");
                let rcio: Rc<RefCell<Io>> = self.io.clone().unwrap();
                let rcio_err = rcio.clone();
                let iostream = libstream
                    .into_future()
                    .map(move |(ev2, _str2)| {
                        let mut bio = rcio.borrow_mut();
                        bio.handle_event(
                            worker_id,
                            fiber_id,
                            rsrc_id,
                            ev2.unwrap(),
                        );
                        ()
                    })
                    .map_err(move |(ev2, _str2)| {
                        let mut bio = rcio_err.borrow_mut();
                        bio.handle_event(worker_id, fiber_id, rsrc_id, ev2);
                        ()
                    });
                vout!("spawn new stream\n");
                TaskExecutor::current()
                    .spawn_local(Box::new(iostream))
                    .expect("spawn local failure");
            }
            */
            Event::Sequence(first, second) => {
                vout!("handle Event::Sequence\n");
                self.handle_event(worker_id, fiber_id, rsrc_id, *first);
                self.handle_event(worker_id, fiber_id, rsrc_id, *second);
            }
        }
    }

    fn create_iop_ctx<'a>(
        &'a mut self,
        src_worker_id: i64,
        src_fiber_id: i64,
        rsrc_id: Option<i64>,
        param_val: Val,
    ) -> IopCtx
    {
        let rcio = self.io.clone().unwrap();
        IopCtx::new(rcio, src_worker_id, src_fiber_id, rsrc_id, param_val)
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
        if let Some(op) = next_op {
            self.push_iop(op);
        }
    }
}

pub struct IoLoop
{
    io: Rc<RefCell<Io>>,
    did_nothing: u64,
    done: bool,
}

impl IoLoop
{
    pub fn run(rcio: Rc<RefCell<Io>>)
    {
        let mut my_loop = IoLoop {
            io: rcio,
            did_nothing: 0,
            done: false,
        };

        let rt = runtime::Builder::new_current_thread()
            .thread_name("leema-io")
            .enable_all()
            .build()
            .unwrap();
        rt.block_on(async move {
            while my_loop.done {
                my_loop.iterate().await;
            }
        });
    }

    async fn iterate(&mut self)
    {
        vout!("IoLoop iterate\n");
        self.io.borrow_mut().run_once().await;
        let opt_iop = self.io.borrow_mut().take_next_iop();
        if let Some(iop) = opt_iop {
            let _ctx = (iop.action)(iop.ctx);
            self.io.borrow_mut().handle_event(
                iop.src_worker_id,
                iop.src_fiber_id,
                iop.rsrc_id,
                rsrc::Event::Result(Val::VOID),
            );
            self.did_nothing = 0;
        } else {
            self.did_nothing = min(self.did_nothing + 1, 100_000);
            if self.did_nothing > 1000 {
                tokio::task::yield_now().await;
            }
        }
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
    use crate::leema::struple::Struple2;
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

        let msg_params = MsgVal::new(&Val::Tuple(params));
        msg_tx.send(msg::IoMsg::NewWorker(11, worker_tx)).unwrap();
        msg_tx
            .send(msg::IoMsg::Iop {
                worker_id: 11,
                fiber_id: 21,
                action,
                rsrc_id: None,
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

        msg_tx.send(msg::IoMsg::NewWorker(8, worker_tx)).unwrap();
        msg_tx
            .send(msg::IoMsg::Iop {
                worker_id: 8,
                fiber_id: 7,
                action: mock_rsrc_action,
                rsrc_id: Some(rsrc_id),
                params: MsgVal::new(&Val::empty_tuple()),
            })
            .unwrap();
        msg_tx.send(msg::IoMsg::Done).unwrap();
        IoLoop::run(io);

        let resp = worker_rx.try_recv();
        assert!(resp.is_ok());
    }
}
