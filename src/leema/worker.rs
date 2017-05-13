
use leema::code::{self, CodeKey, Code, Op, OpVec, ModSym, RustFunc};
use leema::frame::{self, Event, Frame, Parent};
use leema::fiber::{Fiber, FiberToWorkerMsg, WorkerToFiberMsg};
use leema::log;
use leema::reg::{Reg};
use leema::val::{Env, Val, MsgVal};

use std::collections::{HashMap, LinkedList};
use std::fmt;
use std::io::{stderr, Write};
use std::mem;
use std::rc::{Rc};
use std::sync::atomic::{AtomicBool, AtomicIsize, Ordering};
use std::sync::mpsc::{channel, Sender, Receiver};
use std::thread;
use std::time::{Duration};

use futures::{Poll, Async, Stream, Sink};
use futures::future::{Future};
use futures::task;
use futures::unsync;
use futures::unsync::mpsc;
use tokio_core::reactor;


#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Eq)]
#[derive(Hash)]
enum WaitType
{
    Code,
    IO,
}

#[derive(Debug)]
pub enum Msg
{
    // Spawn(module, function)
    Spawn(String, String),
    // RequestCode(worker_id, frame_id, module, function)
    RequestCode(i64, i64, String, String),
    // FoundCode(frame_id, module, function, code)
    FoundCode(i64, String, String, Code),
    MainResult(MsgVal),
}

struct FrameWait
{
    typ: WaitType,
    frame: Frame,
    code: Option<Rc<Code>>,
}

impl FrameWait
{
    fn code_request(f: Frame) -> FrameWait
    {
        FrameWait{
            typ: WaitType::Code,
            frame: f,
            code: None,
        }
    }
}

impl fmt::Debug for FrameWait
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        let code = match &self.code {
            &None => "None",
            &Some(ref icode) => icode.type_name(),
        };
        write!(f, "FrameWait {:?}\n\t{:?}\n{}\n\n", self.typ, self.frame, code)
    }
}

pub struct Worker
{
    fresh: LinkedList<(Rc<Code>, Frame)>,
    waiting: HashMap<i64, FrameWait>,
    handle: reactor::Handle,
    code: HashMap<String, HashMap<String, Rc<Code>>>,
    event: Event,
    app_tx: Sender<Msg>,
    app_rx: Receiver<Msg>,
    send_from_fiber: unsync::mpsc::Sender<FiberToWorkerMsg>,
    recv_from_fiber: unsync::mpsc::Receiver<FiberToWorkerMsg>,
    send_to_fiber: HashMap<i64, unsync::mpsc::Sender<WorkerToFiberMsg>>,
    //io: IOQueue,
    id: i64,
    next_fiber_id: i64,
    done: bool,
}

/**
 * main_loop
 *   get fresh/active frame
 *     iterate until !active
 *   push frame
 *   rotate
 */
impl Worker
{
    pub fn run(wid: i64, send: Sender<Msg>, recv: Receiver<Msg>)
    {
        let mut core = reactor::Core::new().unwrap();
        let h = core.handle();
        let (tx, rx) = unsync::mpsc::channel(1000);
        let w = Worker{
            fresh: LinkedList::new(),
            waiting: HashMap::new(),
            handle: h,
            code: HashMap::new(),
            event: Event::Uneventful,
            app_tx: send,
            app_rx: recv,
            send_from_fiber: tx,
            recv_from_fiber: rx,
            send_to_fiber: HashMap::new(),
            id: wid,
            next_fiber_id: 0,
            done: false,
        };
        let result = core.run(w).unwrap();
        println!("worker {} done with: {:?}", wid, result);
    }

    pub fn run_once(&mut self)
    {
        while let Result::Ok(msg) = self.app_rx.try_recv() {
            self.process_msg(msg);
        }

        while let Some((code, curf)) = self.pop_fresh() {
            self.execute_frame(code, curf);
        }
    }

    pub fn execute_frame(&mut self, code: Rc<Code>, mut curf: Frame)
    {
        match *code {
            Code::Leema(_) => {
                // moved to frame
            }
            Code::Rust(ref rf) => {
                rf(&mut curf);
                self.event = Event::Complete(true);
            }
            Code::RustIo(ref rf) => {
                rf(&mut curf, &self.handle);
                self.event = Event::Complete(true);
            }
        }
    }

    pub fn process_msg(&mut self, msg: Msg)
    {
        match msg {
            Msg::Spawn(module, call) => {
                vout!("worker call {}.{}()\n", module, call);
                self.spawn_fiber(module, call);
            }
            Msg::FoundCode(fiber_id, module, func, code) => {
                let rc_code = Rc::new(code);
                let mut new_mod = HashMap::new();
                new_mod.insert(func, rc_code.clone());
                self.code.insert(module, new_mod);
                let response = WorkerToFiberMsg::FoundCode(rc_code);
                let mut sender = self.send_to_fiber.get_mut(&fiber_id).unwrap();
                sender.start_send(response);
                sender.poll_complete();
            }
            _ => {
                panic!("Must be a message for the app: {:?}", msg);
            }
        }
    }

    pub fn spawn_fiber(&mut self, module: String, func: String)
    {
        let id = self.next_fiber_id;
        self.next_fiber_id += 1;
        let frame = Frame::new_root(module, func);
        let (send_to_fiber, recv_from_worker) =
            unsync::mpsc::channel::<WorkerToFiberMsg>(10);
        self.send_to_fiber.insert(id, send_to_fiber);
        Fiber::spawn(&self.handle, id, frame
            , self.send_from_fiber.clone(), recv_from_worker);
    }

    fn pop_fresh(&mut self) -> Option<(Rc<Code>, Frame)>
    {
        self.fresh.pop_front()
    }

    fn push_fresh(&mut self, code: Rc<Code>, f: Frame)
    {
        self.fresh.push_back((code, f))
    }

    fn add_fork(&mut self, key: &CodeKey, newf: Frame)
    {
vout!("lock app, add_fork\n");
    }

    fn process_msg_from_fiber(&mut self, msg: FiberToWorkerMsg)
    {
        match msg {
            FiberToWorkerMsg::RequestCode(fid, module, func) => {
                let mut found = false;
                if let Some(msrc) = self.code.get(&module) {
                    if let Some(mfunc) = msrc.get(&func) {
                        let response =
                            WorkerToFiberMsg::FoundCode(mfunc.clone());
                        let mut sender =
                            self.send_to_fiber.get_mut(&fid).unwrap();
                        sender.start_send(response);
                        sender.poll_complete();
                        found = true;
                    }
                }
                if !found {
                    let msg = Msg::RequestCode(self.id, fid, module, func);
                    self.app_tx.send(msg);
                }
            }
            FiberToWorkerMsg::MainResult(res) => {
                println!("whoa main finished! {:?}", res);
            }
        }
    }
}

// struct Iterate { w: Worker }
// struct RecvFromFiber { f: Fiber }

impl Future for Worker
{
    type Item = Val;
    type Error = Val;

    fn poll(&mut self) -> Poll<Val, Val>
    {
        loop {
            match self.recv_from_fiber.poll() {
                Result::Ok(Async::NotReady) => {
                    // println!("recv_from_fiber(Async::NotReady)");
                    break;
                }
                Result::Ok(Async::Ready(None)) => {
                    println!("recv_from_fiber(Async::Ready(None))");
                    break;
                }
                Result::Ok(Async::Ready(Some(msg))) => {
                    println!("received msg: {:?}", msg);
                    self.process_msg_from_fiber(msg);
                }
                Result::Err(err) => {
                    println!("recv_from_fiber err: {:?}", err);
                }
            }
        }

        self.run_once();
        thread::yield_now();

        let tp = task::park();
        let t = reactor::Timeout::new(Duration::new(0, 100000), &self.handle)
            .unwrap()
            .map(move |fut| {
                tp.unpark();
            })
            .map_err(|_| {
                () // Val::new_str("timeout error".to_string())
            });
        self.handle.spawn(t);
        Result::Ok(Async::NotReady)
    }
}
