
use leema::code::{self, CodeKey, Code, Op, OpVec, ModSym, RustFunc};
use leema::frame::{self, Event, Frame, Parent};
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
        let w = Worker{
            fresh: LinkedList::new(),
            waiting: HashMap::new(),
            handle: h,
            code: HashMap::new(),
            event: Event::Uneventful,
            app_tx: send,
            app_rx: recv,
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
}

// struct Iterate { w: Worker }
// struct RecvFromFiber { f: Fiber }

impl Future for Worker
{
    type Item = Val;
    type Error = Val;

    fn poll(&mut self) -> Poll<Val, Val>
    {
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
