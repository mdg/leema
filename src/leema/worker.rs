
use leema::code::{self, CodeKey, Code, Op, OpVec, ModSym, RustFunc};
use leema::fiber::{Fiber};
use leema::frame::{self, Event, Frame, Parent};
use leema::log;
use leema::reg::{Reg};
use leema::val::{Env, Val, MsgVal};

use std::cell::{RefCell, RefMut};
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

#[derive(Debug)]
enum ReadyFiber
{
    New(Fiber),
    Ready(Fiber, Rc<Code>),
}

#[derive(Debug)]
enum FiberWait
{
    CodeWait(Fiber),
    IoWait(Fiber, Rc<Code>),
    FutureWait(Fiber, Rc<Code>),
}


pub struct Worker
{
    fresh: LinkedList<ReadyFiber>,
    waiting: HashMap<i64, FiberWait>,
    handle: reactor::Handle,
    code: HashMap<String, HashMap<String, Rc<Code>>>,
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
            handle: h.clone(),
            code: HashMap::new(),
            app_tx: send,
            app_rx: recv,
            id: wid,
            next_fiber_id: 0,
            done: false,
        };
        let wexec = WorkerExec{
            w: Rc::new(RefCell::new(w)),
            h: h,
        };
        let result = core.run(wexec).unwrap();
        println!("worker {} done with: {:?}", wid, result);
    }

    fn load_code(&mut self, curf: Fiber)
    {
        let msg = Msg::RequestCode(self.id, curf.fiber_id
            , curf.module_name().to_string()
            , curf.function_name().to_string());
        self.app_tx.send(msg);
    }

    pub fn handle_event(w: &Rc<RefCell<Worker>>, e: Event, mut f: Fiber
        , code: Rc<Code>)
    {
        match e {
            Event::Complete(success) => {
                if success {
                    // analyze successful function run
                } else {
                    vout!("function call failed\n");
                }
                vout!("function is complete\n");
                let parent = f.head.take_parent();
                match parent {
                    Parent::Caller(old_code, mut pf, dst) => {
                        pf.pc += 1;
                        f.head = *pf;
                    }
                    Parent::Repl(res) => {
                    }
                    Parent::Main(res) => {
                    }
                    Parent::Null => {
                        // this shouldn't have happened
                    }
                }
            }
            Event::Call(dst, module, func, args) => {
                f.push_call(code.clone(), dst, module, func, args);
            }
            Event::FutureWait(reg) => {
                println!("wait for future {:?}", reg);
            }
            Event::IOWait => {
                println!("do I/O");
            }
            Event::Fork => {
                // self.fresh.push_back((code, curf));
                // end this iteration,
            }
            Event::Uneventful => {
                println!("We shouldn't be here with uneventful");
                panic!("code: {:?}, pc: {:?}", code, f.head.pc);
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
        let fib = Fiber::spawn(id, frame, &self.handle);
        self.fresh.push_back(ReadyFiber::New(fib));
    }

    fn pop_fresh(&mut self) -> Option<ReadyFiber>
    {
        self.fresh.pop_front()
    }

    fn push_fresh(&mut self, f: ReadyFiber)
    {
        self.fresh.push_back(f)
    }

    fn add_fork(&mut self, key: &CodeKey, newf: Frame)
    {
vout!("lock app, add_fork\n");
    }
}


struct WorkerExec
{
    w: Rc<RefCell<Worker>>,
    h: reactor::Handle,
}

impl WorkerExec
{
    pub fn run_once(&mut self)
    {
        RefMut::map(self.w.borrow_mut(), |wref| {
            while let Result::Ok(msg) = wref.app_rx.try_recv() {
                wref.process_msg(msg);
            }
            wref
        });

        while let Some(readyf) = self.w.borrow_mut().pop_fresh() {
            match readyf {
                ReadyFiber::New(f) => {
                    self.w.borrow_mut().load_code(f);
                }
                ReadyFiber::Ready(mut f, code) => {
                    let ev = f.head.execute_frame(&code);
                    Worker::handle_event(&self.w, ev, f, code);
                }
            }
        }
    }
}

impl Future for WorkerExec
{
    type Item = Val;
    type Error = Val;

    fn poll(&mut self) -> Poll<Val, Val>
    {
        task::park().unpark();
        self.run_once();
        thread::yield_now();

        /*
        let t = reactor::Timeout::new(Duration::new(0, 100000), &self.h)
            .unwrap()
            .map(move |fut| {
            })
            .map_err(|_| {
                () // Val::new_str("timeout error".to_string())
            });
        self.h.spawn(t);
        */
        Result::Ok(Async::NotReady)
    }
}
