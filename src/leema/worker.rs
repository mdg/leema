
use leema::code::{self, CodeKey, Code, Op, OpVec, ModSym, RustFunc};
use leema::frame::{self, Event, Frame, Parent};
use leema::log;
use leema::msg::{Msg};
use leema::reg::{Reg};
use leema::val::{Env};

use std::collections::{HashMap, LinkedList};
use std::fmt;
use std::io::{stderr, Write};
use std::mem;
use std::rc::{Rc};
use std::sync::atomic::{AtomicBool, AtomicIsize, Ordering};
use std::sync::mpsc::{channel, Sender, Receiver};
use std::thread;

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
    core: reactor::Core,
    handle: reactor::Handle,
    code: HashMap<String, HashMap<String, Rc<Code>>>,
    event: Event,
    tx: Sender<Msg>,
    rx: Receiver<Msg>,
    //io: IOQueue,
    id: i64,
    next_frame_id: i64,
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
    pub fn new(wid: i64, send: Sender<Msg>, recv: Receiver<Msg>) -> Worker
    {
        let c = reactor::Core::new().unwrap();
        let h = c.handle();
        Worker{
            fresh: LinkedList::new(),
            waiting: HashMap::new(),
            core: c,
            handle: h,
            code: HashMap::new(),
            event: Event::Uneventful,
            tx: send,
            rx: recv,
            id: wid,
            next_frame_id: 0,
            done: false,
        }
    }

    pub fn take_event(&mut self) -> Event
    {
        let mut e = Event::Uneventful;
        mem::swap(&mut e, &mut self.event);
        e
    }

    pub fn run(&mut self)
    {
        while !self.done {
            self.run_once();
            thread::yield_now();
        }
    }

    pub fn run_once(&mut self)
    {
        while let Result::Ok(msg) = self.rx.try_recv() {
            self.process_msg(msg);
        }

        while let Some((code, curf)) = self.pop_fresh() {
            self.execute_frame(code, curf);
        }
    }

    pub fn execute_frame(&mut self, code: Rc<Code>, mut curf: Frame)
    {
        match *code {
            Code::Leema(ref ops) => {
                self.execute_leema_frame(&mut curf, ops);
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
        self.handle_event(code, curf);
    }

    fn execute_leema_frame(&mut self, curf: &mut Frame, ops: &OpVec)
    {
        while let Event::Uneventful = self.event {
            self.execute_leema_op(curf, ops);
        }
    }

    pub fn process_msg(&mut self, msg: Msg)
    {
        match msg {
            Msg::Call(module, call) => {
                vout!("worker call {}.{}()\n", module, call);
                self.create_root_frame(module, call);
            }
            Msg::FoundCode(frame_id, module, func, code) => {
                vout!("found code for frame: {} <- {:?}\n", frame_id, code);
                let rc_code = Rc::new(code);
                let mut new_mod = HashMap::new();
                new_mod.insert(func, rc_code.clone());
                self.code.insert(module, new_mod);
                let fwait = self.waiting.remove(&frame_id).unwrap();
                vout!("frame is: {:?}\n", fwait);
                self.fresh.push_back((rc_code.clone(), fwait.frame));
            }
            _ => {
                panic!("Must be a message for the app: {:?}", msg);
            }
        }
    }

    pub fn load_frame(&mut self, frame: Frame)
    {
        let opt_code = match self.code.get(frame.module_name()) {
            Some(ref m) => {
                match m.get(frame.function_name()) {
                    Some(ref code) => {
                        Some((**code).clone())
                    }
                    None => None,
                }
            }
            None => None,
        };
        match opt_code {
            Some(code) => {
                self.fresh.push_back((code, frame));
            }
            None => {
                vout!("lookup code in application {}.{}\n"
                    , frame.module_name(), frame.function_name());
                self.request_code(frame);
            }
        }
    }

    pub fn create_root_frame(&mut self, module: String, func: String)
    {
        let id = self.next_frame_id;
        self.next_frame_id += 1;
        self.load_frame(Frame::new_root(id, module, func));
    }

    fn request_code(&mut self, f: Frame)
    {
        let frame_id = f.id;
        let msg = Msg::RequestCode(self.id, frame_id
            , f.module_name().to_string()
            , f.function_name().to_string()
        );
        let wait = FrameWait::code_request(f);
        self.waiting.insert(frame_id, wait);
        self.tx.send(msg);
    }

    fn pop_fresh(&mut self) -> Option<(Rc<Code>, Frame)>
    {
        self.fresh.pop_front()
    }

    fn push_fresh(&mut self, code: Rc<Code>, f: Frame)
    {
        self.fresh.push_back((code, f))
    }

    pub fn execute_leema_op(&mut self, curf: &mut Frame, ops: &OpVec)
    {
        let op = ops.get(curf.pc as usize).unwrap();
        vout!("exec: {:?}\n", op);
        match op {
            &Op::ConstVal(ref dst, ref v) => {
                frame::execute_const_val(curf, dst, v);
            }
            &Op::Constructor(ref dst, ref typ) => {
                frame::execute_constructor(curf, dst, typ);
            }
            &Op::Copy(ref dst, ref src) => {
                frame::execute_copy(curf, dst, src);
            }
            &Op::Fork(ref dst, ref freg, ref args) => {
                // frame::execute_fork(self, curf, dst, freg, args);
            }
            &Op::Jump(jmp) => {
                frame::execute_jump(curf, jmp);
            }
            &Op::JumpIfNot(jmp, ref reg) => {
                frame::execute_jump_if_not(curf, jmp, reg);
            }
            &Op::MatchPattern(ref dst, ref patt, ref input) => {
                curf.execute_match_pattern(dst, patt, input);
            }
            &Op::ListCons(ref dst, ref head, ref tail) => {
                frame::execute_list_cons(curf, dst, head, tail);
            }
            &Op::ListCreate(ref dst) => {
                frame::execute_list_create(curf, dst);
            }
            &Op::TupleCreate(ref dst, ref sz) => {
                frame::execute_tuple_create(curf, dst, *sz);
            }
            &Op::StrCat(ref dst, ref src) => {
                self.event = curf.execute_strcat(dst, src);
            }
            &Op::LoadFunc(ref reg, ref modsym) => {
                frame::execute_load_func(curf, reg, modsym);
            }
            &Op::ApplyFunc(ref dst, ref func, ref args) => {
                self.event = frame::execute_call(curf, dst, func, args);
            }
            &Op::Return => {
                self.event = Event::Complete(true);
            }
            &Op::SetResult(ref dst) => {
                if *dst == Reg::Void {
                    panic!("return void at {} in {:?}", curf.pc, ops);
                }
                curf.parent.set_result(curf.e.get_reg(dst).clone());
                curf.pc += 1;
            }
            &Op::Failure(ref dst, ref tag, ref msg) => {
                curf.execute_failure(dst, tag, msg);
            }
        }
    }

    fn add_fork(&mut self, key: &CodeKey, newf: Frame)
    {
vout!("lock app, add_fork\n");
    }

    pub fn handle_event(&mut self, code: Rc<Code>, mut curf: Frame)
    {
        match self.take_event() {
            Event::Complete(success) => {
                if success {
                    // analyze successful function run
                } else {
                    vout!("function call failed\n");
                }
                vout!("function is complete: {:?}\n", curf.parent);
                match curf.parent {
                    Parent::Caller(old_code, mut pf, dst) => {
                        pf.pc += 1;
                        self.fresh.push_back((old_code, *pf));
                    }
                    Parent::Repl(res) => {
                        /*
vout!("lock app, repl done in iterate\n");
                        let mut _app = self.app.lock().unwrap();
                        _app.set_result(res);
                        */
                    }
                    Parent::Main(res) => {
vout!("finished main func\n");
                        self.tx.send(Msg::MainResult(res.to_msg()));
                        self.done = true;
                    }
                    Parent::Fork(mut ready, mut tx) => {
                        println!("finished a fork!");
                        /*
                        // still need to pass the
                        // result from the fork
                        // to the parent
                        */
                        let r = curf.e.takeResult();
                        println!("send({:?})", r);
                        tx.send(Msg::MainResult(r.to_msg()));
                        ready.store(
                            true,
                            Ordering::Relaxed,
                        );
                    }
                    Parent::Null => {
                        // this shouldn't have happened
                    }
                }
            }
            Event::Call(dst, module, func, args) => {
                let newf =
                    Frame::push_call(code, curf, dst, module, func, args);
                self.load_frame(newf);
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
                panic!("We shouldn't be here with uneventful");
            }
        }
    }
}
