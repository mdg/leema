
use leema::code::{self, CodeKey, Code, CodeMap, Op, OpVec, ModSym, RustFunc};
use leema::frame::{self, Event, Frame, Parent};
use leema::log;
use leema::msg::{Msg};
use leema::reg::{Reg};
use leema::val::{Env};

use std::collections::{HashMap, LinkedList};
use std::io::{stderr, Write};
use std::mem;
use std::sync::atomic::{AtomicBool, AtomicIsize, Ordering};
use std::sync::mpsc::{channel, Sender, Receiver};
use std::thread;


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
#[derive(PartialEq)]
#[derive(Eq)]
#[derive(Hash)]
struct Wait
{
    typ: WaitType,
    frame_id: i64,
}

impl Wait
{
    fn new(typ: WaitType, f: i64) -> Wait
    {
        Wait{
            typ: typ,
            frame_id: f,
        }
    }
}

pub struct Worker
{
    fresh: LinkedList<(Code, Frame)>,
    waiting: HashMap<Wait, Frame>,
    code: HashMap<String, HashMap<String, Code>>,
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
        Worker{
            fresh: LinkedList::new(),
            waiting: HashMap::new(),
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
        vout!("iterate worker {}\n", self.id);
        while let Result::Ok(msg) = self.rx.try_recv() {
            self.process_msg(msg);
        }

        while let Some((code, curf)) = self.pop_fresh() {
            self.execute_frame(code, curf);
        }
    }

    pub fn execute_frame(&mut self, code: Code, mut curf: Frame)
    {
        match code {
            Code::Leema(ref ops) => {
                self.execute_leema_frame(&mut curf, ops);
            }
            Code::Rust(ref rf) => {
                rf(&mut curf);
                self.event = Event::Complete(true);
            }
            Code::Inter(ref ix) => {
                panic!("cannot execute partial code");
            }
        }
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
            Msg::FoundCode(id, module, func, code) => {
                vout!("found code for frame: {}\n", id);
                let dupe_code = code.clone();
                let mut new_mod = HashMap::new();
                new_mod.insert(func, code);
                self.code.insert(module, new_mod);
                let wait = Wait::new(WaitType::Code, id);
                let frame = self.waiting.remove(&wait).unwrap();
                self.fresh.push_back((dupe_code, frame));
            }
            _ => {
                panic!("Must be a message for the app: {:?}", msg);
            }
        }
    }

    pub fn create_root_frame(&mut self, module: String, func: String)
    {
        let opt_code = match self.code.get(&module) {
            Some(ref m) => {
                match m.get(&func) {
                    Some(ref code) => Some((*code).clone()),
                    None => None,
                }
            }
            None => None,
        };
        let id = self.next_frame_id;
        self.next_frame_id += 1;
        let env = Env::new();
        let frame = Frame::new_root(id, env);
        match opt_code {
            Some(code) => {
                vout!("make new frame with {}.{}\n", module, func);
                self.fresh.push_back((code, frame));
            }
            None => {
                vout!("lookup code in application {}.{}\n", module, func);
                self.request_code(frame, module, func);
            }
        }
    }

    fn request_code(&mut self, f: Frame, mname: String, fname: String)
    {
        let frame_id = f.id;
        let wait = Wait::new(WaitType::Code, frame_id);
        self.waiting.insert(wait, f);
        self.tx.send(Msg::RequestCode(self.id, frame_id, mname, fname));
    }

    fn pop_fresh(&mut self) -> Option<(Code, Frame)>
    {
        self.fresh.pop_front()
    }

    fn push_fresh(&mut self, code: Code, f: Frame)
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
            &Op::MatchPattern(jmp, ref patt, ref input) => {
                curf.execute_match_pattern(jmp, patt, input);
            }
            &Op::ListCons(ref dst, ref src) => {
                frame::execute_list_cons(curf, dst, src);
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
                frame::execute_call(curf, dst, func, args);
            }
            &Op::Return => {
                self.event = Event::Complete(true);
            }
            &Op::SetResult(ref dst) => {
                if dst == &Reg::Void {
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

    pub fn handle_event(&mut self, code: Code, mut curf: Frame)
    {
        match self.take_event() {
            Event::Complete(success) => {
                if success {
                    // analyze successful function run
                } else {
                    vout!("function call failed\n");
                }
                match curf.parent {
                    Parent::Caller(dst, code, mut pf) => {
                        self.fresh.push_back((code, *pf));
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
                        {
vout!("lock app, main done in iterate\n");
                            // let mut _app = self.app.lock().unwrap();
                            self.done = true;
                        }
                        //self.notify_done();
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
                        tx.send(r);
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
            Event::Call(dst, ch_code, mut ch_frame) => {
                ch_frame.parent = Parent::Caller(
                    dst,
                    code,
                    Box::new(curf),
                );
                self.fresh.push_back((ch_code, ch_frame));
            }
            Event::FutureWait(reg) => {
                println!("wait for future {:?}", reg);
            }
            Event::IOWait => {
                println!("do I/O");
            }
            Event::Fork => {
                self.fresh.push_back((code, curf));
                // end this iteration,
            }
            Event::Uneventful => {
                panic!("We shouldn't be here with uneventful");
            }
        }
    }
}
