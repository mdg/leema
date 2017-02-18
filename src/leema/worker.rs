
use leema::code::{self, CodeKey, Code, CodeMap, Op, OpVec, ModSym, RustFunc};
use leema::frame::{self, Event, Frame, Parent};
use leema::log;
use leema::msg::{Msg};
use leema::reg::{Reg};

use std::collections::{HashMap, LinkedList};
use std::io::{stderr, Write};
use std::mem;
use std::sync::atomic::{AtomicBool, AtomicIsize, Ordering};
use std::sync::mpsc::{channel, Sender, Receiver};
use std::thread;


pub struct Worker
{
    fresh: LinkedList<(Code, Frame)>,
    code: HashMap<String, HashMap<String, Code>>,
    event: Event,
    tx: Sender<Msg>,
    rx: Receiver<Msg>,
    //io: IOQueue,
    worker_id: i64,
    code_request_idx: u64,
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
            code: HashMap::new(),
            event: Event::Uneventful,
            tx: send,
            rx: recv,
            worker_id: wid,
            code_request_idx: 0,
            done: false,
        }
    }

    pub fn take_event(&mut self) -> Event
    {
        let mut e = Event::Uneventful;
        mem::swap(&mut e, &mut self.event);
        e
    }

    pub fn pop_fresh(&mut self) -> Option<(Code, Frame)>
    {
        self.fresh.pop_front()
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
        vout!("iterate worker {}\n", self.worker_id);
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
        vout!("received message: {:?}\n", msg);
        match msg {
            Msg::Call(module, call) => {
                vout!("worker call {}.{}()\n", module, call);
                self.create_frame(module, call);
            }
            _ => {
                panic!("Must be a message for the app: {:?}", msg);
            }
        }
    }

    pub fn create_frame(&mut self, module: String, func: String)
    {
        let m = self.code.get(&module);
        let code = if m.is_some() {
            m.unwrap().get(&func)
        } else {
            None
        };
        if code.is_some() {
            vout!("make new frame with {}.{}\n", module, func);
        } else {
            vout!("lookup code in application {}.{}\n", module, func);
            self.tx.send(Msg::RequestCode(module, func));
        }
        //push_frame(get_code(module, func))
    }

    /*
    pub fn get_code(&mut self, module: &str, func: &str)
    {
        c = self.code.find(module, func);
        if c.is_none() {
            i = self.new_code_request();
            c = self.app_channel.push(CodeRequest(i, module, func));
            frame.wait_on_code(i)
        }
        c
    }
    */

    pub fn new_code_request(&mut self) -> u64
    {
        let idx = self.code_request_idx;
        self.code_request_idx += 1;
        idx
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
