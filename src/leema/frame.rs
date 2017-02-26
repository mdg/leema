#[macro_use]
use leema::log;
use leema::val::{Val, Env, FutureVal, Type};
use leema::reg::{Reg, Ireg};
use leema::code::{self, CodeKey, Code, Op, OpVec, ModSym, RustFunc};
use leema::list;
use std::collections::{HashMap, LinkedList};
use std::collections::hash_map;
use std::sync::{Arc, Mutex, MutexGuard, Condvar};
use std::sync::atomic::{AtomicBool, AtomicIsize, Ordering};
use std::sync::mpsc;
use std::mem;
use std::fmt::{self, Debug};
use std::thread;
use std::time;
use std::io::{stderr, Write};


pub enum Parent
{
    Null,
    Caller(Reg, Code, Box<Frame>),
    Fork(Arc<AtomicBool>, mpsc::Sender<Val>),
    Repl(Val),
    Main(Val),
}

impl Parent
{
    pub fn set_result(&mut self, r: Val)
    {
        match self {
            &mut Parent::Caller(ref dst, _, ref mut pf) => {
                pf.e.set_reg(dst, r);
            }
            &mut Parent::Fork(_, _) => {
            }
            &mut Parent::Main(ref mut res) => {
                *res = r;
            }
            &mut Parent::Repl(ref mut res) => {
                *res = r;
            }
            &mut Parent::Null => {}
        }
    }
}

impl Debug for Parent
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self {
            &Parent::Null => write!(f, "Parent::Null"),
            &Parent::Caller(ref dst, ref code, ref pf) => {
                write!(f,
                    "Parent::Caller({:?}, {:?}, {:?})",
                    dst, code, pf
                )
            }
            &Parent::Fork(ref ready, _) => {
                write!(f, "Parent::Fork({:?})", ready)
            }
            &Parent::Repl(ref res) => write!(f, "Parent::Repl({:?})", res),
            &Parent::Main(ref res) => write!(f, "Parent::Main({:?})", res),
        }
    }
}

#[derive(Debug)]
pub struct FrameTrace
{
    // TODO: Implement this in leema later
    direction: i8,
    function: String,
    parent: Option<Arc<FrameTrace>>,
}

impl FrameTrace
{
    pub fn new_root(func: &String) -> Arc<FrameTrace>
    {
        Arc::new(FrameTrace{
            direction: 1,
            function: func.clone(),
            parent: None,
        })
    }

    pub fn push_call(parent: &Arc<FrameTrace>, func: &String) -> Arc<FrameTrace>
    {
        Arc::new(FrameTrace{
            direction: 1,
            function: func.clone(),
            parent: Some(parent.clone()),
        })
    }

    pub fn propagate_down(trace: &Arc<FrameTrace>, func: &String)
        -> Arc<FrameTrace>
    {
        Arc::new(FrameTrace{
            direction: -1,
            function: func.clone(),
            parent: Some(trace.clone()),
        })
    }

    pub fn failure_here(&self) -> Arc<FrameTrace>
    {
        Arc::new(FrameTrace{
            direction: 0,
            function: self.function.clone(),
            parent: self.parent.clone(),
        })
    }
}

impl fmt::Display for FrameTrace
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        let dir = match self.direction {
            0 => "<>",
            1 => " >",
            -1 => "< ",
            d => {
                panic!("Invalid direction: {}", d);
            }
        };

        match self.parent {
            None => {
                write!(f, "{} {}\n", dir, self.function)
            }
            Some(ref p) => {
                write!(f, "{} {}\n{}", dir, self.function, p)
            }
        }
    }
}

#[derive(Debug)]
pub struct Frame
{
    pub name: String,
    pub parent: Parent,
    pub trace: Arc<FrameTrace>,
    pub e: Env,
    pub id: i64,
    pub pc: i32,
}

impl Frame
{
    pub fn new_root(id: i64, env: Env) -> Frame
    {
        let fname = "MAIN".to_string();
        Frame{
            parent: Parent::Main(Val::Void),
            trace: FrameTrace::new_root(&fname),
            name: fname,
            e: env,
            id: id,
            pc: 0,
        }
    }

    pub fn new_call(name: &String, dst: &Reg, e: Env, trace: &Arc<FrameTrace>)
        -> Frame
    {
        Frame{
            name: name.clone(),
            parent: Parent::Null,
            trace: FrameTrace::push_call(&trace, name),
            e: e,
            id: -1,
            pc: 0,
        }
    }

    pub fn new_fork(f: &Frame, ready: &Arc<AtomicBool>, tx: mpsc::Sender<Val>)
        -> Frame
    {
        Frame{
            name: f.name.clone(),
            parent: Parent::Fork(ready.clone(), tx),
            trace: f.trace.clone(),
            e: f.e.clone(),
            id: f.id,
            pc: 0,
        }
    }

    pub fn set_parent(&mut self, p: Parent)
    {
        self.parent = p;
    }

    pub fn take_env(&mut self) -> Env
    {
        let mut e = Env::new();
        mem::swap(&mut e, &mut self.e);
        e
    }

    pub fn receive_future(&mut self, r: &Reg) -> Option<Val>
    {
        let fval = self.e.get_reg(&r);
        if !fval.is_future() {
            panic!("This isn't even a future {:?}", fval);
        }
        if !fval.is_future_ready() {
            return None;
        }
        match fval {
            &Val::Future(FutureVal(_, ref amrx)) => {
                let rx = amrx.lock().unwrap();
                Some(rx.recv().unwrap())
            },
            _ => panic!("Not a future val? {:?}", fval),
        }
    }

    /**
     * handy accessor function when calling from rust native functions
     */
    pub fn get_param(&self, p: i8) -> &Val
    {
        self.e.get_reg(&Reg::Param(Ireg::Reg(p)))
    }

    pub fn get_param_mut(&mut self, p: i8) -> &mut Val
    {
        self.e.get_reg_mut(&Reg::Param(Ireg::Reg(p)))
    }


    pub fn execute_failure(&mut self, dst: &Reg, tag: &Reg, msg: &Reg)
    {
        let tagval = self.e.get_reg(tag).clone();
        let msgval = self.e.get_reg(msg).clone();
        let f = Val::failure(tagval, msgval, self.trace.failure_here());
        self.e.set_reg(dst, f);
        self.pc += 1;
    }

    pub fn execute_match_pattern(&mut self, jmp: i16, patt: &Reg, input: &Reg)
    {
        vout!("execute_match_pattern({}, {:?}, {:?})\n", jmp, patt, input);
        let e: &mut Env = &mut self.e;
        let matches = {
            let pval = e.get_reg(&patt);
            let ival = e.get_reg(&input);
vout!("match input: {:?}={:?}\n", pval, ival);
            Val::pattern_match(pval, ival)
        };
vout!("matches: {:?}\n", matches);
        match matches {
            Some(assignments) => {
                for a in assignments {
                    let (dst, v) = a;
                    e.set_reg(&dst, v);
                }
                self.pc += 1;
            }
            Nothing => self.pc += jmp as i32,
        }
    }

    pub fn execute_strcat(&mut self, dstreg: &Reg, srcreg: &Reg) -> Event
    {
        let result = {
            let src = self.e.get_reg(srcreg);
            if src.is_failure() {
                let mut f = src.clone();
                match &mut f {
                    &mut Val::Failure(_, _, ref mut trace) => {
                        *trace = FrameTrace::propagate_down(trace, &self.name);
                    }
                    ff => {
                        panic!("is failure, but not a failure: {:?}", ff);
                    }
                }
                self.parent.set_result(f);
                return Event::Complete(false)
            } else if src.is_future() {
                // oops, not ready to do this yet, let's bail and wait
                return Event::FutureWait(srcreg.clone())
            }
            let dst = self.e.get_reg(dstreg);
            Val::Str(Arc::new(format!("{}{}", dst, src)))
        };
        self.e.set_reg(dstreg, result);
        self.pc += 1;
        Event::Uneventful
    }
}

/*
impl Debug for Frame
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "Frame({:?}, {:?}, {})",
            self.parent, self.e, self.pc,
        )
    }
}

pub struct IOQueue {
    waiters: LinkedList<Frame>,
}

pub struct IoWorker
{
    queue: LinkedList<Box<Iop>>,
    files: HashMap<u32, File>,
    id: u16,
}
*/

#[derive(Debug)]
pub enum Event
{
    Uneventful,
    Call(Reg, Code, Frame),
    Fork,
    FutureWait(Reg),
    IOWait,
    Complete(bool),
}


impl PartialEq for Event
{
    fn eq(&self, other: &Event) -> bool
    {
        match (self, other) {
            (&Event::Uneventful, &Event::Uneventful) => true,
            (&Event::Call(ref r1, _, _), &Event::Call(ref r2, _, _)) => {
                r1 == r2
            }
            (&Event::Fork, &Event::Fork) => true,
            (&Event::FutureWait(ref r1), &Event::FutureWait(ref r2)) => {
                r1 == r2
            }
            (&Event::IOWait, &Event::IOWait) => true,
            (&Event::Complete(true), &Event::Complete(true)) => true,
            (&Event::Complete(false), &Event::Complete(false)) => true,
            _ => false,
        }
    }
}

pub fn execute_const_val(curf: &mut Frame, reg: &Reg, v: &Val)
{
vout!("execute_const_val({:?}, {:?})\n", reg, v);
    curf.e.set_reg(reg, v.clone());
vout!("e: {:?}\n", curf.e);
    curf.pc = curf.pc + 1;
}

pub fn execute_constructor(curf: &mut Frame, reg: &Reg, typ: &Type)
{
vout!("execute_constructor({:?}, {:?})\n", reg, typ);
    if let &Type::Struct(_, nfields) = typ {
        let mut fields = Vec::with_capacity(nfields as usize);
        fields.resize(nfields as usize, Val::Void);
        curf.e.set_reg(reg, Val::Struct(typ.clone(), fields));
        curf.pc = curf.pc + 1;
    } else {
        panic!("Cannot construct not structure: {:?}", typ);
    }
}

pub fn execute_copy(curf: &mut Frame, dst: &Reg, src: &Reg) {
    let src_val = curf.e.get_reg(src).clone();
    curf.e.set_reg(dst, src_val);
    curf.pc = curf.pc + 1;
}

/**
 * fork the frame and frame state
 * add it to the fresh queue
 * jump current frame state past the fork block
fn execute_fork(curf: &mut Frame,
    dst: &Reg, freg: &Reg, argreg: &Reg
) -> Event {
    println!("execute_fork");

    // args are empty for a fork
    // create new frame
    let e = Env::new();
    // set current state to called
    let (tx, rx) = mpsc::channel::<Val>();
    let ready = Arc::new(AtomicBool::new(false));
    let newf = Frame::new_fork(&curf, &ready, tx);
    curf.e.set_reg(dst, Val::future(ready, rx));

    curf.pc = curf.pc + 1;
    Event::Fork
}
 */

pub fn execute_jump(curf: &mut Frame, jmp: i16)
{
    curf.pc += jmp as i32;
}

pub fn execute_jump_if_not(curf: &mut Frame, jmp: i16, reg: &Reg)
{
vout!("execute_jump_if_not({:?},{:?})\n", jmp, reg);
    let test_val = curf.e.get_reg(reg);
    if let &Val::Bool(test) = test_val {
        if test {
            vout!("if test is true\n");
            curf.pc += 1;
        } else {
            vout!("if test is false\n");
            curf.pc += jmp as i32;
        }
    } else {
        panic!("can't if check a not bool {:?}", test_val);
    }
}

pub fn execute_list_cons(curf: &mut Frame, dst: &Reg, head: &Reg, tail: &Reg)
{
    let new_list = {
        let headval = curf.e.get_reg(&head).clone();
        let tailval = curf.e.get_reg(&tail).clone();
        list::cons(headval, tailval)
    };
    curf.e.set_reg(&dst, new_list);
    curf.pc += 1;
}

pub fn execute_list_create(curf: &mut Frame, dst: &Reg) {
    curf.e.set_reg(&dst, list::empty());
    curf.pc = curf.pc + 1;
}

pub fn execute_tuple_create(curf: &mut Frame, dst: &Reg, ref sz: i8)
{
    vout!("execute_tuple_create({:?}, {})\n", dst, sz);
    let tupsize: usize = *sz as usize;
    curf.e.set_reg(dst, Val::new_tuple(tupsize));
    curf.pc = curf.pc + 1;
}

pub fn execute_load_func(curf: &mut Frame, dst: &Reg, ms: &ModSym)
{
    curf.pc = curf.pc + 1;
}

fn call_arg_failure(args: &Val) -> Option<&Val>
{
    if let &Val::Tuple(ref items) = args {
        for i in items {
            if i.is_failure() {
                return Some(i);
            }
        }
    } else {
        panic!("call args are not a tuple");
    }
    None
}

/**
 * get code from func
 * make an Env from the args
 * make a new frame state
 * create a new frame w/ func code and new frame state
 * set curf.flag to Called(new_frame)
 */
pub fn execute_call(curf: &mut Frame, dst: &Reg, freg: &Reg, argreg: &Reg)
{
    let ref fname_val = curf.e.get_reg(freg);
    match *fname_val {
        &Val::Str(ref name_str) => {
            curf.pc = curf.pc + 1;
            // load code
            /*
            let ck = CodeKey::Name(name_str.clone());
            let code = w.find_code(&ck).unwrap().clone();
            // pass in args
            let args = curf.e.get_reg(argreg);
            match call_arg_failure(args) {
                Some(bfailure) => {
                    let mut failure = bfailure.clone();
                    if let &mut Val::Failure(_, _, ref mut trace) = &mut failure {
                        *trace = FrameTrace::propagate_down(trace, &curf.name);
                    }
                    curf.parent.set_result(failure);
                    w.event = Event::Complete(false);
                    return;
                }
                None => {}
            }
            // create new frame
            let e = Env::with_args(args.clone());
            // set current state to called
            w.event = Event::Call(
                dst.clone(),
                code.clone(),
                Frame::new_call(&name_str, &dst, e, &curf.trace),
            );
            */
        }
        _ => {
            panic!("That's not a function! {:?}", fname_val);
        }
    }
}


/*
process_set
process
|  \- base frames
|      |       \- call code
 \--- fork
*/


#[cfg(test)]
mod tests {
    use leema::log;
    use leema::frame::{Frame, Parent, Event};
    use leema::application::{Application};
    use leema::ast;
    use leema::code::{CodeKey};
    use leema::loader::{Interloader};
    use leema::module::{ModKey, ModuleInterface, ModuleSource};
    use leema::program;
    use leema::reg::{Reg};
    use leema::val::{Env, Val};
    use leema::prefab;
    use leema::lex::{lex};
    use leema::worker::{Worker};

    use std::thread;
    use std::sync::{Arc, Mutex};
    use std::rc::{Rc};
    use std::io::{stderr, Write};
    use libc::{getpid};


#[test]
fn test_normal_strcat()
{
    let mut env = Env::new();
    let r1 = Reg::local(1);
    let r2 = Reg::local(2);
    env.set_reg(&r1, Val::new_str(String::from("i like ")));
    env.set_reg(&r2, Val::new_str(String::from("burritos")));
    let mut frame = Frame::new_root(1, env);

    let event = frame.execute_strcat(&r1, &r2);
    assert_eq!(Event::Uneventful, event);
}

}
