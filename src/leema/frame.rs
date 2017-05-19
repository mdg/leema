#[macro_use]
use leema::log;
use leema::val::{Val, Env, FutureVal, Type};
use leema::reg::{Reg, Ireg};
use leema::code::{self, CodeKey, Code, Op, OpVec, ModSym, RustFunc};
use leema::list;

use std::collections::{HashMap, LinkedList};
use std::collections::hash_map;
use std::rc::{Rc};
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
    Caller(Rc<Code>, Box<Frame>, Reg),
    // Fork(Arc<AtomicBool>, mpsc::Sender<Msg>),
    Repl(Val),
    Main(Val),
}

impl Parent
{
    pub fn set_result(&mut self, r: Val)
    {
        match self {
            &mut Parent::Caller(_, ref mut pf, ref dst) => {
                pf.e.set_reg(dst, r);
            }
            // &mut Parent::Fork(_, _) => {}
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
            &Parent::Caller(ref code, ref pf, ref dst) => {
                write!(f,
                    "Parent::Caller({:?}, {}, {:?})",
                    dst, code, pf
                )
            }
            /*
            &Parent::Fork(ref ready, _) => {
                write!(f, "Parent::Fork({:?})", ready)
            }
            */
            &Parent::Repl(ref res) => write!(f, "Parent::Repl({:?})", res),
            &Parent::Main(ref res) => write!(f, "Parent::Main({:?})", res),
        }
    }
}

#[derive(Debug)]
pub enum Event
{
    Uneventful,
    Call(Reg, Rc<String>, Rc<String>, Val),
    Fork,
    FutureWait(Reg),
    IOWait,
    Complete(bool),
}

impl Event
{
    pub fn success() -> Event
    {
        Event::Complete(true)
    }

    pub fn failure() -> Event
    {
        Event::Complete(false)
    }
}

impl PartialEq for Event
{
    fn eq(&self, other: &Event) -> bool
    {
        match (self, other) {
            (&Event::Uneventful, &Event::Uneventful) => true,
            (&Event::Call(ref r1, ref m1, ref f1, ref a1),
                    &Event::Call(ref r2, ref m2, ref f2, ref a2)) =>
            {
                (r1 == r2 && m1 == m2 && f1 == f2 && a1 == a2)
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

    pub fn push_call(parent: &Arc<FrameTrace>, func: &str) -> Arc<FrameTrace>
    {
        Arc::new(FrameTrace{
            direction: 1,
            function: func.to_string(),
            parent: Some(parent.clone()),
        })
    }

    pub fn propagate_down(trace: &Arc<FrameTrace>, func: &str)
        -> Arc<FrameTrace>
    {
        Arc::new(FrameTrace{
            direction: -1,
            function: String::from(func),
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
    pub parent: Parent,
    pub module: Rc<String>,
    pub function: Rc<String>,
    pub trace: Arc<FrameTrace>,
    pub e: Env,
    pub pc: i32,
}

impl Frame
{
    pub fn new_root(m: String, f: String) -> Frame
    {
        let env = Env::new();
        let modname = Rc::new(m);
        let fname = Rc::new(f);
        Frame{
            parent: Parent::Main(Val::Void),
            trace: FrameTrace::new_root(&fname),
            module: modname,
            function: fname,
            e: env,
            pc: 0,
        }
    }

    /*
    pub fn new_fork(f: &Frame, ready: &Arc<AtomicBool>, tx: mpsc::Sender<Msg>)
        -> Frame
    {
        Frame{
            parent: Parent::Fork(ready.clone(), tx),
            name: f.name.clone(),
            trace: f.trace.clone(),
            e: f.e.clone(),
            id: f.id,
            pc: 0,
        }
    }
    */

    pub fn set_parent(&mut self, p: Parent)
    {
        self.parent = p;
    }

    pub fn take_parent(&mut self) -> Parent
    {
        mem::replace(&mut self.parent, Parent::Null)
    }

    pub fn take_env(&mut self) -> Env
    {
        let mut e = Env::new();
        mem::swap(&mut e, &mut self.e);
        e
    }

    pub fn module_name(&self) -> &str
    {
        &(**self.module)
    }

    pub fn function_name(&self) -> &str
    {
        &(**self.function)
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
                Some(Val::from_msg(rx.recv().unwrap()))
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

    pub fn execute_frame(&mut self, code: &Rc<Code>) -> Event
    {
        match &**code {
            &Code::Leema(ref ops) => {
                // moved to frame
                // let fref: &mut Fiber = f.borrow;
                self.execute_leema_frame(ops)
            }
            &Code::Rust(ref rf) => {
                rf(self)
            }
        }
    }

    pub fn execute_leema_frame(&mut self, ops: &OpVec) -> Event
    {
        let mut e = Event::Uneventful;
        while Event::Uneventful == e {
            e = self.execute_leema_op(ops);
        }
        e
    }

    pub fn execute_leema_op(&mut self, ops: &OpVec) -> Event
    {
        let op = ops.get(self.pc as usize).unwrap();
        vout!("exec: {:?}\n", op);
        match op {
            &Op::ConstVal(ref dst, ref v) => {
                self.execute_const_val(dst, v)
            }
            &Op::Constructor(ref dst, ref typ) => {
                self.execute_constructor(dst, typ)
            }
            &Op::Copy(ref dst, ref src) => {
                self.execute_copy(dst, src)
            }
            &Op::Fork(ref dst, ref freg, ref args) => {
                // frame::execute_fork(self, curf, dst, freg, args);
                Event::Uneventful
            }
            &Op::Jump(jmp) => {
                execute_jump(self, jmp)
            }
            &Op::JumpIfNot(jmp, ref reg) => {
                execute_jump_if_not(self, jmp, reg)
            }
            &Op::MatchPattern(ref dst, ref patt, ref input) => {
                self.execute_match_pattern(dst, patt, input)
            }
            &Op::ListCons(ref dst, ref head, ref tail) => {
                execute_list_cons(self, dst, head, tail)
            }
            &Op::ListCreate(ref dst) => {
                execute_list_create(self, dst)
            }
            &Op::TupleCreate(ref dst, ref sz) => {
                execute_tuple_create(self, dst, *sz)
            }
            &Op::StrCat(ref dst, ref src) => {
                self.execute_strcat(dst, src)
            }
            &Op::LoadFunc(ref reg, ref modsym) => {
                execute_load_func(self, reg, modsym)
            }
            &Op::ApplyFunc(ref dst, ref func, ref args) => {
                execute_call(self, dst, func, args)
            }
            &Op::Return => {
                Event::Complete(true)
            }
            &Op::SetResult(ref dst) => {
                if *dst == Reg::Void {
                    panic!("return void at {} in {:?}", self.pc, ops);
                }
                self.parent.set_result(self.e.get_reg(dst).clone());
                self.pc += 1;
                Event::Uneventful
            }
            &Op::Failure(ref dst, ref tag, ref msg) => {
                self.execute_failure(dst, tag, msg)
            }
        }
    }

    pub fn execute_const_val(&mut self, reg: &Reg, v: &Val) -> Event
    {
        self.e.set_reg(reg, v.clone());
        self.pc += 1;
        Event::Uneventful
    }

    pub fn execute_constructor(&mut self, reg: &Reg, typ: &Type) -> Event
    {
        if let &Type::Struct(_, nfields) = typ {
            let mut fields = Vec::with_capacity(nfields as usize);
            fields.resize(nfields as usize, Val::Void);
            self.e.set_reg(reg, Val::Struct(typ.clone(), fields));
            self.pc = self.pc + 1;
            Event::Uneventful
        } else {
            panic!("Cannot construct not structure: {:?}", typ);
        }
    }

    pub fn execute_copy(&mut self, dst: &Reg, src: &Reg) -> Event
    {
        let src_val = self.e.get_reg(src).clone();
        self.e.set_reg(dst, src_val);
        self.pc = self.pc + 1;
        Event::Uneventful
    }

    pub fn execute_failure(&mut self, dst: &Reg, tag: &Reg, msg: &Reg) -> Event
    {
        let tagval = self.e.get_reg(tag).clone();
        let msgval = self.e.get_reg(msg).clone();
        let f = Val::failure(tagval, msgval, self.trace.failure_here());
        self.e.set_reg(dst, f);
        self.pc += 1;
        Event::Uneventful
    }

    pub fn execute_match_pattern(&mut self, dst: &Reg, patt: &Val, input: &Reg)
        -> Event
    {
        vout!("execute_match_pattern({:?}, {:?}, {:?})\n", dst, patt, input);
        let e: &mut Env = &mut self.e;
        let matches = {
            let ival = e.get_reg(&input);
vout!("match input: {:?}={:?}\n", patt, ival);
            Val::pattern_match(patt, ival)
        };
vout!("matches: {:?}\n", matches);
        match matches {
            Some(assignments) => {
                for a in assignments {
                    let (pdst, v) = a;
                    e.set_reg(&pdst, v);
                }
                e.set_reg(dst, Val::Bool(true));
            }
            Nothing => {
                e.set_reg(dst, Val::Bool(false));
            }
        }
        self.pc += 1;
        Event::Uneventful
    }

    pub fn execute_strcat(&mut self, dstreg: &Reg, srcreg: &Reg) -> Event
    {
        let result = {
            let src = self.e.get_reg(srcreg);
            if src.is_failure() {
                let mut f = src.clone();
                match &mut f {
                    &mut Val::Failure(_, _, ref mut trace) => {
                        *trace = FrameTrace::propagate_down(trace
                            , self.function_name());
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
            Val::new_str(format!("{}{}", dst, src))
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
*/

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

pub fn execute_jump(curf: &mut Frame, jmp: i16) -> Event
{
    curf.pc += jmp as i32;
    Event::Uneventful
}

pub fn execute_jump_if_not(curf: &mut Frame, jmp: i16, reg: &Reg) -> Event
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
        Event::Uneventful
    } else {
        panic!("can't if check a not bool {:?}", test_val);
    }
}

pub fn execute_list_cons(curf: &mut Frame, dst: &Reg, head: &Reg, tail: &Reg)
    -> Event
{
    let new_list = {
        let headval = curf.e.get_reg(&head).clone();
        let tailval = curf.e.get_reg(&tail).clone();
        list::cons(headval, tailval)
    };
    curf.e.set_reg(&dst, new_list);
    curf.pc += 1;
    Event::Uneventful
}

pub fn execute_list_create(curf: &mut Frame, dst: &Reg) -> Event
{
    curf.e.set_reg(&dst, list::empty());
    curf.pc = curf.pc + 1;
    Event::Uneventful
}

pub fn execute_tuple_create(curf: &mut Frame, dst: &Reg, ref sz: i8) -> Event
{
    let tupsize: usize = *sz as usize;
    curf.e.set_reg(dst, Val::new_tuple(tupsize));
    curf.pc = curf.pc + 1;
    Event::Uneventful
}

pub fn execute_load_func(curf: &mut Frame, dst: &Reg, ms: &ModSym) -> Event
{
    curf.pc = curf.pc + 1;
    Event::Uneventful
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
-> Event
{
    let ref fname_val = curf.e.get_reg(freg);
    let (modname, funcname) = match *fname_val {
        &Val::Str(ref name_str) => {
            vout!("execute_call({})\n", name_str);
            // pass in args
            (Rc::new("".to_string()), name_str.clone())
        }
        &Val::Tuple(ref modfunc) if modfunc.len() == 2 => {
            let modnm = modfunc.get(0).unwrap();
            let funcnm = modfunc.get(1).unwrap();
            vout!("execute_call({}.{})\n", modnm, funcnm);
            match (modnm, funcnm) {
                (&Val::Str(ref m), &Val::Str(ref f)) => {
                    (m.clone(), f.clone())
                }
                _ => {
                    panic!("That's not a function! {:?}", fname_val);
                }
            }
        }
        _ => {
            panic!("That's not a function! {:?}", fname_val);
        }
    };

    let args = curf.e.get_reg(argreg);
    match call_arg_failure(args) {
        Some(bfailure) => {
            let mut failure = bfailure.clone();
            if let &mut Val::Failure(_, _, ref mut trace) = &mut failure
            {
                *trace = FrameTrace::propagate_down(
                    trace,
                    curf.function_name(),
                );
            }
            curf.parent.set_result(failure);
            Event::Complete(false)
        }
        None => {
            Event::Call(
                dst.clone(),
                modname,
                funcname,
                args.clone(),
            )
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
    let r1 = Reg::local(1);
    let r2 = Reg::local(2);
    let mut frame = Frame::new_root(1
        , String::from("foo"), String::from("bar"));
    frame.e.set_reg(&r1, Val::new_str(String::from("i like ")));
    frame.e.set_reg(&r2, Val::new_str(String::from("burritos")));

    let event = frame.execute_strcat(&r1, &r2);
    assert_eq!(Event::Uneventful, event);
}

}
