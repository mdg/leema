#[macro_use]
use leema::log;
use leema::frame::{Frame, Event, Parent, FrameTrace};
use leema::val::{self, Val, Env, FutureVal, Type, MsgVal};
use leema::reg::{Reg, Ireg};
use leema::code::{self, CodeKey, Code, Op, OpVec, ModSym, RustFunc};
use leema::list;

use std::cell::{RefCell, RefMut};
use std::collections::{HashMap, LinkedList};
use std::collections::hash_map;
use std::ops::{DerefMut};
use std::rc::{Rc};
use std::mem;
use std::fmt::{self, Debug};
use std::time::{Duration};
use std::thread;
use std::io::{stderr, Write};


#[derive(Debug)]
pub struct Fiber
{
    pub fiber_id: i64,
    pub head: Frame,
}

macro_rules! handle_value {
    ($curf:expr, $reg:expr) => {{
        let val_clone = $curf.head.e.get_reg($reg).clone();
        match &val_clone {
            &Val::Failure(_, _, ref trace) => {
                FrameTrace::propagate_down(trace
                    , $curf.function_name());
                $curf.head.parent.set_result(val_clone.clone());
                return Event::failure();
            }
            &Val::Future(_) => {
                return Event::FutureWait($reg.clone())
            }
            _ => {
                val_clone.clone()
            }
        }
    }}
}

impl Fiber
{
    pub fn spawn(id: i64, root: Frame) -> Fiber
    {
        Fiber{
            fiber_id: id,
            head: root,
        }
    }

    pub fn id(&self) -> i64
    {
        self.fiber_id
    }

    pub fn module_name(&self) -> &str
    {
        self.head.module_name()
    }

    pub fn function_name(&self) -> &str
    {
        self.head.function_name()
    }

    pub fn push_call(&mut self, code: Rc<Code>, dst: Reg
            , module: Rc<String>, func: Rc<String>, args: Val
    ) {
        let trace = self.head.trace.clone();
        let mut newf = Frame{
            parent: Parent::Null,
            module: module.clone(),
            function: func.clone(),
            trace: FrameTrace::push_call(&trace, &(*func)),
            e: Env::with_args(args),
            pc: 0,
        };
        mem::swap(&mut self.head, &mut newf);
        let parent = Parent::Caller(code, Box::new(newf), dst);
        self.head.set_parent(parent);
    }

    pub fn execute_leema_frame(&mut self, ops: &OpVec) -> Event
    {
        let mut e = Event::Uneventful;
        while let Event::Uneventful = e {
            e = self.execute_leema_op(ops);
        }
        e
    }

    pub fn execute_leema_op(&mut self, ops: &OpVec) -> Event
    {
        let op = ops.get(self.head.pc as usize).unwrap();
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
                self.execute_jump(jmp)
            }
            &Op::JumpIfNot(jmp, ref reg) => {
                self.execute_jump_if_not(jmp, reg)
            }
            &Op::MatchPattern(ref dst, ref patt, ref input) => {
                self.execute_match_pattern(dst, patt, input)
            }
            &Op::ListCons(ref dst, ref head, ref tail) => {
                self.execute_cons_list(dst, head, tail)
            }
            &Op::ListCreate(ref dst) => {
                self.execute_create_list(dst)
            }
            &Op::TupleCreate(ref dst, ref sz) => {
                self.execute_create_tuple(dst, *sz)
            }
            &Op::StrCat(ref dst, ref src) => {
                self.execute_strcat(dst, src)
            }
            &Op::LoadFunc(ref reg, ref modsym) => {
                self.execute_load_func(reg, modsym)
            }
            &Op::ApplyFunc(ref dst, ref func, ref args) => {
                self.execute_call(dst, func, args)
            }
            &Op::Return => {
                Event::Complete(true)
            }
            &Op::SetResult(ref dst) => {
                if *dst == Reg::Void {
                    panic!("return void at {} in {:?}", self.head.pc, ops);
                }
                self.head.parent.set_result(self.head.e.get_reg(dst).clone());
                self.head.pc += 1;
                Event::Uneventful
            }
            &Op::Failure(ref dst, ref tag, ref msg) => {
                self.execute_failure(dst, tag, msg)
            }
        }
    }

    pub fn execute_strcat(&mut self, dstreg: &Reg, srcreg: &Reg) -> Event
    {
        let result = {
            let dst = self.head.e.get_reg(dstreg);
            let src = self.head.e.get_reg(srcreg);
            match (dst, src) {
                (&Val::Future(_), _) => {
                    // oops, not ready to do this yet, let's bail and wait
                    return Event::FutureWait(dstreg.clone())
                }
                (_, &Val::Future(_)) => {
                    // oops, not ready to do this yet, let's bail and wait
                    return Event::FutureWait(srcreg.clone())
                }
                (ref a, ref b) => {
                    Val::new_str(format!("{}{}", dst, src))
                }
            }
        };
        self.head.e.set_reg(dstreg, result);
        self.head.pc += 1;
        Event::Uneventful
    }

    pub fn execute_match_pattern(&mut self, dst: &Reg, patt: &Val, input: &Reg)
        -> Event
    {
        vout!("execute_match_pattern({:?}, {:?}, {:?})\n", dst, patt, input);
        let matches = {
            let ival = self.head.e.get_reg(&input);
            Val::pattern_match(patt, ival)
        };
        match matches {
            Some(assignments) => {
                for a in assignments {
                    match a {
                        (Reg::Param(_), _) => {
                            // don't write into param, it's already correct
                        }
                        (pdst, v) => {
                            self.head.e.set_reg(&pdst, v);
                        }
                    }
                }
                self.head.e.set_reg(dst, Val::Bool(true));
            }
            Nothing => {
                self.head.e.set_reg(dst, Val::Bool(false));
            }
        }
        self.head.pc += 1;
        Event::Uneventful
    }

    /**
    * get code from func
    * make an Env from the args
    * make a new frame state
    * create a new frame w/ func code and new frame state
    * set curf.flag to Called(new_frame)
    */
    pub fn execute_call(&mut self, dst: &Reg
        , freg: &Reg, argreg: &Reg) -> Event
    {
        let (modname, funcname) = {
            let ref fname_val = self.head.e.get_reg(freg);
            match *fname_val {
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
            }
        };

        let opt_failure =
            Fiber::call_arg_failure(self.head.e.get_reg(argreg))
                .map(|argv| {
                    argv.clone()
                });
        match opt_failure {
            Some(mut failure) => {
                if let &mut Val::Failure(_, _, ref mut trace) = &mut failure {
                    *trace = FrameTrace::propagate_down(
                        trace,
                        self.head.function_name(),
                    );
                }
                self.head.parent.set_result(failure);
                Event::Complete(false)
            }
            None => {
                let args_copy = self.head.e.get_reg(argreg).clone();
                Event::Call(
                    dst.clone(),
                    modname,
                    funcname,
                    args_copy,
                )
            }
        }
    }

    pub fn execute_const_val(&mut self, reg: &Reg, v: &Val) -> Event
    {
        self.head.e.set_reg(reg, v.clone());
        self.head.pc += 1;
        Event::Uneventful
    }

    pub fn execute_constructor(&mut self, reg: &Reg, typ: &Type) -> Event
    {
        if let &Type::Struct(_, nfields) = typ {
            let mut fields = Vec::with_capacity(nfields as usize);
            fields.resize(nfields as usize, Val::Void);
            self.head.e.set_reg(reg, Val::Struct(typ.clone(), fields));
            self.head.pc = self.head.pc + 1;
            Event::Uneventful
        } else {
            panic!("Cannot construct not structure: {:?}", typ);
        }
    }

    pub fn execute_cons_list(&mut self, dst: &Reg, head: &Reg, tail: &Reg)
        -> Event
    {
        let new_list = {
            let headval = self.head.e.get_reg(&head).clone();
            let tailval = self.head.e.get_reg(&tail).clone();
            list::cons(headval, tailval)
        };
        self.head.e.set_reg(&dst, new_list);
        self.head.pc += 1;
        Event::Uneventful
    }

    pub fn execute_create_list(&mut self, dst: &Reg) -> Event
    {
        self.head.e.set_reg(&dst, list::empty());
        self.head.pc = self.head.pc + 1;
        Event::Uneventful
    }

    pub fn execute_create_tuple(&mut self, dst: &Reg, ref sz: i8) -> Event
    {
        let tupsize: usize = *sz as usize;
        self.head.e.set_reg(dst, Val::new_tuple(tupsize));
        self.head.pc = self.head.pc + 1;
        Event::Uneventful
    }

    pub fn execute_jump(&mut self, jmp: i16) -> Event
    {
        self.head.pc += jmp as i32;
        Event::Uneventful
    }

    pub fn execute_jump_if_not(&mut self, jmp: i16, reg: &Reg) -> Event
    {
        vout!("execute_jump_if_not({:?},{:?})\n", jmp, reg);
        let tjump: i32 = {
            let test_val = self.head.e.get_reg(reg);
            if let &Val::Bool(test) = test_val {
                if test {
                    vout!("if test is true\n");
                    1
                } else {
                    vout!("if test is false\n");
                    jmp as i32
                }
            } else {
                panic!("can't if check a not bool {:?}", test_val);
            }
        };
        self.head.pc += tjump;
        Event::Uneventful
    }

    pub fn execute_load_func(&mut self, dst: &Reg, ms: &ModSym) -> Event
    {
        self.head.pc = self.head.pc + 1;
        Event::Uneventful
    }

    pub fn execute_copy(&mut self, dst: &Reg, src: &Reg) -> Event
    {
        let src_val = self.head.e.get_reg(src).clone();
        self.head.e.set_reg(dst, src_val);
        self.head.pc = self.head.pc + 1;
        Event::Uneventful
    }

    pub fn execute_failure(&mut self, dst: &Reg, tag: &Reg, msg: &Reg) -> Event
    {
        let tagval = self.head.e.get_reg(tag).clone();
        let msgval = self.head.e.get_reg(msg).clone();
        let f = Val::failure(tagval, msgval, self.head.trace.failure_here());
        self.head.e.set_reg(dst, f);
        self.head.pc += 1;
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
}


#[cfg(test)]
mod tests {
    use leema::log;
    use leema::frame::{Frame, Parent, Event};
    use leema::fiber::{Fiber};
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
    let mut frame = Frame::new_root(String::from("foo"), String::from("bar"));
    frame.e.set_reg(&r1, Val::new_str(String::from("i like ")));
    frame.e.set_reg(&r2, Val::new_str(String::from("burritos")));
    let mut fib = Fiber::spawn(1, frame);

    let event = fib.execute_strcat(&r1, &r2);
    assert_eq!(Event::Uneventful, event);
}

}
