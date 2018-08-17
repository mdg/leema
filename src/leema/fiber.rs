use leema::code::{Code, Op, OpVec};
use leema::frame::{Event, Frame, FrameTrace, Parent};
use leema::list;
use leema::log;
use leema::reg::Reg;
use leema::struple::Struple;
use leema::val::{Env, Type, Val};

use std::io::Write;
use std::mem;
use std::rc::Rc;


#[derive(Debug)]
pub struct Fiber
{
    pub fiber_id: i64,
    pub head: Frame,
}

impl Fiber
{
    pub fn spawn(id: i64, root: Frame) -> Fiber
    {
        Fiber {
            fiber_id: id,
            head: root,
        }
    }

    pub fn module_name(&self) -> &str
    {
        self.head.module_name()
    }

    pub fn function_name(&self) -> &str
    {
        self.head.function_name()
    }

    pub fn push_call(
        &mut self,
        code: Rc<Code>,
        dst: Reg,
        line: i16,
        module: Rc<String>,
        func: Rc<String>,
        args: Val,
    )
    {
        let mut newf = Frame {
            parent: Parent::Null,
            module: module.clone(),
            function: func.clone(),
            trace: self.head.push_frame_trace(line),
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
        let line = op.1;
        vout!("exec: {:?}\n", op);
        match &op.0 {
            &Op::ConstVal(ref dst, ref v) => self.execute_const_val(dst, v),
            &Op::Construple(ref dst, ref typ, ref flds) => {
                self.execute_construple(dst, typ, flds)
            }
            &Op::Copy(ref dst, ref src) => self.execute_copy(dst, src),
            &Op::Jump(jmp) => self.execute_jump(jmp),
            &Op::JumpIfNot(jmp, ref reg) => self.execute_jump_if_not(jmp, reg),
            &Op::IfFailure(ref dst, ref src, jmp) => {
                self.execute_if_failure(dst, src, jmp)
            }
            &Op::MatchPattern(ref dst, ref patt, ref input) => {
                self.execute_match_pattern(dst, patt, input)
            }
            &Op::ListCons(ref dst, ref head, ref tail) => {
                self.execute_cons_list(dst, head, tail)
            }
            &Op::ListCreate(ref dst) => self.execute_create_list(dst),
            &Op::TupleCreate(ref dst, ref sz) => {
                self.execute_create_tuple(dst, *sz)
            }
            &Op::StrCat(ref dst, ref src) => self.execute_strcat(dst, src),
            &Op::ApplyFunc(ref dst, ref func, ref args) => {
                self.execute_call(dst, func, args, line)
            }
            &Op::Return => Event::Complete(true),
            &Op::SetResult(ref dst) => {
                if *dst == Reg::Void {
                    panic!("return void at {} in {:?}", self.head.pc, ops);
                }
                self.head
                    .parent
                    .set_result(self.head.e.get_reg(dst).clone());
                self.head.pc += 1;
                Event::Uneventful
            }
            &Op::PropagateFailure(ref src, line) => {
                let ev = self.propagate_failure(src, line);
                self.head.pc += 1;
                ev
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
                    return Event::FutureWait(dstreg.clone());
                }
                (_, &Val::Future(_)) => {
                    // oops, not ready to do this yet, let's bail and wait
                    return Event::FutureWait(srcreg.clone());
                }
                _ => Val::new_str(format!("{}{}", dst, src)),
            }
        };
        self.head.e.set_reg(dstreg, result);
        self.head.pc += 1;
        Event::Uneventful
    }

    pub fn execute_match_pattern(
        &mut self,
        dst: &Reg,
        patt: &Val,
        input: &Reg,
    ) -> Event
    {
        vout!(
            "execute_match_pattern({:?}, {:?}, {:?})\n",
            dst,
            patt,
            input
        );
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
            None => {
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
    pub fn execute_call(
        &mut self,
        dst: &Reg,
        freg: &Reg,
        argreg: &Reg,
        line: i16,
    ) -> Event
    {
        let (modname, funcname) = {
            let ref fname_val = self.head.e.get_reg(freg);
            match *fname_val {
                &Val::Str(ref name_str) => {
                    // pass in args
                    (Rc::new("".to_string()), name_str.clone())
                }
                &Val::Tuple(ref modfunc) if modfunc.0.len() == 2 => {
                    let modnm = &modfunc.0.get(0).unwrap().1;
                    let funcnm = &modfunc.0.get(1).unwrap().1;
                    match (modnm, funcnm) {
                        (&Val::Str(ref m), &Val::Str(ref f)) => {
                            (m.clone(), f.clone())
                        }
                        _ => {
                            panic!("That's not a function! {:?}", fname_val);
                        }
                    }
                }
                &Val::FuncRef(ref modnm, ref funcnm, _) => {
                    (modnm.clone(), funcnm.clone())
                }
                _ => {
                    panic!("That's not a function! {:?}", fname_val);
                }
            }
        };
        vout!("execute_call({}::{})\n", modname, funcname);

        let opt_failure = Fiber::call_arg_failure(self.head.e.get_reg(argreg))
            .map(|argv| argv.clone());
        match opt_failure {
            Some(mut failur) => {
                if let &mut Val::Failure(_, _, ref mut trace, _) = &mut failur {
                    *trace = FrameTrace::propagate_down(
                        trace,
                        self.head.function_name(),
                        0,
                    );
                }
                self.head.parent.set_result(failur);
                Event::Complete(false)
            }
            None => {
                let args_copy = self.head.e.get_reg(argreg).clone();
                Event::Call(dst.clone(), line, modname, funcname, args_copy)
            }
        }
    }

    pub fn execute_const_val(&mut self, reg: &Reg, v: &Val) -> Event
    {
        self.head.e.set_reg(reg, v.clone());
        self.head.pc += 1;
        Event::Uneventful
    }

    pub fn execute_construple(
        &mut self,
        reg: &Reg,
        new_typ: &Type,
        flds: &Struple<Type>,
    ) -> Event
    {
        let construple = match self.head.e.get_params() {
            &Val::Struct(_, ref items) => {
                if let &Type::UserDef(ref i_new_typ) = new_typ {
                    Val::Struct(i_new_typ.clone(), items.clone())
                } else {
                    panic!("struct type is not user defined: {:?}", new_typ);
                }
            }
            &Val::Tuple(ref items) => {
                if let &Type::UserDef(ref i_new_typ) = new_typ {
                    let new_items = items
                        .0
                        .iter()
                        .zip(flds.0.iter())
                        .map(|(i, f)| {
                            if i.0.is_some() {
                                (i.0.clone(), i.1.clone())
                            } else {
                                (f.0.clone(), i.1.clone())
                            }
                        }).collect();
                    Val::Struct(i_new_typ.clone(), Struple(new_items))
                } else {
                    panic!("struct type is not user defined: {:?}", new_typ);
                }
            }
            what => {
                panic!("cannot construct a not construple: {:?}", what);
            }
        };
        self.head.e.set_reg(reg, construple);
        self.head.pc = self.head.pc + 1;
        Event::Uneventful
    }

    pub fn execute_cons_list(
        &mut self,
        dst: &Reg,
        head: &Reg,
        tail: &Reg,
    ) -> Event
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

    pub fn execute_if_failure(
        &mut self,
        dst: &Reg,
        src: &Reg,
        jmp: i16,
    ) -> Event
    {
        let dst_val = {
            if let &Val::Failure(ref tag, _, _, _) = self.head.e.get_reg(src) {
                tag.clone()
            } else {
                self.head.pc += jmp as i32;
                return Event::Uneventful;
            }
        };
        self.head.e.set_reg(dst, *dst_val);
        self.head.pc += 1;
        Event::Uneventful
    }

    pub fn execute_copy(&mut self, dst: &Reg, src: &Reg) -> Event
    {
        let src_val = self.head.e.get_reg(src).clone();
        self.head.e.set_reg(dst, src_val);
        self.head.pc = self.head.pc + 1;
        Event::Uneventful
    }

    pub fn propagate_failure(&mut self, src: &Reg, line: i16) -> Event
    {
        let srcval = self.head.e.get_reg(src);
        if let &Val::Failure(ref tag, ref msg, ref trace, status) = srcval {
            let new_trace =
                FrameTrace::propagate_down(trace, &*self.head.function, line);
            let new_fail =
                Val::Failure(tag.clone(), msg.clone(), new_trace, status);
            self.head.parent.set_result(new_fail);
            Event::Complete(false)
        } else {
            Event::Uneventful
        }
    }

    fn call_arg_failure(args: &Val) -> Option<&Val>
    {
        if let &Val::Tuple(ref items) = args {
            for i in items.0.iter() {
                if i.1.is_failure() {
                    return Some(&i.1);
                }
            }
        } else {
            panic!("call args are not a tuple");
        }
        None
    }
}


#[cfg(test)]
mod tests
{
    use leema::fiber::Fiber;
    use leema::frame::{Event, Frame};
    use leema::reg::Reg;
    use leema::val::Val;


    #[test]
    fn test_normal_strcat()
    {
        let r1 = Reg::local(1);
        let r2 = Reg::local(2);
        let mut frame =
            Frame::new_root(String::from("foo"), String::from("bar"));
        frame.e.set_reg(&r1, Val::new_str(String::from("i like ")));
        frame.e.set_reg(&r2, Val::new_str(String::from("burritos")));
        let mut fib = Fiber::spawn(1, frame);

        let event = fib.execute_strcat(&r1, &r2);
        assert_eq!(Event::Uneventful, event);
    }

}
