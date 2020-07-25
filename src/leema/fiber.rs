use crate::leema::code::{Code, Op, OpVec};
use crate::leema::failure::{Failure, Lresult};
use crate::leema::frame::{Event, Frame, FrameTrace, Parent};
use crate::leema::list;
use crate::leema::lmap::Lmap;
use crate::leema::lstr::Lstr;
use crate::leema::module::ModKey;
use crate::leema::reg::Reg;
use crate::leema::struple::{Struple2, StrupleItem};
use crate::leema::val::{Env, Fref, Type, Val};

use std::mem;
use std::rc::Rc;


#[derive(Debug)]
pub struct Fiber
{
    pub fiber_id: i64,
    pub next_task_id: i64,
    pub head: Frame,
}

impl Fiber
{
    pub fn spawn(id: i64, root: Frame) -> Fiber
    {
        Fiber {
            fiber_id: id,
            next_task_id: 1,
            head: root,
        }
    }

    pub fn module_name(&self) -> &ModKey
    {
        &self.head.function.m
    }

    pub fn function_name(&self) -> &'static str
    {
        &self.head.function.f
    }

    pub fn new_task_key(&mut self) -> (i64, i64)
    {
        let child = self.next_task_id;
        self.next_task_id += 1;
        (child, self.fiber_id)
    }

    pub fn push_call(
        &mut self,
        code: Rc<Code>,
        dst: Reg,
        line: i16,
        func: Fref,
        args: Struple2<Val>,
    )
    {
        let mut newf = Frame {
            parent: Parent::Null,
            function: func,
            trace: self.head.push_frame_trace(line),
            e: Env::with_args(args),
            pc: 0,
        };
        mem::swap(&mut self.head, &mut newf);
        let parent = Parent::Caller(code, Box::new(newf), dst);
        self.head.set_parent(parent);
    }

    pub fn push_tailcall(&mut self, func: Fref, args: Struple2<Val>)
    {
        self.head.function = func;
        self.head.pc = 0;
        self.head.e = Env::with_args(args);
        self.head.trace = self.head.push_frame_trace(0);
    }

    pub fn execute_leema_frame(&mut self, ops: &OpVec) -> Lresult<Event>
    {
        let mut e = Event::Uneventful;
        while let Event::Uneventful = e {
            e = self.execute_leema_op(ops)?;
        }
        Ok(e)
    }

    pub fn execute_leema_op(&mut self, ops: &OpVec) -> Lresult<Event>
    {
        let opc = self.head.pc as usize;
        let op = ops.get(opc).unwrap();
        vout!("exec: {:?}\n", op);
        let result = match op {
            &Op::ConstVal(ref dst, ref v) => self.execute_const_val(*dst, v),
            &Op::Copy(dst, src) => self.execute_copy(dst, src),
            &Op::Jump(jmp) => self.execute_jump(jmp),
            &Op::JumpIfNot(jmp, reg) => self.execute_jump_if_not(jmp, reg),
            &Op::IfFailure(src, jmp) => self.execute_if_failure(src, jmp),
            &Op::MatchPattern(ref dst, ref patt, ref input) => {
                self.execute_match_pattern(*dst, patt, *input)
            }
            &Op::ListCons(dst, head, tail) => {
                self.execute_cons_list(dst, head, tail)
            }
            &Op::StrCat(dst, src) => self.execute_strcat(dst, src),
            &Op::ApplyFunc(dst, func, lineno) => {
                self.execute_call(dst, func, lineno)
            }
            &Op::Return => Ok(Event::Success),
            &Op::SetResult(dst) => {
                if dst == Reg::Void {
                    return Err(rustfail!(
                        "leema_failure",
                        "return void at {} in {:?}",
                        self.head.pc,
                        ops,
                    ));
                }
                let result = ltry!(self.head.e.get_reg(dst)).clone();
                self.head.parent.set_result(result);
                self.head.pc += 1;
                Ok(Event::Uneventful)
            }
            &Op::PropagateFailure(src, lineno) => {
                let ev = self.propagate_failure(src, lineno);
                self.head.pc += 1;
                ev
            }
        };
        result.map_err(|f| f.add_context(lstrf!("pc: {}", opc)))
    }

    pub fn execute_strcat(&mut self, dstreg: Reg, srcreg: Reg)
        -> Lresult<Event>
    {
        let result = {
            let dst = ltry!(self.head.e.get_reg(dstreg));
            let src = ltry!(self.head.e.get_reg(srcreg));
            match (dst, src) {
                (&Val::Future(_), _) => {
                    // oops, not ready to do this yet, let's bail and wait
                    return Ok(Event::FutureWait(dstreg.clone()));
                }
                (_, &Val::Future(_)) => {
                    // oops, not ready to do this yet, let's bail and wait
                    return Ok(Event::FutureWait(srcreg.clone()));
                }
                _ => Val::Str(Lstr::from(format!("{}{}", dst, src))),
            }
        };
        self.head.e.set_reg(dstreg, result);
        self.head.pc += 1;
        Ok(Event::Uneventful)
    }

    pub fn execute_match_pattern(
        &mut self,
        dst: Reg,
        patt: &Val,
        input: Reg,
    ) -> Lresult<Event>
    {
        vout!(
            "execute_match_pattern({:?}, {:?}, {:?})\n",
            dst,
            patt,
            input
        );
        let matches = {
            let ival = ltry!(self.head.e.get_reg(input));
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
                            self.head.e.set_reg(pdst, v);
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
        Ok(Event::Uneventful)
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
        dst: Reg,
        freg: Reg,
        line: u16,
    ) -> Lresult<Event>
    {
        let (fref, args): (Fref, Struple2<Val>) = {
            let ref fname_val = ltry!(self.head.e.get_reg(freg));
            match *fname_val {
                &Val::Call(ref f, ref args) => (f.clone(), args.clone()),
                _ => {
                    return Err(rustfail!(
                        "failure",
                        "that's not a function! {:?}",
                        fname_val,
                    ));
                }
            }
        };
        vout!("execute_call({})\n", fref);

        Ok(Event::Call(dst.clone(), line as i16, fref, args))
    }

    pub fn execute_const_val(&mut self, reg: Reg, v: &Val) -> Lresult<Event>
    {
        self.head.e.set_reg(reg, v.clone());
        self.head.pc += 1;
        Ok(Event::Uneventful)
    }

    pub fn execute_construct_enum(
        &mut self,
        reg: Reg,
        new_typ: &Type,
        variant: &Lstr,
        flds: &Struple2<Type>,
    ) -> Lresult<Event>
    {
        let items = self.head.e.get_params();
        let construple = if let Type::User(_mname, _fname) = new_typ {
            let new_items = items
                .iter()
                .zip(flds.iter())
                .map(|(i, f)| {
                    if i.k.is_some() {
                        StrupleItem::new(i.k.clone(), i.v.clone())
                    } else {
                        StrupleItem::new(f.k.clone(), i.v.clone())
                    }
                })
                .collect();
            Val::EnumStruct(new_typ.clone(), variant.clone(), new_items)
        } else {
            return Err(rustfail!(
                "leema_failure",
                "struct type is not user defined: {:?}",
                new_typ,
            ));
        };

        self.head.e.set_reg(reg, construple);
        self.head.pc = self.head.pc + 1;
        Ok(Event::Uneventful)
    }

    pub fn execute_construple(
        &mut self,
        reg: Reg,
        new_typ: &Type,
        flds: &Struple2<Type>,
    ) -> Lresult<Event>
    {
        let items = self.head.e.get_params();
        let construple = if let Type::User(_, _) = new_typ {
            let new_items = items
                .iter()
                .zip(flds.iter())
                .map(|(i, f)| {
                    if i.k.is_some() {
                        StrupleItem::new(i.k.clone(), i.v.clone())
                    } else {
                        StrupleItem::new(f.k.clone(), i.v.clone())
                    }
                })
                .collect();
            Val::Struct(new_typ.clone(), new_items)
        } else {
            return Err(rustfail!(
                "leema_failure",
                "struct type is not user defined: {:?}",
                new_typ,
            ));
        };

        self.head.e.set_reg(reg, construple);
        self.head.pc = self.head.pc + 1;
        Ok(Event::Uneventful)
    }

    pub fn execute_cons_list(
        &mut self,
        dst: Reg,
        head: Reg,
        tail: Reg,
    ) -> Lresult<Event>
    {
        let new_list = {
            let headval = ltry!(self.head.e.get_reg(head)).clone();
            let tailval = ltry!(self.head.e.get_reg(tail)).clone();
            list::cons(headval, tailval)
        };
        self.head.e.set_reg(dst, new_list);
        self.head.pc += 1;
        Ok(Event::Uneventful)
    }

    pub fn execute_create_list(&mut self, dst: Reg) -> Lresult<Event>
    {
        self.head.e.set_reg(dst, list::empty());
        self.head.pc = self.head.pc + 1;
        Ok(Event::Uneventful)
    }

    pub fn execute_create_map(&mut self, dst: Reg) -> Lresult<Event>
    {
        self.head.e.set_reg(dst, Val::Map(Lmap::new()));
        self.head.pc = self.head.pc + 1;
        Ok(Event::Uneventful)
    }

    pub fn execute_create_tuple(
        &mut self,
        dst: Reg,
        ref sz: i8,
    ) -> Lresult<Event>
    {
        let tupsize: usize = *sz as usize;
        self.head.e.set_reg(dst, Val::new_tuple(tupsize));
        self.head.pc = self.head.pc + 1;
        Ok(Event::Uneventful)
    }

    pub fn execute_jump(&mut self, jmp: i16) -> Lresult<Event>
    {
        self.head.pc += jmp as i32;
        Ok(Event::Uneventful)
    }

    pub fn execute_jump_if_not(&mut self, jmp: i16, reg: Reg)
        -> Lresult<Event>
    {
        vout!("execute_jump_if_not({:?},{:?})\n", jmp, reg);
        let tjump: i32 = {
            match ltry!(self.head.e.get_reg(reg)) {
                &Val::Bool(test) => {
                    if test {
                        vout!("if test is true\n");
                        1
                    } else {
                        vout!("if test is false\n");
                        jmp as i32
                    }
                }
                // EnumToken(Type, Lstr),
                &Val::EnumToken(ref typ, ref var) if *typ == Type::BOOL => {
                    if var.as_str() == "True" {
                        vout!("if test is True\n");
                        1
                    } else {
                        vout!("if test is False\n");
                        jmp as i32
                    }
                }
                unexpected => {
                    return Err(rustfail!(
                        "type_failure",
                        "can't if check a not bool {:?}",
                        unexpected,
                    ));
                }
            }
        };
        self.head.pc += tjump;
        Ok(Event::Uneventful)
    }

    pub fn execute_if_failure(&mut self, src: Reg, jmp: i16) -> Lresult<Event>
    {
        if self.head.e.get_reg(src)?.is_failure() {
            self.head.pc += 1;
        } else {
            self.head.pc += jmp as i32;
        }
        Ok(Event::Uneventful)
    }

    pub fn execute_copy(&mut self, dst: Reg, src: Reg) -> Lresult<Event>
    {
        let src_val = ltry!(self.head.e.get_reg(src)).clone();
        self.head.e.set_reg(dst, src_val);
        self.head.pc = self.head.pc + 1;
        Ok(Event::Uneventful)
    }

    pub fn propagate_failure(&mut self, src: Reg, line: u16) -> Lresult<Event>
    {
        let srcval = ltry!(self.head.e.get_reg(src));
        match srcval {
            &Val::Failure2(ref failure) => {
                let new_trace = FrameTrace::propagate_down(
                    failure.trace.as_ref().unwrap(),
                    &self.head.function,
                    line as i16,
                );
                Err(Failure::leema_new(
                    failure.tag.clone(),
                    failure.msg.clone(),
                    Some(new_trace),
                    failure.code,
                ))
            }
            _ => Ok(Event::Uneventful),
        }
    }

    fn call_arg_failure(args: &Struple2<Val>) -> Option<&Val>
    {
        for i in args.iter() {
            if i.v.is_failure() {
                return Some(&i.v);
            }
        }
        None
    }
}


#[cfg(test)]
mod tests
{
    use crate::leema::fiber::Fiber;
    use crate::leema::frame::{Event, Frame, Parent};
    use crate::leema::lstr::Lstr;
    use crate::leema::reg::Reg;
    use crate::leema::val::{Fref, Val};


    #[test]
    fn test_normal_strcat()
    {
        let r1 = Reg::local(1);
        let r2 = Reg::local(2);
        let main_parent = Parent::new_main();
        let callri = Fref::with_modules(From::from("foo"), "bar");
        let mut frame = Frame::new_root(main_parent, callri, Vec::new());
        frame.e.set_reg(r1, Val::Str(Lstr::Sref("i like ")));
        frame.e.set_reg(r2, Val::Str(Lstr::Sref("burritos")));
        let mut fib = Fiber::spawn(1, frame);

        let event = fib.execute_strcat(r1, r2).unwrap();
        assert_eq!(Event::Uneventful, event);
    }
}
