use crate::leema::code::{Op, OpVec};
use crate::leema::failure::{self, Failure, Lresult};
use crate::leema::frame::{Event, Frame, FrameTrace};
use crate::leema::list;
use crate::leema::lmap::Lmap;
use crate::leema::lstr::Lstr;
use crate::leema::reg::Reg;
use crate::leema::stack;
use crate::leema::struple::{Struple2, StrupleItem};
use crate::leema::val::{Fref, Type, Val};

use std::pin::Pin;
use std::sync::mpsc::Sender;


#[derive(Debug)]
pub struct Fiber
{
    pub fiber_id: i64,
    pub next_task_id: i64,
    pub head: Frame,
    stack: Pin<Box<stack::Buffer>>,
    result_sender: Option<Sender<Val>>,
}

impl Fiber
{
    pub fn spawn(
        id: i64,
        stack: Pin<Box<stack::Buffer>>,
        root: Frame,
        result: Option<Sender<Val>>,
    ) -> Fiber
    {
        Fiber {
            fiber_id: id,
            next_task_id: 1,
            head: root,
            stack,
            result_sender: result,
        }
    }

    pub fn new_task_key(&mut self) -> (i64, i64)
    {
        let child = self.next_task_id;
        self.next_task_id += 1;
        (child, self.fiber_id)
    }

    pub fn push_tailcall(&mut self, func: Fref, args: Struple2<Val>)
    {
        let callv = Val::Call(func.clone(), vec![]);
        self.head.pc = 0;
        self.head.tail_call_args(callv, args);
        self.head.trace = self.head.push_frame_trace(0);
    }

    pub fn take_result_sender(&mut self) -> Option<Sender<Val>>
    {
        self.result_sender.take()
    }

    pub fn take_result(&mut self) -> Val
    {
        (*self.stack).take_result()
    }

    pub fn get_result(&self) -> &Val
    {
        (*self.stack).get_result()
    }

    pub fn execute_leema_frame(&mut self, ops: &OpVec) -> Lresult<Event>
    {
        let mut e = Event::Uneventful;
        while let Event::Uneventful = e {
            e = match self.execute_leema_op(ops) {
                Ok(success) => success,
                Err(f) => {
                    return Err(f.lstr_loc(self.head.module().best_path(), 0));
                }
            };
        }
        Ok(e)
    }

    pub fn execute_leema_op(&mut self, ops: &OpVec) -> Lresult<Event>
    {
        let opc = self.head.pc as usize;
        let op = ops.get(opc).unwrap();
        vout!("exec: {:?}\n", op);
        let result = match op {
            &Op::PushConst(ref v) => self.execute_push_const(v),
            &Op::PushReg(ref src) => self.execute_push_reg(*src),
            &Op::PopReg(ref dst) => self.execute_pop_reg(*dst),
            &Op::PopMatch(ref patt) => self.execute_pop_match(patt),
            &Op::BranchMatch(jmp, ref patt) => {
                self.execute_branch_match(jmp, patt)
            }
            &Op::BranchIf(jmp) => self.execute_branch_if(jmp),
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
            &Op::PopListCons => self.execute_pop_list_cons(),
            &Op::PopStrCat => self.execute_pop_str_cat(),
            &Op::StrCat(dst, src) => self.execute_strcat(dst, src),
            &Op::PushCall(func, lineno) => self.execute_push_call(func, lineno),
            &Op::StackPush => {
                self.head.e.stack_push(Val::VOID);
                self.head.pc += 1;
                Ok(Event::Uneventful)
            }
            &Op::Return => Ok(Event::Success),
            &Op::ReserveLocal(n, s) => {
                self.head.e.reserve_local(n as usize);
                self.head.e.reserve_stack(s as usize);
                self.head.pc = self.head.pc + 1;
                Ok(Event::Uneventful)
            }
            &Op::PushResult => {
                let result = ltry!(self.head.e.stack_pop());
                self.head.e.set_result(result);
                Ok(Event::Uneventful)
            }
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
                self.head.e.set_result(result);
                self.head.pc += 1;
                Ok(Event::Uneventful)
            }
            &Op::PropagateFailure(src, lineno) => {
                let ev = self.propagate_failure(src, lineno);
                self.head.pc += 1;
                ev
            }
            &Op::Label(lbl) => {
                return Err(lfail!(
                    failure::Mode::RuntimeLeemaFailure,
                    "unexpected label op",
                    "label": ldisplay!(lbl),
                ));
            }
        };
        Ok(ltry!(
            result,
            "pc": lstrf!("{}", opc),
            "mod": self.head.module().name.as_lstr().clone(),
            "func": Lstr::Sref(self.head.function().f),
        ))
    }

    pub fn execute_pop_str_cat(&mut self) -> Lresult<Event>
    {
        let result = {
            let dst = ltry!(self.head.e.stack_pop());
            let src = ltry!(self.head.e.stack_pop());
            Val::Str(Lstr::from(format!("{}{}", dst, src)))
        };
        self.head.e.stack_push(result);
        self.head.pc += 1;
        Ok(Event::Uneventful)
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
        ltry!(self.head.e.set_reg(dstreg, result));
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
                            ltry!(self.head.e.set_reg(pdst, v));
                        }
                    }
                }
                ltry!(self.head.e.set_reg(dst, Val::Bool(true)));
            }
            None => {
                ltry!(self.head.e.set_reg(dst, Val::Bool(false)));
            }
        }
        self.head.pc += 1;
        Ok(Event::Uneventful)
    }

    pub fn execute_push_call(
        &mut self,
        func: i16,
        lineno: i16,
    ) -> Lresult<Event>
    {
        Ok(Event::PushCall(func, lineno as i16))
    }

    pub fn execute_push_const(&mut self, v: &Val) -> Lresult<Event>
    {
        self.head.e.stack_push(v.clone());
        self.head.pc += 1;
        Ok(Event::Uneventful)
    }

    pub fn execute_push_reg(&mut self, src: Reg) -> Lresult<Event>
    {
        let v = ltry!(self.head.e.get_reg(src)).clone();
        self.head.e.stack_push(v);
        self.head.pc += 1;
        Ok(Event::Uneventful)
    }

    pub fn execute_pop_reg(&mut self, dst: Reg) -> Lresult<Event>
    {
        let v = ltry!(self.head.e.stack_pop());
        ltry!(self.head.e.set_reg(dst, v));
        self.head.pc += 1;
        Ok(Event::Uneventful)
    }

    pub fn execute_pop_match(&mut self, patt: &Val) -> Lresult<Event>
    {
        vout!("execute_pop_match({:?})\n", patt,);
        let matches = {
            let ival = ltry!(self.head.e.stack_pop());
            Val::pattern_match(patt, &ival)
        };
        if let Some(assignments) = matches {
            for a in assignments {
                match a {
                    (Reg::Param(_), _) => {
                        // don't write into param, it's already correct
                    }
                    (pdst, v) => {
                        ltry!(self.head.e.set_reg(pdst, v));
                    }
                }
            }
        }
        self.head.pc += 1;
        Ok(Event::Uneventful)
    }

    pub fn execute_branch_match(
        &mut self,
        jmp: i16,
        patt: &Val,
    ) -> Lresult<Event>
    {
        vout!("execute_branch_match({:?})\n", patt,);
        let matches = {
            let ival = ltry!(self.head.e.stack_top());
            Val::pattern_match(patt, ival)
        };
        if let Some(assignments) = matches {
            for a in assignments {
                match a {
                    (Reg::Param(_), _) => {
                        // don't write into param, it's already correct
                    }
                    (pdst, v) => {
                        ltry!(self.head.e.set_reg(pdst, v));
                    }
                }
            }
            ltry!(self.head.e.stack_pop());
            self.head.pc += 1;
        } else {
            // no match, so do the jump
            self.head.pc += jmp as i32;
        }
        Ok(Event::Uneventful)
    }

    pub fn execute_branch_if(&mut self, jmp: i16) -> Lresult<Event>
    {
        vout!("execute_branch_if({:?})\n", jmp);
        let tjump: i32 = {
            match ltry!(self.head.e.stack_pop()) {
                Val::Bool(test) => {
                    if test {
                        vout!("if test is true\n");
                        1
                    } else {
                        vout!("if test is false\n");
                        jmp as i32
                    }
                }
                // EnumToken(Type, Lstr),
                Val::EnumToken(typ, var) if typ == Type::BOOL => {
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

    pub fn execute_const_val(&mut self, reg: Reg, v: &Val) -> Lresult<Event>
    {
        ltry!(
            self.head.e.set_reg(reg, v.clone()),
            "cannot_load_constant": lstrf!("{:?}", v),
        );
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
        let construple =
            Val::EnumStruct(new_typ.clone(), variant.clone(), new_items);

        ltry!(self.head.e.set_reg(reg, construple));
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
        let construple = Val::Struct(new_typ.clone(), new_items);

        ltry!(self.head.e.set_reg(reg, construple));
        self.head.pc = self.head.pc + 1;
        Ok(Event::Uneventful)
    }

    pub fn execute_pop_list_cons(&mut self) -> Lresult<Event>
    {
        let new_list = {
            let headval = ltry!(self.head.e.stack_pop());
            let tailval = ltry!(self.head.e.stack_pop());
            list::cons(headval, tailval)
        };
        self.head.e.stack_push(new_list);
        self.head.pc += 1;
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
        ltry!(self.head.e.set_reg(dst, new_list));
        self.head.pc += 1;
        Ok(Event::Uneventful)
    }

    pub fn execute_create_list(&mut self, dst: Reg) -> Lresult<Event>
    {
        ltry!(self.head.e.set_reg(dst, list::empty()));
        self.head.pc = self.head.pc + 1;
        Ok(Event::Uneventful)
    }

    pub fn execute_create_map(&mut self, dst: Reg) -> Lresult<Event>
    {
        ltry!(self.head.e.set_reg(dst, Val::Map(Lmap::new())));
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
        ltry!(self.head.e.set_reg(dst, Val::new_tuple(tupsize)));
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
        ltry!(self.head.e.set_reg(dst, src_val));
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
                    self.head.function(),
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
    use crate::leema::frame::{Event, Frame};
    use crate::leema::lstr::Lstr;
    use crate::leema::reg::Reg;
    use crate::leema::stack;
    use crate::leema::val::{Fref, Val};


    #[test]
    fn test_normal_strcat()
    {
        let r1 = Reg::local(1);
        let r2 = Reg::local(2);
        let callri = Fref::with_modules(From::from("foo"), "bar");
        let (stack, e) = stack::Buffer::new(100, callri.clone(), Vec::new());
        let mut frame = Frame::new_root(e);
        frame.e.reserve_local(10);
        frame
            .e
            .set_reg(r1, Val::Str(Lstr::Sref("i like ")))
            .unwrap();
        frame
            .e
            .set_reg(r2, Val::Str(Lstr::Sref("burritos")))
            .unwrap();
        let mut fib = Fiber::spawn(1, stack, frame, None);

        let event = fib.execute_strcat(r1, r2).unwrap();
        assert_eq!(Event::Uneventful, event);
    }
}
