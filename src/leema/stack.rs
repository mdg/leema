use crate::leema::failure::{self, Lresult};
use crate::leema::lstr::Lstr;
use crate::leema::reg::{Ireg, Iregistry, Reg};
use crate::leema::struple::{Struple2, Struple2Slice, StrupleItem};
use crate::leema::val::{Fref, Val};

use std::ops::{Range, RangeFrom};
use std::pin::Pin;

/// StackBuffer stores the data for a particular fiber/task
///
/// ### Call stack handling
///
/// Push Result Reg       <-- frame base
/// Push Fref
/// Push Closed or Self
/// Push Arg0
/// ...
/// Push ArgN
/// Call(Base)   <- count up in stack for result/call
/// Expand Locals w/ Void
/// Measure diff to frame base
///
/// ### Call stack return
///
/// pop Args
/// pop Closed or Self
/// pop Fref
/// how to know how much to pop?
///
/// ### Tail call
///
/// pop Args
/// pop Closed or Self
/// pop Fref
/// push Fref
/// push Closed or Self
/// push Args
/// Call
///
#[derive(Debug)]
pub struct Buffer
{
    data: Struple2<Val>,
}

impl Buffer
{
    pub fn new(
        size: usize,
        f: Fref,
        args: Struple2<Val>,
    ) -> (Pin<Box<Buffer>>, Ref)
    {
        let mut b = Box::pin(Buffer {
            data: Vec::with_capacity(size),
        });
        let frame = (*b).push_frame_args(f, args);
        (b, frame)
    }

    pub fn take_result(&mut self) -> Val
    {
        self.data.swap_remove(0).v
    }

    pub fn get_result(&self) -> &Val
    {
        &self.data[0].v
    }

    fn push_frame_args(&mut self, f: Fref, args: Struple2<Val>) -> Ref
    {
        // push result
        let result_index = self.data.len();
        self.data
            .push(StrupleItem::new(Some(Lstr::Sref("result")), Val::VOID));

        // push func
        self.data.push(StrupleItem::new_v(Val::Func(f)));

        // push args
        let arg_0 = self.data.len();
        // push args
        for a in args.into_iter() {
            self.data.push(a);
        }
        let stack_base = self.data.len();

        let stack: *mut Buffer = self;
        Ref {
            stack,
            sp: result_index,
            paramp: arg_0,
            localp: stack_base,
            stackp: stack_base,
        }
    }
}

/// Stack frame representation
/// sp/0: result
/// 1: function
/// 2: subject - module, method, closure, subject
/// 3..lp: args
/// lp..sp: locals
/// sp..: stack, calls
#[derive(Debug)]
pub struct Ref
{
    stack: *mut Buffer,
    sp: usize,
    paramp: usize,
    localp: usize,
    stackp: usize,
}

impl Ref
{
    const RESULT_INDEX: usize = 0;
    const FUNC_INDEX: usize = 1;
    const PARAM_INDEX: usize = 2;
    const BASE_STACK_SIZE: usize = Self::PARAM_INDEX;

    pub fn push_new_call(&self, iargc: i16) -> Ref
    {
        let mut argc = iargc as usize;
        let stack: &mut Buffer = unsafe { &mut *self.stack };
        let new_sp = stack.data.len() - Self::BASE_STACK_SIZE - argc;
        let paramp = new_sp + Self::PARAM_INDEX;
        // if there's an env from a closure, push it onto the stack
        let e = stack.data.get(new_sp + Self::FUNC_INDEX).and_then(|i| {
            if let Val::FuncWithData(_, d) = &i.v {
                Some((**d).clone())
            } else {
                None
            }
        });
        if let Some(env) = e {
            stack
                .data
                .push(StrupleItem::new(Some(Lstr::Sref("env")), env));
            argc += 1;
        }
        let localp = paramp + argc;
        stack.data[new_sp].k = Some(Lstr::Sref("result"));
        Ref {
            stack: self.stack,
            sp: new_sp,
            paramp,
            localp,
            stackp: localp,
        }
    }

    /// does this need to return a Result? push doesn't return anything.
    /// looks like there's Vec::try_reserve() that will do it
    pub fn stack_push(&mut self, v: Val)
    {
        let stack_ref = unsafe { &mut *self.stack };
        stack_ref.data.push(StrupleItem::new_v(v));
    }

    pub fn stack_pop(&mut self) -> Lresult<Val>
    {
        let stack_ref = unsafe { &mut *self.stack };
        stack_ref.data.pop().map(|i| i.v).ok_or_else(|| {
            lfail!(failure::Mode::RuntimeLeemaFailure, "stack pop underflow")
        })
    }

    pub fn popn(&mut self, n: usize) -> Lresult<Struple2<Val>>
    {
        if self.stack_frame().data.len() < n {
            return Err(lfail!(
                failure::Mode::RuntimeLeemaFailure,
                "stack popn underflow",
                "n": ldisplay!(n),
                "stack_size": ldisplay!(self.stack_frame().data.len()),
                "module": self.fref()?.m.name.as_lstr().clone(),
                "function": Lstr::Sref(self.fref()?.f),
            ));
        }
        let stack_ref = &mut unsafe { &mut *self.stack }.data;
        let start = stack_ref.len() - n;
        Ok(stack_ref.drain(start..).collect())
    }

    pub fn stack_top(&self) -> Lresult<&Val>
    {
        let stack_ref = unsafe { &mut *self.stack };
        stack_ref.data.last().map(|i| &i.v).ok_or_else(|| {
            lfail!(failure::Mode::RuntimeLeemaFailure, "stack top underflow")
        })
    }

    /// get a mutable reference to the top item on the stack
    pub fn stack_top_mut(&mut self) -> Lresult<&mut Val>
    {
        self.stack_data_mut()
            .last_mut()
            .map(|i| &mut i.v)
            .ok_or_else(|| {
                lfail!(
                    failure::Mode::RuntimeLeemaFailure,
                    "stack top mut underflow"
                )
            })
    }

    pub fn reserve_local(&mut self, num: usize)
    {
        if num == 0 {
            eprintln!("cannot reserve_local with 0 size");
            return;
        }
        self.stackp = self.localp + num;
        let stack_ref = unsafe { &mut *self.stack };
        stack_ref
            .data
            .resize(self.stackp, StrupleItem::new_v(Val::VOID));
    }

    pub fn pop_frame(&mut self)
    {
        let stack: &mut Buffer = unsafe { &mut *self.stack };
        // +1 for the function result
        stack.data.truncate(self.sp + 1);
    }

    pub fn get_sp(&self) -> usize
    {
        self.sp
    }

    /**
     * handy accessor function when calling from rust native functions
     */
    pub fn get_param(&self, p: i8) -> Lresult<&Val>
    {
        Ok(ltry!(
            FrameRef::ireg_get(&self.param_frame(), Ireg::Reg(p)),
            "param": ldisplay!(p),
            "stack_size": ldisplay!(self.stack_data().len()),
            "sp": ldisplay!(self.sp),
        ))
    }

    /**
     * handy accessor function when calling from rust native functions
     */
    pub fn get_params(&self) -> &Struple2Slice<Val>
    {
        self.param_frame().data
    }

    pub fn get_reg<'a>(&'a self, r: Reg) -> Lresult<&'a Val>
    {
        match r {
            Reg::Param(i) => {
                Ok(ltry!(
                    self.param_frame().ireg_get(i),
                    "reg": ldisplay!(r),
                    "stack_size": ldisplay!(self.stack_data().len()),
                    "sp": ldisplay!(self.sp),
                ))
            }
            Reg::Local(i) => {
                Ok(ltry!(
                    self.local_frame().ireg_get(i),
                    "reg": ldisplay!(r),
                    "stack_size": ldisplay!(self.stack_data().len()),
                    "sp": ldisplay!(self.sp),
                ))
            }
            Reg::Top => {
                eprintln!("unexpected get Reg::Top");
                Ok(&self.stack_data().last().unwrap().v)
            }
            _ => {
                Err(lfail!(
                    failure::Mode::RuntimeLeemaFailure,
                    "cannot get register",
                    "reg": ldisplay!(r),
                ))
            }
        }
    }

    pub fn set_result(&mut self, v: Val)
    {
        let data: &mut Struple2<Val> = &mut unsafe { &mut *self.stack }.data;
        data[self.sp].v = v;
    }

    pub fn set_reg(&mut self, r: Reg, v: Val) -> Lresult<()>
    {
        match r {
            Reg::Local(i) => {
                Ok(ltry!(
                    self.local_frame_mut().ireg_set(i, v),
                    "reg": ldisplay!(r),
                    "stack_size": ldisplay!(self.stack_data().len()),
                    "sp": ldisplay!(self.sp),
                ))
            }
            Reg::Param(_) => {
                // debatable whether param should allow writes
                // might need to reverse this at some point and
                // copy params before writing
                // Struple.ireg_set checks bounds
                return Err(lfail!(
                    failure::Mode::RuntimeLeemaFailure,
                    "cannot set function param",
                    "reg": ldisplay!(r),
                ));
                // self.params.ireg_set(i, v)
            }
            _ => {
                Err(lfail!(
                    failure::Mode::RuntimeLeemaFailure,
                    "cannot set register",
                    "reg": ldisplay!(r),
                ))
            }
        }
    }

    pub fn stack_data(&self) -> &Struple2Slice<Val>
    {
        &unsafe { &*self.stack }.data
    }

    fn stack_data_mut(&self) -> &mut Struple2Slice<Val>
    {
        &mut unsafe { &mut *self.stack }.data
    }

    pub fn func_val<'a>(&'a self) -> &'a Val
    {
        &self.stack_data()[self.sp + Self::FUNC_INDEX].v
    }

    pub fn fref<'a>(&'a self) -> Lresult<&'a Fref>
    {
        match self.func_val() {
            Val::Func(ref fref) => Ok(fref),
            Val::FuncWithData(ref fref, _) => Ok(fref),
            other => {
                Err(lfail!(
                    failure::Mode::RuntimeLeemaFailure,
                    "expected function value",
                    "sp": ldisplay!(self.sp),
                    "value": ldebug!(other),
                ))
            }
        }
    }

    fn param_frame<'a>(&'a self) -> FrameRef<'a>
    {
        FrameRef::new("param", self.stack, self.paramp..self.localp)
    }

    fn local_frame<'a>(&'a self) -> FrameRef<'a>
    {
        FrameRef::new("local", self.stack, self.localp..self.stackp)
    }

    fn stack_frame<'a>(&'a self) -> FrameRef<'a>
    {
        FrameRef::new_from("stack", self.stack, self.stackp..)
    }

    fn result_mut<'a>(&'a self) -> FrameRef<'a>
    {
        FrameRef::new("param", self.stack, self.paramp..self.localp)
    }

    /*
    fn param_frame_mut<'a>(&mut self) -> FrameRefMut<'a>
    {
        FrameRefMut::new("param", self.stack, self.paramp..self.localp)
    }
    */

    fn local_frame_mut<'a>(&mut self) -> FrameRefMut<'a>
    {
        FrameRefMut::new("local", self.stack, self.localp..self.stackp)
    }

    fn stack_frame_mut<'a>(&mut self) -> FrameRefMut<'a>
    {
        FrameRefMut::new_from("stack", self.stack, self.stackp..)
    }
}

struct FrameRef<'a>
{
    pub data: &'a Struple2Slice<Val>,
    name: &'static str,
    range: Range<usize>,
}

impl<'a> FrameRef<'a>
{
    pub fn new(
        name: &'static str,
        stack: *const Buffer,
        r: Range<usize>,
    ) -> FrameRef<'a>
    {
        let data = &unsafe { &*stack }.data[r.clone()];
        FrameRef {
            data,
            name,
            range: r,
        }
    }

    pub fn new_from(
        name: &'static str,
        stack: *const Buffer,
        r: RangeFrom<usize>,
    ) -> FrameRef<'a>
    {
        let r_start = r.start;
        let data = &unsafe { &*stack }.data[r];
        let range_end = unsafe { &*stack }.data.len();
        FrameRef {
            data,
            name,
            range: r_start..range_end,
        }
    }

    fn ireg_get<'b>(&'b self, i: Ireg) -> Lresult<&'a Val>
    {
        match self.data.get(i.get_primary() as usize).as_mut() {
            Some(val) => {
                if let Ireg::Sub(_, sub) = i {
                    val.v.ireg_get(Ireg::Reg(sub))
                } else {
                    Ok(&val.v)
                }
            }
            None => {
                Err(lfail!(
                    failure::Mode::RuntimeLeemaFailure,
                    "cannot get invalid register",
                    "frame": Lstr::Sref(self.name),
                    "range": ldebug!(self.range),
                ))
            }
        }
    }
}

struct FrameRefMut<'a>
{
    data: &'a mut Struple2Slice<Val>,
    name: &'static str,
    range: Range<usize>,
}

impl<'a> FrameRefMut<'a>
{
    pub fn new(
        name: &'static str,
        stack: *mut Buffer,
        r: Range<usize>,
    ) -> FrameRefMut<'a>
    {
        let data = &mut unsafe { &mut *stack }.data[r.clone()];
        FrameRefMut {
            data,
            name,
            range: r,
        }
    }

    pub fn new_from(
        name: &'static str,
        stack: *mut Buffer,
        r: RangeFrom<usize>,
    ) -> FrameRefMut<'a>
    {
        let r_start = r.start;
        let data = &mut unsafe { &mut *stack }.data[r];
        let range_end = unsafe { &*stack }.data.len();
        FrameRefMut {
            data,
            name,
            range: r_start..range_end,
        }
    }

    fn ireg_set(&mut self, r: Ireg, v: Val) -> Lresult<()>
    {
        match self.data.get_mut(r.get_primary() as usize).as_mut() {
            Some(dst) => {
                if let Ireg::Sub(_, sub) = r {
                    dst.v.ireg_set(Ireg::Reg(sub), v)
                } else {
                    dst.v = v;
                    Ok(())
                }
            }
            None => {
                Err(lfail!(
                    failure::Mode::RuntimeLeemaFailure,
                    "cannot set invalid register",
                    "frame": Lstr::Sref(self.name),
                    "range": ldebug!(self.range),
                    "reg_index": ldisplay!(r.get_primary()),
                    "val": ldebug!(v),
                ))
            }
        }
    }
}
