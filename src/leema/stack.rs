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

    fn push_frame_args(&mut self, f: Fref, args: Struple2<Val>) -> Ref
    {
        // push result
        let result_index = self.data.len();
        self.data.push(StrupleItem::new_v(Val::VOID));

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
            subj: false,
            paramp: arg_0,
            localp: stack_base,
            stackp: stack_base,
        }
    }
}

#[derive(Debug)]
pub struct Ref
{
    stack: *mut Buffer,
    sp: usize,
    paramp: usize,
    localp: usize,
    stackp: usize,
    subj: bool,
}

impl Ref
{
    pub fn push_frame_args(&mut self, func: Fref, args: Struple2<Val>) -> Ref
    {
        let stack: &mut Buffer = unsafe { &mut *self.stack };
        stack.push_frame_args(func, args)
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

    pub fn reserve_stack(&mut self, num: usize)
    {
        if num == 0 {
            eprintln!("cannot reserve_stack with 0 size");
            return;
        }
        let stack_ref = unsafe { &mut *self.stack };
        stack_ref
            .data
            .resize(stack_ref.data.len() + num, StrupleItem::new_v(Val::VOID));
    }

    pub fn pop_frame(self)
    {
        let stack: &mut Buffer = unsafe { &mut *self.stack };
        stack.data.truncate(self.sp + 1);
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
            Reg::Stack(i) => {
                let sf: FrameRef<'a> = self.stack_frame();
                let v: Lresult<&'a Val> = FrameRef::<'a>::ireg_get(&sf, i);
                match v {
                    Ok(r) => Ok(r),
                    Err(f) => {
                        Err(f.with_context(vec![
                            StrupleItem::new(Lstr::Sref("reg"), ldisplay!(r)),
                            StrupleItem::new(
                                Lstr::Sref("stack_size"),
                                ldisplay!(self.stack_data().len()),
                            ),
                            StrupleItem::new(
                                Lstr::Sref("sp"),
                                ldisplay!(self.sp),
                            ),
                        ]))
                    }
                }
                /*
                Ok(ltry!(sf.ireg_get(i),
                    "reg": ldisplay!(r),
                    "stack_size": ldisplay!(self.stack_data().len()),
                    "sp": ldisplay!(self.sp),
                ))
                */
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
        unsafe { &mut *self.stack }.data[0].v = v;
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
            Reg::Stack(i) => {
                Ok(ltry!(
                    self.stack_frame_mut().ireg_set(i, v),
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
                ))
            }
        }
    }
}
