use crate::leema::failure::{self, Lresult};
use crate::leema::reg::{Ireg, Reg};
use crate::leema::struple::{Struple2, Struple2Slice, StrupleItem};
use crate::leema::val::{Fref, Val};

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
        let result: *mut Val = &mut self.data.last_mut().unwrap().v;

        // push func
        self.data.push(StrupleItem::new_v(Val::Func(f)));
        let func: *mut Val = &mut self.data.last_mut().unwrap().v;

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
        self.param_frame()
            .get(p as usize)
            .map(|p| &p.v)
            .ok_or_else(|| {
                lfail!(
                    failure::Mode::RuntimeLeemaFailure,
                    "invalid param index",
                    "param": ldisplay!(p),
                )
            })
    }

    /**
     * handy accessor function when calling from rust native functions
     */
    pub fn get_params(&self) -> &Struple2Slice<Val>
    {
        self.param_frame()
    }

    pub fn get_reg(&self, r: Reg) -> Lresult<&Val>
    {
        match r {
            Reg::Param(Ireg::Reg(i)) => self.get_param(i),
            Reg::Local(Ireg::Reg(i)) => {
                self.local_frame().get(i as usize).map(|p| &p.v).ok_or_else(
                    || {
                        lfail!(
                            failure::Mode::RuntimeLeemaFailure,
                            "no locals allocated"
                        )
                    },
                )
            }
            Reg::Stack(Ireg::Reg(i)) => {
                self.stack_frame().get(i as usize).map(|p| &p.v).ok_or_else(
                    || {
                        lfail!(
                            failure::Mode::RuntimeLeemaFailure,
                            "no stack allocation",
                            "reg": ldisplay!(r),
                            "stack_size": ldisplay!(self.stack_data().len()),
                            "sp": ldisplay!(self.sp),
                        )
                    },
                )
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

    pub fn set_reg(&mut self, r: Reg, v: Val) -> Lresult<()>
    {
        match r {
            Reg::Local(i) => {
                let dst = self
                    .local_frame()
                    .get_mut(i.get_primary() as usize)
                    .map(|p| &mut p.v)
                    .ok_or_else(|| {
                        lfail!(
                            failure::Mode::RuntimeLeemaFailure,
                            "no locals allocated"
                        )
                    })?;
            }
            Reg::Stack(i) => {
                let dst = self
                    .stack_frame()
                    .get_mut(i.get_primary() as usize)
                    .map(|p| &p.v)
                    .ok_or_else(|| {
                        lfail!(
                            failure::Mode::RuntimeLeemaFailure,
                            "no stack allocation to set",
                            "reg": ldisplay!(r),
                            "stack_size": ldisplay!(self.stack_data().len()),
                            "sp": ldisplay!(self.sp),
                        )
                    })?;
                // dst.ireg_set(
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
                return Err(lfail!(
                    failure::Mode::RuntimeLeemaFailure,
                    "cannot set register",
                    "reg": ldisplay!(r),
                ));
            }
        }
        Ok(())
    }

    pub fn stack_data(&self) -> &Struple2Slice<Val>
    {
        &unsafe { &*self.stack }.data
    }

    fn param_frame(&self) -> &Struple2Slice<Val>
    {
        self.stack_data()[self.paramp..self.localp]
    }

    fn local_frame(&self) -> &Struple2Slice<Val>
    {
        self.stack_data()[self.localp..self.stackp]
    }

    fn stack_frame(&self) -> &Struple2Slice<Val>
    {
        self.stack_data()[self.stackp..]
    }
}
