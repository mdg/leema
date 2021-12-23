use crate::leema::failure::{self, Lresult};
use crate::leema::reg::{Ireg, Reg};
use crate::leema::struple::{Struple2, Struple2Slice, StrupleItem};
use crate::leema::val::{Fref, Val};

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
    pub fn new(size: usize, f: Fref, args: Struple2<Val>) -> (Buffer, Ref)
    {
        let mut b = Buffer {
            data: Vec::with_capacity(size),
        };
        let frame = b.push_frame_args(f, args);
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
            result,
            func,
            subj: None,
            param: &self.data[arg_0..self.data.len()],
            local: None,
            result_index,
            stack_base,
        }
    }
}

#[derive(Debug)]
pub struct Ref
{
    stack: *mut Buffer,
    result: *mut Val,
    func: *const Val,
    subj: Option<*mut Val>,
    param: *const Struple2Slice<Val>,
    local: Option<*mut Struple2Slice<Val>>,
    result_index: usize,
    stack_base: usize,
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
        let base = self.stack_data().len();
        let new_size = base + num;
        let stack_ref = unsafe { &mut *self.stack };
        stack_ref
            .data
            .resize(new_size, StrupleItem::new_v(Val::VOID));
        self.local = Some(&mut stack_ref.data[base..new_size]);
        self.stack_base = self.stack_data().len();
    }

    pub fn pop_frame(self)
    {
        let stack: &mut Buffer = unsafe { &mut *self.stack };
        stack.data.truncate(self.result_index + 1);
    }

    /**
     * handy accessor function when calling from rust native functions
     */
    pub fn get_param(&self, p: i8) -> Lresult<&Val>
    {
        unsafe { &*self.param }
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
        unsafe { &*self.param }
    }

    pub fn get_reg(&self, r: Reg) -> Lresult<&Val>
    {
        match r {
            Reg::Param(Ireg::Reg(i)) => {
                Ok(&unsafe { &*self.param }.get(i as usize).unwrap().v)
            }
            Reg::Local(Ireg::Reg(i)) => {
                if let Some(local) = self.local {
                    Ok(&unsafe { &*local }.get(i as usize).unwrap().v)
                } else {
                    Err(lfail!(
                        failure::Mode::RuntimeLeemaFailure,
                        "no locals allocated"
                    ))
                }
            }
            Reg::Stack(Ireg::Reg(i)) => {
                let index: usize = self.stack_base + (i as usize);
                Ok(&self.stack_data().get(index).unwrap().v)
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
                let primary = i.get_primary() as usize;
                if let Some(plocal) = self.local {
                    let local = unsafe { &mut *plocal };
                    if primary >= local.len() {
                        return Err(lfail!(
                            failure::Mode::RuntimeLeemaFailure,
                            "cannot set local overflow",
                            "reg": ldisplay!(r),
                        ));
                    }
                    local[primary].v = v;
                } else {
                    return Err(lfail!(
                        failure::Mode::RuntimeLeemaFailure,
                        "cannot set unallocated local",
                        "stack_size": ldisplay!(self.stack_data().len()),
                    ));
                }
            }
            Reg::Stack(i) => {
                let primary = i.get_primary() as usize;
                let stack_data = &mut unsafe { &mut *self.stack }.data;
                if self.stack_base + primary >= stack_data.len() {
                    return Err(lfail!(
                        failure::Mode::RuntimeLeemaFailure,
                        "cannot set stack overflow",
                        "stack_base": ldisplay!(self.stack_base),
                        "stack_size": ldisplay!(stack_data.len()),
                        "reg": ldisplay!(r),
                    ));
                }
                stack_data[self.stack_base + primary].v = v;
                // self.stack.ireg_set(i, v);
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
}
