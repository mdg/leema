use crate::leema::failure::{self, Lresult};
use crate::leema::reg::{Ireg, Reg};
use crate::leema::struple::Struple2;
use crate::leema::val::{Fref, Val};

/// StackBuffer stores the data for a particular fiber/task
///
/// ### Call stack handling
///
/// Push Result Reg       <-- frame base
/// Push Arg0
/// ...
/// Push ArgN
/// Push Closed or Self
/// Push Fref
/// Call     <- what's the call register?
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
    data: Vec<Val>,
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
        self.data.push(Val::VOID);
        let result_index = self.data.len();

        // push func
        self.data.push(Val::Func(f));
        let arg_0 = self.data.len();
        // push args
        for a in args.into_iter() {
            self.data.push(a.v);
        }

        let stack: *mut Buffer = unsafe { self };
        Ref {
            param: &self.data[arg_0..self.data.len()],
            local: &mut [],
            result_index: 1,
            stack,
        }
    }
}

pub struct Ref
{
    param: *const [Val],
    local: *mut [Val],
    result_index: usize,
    stack: *mut Buffer,
}

impl Ref
{
    pub fn push_frame_args(&mut self, func: Fref, args: Struple2<Val>) -> Ref
    {
        let stack: &mut Buffer = &mut unsafe { *self.stack };
        stack.push_frame_args(func, args)
    }

    /**
     * handy accessor function when calling from rust native functions
     */
    pub fn get_param(&self, p: i8) -> Lresult<&Val>
    {
        unsafe { *self.param }.get(p as usize).ok_or_else(|| lfail!(
            failure::Mode::RuntimeLeemaFailure,
            "invalid param index",
            "param": ldisplay!(p),
        ))
    }

    /**
     * handy accessor function when calling from rust native functions
    pub fn get_params(&self) -> &Struple2Slice<Val>
    {
        unsafe { *self.param }
    }
     */

    pub fn get_reg(&self, r: Reg) -> Lresult<&Val>
    {
        match r {
            Reg::Params(Ireg::Reg(i)) => {
                Ok(unsafe { *self.param }.get(i).unwrap())
            }
            Reg::Locals(Ireg::Reg(i)) => {
                Ok(unsafe { *self.local }.get(i).unwrap())
            }
            _ => {
                Err(lfail!(
                    failure::Mode::RuntimeLeemaFailure,
                    "cannot get register",
                    "reg": r,
                ))
            }
        }
    }

    pub fn set_reg(&mut self, r: Reg, v: Val) -> Lresult<()>
    {
        match r {
            Reg::Local(i) => {
                let primary = i.get_primary() as usize;
                if primary >= self.locals.len() {
                    self.locals.resize(primary + 1, Default::default())
                }
                self.locals.ireg_set(i, v)
            }
            Reg::Stack(i) => {
                let primary = i.get_primary() as usize;
                if primary >= self.stack.len() {
                    self.stack.resize(primary + 1, Default::default())
                }
                self.stack.ireg_set(i, v)
            }
            Reg::Param(i) => {
                // debatable whether param should allow writes
                // might need to reverse this at some point and
                // copy params before writing
                // Struple.ireg_set checks bounds
                self.params.ireg_set(i, v)
            }
            _ => {
                Err(lfail!(
                    failure::Mode::RuntimeLeemaFailure,
                    "cannot set register",
                    "reg": r,
                ))
            }
        }
        Ok(())
    }
}
