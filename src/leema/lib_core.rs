use crate::leema::code::Code;
use crate::leema::failure::{Failure, Lresult};
use crate::leema::frame;
use crate::leema::fiber::Fiber;
use crate::leema::list;
use crate::leema::val::{self, Val};
use crate::leema::worker::RustFuncContext;


pub fn create_failure(ctx: RustFuncContext) -> Lresult<frame::Event>
{
    let failtag = ctx.get_param(0)?;
    let failmsg = ctx.get_param(1)?;
    Err(Failure::leema_new(
        failtag.clone(),
        failmsg.clone(),
        Some(ctx.fail_here()),
        val::FAILURE_INTERNAL,
    ))
}

pub fn list_cons(mut ctx: RustFuncContext) -> Lresult<frame::Event>
{
    let head = ctx.get_param(0)?;
    let tail = ctx.get_param(1)?;
    let result = list::cons(head.clone(), tail.clone());
    ctx.set_result(result);
    frame::Event::success()
}

pub fn int_add(f: &mut Fiber) -> Lresult<frame::Event>
{
    let ic;
    {
        let a = f.head.get_param(0)?;
        let b = f.head.get_param(1)?;
        match (a, b) {
            (&Val::Int(ia), &Val::Int(ib)) => {
                ic = ia + ib;
            }
            _ => {
                panic!("wtf is all that? {:?}", (a, b));
            }
        }
    }
    f.head.parent.set_result(Val::Int(ic));
    frame::Event::success()
}

pub fn int_sub(f: &mut Fiber) -> Lresult<frame::Event>
{
    let ic;
    {
        let a = f.head.get_param(0)?;
        let b = f.head.get_param(1)?;
        match (a, b) {
            (&Val::Int(ia), &Val::Int(ib)) => {
                ic = ia - ib;
            }
            _ => {
                panic!("wtf is all that? {:?}", (a, b));
            }
        }
    }
    f.head.parent.set_result(Val::Int(ic));
    frame::Event::success()
}

pub fn int_mult(f: &mut Fiber) -> Lresult<frame::Event>
{
    let ic;
    {
        let a = f.head.get_param(0)?;
        let b = f.head.get_param(1)?;
        match (a, b) {
            (&Val::Int(ia), &Val::Int(ib)) => {
                ic = ia * ib;
            }
            _ => {
                panic!("can't multiply that! {:?}", (a, b));
            }
        }
    }
    f.head.parent.set_result(Val::Int(ic));
    frame::Event::success()
}

pub fn int_div(f: &mut Fiber) -> Lresult<frame::Event>
{
    let ic;
    {
        let a = f.head.get_param(0)?;
        let b = f.head.get_param(1)?;
        match (a, b) {
            (&Val::Int(ia), &Val::Int(ib)) => {
                ic = ia / ib;
            }
            _ => {
                panic!("can't divide that! {:?}", (a, b));
            }
        }
    }
    f.head.parent.set_result(Val::Int(ic));
    frame::Event::success()
}

pub fn int_mod(f: &mut Fiber) -> Lresult<frame::Event>
{
    let ic;
    {
        let a = f.head.get_param(0)?;
        let b = f.head.get_param(1)?;
        match (a, b) {
            (&Val::Int(ia), &Val::Int(ib)) => {
                ic = ia % ib;
            }
            _ => {
                panic!("can't mod that! {:?}", (a, b));
            }
        }
    }
    f.head.parent.set_result(Val::Int(ic));
    frame::Event::success()
}

pub fn int_negate(f: &mut Fiber) -> Lresult<frame::Event>
{
    let result;
    {
        let a = f.head.get_param(0)?;
        match a {
            &Val::Int(a) => {
                result = -a;
            }
            _ => {
                panic!("can't negate a not int? {:?}", a);
            }
        }
    }
    f.head.parent.set_result(Val::Int(result));
    frame::Event::success()
}

pub fn int_equal(mut f: RustFuncContext) -> Lresult<frame::Event>
{
    let result = {
        let pa = f.get_param(0)?;
        let pb = f.get_param(1)?;
        match (pa, pb) {
            (Val::Int(a), Val::Int(b)) => Val::Bool(a == b),
            (Val::Int(_), _) => {
                return Err(rustfail!(
                    "runtime_type_failure",
                    "parameter b is not an integer: {}",
                    pb,
                ))
            }
            (_, Val::Int(_)) => {
                return Err(rustfail!(
                    "runtime_type_failure",
                    "parameter a is not an integer: {}",
                    pa,
                ))
            }
            _ => {
                return Err(rustfail!(
                    "runtime_type_failure",
                    "equal parameters are not integers: {} == {}",
                    pa,
                    pb,
                ))
            }
        }
    };
    f.set_result(result);
    frame::Event::success()
}

pub fn int_less_than(mut f: RustFuncContext) -> Lresult<frame::Event>
{
    let result = {
        let pa = f.get_param(0)?;
        let pb = f.get_param(1)?;
        match (pa, pb) {
            (Val::Int(a), Val::Int(b)) => Val::Bool(a < b),
            (Val::Int(_), _) => {
                return Err(rustfail!(
                    "runtime_type_failure",
                    "parameter b is not an integer: {}",
                    pb,
                ))
            }
            (_, Val::Int(_)) => {
                return Err(rustfail!(
                    "runtime_type_failure",
                    "parameter a is not an integer: {}",
                    pa,
                ))
            }
            _ => {
                return Err(rustfail!(
                    "runtime_type_failure",
                    "parameters are not integers: {} < {}",
                    pa,
                    pb,
                ))
            }
        }
    };
    f.set_result(result);
    frame::Event::success()
}

pub fn load_rust_func(func_name: &str) -> Option<Code>
{
    match func_name {
        "cons" => Some(Code::Rust2(list_cons)),
        "create_failure" => Some(Code::Rust2(create_failure)),
        "int_add" => Some(Code::Rust(int_add)),
        "int_sub" => Some(Code::Rust(int_sub)),
        "int_mult" => Some(Code::Rust(int_mult)),
        "int_div" => Some(Code::Rust(int_div)),
        "int_mod" => Some(Code::Rust(int_mod)),
        "int_negate" => Some(Code::Rust(int_negate)),
        "int_equal" => Some(Code::Rust2(int_equal)),
        "int_less_than" => Some(Code::Rust2(int_less_than)),
        _ => None,
    }
}
