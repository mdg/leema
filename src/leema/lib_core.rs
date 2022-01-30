use crate::leema::code::Code;
use crate::leema::failure::{Failure, Lresult};
use crate::leema::fiber::Fiber;
use crate::leema::frame;
use crate::leema::list;
use crate::leema::lstr::Lstr;
use crate::leema::program;
use crate::leema::rsrc;
use crate::leema::val::{self, Val};
use crate::leema::worker::RustFuncContext;


pub fn init_main(ctx: RustFuncContext) -> Lresult<frame::Event>
{
    let _prog = ctx.get_param(0)?;
    let mainf = match ctx.get_param(1)? {
        Val::Func(fref) => fref.clone(),
        unexpected => {
            return Err(rustfail!(
                "runtime_failure",
                "unexpected main call {:?}",
                unexpected,
            ));
        }
    };
    let main_args = match ctx.get_param(2)? {
        Val::Tuple(args) => args.clone(),
        unexpected => {
            return Err(rustfail!(
                "runtime_failure",
                "unexpected main args {:?}",
                unexpected,
            ));
        }
    };
    Ok(frame::Event::TailCall(mainf, main_args))
}

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
        let a = f.head.e.get_param(0)?;
        let b = f.head.e.get_param(1)?;
        match (a, b) {
            (&Val::Int(ia), &Val::Int(ib)) => {
                ic = ia + ib;
            }
            _ => {
                panic!("wtf is all that? {:?}", (a, b));
            }
        }
    }
    f.head.e.set_result(Val::Int(ic));
    frame::Event::success()
}

pub fn int_sub(f: &mut Fiber) -> Lresult<frame::Event>
{
    let ic;
    {
        let a = f.head.e.get_param(0)?;
        let b = f.head.e.get_param(1)?;
        match (a, b) {
            (&Val::Int(ia), &Val::Int(ib)) => {
                ic = ia - ib;
            }
            _ => {
                panic!("wtf is all that? {:?}", (a, b));
            }
        }
    }
    f.head.e.set_result(Val::Int(ic));
    frame::Event::success()
}

pub fn int_mult(f: &mut Fiber) -> Lresult<frame::Event>
{
    let ic;
    {
        let a = f.head.e.get_param(0)?;
        let b = f.head.e.get_param(1)?;
        match (a, b) {
            (&Val::Int(ia), &Val::Int(ib)) => {
                ic = ia * ib;
            }
            _ => {
                panic!("can't multiply that! {:?}", (a, b));
            }
        }
    }
    f.head.e.set_result(Val::Int(ic));
    frame::Event::success()
}

pub fn int_div(f: &mut Fiber) -> Lresult<frame::Event>
{
    let ic;
    {
        let a = f.head.e.get_param(0)?;
        let b = f.head.e.get_param(1)?;
        match (a, b) {
            (&Val::Int(ia), &Val::Int(ib)) => {
                ic = ia / ib;
            }
            _ => {
                panic!("can't divide that! {:?}", (a, b));
            }
        }
    }
    f.head.e.set_result(Val::Int(ic));
    frame::Event::success()
}

pub fn int_mod(f: &mut Fiber) -> Lresult<frame::Event>
{
    let ic;
    {
        let a = f.head.e.get_param(0)?;
        let b = f.head.e.get_param(1)?;
        match (a, b) {
            (&Val::Int(ia), &Val::Int(ib)) => {
                ic = ia % ib;
            }
            _ => {
                panic!("can't mod that! {:?}", (a, b));
            }
        }
    }
    f.head.e.set_result(Val::Int(ic));
    frame::Event::success()
}

pub fn int_negate(f: &mut Fiber) -> Lresult<frame::Event>
{
    let result;
    {
        let a = f.head.e.get_param(0)?;
        match a {
            &Val::Int(a) => {
                result = -a;
            }
            _ => {
                panic!("can't negate a not int? {:?}", a);
            }
        }
    }
    f.head.e.set_result(Val::Int(result));
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

pub fn boolean_not(f: &mut Fiber) -> Lresult<frame::Event>
{
    let i = f.head.e.get_param(0)?;
    if let &Val::Bool(b) = i {
        f.head.e.set_result(Val::Bool(!b));
        frame::Event::success()
    } else {
        Err(Failure::leema_new(
            Val::Hashtag(Lstr::Sref("invalid_type")),
            Val::Str(Lstr::from(format!(
                "input to not must be a boolean: {:?}",
                i
            ))),
            Some(f.head.trace.clone()),
            val::FAILURE_TYPE,
        ))
    }
}

pub fn load_code(mut ctx: rsrc::IopCtx) -> Lresult<rsrc::Event>
{
    vout!("load_code()\n");
    let prog: &mut program::Lib = ltry!(ctx.rsrc_mut(0));
    let fref = match ctx.take_param(1).unwrap() {
        Val::Func(fr) => fr,
        what => panic!("what is this? {:?}", what),
    };
    let code = ltry!(
        prog.load_code(&fref).map(|c| (*c).clone()),
        "func": ldisplay!(fref),
    );
    Ok(rsrc::Event::FoundCode(fref, code))
}

pub fn load_rust_func(func_name: &str) -> Option<Code>
{
    match func_name {
        "__init_main__" => Some(Code::Rust2(init_main)),
        "boolean_not" => Some(Code::Rust(boolean_not)),
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
