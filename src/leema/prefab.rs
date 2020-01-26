use crate::leema::code::Code;
use crate::leema::failure::{Failure, Lresult};
use crate::leema::fiber::Fiber;
use crate::leema::frame::Event;
use crate::leema::lstr::Lstr;
use crate::leema::rsrc;
use crate::leema::val::{self, Type, Val};

use std::io::{stderr, stdin, Write};
use std::time::{Duration, Instant};

use futures::future::empty;
use futures::Future;
use tokio::timer::Delay;


pub fn int_add(f: &mut Fiber) -> Lresult<Event>
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
    Event::success()
}

pub fn int_sub(f: &mut Fiber) -> Lresult<Event>
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
    Event::success()
}

pub fn int_mult(f: &mut Fiber) -> Lresult<Event>
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
    Event::success()
}

pub fn int_div(f: &mut Fiber) -> Lresult<Event>
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
    Event::success()
}

pub fn int_mod(f: &mut Fiber) -> Lresult<Event>
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
    Event::success()
}

pub fn int_negate(f: &mut Fiber) -> Lresult<Event>
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
    Event::success()
}

pub fn bool_not(f: &mut Fiber) -> Lresult<Event>
{
    let i = f.head.e.get_param(0)?;
    if let &Val::Bool(b) = i {
        f.head.parent.set_result(Val::Bool(!b));
        Event::success()
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

pub fn bool_xor(f: &mut Fiber) -> Lresult<Event>
{
    let result;
    {
        let va = f.head.get_param(0)?;
        let vb = f.head.get_param(1)?;
        match (va, vb) {
            (&Val::Bool(a), &Val::Bool(b)) => {
                result = a && !b || b && !a;
            }
            _ => {
                panic!("wtf is all that? {:?}", (va, vb));
            }
        }
    }
    f.head.parent.set_result(Val::Bool(result));
    Event::success()
}

pub fn less_than(f: &mut Fiber) -> Lresult<Event>
{
    let result = {
        let va = f.head.e.get_param(0)?;
        let vb = f.head.e.get_param(1)?;
        Val::Bool(va < vb)
    };
    f.head.parent.set_result(result);
    Event::success()
}

pub fn less_than_equal(f: &mut Fiber) -> Lresult<Event>
{
    let result = {
        let va = f.head.e.get_param(0)?;
        let vb = f.head.e.get_param(1)?;
        Val::Bool(va <= vb)
    };
    f.head.parent.set_result(result);
    Event::success()
}

pub fn equal(f: &mut Fiber) -> Lresult<Event>
{
    let result = {
        let va = f.head.e.get_param(0)?;
        let vb = f.head.e.get_param(1)?;
        Val::Bool(va == vb)
    };
    f.head.parent.set_result(result);
    Event::success()
}

pub fn greater_than(f: &mut Fiber) -> Lresult<Event>
{
    let result = {
        let va = f.head.get_param(0)?;
        let vb = f.head.get_param(1)?;
        va > vb
    };
    f.head.parent.set_result(Val::Bool(result));
    Event::success()
}

pub fn greater_than_equal(f: &mut Fiber) -> Lresult<Event>
{
    let result = {
        let va = f.head.get_param(0)?;
        let vb = f.head.get_param(1)?;
        va >= vb
    };
    f.head.parent.set_result(Val::Bool(result));
    Event::success()
}

pub fn get_type(f: &mut Fiber) -> Lresult<Event>
{
    let result: Type;
    {
        let v = f.head.get_param(0)?;
        result = v.get_type();
    }
    f.head.parent.set_result(Val::Type(result));
    Event::success()
}


pub fn leema_sleep(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    let tint = ctx.take_param(0).unwrap().to_int() as u64;
    let i = Instant::now() + Duration::from_millis(tint);
    let d = Delay::new(i)
        .map(|_| rsrc::Event::Result(Val::Void))
        .map_err(|_e| rsrc::Event::Result(Val::Int(5)));
    rsrc::Event::Future(Box::new(d))
}

pub fn leema_sleep_forever(_ctx: rsrc::IopCtx) -> rsrc::Event
{
    rsrc::Event::Future(Box::new(empty()))
}

/**
 * cin
 */
pub fn cin(f: &mut Fiber) -> Lresult<Event>
{
    vout!("cin()\n");
    let mut input = String::new();
    match stdin().read_line(&mut input) {
        Ok(_) => {
            f.head.parent.set_result(Val::Str(Lstr::from(input)));
            Event::success()
        }
        Err(_) => {
            f.head.parent.set_result(Val::failure(
                Val::Hashtag(Lstr::Sref("console_read_fail")),
                Val::Str(Lstr::EMPTY),
                f.head.trace.fail_here(),
                val::FAILURE_INTERNAL,
            ));

            Event::success()
        }
    }
}

pub fn printerr(f: &mut Fiber) -> Lresult<Event>
{
    {
        let va = f.head.get_param(0)?;
        write!(stderr(), "{}", va)
            .map_err(|e| rustfail!("io_failure", "{}", e))?;
    }
    f.head.parent.set_result(Val::Void);
    Event::success()
}


/*
macro_rules! load_rust_funcs {
    ( $fname:ident, $( $f:ident ),* ) => {
        match $fname {
            $(
            stringify!($f) => Some(Code::Rust($f)),
            )*
            _ => None,
        }
    }
}
*/

pub fn load_rust_func(func_name: &str) -> Option<Code>
{
    match func_name {
        "int_add" => Some(Code::Rust(int_add)),
        "int_sub" => Some(Code::Rust(int_sub)),
        "int_mult" => Some(Code::Rust(int_mult)),
        "int_div" => Some(Code::Rust(int_div)),
        "int_mod" => Some(Code::Rust(int_mod)),
        "int_negate" => Some(Code::Rust(int_negate)),
        "bool_not" => Some(Code::Rust(bool_not)),
        "bool_xor" => Some(Code::Rust(bool_xor)),
        "sleep" => Some(Code::Iop(leema_sleep, None)),
        "sleep_forever" => Some(Code::Iop(leema_sleep_forever, None)),
        "less_than" => Some(Code::Rust(less_than)),
        "less_than_equal" => Some(Code::Rust(less_than_equal)),
        "equal" => Some(Code::Rust(equal)),
        "greater_than" => Some(Code::Rust(greater_than)),
        "greater_than_equal" => Some(Code::Rust(greater_than_equal)),
        "get_type" => Some(Code::Rust(get_type)),
        "cin" => Some(Code::Rust(cin)),
        _ => None,
    }
}
