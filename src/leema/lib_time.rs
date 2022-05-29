/// Time module
///
/// Time from unix in UTC
/// Time from unix but not UTC
/// Time from unix in UTC, convert to TZ
use crate::leema::code::Code;
use crate::leema::failure::Lresult;
use crate::leema::frame::Event;
use crate::leema::val::Val;
use crate::leema::worker::RustFuncContext;

// use std::time::{Duration, SystemTime, UNIX_EPOCH};

pub fn lib_from_unix(mut ctx: RustFuncContext) -> Lresult<Event>
{
    let result = match ctx.get_param(0)? {
        Val::Int(_unix_secs) => Val::Int(0),
        _ => {
            Val::Failure2(Box::new(rustfail!(
                "type_err",
                "from_unix param not an integer"
            )))
        }
    };
    ctx.set_result(result);
    Event::success()
}

pub fn lib_now(mut ctx: RustFuncContext) -> Lresult<Event>
{
    // let now = time::now();
    ctx.set_result(Val::Int(0));
    Event::success()
}

pub fn load_rust_func(func_name: &str) -> Option<Code>
{
    match func_name {
        "from_unix" => Some(Code::Rust2(lib_from_unix)),
        "now" => Some(Code::Rust2(lib_now)),
        _ => None,
    }
}
