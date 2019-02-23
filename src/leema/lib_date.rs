/// Date module
///
/// Date from unix in UTC
/// Date from unix but not UTC
/// Date from unix in UTC, convert to TZ
use leema::code::Code;
// use leema::fiber::Fiber;
use leema::frame::Event;
use leema::lstr::Lstr;
use leema::val::Val;
use leema::worker::RustFuncContext;

// use std::time::{Duration, SystemTime, UNIX_EPOCH};


pub fn lib_from_unix(mut ctx: RustFuncContext) -> Event
{
    let result = match ctx.get_param(0) {
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

pub fn lib_today(mut ctx: RustFuncContext) -> Event
{
    // let now = time::now();
    ctx.set_result(Val::Int(0));
    Event::success()
}

pub fn load_rust_func(func_name: &str) -> Option<Code>
{
    match func_name {
        "from_unix" => Some(Code::Rust2(lib_from_unix)),
        "today" => Some(Code::Rust2(lib_today)),
        _ => None,
    }
}
