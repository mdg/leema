use crate::leema::code::Code;
use crate::leema::failure::{Failure, Lresult};
use crate::leema::frame;
use crate::leema::list;
use crate::leema::val;
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

pub fn load_rust_func(func_name: &str) -> Option<Code>
{
    match func_name {
        "cons" => Some(Code::Rust2(list_cons)),
        "create_failure" => Some(Code::Rust2(create_failure)),
        _ => None,
    }
}
