use crate::leema::code::Code;
use crate::leema::failure::Lresult;
use crate::leema::frame;
use crate::leema::list;
use crate::leema::worker::RustFuncContext;

pub fn sort(mut ctx: RustFuncContext) -> Lresult<frame::Event>
{
    let src = ctx.get_param(0)?;
    let result = list::sort(src);
    ctx.set_result(result);
    frame::Event::success()
}

pub fn load_rust_func(func_name: &str) -> Option<Code>
{
    match func_name {
        "sort" => Some(Code::Rust2(sort)),
        _ => None,
    }
}
