use crate::leema::code::Code;
use crate::leema::frame;
use crate::leema::worker::RustFuncContext;
use crate::leema::Lresult;

/// this function was lost in a laptop crash
/// not exactly sure what it's supposed to do
fn make_closure(_ctx: RustFuncContext) -> Lresult<frame::Event>
{
    frame::Event::success()
}

pub fn load_rust_func(func_name: &str) -> Option<Code>
{
    match func_name {
        "make_closure" => Some(Code::Rust2(make_closure)),
        _ => None,
    }
}
