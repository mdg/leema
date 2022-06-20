use crate::leema::code::Code;
use crate::leema::frame;
use crate::leema::worker::RustFuncContext;
use crate::leema::{Lresult, Val};

/// this function was lost in a laptop crash
/// not exactly sure what it's supposed to do
fn make_closure(mut ctx: RustFuncContext) -> Lresult<frame::Event>
{
    let f = ltry!(ltry!(ctx.get_param(0)).fref());
    let data = ltry!(ctx.get_param(1));
    let clos = Val::FuncWithData(f.clone(), Box::new(data.clone()));
    ctx.set_result(clos);
    frame::Event::success()
}

pub fn load_rust_func(func_name: &str) -> Option<Code>
{
    match func_name {
        "make_closure" => Some(Code::Rust2(make_closure)),
        _ => None,
    }
}
