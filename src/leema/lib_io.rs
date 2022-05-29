use crate::leema::code::Code;
use crate::leema::failure::Lresult;
use crate::leema::frame;
use crate::leema::val::Val;
use crate::leema::worker::RustFuncContext;

pub fn print(mut ctx: RustFuncContext) -> Lresult<frame::Event>
{
    let v = ctx.get_param(0)?;
    print!("{}", v);
    ctx.set_result(Val::VOID);
    frame::Event::success()
}

pub fn load_rust_func(func_name: &str) -> Option<Code>
{
    match func_name {
        "print" => Some(Code::Rust2(print)),
        _ => None,
    }
}
