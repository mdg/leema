use crate::leema::code::Code;
use crate::leema::failure::Lresult;
use crate::leema::frame;
use crate::leema::val::Val;
use crate::leema::worker::RustFuncContext;

use rand;

pub fn int_random(mut ctx: RustFuncContext) -> Lresult<frame::Event>
{
    let result = rand::random::<i64>(); // as i64;
    ctx.set_result(Val::Int(result));
    frame::Event::success()
}

pub fn load_rust_func(func_name: &str) -> Option<Code>
{
    match func_name {
        "int_random" => Some(Code::Rust2(int_random)),
        _ => None,
    }
}
