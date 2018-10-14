use leema::code::Code;
use leema::frame::Event;
use leema::lstr::Lstr;
use leema::val::Val;
use leema::worker::RustFuncContext;

use serde::ser::Serialize;
use serde::Serializer;
use serde_json;


impl Serialize for Val
{
    fn serialize<S>(&self, ser: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Val::Int(i) => ser.serialize_i64(*i),
            Val::Bool(b) => ser.serialize_bool(*b),
            _ => {
                panic!("cannot json serialize: {:?}", self);
            }
        }
    }
}

pub fn decode(_ctx: RustFuncContext) -> Event
{
    Event::success()
}

pub fn encode(mut ctx: RustFuncContext) -> Event
{
    let json = {
        let val = ctx.get_param(0);
        serde_json::to_string(val)
    };
    ctx.set_result(Val::Str(Lstr::from(json.unwrap())));
    Event::success()
}

pub fn load_rust_func(func_name: &str) -> Option<Code>
{
    match func_name {
        "decode" => Some(Code::Rust2(decode)),
        "encode" => Some(Code::Rust2(encode)),
        _ => None,
    }
}
