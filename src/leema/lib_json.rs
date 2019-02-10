use leema::code::Code;
use leema::frame::Event;
use leema::list;
use leema::lmap::{Lmap, LmapNode};
use leema::lstr::Lstr;
use leema::val::{Type, Val};
use leema::worker::RustFuncContext;

use serde::ser::{Serialize, SerializeMap, SerializeSeq};
use serde::Serializer;
use serde_json::{self, Value};


impl Serialize for Val
{
    fn serialize<S>(&self, ser: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Val::Bool(b) => ser.serialize_bool(*b),
            Val::Int(i) => ser.serialize_i64(*i),
            Val::Str(s) => ser.serialize_str(s),
            Val::Hashtag(s) => {
                let tag = format!("#{}", s);
                ser.serialize_str(&tag)
            }
            Val::Map(m) => {
                let mut mser = ser.serialize_map(None)?;
                serialize_lmap(m, &mut mser)?;
                mser.end()
            }
            Val::Cons(_, _) => ser.collect_seq(list::iter(self)),
            Val::Nil => {
                let ss = ser.serialize_seq(Some(0))?;
                ss.end()
            }
            Val::Struct(_, ref flds) => {
                let mut sser = ser.serialize_map(Some(flds.0.len()))?;
                for f in flds.0.iter() {
                    sser.serialize_entry(f.0.as_ref().unwrap().str(), &f.1)?;
                }
                sser.end()
            }
            _ => {
                panic!("cannot json serialize: {:?}", self);
            }
        }
    }
}

fn serialize_lmap<S>(m: &LmapNode, s: &mut S) -> Result<(), S::Error>
where
    S: SerializeMap,
{
    match m {
        None => {
            // all done
            Ok(())
        }
        Some(lm) => {
            match **lm {
                Lmap(ref left, (ref key, ref val), ref right) => {
                    serialize_lmap(left, s)?;
                    s.serialize_entry(key, val)?;
                    serialize_lmap(right, s)
                }
            }
        }
    }
}

pub fn decode(mut ctx: RustFuncContext) -> Event
{
    let result: Val = {
        let text = ctx.get_param(0).str();
        let fri = ctx.current_fri();
        // previous typechecking should assure that the type param is
        // there and that there will be exactly 1
        match fri.params.as_ref().unwrap()[0] {
            Type::Bool => {
                let b = serde_json::from_str(text).unwrap();
                Val::Bool(b)
            }
            Type::Int => {
                let i = serde_json::from_str(text).unwrap();
                Val::Int(i)
            }
            Type::Str => {
                let v: String = serde_json::from_str(text).unwrap();
                Val::Str(Lstr::from(v))
            }
            Type::Hashtag => {
                let s: String = serde_json::from_str(text).unwrap();
                Val::Hashtag(Lstr::from(s))
            }
            ref bad_type => {
                panic!("cannot decode json into type: {}", bad_type);
            }
        }
    };
    ctx.set_result(result);
    Event::success()
}

pub fn json_to_leema(jv: Value) -> Val
{
    match jv {
        Value::Bool(b) => {
            Val::Bool(b)
        }
        Value::Number(num) => {
            if num.is_i64() {
                Val::Int(num.as_i64().unwrap())
            } else {
                Val::Failure2(Box::new(rustfail!(
                    "json_failure",
                    "unknown number format: {:?}",
                    num,
                )))
            }
        }
        Value::String(s) => {
            Val::Str(Lstr::from(s))
        }
        Value::Null => {
            panic!("What to do with json nulls?");
        }
        Value::Array(items) => {
            // array stuff
            items
                .into_iter()
                .rev()
                .fold(Val::Nil, |acc, i| {
                    let lv = json_to_leema(i);
                    list::cons(lv, acc)
                })
        }
        Value::Object(jitems) => {
            // object stuff
            let litems = jitems
                .into_iter()
                .fold(Lmap::new(), |acc, i| {
                    let (k, v) = i;
                    let lv = json_to_leema(v);
                    let lk = Val::Str(Lstr::from(k));
                    Lmap::insert(&acc, lk, lv)
                });
            Val::Map(litems)
        }
    }
}

pub fn decode_val(mut ctx: RustFuncContext) -> Event
{
    let result: Val = {
        let text = ctx.get_param(0).str();
        // previous typechecking should assure that the type param is
        // there and that there will be exactly 1
        let json_val: Value = serde_json::from_str(text)
            .expect("invalid json");
        json_to_leema(json_val)
    };
    ctx.set_result(result);
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
        "decode_val" => Some(Code::Rust2(decode_val)),
        "encode" => Some(Code::Rust2(encode)),
        _ => None,
    }
}
