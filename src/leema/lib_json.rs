use crate::leema::code::Code;
use crate::leema::failure::Lresult;
use crate::leema::frame::Event;
use crate::leema::list;
use crate::leema::lmap::{Lmap, LmapNode};
use crate::leema::lstr::Lstr;
use crate::leema::module::{ModKey, TypeMod};
use crate::leema::struple::StrupleItem;
use crate::leema::val::{Fref, Type, Val};
use crate::leema::worker::RustFuncContext;

use serde::ser::{Serialize, SerializeMap, SerializeSeq};
use serde::Serializer;
use serde_json::{self, Value};


const MODULE: TypeMod = canonical_typemod!("/json");
const JSON_VAL_TYPE: Type = Type::User(MODULE, "Val");

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
                let mut sser = ser.serialize_map(Some(flds.len()))?;
                for f in flds.iter() {
                    sser.serialize_entry(f.k.as_ref().unwrap().str(), &f.v)?;
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

pub fn decode(mut ctx: RustFuncContext) -> Lresult<Event>
{
    const DECODE_WITH_ARGS: i32 = 1;
    match ctx.pc() {
        0 => {
            let fref = ctx.current_fref();
            match &fref.t {
                Type::Generic(true, _, _) => {
                    panic!("generic is open: {}", fref);
                }
                Type::Generic(false, _inner, targs) => {
                    let json_type = targs.first().unwrap().v.clone();
                    let json_type2 = json_type.clone();
                    let jtval = Val::Type(json_type);
                    ctx.new_call(
                        DECODE_WITH_ARGS,
                        Fref {
                            m: ModKey::from("core"),
                            f: "type_fields",
                            t: json_type2,
                        },
                        vec![StrupleItem::new_v(jtval)],
                    )
                }
                Type::Func(_) => {
                    panic!("function is not generic: {}", fref);
                }
                _ => {
                    panic!("not a function: {}", fref);
                }
            }
        }
        DECODE_WITH_ARGS => decode_with_args(ctx),
        what => panic!("unexpected pc: {}", what),
    }
}

fn decode_with_args(mut ctx: RustFuncContext) -> Lresult<Event>
{
    let result: Val = {
        let text = ctx.get_param(0)?.str();
        let fref = ctx.current_fref();
        // previous typechecking should assure that the type param is
        // there and that there will be exactly 1
        let tparam = if let Type::Generic(_, _, args) = &fref.t {
            &args.get(0).unwrap().v
        } else {
            panic!("not a generic");
        };

        if *tparam == Type::BOOL {
            let b = serde_json::from_str(text).unwrap();
            Val::Bool(b)
        } else if *tparam == Type::INT {
            let i = serde_json::from_str(text).unwrap();
            Val::Int(i)
        } else if *tparam == Type::STR {
            let v: String = serde_json::from_str(text).unwrap();
            Val::Str(Lstr::from(v))
        } else if *tparam == Type::HASHTAG {
            let s: String = serde_json::from_str(text).unwrap();
            Val::Hashtag(Lstr::from(s))
        } else {
            return Err(rustfail!(
                "runtime_type_failure",
                "cannot decode json into type: {}",
                tparam,
            ));
        }
    };
    ctx.set_result(result);
    Event::success()
}

pub fn json_to_leema(jv: Value) -> Val
{
    match jv {
        Value::Bool(b) => new_json_val("Boolean", Val::Bool(b)),
        Value::Number(num) => {
            if num.is_i64() {
                let inner = Val::Int(num.as_i64().unwrap());
                new_json_val("Integer", inner)
            } else {
                Val::Failure2(Box::new(rustfail!(
                    "json_failure",
                    "unknown number format: {:?}",
                    num,
                )))
            }
        }
        Value::String(s) => {
            let inner = Val::Str(Lstr::from(s));
            new_json_val("String", inner)
        }
        Value::Null => {
            Val::EnumToken(JSON_VAL_TYPE.clone(), Lstr::Sref("Null"))
        }
        Value::Array(items) => {
            // array stuff
            let inner = items.into_iter().rev().fold(Val::Nil, |acc, i| {
                let lv = json_to_leema(i);
                list::cons(lv, acc)
            });
            new_json_val("Array", inner)
        }
        Value::Object(jitems) => {
            // object stuff
            let litems = jitems.into_iter().fold(Lmap::new(), |acc, i| {
                let (k, v) = i;
                let lv = json_to_leema(v);
                let lk = Val::Str(Lstr::from(k));
                Lmap::insert(&acc, lk, lv)
            });
            let inner = Val::Map(litems);
            new_json_val("Object", inner)
        }
    }
}

fn new_json_val(variant: &'static str, inner: Val) -> Val
{
    Val::EnumStruct(
        JSON_VAL_TYPE.clone(),
        Lstr::Sref(variant),
        vec![StrupleItem::new(None, inner)],
    )
}

pub fn decode_val(mut ctx: RustFuncContext) -> Lresult<Event>
{
    let result: Val = {
        let text = ctx.get_param(0)?.str();
        // previous typechecking should assure that the type param is
        // there and that there will be exactly 1
        let json_val: Value = serde_json::from_str(text)
            .map_err(|e| rustfail!("invalid_json", "{}", e))?;
        json_to_leema(json_val)
    };
    ctx.set_result(result);
    Event::success()
}

pub fn encode(mut ctx: RustFuncContext) -> Lresult<Event>
{
    let json = {
        let val = ctx.get_param(0)?;
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
