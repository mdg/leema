use crate::leema::code::Code;
use crate::leema::failure::Lresult;
use crate::leema::fiber::Fiber;
use crate::leema::frame;
use crate::leema::lmap::Lmap;
use crate::leema::types;
use crate::leema::val::{Type, Val};
use crate::leema::worker::RustFuncContext;


pub fn new(f: &mut Fiber) -> Lresult<frame::Event>
{
    f.head.e.set_result(Val::Map(Lmap::new()));
    frame::Event::success()
}

pub fn set(f: &mut Fiber) -> Lresult<frame::Event>
{
    let mresult = {
        let p0 = f.head.e.get_param(0)?;
        if let &Val::Map(ref m) = p0 {
            let k = f.head.e.get_param(1)?;
            let v = f.head.e.get_param(2)?;
            Lmap::insert(m, k.clone(), v.clone())
        } else {
            panic!("first param to map::set is not a map: {:?}", p0);
        }
    };
    f.head.e.set_result(Val::Map(mresult));
    frame::Event::success()
}

pub fn get(mut ctx: RustFuncContext) -> Lresult<frame::Event>
{
    let map_val = {
        let p0 = ctx.get_param(0)?;
        if let &Val::Map(ref m) = p0 {
            let k = ctx.get_param(1)?;
            Lmap::get(m, k).map(|v| v.clone())
        } else {
            panic!("first param to map::has is not a map: {:?}", p0);
        }
    };
    let result = match map_val {
        Some(inner_val) => types::new_some(inner_val),
        None => types::new_none(Type::VOID),
    };
    ctx.set_result(result);
    frame::Event::success()
}

/// Eventually maybe move this function to leema
/// once option types and type parameters are in better shape
pub fn has_key(f: &mut Fiber) -> Lresult<frame::Event>
{
    let map_has = {
        let p0 = f.head.e.get_param(0)?;
        if let &Val::Map(ref m) = p0 {
            let k = f.head.e.get_param(1)?;
            Lmap::get(m, k).is_some()
        } else {
            panic!("first param to map::has is not a map: {:?}", p0);
        }
    };
    f.head.e.set_result(Val::Bool(map_has));
    frame::Event::success()
}

pub fn len(f: &mut Fiber) -> Lresult<frame::Event>
{
    let len = {
        let p0 = f.head.e.get_param(0)?;
        if let &Val::Map(ref m) = p0 {
            Lmap::len(m)
        } else {
            panic!("first param to map::len is not a map: {:?}", p0);
        }
    };
    f.head.e.set_result(Val::Int(len as i64));
    frame::Event::success()
}

pub fn load_rust_func(func_name: &str) -> Option<Code>
{
    match func_name {
        "get" => Some(Code::Rust2(get)),
        "has_key" => Some(Code::Rust(has_key)),
        "len" => Some(Code::Rust(len)),
        "new" => Some(Code::Rust(new)),
        "set" => Some(Code::Rust(set)),
        _ => None,
    }
}
