use crate::leema::code::Code;
use crate::leema::failure::Lresult;
use crate::leema::fiber::Fiber;
use crate::leema::frame;
use crate::leema::list;
use crate::leema::lstr::Lstr;
use crate::leema::val::Val;
use crate::leema::worker::RustFuncContext;

pub fn len(f: &mut Fiber) -> Lresult<frame::Event>
{
    let result = {
        let src = f.head.e.get_param(0)?;
        Val::Int(src.str().len() as i64)
    };
    f.head.e.set_result(result);
    frame::Event::success()
}

pub fn is_empty(f: &mut Fiber) -> Lresult<frame::Event>
{
    let src = f.head.e.get_param(0)?;
    let empty = src.str().is_empty();
    f.head.e.set_result(Val::Bool(empty));
    frame::Event::success()
}

pub fn join(f: &mut Fiber) -> Lresult<frame::Event>
{
    let src = f.head.e.get_param(0)?;
    let total_len = list::fold_ref(0, src, |tlen, s| tlen + s.str().len());
    f.head.e.set_result(Val::Int(total_len as i64));
    frame::Event::success()
}

pub fn libstr_replace(mut ctx: RustFuncContext) -> Lresult<frame::Event>
{
    let result = {
        let src_val = ctx.get_param(0)?;
        let from_val = ctx.get_param(1)?;
        let to_val = ctx.get_param(2)?;
        match (src_val, from_val, to_val) {
            (Val::Str(src), Val::Str(from), Val::Str(to)) => {
                let src_str: &str = src.str();
                let result_str = src_str.replace(from.str(), &to);
                Val::Str(Lstr::from(result_str))
            }
            _ => {
                return Err(rustfail!(
                    "runtime_type_failure",
                    "str::replace parameters are not strings",
                ));
            }
        }
    };
    ctx.set_result(result);
    frame::Event::success()
}

pub fn split(f: &mut Fiber) -> Lresult<frame::Event>
{
    let result = {
        let src = f.head.e.get_param(0)?;
        let div = f.head.e.get_param(1)?;
        src.str().rsplit(div.str()).fold(Val::Nil, |acc, s| {
            list::cons(Val::Str(Lstr::from(s.to_string())), acc)
        })
    };
    f.head.e.set_result(result);
    frame::Event::success()
}

pub fn to_lowercase(mut ctx: RustFuncContext) -> Lresult<frame::Event>
{
    let result = match ctx.get_param(0)? {
        Val::Str(ref istr) => istr.to_lowercase(),
        not_str => {
            return Err(rustfail!(
                "string_failure",
                "cannot lowercase a not-string: {}",
                not_str,
            ));
        }
    };
    ctx.set_result(Val::Str(Lstr::from(result)));
    frame::Event::success()
}

pub fn to_uppercase(mut ctx: RustFuncContext) -> Lresult<frame::Event>
{
    let result = match ctx.get_param(0)? {
        Val::Str(ref istr) => istr.to_uppercase(),
        not_str => {
            return Err(rustfail!(
                "type_failure",
                "cannot uppercase a not-string: {}",
                not_str,
            ));
        }
    };
    ctx.set_result(Val::Str(Lstr::from(result)));
    frame::Event::success()
}

pub fn load_rust_func(func_name: &str) -> Option<Code>
{
    match func_name {
        "is_empty" => Some(Code::Rust(is_empty)),
        "join" => Some(Code::Rust(join)),
        "len" => Some(Code::Rust(len)),
        "replace" => Some(Code::Rust2(libstr_replace)),
        "split" => Some(Code::Rust(split)),
        "to_lowercase" => Some(Code::Rust2(to_lowercase)),
        "to_uppercase" => Some(Code::Rust2(to_uppercase)),
        _ => None,
    }
}
