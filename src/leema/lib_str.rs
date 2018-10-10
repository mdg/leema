use leema::code::Code;
use leema::fiber::Fiber;
use leema::frame;
use leema::list;
use leema::lstr::Lstr;
use leema::val::Val;
use leema::worker::RustFuncContext;


pub fn len(f: &mut Fiber) -> frame::Event
{
    let result = {
        let src = f.head.e.get_param(0);
        Val::Int(src.str().len() as i64)
    };
    f.head.parent.set_result(result);
    frame::Event::success()
}

pub fn is_empty(f: &mut Fiber) -> frame::Event
{
    let src = f.head.e.get_param(0);
    let empty = src.str().is_empty();
    f.head.parent.set_result(Val::Bool(empty));
    frame::Event::success()
}

pub fn join(f: &mut Fiber) -> frame::Event
{
    let src = f.head.e.get_param(0);
    let total_len = list::fold_ref(0, src, |tlen, s| tlen + s.str().len());
    f.head.parent.set_result(Val::Int(total_len as i64));
    frame::Event::success()
}

pub fn split(f: &mut Fiber) -> frame::Event
{
    let result = {
        let src = f.head.e.get_param(0);
        let div = f.head.e.get_param(1);
        src.str().rsplit(div.str()).fold(Val::Nil, |acc, s| {
            list::cons(Val::Str(Lstr::from(s.to_string())), acc)
        })
    };
    f.head.parent.set_result(result);
    frame::Event::success()
}

pub fn to_lowercase(mut ctx: RustFuncContext) -> frame::Event
{
    let result = match ctx.get_param(0) {
        Val::Str(ref istr) => istr.to_lowercase(),
        not_str => {
            panic!("cannot lowercase a not-string: {}", not_str);
        }
    };
    ctx.set_result(Val::Str(Lstr::from(result)));
    frame::Event::success()
}

pub fn to_uppercase(mut ctx: RustFuncContext) -> frame::Event
{
    let result = match ctx.get_param(0) {
        Val::Str(ref istr) => istr.to_uppercase(),
        not_str => {
            panic!("cannot uppercase a not-string: {}", not_str);
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
        "split" => Some(Code::Rust(split)),
        "to_lowercase" => Some(Code::Rust2(to_lowercase)),
        "to_uppercase" => Some(Code::Rust2(to_uppercase)),
        _ => None,
    }
}
