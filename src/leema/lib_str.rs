
use leema::code::{Code, RustFunc};
use leema::fiber::{Fiber};
use leema::frame;
use leema::list;
use leema::val::{Val};


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

pub fn split(f: &mut Fiber) -> frame::Event
{
    let result = {
        let src = f.head.e.get_param(0);
        let div = f.head.e.get_param(1);
        let subs =
            src.str().rsplit(div.str()).fold(Val::Nil, |acc, s| {
                list::cons(
                    Val::new_str(s.to_string()),
                    acc
                )
            });
        subs
    };
    f.head.parent.set_result(result);
    frame::Event::success()
}


pub fn load_rust_func(func_name: &str) -> Option<Code>
{
    match func_name {
        "is_empty" => Some(Code::Rust(is_empty)),
        "len" => Some(Code::Rust(len)),
        "split" => Some(Code::Rust(split)),
        _ => None,
    }
}
