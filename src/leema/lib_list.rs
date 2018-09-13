use leema::code::Code;
use leema::fiber::Fiber;
use leema::frame;
use leema::list;


pub fn sort(f: &mut Fiber) -> frame::Event
{
    let src = f.head.e.get_param(0);
    let result = list::sort(src);
    f.head.parent.set_result(result);
    frame::Event::success()
}

pub fn load_rust_func(func_name: &str) -> Option<Code>
{
    match func_name {
        "sort" => Some(Code::Rust(sort)),
        _ => None,
    }
}
