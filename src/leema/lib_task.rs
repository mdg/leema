use leema::code::Code;
use leema::fiber::Fiber;
use leema::frame::Event;


pub fn detach(_f: &mut Fiber) -> Event
{
    Event::success()
}

pub fn load_rust_func(func_name: &str) -> Option<Code>
{
    match func_name {
        "detach_f" => Some(Code::Rust(detach)),
        _ => None,
    }
}
