use leema::code::Code;
use leema::fiber::Fiber;
use leema::frame::Event;


pub fn detach(f: &mut Fiber) -> Event
{
    // let child_key = f.new_task_key();
    let call = f.head.e.get_param(0);
    Event::Detach(call.clone())
}

pub fn load_rust_func(func_name: &str) -> Option<Code>
{
    match func_name {
        "detach_f" => Some(Code::Rust(detach)),
        _ => None,
    }
}
