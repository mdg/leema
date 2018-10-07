use leema::code::Code;
use leema::fiber::Fiber;
use leema::frame::Event;


pub fn start(f: &mut Fiber) -> Event
{
    // let child_key = f.new_task_key();
    let call = f.head.e.get_param(0);
    Event::NewTask(call.clone())
}

pub fn load_rust_func(func_name: &str) -> Option<Code>
{
    match func_name {
        "start" => Some(Code::Rust(start)),
        _ => None,
    }
}
