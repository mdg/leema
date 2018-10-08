use leema::code::Code;
use leema::frame::Event;
use leema::struple::Struple;
use leema::val::Val;
use leema::worker::RustFuncContext;


pub fn start(mut ctx: RustFuncContext) -> Event
{
    // let child_key = f.new_task_key();
    vout!("lib_task::start()\n");
    match ctx.get_param(0) {
        &Val::FuncRef(ref fri, ref args, _) => {
            ctx.new_task(fri.clone(), args.clone())
        }
        not_func => {
            panic!("start fork parameter is not a func: {:?}", not_func);
        }
    };
    ctx.set_result(Val::Int(7));
    Event::success()
}

pub fn start_fork(mut ctx: RustFuncContext) -> Event
{
    vout!("lib_task::start_fork()\n");
    let receiver = match ctx.get_param(0) {
        &Val::FuncRef(ref fri, ref args, _) => {
            ctx.new_fork(fri.clone(), args.clone())
        }
        not_func => {
            panic!("start fork parameter is not a func: {:?}", not_func);
        }
    };
    let result = Val::Tuple(Struple(vec![
        (None, Val::Int(7)),
        (None, Val::future(receiver)),
    ]));
    ctx.set_result(result);
    Event::success()
}

pub fn load_rust_func(func_name: &str) -> Option<Code>
{
    match func_name {
        "start" => Some(Code::Rust2(start)),
        "start_fork" => Some(Code::Rust2(start_fork)),
        _ => None,
    }
}
