use leema::code::Code;
use leema::frame::Event;
use leema::io::RunQueueReceiver;
use leema::lstr::Lstr;
use leema::rsrc;
use leema::val::{Type, Val};
use leema::worker::RustFuncContext;

use futures::future::Future;


#[derive(Debug)]
struct Lfuture
{
    receiver: RunQueueReceiver,
}

impl rsrc::Rsrc for Lfuture
{
    fn get_type(&self) -> Type
    {
        Type::future(Type::Var(Lstr::Sref("T")))
    }
}


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

pub fn start_fork(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    vout!("lib_task::start_fork()\n");
    let receiver: RunQueueReceiver = match ctx.take_param(0).unwrap() {
        Val::FuncRef(fri, args, _) => {
            let runq = ctx.clone_run_queue();
            runq.spawn(fri, args)
        }
        not_func => {
            panic!("start fork parameter is not a func: {:?}", not_func);
        }
    };
    let future_rsrc = Lfuture { receiver };
    rsrc::Event::NewRsrc(Box::new(future_rsrc), None)
}

pub fn join_fork(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    let receiver: Lfuture = ctx.take_rsrc();
    let rfut = receiver.receiver
        .map(|result| {
            rsrc::Event::Result(result, None)
        })
        .map_err(|e| {
            rsrc::Event::Result(e, None)
        });
    rsrc::Event::Future(Box::new(rfut))
}

pub fn load_rust_func(func_name: &str) -> Option<Code>
{
    match func_name {
        "start" => Some(Code::Rust2(start)),
        "start_fork" => Some(Code::Iop(start_fork, None)),
        "join_fork" => Some(Code::Iop(join_fork, Some(0))),
        _ => None,
    }
}
