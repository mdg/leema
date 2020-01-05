use crate::leema::code::Code;
use crate::leema::failure::Lresult;
use crate::leema::frame::Event;
use crate::leema::io::RunQueueReceiver;
use crate::leema::rsrc;
use crate::leema::struple::StrupleItem;
use crate::leema::val::{Type, Val};
use crate::leema::worker::RustFuncContext;

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
        // this should probably have a known type field later
        let type_args = vec![StrupleItem::new("T", Type::Unknown)];
        let futype = Box::new(user_type!("/task", "Future"));
        Type::Generic(true, futype, type_args)
    }
}


pub fn start(mut ctx: RustFuncContext) -> Lresult<Event>
{
    // let child_key = f.new_task_key();
    vout!("lib_task::start()\n");
    match ctx.get_param(0)? {
        &Val::Call(ref fref, ref args) => {
            ctx.new_task(fref.clone(), args.clone())
        }
        not_func => {
            return Err(rustfail!(
                "runtime_type_failure",
                "start fork parameter is not a func: {:?}",
                not_func,
            ));
        }
    };
    ctx.set_result(Val::Int(7));
    Event::success()
}

pub fn start_fork(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    vout!("lib_task::start_fork()\n");
    let receiver: RunQueueReceiver = match ctx.take_param(0).unwrap() {
        Val::Call(fref, args) => {
            let runq = ctx.clone_run_queue();
            runq.spawn(fref, args)
        }
        not_func => {
            panic!("start fork parameter is not a func: {:?}", not_func);
        }
    };
    let future_rsrc = Lfuture { receiver };
    rsrc::Event::NewRsrc(Box::new(future_rsrc))
}

pub fn join_fork(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    let receiver: Lfuture = ctx.take_rsrc();
    let rfut = receiver
        .receiver
        .map(|result| {
            rsrc::Event::seq(rsrc::Event::DropRsrc, rsrc::Event::Result(result))
        })
        .map_err(|e| {
            rsrc::Event::seq(rsrc::Event::DropRsrc, rsrc::Event::Result(e))
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
