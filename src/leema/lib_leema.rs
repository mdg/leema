use crate::leema::code::Code;
use crate::leema::failure;
use crate::leema::frame;
use crate::leema::program;
use crate::leema::rsrc;
use crate::leema::worker::RustFuncContext;
use crate::leema::{Lresult, Lstr, Val};

use std::future::Future;
use std::pin::Pin;

pub fn load_method(
    mut ctx: rsrc::IopCtx,
) -> Pin<Box<dyn Future<Output = rsrc::IopCtx>>>
{
    Box::pin(async move {
        vout!("load_method()\n");
        let (meth_f, impl_f) = match (ctx.take_param(1), ctx.take_param(2)) {
            (Some(Val::Func(m)), Some(Val::Func(i))) => (m, i),
            (_, None) => {
                ctx.return_failure(lfail!(
                    failure::Mode::RuntimeLeemaFailure,
                    "implementation function is None"
                ));
                return ctx;
            }
            (None, _) => {
                ctx.return_failure(lfail!(
                    failure::Mode::RuntimeLeemaFailure,
                    "method is None"
                ));
                return ctx;
            }
            what => panic!("what is this? {:?}", what),
        };
        let prog: &mut program::Lib = iotry!(ctx, ctx.rsrc_mut(0));
        let code = iotry!(
            ctx,
            prog.load_code(&impl_f).map(|c| (*c).clone()),
            "method": ldisplay!(impl_f),
        );
        ctx.return_code(impl_f, code);
        ctx
    })
}

/// this function was lost in a laptop crash
/// not exactly sure what it's supposed to do
fn make_closure(mut ctx: RustFuncContext) -> Lresult<frame::Event>
{
    let f = ltry!(ltry!(ctx.get_param(0)).fref());
    let data = ltry!(ctx.get_param(1));
    let clos = Val::FuncWithData(f.clone(), Box::new(data.clone()));
    ctx.set_result(clos);
    frame::Event::success()
}

pub fn load_rust_func(func_name: &str) -> Option<Code>
{
    match func_name {
        "make_closure" => Some(Code::Rust2(make_closure)),
        _ => None,
    }
}
