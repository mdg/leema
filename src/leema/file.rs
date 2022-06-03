use crate::leema::code::Code;
use crate::leema::failure::Failure;
use crate::leema::lstr::Lstr;
use crate::leema::rsrc;
use crate::leema::val::{Type, Val};

use std::future::Future;
use std::path::Path;
use std::pin::Pin;

use tokio::fs::{File, OpenOptions};
use tokio::io::{AsyncReadExt, AsyncWriteExt};

const TYPEPATH_FILE: &str = "/file/File";
const TYPE_FILE: Type = Type::named(TYPEPATH_FILE);

pub fn file_open(ctx: rsrc::IopCtx) -> rsrc::Event
{
    vout!("file_open()\n");
    rsrc::Event::Complete(ctx)
}

pub fn file_read_file(
    mut ctx: rsrc::IopCtx,
) -> Pin<Box<dyn Future<Output = rsrc::IopCtx>>>
{
    Box::pin(async move {
        vout!("file_read_file()\n");
        let pathval = ctx.take_param(0).unwrap();
        let path = Path::new(pathval.str());
        let mut f = iotry!(
            ctx,
            File::open(path).await.map_err(|e| { Failure::from(e) })
        );
        let mut s = String::new();
        f.read_to_string(&mut s)
            .await
            .expect("read_to_string failure");
        ctx.set_result(Val::Str(Lstr::from(s)));
        ctx
    })
}

pub fn file_write(ctx: rsrc::IopCtx) -> rsrc::Event
{
    vout!("file_write()\n");
    rsrc::Event::Complete(ctx)
}

pub fn file_write_file(
    mut ctx: rsrc::IopCtx,
) -> Pin<Box<dyn Future<Output = rsrc::IopCtx>>>
{
    Box::pin(async move {
        vout!("file_write_file()\n");
        let pathval = ctx.take_param(0).unwrap();
        let output = ctx.take_param(1).unwrap();
        let path = Path::new(pathval.str());
        let mut f = iotry!(
            ctx,
            OpenOptions::new()
                .write(true)
                .create(true)
                .truncate(true)
                .open(path)
                .await
                .map_err(Failure::from)
        );
        f.write_all(output.str().as_bytes())
            .await
            .expect("write_all failure");
        ctx
    })
}

pub fn file_exists(
    mut ctx: rsrc::IopCtx,
) -> Pin<Box<dyn Future<Output = rsrc::IopCtx>>>
{
    Box::pin(async move {
        let pathval = ctx.take_param(0).unwrap();
        let exists = Path::new(pathval.str()).exists();
        ctx.set_result(Val::Bool(exists));
        ctx
    })
}

pub fn load_rust_func(func_name: &str) -> Option<Code>
{
    match func_name {
        "read_file" => Some(Code::Iop(file_read_file, None)),
        "write_file" => Some(Code::Iop(file_write_file, None)),
        "exists" => Some(Code::Iop(file_exists, None)),
        _ => None,
    }
}
