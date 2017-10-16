
use leema::code::{Code};
use leema::log;
use leema::rsrc;
use leema::val::{Val};

use std::io::{self, stderr, Write};


pub fn file_open(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    vout!("file_open()\n");
    rsrc::Event::Success(Val::Void, None)
}

pub fn file_read_file(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    vout!("file_read_file()\n");
    rsrc::Event::Success(Val::Void, None)
}

pub fn file_write(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    vout!("file_write()\n");
    rsrc::Event::Success(Val::Void, None)
}

pub fn file_write_file(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    vout!("file_write_file()\n");
    rsrc::Event::Success(Val::Void, None)
}

pub fn load_rust_func(func_name: &str) -> Option<Code>
{
    match func_name {
        "read_file" => Some(Code::Iop(file_read_file, None)),
        "write_file" => Some(Code::Iop(file_read_file, None)),
        _ => None,
    }
}
