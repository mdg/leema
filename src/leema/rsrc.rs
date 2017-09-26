
use leema::val::{Val, Type};
use leema::io::{Io};

use std::fmt;
use std::collections::{HashMap};

use futures::future;
use tokio_core::reactor;
use mopa;


pub trait Rsrc
    : mopa::Any
    + fmt::Debug
{
    fn get_type(&self) -> Type;
}

mopafy!(Rsrc);

pub enum Event
{
    Future(Box<future::Future<Item=(Val, Option<Box<Rsrc>>), Error=Val>>),
    Success(Val),
    Failure(Val),
}

pub struct IopCtx<'a>
{
    io: &'a mut Io,
    src_worker_id: i64,
    src_frame_id: i64,
    rsrc_id: i64,
    /*
    resource: &'a mut HashMap<i64, Ioq>,
    next_rsrc_id: &'a mut i64,
    */
}

impl<'a> IopCtx<'a>
{
    pub fn new(io: &'a mut Io, wid: i64, fid: i64, rsrc_id: i64) -> IopCtx<'a>
    {
        IopCtx{
            io: io,
            src_worker_id: wid,
            src_frame_id: fid,
            rsrc_id: rsrc_id,
        }
    }

    pub fn handle(&mut self) -> &mut reactor::Handle
    {
        &mut self.io.handle
    }

    pub fn new_rsrc(&mut self, rsrc: Box<Rsrc>) -> i64
    {
        self.io.new_rsrc(rsrc)
    }
}

pub type IopAction = Box<fn(&mut IopCtx, Vec<Val>) -> Event>;
pub type RsrcAction = Box<fn(&mut IopCtx, Box<Rsrc>, Vec<Val>) -> Event>;
