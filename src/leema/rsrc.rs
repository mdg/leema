
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
    NewRsrc(Box<Rsrc>),
    Success(Option<Box<Rsrc>>),
    Failure(Option<Box<Rsrc>>),
}

pub struct IopCtx<'a>
{
    io: &'a mut Io,
    src_worker_id: i64,
    src_fiber_id: i64,
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
            src_fiber_id: fid,
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

    pub fn send_result(&mut self, result: Val)
    {
        self.io.send_result(self.src_worker_id, self.src_fiber_id, result);
    }

    pub fn return_rsrc(&mut self, rsrc: Box<Rsrc>)
    {
        self.io.return_rsrc(self.rsrc_id, Some(rsrc));
    }
}

pub type IopAction = Box<fn(IopCtx, Vec<Val>) -> Event>;
pub type RsrcAction = Box<fn(IopCtx, Box<Rsrc>, Vec<Val>) -> Event>;
