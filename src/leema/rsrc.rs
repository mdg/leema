
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
    Future(Box<future::Future<Item=(), Error=()>>),
    Success(Val),
    Failure(Val),
}

pub type Result = Fn(Val, Box<Rsrc>);
pub type Action = fn(Box<Result>, Box<Rsrc>, Vec<Val>) -> Event;

pub struct IopCtx<'a>
{
    io: &'a mut Io,
    /*
    resource: &'a mut HashMap<i64, Ioq>,
    next_rsrc_id: &'a mut i64,
    */
}

impl<'a> IopCtx<'a>
{
    pub fn new(io: &'a mut Io) -> IopCtx<'a>
    {
        IopCtx{
            io: io,
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

pub type IopAction = fn(&mut IopCtx, Vec<Val>) -> Event;


// pub type ResourceFunc2 = fn(&mut Fiber, Val) -> frame::Event;
