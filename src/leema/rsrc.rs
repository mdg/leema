
use leema::val::{Val, Type};
use leema::io::{Io};

use std::fmt;
use std::collections::{HashMap};
use std::rc::{Rc};
use std::cell::{RefCell, RefMut};

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
    Future(Box<future::Future<Item=Event, Error=Event>>),
    NewRsrc(Box<Rsrc>),
    Success(Val, Option<Box<Rsrc>>),
    Failure(Val, Option<Box<Rsrc>>),
}

pub struct IopCtx
{
    rcio: Rc<RefCell<Io>>,
    src_worker_id: i64,
    src_fiber_id: i64,
    rsrc_id: Option<i64>,
    rsrc: Option<Box<Rsrc>>,
    params: Vec<Option<Val>>,
}

impl IopCtx
{
    pub fn new(rcio: Rc<RefCell<Io>>, wid: i64, fid: i64
        , rsrc_id: Option<i64>, rsrc: Option<Box<Rsrc>>
        , param_val: Val)
        -> IopCtx
    {
        let params = match param_val {
            Val::Tuple(items) => {
                items.into_iter().map(|i| {
                    Some(i)
                }).collect()
            }
            _ => {
                panic!("IopCtx params not a tuple");
            }
        };
        IopCtx{
            rcio: rcio,
            src_worker_id: wid,
            src_fiber_id: fid,
            rsrc_id: rsrc_id,
            rsrc: rsrc,
            params: params,
        }
    }

    pub fn handle(&self) -> reactor::Handle
    {
        self.rcio.borrow().handle.clone()
    }

    pub fn init_rsrc(&mut self, rsrc: Box<Rsrc>)
    {
        if self.rsrc_id.is_none() {
            panic!("cannot init rsrc with no rsrc_id");
        }
        if self.rsrc.is_some() {
            panic!("cannot reinitialize rsrc");
        }
        self.rsrc = Some(rsrc);
    }

    pub fn take_rsrc<T>(&mut self) -> T
        where T: Rsrc
    {
        let opt_rsrc = self.rsrc.take();
        match opt_rsrc {
            Some(rsrc) => {
                let result = rsrc.downcast::<T>();
                *(result.unwrap())
            }
            None => {
                panic!("no resource to take");
            }
        }
    }

    /**
     * Take a parameter from the context
     */
    pub fn take_param(&mut self, i: i8) -> Option<Val>
    {
        self.params.get_mut(i as usize).unwrap().take()
    }
}

pub type IopAction = fn(IopCtx) -> Event;
