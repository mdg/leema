
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
    Future(Box<future::Future<Item=(Val, Option<Box<Rsrc>>), Error=Val>>),
    NewRsrc(Box<Rsrc>),
    Success(Option<Box<Rsrc>>),
    Failure(Option<Box<Rsrc>>),
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

    pub fn new_rsrc(&mut self, rsrc: Box<Rsrc>) -> i64
    {
        self.rcio.borrow_mut().new_rsrc(rsrc)
    }

    pub fn send_result(&mut self, result: Val)
    {
        self.rcio.borrow_mut()
            .send_result(self.src_worker_id, self.src_fiber_id, result);
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

    pub fn return_rsrc(&mut self, rsrc: Box<Rsrc>)
    {
        match self.rsrc_id {
            Some(rsrc_id) => {
                self.rcio.borrow_mut().return_rsrc(rsrc_id, Some(rsrc));
            }
            None => {
                panic!("cannot return resource without resource id");
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
