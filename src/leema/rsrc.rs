use crate::leema::code::Code;
use crate::leema::failure::{self, Lresult};
use crate::leema::io::Io;
// pub use crate::leema::io::RunQueue;
use crate::leema::val::{Fref, Type, Val};

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::pin::Pin;
use std::rc::Rc;

use futures::future;
// use futures::stream;
use mopa::mopafy;


pub const ID_PROGLIB: i64 = 0;
pub const ID_INITIAL: i64 = 10;

pub trait Rsrc: mopa::Any + fmt::Debug
{
    fn get_type(&self) -> Type;
}

mopafy!(Rsrc);

pub enum Event
{
    Complete(IopCtx),
    Future(Box<dyn future::Future<Output = Event>>),
    // Stream(Box<dyn stream::Stream<Item = Event, Error = Event>>),
    NewRsrc(Box<dyn Rsrc>),
    ReturnRsrc(Box<dyn Rsrc>),
    DropRsrc,
    Result(Val),
    FoundCode(Fref, Code),
    Sequence(Box<Event>, Box<Event>),
}

impl Event
{
    pub fn seq(first: Event, second: Event) -> Event
    {
        Event::Sequence(Box::new(first), Box::new(second))
    }

    pub fn cat(self, next: Event) -> Event
    {
        Event::Sequence(Box::new(self), Box::new(next))
    }
}

impl fmt::Debug for Event
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match *self {
            Event::Complete(_) => write!(f, "Event::Complete"),
            Event::Future(_) => write!(f, "Event::Future"),
            // Event::Stream(_) => write!(f, "Event::Stream"),
            Event::NewRsrc(ref r) => write!(f, "Event::Rsrc({:?})", r),
            Event::ReturnRsrc(ref r) => write!(f, "Event::ReturnRsrc({:?})", r),
            Event::DropRsrc => write!(f, "Event::ReturnRsrc"),
            Event::Result(ref r) => write!(f, "Event::Result({:?})", r),
            Event::FoundCode(ref fc, _) => {
                write!(f, "Event::FoundCode({})", fc)
            }
            Event::Sequence(ref first, ref second) => {
                write!(f, "Event::Seq({:?}, {:?})", first, second)
            }
        }
    }
}

pub struct IopCtx
{
    rcio: Rc<RefCell<Io>>,
    src_worker_id: i64,
    src_fiber_id: i64,
    // run_queue: RunQueue,
    rsrc_id: Option<i64>,
    pub params: Vec<Option<Val>>,
    pub rsrc: HashMap<i64, Box<dyn Rsrc>>,
    result: Val,
    pub code: Option<(Fref, Code)>,
}

impl IopCtx
{
    pub fn new(
        rcio: Rc<RefCell<Io>>,
        wid: i64,
        fid: i64,
        // run_queue: RunQueue,
        rsrc_id: Option<i64>,
        param_val: Val,
    ) -> IopCtx
    {
        let params = match param_val {
            Val::Tuple(items) => items.into_iter().map(|i| Some(i.v)).collect(),
            _ => {
                panic!("IopCtx params not a tuple");
            }
        };
        IopCtx {
            rcio,
            src_worker_id: wid,
            src_fiber_id: fid,
            // run_queue,
            rsrc_id,
            params,
            rsrc: HashMap::new(),
            result: Val::VOID,
            code: None,
        }
    }

    pub fn set_result(&mut self, r: Val)
    {
        self.result = r;
    }

    pub fn return_code(&mut self, f: Fref, c: Code)
    {
        self.code = Some((f, c));
    }

    pub fn init_rsrc(&mut self, _rsrc: Box<dyn Rsrc>)
    {
        if self.rsrc_id.is_none() {
            panic!("cannot init rsrc with no rsrc_id");
        }
    }

    /**
     * Take a parameter from the context
     */
    pub fn take_param(&mut self, i: i8) -> Option<Val>
    {
        self.params.get_mut(i as usize).unwrap().take()
    }

    /// Get a resource parameter from the context
    pub fn rsrc<T>(&self, i: i8) -> Lresult<&T>
    where
        T: Rsrc,
    {
        if let Some(val) = self.params.get(i as usize).unwrap() {
            if let Val::ResourceRef(rsrc_id) = val {
                if let Some(rsrc) = self.rsrc.get(rsrc_id) {
                    if let Some(rval) = rsrc.downcast_ref::<T>() {
                        Ok(rval)
                    } else {
                        Err(lfail!(
                            failure::Mode::CodeFailure,
                            "invalid resource type",
                            "param": ldisplay!(i),
                            "rsrc_id": ldisplay!(rsrc_id),
                            "value": ldebug!(val),
                            "valtype": ldebug!(val.get_type()),
                        ))
                    }
                } else {
                    Err(lfail!(
                        failure::Mode::RuntimeLeemaFailure,
                        "unknown resource",
                        "param": ldisplay!(i),
                        "rsrc_id": ldisplay!(rsrc_id),
                    ))
                }
            } else {
                Err(lfail!(
                    failure::Mode::CodeFailure,
                    "non-resource parameter",
                    "param": ldisplay!(i),
                    "value": ldisplay!(val),
                ))
            }
        } else {
            Err(lfail!(
                failure::Mode::CodeFailure,
                "invalid parameter index",
                "param": ldisplay!(i),
            ))
        }
    }

    /// Get a mutable resource parameter from the context
    pub fn rsrc_mut<T>(&mut self, i: i8) -> Lresult<&mut T>
    where
        T: Rsrc,
    {
        if let Some(val) = self.params.get(i as usize).unwrap() {
            if let Val::ResourceRef(rsrc_id) = val {
                if let Some(rsrc) = self.rsrc.get_mut(rsrc_id) {
                    if let Some(rval) = rsrc.downcast_mut::<T>() {
                        Ok(rval)
                    } else {
                        Err(lfail!(
                            failure::Mode::CodeFailure,
                            "invalid resource type",
                            "param": ldisplay!(i),
                            "rsrc_id": ldisplay!(rsrc_id),
                            "value": ldebug!(val),
                            "valtype": ldebug!(val.get_type()),
                        ))
                    }
                } else {
                    Err(lfail!(
                        failure::Mode::RuntimeLeemaFailure,
                        "unknown resource",
                        "param": ldisplay!(i),
                        "rsrc_id": ldisplay!(rsrc_id),
                    ))
                }
            } else {
                Err(lfail!(
                    failure::Mode::CodeFailure,
                    "non-resource parameter",
                    "param": ldisplay!(i),
                    "value": ldisplay!(val),
                ))
            }
        } else {
            Err(lfail!(
                failure::Mode::CodeFailure,
                "invalid parameter index",
                "param": ldisplay!(i),
            ))
        }
    }

    pub fn get_result(&self) -> &Val
    {
        &self.result
    }

    /*
    pub fn clone_run_queue(&self) -> RunQueue
    {
        self.run_queue.clone()
    }
    */
}

pub type IopAction =
    fn(IopCtx) -> Pin<Box<dyn futures::Future<Output = IopCtx>>>;
