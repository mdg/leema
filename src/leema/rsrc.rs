use crate::leema::code::Code;
use crate::leema::failure::{self, Lresult};
pub use crate::leema::io::RunQueue;
use crate::leema::io::{Io, Iop};
use crate::leema::val::{Fref, Type, Val};

use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use futures::future;
use futures::stream;
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
    Future(Box<dyn future::Future<Item = Event, Error = Event>>),
    Stream(Box<dyn stream::Stream<Item = Event, Error = Event>>),
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
            Event::Future(_) => write!(f, "Event::Future"),
            Event::Stream(_) => write!(f, "Event::Stream"),
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
    pub iop: Iop,
    run_queue: RunQueue,
    new_rsrc_id: i64,
    new_rsrc: Option<Box<dyn Rsrc>>,
}

impl IopCtx
{
    pub fn new(
        rcio: Rc<RefCell<Io>>,
        iop: Iop,
        run_queue: RunQueue,
        new_rsrc_id: i64,
    ) -> IopCtx
    {
        IopCtx {
            rcio,
            iop,
            run_queue,
            new_rsrc_id,
            new_rsrc: None,
        }
    }

    pub fn init_rsrc(&mut self, rsrc: Box<dyn Rsrc>) -> Lresult<Val>
    {
        if self.new_rsrc.is_some() {
            return Err(lfail!(
                failure::Mode::CodeFailure,
                "attempt to create second resource"
            ));
        }
        self.new_rsrc = Some(rsrc);
        Ok(Val::ResourceRef(self.new_rsrc_id))
    }

    pub fn take_rsrc<T>(&mut self) -> T
    where
        T: Rsrc,
    {
        self.iop.take_rsrc(0).unwrap()
    }

    /**
     * Take a parameter from the context
     */
    pub fn take_param(&mut self, i: i8) -> Lresult<Val>
    {
        Ok(ltry!(self.iop.get_param(i)).clone())
    }

    pub fn clone_run_queue(&self) -> RunQueue
    {
        self.run_queue.clone()
    }
}

pub type IopAction = fn(IopCtx) -> Event;
