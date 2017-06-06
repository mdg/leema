#[macro_use]
use leema::log;
use leema::frame::{Frame, Event, Parent, FrameTrace};
use leema::val::{Val, Env, FutureVal, Type, MsgVal};
use leema::reg::{Reg, Ireg};
use leema::code::{self, CodeKey, Code, Op, OpVec, ModSym, RustFunc};
use leema::list;

use std::cell::{RefCell, RefMut};
use std::collections::{HashMap, LinkedList};
use std::collections::hash_map;
use std::ops::{DerefMut};
use std::rc::{Rc};
use std::mem;
use std::fmt::{self, Debug};
use std::time::{Duration};
use std::thread;
use std::io::{stderr, Write};

use futures::{Poll, Async, Sink, Stream};
use futures::future::{Future};
use futures::task;
use futures::unsync::mpsc::{Sender, Receiver};
use tokio_core::reactor;


#[derive(Debug)]
pub struct Fiber
{
    pub fiber_id: i64,
    pub head: Frame,
    pub handle: reactor::Handle,
}

impl Fiber
{
    pub fn spawn(id: i64, root: Frame, h: &reactor::Handle) -> Fiber
    {
        Fiber{
            fiber_id: id,
            head: root,
            handle: h.clone(),
        }
    }

    pub fn id(&self) -> i64
    {
        self.fiber_id
    }

    pub fn module_name(&self) -> &str
    {
        self.head.module_name()
    }

    pub fn function_name(&self) -> &str
    {
        self.head.function_name()
    }

    pub fn push_call(&mut self, code: Rc<Code>, dst: Reg
            , module: Rc<String>, func: Rc<String>, args: Val
    ) {
        let trace = self.head.trace.clone();
        let mut newf = Frame{
            parent: Parent::Null,
            module: module.clone(),
            function: func.clone(),
            trace: FrameTrace::push_call(&trace, &(*func)),
            e: Env::with_args(args),
            pc: 0,
        };
        mem::swap(&mut self.head, &mut newf);
        let parent = Parent::Caller(code, Box::new(newf), dst);
        self.head.set_parent(parent);
    }
}
