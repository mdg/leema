use leema::code::Code;
use leema::lri::Lri;
use leema::lstr::Lstr;
use leema::reg::{Ireg, Reg};
use leema::rsrc;
use leema::struple::Struple;
use leema::val::{Env, Val};

use std::fmt::{self, Debug};
use std::mem;
use std::rc::Rc;
use std::sync::Arc;

use futures::sync::oneshot::Sender as FutureSender;


pub enum Parent
{
    Null,
    Caller(Rc<Code>, Box<Frame>, Reg),
    // Fork(Arc<AtomicBool>, mpsc::Sender<Msg>),
    Future(FutureSender<Val>, Val),
    Repl(Val),
    Main(Val),
}

impl Parent
{
    pub fn new_main() -> Parent
    {
        Parent::Main(Val::Void)
    }

    pub fn new_future(dst: FutureSender<Val>) -> Parent
    {
        Parent::Future(dst, Val::Void)
    }

    pub fn set_result(&mut self, r: Val)
    {
        match self {
            &mut Parent::Caller(_, ref mut pf, ref dst) => {
                pf.e.set_reg(dst, r);
            }
            // &mut Parent::Fork(_, _) => {}
            &mut Parent::Main(ref mut res) => {
                *res = r;
            }
            &mut Parent::Repl(ref mut res) => {
                *res = r;
            }
            &mut Parent::Null => {}
            &mut Parent::Future(_, ref mut dst) => {
                *dst = r;
            }
        }
    }
}

impl Debug for Parent
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self {
            &Parent::Null => write!(f, "Parent::Null"),
            &Parent::Caller(ref code, ref pf, ref dst) => {
                write!(f, "Parent::Caller({:?}, {}, {:?})", dst, code, pf)
            }
            /*
            &Parent::Fork(ref ready, _) => {
                write!(f, "Parent::Fork({:?})", ready)
            }
            */
            &Parent::Repl(ref res) => write!(f, "Parent::Repl({:?})", res),
            &Parent::Main(ref res) => write!(f, "Parent::Main({:?})", res),
            &Parent::Future(_, ref res) => {
                write!(f, "Parent::Future({:?})", res)
            }
        }
    }
}

pub enum Event
{
    Uneventful,
    Call(Reg, i16, Lri, Val),
    Fork,
    FutureWait(Reg),
    IOWait,
    Iop((i64, i64), rsrc::IopAction, Vec<Val>),
    // IoFuture(Box<future::Future<Item=(), Error=()>>),
    Complete(bool),
    Success,
    Failure,
}

impl Event
{
    pub fn success() -> Event
    {
        Event::Complete(true)
    }

    pub fn failure() -> Event
    {
        Event::Complete(false)
    }
}

impl fmt::Debug for Event
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self {
            &Event::Uneventful => write!(f, "Uneventful"),
            &Event::Call(ref r, line, ref cfunc, ref cargs) => {
                write!(
                    f,
                    "Event::Call({:?}@{}, {}, {:?})",
                    r, line, cfunc, cargs
                )
            }
            &Event::Fork => write!(f, "Event::Fork"),
            &Event::FutureWait(ref r) => write!(f, "Event::FutureWait({})", r),
            &Event::IOWait => write!(f, "Event::IOWait"),
            &Event::Iop(wrid, _, ref iopargs) => {
                write!(f, "Event::Iop({:?}, f, {:?})", wrid, iopargs)
            }
            &Event::Complete(c) => write!(f, "Event::Complete({})", c),
            &Event::Success => write!(f, "Event::Success"),
            &Event::Failure => write!(f, "Event::Failure"),
        }
    }
}

impl PartialEq for Event
{
    fn eq(&self, other: &Event) -> bool
    {
        match (self, other) {
            (&Event::Uneventful, &Event::Uneventful) => true,
            (
                &Event::Call(ref r1, line1, ref f1, ref a1),
                &Event::Call(ref r2, line2, ref f2, ref a2),
            ) => r1 == r2 && line1 == line2 && f1 == f2 && a1 == a2,
            (&Event::Fork, &Event::Fork) => true,
            (&Event::FutureWait(ref r1), &Event::FutureWait(ref r2)) => {
                r1 == r2
            }
            (&Event::IOWait, &Event::IOWait) => true,
            (&Event::Success, &Event::Success) => true,
            (&Event::Failure, &Event::Failure) => true,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub enum FrameTraceDirection
{
    CallUp,
    FailHere,
    ReturnDown,
}

#[derive(Debug)]
pub struct FrameTrace
{
    // TODO: Implement this in leema later
    direction: FrameTraceDirection,
    function: Lri,
    line: i16,
    parent: Option<Arc<FrameTrace>>,
}

impl FrameTrace
{
    pub fn new_root() -> Arc<FrameTrace>
    {
        Arc::new(FrameTrace {
            direction: FrameTraceDirection::CallUp,
            function: Lri::new(Lstr::Sref("__init__")),
            line: 0,
            parent: None,
        })
    }

    pub fn push_call(
        parent: &Arc<FrameTrace>,
        func: &Lri,
        line: i16,
    ) -> Arc<FrameTrace>
    {
        Arc::new(FrameTrace {
            direction: FrameTraceDirection::CallUp,
            function: func.clone(),
            line,
            parent: Some(parent.clone()),
        })
    }

    pub fn propagate_down(
        trace: &Arc<FrameTrace>,
        func: &Lri,
        line: i16,
    ) -> Arc<FrameTrace>
    {
        Arc::new(FrameTrace {
            direction: FrameTraceDirection::ReturnDown,
            function: func.clone(),
            line,
            parent: Some(trace.clone()),
        })
    }

    pub fn fail_here(&self) -> Arc<FrameTrace>
    {
        Arc::new(FrameTrace {
            direction: FrameTraceDirection::FailHere,
            function: self.function.clone(),
            line: self.line,
            parent: self.parent.clone(),
        })
    }
}

impl fmt::Display for FrameTrace
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        let dir = match self.direction {
            FrameTraceDirection::CallUp => " >",
            FrameTraceDirection::FailHere => "<>",
            FrameTraceDirection::ReturnDown => "< ",
        };

        write!(f, "{} {}", dir, self.function).ok();
        if self.line != 0 {
            write!(f, ":{}", self.line).ok();
        }
        match self.parent {
            None => writeln!(f),
            Some(ref p) => write!(f, "\n{}", p),
        }
    }
}

#[derive(Debug)]
pub struct Frame
{
    pub parent: Parent,
    pub function: Lri,
    pub trace: Arc<FrameTrace>,
    pub e: Env,
    pub pc: i32,
}

impl Frame
{
    pub fn new_root(parent: Parent, function: Lri, args: Struple<Val>)
        -> Frame
    {
        let env = Env::with_args(Val::Tuple(args));
        Frame {
            parent,
            trace: FrameTrace::new_root(),
            function,
            e: env,
            pc: 0,
        }
    }

    /*
    pub fn new_fork(f: &Frame, ready: &Arc<AtomicBool>, tx: mpsc::Sender<Msg>)
        -> Frame
    {
        Frame{
            parent: Parent::Fork(ready.clone(), tx),
            name: f.name.clone(),
            trace: f.trace.clone(),
            e: f.e.clone(),
            id: f.id,
            pc: 0,
        }
    }
    */

    pub fn set_parent(&mut self, p: Parent)
    {
        self.parent = p;
    }

    pub fn take_parent(&mut self) -> Parent
    {
        mem::replace(&mut self.parent, Parent::Null)
    }

    pub fn push_frame_trace(&self, line: i16) -> Arc<FrameTrace>
    {
        FrameTrace::push_call(&self.trace, &self.function, line)
    }

    pub fn take_env(&mut self) -> Env
    {
        let mut e = Env::new();
        mem::swap(&mut e, &mut self.e);
        e
    }

    /**
     * handy accessor function when calling from rust native functions
     */
    pub fn get_param(&self, p: i8) -> &Val
    {
        self.e.get_reg(&Reg::Param(Ireg::Reg(p)))
    }

    pub fn get_param_mut(&mut self, p: i8) -> &mut Val
    {
        self.e.get_reg_mut(&Reg::Param(Ireg::Reg(p)))
    }
}

/*
impl Debug for Frame
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "Frame({:?}, {:?}, {})",
            self.parent, self.e, self.pc,
        )
    }
}
*/

/*
 * fork the frame and frame state
 * add it to the fresh queue
 * jump current frame state past the fork block
fn execute_fork(curf: &mut Frame,
    dst: &Reg, freg: &Reg, argreg: &Reg
) -> Event {
    println!("execute_fork");

    // args are empty for a fork
    // create new frame
    let e = Env::new();
    // set current state to called
    let (tx, rx) = mpsc::channel::<Val>();
    let ready = Arc::new(AtomicBool::new(false));
    let newf = Frame::new_fork(&curf, &ready, tx);
    curf.e.set_reg(dst, Val::future(ready, rx));

    curf.pc = curf.pc + 1;
    Event::Fork
}
 */


/*
process_set
process
|  \- base frames
|      |       \- call code
 \--- fork
*/
