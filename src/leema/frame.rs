use crate::leema::code::Code;
use crate::leema::failure::Lresult;
use crate::leema::module::ModKey;
use crate::leema::reg::{Ireg, Reg};
use crate::leema::rsrc;
use crate::leema::struple::Struple2;
use crate::leema::val::{Env, Fref, Val};

use std::fmt::{self, Debug};
use std::mem;
use std::rc::Rc;
use std::sync::mpsc::Sender;
use std::sync::Arc;


pub enum Parent
{
    Null,
    Caller(Rc<Code>, Box<Frame>, Reg),
    Fork(Sender<Val>),
    Task,
    Repl(Val),
    Main(Val),
}

impl Parent
{
    pub fn new_main() -> Parent
    {
        Parent::Main(Val::VOID)
    }

    pub fn new_fork(dst: Sender<Val>) -> Parent
    {
        Parent::Fork(dst)
    }

    pub fn set_result(&mut self, r: Val)
    {
        match self {
            &mut Parent::Caller(_, ref mut pf, ref dst) => {
                pf.e.set_reg(*dst, r).unwrap();
            }
            // &mut Parent::Fork(_, _) => {}
            &mut Parent::Main(ref mut res) => {
                *res = r;
            }
            &mut Parent::Repl(ref mut res) => {
                *res = r;
            }
            &mut Parent::Fork(ref mut dst) => {
                let send_result = dst.send(r.clone());
                if send_result.is_err() {
                    panic!("fail sending fork result: {}", r);
                }
            }
            &mut Parent::Task => {}
            &mut Parent::Null => {}
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
            &Parent::Repl(ref res) => write!(f, "Parent::Repl({:?})", res),
            &Parent::Main(ref res) => write!(f, "Parent::Main({:?})", res),
            &Parent::Fork(_) => write!(f, "Parent::Fork"),
            &Parent::Task => write!(f, "Parent::Task"),
        }
    }
}

pub enum Event
{
    Uneventful,
    Call(Reg, i16, Fref, Struple2<Val>),
    TailCall(Fref, Struple2<Val>),
    NewTask(Fref, Struple2<Val>),
    FutureWait(Reg),
    Iop((i64, i64), rsrc::IopAction, Vec<Val>),
    Success,
}

impl Event
{
    pub fn success() -> Lresult<Event>
    {
        Ok(Event::Success)
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
            &Event::TailCall(ref cfunc, ref cargs) => {
                write!(f, "Event::TailCall({}, {:?})", cfunc, cargs)
            }
            &Event::NewTask(ref fref, ref args) => {
                write!(f, "Event::NewTask({}, {:?})", fref, args)
            }
            &Event::FutureWait(ref r) => write!(f, "Event::FutureWait({})", r),
            &Event::Iop(wrid, _, ref iopargs) => {
                write!(f, "Event::Iop({:?}, f, {:?})", wrid, iopargs)
            }
            &Event::Success => write!(f, "Event::Success"),
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
            (
                &Event::NewTask(ref f1, ref a1),
                &Event::NewTask(ref f2, ref a2),
            ) => f1 == f2 && a1 == a2,
            (&Event::FutureWait(ref r1), &Event::FutureWait(ref r2)) => {
                r1 == r2
            }
            (&Event::Success, &Event::Success) => true,
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
    function: Fref,
    line: i16,
    parent: Option<Arc<FrameTrace>>,
}

impl FrameTrace
{
    pub fn new_root() -> Arc<FrameTrace>
    {
        Arc::new(FrameTrace {
            direction: FrameTraceDirection::CallUp,
            function: Fref::with_modules(ModKey::default(), "__init__"),
            line: 0,
            parent: None,
        })
    }

    pub fn push_call(
        parent: &Arc<FrameTrace>,
        func: &Fref,
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
        func: &Fref,
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
pub struct Stack
{
    data: Vec<Val>,
    base: usize,
    size: usize,
}

impl Stack
{
    pub fn new(size: usize) -> Stack
    {
        Stack {
            data: Vec::with_capacity(std::cmp::max(64, size)),
            base: 0,
            size,
        }
    }

    pub fn push(&mut self, new_size: usize) -> Stack
    {
        let mut new_stack = Stack {
            data: vec![],
            base: self.base + self.size,
            size: new_size,
        };
        self.data.reserve(new_stack.base + new_stack.size);
        mem::swap(&mut self.data, &mut new_stack.data);
        new_stack
    }

    pub fn restore(&mut self, old_stack: &mut Stack)
    {
        mem::swap(&mut self.data, &mut old_stack.data);
    }

    pub fn get(&self, i: usize) -> Lresult<&Val>
    {
        Ok(self.data.get(self.base + i).unwrap())
    }

    pub fn get_mut(&mut self, i: usize) -> Lresult<&mut Val>
    {
        Ok(self.data.get_mut(self.base + i).unwrap())
    }

    pub fn set(&mut self, i: usize, d: Val) -> Lresult<()>
    {
        *self.data.get_mut(self.base + i).unwrap() = d;
        Ok(())
    }
}

#[derive(Debug)]
pub struct Frame
{
    pub parent: Parent,
    pub function: Fref,
    pub trace: Arc<FrameTrace>,
    pub e: Env,
    pub stack: Stack,
    pub pc: i32,
}

impl Frame
{
    pub fn new_root(
        parent: Parent,
        function: Fref,
        args: Struple2<Val>,
    ) -> Frame
    {
        let env = Env::with_args(args);
        Frame {
            parent,
            trace: FrameTrace::new_root(),
            function,
            e: env,
            stack: Stack::new(50),
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
    pub fn get_param(&self, p: i8) -> Lresult<&Val>
    {
        self.e.get_reg(Reg::Param(Ireg::Reg(p)))
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
