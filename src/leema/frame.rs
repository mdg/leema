use crate::leema::code::Code;
use crate::leema::failure::Lresult;
use crate::leema::module::ModKey;
use crate::leema::reg::Reg;
use crate::leema::rsrc;
use crate::leema::stack;
use crate::leema::struple::Struple2;
use crate::leema::val::{Fref, Val};

use std::fmt::{self, Debug};
use std::rc::Rc;
use std::sync::Arc;


/*
pub enum Result
{
    Parent(Rc<Code>, Frame),
    Complete(Val),
}
*/

pub enum Event
{
    Uneventful,
    PushCall
    {
        argc: i16,
        line: i16,
    },
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
            &Event::PushCall { argc, line } => {
                write!(f, "Event::PushCall({} @{})", argc, line)
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
                &Event::PushCall { argc: a1, line: l1 },
                &Event::PushCall { argc: a2, line: l2 },
            ) => a1 == a2 && l1 == l2,
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

    pub fn pop_call(&self) -> Option<Arc<FrameTrace>>
    {
        self.parent.as_ref().map(|t| t.clone())
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
struct ParentFrame
{
    code: Rc<Code>,
    stack: stack::Ref,
    pc: i32, // program counter
}

#[derive(Debug)]
pub struct Frame
{
    pub trace: Arc<FrameTrace>,
    /// rename this to something better than "e"
    pub e: stack::Ref,
    parents: Vec<ParentFrame>,

    // pub sp: u32, // stack pointer
    pub pc: i32, // program counter
}

impl Frame
{
    pub fn new_root(stack: stack::Ref) -> Frame
    {
        Frame {
            trace: FrameTrace::new_root(),
            e: stack,
            parents: vec![],
            pc: 0,
        }
    }

    pub fn push_call(
        self,
        calling_code: Rc<Code>,
        argc: i16,
        line: i16,
    ) -> Lresult<Frame>
    {
        /*
         * TODO come back to this after regular stack calls are done
         * If this is going to be done, it can be done in Frame::push_call
        let mut func = match self.head.e.func_val() {
            Val::Func(ref fref) => fref.clone(),
            other => {
                return Err(lfail!(
                    failure::Mode::RuntimeLeemaFailure,
                    "expected function value",
                    "value": ldebug!(other),
                ));
            }
        };
        // if it's a method, substitute in the implementing type
        if func.is_method() {
            if let Some(selfie) = args.first() {
                let self_val_t = selfie.v.get_type();
                // replace self type instead of mod
                let ftyp = func.t.func_ref_mut().unwrap();
                ftyp.args.first_mut().map(|f| {
                    f.v = self_val_t;
                });
            }
        }
        */
        let parent_trace = self.push_frame_trace(line);
        let stack = self.e.push_new_call(argc);
        let mut parents = self.parents;
        parents.push(ParentFrame {
            code: calling_code,
            stack: self.e,
            pc: self.pc,
        });
        Ok(Frame {
            trace: parent_trace,
            parents,
            e: stack,
            pc: 0,
        })
    }


    pub fn tail_call_args(&mut self, _call: Val, _args: Struple2<Val>)
    {
        /*
        (*self.func) = call;
        for a in args.iter_mut() {
            a
        }
        */
    }

    pub fn pop_call(&mut self) -> Option<Rc<Code>>
    {
        // pop the stack frame first
        self.e.pop_frame();

        let parent = self.parents.pop()?;
        let parent_trace = self.trace.pop_call()?;

        self.trace = parent_trace;
        self.e = parent.stack;
        // move forward one, past the previous call
        self.pc = parent.pc + 1;
        Some(parent.code)
    }

    pub fn push_frame_trace(&self, line: i16) -> Arc<FrameTrace>
    {
        FrameTrace::push_call(&self.trace, self.function(), line)
    }

    pub fn module(&self) -> &ModKey
    {
        // self.e.subject().module()
        &self.function().m
    }

    pub fn function(&self) -> &Fref
    {
        self.e.fref().unwrap()
    }

    pub fn get_param(&self, p: i8) -> Lresult<&Val>
    {
        self.e.get_param(p)
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
