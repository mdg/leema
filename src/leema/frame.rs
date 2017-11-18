#[macro_use]
use leema::log;
use leema::fiber::Fiber;
use leema::val::{self, Val, Env, FutureVal, Type};
use leema::reg::{Reg, Ireg};
use leema::code::{self, CodeKey, Code, Op, OpVec, ModSym, RustFunc};
use leema::rsrc::{self};

use std::rc::{Rc};
use std::sync::{Arc};
use std::mem;
use std::fmt::{self, Debug};
use std::io::{stderr, Write};

use ::tokio_core::reactor::{self};


pub enum Parent
{
    Null,
    Caller(Rc<Code>, Box<Frame>, Reg),
    // Fork(Arc<AtomicBool>, mpsc::Sender<Msg>),
    Repl(Val),
    Main(Val),
}

impl Parent
{
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
                write!(f,
                    "Parent::Caller({:?}, {}, {:?})",
                    dst, code, pf
                )
            }
            /*
            &Parent::Fork(ref ready, _) => {
                write!(f, "Parent::Fork({:?})", ready)
            }
            */
            &Parent::Repl(ref res) => write!(f, "Parent::Repl({:?})", res),
            &Parent::Main(ref res) => write!(f, "Parent::Main({:?})", res),
        }
    }
}

pub enum Event
{
    Uneventful,
    Call(Reg, Rc<String>, Rc<String>, Val),
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

impl fmt::Debug for Event {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Event::Uneventful => write!(f, "Uneventful"),
            &Event::Call(ref r, ref cmod, ref cfunc, ref cargs) => {
                write!(f, "Event::Call({:?}, {}, {}, {:?})",
                    r, cmod, cfunc, cargs)
            }
            &Event::Fork => write!(f, "Event::Fork"),
            &Event::FutureWait(ref r) => write!(f, "Event::FutureWait({})", r),
            &Event::IOWait => write!(f, "Event::IOWait"),
            &Event::Iop(wrid, ref iopf, ref iopargs) => {
                write!(f, "Event::Iop({:?}, f, {:?})", wrid, iopargs)
            }
            &Event::Complete(c) => {
                write!(f, "Event::Complete({})", c)
            }
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
            (&Event::Call(ref r1, ref m1, ref f1, ref a1),
                    &Event::Call(ref r2, ref m2, ref f2, ref a2)) =>
            {
                r1 == r2 && m1 == m2 && f1 == f2 && a1 == a2
            }
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
pub struct FrameTrace
{
    // TODO: Implement this in leema later
    direction: i8,
    function: String,
    parent: Option<Arc<FrameTrace>>,
}

impl FrameTrace
{
    pub fn new_root(func: &String) -> Arc<FrameTrace>
    {
        Arc::new(FrameTrace{
            direction: 1,
            function: func.clone(),
            parent: None,
        })
    }

    pub fn push_call(parent: &Arc<FrameTrace>, func: &str) -> Arc<FrameTrace>
    {
        Arc::new(FrameTrace{
            direction: 1,
            function: func.to_string(),
            parent: Some(parent.clone()),
        })
    }

    pub fn propagate_down(trace: &Arc<FrameTrace>, func: &str)
        -> Arc<FrameTrace>
    {
        Arc::new(FrameTrace{
            direction: -1,
            function: String::from(func),
            parent: Some(trace.clone()),
        })
    }

    pub fn failure_here(&self) -> Arc<FrameTrace>
    {
        Arc::new(FrameTrace{
            direction: 0,
            function: self.function.clone(),
            parent: self.parent.clone(),
        })
    }
}

impl fmt::Display for FrameTrace
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        let dir = match self.direction {
            0 => "<>",
            1 => " >",
            -1 => "< ",
            d => {
                panic!("Invalid direction: {}", d);
            }
        };

        match self.parent {
            None => {
                write!(f, "{} {}\n", dir, self.function)
            }
            Some(ref p) => {
                write!(f, "{} {}\n{}", dir, self.function, p)
            }
        }
    }
}

#[derive(Debug)]
pub struct Frame
{
    pub parent: Parent,
    pub module: Rc<String>,
    pub function: Rc<String>,
    pub trace: Arc<FrameTrace>,
    pub e: Env,
    pub pc: i32,
}

impl Frame
{
    pub fn new_root(m: String, f: String) -> Frame
    {
        let env = Env::new();
        let modname = Rc::new(m);
        let fname = Rc::new(f);
        Frame{
            parent: Parent::Main(Val::Void),
            trace: FrameTrace::new_root(&fname),
            module: modname,
            function: fname,
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

    pub fn take_env(&mut self) -> Env
    {
        let mut e = Env::new();
        mem::swap(&mut e, &mut self.e);
        e
    }

    pub fn module_name(&self) -> &str
    {
        &(**self.module)
    }

    pub fn function_name(&self) -> &str
    {
        &(**self.function)
    }

    pub fn receive_future(&mut self, r: &Reg) -> Option<Val>
    {
        let fval = self.e.get_reg(&r);
        if !fval.is_future() {
            panic!("This isn't even a future {:?}", fval);
        }
        if !fval.is_future_ready() {
            return None;
        }
        match fval {
            &Val::Future(FutureVal(_, ref amrx)) => {
                let rx = amrx.lock().unwrap();
                // Some(Val::from_msg(rx.recv().unwrap()))
                Some(Val::Int(277))
            },
            _ => panic!("Not a future val? {:?}", fval),
        }
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
