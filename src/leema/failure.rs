use leema::frame::FrameTrace;
use leema::lmap::LmapNode;
use leema::lstr::Lstr;
use leema::val::Val;

use std::fmt;
use std::sync::Arc;


macro_rules! rustfail {
    ($tag:expr, $msg:expr) => {
        ::leema::failure::Failure::new($tag, Lstr::from($msg))
            .set_rustloc(file!(), line!())
    };
    ($tag:expr, $fmt:expr, $($arg:tt)*) => {
        ::leema::failure::Failure::new(
                $tag,
                Lstr::from(format!($fmt, $($arg)*))
            )
            .set_rustloc(file!(), line!())
    };
}

// need to move these to leema code
#[derive(Copy)]
#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub enum Status
{
    None,
    // end-user input errors
    InvalidUserInput,
    Unauthenticated,
    Unauthorized,
    NotFound,
    // programmer-user errors
    Timeout,
    ParseFailure,
    CompileFailure,
    TypeFailure,
    // internal leema errors
    StaticLeemaFailure,
    RuntimeLeemaFailure,
}

#[derive(Clone)]
#[derive(Debug)]
pub struct Failure
{
    tag: Val,
    msg: Val,
    trace: Option<Arc<FrameTrace>>,
    status: Status,
    rustloc: Option<(&'static str, u32)>,
    meta: LmapNode,
    context: Vec<Lstr>,
}

pub type Lresult<T> = Result<T, Failure>;

impl Failure
{
    pub fn new(tag: &'static str, msg: Lstr) -> Failure
    {
        Failure {
            tag: Val::Hashtag(Lstr::Sref(tag)),
            msg: Val::Str(msg),
            trace: None,
            status: Status::None,
            rustloc: None,
            meta: None,
            context: vec![],
        }
    }

    pub fn set_rustloc(mut self, file: &'static str, line: u32) -> Self
    {
        self.rustloc = Some((file, line));
        self
    }

    pub fn add_context(mut self, ctx: Lstr) -> Self
    {
        self.context.push(ctx);
        self
    }

    // duplicating a capability that's currently in nightly but not yet
    // in stable. Get rid of this once it's in stable
    pub fn transpose<T>(optr: Option<Lresult<T>>) -> Lresult<Option<T>>
    {
        match optr {
            Some(Ok(res)) => Ok(Some(res)),
            Some(Err(err)) => Err(err),
            None => Ok(None),
        }
    }
}

impl fmt::Display for Failure
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "Failure({} ", self.tag)?;
        if let Some(ref rustloc) = self.rustloc {
            write!(f, " @ {:?}", rustloc)?;
        }
        write!(f, "\n'{}')", self.msg)
    }
}

impl PartialEq for Failure
{
    fn eq(&self, b: &Failure) -> bool
    {
        self.tag == b.tag
    }
}