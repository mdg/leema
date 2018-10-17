use leema::frame::FrameTrace;
use leema::lmap::LmapNode;
use leema::lstr::Lstr;
use leema::val::Val;

use std::fmt;
use std::sync::Arc;


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
    meta: LmapNode,
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
            meta: None,
        }
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
        write!(f, "Failure(#{} '{}')", self.tag, self.msg)
    }
}

impl PartialEq for Failure
{
    fn eq(&self, b: &Failure) -> bool
    {
        self.tag == b.tag
    }
}
