
use leema::frame::FrameTrace;
use leema::val::Val;

use std::collections::HashMap;
use std::sync::Arc;


// need to move these to leema code
#[derive(Copy)]
#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
struct Status
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

struct Failure
{
    tag: Val,
    msg: Val,
    trace: Option<Arc<FrameTrace>>,
    status: Status,
    meta: Lmap,
}

type Lresult<T> = Result<T, Failure>;

impl Failure
{
    pub fn new(tag: &'static str, msg: Lstr) -> Failure
    {
        Failure {
            tag: Val::Hashtag(Lstr::Sref(tag)),
            msg: Val::Str(msg),
            trace: None,
            status: None,
            meta: None,
        }
    }
}
