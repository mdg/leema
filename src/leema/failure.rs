use crate::leema::frame::FrameTrace;
use crate::leema::lmap::LmapNode;
use crate::leema::lstr::Lstr;
use crate::leema::val::Val;

use std::fmt;
use std::sync::Arc;


macro_rules! rustfail {
    ($tag:expr, $msg:expr) => {
        crate::leema::failure::Failure::new($tag, crate::leema::lstr::Lstr::from($msg))
            .loc(file!(), line!())
    };
    ($tag:expr, $fmt:expr, $($arg:tt)*) => {
        crate::leema::failure::Failure::new(
                $tag,
                crate::leema::lstr::Lstr::from(format!($fmt, $($arg)*))
            )
            .loc(file!(), line!())
    };
}

macro_rules! lfailoc {
    ($r:expr) => {
        match $r {
            Ok(x) => Ok(x),
            Err(f) => Err(f.loc(file!(), line!())),
        }
    };
}

macro_rules! ltry {
    ($r:expr) => {
        match $r {
            Ok(x) => x,
            Err(f) => {
                return Err(f.loc(file!(), line!()));
            }
        }
    };
}

// need to move these to leema code
#[derive(Copy)]
#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub enum Mode
{
    Success,
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

impl Mode
{
    pub fn cli_code(&self) -> i32
    {
        match self {
            Mode::Success => 0,
            // end-user input errors
            Mode::InvalidUserInput => 1,
            Mode::Unauthenticated => 2,
            Mode::Unauthorized => 3,
            Mode::NotFound => 4,
            // programmer-user errors
            Mode::Timeout => 5,
            Mode::ParseFailure => 6,
            Mode::CompileFailure => 7,
            Mode::TypeFailure => 8,
            // internal leema errors
            Mode::StaticLeemaFailure => -1,
            Mode::RuntimeLeemaFailure => -2,
        }
    }

    fn as_str(&self) -> &'static str
    {
        match self {
            Mode::Success => "success",
            // end-user input errors
            Mode::InvalidUserInput => "invalid_input",
            Mode::Unauthenticated => "unauthenticated",
            Mode::Unauthorized => "unauthorized",
            Mode::NotFound => "not_found",
            // programmer-user errors
            Mode::Timeout => "timeout",
            Mode::ParseFailure => "parse_failure",
            Mode::CompileFailure => "compile_failure",
            Mode::TypeFailure => "type_failure",
            // internal leema errors
            Mode::StaticLeemaFailure => "static_leema_failure",
            Mode::RuntimeLeemaFailure => "runtime_leema_failure",
        }
    }
}

#[derive(Clone)]
#[derive(Debug)]
pub struct Failure
{
    pub tag: Val,
    pub msg: Val,
    pub trace: Option<Arc<FrameTrace>>,
    pub status: Mode,
    pub code: i8,
    loc: Vec<(Lstr, u32)>,
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
            status: Mode::Success,
            code: 0,
            loc: vec![],
            meta: None,
            context: vec![],
        }
    }

    pub fn leema_new(
        tag: Val,
        msg: Val,
        trace: Option<Arc<FrameTrace>>,
        code: i8,
    ) -> Failure
    {
        Failure {
            tag,
            msg,
            trace,
            status: Mode::Success,
            code,
            loc: vec![],
            meta: None,
            context: vec![],
        }
    }

    /// Return a static, compile-time leema code error
    pub fn static_leema(
        status: Mode,
        msg: Lstr,
        module: Lstr,
        lineno: u16,
    ) -> Failure
    {
        Failure {
            tag: Val::Str(Lstr::Sref(status.as_str())),
            msg: Val::Str(msg),
            trace: None,
            status,
            code: 0,
            loc: vec![(module, lineno as u32)],
            meta: None,
            context: vec![],
        }
    }

    pub fn loc(mut self, file: &'static str, line: u32) -> Self
    {
        self.loc.push((Lstr::from(file), line));
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
        if !self.loc.is_empty() {
            write!(f, "\n @ {:?}\n", self.loc)?;
        }
        for c in &self.context {
            write!(f, "   {}\n", c)?;
        }
        write!(f, "'{}')", self.msg)
    }
}

impl PartialEq for Failure
{
    fn eq(&self, b: &Failure) -> bool
    {
        self.tag == b.tag
    }
}
