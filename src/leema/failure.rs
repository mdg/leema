use crate::leema::frame::FrameTrace;
use crate::leema::lstr::Lstr;
use crate::leema::sendclone::SendClone;
use crate::leema::struple::{StrupleItem, StrupleKV};
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

macro_rules! lfail {
    ($mode:expr, $msg:expr) => {
        crate::leema::failure::Failure::with_status($mode, $msg).with_context(
            vec![
                crate::leema::struple::StrupleItem::new(
                    crate::leema::lstr::Lstr::Sref("rfile"),
                    crate::leema::lstr::Lstr::Sref(file!()),
                ),
                crate::leema::struple::StrupleItem::new(
                    crate::leema::lstr::Lstr::Sref("rline"),
                    lstrf!("{}", line!()),
                ),
                crate::leema::struple::StrupleItem::new(
                    crate::leema::lstr::Lstr::Sref("msg"),
                    crate::leema::lstr::Lstr::Sref($msg),
                ),
            ],
        )
    };
    ($mode:expr, $msg:expr, $key:literal : $val:expr $(,)?) => {
        crate::leema::failure::Failure::with_status($mode, $msg).with_context(
            vec![
                crate::leema::struple::StrupleItem::new(
                    crate::leema::lstr::Lstr::Sref("rfile"),
                    crate::leema::lstr::Lstr::Sref(file!()),
                ),
                crate::leema::struple::StrupleItem::new(
                    crate::leema::lstr::Lstr::Sref("rline"),
                    lstrf!("{}", line!()),
                ),
                crate::leema::struple::StrupleItem::new(
                    crate::leema::lstr::Lstr::Sref("msg"),
                    crate::leema::lstr::Lstr::Sref($msg),
                ),
                crate::leema::struple::StrupleItem::new(
                    crate::leema::lstr::Lstr::Sref($key),
                    $val,
                ),
            ],
        )
    };
    ($mode:expr, $msg:expr, $($key:literal : $val:expr),+, $(,)?) => {
        crate::leema::failure::Failure::with_status($mode, $msg).with_context(
            vec![
                crate::leema::struple::StrupleItem::new(
                    crate::leema::lstr::Lstr::Sref("rfile"),
                    crate::leema::lstr::Lstr::Sref(file!()),
                ),
                crate::leema::struple::StrupleItem::new(
                    crate::leema::lstr::Lstr::Sref("rline"),
                    lstrf!("{}", line!()),
                ),
                crate::leema::struple::StrupleItem::new(crate::leema::lstr::Lstr::Sref("msg"), crate::leema::lstr::Lstr::Sref($msg)),
                $(crate::leema::struple::StrupleItem::new(crate::leema::lstr::Lstr::Sref($key), $val)),+
            ],
        )
    };
}

macro_rules! lfailoc {
    ($r:expr) => {
        match $r {
            Ok(x) => Ok(x),
            Err(f) => Err(f.rloc(file!(), line!())),
        }
    };
}

macro_rules! ltry {
    ($r:expr) => {
        match $r {
            Ok(x) => x,
            Err(f) => {
                return Err(f.rloc(file!(), line!()));
            }
        }
    };
    ($r:expr, $key:literal : $val:expr $(,)?) => {
        match $r {
            Ok(success) => success,
            Err(f) => {
                return Err(f.with_context(vec![
                    crate::leema::struple::StrupleItem::new(Lstr::Sref("rfile"), Lstr::Sref(file!())),
                    crate::leema::struple::StrupleItem::new(Lstr::Sref("rline"), lstrf!("{}", line!())),
                    crate::leema::struple::StrupleItem::new(Lstr::Sref($key), $val),
                ]));
            }
        }
    };
    ($r:expr, $($key:literal : $val:expr),+, $(,)?) => {
        match $r {
            Ok(success) => success,
            Err(f) => {
                return Err(f.with_context(vec![
                    crate::leema::struple::StrupleItem::new(Lstr::Sref("rfile"), Lstr::Sref(file!())),
                    crate::leema::struple::StrupleItem::new(Lstr::Sref("rline"), lstrf!("{}", line!())),
                    $(crate::leema::struple::StrupleItem::new(Lstr::Sref($key), $val)),+
                ]));
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
    Ok,
    // end-user input errors
    InvalidUserInput,
    Unauthenticated,
    Unauthorized,
    NotFound,
    // programmer-user errors
    ParseFailure,
    /// rename Semantic
    CompileFailure,
    Scope,
    TypeFailure,
    /// rename to Logic
    CodeFailure,

    // dynamic user-space execution errors
    /// File, network or other IO based failure
    Io,
    /// Ran out of time before completion
    Timeout,
    /// Found too many of something
    Overflow,
    /// Tried to get something that wasn't there
    Underflow,
    // internal leema errors
    StaticLeemaFailure,
    RuntimeLeemaFailure,
    LeemaTodoFailure,
    UnknownLeemaFailure,
}

impl Mode
{
    pub fn cli_code(&self) -> i32
    {
        match self {
            Mode::Ok => 0,
            // user-space errors
            Mode::InvalidUserInput => 1,
            Mode::Unauthenticated => 2,
            Mode::Unauthorized => 3,
            Mode::NotFound => 4,
            Mode::Timeout => 5,
            Mode::ParseFailure => 6,
            Mode::CompileFailure => 7,
            Mode::Scope => 9,
            Mode::TypeFailure => 10,
            Mode::CodeFailure => 11,
            Mode::Overflow => 12,
            Mode::Underflow => 13,
            Mode::Io => 14,
            // leema-space errors
            Mode::StaticLeemaFailure => -1,
            Mode::RuntimeLeemaFailure => -2,
            Mode::LeemaTodoFailure => -3,
            Mode::UnknownLeemaFailure => -4,
        }
    }

    pub fn as_str(&self) -> &'static str
    {
        match self {
            Mode::Ok => "ok",
            // end-user input errors
            Mode::InvalidUserInput => "invalid_input",
            Mode::Unauthenticated => "unauthenticated",
            Mode::Unauthorized => "unauthorized",
            Mode::NotFound => "not_found",
            // programmer-user errors
            Mode::ParseFailure => "parse_failure",
            Mode::CompileFailure => "compile_failure",
            Mode::Scope => "scope_failure",
            Mode::TypeFailure => "type_failure",
            Mode::CodeFailure => "code_failure",
            // runtime user-space errors
            Mode::Io => "io",
            Mode::Timeout => "timeout",
            Mode::Overflow => "overflow",
            Mode::Underflow => "underflow",
            // internal leema errors
            Mode::StaticLeemaFailure => "static_leema_failure",
            Mode::RuntimeLeemaFailure => "runtime_leema_failure",
            Mode::LeemaTodoFailure => "leema_todo_failure",
            Mode::UnknownLeemaFailure => "unknown_leema_failure",
        }
    }
}

/// struct to hold all the necessary info about a failure
/// data that should be on a failure:
/// * tag (multiple?)
/// * failure mode (multiple?)
/// * a main message/reason/source that isn't just regular map
/// * stack of tag, failure mode and reason? all 3 necessary?
///   * maybe tag and mode are kind of the same thing?
/// * map of Str -> Str
/// * stacktrace
#[derive(Clone)]
#[derive(Debug)]
pub struct Failure
{
    pub tag: Val,
    pub msg: Val,
    pub trace: Option<Arc<FrameTrace>>,
    pub status: Mode,
    pub code: i8,
    pub context: Vec<StrupleKV<Lstr, Lstr>>,
}

pub type Lresult<T> = Result<T, Failure>;

impl Failure
{
    pub fn new(tag: &'static str, msg: Lstr) -> Failure
    {
        let status = Mode::UnknownLeemaFailure;
        Failure {
            tag: Val::Hashtag(Lstr::Sref(tag)),
            msg: Val::Str(msg),
            trace: None,
            status,
            code: status.cli_code() as i8,
            context: vec![],
        }
    }

    pub fn with_status(mode: Mode, msg: &'static str) -> Failure
    {
        Failure {
            tag: Val::Hashtag(Lstr::Sref(mode.as_str())),
            msg: Val::Str(Lstr::Sref(msg)),
            trace: None,
            status: mode,
            code: mode.cli_code() as i8,
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
            status: Mode::Ok,
            code,
            context: vec![],
        }
    }

    pub fn propagate(&self, trace: Arc<FrameTrace>) -> Failure
    {
        Failure {
            tag: self.tag.clone(),
            msg: self.tag.clone(),
            trace: Some(trace),
            status: self.status,
            code: self.code,
            context: self.context.clone(),
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
            msg: Val::Str(msg.clone()),
            trace: None,
            status,
            code: status.cli_code() as i8,
            context: vec![vec![
                StrupleItem::new(Lstr::Sref("module"), module),
                StrupleItem::new(Lstr::Sref("line"), lstrf!("{}", lineno)),
                StrupleItem::new(Lstr::Sref("msg"), msg),
            ]],
        }
    }

    pub fn rloc(mut self, file: &'static str, line: u32) -> Self
    {
        self.context.push(vec![
            StrupleItem::new(Lstr::Sref("rfile"), Lstr::Sref(file)),
            StrupleItem::new(Lstr::Sref("rline"), lstrf!("{}", line)),
        ]);
        self
    }

    pub fn loc(mut self, file: &'static str, line: u32) -> Self
    {
        self.context.push(vec![
            StrupleItem::new(Lstr::Sref("file"), Lstr::Sref(file)),
            StrupleItem::new(Lstr::Sref("line"), lstrf!("{}", line)),
        ]);
        self
    }

    pub fn lstr_loc(mut self, file: Lstr, line: u32) -> Self
    {
        self.context.push(vec![
            StrupleItem::new(Lstr::Sref("file"), file),
            StrupleItem::new(Lstr::Sref("line"), lstrf!("{}", line)),
        ]);
        self
    }

    pub fn add_context(mut self, ctx: Lstr) -> Self
    {
        self.context
            .push(vec![StrupleItem::new(Lstr::Sref("context"), ctx)]);
        self
    }

    pub fn with_context(mut self, ctx: StrupleKV<Lstr, Lstr>) -> Self
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
        for c in &self.context {
            writeln!(f, "   {:?}", c)?;
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

impl SendClone for Failure
{
    type Item = Failure;

    fn clone_for_send(&self) -> Failure
    {
        let context = self.context.iter().map(|c| c.clone_for_send()).collect();
        Failure {
            tag: self.tag.clone_for_send(),
            msg: self.msg.clone_for_send(),
            trace: self.trace.clone_for_send(),
            status: self.status,
            code: self.code,
            context,
        }
    }
}

impl From<std::io::Error> for Failure
{
    fn from(e: std::io::Error) -> Failure
    {
        Failure {
            tag: Val::Hashtag(Lstr::Sref("#io")),
            msg: Val::Str(ldisplay!(e)),
            trace: None,
            status: Mode::Io,
            code: 0,
            context: vec![],
        }
    }
}

impl From<std::string::FromUtf8Error> for Failure
{
    fn from(e: std::string::FromUtf8Error) -> Failure
    {
        Failure {
            tag: Val::Hashtag(Lstr::Sref("#utf8")),
            msg: Val::Str(ldisplay!(e)),
            trace: None,
            status: Mode::InvalidUserInput,
            code: 0,
            context: vec![],
        }
    }
}
