use leema::reg::{self, Reg, Ireg, Iregistry};
use leema::list;
use leema::lri::{Lri};
use leema::lstr::{Lstr};
use leema::frame::{FrameTrace};
use leema::log;
use leema::msg;
use leema::sendclone::{self, SendClone};
use leema::struple::{Struple};

use std::fmt::{self};
use std::collections::{BTreeMap, HashMap};
use std::sync::{Arc, Mutex};
use std::sync::atomic::{self, AtomicBool};
use std::rc::{Rc};
use std::cmp::{PartialEq, PartialOrd, Ordering};
use std::io::{Write, Error};

use ::futures::sync::mpsc::{Receiver};

use mopa;


#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
pub enum FuncType
{
    Pure,
    Obs,
    Sys,
    Query,
    Cmd,
    Main, // or Control?
}

// #[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
#[derive(Eq)]
#[derive(Hash)]
pub enum Type
{
    Int,
    Str,
    Bool,
    Hashtag,
    Tuple(Struple<Type>),
    Failure,
    Func(Vec<Type>, Box<Type>),
    // different from base collection/map interfaces?
    // base interface/type should probably be iterator
    // and then it should be a protocol, not type
    StrictList(Box<Type>),
    UserDef(Lri),
    Lib(String),
    Resource(Rc<String>),
    RustBlock,
    Param(i8),
    // Future(Box<Type>),
    Void,
    /*
    Map(Box<Type>, Box<Type>),
    */
    Kind,
    Any,

    Unknown,
    Var(Lstr),
    AnonVar,
    // the inner type, dereferenced
    Deref(Box<Type>),
}

impl Type
{
    pub fn f(inputs: Vec<Type>, result: Type) -> Type
    {
        Type::Func(inputs, Box::new(result))
    }

    /**
     * Get the typename including the module
     */
    pub fn full_typename(&self) -> Lstr
    {
        match self {
            &Type::Int => Lstr::Sref("Int"),
            &Type::UserDef(ref name) => {
                Lstr::from(name)
            }
            &Type::Void => Lstr::Sref("Void"),
            _ => {
                panic!("no typename for {:?}", self);
            }
        }
    }

    /**
     * Get the typename without any module information
     */
    pub fn local_typename(&self) -> Rc<String>
    {
        match self {
            &Type::UserDef(ref i) => {
                i.local().rc()
            }
            _ => {
                self.full_typename().rc()
            }
        }
    }

    pub fn split_func(t: &Type) -> (&Vec<Type>, &Type)
    {
        match t {
            &Type::Func(ref args, ref result) => {
                (args, &*result)
            }
            _ => {
                panic!("Not a func type {:?}", t);
            }
        }
    }

    pub fn is_func(&self) -> bool
    {
        if let &Type::Func(_, _) = self {
            true
        } else {
            false
        }
    }

    pub fn is_var(&self) -> bool
    {
        match self {
            &Type::Var(_) => true,
            &Type::AnonVar => true,
            _ => false,
        }
    }

    pub fn var_name(&self) -> Lstr
    {
        match self {
            &Type::Var(ref id) => id.clone(),
            &Type::AnonVar => Lstr::Sref("anon"),
            _ => {
                panic!("Not a Type::Var {:?}", self);
            }
        }
    }

    pub fn deep_clone(&self) -> Type
    {
        match self {
            &Type::Str => Type::Str,
            &Type::Bool => Type::Bool,
            &Type::Int => Type::Int,
            &Type::Hashtag => Type::Hashtag,
            &Type::Tuple(ref items) => {
                Type::Tuple(items.clone_for_send())
            }
            &Type::UserDef(ref i) => {
                Type::UserDef(i.deep_clone())
            }
            &Type::StrictList(ref i) => {
                Type::StrictList(Box::new(i.deep_clone()))
            }
            &Type::Func(ref args, ref result) => {
                let dc_args = args.iter().map(|t| {
                    t.deep_clone()
                }).collect();
                Type::Func(dc_args, Box::new(result.deep_clone()))
            }
            &Type::Var(ref id) => {
                Type::Var(id.deep_clone())
            }
            &Type::Void => Type::Void,
            _ => {
                panic!("cannot deep_clone Type: {:?}", self);
            }
        }
    }

    pub fn list_inner_type(&self) -> Type
    {
        match self {
            &Type::StrictList(ref inner) => (**inner).clone(),
            &Type::Var(_) => Type::Unknown,
            &Type::Unknown => Type::Unknown,
            _ => {
                panic!("cannot get inner type of a not list: {:?}", self);
            }
        }
    }

    pub fn wrap_in_list(inner: Type) -> Type
    {
        Type::StrictList(Box::new(inner))
    }
}

impl sendclone::SendClone for Type
{
    type Item = Type;

    fn clone_for_send(&self) -> Type
    {
        self.deep_clone()
    }
}

impl fmt::Display for Type
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self {
            &Type::Int => write!(f, "Int"),
            &Type::Str => write!(f, "Str"),
            &Type::Bool => write!(f, "Bool"),
            &Type::Hashtag => write!(f, "Hashtag"),
            &Type::Tuple(ref items) => {
                write!(f, "{}", items)
            }
            &Type::UserDef(ref name) => write!(f, "{}", name),
            &Type::Failure => write!(f, "Failure"),
            &Type::Func(ref args, ref result) => {
                for a in args {
                    write!(f, "{} => ", a).ok();
                }
                write!(f, "{}", result)
            }
            // different from base collection/map interfaces?
            // base interface/type should probably be iterator
            // and then it should be a protocol, not type
            &Type::StrictList(ref typ) => write!(f, "List<{}>", typ),
            &Type::Lib(ref name) => write!(f, "LibType({})", &name),
            &Type::Resource(ref name) => write!(f, "{}", &name),
            &Type::RustBlock => write!(f, "RustBlock"),
            &Type::Void => write!(f, "Void"),
            &Type::Kind => write!(f, "Kind"),
            &Type::Any => write!(f, "Any"),

            &Type::Unknown => write!(f, "TypeUnknown"),
            &Type::Param(index) => {
                write!(f, "Type::Param({})", index)
            }
            &Type::Var(ref name) => {
                write!(f, "Type::Var({})", name)
            }
            &Type::AnonVar => write!(f, "TypeAnonymous"),
            &Type::Deref(ref inner) => {
                write!(f, "`{}", inner)
            }
        }
    }
}

impl fmt::Debug for Type
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self {
            &Type::Int => write!(f, "Int"),
            &Type::Str => write!(f, "Str"),
            &Type::Bool => write!(f, "Bool"),
            &Type::Hashtag => write!(f, "Hashtag"),
            &Type::Tuple(ref items) => {
                write!(f, "T{}", items)
            }
            &Type::UserDef(ref name) => write!(f, "UserDef({})", name),
            &Type::Failure => write!(f, "Failure"),
            &Type::Func(ref args, ref result) => {
                for a in args {
                    write!(f, "{:?}>", a).ok();
                }
                write!(f, "{:?}", result)
            }
            // different from base collection/map interfaces?
            // base interface/type should probably be iterator
            // and then it should be a protocol, not type
            &Type::StrictList(ref typ) => write!(f, "List<{}>", typ),
            &Type::Lib(ref name) => write!(f, "LibType({})", &name),
            &Type::Resource(ref name) => write!(f, "Resource({})", &name),
            &Type::RustBlock => write!(f, "RustBlock"),
            &Type::Void => write!(f, "Void"),
            &Type::Kind => write!(f, "Kind"),
            &Type::Any => write!(f, "Any"),

            &Type::Unknown => write!(f, "TypeUnknown"),
            &Type::Var(ref name) => {
                write!(f, "Type::Var({})", name)
            }
            &Type::Param(index) => {
                write!(f, "Type::Param({})", index)
            }
            &Type::AnonVar => write!(f, "TypeAnonymous"),
            &Type::Deref(ref inner) => {
                write!(f, "TypeDeref({:?})", inner)
            }
        }
    }
}

#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub enum TypeErr
{
    Error(Lstr),
    Mismatch(Type, Type),
    Context(Box<TypeErr>, Rc<String>),
}

impl TypeErr
{
    pub fn add_context(self, ctx: String) -> TypeErr
    {
        TypeErr::Context(Box::new(self), Rc::new(ctx))
    }
}

impl fmt::Display for TypeErr
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self {
            &TypeErr::Error(ref estr) => write!(f, "TypeError({})", estr),
            &TypeErr::Mismatch(ref a, ref b) => {
                write!(f, "TypeMismatch({},{})", a, b)
            }
            &TypeErr::Context(ref inner_e, ref ctx) => {
                write!(f, "({}, '{}')", inner_e, ctx)
            }
        }
    }
}

impl<'a> From<&'a TypeErr> for String
{
    fn from(e: &'a TypeErr) -> String
    {
        format!("{}", e)
    }
}

pub type TypeResult = Result<Type, TypeErr>;


pub trait LibVal
    : mopa::Any
    + fmt::Debug
{
    fn get_type(&self) -> Type;
}

mopafy!(LibVal);

#[derive(Clone)]
pub struct FutureVal(pub Arc<AtomicBool>, pub Arc<Mutex<Receiver<MsgVal>>>);

impl fmt::Debug for FutureVal
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "FutureVal({:?})", self.0)
    }
}

#[derive(Copy)]
#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub struct SrcLoc
{
    pub lineno: i16,
    pub column: i8,
}

impl SrcLoc
{
    pub fn new(l: i16, c: i8) -> SrcLoc
    {
        SrcLoc{lineno: l, column: c}
    }
}

impl Default for SrcLoc
{
    fn default() -> SrcLoc
    {
        SrcLoc::new(0, 0)
    }
}

impl fmt::Display for SrcLoc
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "line {}", self.lineno)
    }
}

pub const DEFAULT_SRC_LOC: SrcLoc = SrcLoc{lineno: 0, column: 0};

#[derive(Copy)]
#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub enum FailureStatus
{
    // user input errors
    BadUserInput,
    Unauthenticated,
    Unauthorized,
    MissingData,
    // leema program errors
    MissingEntryPoint,
    Timeout,
    ProgramError{
        status: i8 // positive numbers only
    },
    // vm errors
    LeemaInternalError,
}

pub const FAILURE_SUCCESS : i8          =  0;
pub const FAILURE_NOENTRY : i8          = -1;
pub const FAILURE_BADINPUT : i8         = -2;
pub const FAILURE_UNAUTHENTICATED : i8  = -3;
pub const FAILURE_UNAUTHORIZED : i8     = -4;
pub const FAILURE_MISSINGDATA : i8      = -5;
pub const FAILURE_TIMEOUT : i8          = -6;
pub const FAILURE_INTERNAL : i8         = -7;
pub const FAILURE_TYPE : i8             = -8;


pub type MsgVal = msg::MsgItem<Val>;

#[derive(Clone)]
pub enum Val
{
    Int(i64),
    Str(Rc<String>),
    // StrCat(Rc<Val>, Box<Val>),
    // EmptyStr,
    Bool(bool),
    Hashtag(Rc<String>),
    Buffer(Vec<u8>),
    Cons(Box<Val>, Rc<Val>),
    Nil,
    Tuple(Struple<Val>),
    Struct(Lri, Struple<Val>),
    EnumStruct(Lri, Lstr, Struple<Val>),
    EnumToken(Lri, Lstr),
    Token(Lri),
    Failure(
        Box<Val>, // tag
        Box<Val>, // msg
        Arc<FrameTrace>,
        i8, // status
    ),
    Id(Rc<String>),
    Lri(Lri),
    Type(Type),
    Kind(u8),
    Lib(Arc<LibVal>),
    LibRc(Rc<LibVal>),
    FuncRef(Rc<String>, Rc<String>, Type),
    ResourceRef(i64),
    RustBlock,
    Future(FutureVal),
    Void,
    Wildcard,
    PatternVar(Reg),
}

const NIL: Val = Val::Nil;
pub const VOID: Val = Val::Void;
pub const FALSE: Val = Val::Bool(false);
pub const TRUE: Val = Val::Bool(true);

impl Val
{
    pub fn id(s: String) -> Val
    {
        Val::Id(Rc::new(s))
    }

    pub fn is_id(&self) -> bool
    {
        match self {
            &Val::Id(_) => true,
            _ => false,
        }
    }

    pub fn id_name(&self) -> Rc<String>
    {
        match self {
            &Val::Id(ref name) => name.clone(),
            &Val::Type(ref typ) => {
                typ.full_typename().rc()
            }
            _ => {
                panic!("not an id {:?}", self);
            }
        }
    }

    pub fn empty_tuple() -> Val
    {
        Val::new_tuple(0)
    }

    pub fn new_tuple(ref sz: usize) -> Val
    {
        let mut t = Vec::with_capacity(*sz);
        let mut i: usize = *sz;
        while i > 0 {
            t.push((None, VOID));
            i = i - 1;
        }
        Val::Tuple(Struple(t))
    }

    pub fn tuple_from_list(l: &Val) -> Val
    {
        // TODO switch to be list::to_vec(), use regular Tuple constructor
        if !l.is_list() {
            panic!("Cannot make tuple from not-list: {:?}", l);
        }
        let empties: Vec<(Option<Lstr>, Val)> =
            Vec::with_capacity(list::len(l));
        let items = list::fold_ref(empties, l, |mut res, item| {
            res.push((None, item.clone()));
            res
        });
        Val::Tuple(Struple(items))
    }

    pub fn is_list(&self) -> bool
    {
        match self {
            &Val::Cons(_, _) => true,
            &Val::Nil => true,
            _ => false,
        }
    }

    pub fn new_str(s: String) -> Val
    {
        Val::Str(Rc::new(s))
    }

    pub fn empty_str() -> Val
    {
        Val::Str(Rc::new("".to_string()))
    }

    pub fn str(&self) -> &str
    {
        match self {
            &Val::Id(ref id) => id,
            &Val::Str(ref s) => s,
            _ => {
                panic!("Cannot convert to string: {:?}", self);
            }
        }
    }

    pub fn to_str(&self) -> Rc<String>
    {
        match self {
            &Val::Id(ref id) => id.clone(),
            &Val::Str(ref s) => s.clone(),
            _ => {
                panic!("Cannot convert to string: {:?}", self);
            }
        }
    }

    pub fn to_int(&self) -> i64
    {
        match self {
            &Val::Int(i) => i,
            _ => {
                panic!("Not an int: {:?}", self);
            }
        }
    }

    pub fn hashtag(s: String) -> Val
    {
        Val::Hashtag(Rc::new(s))
    }

    pub fn is_type(&self) -> bool
    {
        match self {
            &Val::Type(_) => true,
            _ => false,
        }
    }

    pub fn future(ready: Arc<AtomicBool>, r: Receiver<MsgVal>) -> Val
    {
        Val::Future(FutureVal(ready, Arc::new(Mutex::new(r))))
    }

    pub fn is_future(&self) -> bool
    {
        match self {
            &Val::Future(_) => true,
            _ => false,
        }
    }

    pub fn is_future_ready(&self) -> bool
    {
        match self {
            &Val::Future(ref fv) => {
                fv.0.load(atomic::Ordering::Relaxed)
            }
            _ => false,
        }
    }

    pub fn is_failure(&self) -> bool
    {
        match self {
            &Val::Failure(_, _, _, _) => true,
            _ => false,
        }
    }

    pub fn failure(tag: Val, msg: Val, trace: Arc<FrameTrace>, status: i8
        ) -> Val
    {
        Val::Failure(
            Box::new(tag),
            Box::new(msg),
            trace,
            status,
        )
    }

    pub fn resource_ref(&self) -> i64
    {
        match self {
            &Val::ResourceRef(rid) => rid,
            _ => panic!("not a resource ref"),
        }
    }

    pub fn libval<T: LibVal>(lv: T) -> Val
    {
        Val::Lib(Arc::new(lv))
    }

    pub fn libval_rc<T: LibVal>(lv: T) -> Val
    {
        Val::LibRc(Rc::new(lv))
    }

    pub fn libval_as<T>(&self) -> Option<&T>
        where T: LibVal
    {
        match self {
            &Val::Lib(ref lvarc) => {
                vout!("lvarc: {:?}\n", lvarc);
                let lvref: &LibVal = &**lvarc;
                vout!("lvref: {:?}\n", lvref);
                lvref.downcast_ref::<T>()
            }
            &Val::LibRc(ref lvrc) => {
                let lvref: &LibVal = &**lvrc;
                lvref.downcast_ref::<T>()
            }
            _ => None,
        }
    }

    pub fn get_type(&self) -> Type
    {
        match self {
            &Val::Bool(_) => Type::Bool,
            &Val::Int(_) => Type::Int,
            &Val::Str(_) => Type::Str,
            &Val::Hashtag(_) => Type::Hashtag,
            &Val::Cons(ref head, _) => {
                let inner = head.get_type();
                Type::StrictList(Box::new(inner))
            }
            &Val::Nil => Type::StrictList(Box::new(Type::Unknown)),
            &Val::Failure(_, _, _, _) => Type::Failure,
            &Val::Type(_) => Type::Kind,
            &Val::Void => Type::Void,
            &Val::Wildcard => Type::Unknown,
            &Val::PatternVar(_) => Type::Unknown,
            &Val::Id(_) => Type::AnonVar,
            &Val::Lri(_) => Type::AnonVar,
            &Val::RustBlock => Type::RustBlock,
            &Val::Tuple(ref items) if items.0.len() == 1 => {
                items.0.get(0).unwrap().1.get_type()
            }
            &Val::Tuple(ref items) => {
                let tuptypes = items.0.iter().map(|i| {
                    (i.0.clone(), i.1.get_type())
                }).collect();
                Type::Tuple(Struple(tuptypes))
            }
            &Val::Struct(ref typ, _) => {
                Type::UserDef(typ.clone())
            }
            &Val::EnumStruct(ref typ, _, _) => {
                Type::UserDef(typ.clone())
            }
            &Val::EnumToken(ref typ, _) => {
                Type::UserDef(typ.clone())
            }
            &Val::Token(ref typ) => {
                Type::UserDef(typ.clone())
            }
            &Val::Buffer(_) => Type::Str,
            &Val::Kind(_) => {
                panic!("is kind even a thing here?");
            }
            &Val::FuncRef(_, _, ref typ) => {
                typ.clone()
            }
            &Val::Lib(ref lv) => {
                lv.get_type()
            }
            &Val::LibRc(ref lv) => {
                lv.get_type()
            }
            &Val::ResourceRef(_) => {
                panic!("cannot get type of ResourceRef: {:?}", self);
            }
            &Val::Future(_) => {
                panic!("cannot get type of Future: {:?}", self);
            }
        }
    }

    pub fn pattern_match(patt: &Val, input: &Val) -> Option<Vec<(Reg, Val)>>
    {
        let mut assigns = vec![];
        if Val::_pattern_match(&mut assigns, patt, input) {
            Some(assigns)
        } else {
            None
        }
    }

    fn _pattern_match(assigns: &mut Vec<(Reg, Val)>,
        patt: &Val, input: &Val) -> bool
    {
        let result = match (patt, input) {
            (&Val::Wildcard, _) => true,
            (&Val::PatternVar(ref dst), _) => {
                // should put something in assigns vector here
                assigns.push((dst.clone(), input.clone()));
                true
            }
            (&Val::Int(p), &Val::Int(i)) if p == i => true,
            (&Val::Bool(p), &Val::Bool(i)) if p == i => true,
            (&Val::Str(ref p), &Val::Str(ref i)) if p == i => true,
            (&Val::Hashtag(ref p), &Val::Hashtag(ref i)) if p == i => true,
            (&Val::Cons(_, _), &Val::Cons(_, _)) => {
                Val::_pattern_match_list(assigns, patt, input)
            }
            (&Val::Tuple(ref pv), &Val::Tuple(ref iv))
                if pv.0.len() == iv.0.len() =>
            {
                pv.0.iter().zip(iv.0.iter()).all(|(p_item, i_item)| {
                    Val::_pattern_match(assigns, &p_item.1, &i_item.1)
                })
            }
            (&Val::Struct(ref pt, ref pv), &Val::Struct(ref it, ref iv))
                if pv.0.len() == iv.0.len() =>
            {
                // this type check shouldn't be necessary
                // if the type checking was right
                if pt != it {
                    return false;
                }
                pv.0.iter().zip(iv.0.iter()).all(|(p_item, i_item)| {
                    Val::_pattern_match(assigns, &p_item.1, &i_item.1)
                })
            }
            (&Val::EnumStruct(ref pt, ref pname, ref pv)
                    , &Val::EnumStruct(ref it, ref iname, ref iv))
                if pv.0.len() == iv.0.len() =>
            {
                if pt != it || pname != iname {
                    return false;
                }
                pv.0.iter().zip(iv.0.iter()).all(|(p_item, i_item)| {
                    Val::_pattern_match(assigns, &p_item.1, &i_item.1)
                })
            }
            (&Val::EnumToken(ref pt, ref pname)
                    , &Val::EnumToken(ref it, ref iname)) =>
            {
                pt == it && pname == iname
            }
            (&Val::Token(ref pt), &Val::Token(ref it)) =>
            {
                pt == it
            }
            (&Val::Nil, &Val::Nil) => true,
            _ => false,
        };
        result
    }

    fn _pattern_match_list(assigns: &mut Vec<(Reg, Val)>,
        patt: &Val, input: &Val) -> bool
    {
        let result = match (patt, input) {
            (&Val::Cons(ref ph, ref pt), &Val::Cons(ref ih, ref it)) => {
                Val::_pattern_match(assigns, ph, ih)
                    && Val::_pattern_match_list(assigns, pt, it)
            }
            (&Val::Wildcard, _) => true,
            (&Val::PatternVar(ref dst), _) => {
                assigns.push((dst.clone(), input.clone()));
                true
            }
            (&Val::Nil, &Val::Nil) => true,
            _ => false,
        };
        result
    }

    pub fn replace_ids(node: &Val,
        idvals: &HashMap<Rc<String>, Val>) -> Val
    {
        match node {
            &Val::Cons(_, _) => {
                let f = |v: &Val| -> Val {
                    Val::replace_ids(v, idvals)
                };
                list::map_ref(&node, f)
            }
            &Val::Tuple(ref t) => {
                let new_items = t.0.iter().map(|i| {
                    (i.0.clone(), Val::replace_ids(&i.1, idvals))
                }).collect();
                Val::Tuple(Struple(new_items))
            }
            &Val::Struct(ref st, ref t) => {
                let new_items = t.0.iter().map(|i| {
                    (i.0.clone(), Val::replace_ids(&i.1, idvals))
                }).collect();
                Val::Struct(st.clone(), Struple(new_items))
            }
            &Val::EnumStruct(ref st, ref vname, ref t) => {
                let new_items = t.0.iter().map(|i| {
                    (i.0.clone(), Val::replace_ids(&i.1, idvals))
                }).collect();
                Val::EnumStruct(st.clone(), vname.clone(), Struple(new_items))
            }
            // tokens are fine to just clone
            &Val::Id(ref name) => {
                match idvals.get(&*name) {
                    Some(newx) => newx.clone(),
                    None => node.clone()
                }
            }
            _ => node.clone(),
        }
    }

    pub fn deep_clone(&self) -> Val
    {
        match self {
            &Val::Int(i) => Val::Int(i),
            &Val::Str(ref s) => Val::new_str((**s).clone()),
            &Val::Bool(b) => Val::Bool(b),
            &Val::Hashtag(ref s) => Val::hashtag((**s).clone()),
            &Val::Cons(ref head, ref tail) => {
                Val::Cons(
                    Box::new(head.deep_clone()),
                    Rc::new(tail.deep_clone()),
                )
            }
            &Val::Nil => Val::Nil,
            &Val::Tuple(ref flds) => {
                Val::Tuple(flds.clone_for_send())
            }
            &Val::Struct(ref typ, ref flds) => {
                Val::Struct(typ.deep_clone(), flds.clone_for_send())
            }
            &Val::EnumStruct(ref typ, ref vname, ref flds) => {
                Val::EnumStruct(typ.deep_clone(), vname.deep_clone(),
                    flds.clone_for_send(),
                )
            }
            &Val::EnumToken(ref typ, ref vname) => {
                Val::EnumToken(typ.deep_clone(), vname.deep_clone())
            }
            &Val::Token(ref typ) => {
                Val::Token(typ.deep_clone())
            }
            &Val::FuncRef(ref modname, ref fname, ref typ) => {
                Val::FuncRef(
                    Rc::new((**modname).clone()),
                    Rc::new((**fname).clone()),
                    typ.deep_clone()
                )
            }
            // &Val::Failure(ref tag, ref msg, ref ft),
            &Val::Id(ref s) => Val::id((**s).clone()),
            &Val::Type(ref t) => Val::Type(t.deep_clone()),
            &Val::ResourceRef(r) => Val::ResourceRef(r),
            &Val::Kind(k) => Val::Kind(k),
            // &Val::Lib(LibVal),
            // &Val::RustBlock,
            // &Val::Future(FutureVal),
            &Val::Void => Val::Void,
            &Val::Wildcard => Val::Wildcard,
            &Val::PatternVar(ref r) => Val::PatternVar(r.clone()),
            _ => {
                panic!("cannot deep clone val: {:?}", self);
            }
        }
    }

    fn fmt_list(f: &mut fmt::Formatter, l: &Val, dbg: bool) -> fmt::Result
    {
        match l {
            &Val::Cons(ref head, ref tail) => {
                if dbg {
                    write!(f, "{:?},", head)?;
                } else {
                    write!(f, "{},", head)?;
                }
                Val::fmt_list(f, tail, dbg)
            }
            &Val::Nil => {
                // do nothing, we've formatted enough
                write!(f, "")
            }
            &Val::Wildcard => {
                write!(f, ";_")
            }
            &Val::Id(ref name) => {
                if dbg {
                    write!(f, ";{:?}", l)
                } else {
                    write!(f, ";{}", name)
                }
            }
            &Val::PatternVar(_) => {
                write!(f, ";{:?}", l)
            }
            _ => {
                panic!("Not a list: {:?}", l);
            }
        }
    }

    fn fmt_tuple(f: &mut fmt::Formatter, t: &Vec<Val>, dbg: bool) -> fmt::Result
    {
        f.write_str("(")?;
        for x in t {
            if dbg {
                write!(f, "{:?},", x)?;
            } else {
                write!(f, "{},", x)?;
            }
        }
        f.write_str(")")
    }
}

impl From<Error> for Val
{
    fn from(_e: Error) -> Val
    {
        Val::Void
    }
}

impl sendclone::SendClone for Val
{
    type Item = Val;

    fn clone_for_send(&self) -> Val
    {
        self.deep_clone()
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Val::Str(ref s) => {
                write!(f, "{}", s)
            }
            Val::Int(ref i) => {
                write!(f, "{}", i)
            }
            Val::Bool(false) => {
                write!(f, "false")
            }
            Val::Bool(true) => {
                write!(f, "true")
            }
            Val::Cons(_, _) => {
                write!(f, "[")
                    .and_then(|_| {
                        Val::fmt_list(f, self, false)
                    })
                    .and_then(|_| {
                        write!(f, "]")
                    })
            }
            Val::Nil => {
                write!(f, "[]")
            }
            Val::Hashtag(ref s) => {
                write!(f, "#{}", s)
            }
            Val::Tuple(ref items) => {
                write!(f, "{}", items)
            }
            Val::Struct(ref typename, ref items) => {
                write!(f, "{}{}", typename, items)
            }
            Val::EnumStruct(ref tname, ref var, ref items) => {
                write!(f, "{}.{}{}", tname, var, items)
            }
            Val::EnumToken(_, ref var_name) => {
                write!(f, "{}", var_name)
            }
            Val::Token(ref typename) => {
                write!(f, "{}", typename)
            }
            Val::Buffer(ref _buf) => {
                write!(f, "Buffer")
            }
            Val::Lib(ref lv) => {
                write!(f, "LibVal({:?})", lv)
            }
            Val::LibRc(ref lv) => {
                write!(f, "LibValRc({:?})", lv)
            }
            Val::ResourceRef(rid) => {
                write!(f, "ResourceRef({})", rid)
            }
            Val::RustBlock => {
                write!(f, "RustBlock")
            }
            Val::Failure(ref tag, ref msg, ref stack, _status) => {
                write!(f, "Failure({}, {}\n{})", tag, msg, **stack)
            }
            Val::Lri(ref name) => {
                write!(f, "{}", name)
            }
            Val::Id(ref name) => {
                write!(f, "{}", name)
            }
            Val::Type(ref t) => {
                write!(f, "{}", t)
            }
            Val::Kind(c) => {
                write!(f, "Kind({})", c)
            }
            Val::FuncRef(ref module, ref name, ref typ) => {
                write!(f, "{}::{} : {}", module, name, typ)
            }
            Val::Future(_) => {
                write!(f, "Future")
            }
            Val::Void => {
                write!(f, "Void")
            }
            Val::PatternVar(ref r) => {
                write!(f, "pvar:{:?}", r)
            }
            Val::Wildcard => {
                write!(f, "_")
            }
        }
    }
}

impl fmt::Debug for Val {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Val::Str(ref s) => {
                let escaped = s.replace("\n", "\\n");
                write!(f, "Str(\"{}\")", escaped)
            }
            Val::Int(ref i) => {
                write!(f, "Int({})", i)
            }
            Val::Bool(b) => {
                write!(f, "Bool({:?})", b)
            }
            Val::Cons(_, _) => {
                write!(f, "L[")
                    .and_then(|_| {
                        Val::fmt_list(f, self, true)
                    })
                    .and_then(|_| {
                        write!(f, "]")
                    })
            }
            Val::Nil => {
                write!(f, "L[]")
            }
            Val::Hashtag(ref s) => {
                write!(f, "#{}", s)
            }
            Val::Buffer(ref buf) => {
                write!(f, "Buffer<{:?}>", buf)
            }
            Val::Tuple(ref fields) => {
                write!(f, "struple {:?}", fields)
            }
            Val::Struct(ref typ, ref fields) => {
                write!(f, "struct({}{:?})", typ, fields)
            }
            Val::EnumStruct(ref name, ref var_name, ref val) => {
                write!(f, "enum({:?}.{}{:?})", name, var_name, val)
            }
            Val::EnumToken(ref typ, ref var_name) => {
                write!(f, "EnumToken({:?}.{:?})", typ, var_name)
            }
            Val::Token(ref name) => {
                write!(f, "Token({:?})", name)
            }
            Val::Lib(ref lv) => {
                write!(f, "LibVal({:?})", lv)
            }
            Val::LibRc(ref lv) => {
                write!(f, "LibValRc({:?})", lv)
            }
            Val::ResourceRef(rid) => {
                write!(f, "ResourceRef({})", rid)
            }
            Val::RustBlock => {
                write!(f, "RustBlock")
            }
            Val::Failure(ref tag, ref msg, ref stack, status) => {
                write!(f, "Failure({}, {}, {}, {:?})", tag, status, msg, stack)
            }
            Val::Lri(ref name) => {
                write!(f, "{:?}", name)
            }
            Val::Id(ref id) => {
                write!(f, "Id({})", id)
            }
            Val::Type(ref t) => {
                write!(f, "TypeVal({:?})", t)
            }
            Val::Kind(c) => {
                write!(f, "Kind{:?}", c)
            }
            Val::FuncRef(ref module, ref name, ref typ) => {
                write!(f, "FuncRef({}::{} : {})", module, name, typ)
            }
            Val::Future(_) => {
                write!(f, "Future")
            }
            Val::PatternVar(ref r) => {
                write!(f, "pvar:{:?}", r)
            }
            Val::Void => {
                write!(f, "Void")
            }
            Val::Wildcard => {
                write!(f, "_Wildcard")
            }
        }
    }
}

/*
impl fmt::Debug for Val {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}
*/


impl reg::Iregistry for Val
{
    fn ireg_get(&self, i: &Ireg) -> &Val
    {
        match (i, self) {
            // get reg on tuple
            (_, &Val::Tuple(ref items)) => {
                items.ireg_get(i)
            }
            // get reg on struct
            (_, &Val::Struct(_, ref items)) => {
                items.ireg_get(i)
            }
            // Failures
            (&Ireg::Reg(0), &Val::Failure(ref tag, _, _, _)) => tag,
            (&Ireg::Reg(1), &Val::Failure(_, ref msg, _, _)) => msg,
            (&Ireg::Reg(2), &Val::Failure(_, _, ref trace, _)) => {
                panic!("Cannot access frame trace until it is implemented as a leema value {}", trace);
            }
            (&Ireg::Sub(_, _), &Val::Failure(ref tag, ref msg, _, _)) => {
                panic!("Cannot access sub data for Failure {} {}", tag, msg);
            }
            _ => {
                panic!("unsupported registry value {:?}{:?}",
                    self, i);
            }
        }
    }

    fn ireg_get_mut<'a, 'b>(&'a mut self, i: &'b Ireg) -> &'a mut Val
    {
        match (i, self) {
            (_, &mut Val::Tuple(ref mut items)) => {
                items.ireg_get_mut(i)
            }
            _ => {
                panic!("Tuple is only mut registry value");
            }
        }
    }

    fn ireg_set(&mut self, i: &Ireg, v: Val)
    {
        match (i, self) {
            // set reg on tuples
            (_, &mut Val::Tuple(ref mut fields)) => {
                fields.ireg_set(i, v);
            }
            // set reg on structs
            (_, &mut Val::Struct(_, ref mut fields)) => {
                fields.ireg_set(i, v);
            }
            // set reg on lists
            (&Ireg::Reg(0), &mut Val::Cons(ref mut head, _)) => {
                *head = Box::new(v);
            }
            (&Ireg::Sub(0, ref s), &mut Val::Cons(ref mut head, _)) => {
                head.ireg_set(&*s, v);
            }
            (_, &mut Val::Cons(_, _)) => {
                panic!("Cannot set a register on a list");
            }
            (_, &mut Val::Nil) => {
                panic!("cannot set reg on empty list: {:?}", i);
            }
            // set reg on Failures
            (&Ireg::Reg(0), &mut Val::Failure(ref mut tag, _, _, _)) => {
                *tag = Box::new(v);
            }
            (&Ireg::Reg(1), &mut Val::Failure(_, ref mut msg, _, _)) => {
                *msg = Box::new(v);
            }
            // values that can't act as registries
            _ => {
                panic!("Can't ireg_set({:?})", i);
            }
        }
    }
}

impl PartialOrd for Val
{
    fn partial_cmp(&self, other: &Val) -> Option<Ordering>
    {
        match (self, other) {
            (&Val::Int(a), &Val::Int(b)) => {
                PartialOrd::partial_cmp(&a, &b)
            }
            (&Val::Str(ref a), &Val::Str(ref b)) => {
                PartialOrd::partial_cmp(a, b)
            }
            (&Val::Bool(false), &Val::Bool(false)) => {
                Some(Ordering::Equal)
            }
            (&Val::Bool(true), &Val::Bool(true)) => {
                Some(Ordering::Equal)
            }
            (&Val::Nil, &Val::Nil) => {
                Some(Ordering::Equal)
            }
            (&Val::Hashtag(ref a), &Val::Hashtag(ref b)) => {
                PartialOrd::partial_cmp(a, b)
            }
            (&Val::Type(ref a), &Val::Type(ref b)) => {
                PartialOrd::partial_cmp(a, b)
            }
            (&Val::Id(ref a), &Val::Id(ref b)) => {
                PartialOrd::partial_cmp(a, b)
            }
            (&Val::Wildcard, &Val::Wildcard) => {
                Some(Ordering::Equal)
            }
            (&Val::Nil, &Val::Cons(_, _)) => {
                Some(Ordering::Less)
            }
            (&Val::Cons(_, _), &Val::Nil) => {
                Some(Ordering::Greater)
            }
            (&Val::Cons(ref h1, ref t1), &Val::Cons(ref h2, ref t2)) => {
                let cmp = PartialOrd::partial_cmp(&*h1, &*h2);
                match cmp {
                    Some(Ordering::Equal) => {
                        PartialOrd::partial_cmp(
                            &*t1, &*t2
                        )
                    }
                    _ => {
                        cmp
                    }
                }
            }
            (&Val::RustBlock, &Val::RustBlock) => {
                Some(Ordering::Equal)
            }
            (&Val::Void, &Val::Void) => {
                Some(Ordering::Equal)
            }
            // tuple to tuple comparison
            (&Val::Tuple(ref av), &Val::Tuple(ref bv)) => {
                PartialOrd::partial_cmp(av, bv)
            }
            // struct to struct comparison
            (&Val::Struct(ref at, ref av), &Val::Struct(ref bt, ref bv)) => {
                match PartialOrd::partial_cmp(&*at, &*bt) {
                    Some(Ordering::Equal) => {
                        PartialOrd::partial_cmp(av, bv)
                    }
                    tcmp => tcmp,
                }
            }
            // enum to enum comparison
            (&Val::EnumStruct(ref at, ref an, ref av)
                    , &Val::EnumStruct(ref bt, ref bn, ref bv)) =>
            {
                Some(PartialOrd::partial_cmp(&*at, &*bt).unwrap()
                    .then_with(|| {
                        PartialOrd::partial_cmp(an, bn).unwrap()
                    })
                    .then_with(|| {
                        PartialOrd::partial_cmp(av, bv).unwrap()
                    })
                )
            }
            // enumtoken to enumtoken comparison
            (&Val::EnumToken(ref at, ref an), &Val::EnumToken(ref bt, ref bn)) => {
                Some(PartialOrd::partial_cmp(&*at, &*bt).unwrap()
                    .then_with(|| {
                        PartialOrd::partial_cmp(&*an, &*bn).unwrap()
                    })
                )
            }
            // token to token comparison
            (&Val::Token(ref at), &Val::Token(ref bt)) => {
                PartialOrd::partial_cmp(&*at, &*bt)
            }
            // func ref to func ref comparison
            (&Val::FuncRef(ref m1, ref n1, ref t1)
                    , &Val::FuncRef(ref m2, ref n2, ref t2)) =>
            {
                Some(PartialOrd::partial_cmp(m1, m2).unwrap()
                    .then_with(|| {
                        PartialOrd::partial_cmp(n1, n2).unwrap()
                    })
                    .then_with(|| {
                        PartialOrd::partial_cmp(t1, t2).unwrap()
                    })
                )
            }
            (&Val::ResourceRef(rra), &Val::ResourceRef(rrb)) => {
                PartialOrd::partial_cmp(&rra, &rrb)
            }
            (&Val::Buffer(ref b1), &Val::Buffer(ref b2)) => {
                PartialOrd::partial_cmp(b1, b2)
            }

            // start comparing mixed types
            (&Val::Bool(false), _) => {
                Some(Ordering::Less)
            }
            (_, &Val::Bool(false)) => {
                Some(Ordering::Greater)
            }
            (&Val::Bool(true), _) => {
                Some(Ordering::Less)
            }
            (_, &Val::Bool(true)) => {
                Some(Ordering::Greater)
            }
            (&Val::Int(_), _) => {
                Some(Ordering::Less)
            }
            (_, &Val::Int(_)) => {
                Some(Ordering::Greater)
            }
            (&Val::Str(_), _) => {
                Some(Ordering::Less)
            }
            (_, &Val::Str(_)) => {
                Some(Ordering::Greater)
            }
            (&Val::Hashtag(_), _) => {
                Some(Ordering::Less)
            }
            (_, &Val::Hashtag(_)) => {
                Some(Ordering::Greater)
            }
            (&Val::Type(_), _) => {
                Some(Ordering::Less)
            }
            (_, &Val::Type(_)) => {
                Some(Ordering::Greater)
            }
            (&Val::Id(_), _) => {
                Some(Ordering::Less)
            }
            (_, &Val::Id(_)) => {
                Some(Ordering::Greater)
            }
            (&Val::ResourceRef(_), _) => {
                Some(Ordering::Less)
            }
            (_, &Val::ResourceRef(_)) => {
                Some(Ordering::Greater)
            }
            (&Val::Nil, _) => {
                Some(Ordering::Less)
            }
            (&Val::Cons(_, _), _) => {
                Some(Ordering::Less)
            }
            (_, &Val::Nil) => {
                Some(Ordering::Greater)
            }
            (_, &Val::Cons(_, _)) => {
                Some(Ordering::Greater)
            }
            (&Val::Tuple(_), _) => {
                Some(Ordering::Less)
            }
            (_, &Val::Tuple(_)) => {
                Some(Ordering::Greater)
            }
            (&Val::Struct(_, _), _) => {
                Some(Ordering::Less)
            }
            (_, &Val::Struct(_, _)) => {
                Some(Ordering::Greater)
            }
            (&Val::EnumStruct(_, _, _), _) => {
                Some(Ordering::Less)
            }
            (_, &Val::EnumStruct(_, _, _)) => {
                Some(Ordering::Greater)
            }
            (&Val::EnumToken(_, _), _) => {
                Some(Ordering::Less)
            }
            (_, &Val::EnumToken(_, _)) => {
                Some(Ordering::Greater)
            }
            (&Val::Token(_), _) => {
                Some(Ordering::Less)
            }
            (_, &Val::Token(_)) => {
                Some(Ordering::Greater)
            }
            (&Val::Void, _) => {
                Some(Ordering::Less)
            }
            (_, &Val::Void) => {
                Some(Ordering::Greater)
            }
            (&Val::RustBlock, _) => Some(Ordering::Less),
            (_, &Val::RustBlock) => Some(Ordering::Greater),
            (&Val::Wildcard, _) => Some(Ordering::Less),
            (_, &Val::Wildcard) => Some(Ordering::Greater),
            _ => {
                panic!("cannot compare({:?},{:?})", self, other);
            }
        }
    }
}

impl PartialEq for Val
{
    fn eq(&self, other: &Val) -> bool
    {
        let cmp = PartialOrd::partial_cmp(self, other);
        if cmp.is_none() {
            false
        } else {
            cmp.unwrap() == Ordering::Equal
        }
    }
}

impl AsMut<[u8]> for Val
{
    fn as_mut(&mut self) -> &mut [u8]
    {
        match self {
            &mut Val::Buffer(ref mut buf) => {
                buf.as_mut()
            }
            _ => {
                panic!("Cannot convert val to AsMut<[u8]>: {:?}", self);
            }
        }
    }
}

/*
impl Clone for Val
{
    fn clone(&self) -> Val
    {
        match self {
            &Val::Int(i) => {
                Val::Int(i)
            }
            &Val::Str(ref s) => {
                Val::Str(s.clone())
            }
            &Val::Bool(b) => {
                Val::Bool(b)
            }
            &Val::Hashtag(ref h) => {
                Val::Hashtag(h.clone())
            }
            &Val::List(ref l) => {
                Val::List(l.clone())
            }
            &Val::Tuple(ref t) => {
                Val::Tuple(t.clone())
            }
            &Val::Failure => {
                Val::Failure
            }
            &Val::Sxpr(ref s) => {
                Val::Sxpr(s.clone())
            }
            &Val::Type(ref t) => {
                Val::Type(t.clone())
            }
            &Val::Lib(ref lv, ref typ) => {
                Val::Lib((*lv).clone(), typ.clone())
            }
            &Val::Void => {
                Val::Void
            }
        }
    }
}
*/



#[derive(Debug)]
#[derive(Clone)]
pub struct Env
{
    params: Val,
    result: Option<Val>,
    reg: BTreeMap<i8, Val>,
    kv: BTreeMap<String, Val>,
    error: Val,
}

impl Env
{
    pub fn new() -> Env {
        Env{
            params: Val::Void,
            result: None,
            reg: BTreeMap::new(),
            kv: BTreeMap::new(),
            error: Val::Void,
        }
    }

    pub fn with_args(v: Val) -> Env
    {
        Env{
            params: v,
            result: None,
            reg: BTreeMap::new(),
            kv: BTreeMap::new(),
            error: Val::Void,
        }
    }

    pub fn get_reg_mut(&mut self, reg: &Reg) -> &mut Val
    {
        match reg {
            &Reg::Param(ref r) => {
                self.params.ireg_get_mut(r)
            }
            &Reg::Local(ref i) => {
                self.ireg_get_mut(i)
            }
            &Reg::Void => {
                panic!("Cannot get Reg::Void");
            }
            &Reg::Lib => {
                panic!("Please look in application library for Reg::Lib");
            }
            &Reg::Undecided => {
                panic!("Cannot get undecided register");
            }
        }
    }

    pub fn set_reg(&mut self, reg: &Reg, v: Val) {
        match reg {
            &Reg::Local(ref i) => {
                self.ireg_set(i, v)
            }
            &Reg::Void => {
                // do nothing, void reg is like /dev/null
            }
            _ => {
                panic!("set other reg: {:?}", reg);
            }
        }
    }

    pub fn get_reg(&self, reg: &Reg) -> &Val
    {
        match reg {
            &Reg::Param(ref r) => {
                self.params.ireg_get(r)
            }
            &Reg::Local(ref i) => {
                self.ireg_get(i)
            }
            &Reg::Void => {
                panic!("Cannot get Reg::Void");
            }
            &Reg::Lib => {
                panic!("Please look in application library for Reg::Lib");
            }
            &Reg::Undecided => {
                panic!("Cannot get undecided register");
            }
        }
    }

    pub fn get_params(&self) -> &Val
    {
        &self.params
    }

    pub fn get_param(&self, reg: i8) -> &Val
    {
        self.get_reg(&Reg::Param(Ireg::Reg(reg)))
    }

    pub fn get_param_mut(&mut self, reg: i8) -> &mut Val
    {
        self.get_reg_mut(&Reg::Param(Ireg::Reg(reg)))
    }
}

impl reg::Iregistry for Env
{
    fn ireg_get(&self, i: &Ireg) -> &Val
    {
        let p = i.get_primary();
        if !self.reg.contains_key(&p) {
            vout!("{:?} not set in {:?}\n", i, self.reg);
            panic!("register is not set: {:?}", i);
        }
        let v = self.reg.get(&p).unwrap();

        if let &Ireg::Sub(_, ref s) = i {
            v.ireg_get(&*s)
        } else {
            v
        }
    }

    fn ireg_get_mut(&mut self, i: &Ireg) -> &mut Val
    {
        let p = i.get_primary();
        if !self.reg.contains_key(&p) {
            vout!("{:?} not set in {:?}\n", i, self.reg);
            panic!("register_mut is not set: {:?}", i);
        }
        let v = self.reg.get_mut(&p).unwrap();

        if let &Ireg::Sub(_, ref s) = i {
            v.ireg_get_mut(&*s)
        } else {
            v
        }
    }

    fn ireg_set(&mut self, i: &Ireg, v: Val)
    {
        match i {
            &Ireg::Reg(p) => {
                if self.reg.contains_key(&p) {
                    vout!("register already set: {:?}\n", i);
                    vout!("\toverwrite {:?}\twith {:?}\n", self.reg.get(&p), v);
                }
                self.reg.insert(p, v);
            }
            &Ireg::Sub(p, ref s) => {
                if !self.reg.contains_key(&p) {
                    panic!("primary register not set: {:?}", i);
                }
                self.reg.get_mut(&p).unwrap().ireg_set(&*s, v);
            }
        }
    }
}


#[cfg(test)]
mod tests {
    use leema::val::{Type, Val, SrcLoc};
    use leema::list;
    use leema::lri::{Lri};
    use leema::lstr::{Lstr};
    use leema::reg::{Reg};
    use leema::struple::{Struple};

    use std::collections::{HashMap};
    use std::rc::{Rc};


#[test]
fn test_equal_type_str() {
    assert_eq!(Type::Str, Type::Str);
}

#[test]
fn test_equal_type_int() {
    assert_eq!(Type::Int, Type::Int);
}

#[test]
fn test_tuple_from_list() {
    let origl = list::cons(Val::Int(4), list::singleton(Val::Int(7)));
    let tuple = Val::tuple_from_list(&origl);
    print!("wtf?({:?})", tuple);
    let exp = Val::Tuple(Struple::new_tuple2(Val::Int(4), Val::Int(7)));
    assert_eq!(exp, tuple);
}

#[test]
fn test_equal_int() {
    let a = Val::Int(7);
    let b = Val::Int(7);
    assert!(a == b);
}

#[test]
fn test_equal_str() {
    let a = Val::new_str("hello".to_string());
    let b = Val::new_str("hello".to_string());
    assert!(a == b);
}

#[test]
fn test_equal_true() {
    let a = Val::Bool(true);
    let b = Val::Bool(true);
    assert!(a == b);
}

#[test]
fn test_equal_false() {
    let a = Val::Bool(false);
    let b = Val::Bool(false);
    assert!(a == b);
}

#[test]
fn test_tuple() {
    let a = Val::Tuple(Struple::new_tuple2(Val::Int(3), Val::Int(7)));
    let b = Val::Tuple(Struple::new_tuple2(Val::Int(3), Val::Int(7)));
    assert!(a == b);
}

#[test]
fn test_compare_false_true() {
    let f = Val::Bool(false);
    let t = Val::Bool(true);
    assert!(f < t);
}

#[test]
fn test_compare_true_false() {
    let f = Val::Bool(false);
    let t = Val::Bool(true);
    assert!(t > f);
}

#[test]
fn test_struct_eq() {
    let t = Lri::new(Lstr::Sref("Taco"));
    let a =
        Val::Struct(t.clone(), Struple::new_tuple2(
            Val::Int(3),
            Val::Bool(false),
        ));
    let b =
        Val::Struct(t, Struple::new_tuple2(
            Val::Int(3),
            Val::Bool(false),
        ));
    assert_eq!(a, b);
}

#[test]
fn test_struct_lt_type() {
    let a =
        Val::Struct(
            Lri::new(Lstr::Sref("Burrito")),
            Struple::new_tuple2(
                Val::Int(3),
                Val::Bool(false),
            ),
        );
    let b =
        Val::Struct(
            Lri::new(Lstr::Sref("Taco")),
            Struple::new_tuple2(
                Val::Int(3),
                Val::Bool(false),
            ),
        );
    assert!(a < b);
}

#[test]
fn test_struct_lt_val() {
    let typ = Lri::new(Lstr::Sref("Taco"));
    let a =
        Val::Struct(typ.clone(), Struple::new_tuple2(
            Val::Bool(false),
            Val::Int(3),
        ));
    let b =
        Val::Struct(typ.clone(), Struple::new_tuple2(
            Val::Bool(false),
            Val::Int(7),
        ));
    assert!(a < b);
}

#[test]
fn test_enum_eq() {
    let etype = Lri::with_modules(
        Lstr::Sref("animals"),
        Lstr::Sref("Animal"),
    );

    let a =
        Val::EnumToken(
            etype.clone(),
            Lstr::Sref("Dog"),
        );
    let b =
        Val::EnumToken(
            etype.clone(),
            Lstr::Sref("Dog"),
        );
    assert_eq!(a, b);
}

#[test]
fn test_enum_lt_type()
{
    let typ = Lri::new(Lstr::Sref("Taco"));
    let a =
        Val::EnumToken(
            typ.clone(),
            Lstr::Sref("Quesadilla"),
        );
    let b =
        Val::EnumToken(
            typ,
            Lstr::Sref("Torta"),
        );
    assert!(a < b);
}

#[test]
fn test_enum_lt_variant()
{
    let typ = Lri::new(Lstr::Sref("Taco"));
    let a =
        Val::EnumToken(
            typ.clone(),
            Lstr::Sref("Burrito"),
        );
    let b =
        Val::EnumToken(
            typ,
            Lstr::Sref("Torta"),
        );
    assert!(a < b);
}

#[test]
fn test_enum_lt_val()
{
    let typ = Lri::new(Lstr::Sref("Taco"));
    let a =
        Val::EnumStruct(
            typ.clone(),
            Lstr::Sref("Burrito"),
            Struple::new_tuple2(Val::Int(5), Val::Int(8)),
        );
    let b =
        Val::EnumStruct(
            typ,
            Lstr::Sref("Burrito"),
            Struple::new_tuple2(Val::Int(9), Val::Int(8)),
        );
    assert!(a < b);
}

#[test]
fn test_format_struct_empty()
{
    let s = Val::Token(Lri::new(Lstr::Sref("Taco")));

    let s_str = format!("{}", s);
    assert_eq!("Taco", s_str);
}

#[test]
fn test_format_enum_token()
{
    let type_lri = Lri::new(Lstr::Sref("Taco"));
    let e = Val::EnumToken(
        type_lri,
        Lstr::Sref("Burrito"),
    );

    let e_str = format!("{}", e);
    assert_eq!("Burrito", e_str);
}

#[test]
fn test_format_enum_namedtuple()
{
    let burrito_str = Lstr::Sref("Burrito");
    let stype_lri = Lri::with_modules(
        Lstr::Sref("tortas"),
        Lstr::Sref("Taco"),
    );
    let s = Val::EnumStruct(
        stype_lri,
        burrito_str.clone(),
        Struple::new_tuple2(Val::Int(5), Val::Int(8)),
    );

    let s_str = format!("{}", s);
    assert_eq!("tortas::Taco.Burrito(5,8,)", s_str);
}

#[test]
fn test_compare_across_types() {
    let f = Val::Bool(false);
    let t = Val::Bool(true);
    let i = Val::Int(7);
    let s = Val::new_str("hello".to_string());
    let strct = Val::Struct(
        Lri::new(Lstr::Sref("Foo")),
        Struple::new_tuple2(Val::Int(2), Val::Bool(true)),
    );
    let enm = Val::EnumStruct(
        Lri::new(Lstr::Sref("Taco")),
        Lstr::Sref("Burrito"),
        Struple::new_tuple2(Val::Int(8), Val::Int(6)),
    );

    assert!(f < t);
    assert!(t < i);
    assert!(i < s);
    assert!(i < strct);
    assert!(i < enm);
    assert!(strct < enm);
    assert!(strct < Val::Wildcard);
    assert!(enm < Val::Wildcard);
}

#[test]
fn test_get_type_empty_list()
{
    let typ = Val::Nil.get_type();
    assert_eq!(Type::StrictList(Box::new(Type::Unknown)), typ);
}

#[test]
fn test_get_type_int_list()
{
    let typ = list::from2(Val::Int(3), Val::Int(8)).get_type();
    assert_eq!(Type::StrictList(Box::new(Type::Int)), typ);
}

#[test]
fn test_pattern_match_list_cons_wildcard_head()
{
    let patt = list::cons(Val::Wildcard, Val::PatternVar(Reg::local(1)));
    let input = list::from3(Val::Int(1), Val::Int(2), Val::Int(3));
    let pmatch = Val::pattern_match(&patt, &input);
    assert!(pmatch.is_some());
}

#[test]
fn test_pattern_match_list_cons_wildcard_tail()
{
    let patt = list::cons(Val::PatternVar(Reg::local(1)), Val::Wildcard);
    let input = list::from3(Val::Int(1), Val::Int(2), Val::Int(3));
    let pmatch = Val::pattern_match(&patt, &input);
    assert!(pmatch.is_some());
}

#[test]
fn test_pattern_match_wildcard_inside_tuple()
{
    let patt = Val::Tuple(Struple::new_tuple2(
        Val::Int(1), Val::Wildcard
        ));
    let input = Val::Tuple(Struple::new_tuple2(
        Val::Int(1), Val::Int(4)
        ));
    let pmatch = Val::pattern_match(&patt, &input);
    assert!(pmatch.is_some());
}

}
