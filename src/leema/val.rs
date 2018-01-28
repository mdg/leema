use leema::reg::{self, Reg, Ireg, Iregistry};
use leema::sxpr;
use leema::list;
use leema::frame::{FrameTrace};
use leema::log;

use std::fmt::{self};
use std::collections::{BTreeMap, HashMap};
use std::sync::{Arc, Mutex};
use std::sync::atomic::{self, AtomicBool};
use std::rc::{Rc};
use std::cmp::{PartialEq, PartialOrd, Ordering};
use std::clone::Clone;
use std::fmt::{Debug};
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
    Tuple(Vec<Type>),
    Struct(Rc<String>),
    Enum(Rc<String>),
    Failure,
    Func(Vec<Type>, Box<Type>),
    // different from base collection/map interfaces?
    // base interface/type should probably be iterator
    // and then it should be a protocol, not type
    StrictList(Box<Type>),
    Lib(String),
    Resource(Rc<String>),
    RustBlock,
    // Future(Box<Type>),
    Void,
    /*
    Map(Box<Type>, Box<Type>),
    */
    Kind,
    Any,

    Unknown,
    Id(Rc<String>),
    ModPrefix(Rc<String>, Rc<Type>),
    Texpr(Rc<String>, Vec<Type>),
    Var(Rc<String>),
    AnonVar,
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
    pub fn full_typename(&self) -> Rc<String>
    {
        match self {
            &Type::Int => Rc::new("Int".to_string()),
            &Type::Id(ref name) => name.clone(),
            &Type::Struct(ref name) => name.clone(),
            &Type::Enum(ref name) => name.clone(),
            &Type::ModPrefix(_, _) => {
                let str = format!("{}", self);
                Rc::new(str)
            }
            _ => {
                panic!("No typename for {:?}", self);
            }
        }
    }

    /**
     * Get the typename without any module information
     */
    pub fn local_typename(&self) -> Rc<String>
    {
        match self {
            &Type::ModPrefix(_, ref inner) => inner.local_typename(),
            _ => self.full_typename(),
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

    pub fn var_name(&self) -> Rc<String>
    {
        match self {
            &Type::Var(ref id) => id.clone(),
            &Type::AnonVar => Rc::new("anon".to_string()),
            _ => {
                panic!("Not a Type::Var {:?}", self);
            }
        }
    }

    pub fn var_name_str(&self) -> &str
    {
        match self {
            &Type::Var(ref id) => id,
            &Type::AnonVar => "anon",
            _ => {
                panic!("Not a Type::Var {:?}", self);
            }
        }
    }

    /**
     * Convert this ID type to a struct type
     */
    pub fn to_struct(&self) -> Type
    {
        match self {
            &Type::Id(ref name) => Type::Struct(name.clone()),
            &Type::Struct(ref name) => Type::Struct(name.clone()),
            &Type::ModPrefix(ref module, ref local) => {
                Type::ModPrefix(module.clone(), Rc::new(local.to_struct()))
            }
            _ => {
                panic!("cannot convert to struct type: {:?}", self);
            }
        }
    }

    /**
     * Check if this type is a structure
     */
    pub fn is_struct(&self) -> bool
    {
        match self {
            &Type::Struct(_) => true,
            &Type::ModPrefix(_, ref local) => local.is_struct(),
            _ => false,
        }
    }

    /**
     * Check if this type is an enum
     */
    pub fn is_enum(&self) -> bool
    {
        match self {
            &Type::Enum(_) => true,
            &Type::ModPrefix(_, ref local) => local.is_enum(),
            _ => false,
        }
    }

    pub fn tuple_items(self) -> Vec<Type>
    {
        match self {
            Type::Tuple(items) => items,
            _ => {
                panic!("No items in not tuple type {:?}", self);
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
            &Type::Struct(ref s) => {
                Type::Struct(Rc::new((**s).clone()))
            }
            &Type::Enum(ref s) => {
                Type::Enum(Rc::new((**s).clone()))
            }
            &Type::Id(ref id) => {
                let old_str: &str = &**id;
                Type::Id(Rc::new(old_str.to_string()))
            }
            &Type::ModPrefix(ref prefix, ref base) => {
                let new_prefix = (&**prefix).to_string();
                let new_base = base.deep_clone();
                Type::ModPrefix(Rc::new(new_prefix), Rc::new(new_base))
            }
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
                write!(f, "(");
                for i in items {
                    write!(f, "{},", i);
                }
                write!(f, ")")
            }
            &Type::Struct(ref name) => write!(f, "{}", name),
            &Type::Enum(ref name) => write!(f, "{}", name),
            &Type::Failure => write!(f, "Failure"),
            &Type::Func(ref args, ref result) => {
                for a in args {
                    write!(f, "{} > ", a).ok();
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
            &Type::Id(ref name) => write!(f, "{}", name),
            &Type::ModPrefix(ref prefix, ref sub) => {
                write!(f, "{}::{}", prefix, sub)
            }
            &Type::Texpr(ref base, ref args) => write!(f, "Texpr"),
            &Type::Var(ref name) => {
                write!(f, "Type::Var({})", name)
            }
            &Type::AnonVar => write!(f, "TypeAnonymous"),
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
                write!(f, "T(");
                for i in items {
                    write!(f, "{:?},", i);
                }
                write!(f, ")")
            }
            &Type::Struct(ref name) => {
                write!(f, "StructType({})", name)
            }
            &Type::Enum(ref name) => write!(f, "Enum({})", name),
            &Type::Failure => write!(f, "Failure"),
            &Type::Func(ref args, ref result) => {
                for a in args {
                    write!(f, "{}->", a).ok();
                }
                write!(f, "{}", result)
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
            &Type::Id(ref name) => write!(f, "TypeId({})", name),
            &Type::ModPrefix(ref prefix, ref sub) => {
                write!(f, "Module({})::{:?}", prefix, sub)
            }
            &Type::Texpr(ref base, ref args) => write!(f, "Texpr"),
            &Type::Var(ref name) => {
                write!(f, "Type::Var({})", name)
            }
            &Type::AnonVar => write!(f, "TypeAnonymous"),
        }
    }
}

#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub enum TypeErr
{
    Error(Rc<String>),
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

pub type TypeResult = Result<Type, TypeErr>;


pub trait LibVal
    : mopa::Any
    + Debug
{
    fn get_type(&self) -> Type;
}

mopafy!(LibVal);

#[derive(Clone)]
pub struct FutureVal(pub Arc<AtomicBool>, pub Arc<Mutex<Receiver<MsgVal>>>);

impl Debug for FutureVal
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


#[derive(Copy)]
#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
pub enum SxprType {
    Let,
    Fork,
    StrExpr,
    BlockExpr,
    Call,
    NamedParam,
    DefFunc,
    DefMacro,
    DefStruct,
    DefEnum,
    IfExpr,
    Import,
    MatchExpr,
    MatchFailed,
    Return,
    Comparison,
    /*
    LessThan3(bool, bool),
    BooleanAnd,
    BooleanOr,
    */
}

#[derive(Debug)]
pub enum MsgVal
{
    Int(i64),
    Str(String),
    Bool(bool),
    Hashtag(String),
    Cons(Box<MsgVal>, Box<MsgVal>),
    Tuple(Vec<MsgVal>),
    Buffer(Vec<u8>),
    Nil,
    Void,
    ResourceRef(i64),
    Failure(Box<MsgVal>, Box<MsgVal>, Arc<FrameTrace>, i8),
}

#[derive(Clone)]
pub enum Val {
    Int(i64),
    Str(Rc<String>),
    // StrCat(Rc<Val>, Box<Val>),
    // EmptyStr,
    Bool(bool),
    Hashtag(Rc<String>),
    Buffer(Vec<u8>),
    Cons(Box<Val>, Rc<Val>),
    Nil,
    Tuple(Vec<Val>),
    Sxpr(SxprType, Rc<Val>, SrcLoc),
    Struct(Type, Vec<Val>),
    Enum(Type, i16, Box<Val>),
    Failure(
        Box<Val>, // tag
        Box<Val>, // msg
        Arc<FrameTrace>,
        i8, // status
    ),
    Id(Rc<String>),
    ModPrefix(Rc<String>, Rc<Val>),
    TypedId(Rc<String>, Type),
    Type(Type),
    Kind(u8),
    DotAccess(Box<Val>, Rc<String>),
    Lib(Arc<LibVal>),
    LibRc(Rc<LibVal>),
    FuncRef(Rc<String>, Rc<String>, Type),
    ResourceRef(i64),
    RustBlock,
    Future(FutureVal),
    Void,
    Wildcard,
    PatternVar(Reg),
    Loc(Box<Val>, SrcLoc),
}

const NIL: Val = Val::Nil;
pub const VOID: Val = Val::Void;
pub const FALSE: Val = Val::Bool(false);
pub const TRUE: Val = Val::Bool(true);

impl Val
{
    pub fn is_sxpr(&self) -> bool
    {
        match self {
            &Val::Sxpr(_, _, _) => true,
            _ => false,
        }
    }

    pub fn is_sxpr_type(&self, st: SxprType) -> bool
    {
        match self {
            &Val::Sxpr(st, _, _) => true,
            _ => false,
        }
    }

    pub fn id(s: String) -> Val
    {
        Val::Id(Rc::new(s))
    }

    pub fn typed_id(s: &str, t: Type) -> Val
    {
        Val::TypedId(Rc::new(String::from(s)), t)
    }

    pub fn is_id(&self) -> bool
    {
        match self {
            &Val::Id(_) => true,
            &Val::TypedId(_, _) => true,
            &Val::Loc(ref v, _) => v.is_id(),
            _ => false,
        }
    }

    pub fn id_name(&self) -> Rc<String>
    {
        match self {
            &Val::Id(ref name) => name.clone(),
            &Val::TypedId(ref name, ref typ) => name.clone(),
            &Val::Type(Type::Id(ref name)) => name.clone(),
            &Val::Loc(ref v, _) => v.id_name(),
            _ => {
                panic!("not an id {:?}", self);
            }
        }
    }

    pub fn empty_tuple() -> Val
    {
        Val::new_tuple(0)
    }

    pub fn new_tuple(ref sz: usize) -> Val {
        let mut t = Vec::with_capacity(*sz);
        let mut i: usize = *sz;
        while i > 0 {
            t.push(VOID);
            i = i - 1;
        }
        Val::Tuple(t)
    }

    pub fn tuple_from_list(l: &Val) -> Val
    {
        // TODO switch to be list::to_vec(), use regular Tuple constructor
        if !l.is_list() {
            panic!("Cannot make tuple from not-list: {:?}", l);
        }
        let mut empties: Vec<Val> = Vec::with_capacity(list::len(l));
        let items = list::fold_ref(empties, l, |mut res, item| {
            res.push(item.clone());
            res
        });
        Val::Tuple(items)
    }

    pub fn is_list(&self) -> bool
    {
        match self {
            &Val::Cons(_, _) => true,
            &Val::Nil => true,
            _ => false,
        }
    }

    pub fn is_tuple(&self) -> bool
    {
        match self {
            &Val::Tuple(_) => true,
            _ => false,
        }
    }

    pub fn tuple_items(tup: Val) -> Vec<Val>
    {
        match tup {
            Val::Tuple(items) => items,
            _ => {
                panic!("No items in not tuple {:?}", tup);
            }
        }
    }

    pub fn tuple_items_ref(&self) -> &Vec<Val>
    {
        match self {
            &Val::Tuple(ref items) => items,
            _ => {
                panic!("No items in not tuple type {:?}", self);
            }
        }
    }

    /*
    pub fn take_tuple_vals(&mut self) -> Vec<Val>
    {
        match self {
            &mut Sxpr::Val(ref mut tv) => {
                let mut tmp = Box::new(Val::Void);
                mem::swap(&mut *tv, &mut tmp);
                match *tmp {
                    Val::Tuple(vals) => {
                        vals
                    }
                    _ => panic!("sxpr val is not a tuple"),
                }
            }
            _ => panic!("tuple sxpr not a val"),
        }
    }
    */

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
            &Val::TypedId(ref id, ref typ) => id,
            &Val::Str(ref s) => s,
            &Val::Loc(ref v, _) => v.str(),
            _ => {
                panic!("Cannot convert to string: {:?}", self);
            }
        }
    }

    pub fn to_str(&self) -> Rc<String>
    {
        match self {
            &Val::Id(ref id) => id.clone(),
            &Val::TypedId(ref id, ref typ) => id.clone(),
            &Val::Str(ref s) => s.clone(),
            &Val::ModPrefix(ref prefix, ref name) => {
                let name_str = name.to_str();
                let s = format!("{}::{}", prefix, name_str);
                Rc::new(s)
            }
            &Val::Loc(ref v, _) => v.to_str(),
            _ => {
                panic!("Cannot convert to string: {:?}", self);
            }
        }
    }

    pub fn to_int(&self) -> i64
    {
        match self {
            &Val::Int(i) => i,
            &Val::Loc(ref v, _) => v.to_int(),
            _ => {
                panic!("Not an int: {:?}", self);
            }
        }
    }

    pub fn hashtag(s: String) -> Val
    {
        Val::Hashtag(Rc::new(s))
    }

    pub fn dot_access(base: Val, sub: String) -> Val
    {
        Val::DotAccess(Box::new(base), Rc::new(sub))
    }

    pub fn is_type(&self) -> bool
    {
        match self {
            &Val::Type(_) => true,
            _ => false,
        }
    }

    pub fn to_type(&self) -> Type
    {
        match self {
            &Val::Type(ref t) => t.clone(),
            &Val::Id(ref id) => Type::Id(id.clone()),
            &Val::ModPrefix(ref prefix, ref base) => {
                let tbase = base.to_type();
                Type::ModPrefix(prefix.clone(), Rc::new(tbase))
            }
            &Val::Loc(ref v, _) => v.to_type(),
            _ => {
                panic!("Cannot unwrap not-type as type {:?}", self);
            }
        }
    }

    pub fn split_typed_id(&self) -> (Val, Type)
    {
        match self {
            &Val::Id(ref id) => (self.clone(), Type::AnonVar),
            &Val::TypedId(ref tid, ref typ) => (
                Val::Id(tid.clone()),
                typ.clone(),
            ),
            &Val::Loc(ref v, _) => v.split_typed_id(),
            _ => {
                panic!("not a TypedId: {:?}", self);
            }
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

    pub fn loc(v: Val, l: SrcLoc) -> Val
    {
        Val::Loc(Box::new(v), l)
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
            &Val::Tuple(ref items) if items.len() == 1 => {
                items.get(0).unwrap().get_type()
            }
            &Val::Tuple(ref items) => {
                let mut tuptypes = vec![];
                for i in items {
                    tuptypes.push(i.get_type());
                }
                Type::Tuple(tuptypes)
            }
            &Val::Cons(ref head, ref tail) => {
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
            &Val::TypedId(_, ref typ) => typ.clone(),
            &Val::Sxpr(SxprType::DefFunc, _, _) => sxpr::defunc_type(self),
            &Val::Sxpr(SxprType::StrExpr, _, _) => Type::Str,
            &Val::Sxpr(SxprType::BlockExpr, ref exprs, _) => {
                match list::last(exprs) {
                    Some(last) => {
                        last.get_type()
                    }
                    None => {
                        Type::Void
                    }
                }
            }
            &Val::Sxpr(SxprType::Call, _, _) => Type::Unknown,
            &Val::Sxpr(st, _, ref loc) => {
                panic!("cannot find type for: {:?}@{:?}", st, loc);
            }
            &Val::RustBlock => Type::RustBlock,
            &Val::Struct(ref typ, _) => {
                typ.clone()
            }
            &Val::Enum(ref typ, _, _) => {
                typ.clone()
            }
            &Val::Buffer(_) => Type::Str,
            &Val::ModPrefix(_, _) => {
                panic!("module prefixes have no type");
            }
            &Val::Kind(_) => {
                panic!("is kind even a thing here?");
            }
            &Val::DotAccess(_, _) => {
                panic!("maybe DotAccess should be an sxpr?");
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
            &Val::Loc(ref v, _) => v.get_type(),
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
            (&Val::Tuple(ref p), &Val::Tuple(ref i)) if p.len() == i.len() => {
                let it = p.iter().zip(i.iter());
                let m = it.fold(true, |m, (p_item, i_item)| {
                    m && Val::_pattern_match(assigns, p_item, i_item)
                });
                m
            }
            (&Val::Struct(_, ref dst), &Val::Struct(_, ref src)) => {
                let mut m = true;
                for (idst, isrc) in dst.iter().zip(src.iter()) {
                    m = m && Val::_pattern_match(assigns, idst, isrc);
                }
                m
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
                let mut result = Vec::with_capacity(t.len());
                for tv in t.iter() {
                    let rv = Val::replace_ids(tv, idvals);
                    result.push(rv);
                }
                Val::Tuple(result)
            }
            &Val::Id(ref name) => {
                match idvals.get(&*name) {
                    Some(newx) => newx.clone(),
                    None => node.clone()
                }
            }
            &Val::Loc(ref v, ref loc) => {
                let v2 = Val::replace_ids(v, idvals);
                Val::loc(v2, *loc)
            }
            &Val::Sxpr(stype, ref sdata, ref loc) => {
                sxpr::new(
                    stype,
                    Val::replace_ids(&**sdata, idvals),
                    *loc,
                )
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
            &Val::Tuple(ref items) => {
                Val::Tuple(items.iter().map(|i| i.deep_clone()).collect())
            }
            &Val::Sxpr(st, ref sx, ref loc) => {
                Val::Sxpr(st, Rc::new(sx.deep_clone()), *loc)
            }
            &Val::Struct(ref typ, ref flds) => {
                Val::Struct(typ.deep_clone(), flds.iter().map(|f| {
                    f.deep_clone()
                }).collect())
            }
            &Val::Enum(ref typ, idx, ref flds) => {
                Val::Enum(typ.deep_clone(), idx, Box::new(flds.deep_clone()))
            }
            // &Val::Struct(Type, Vec<Val>),
            // &Val::Enum(Type, u8, Box<Val>),
            // &Val::Failure(ref tag, ref msg, ref ft),
            &Val::Id(ref s) => Val::id((**s).clone()),
            // &Val::TypedId(Rc<String>, Type),
            &Val::Type(ref t) => Val::Type(t.deep_clone()),
            &Val::Kind(k) => Val::Kind(k),
            // &Val::DotAccess(Box<Val>, Rc<String>),
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
                    write!(f, "{:?},", head);
                } else {
                    write!(f, "{},", head);
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
            &Val::Loc(ref v, ref loc) => {
                if dbg {
                    write!(f, "{:?}@{:?}", v, loc)
                } else {
                    write!(f, "{}", v)
                }
            }
            _ => {
                panic!("Not a list: {:?}", l);
            }
        }
    }

    fn fmt_tuple(f: &mut fmt::Formatter, t: &Vec<Val>, dbg: bool) -> fmt::Result
    {
        f.write_str("(").ok();
        for x in t {
            if dbg {
                write!(f, "{:?},", x).ok();
            } else {
                write!(f, "{},", x).ok();
            }
        }
        f.write_str(")")
    }

    fn fmt_sxpr(f: &mut fmt::Formatter, st: SxprType, x: &Val, loc: &SrcLoc
        , dbg: bool
        ) -> fmt::Result
    {
        match (st, x) {
            (SxprType::Let, b) => {
                let (id, exprtail) = list::take_ref(b);
                let (expr, _) = list::take_ref(&*exprtail);
                if dbg {
                    write!(f, "let {:?} @ {:?} := {:?}", id, expr, loc)
                } else {
                    write!(f, "let {} := {}", id, expr)
                }
            }
            (SxprType::Fork, b) => {
                let (id, exprtail) = list::take_ref(b);
                let (expr, _) = list::take_ref(&*exprtail);
                if dbg {
                    write!(f, "fork {:?} @ {:?} := {:?}", id, loc, expr)
                } else {
                    write!(f, "fork {} := {}", id, expr)
                }
            }
            (SxprType::BlockExpr, lines) => {
                if dbg {
                    write!(f, "B@{:?}{{", loc);
                    Val::fmt_list(f, lines, dbg);
                    write!(f, "}}")
                } else {
                    write!(f, "block-expr")
                }
            }
            (SxprType::Call, &Val::Cons(ref id, ref args)) => {
                if dbg {
                    write!(f, "{:?}@{:?}({:?})", id, loc, args)
                } else {
                    write!(f, "{}({})", id, args)
                }
            }
            (SxprType::StrExpr, strs) => {
                if dbg {
                    write!(f, "\"{}\"@{:?}", strs, loc)
                } else {
                    write!(f, "\"{}\"", strs)
                }
            }
            (SxprType::MatchExpr, mc) => {
                let (x, m2) = list::take_ref(mc);
                let (cases, _) = list::take_ref(&*m2);
                if dbg {
                    write!(f, "match({:?}@{:?},{:?})", x, loc, cases)
                } else {
                    write!(f, "match({},{})", x, cases)
                }
            }
            (SxprType::MatchFailed, ref sx) => {
                let (name, m2) = list::take_ref(sx);
                let (cases, _) = list::take_ref(&*m2);
                if dbg {
                    write!(f, "MatchFailed({}@{:?}, {:?})", name, loc, cases)
                } else {
                    write!(f, "failed {} cases({})", name, cases)
                }
            }
            (SxprType::IfExpr, casex) => {
                if dbg {
                    write!(f, "if@{:?}({:?})", loc, casex)
                } else {
                    write!(f, "if({:?})", casex)
                }
            }
            (SxprType::DefFunc, ref func) => {
                let (name, f2) = list::take_ref(func);
                let (args, f3) = list::take_ref(&*f2);
                let (rtype, f4) = list::take_ref(&*f3);
                let (body, _) = list::take_ref(&*f4);
                write!(f, "DefFunc({}({:?}):{:?} {:?})",
                    name,
                    args,
                    rtype,
                    body,
                )
            }
            (SxprType::DefMacro, ref mac) => {
                let (name, m2) = list::take_ref(mac);
                let (args, m3) = list::take_ref(&*m2);
                let (body, _) = list::take_ref(&*m3);
                write!(f, "DefMacro({},{:?},{:?})", name, args, body)
            }
            (SxprType::DefStruct, ref ds) => {
                let (name, fields) = list::take_ref(ds);
                if **fields == Val::Nil {
                    write!(f, "struct({})", name)
                } else {
                    write!(f, "struct({},{:?})", name, fields)
                }
            }
            (SxprType::Import, ref filelist) => {
                let file = list::head_ref(filelist);
                if dbg {
                    write!(f, "(import {:?} @ {:?})", file, loc)
                } else {
                    write!(f, "(import {:?})", file)
                }
            }
            (SxprType::Return, ref result_list) => {
                let result_item = list::head_ref(result_list);
                if dbg {
                    write!(f, "(Return {:?} @ {:?})", result_item, loc)
                } else {
                    write!(f, "return {}", result_item)
                }
            }
            _ => {
                write!(f, "something else: {:?}/{:?} @ {:?}", st, x, loc)
            }
        }
    }

    pub fn to_msg(&self) -> MsgVal
    {
        match self {
            &Val::Int(i) => MsgVal::Int(i),
            &Val::Bool(b) => MsgVal::Bool(b),
            &Val::Str(ref s) => MsgVal::Str((**s).clone()),
            &Val::Hashtag(ref t) => MsgVal::Hashtag((**t).clone()),
            &Val::Cons(ref head, ref tail) => {
                let msghead = Box::new(head.to_msg());
                let msgtail = Box::new(tail.to_msg());
                MsgVal::Cons(msghead, msgtail)
            }
            &Val::Tuple(ref items) => {
                MsgVal::Tuple(items.iter().map(|iv| {
                    iv.to_msg()
                }).collect())
            }
            &Val::Buffer(ref buf) => MsgVal::Buffer(buf.clone()),
            &Val::Nil => MsgVal::Nil,
            &Val::Void => MsgVal::Void,
            &Val::ResourceRef(rsrc_id) => MsgVal::ResourceRef(rsrc_id),
            &Val::Failure(ref tag, ref msg, ref stack, status) => {
                MsgVal::Failure(
                    Box::new(tag.to_msg()),
                    Box::new(msg.to_msg()),
                    stack.clone(),
                    status,
                )
            }
            _ => {
                panic!("Not yet convertable to a msg: {:?}", self);
            }
        }
    }

    pub fn from_msg(mv: MsgVal) -> Val
    {
        match mv {
            MsgVal::Int(i) => Val::Int(i),
            MsgVal::Bool(b) => Val::Bool(b),
            MsgVal::Str(s) => Val::new_str(s),
            MsgVal::Hashtag(s) => Val::hashtag(s),
            MsgVal::Cons(mhead, mtail) => {
                let head = Val::from_msg(*mhead);
                let tail = Val::from_msg(*mtail);
                Val::Cons(Box::new(head), Rc::new(tail))
            }
            MsgVal::Tuple(items) => {
                Val::Tuple(items.into_iter().map(|mv| {
                    Val::from_msg(mv)
                }).collect())
            }
            MsgVal::Buffer(buf) => Val::Buffer(buf),
            MsgVal::Failure(tag, msg, trace, status) => {
                Val::Failure(
                    Box::new(Val::from_msg(*tag)),
                    Box::new(Val::from_msg(*msg)),
                    trace,
                    status,
                )
            }
            MsgVal::Nil => Val::Nil,
            MsgVal::Void => Val::Void,
            MsgVal::ResourceRef(rsrc_id) => Val::ResourceRef(rsrc_id),
        }
    }
}

impl From<Error> for Val
{
    fn from(e: Error) -> Val
    {
        Val::Void
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
            Val::Cons(ref head, ref tail) => {
                write!(f, "[");
                Val::fmt_list(f, self, false);
                write!(f, "]")
            }
            Val::Nil => {
                write!(f, "[]")
            }
            Val::Hashtag(ref s) => {
                write!(f, "#{}", s)
            }
            Val::Tuple(ref t) => {
                Val::fmt_tuple(f, t, false)
            }
            Val::Struct(ref name, ref fields) => {
                write!(f, "{}", name)
                    .and_then(|prev_result| {
                        if !fields.is_empty() {
                            Val::fmt_tuple(f, fields, false)
                        } else {
                            Ok(prev_result)
                        }
                    })
            }
            Val::Enum(Type::ModPrefix(ref modname, _), _var_idx, ref val) => {
                write!(f, "{}::{}", modname, val)
            }
            Val::Enum(ref typename, _variant_idx, ref val) => {
                write!(f, "_no_mod_enum_::{:?}::{}", typename, val)
            }
            Val::Buffer(ref buf) => {
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
            Val::Failure(ref tag, ref msg, ref stack, status) => {
                write!(f, "Failure({}, {}\n{})", tag, msg, **stack)
            }
            Val::Sxpr(ref t, ref head, ref loc) => {
                Val::fmt_sxpr(f, *t, head, loc, false)
            }
            Val::ModPrefix(ref module, ref next) => {
                write!(f, "{}::{}", module, next)
            }
            Val::Id(ref name) => {
                write!(f, "{}", name)
            }
            Val::TypedId(ref name, ref typ) => {
                write!(f, "{}:{}", name, typ)
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
            Val::DotAccess(ref outer, ref inner) => {
                write!(f, "{}.{}", outer, inner)
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
            Val::Loc(ref v, _) => {
                write!(f, "{}", v)
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
            Val::Cons(ref head, ref tail) => {
                write!(f, "L[");
                Val::fmt_list(f, self, true);
                write!(f, "]")
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
            Val::Tuple(ref t) => {
                write!(f, "T").ok();
                Val::fmt_tuple(f, t, true)
            }
            Val::Struct(ref name, ref fields) => {
                write!(f, "struct {}", name).ok();
                Val::fmt_tuple(f, fields, false)
            }
            Val::Enum(ref name, variant, ref val) => {
                write!(f, "enum({:?}.{}:{:?})", name, variant, val)
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
            Val::Sxpr(ref t, ref head, ref loc) => {
                Val::fmt_sxpr(f, *t, head, loc, true)
            }
            Val::ModPrefix(ref head, ref tail) => {
                write!(f, "Id({}::{})", head, tail)
            }
            Val::Id(ref id) => {
                write!(f, "Id({})", id)
            }
            Val::TypedId(ref id, ref typ) => {
                write!(f, "TypedId({}, {:?})", id, typ)
            }
            Val::Type(ref t) => {
                write!(f, "TypeVal({:?})", t)
            }
            Val::Kind(c) => {
                write!(f, "Kind{:?}", c)
            }
            Val::DotAccess(ref outer, ref inner) => {
                write!(f, "{:?}.{}", outer, inner)
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
            Val::Loc(ref v, ref loc) => {
                write!(f, "{:?}@{:?}", v, loc)
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
            (&Ireg::Reg(p), &Val::Tuple(ref tup)) => {
                if p as usize >= tup.len() {
                    panic!("{:?} too big for {:?}", i, tup);
                }
                &tup[p as usize]
            }
            (&Ireg::Sub(p, ref s), &Val::Tuple(ref tup)) => {
                if p as usize >= tup.len() {
                    panic!("{:?} too big for {:?}", i, tup);
                }
                tup[p as usize].ireg_get(&*s)
            }
            // get reg on struct
            (&Ireg::Reg(p), &Val::Struct(_, ref fields)) => {
                if p as usize >= fields.len() {
                    panic!("{:?} too big for {:?}", i, fields);
                }
                &fields[p as usize]
            }
            (&Ireg::Sub(p, ref s), &Val::Struct(_, ref fields)) => {
                if p as usize >= fields.len() {
                    panic!("{:?} too big for {:?}", i, fields);
                }
                fields[p as usize].ireg_get(&*s)
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
                panic!("Unsupported registry value {:?}{:?}",
                    self, i);
            }
        }
    }

    fn ireg_get_mut<'a, 'b>(&'a mut self, i: &'b Ireg) -> &'a mut Val
    {
        match (i, self) {
            // set reg on tuple
            (&Ireg::Reg(p), &mut Val::Tuple(ref mut tup)) => {
                if p as usize >= tup.len() {
                    panic!("{:?} too big for {:?}", i, tup);
                }
                tup.get_mut(p as usize).unwrap()
            }
            (&Ireg::Sub(p, ref s), &mut Val::Tuple(ref mut tup)) => {
                if p as usize >= tup.len() {
                    panic!("{:?} too big for {:?}", i, tup);
                }
                let ch = tup.get_mut(p as usize).unwrap();
                ch.ireg_get_mut(&*s)
            }
            _ => {
                panic!("Tuple is only mut registry value");
            }
        }
    }

    fn ireg_set(&mut self, i: &Ireg, v: Val)
    {
        match (i, self) {
            // set reg on tuple
            (&Ireg::Reg(p), &mut Val::Tuple(ref mut tup)) => {
                if p as usize >= tup.len() {
                    panic!("{:?} too big for {:?}", i, tup);
                }
                tup[p as usize] = v;
            }
            (&Ireg::Sub(p, ref s), &mut Val::Tuple(ref mut tup)) => {
                if p as usize >= tup.len() {
                    panic!("{:?} too big for {:?}", i, tup);
                }
                tup[p as usize].ireg_set(&*s, v);
            }
            // set reg on structs
            (&Ireg::Reg(p), &mut Val::Struct(ref name, ref mut fields)) => {
                if p as usize >= fields.len() {
                    panic!("{:?} too big for struct {}({:?})", i, name, fields);
                }
                fields[p as usize] = v;
            }
            (&Ireg::Sub(p, ref s), &mut Val::Struct(ref name, ref mut fld)) => {
                if p as usize >= fld.len() {
                    panic!("{:?} too big for struct {:?}", i, name);
                }
                fld[p as usize].ireg_set(&*s, v);
            }
            // set reg on lists
            (&Ireg::Reg(0), &mut Val::Cons(ref mut head, _)) => {
                *head = Box::new(v);
            }
            (&Ireg::Sub(0, ref s), &mut Val::Cons(ref mut head, _)) => {
                head.ireg_set(&*s, v);
            }
            (&Ireg::Reg(p), &mut Val::Cons(_, ref mut tail)) => {
                panic!("Cannot set a register on a list");
            }
            (&Ireg::Sub(p, ref s), &mut Val::Cons(_, ref mut tail)) => {
                panic!("Cannot set a subregister on a list");
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
            (&Val::Tuple(ref a), &Val::Tuple(ref b)) => {
                PartialOrd::partial_cmp(&*a, &*b)
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
            (&Val::Enum(ref at, ai, ref av)
                    , &Val::Enum(ref bt, bi, ref bv)) =>
            {
                match PartialOrd::partial_cmp(&*at, &*bt) {
                    Some(Ordering::Equal) => {
                        match PartialOrd::partial_cmp(&ai, &bi) {
                            Some(Ordering::Equal) => {
                                PartialOrd::partial_cmp(av, bv)
                            }
                            icmp => icmp,
                        }
                    }
                    tcmp => tcmp,
                }
            }
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
            (&Val::TypedId(ref ida, ref typa),
                    &Val::TypedId(ref idb, ref typb)) => {
                let cmp = PartialOrd::partial_cmp(&ida, &idb);
                match cmp {
                    Some(Ordering::Equal) => {
                        PartialOrd::partial_cmp(&typa, &typb)
                    }
                    _ => cmp,
                }
            }
            (&Val::ResourceRef(rra), &Val::ResourceRef(rrb)) => {
                PartialOrd::partial_cmp(&rra, &rrb)
            }
            (&Val::Sxpr(t1, ref x1, _), &Val::Sxpr(t2, ref x2, _)) => {
                let cmp = PartialOrd::partial_cmp(&t1, &t2);
                match cmp {
                    Some(Ordering::Equal) => {
                        PartialOrd::partial_cmp(
                            &*x1, &*x2
                        )
                    }
                    _ => cmp,
                }
            }
            (&Val::DotAccess(ref base1, ref sub1),
                    &Val::DotAccess(ref base2, ref sub2)) =>
            {
                let cmp = PartialOrd::partial_cmp(&base1, &base2);
                match cmp {
                    Some(Ordering::Equal) => {
                        PartialOrd::partial_cmp(&sub1, &sub2)
                    }
                    _ => cmp,
                }
            }
            (&Val::Loc(ref v1, _), &Val::Loc(ref v2, _)) => {
                PartialOrd::partial_cmp(&**v1, &**v2)
            }
            (&Val::Loc(ref v1, _), _) => {
                PartialOrd::partial_cmp(&**v1, other)
            }
            (_, &Val::Loc(ref v2, _)) => {
                PartialOrd::partial_cmp(self, &**v2)
            }
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
            (&Val::TypedId(_, _), _) => {
                Some(Ordering::Less)
            }
            (_, &Val::TypedId(_, _)) => {
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
            (&Val::Struct(_, _), _) => {
                Some(Ordering::Less)
            }
            (_, &Val::Struct(_, _)) => {
                Some(Ordering::Greater)
            }
            (&Val::Enum(_, _, _), _) => {
                Some(Ordering::Less)
            }
            (_, &Val::Enum(_, _, _)) => {
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
            (&Val::Sxpr(_, _, _), _) => Some(Ordering::Less),
            (_, &Val::Sxpr(_, _, _)) => Some(Ordering::Greater),
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

    pub fn with_args(v: Val) -> Env {
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

    pub fn takeResult(&mut self) -> Val {
        match self.result.take() {
            None => {
                Val::Void
            }
            Some(v) => {
                v
            }
        }
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
    use leema::val::{Type, Val, SxprType, SrcLoc};
    use leema::list;
    use leema::reg::{Reg};
    use leema::sxpr;

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
fn test_type_id_to_struct()
{
    let typename = Rc::new("Taco".to_string());
    assert_eq!(
        Type::Struct(typename.clone()),
        Type::Id(typename.clone()).to_struct()
    );
}

#[test]
fn test_type_module_id_to_struct()
{
    let module = Rc::new("Foo".to_string());
    let typname = Rc::new("Taco".to_string());
    let input =
        Type::ModPrefix(module.clone(), Rc::new(Type::Id(typname.clone())));
    assert_eq!(
        Type::ModPrefix(module.clone(), Rc::new(Type::Struct(typname.clone()))),
        input.to_struct()
    );
}

#[test]
fn test_tuple_from_list() {
    let origl = list::cons(Val::Int(4), list::singleton(Val::Int(7)));
    let tuple = Val::tuple_from_list(&origl);
    print!("wtf?({:?})", tuple);
    let exp = Val::Tuple(vec![Val::Int(4), Val::Int(7)]);
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
    let a = Val::Tuple(vec![Val::Int(3), Val::Int(7)]);
    let b = Val::Tuple(vec![Val::Int(3), Val::Int(7)]);
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
    let a =
        Val::Struct(Type::Id(Rc::new("Taco".to_string())), vec![
            Val::Int(3),
            Val::Bool(false),
        ]);
    let b =
        Val::Struct(Type::Id(Rc::new("Taco".to_string())), vec![
            Val::Int(3),
            Val::Bool(false),
        ]);
    assert_eq!(a, b);
}

#[test]
fn test_struct_lt_type() {
    let a =
        Val::Struct(Type::Id(Rc::new("Burrito".to_string())), vec![
            Val::Int(3),
            Val::Bool(false),
        ]);
    let b =
        Val::Struct(Type::Id(Rc::new("Taco".to_string())), vec![
            Val::Int(3),
            Val::Bool(false),
        ]);
    assert!(a < b);
}

#[test]
fn test_struct_lt_val() {
    let a =
        Val::Struct(Type::Id(Rc::new("Taco".to_string())), vec![
            Val::Bool(false),
            Val::Int(3),
        ]);
    let b =
        Val::Struct(Type::Id(Rc::new("Taco".to_string())), vec![
            Val::Bool(false),
            Val::Int(7),
        ]);
    assert!(a < b);
}

#[test]
fn test_enum_eq() {
    let etype =
        Type::ModPrefix(
            Rc::new("animals".to_string()),
            Rc::new(Type::Struct(Rc::new("Animal".to_string()))),
        );
    let vartype =
        Type::Struct(Rc::new("Dog".to_string()));

    let a =
        Val::Enum(etype.clone(), 0, Box::new(
            Val::Struct(vartype.clone(), Vec::with_capacity(0))
        ));
    let b =
        Val::Enum(etype.clone(), 0, Box::new(
            Val::Struct(vartype.clone(), Vec::with_capacity(0))
        ));
    assert_eq!(a, b);
}

#[test]
fn test_enum_lt_type() {
    let a =
        Val::Enum(
            Type::Id(Rc::new("Burrito".to_string())), 0, Box::new(Val::Void)
        );
    let b =
        Val::Enum(
            Type::Id(Rc::new("Taco".to_string())), 0, Box::new(Val::Void)
        );
    assert!(a < b);
}

#[test]
fn test_enum_lt_variant() {
    let a =
        Val::Enum(
            Type::Id(Rc::new("Taco".to_string())), 1, Box::new(Val::Void)
        );
    let b =
        Val::Enum(
            Type::Id(Rc::new("Taco".to_string())), 2, Box::new(Val::Void)
        );
    assert!(a < b);
}

#[test]
fn test_enum_lt_val() {
    let a =
        Val::Enum(
            Type::Id(Rc::new("Taco".to_string())), 0, Box::new(Val::Int(5))
        );
    let b =
        Val::Enum(
            Type::Id(Rc::new("Taco".to_string())), 0, Box::new(Val::Int(9))
        );
    assert!(a < b);
}

#[test]
fn test_compare_across_types() {
    let f = Val::Bool(false);
    let t = Val::Bool(true);
    let i = Val::Int(7);
    let s = Val::new_str("hello".to_string());
    let strct = Val::Struct(
        Type::Struct(Rc::new("Foo".to_string())),
        vec![Val::Int(2), Val::Bool(true)],
    );
    let enm = Val::Enum(
        Type::Enum(Rc::new("Taco".to_string())),
        1,
        Box::new(Val::Struct(
            Type::Struct(Rc::new("Burrito".to_string())),
            vec![Val::Int(8), Val::Int(6)],
        ))
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
fn test_replace_ids_if()
{
    let loc = SrcLoc::new(3, 4);
    let body = sxpr::new(
        SxprType::IfExpr,
        list::cons(Val::id("a".to_string()),
            list::cons(Val::id("b".to_string()),
            list::cons(Val::Bool(false),
            Val::Nil,
            ))),
        loc,
    );
    let mut ids = HashMap::new();
    ids.insert(Rc::new("a".to_string()), Val::Bool(true));
    ids.insert(Rc::new("b".to_string()), Val::Bool(false));

    let result = Val::replace_ids(&body, &ids);

    let expected = sxpr::new(
        SxprType::IfExpr,
        list::cons(Val::Bool(true),
            list::cons(Val::Bool(false),
            list::cons(Val::Bool(false),
            Val::Nil,
            ))),
        loc,
    );
    assert_eq!(expected, result);
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

}
