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
use std::io::{stderr, Write};
use std::marker::{Send};
use std::ops::Deref;

use ::futures::sync::mpsc::{Receiver};
use ::tokio_core::reactor::{Handle, Remote};

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

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
#[derive(Eq)]
#[derive(Hash)]
pub enum FuncCallType
{
    FrameCall,
    IoCall,
    // RsrcCall,
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
    Struct(Rc<String>, i8),
    Failure,
    Enum(String),
    Func(FuncCallType, Vec<Type>, Box<Type>),
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
    Texpr(Rc<String>, Vec<Type>),
    Var(Rc<String>),
    AnonVar,
}

impl Type
{
    pub fn f(calltype: FuncCallType, inputs: Vec<Type>, result: Type) -> Type
    {
        Type::Func(calltype, inputs, Box::new(result))
    }

    pub fn split_func(t: &Type) -> (&FuncCallType, &Vec<Type>, &Type)
    {
        match t {
            &Type::Func(ref calltype, ref args, ref result) => {
                (calltype, args, &*result)
            }
            _ => {
                panic!("Not a func type {:?}", t);
            }
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
            &Type::Int => Type::Int,
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
            &Type::Struct(ref name, nfields) => write!(f, "{}", name),
            &Type::Enum(ref name) => write!(f, "Enum"),
            &Type::Failure => write!(f, "Failure"),
            &Type::Func(ref calltype, ref args, ref result) => {
                write!(f, "{:?}:", calltype).ok();
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
            &Type::Resource(ref name) => write!(f, "{}", &name),
            &Type::RustBlock => write!(f, "RustBlock"),
            &Type::Void => write!(f, "Void"),
            &Type::Kind => write!(f, "Kind"),
            &Type::Any => write!(f, "Any"),

            &Type::Unknown => write!(f, "TypeUnknown"),
            &Type::Id(ref name) => write!(f, "TypeId({})", name),
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
            &Type::Struct(ref name, nfields) => write!(f, "{}", name),
            &Type::Enum(ref name) => write!(f, "Enum"),
            &Type::Failure => write!(f, "Failure"),
            &Type::Func(ref calltype, ref args, ref result) => {
                write!(f, "{:?}:", calltype).ok();
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
            &Type::Texpr(ref base, ref args) => write!(f, "Texpr"),
            &Type::Var(ref name) => {
                write!(f, "Type::Var({})", name)
            }
            &Type::AnonVar => write!(f, "TypeAnonymous"),
        }
    }
}

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
    Fail,
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
#[derive(PartialEq)]
pub enum MsgVal
{
    Int(i64),
    Str(String),
    Bool(bool),
    Hashtag(String),
    Cons(Box<MsgVal>, Box<MsgVal>),
    Tuple(Vec<MsgVal>),
    Nil,
    Void,
    ResourceRef(i64),
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
    Cons(Box<Val>, Box<Val>),
    Nil,
    Tuple(Vec<Val>),
    Sxpr(SxprType, Box<Val>),
    Struct(Type, Vec<Val>),
    Enum(Type, u8, Box<Val>),
    Failure(
        Box<Val>, // tag
        Box<Val>, // msg
        Arc<FrameTrace>,
    ),
    Id(Rc<String>),
    ModPrefix(Rc<String>, Rc<Val>),
    TypedId(Rc<String>, Type),
    Type(Type),
    Kind(u8),
    DotAccess(Box<Val>, Rc<String>),
    Lib(Arc<LibVal>),
    LibRc(Rc<LibVal>),
    ResourceRef(i64),
    RustBlock,
    Future(FutureVal),
    CallParams,
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
    pub fn is_sxpr(&self) -> bool
    {
        match self {
            &Val::Sxpr(_, _) => true,
            _ => false,
        }
    }

    pub fn is_sxpr_type(&self, st: SxprType) -> bool
    {
        match self {
            &Val::Sxpr(st, _) => true,
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
            _ => false,
        }
    }

    pub fn id_name(&self) -> Rc<String>
    {
        match self {
            &Val::Id(ref name) => name.clone(),
            &Val::TypedId(ref name, ref typ) => name.clone(),
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

    pub fn tuple_from_list(l: Val) -> Val
    {
        // TODO switch to be list::to_vec(), use regular Tuple constructor
        if !l.is_list() {
            panic!("Cannot make tuple from not-list: {:?}", l);
        }
        let mut empties: Vec<Val> = vec![];
        let items = list::fold(empties, l, |mut res, item| {
            res.push(item);
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
            &Val::Failure(_, _, _) => true,
            _ => false,
        }
    }

    pub fn failure(tag: Val, msg: Val, trace: Arc<FrameTrace>) -> Val
    {
        Val::Failure(
            Box::new(tag),
            Box::new(msg),
            trace,
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
            &Val::Failure(_, _, _) => Type::Failure,
            &Val::Type(_) => Type::Kind,
            &Val::Void => Type::Void,
            &Val::Wildcard => Type::Unknown,
            &Val::CallParams => Type::Unknown,
            &Val::PatternVar(_) => Type::Unknown,
            &Val::Id(_) => Type::AnonVar,
            &Val::TypedId(_, ref typ) => typ.clone(),
            &Val::Sxpr(SxprType::DefFunc, _) => sxpr::defunc_type(self),
            &Val::RustBlock => Type::RustBlock,
            _ => { panic!("dunno what type {:?}", self) }
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

    pub fn replace_ids(node: Val,
        idvals: &HashMap<Rc<String>, Val>) -> Val
    {
        match node {
            Val::Cons(_, _) => {
                let f = |v: Val| -> Val {
                    Val::replace_ids(v, idvals)
                };
                list::map(node, f)
            }
            Val::Tuple(t) => {
                let mut result = vec![];
                for tv in t {
                    let rv = Val::replace_ids(tv, idvals);
                    result.push(rv);
                }
                Val::Tuple(result)
            }
            Val::Id(name) => {
                match idvals.get(&*name) {
                    Some(newx) => newx.clone(),
                    None => Val::Id(name),
                }
            }
            Val::Sxpr(stype, sdata) => {
                sxpr::new(
                    stype,
                    Val::replace_ids(*sdata, idvals),
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
                    Box::new(tail.deep_clone()),
                )
            }
            &Val::Nil => Val::Nil,
            &Val::Tuple(ref items) => {
                Val::Tuple(items.iter().map(|i| i.deep_clone()).collect())
            }
            &Val::Sxpr(st, ref sx) => {
                Val::Sxpr(st, Box::new(sx.deep_clone()))
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
            // &Val::CallParams,
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

    fn fmt_sxpr(st: SxprType, x: &Val, f: &mut fmt::Formatter, dbg: bool) -> fmt::Result
    {
        match (st, x) {
            (SxprType::Let, b) => {
                let (id, exprtail) = list::take_ref(b);
                let (expr, _) = list::take_ref(exprtail);
                if dbg {
                    write!(f, "let {:?} := {:?}", id, expr)
                } else {
                    write!(f, "let {} := {}", id, expr)
                }
            }
            (SxprType::Fork, b) => {
                let (id, exprtail) = list::take_ref(b);
                let (expr, _) = list::take_ref(exprtail);
                if dbg {
                    write!(f, "fork {:?} := {:?}", id, expr)
                } else {
                    write!(f, "fork {} := {}", id, expr)
                }
            }
            (SxprType::BlockExpr, lines) => {
                if dbg {
                    write!(f, "B{{");
                    Val::fmt_list(f, lines, dbg);
                    write!(f, "}}")
                } else {
                    write!(f, "block-expr")
                }
            }
            (SxprType::Call, &Val::Cons(ref id, ref args)) => {
                if dbg {
                    write!(f, "{:?}({:?})", id, args)
                } else {
                    write!(f, "{}({})", id, args)
                }
            }
            (SxprType::StrExpr, strs) => {
                write!(f, "\"{}\"", strs)
            }
            (SxprType::MatchExpr, mc) => {
                let (x, m2) = list::take_ref(mc);
                let (cases, _) = list::take_ref(m2);
                if dbg {
                    write!(f, "match({:?},{:?})", x, cases)
                } else {
                    write!(f, "match({},{})", x, cases)
                }
            }
            (SxprType::IfExpr, casex) => {
                write!(f, "if({:?})", casex)
            }
            (SxprType::DefFunc, ref func) => {
                let (name, f2) = list::take_ref(func);
                let (args, f3) = list::take_ref(f2);
                let (rtype, f4) = list::take_ref(f3);
                let (body, _) = list::take_ref(f4);
                write!(f, "DefFunc({}({:?}):{:?} {:?})",
                    name,
                    args,
                    rtype,
                    body,
                )
            }
            (SxprType::DefMacro, ref mac) => {
                let (name, m2) = list::take_ref(mac);
                let (args, m3) = list::take_ref(m2);
                let (body, _) = list::take_ref(m3);
                write!(f, "DefMacro({},{:?},{:?})", name, args, body)
            }
            (SxprType::DefStruct, ref ds) => {
                let (name, m2) = list::take_ref(ds);
                let (fields, _) = list::take_ref(m2);
                write!(f, "struct({},{:?})", name, fields)
            }
            (SxprType::Import, ref filelist) => {
                let file = list::head_ref(filelist);
                write!(f, "(import {:?})", file)
            }
            _ => {
                write!(f, "something else: {:?}/{:?}", st, x)
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
            &Val::Nil => MsgVal::Nil,
            &Val::Void => MsgVal::Void,
            &Val::ResourceRef(rsrc_id) => MsgVal::ResourceRef(rsrc_id),
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
                Val::Cons(Box::new(head), Box::new(tail))
            }
            MsgVal::Tuple(items) => {
                Val::Tuple(items.into_iter().map(|mv| {
                    Val::from_msg(mv)
                }).collect())
            }
            MsgVal::Nil => Val::Nil,
            MsgVal::Void => Val::Void,
            MsgVal::ResourceRef(rsrc_id) => Val::ResourceRef(rsrc_id),
        }
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
                write!(f, "{}", name).ok();
                Val::fmt_tuple(f, fields, false)
            }
            Val::Enum(ref name, variant, ref val) => {
                write!(f, "Enum-{}.{}:{}", name, variant, val)
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
            Val::Failure(ref tag, ref msg, ref stack) => {
                write!(f, "Failure({}, {}\n{:?}\n)", tag, msg, **stack)
            }
            Val::Sxpr(ref t, ref head) => {
                Val::fmt_sxpr(*t, head, f, false)
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
            Val::Future(_) => {
                write!(f, "Future")
            }
            Val::DotAccess(ref outer, ref inner) => {
                write!(f, "{}.{}", outer, inner)
            }
            Val::Void => {
                write!(f, "Void")
            }
            Val::CallParams => {
                write!(f, "CallParams")
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
                write!(f, "Enum-{}.{}:{:?}", name, variant, val)
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
            Val::Failure(ref tag, ref msg, ref stack) => {
                write!(f, "Failure({}, {}, {:?})", tag, msg, stack)
            }
            Val::Sxpr(ref t, ref head) => {
                Val::fmt_sxpr(*t, head, f, true)
            }
            Val::ModPrefix(ref head, ref tail) => {
                write!(f, "{}::{:?}", head, tail)
            }
            Val::Id(ref id) => {
                write!(f, "ID({})", id)
            }
            Val::TypedId(ref id, ref typ) => {
                write!(f, "TypedId({}, {:?})", id, typ)
            }
            Val::Type(ref t) => {
                write!(f, "Type:{:?}", t)
            }
            Val::Kind(c) => {
                write!(f, "Kind{:?}", c)
            }
            Val::DotAccess(ref outer, ref inner) => {
                write!(f, "{:?}.{}", outer, inner)
            }
            Val::Future(_) => {
                write!(f, "Future")
            }
            Val::PatternVar(ref r) => {
                write!(f, "pvar:{:?}", r)
            }
            Val::CallParams => {
                write!(f, "CallParams")
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
            (&Ireg::Reg(0), &Val::Failure(ref tag, _, _)) => tag,
            (&Ireg::Reg(1), &Val::Failure(_, ref msg, _)) => msg,
            (&Ireg::Reg(2), &Val::Failure(_, _, ref trace)) => {
                panic!("Cannot access frame trace until it is implemented as a leema value {}", trace);
            }
            (&Ireg::Sub(_, _), &Val::Failure(ref tag, ref msg, _)) => {
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
                tail.ireg_set(&Ireg::Reg(p-1), v);
            }
            (&Ireg::Sub(p, ref s), &mut Val::Cons(_, ref mut tail)) => {
                tail.ireg_set(&Ireg::Sub(p-1, s.clone()), v);
            }
            (_, &mut Val::Nil) => {
                panic!("cannot set reg on empty list: {:?}", i);
            }
            // set reg on Failures
            (&Ireg::Reg(0), &mut Val::Failure(ref mut tag, _, _)) => {
                *tag = Box::new(v);
            }
            (&Ireg::Reg(1), &mut Val::Failure(_, ref mut msg, _)) => {
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
            (&Val::CallParams, &Val::CallParams) => {
                Some(Ordering::Equal)
            }
            (&Val::Tuple(ref a), &Val::Tuple(ref b)) => {
                PartialOrd::partial_cmp(&*a, &*b)
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
            (&Val::Sxpr(t1, ref x1), &Val::Sxpr(t2, ref x2)) => {
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
            (_, &Val::Nil) => {
                Some(Ordering::Greater)
            }
            (&Val::Void, _) => {
                Some(Ordering::Less)
            }
            (_, &Val::Void) => {
                Some(Ordering::Greater)
            }
            (&Val::CallParams, _) => {
                Some(Ordering::Less)
            }
            (_, &Val::CallParams) => {
                Some(Ordering::Greater)
            }
            (&Val::RustBlock, _) => Some(Ordering::Less),
            (_, &Val::RustBlock) => Some(Ordering::Greater),
            (&Val::Sxpr(_, _), _) => Some(Ordering::Less),
            (_, &Val::Sxpr(_, _)) => Some(Ordering::Greater),
            (&Val::Wildcard, _) => Some(Ordering::Less),
            (_, &Val::Wildcard) => Some(Ordering::Greater),
            //(&Val::Sxpr(_, ref x1), &Val::Sxpr(t2, ref x2)) => {
            _ => {
                panic!("can't compare({:?},{:?})", self, other);
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
            &Reg::Params => {
                &mut self.params
            }
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
            &Reg::Params => {
                &self.params
            }
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
                    vout!("overwrite {:?} with {:?}\n", self.reg.get(&p), v);
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
    use leema::val::{Type, Val, SxprType};
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
fn test_tuple_from_list() {
    let origl = list::cons(Val::Int(4), list::singleton(Val::Int(7)));
    let tuple = Val::tuple_from_list(origl);
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
fn test_compare_across_types() {
    let f = Val::Bool(false);
    let t = Val::Bool(true);
    let i = Val::Int(7);
    let s = Val::new_str("hello".to_string());

    assert!(f < t);
    assert!(t < i);
    assert!(i < s);
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
    let body = sxpr::new(SxprType::IfExpr,
        list::cons(Val::id("a".to_string()),
        list::cons(Val::id("b".to_string()),
        list::cons(Val::Bool(false),
        Val::Nil,
        ))),
    );
    let mut ids = HashMap::new();
    ids.insert(Rc::new("a".to_string()), Val::Bool(true));
    ids.insert(Rc::new("b".to_string()), Val::Bool(false));

    let result = Val::replace_ids(body, &ids);

    let expected = sxpr::new(SxprType::IfExpr,
        list::cons(Val::Bool(true),
        list::cons(Val::Bool(false),
        list::cons(Val::Bool(false),
        Val::Nil,
        ))),
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
