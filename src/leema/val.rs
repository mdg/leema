use leema::reg::{self, Reg, Ireg, Iregistry};
use leema::sexpr;
use leema::list;
use leema::frame::{FrameTrace};
use leema::log;
use std::fmt::{self};
use std::collections::{BTreeMap, HashMap};
use std::sync::{Arc, Mutex};
use std::sync::atomic::{self, AtomicBool};
use std::sync::mpsc;
use std::any::{Any};
use std::cmp::{PartialEq, PartialOrd, Ordering};
use std::clone::Clone;
use std::fmt::{Debug};
use std::io::{stderr, Write};


#[derive(Clone)]
#[derive(Debug)]
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
    Struct(Arc<String>, i8),
    Failure,
    Enum(String),
    Func(Vec<Type>, Box<Type>),
    // different from base collection/map interfaces?
    // base interface/type should probably be iterator
    // and then it should be a protocol, not type
    StrictList(Box<Type>),
    RelaxedList,
    Lib(String),
    // Future(Box<Type>),
    Void,
    /*
    Map(Box<Type>, Box<Type>),
    */
    Kind,
    Any,

    Unknown,
    Id(Arc<String>),
    Texpr(Arc<String>, Vec<Type>),
    Var(Arc<String>),
    AnonVar,
}

impl Type
{
    pub fn f(inputs: Vec<Type>, result: Type) -> Type
    {
        Type::Func(inputs, Box::new(result))
    }

    pub fn split_func(t: &Type) -> (&Vec<Type>, &Type)
    {
        match t {
            &Type::Func(ref args, ref result) => (args, &*result),
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

    pub fn var_name(&self) -> Arc<String>
    {
        match self {
            &Type::Var(ref id) => id.clone(),
            &Type::AnonVar => Arc::new("anon".to_string()),
            _ => {
                panic!("Not a Type::Var {:?}", self);
            }
        }
    }

    pub fn tuple_items(tup: Type) -> Vec<Type>
    {
        match tup {
            Type::Tuple(items) => items,
            _ => {
                panic!("No items in not tuple type {:?}", tup);
            }
        }
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
            &Type::Tuple(ref items) => write!(f, "Ttuple()"),
            &Type::Struct(ref name, nfields) => write!(f, "{}", name),
            &Type::Enum(ref name) => write!(f, "Enum"),
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
            &Type::RelaxedList => write!(f, "List"),
            &Type::Lib(ref name) => write!(f, "Lib({})", name),
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


/*
pub trait LibTrait
    : Any
    + Send
    + Sync
    + Debug
{}
*/

//pub struct LibVal(pub Arc<Any + Send + Sync + 'static>);
pub struct LibVal{
    pub v: Arc<Any + Send + Sync>,
    pub t: Type,
}

impl Clone for LibVal
{
    fn clone(&self) -> LibVal
    {
        LibVal{v: self.v.clone(), t: self.t.clone()}
    }
}

impl Debug for LibVal
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "LibVal<{:?}>", self.t)
    }
}

#[derive(Clone)]
pub struct FutureVal(pub Arc<AtomicBool>, pub Arc<Mutex<mpsc::Receiver<Val>>>);

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
pub enum SexprType {
    Let,
    Fork,
    IdWithType,
    TypeExpr,
    StrExpr,
    BlockExpr,
    Call,
    DefFunc,
    DefMacro,
    DefStruct,
    Fail,
    CaseExpr,
    IfStmt,
    MatchExpr,
    MatchFailed,
    Return,
    Comparison,
    FieldAccess,
    /*
    LessThan3(bool, bool),
    BooleanAnd,
    BooleanOr,
    */
}

#[derive(Clone)]
pub enum Val {
    Int(i64),
    Str(Arc<String>),
    Bool(bool),
    Hashtag(Arc<String>),
    Cons(Box<Val>, Box<Val>),
    Nil,
    Tuple(Vec<Val>),
    Sexpr(SexprType, Box<Val>),
    Struct(Type, Vec<Val>),
    Enum(Type, u8, Box<Val>),
    Failure(
        Box<Val>, // tag
        Box<Val>, // msg
        Arc<FrameTrace>,
    ),
    Id(Arc<String>),
    Type(Type),
    Kind(u8),
    Lib(LibVal),
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

impl Val {
    pub fn is_sexpr(&self) -> bool
    {
        match self {
            &Val::Sexpr(_, _) => true,
            _ => false,
        }
    }

    pub fn is_sexpr_type(&self, st: SexprType) -> bool
    {
        match self {
            &Val::Sexpr(st, _) => true,
            _ => false,
        }
    }

    pub fn id(s: String) -> Val
    {
        Val::Id(Arc::new(s))
    }

    pub fn is_id(&self) -> bool
    {
        match self {
            &Val::Id(_) => true,
            &Val::Sexpr(SexprType::IdWithType, _) => true,
            _ => false,
        }
    }

    pub fn id_name(&self) -> Arc<String>
    {
        match self {
            &Val::Id(ref name) => name.clone(),
            &Val::Sexpr(SexprType::IdWithType, ref idhead) => {
                list::head_ref(idhead).to_str()
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
        let items = list::fold(l, empties, |mut res, item| {
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

    /*
    pub fn take_tuple_vals(&mut self) -> Vec<Val>
    {
        match self {
            &mut Sexpr::Val(ref mut tv) => {
                let mut tmp = Box::new(Val::Void);
                mem::swap(&mut *tv, &mut tmp);
                match *tmp {
                    Val::Tuple(vals) => {
                        vals
                    }
                    _ => panic!("sexpr val is not a tuple"),
                }
            }
            _ => panic!("tuple sexpr not a val"),
        }
    }
    */

    pub fn new_str(s: String) -> Val
    {
        Val::Str(Arc::new(s))
    }

    pub fn empty_str() -> Val
    {
        Val::Str(Arc::new("".to_string()))
    }

    pub fn to_str(&self) -> Arc<String>
    {
        match self {
            &Val::Id(ref id) => id.clone(),
            &Val::Str(ref s) => s.clone(),
            &Val::Sexpr(SexprType::IdWithType, ref id) => {
                id.to_str()
            }
            _ => {
                panic!("Cannot convert to string: {:?}", self);
            }
        }
    }

    pub fn hashtag(s: String) -> Val
    {
        Val::Hashtag(Arc::new(s))
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

    pub fn future(ready: Arc<AtomicBool>, r: mpsc::Receiver<Val>) -> Val
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

    pub fn new_lib<T: Any + Send + Sync>(lv: T, typ: Type) -> Val
    {
        Val::Lib(LibVal{v: Arc::new(lv), t: typ})
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
            &Val::Cons(_, _) => Type::StrictList(Box::new(Type::Unknown)),
            &Val::Nil => Type::StrictList(Box::new(Type::Unknown)),
            &Val::Failure(_, _, _) => Type::Failure,
            &Val::Type(_) => Type::Kind,
            &Val::Void => Type::Void,
            &Val::Wildcard => Type::Unknown,
            &Val::CallParams => Type::Unknown,
            &Val::PatternVar(_) => Type::Unknown,
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
        match (patt, input) {
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
            (&Val::Cons(ref ph, ref pt), &Val::Cons(ref ih, ref it)) => {
                Val::_pattern_match(assigns, ph, ih)
                    && Val::_pattern_match(assigns, pt, it)
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
        }
    }

    pub fn replace_ids(node: Val,
        idvals: &HashMap<Arc<String>, Val>) -> Val
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
            Val::Sexpr(stype, sdata) => {
                sexpr::new(
                    stype,
                    Val::replace_ids(*sdata, idvals),
                )
            }
            _ => node.clone(),
        }
    }

    fn fmt_list(f: &mut fmt::Formatter, l: &Val, dbg: bool) -> fmt::Result
    {
        match l {
            &Val::Cons(ref head, ref tail) => {
                write!(f, "{:?},", head);
                Val::fmt_list(f, tail, dbg)
            }
            &Val::Nil => {
                // do nothing, we've formatted enough
                write!(f, "")
            }
            &Val::Id(ref name) => {
                if dbg {
                    write!(f, "{:?},", l)
                } else {
                    write!(f, "{},", name)
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

    fn fmt_sexpr(st: SexprType, x: &Val, f: &mut fmt::Formatter, dbg: bool) -> fmt::Result
    {
        match (st, x) {
            (SexprType::Let, b) => {
                let (id, exprtail) = list::take_ref(b);
                let (expr, _) = list::take_ref(exprtail);
                if dbg {
                    write!(f, "let {:?} := {:?}", id, expr)
                } else {
                    write!(f, "let {} := {}", id, expr)
                }
            }
            (SexprType::Fork, b) => {
                let (id, exprtail) = list::take_ref(b);
                let (expr, _) = list::take_ref(exprtail);
                if dbg {
                    write!(f, "fork {:?} := {:?}", id, expr)
                } else {
                    write!(f, "fork {} := {}", id, expr)
                }
            }
            (SexprType::BlockExpr, lines) => {
                if dbg {
                    write!(f, "{{");
                    Val::fmt_list(f, lines, dbg);
                    write!(f, "}}")
                } else {
                    write!(f, "block-expr")
                }
            }
            (SexprType::Call, &Val::Cons(ref id, ref args)) => {
                let (argst, _) = list::take_ref(args);
                if dbg {
                    write!(f, "{:?}({:?})", id, argst)
                } else {
                    write!(f, "{}({})", id, argst)
                }
            }
            (SexprType::StrExpr, strs) => {
                write!(f, "\"{}\"", strs)
            }
            (SexprType::MatchExpr, mc) => {
                let (x, m2) = list::take_ref(mc);
                let (cases, _) = list::take_ref(m2);
                if dbg {
                    write!(f, "match({:?},{:?})", x, cases)
                } else {
                    write!(f, "match({},{})", x, cases)
                }
            }
            (SexprType::CaseExpr, casex) => {
                write!(f, "case({:?})", casex)
            }
            (SexprType::IfStmt, ifs) => {
                write!(f, "if({:?})", ifs)
            }
            (SexprType::IdWithType, &Val::Cons(ref id, ref typ)) => {
                let t = list::head_ref(typ);
                write!(f, "{:?}:{:?}", id, t)
            }
            (SexprType::DefMacro, ref mac) => {
                let (name, m2) = list::take_ref(mac);
                let (args, m3) = list::take_ref(m2);
                let (body, _) = list::take_ref(m3);
                write!(f, "DefMacro({},{:?},{:?})", name, args, body)
            }
            (SexprType::DefStruct, ref ds) => {
                let (name, m2) = list::take_ref(ds);
                let (fields, _) = list::take_ref(m2);
                write!(f, "struct({},{:?})", name, fields)
            }
            (SexprType::FieldAccess, ref fa) => {
                let (base, fa2) = list::take_ref(fa);
                let (field, _) = list::take_ref(fa2);
                write!(f, "({:?}.{})", base, field)
            }
            _ => {
                write!(f, "something else: {:?}/{:?}", st, x)
            }
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
                Val::fmt_list(f, self, true);
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
            Val::Lib(ref lv) => {
                write!(f, "LibVal({:?})", lv.t)
            }
            Val::Failure(ref tag, ref msg, ref stack) => {
                write!(f, "Failure({}, {}\n{:?}\n)", tag, msg, **stack)
            }
            Val::Sexpr(ref t, ref head) => {
                Val::fmt_sexpr(*t, head, f, false)
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
            Val::Future(_) => {
                write!(f, "Future")
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
                write!(f, "Str(\"{}\")", s)
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
            Val::Tuple(ref t) => {
                write!(f, "tuple/").ok();
                Val::fmt_tuple(f, t, true).ok();
                write!(f, "/t")
            }
            Val::Struct(ref name, ref fields) => {
                write!(f, "{}", name).ok();
                Val::fmt_tuple(f, fields, false)
            }
            Val::Enum(ref name, variant, ref val) => {
                write!(f, "Enum-{}.{}:{:?}", name, variant, val)
            }
            Val::Lib(ref lv) => {
                write!(f, "LibVal({:?})", lv.t)
            }
            Val::Failure(ref tag, ref msg, ref stack) => {
                write!(f, "Failure({}, {}, {:?})", tag, msg, stack)
            }
            Val::Sexpr(ref t, ref head) => {
                Val::fmt_sexpr(*t, head, f, true)
            }
            Val::Id(ref id) => {
                write!(f, "ID({})", id)
            }
            Val::Type(ref t) => {
                write!(f, "{:?}", t)
            }
            Val::Kind(c) => {
                write!(f, "Kind{:?}", c)
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
            (&Val::Void, &Val::Void) => {
                Some(Ordering::Equal)
            }
            (&Val::CallParams, &Val::CallParams) => {
                Some(Ordering::Equal)
            }
            (&Val::Tuple(ref a), &Val::Tuple(ref b)) => {
                PartialOrd::partial_cmp(&*a, &*b)
            }
            (&Val::Sexpr(t1, ref x1), &Val::Sexpr(t2, ref x2)) => {
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
            (&Val::Sexpr(_, _), _) => Some(Ordering::Less),
            (_, &Val::Sexpr(_, _)) => Some(Ordering::Greater),
            (&Val::Wildcard, _) => Some(Ordering::Less),
            (_, &Val::Wildcard) => Some(Ordering::Greater),
            //(&Val::Sexpr(_, ref x1), &Val::Sexpr(t2, ref x2)) => {
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
            &Val::Sexpr(ref s) => {
                Val::Sexpr(s.clone())
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
        panic!("should totally implement this");
    }

    pub fn set_reg(&mut self, reg: &Reg, v: Val) {
        match reg {
            &Reg::Reg(ref i) => {
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
            &Reg::Reg(ref i) => {
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
        self.get_reg(&Reg::Reg(Ireg::Reg(reg)))
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
                    vout!("register already set: {:?}", i);
                    vout!("overwrite {:?} with {:?}", self.reg.get(&p), v);
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
    use leema::val::{Type, Val, SexprType};
    use leema::list;
    use leema::sexpr;
    use std::collections::{HashMap};
    use std::sync::{Arc};


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
fn test_replace_ids_if()
{
    let body = sexpr::new(SexprType::IfStmt,
        list::cons(Val::id("a".to_string()),
        list::cons(Val::id("b".to_string()),
        list::cons(Val::Bool(false),
        Val::Nil,
        ))),
    );
    let mut ids = HashMap::new();
    ids.insert(Arc::new("a".to_string()), Val::Bool(true));
    ids.insert(Arc::new("b".to_string()), Val::Bool(false));

    let result = Val::replace_ids(body, &ids);

    let expected = sexpr::new(SexprType::IfStmt,
        list::cons(Val::Bool(true),
        list::cons(Val::Bool(false),
        list::cons(Val::Bool(false),
        Val::Nil,
        ))),
    );
    assert_eq!(expected, result);
}

}
