use leema::reg::{self, Reg, NumericRegistry};
use leema::sexpr;
use leema::list;
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
pub enum Type
{
    Int,
    Str,
    Bool,
    Hashtag,
    Tuple(Vec<Type>),
    Unit,
    Struct(String),
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

    Struct(String),
    // partial/parameterized struct
    Pstruct(String, Vec<Type>),
    Arrow(Vec<Type>),
    // partial/parameterized arrow
    Parrow(Vec<Type>, Vec<Type>),
    Failure,
    */
    Kind,
    Any,

    Unknown,
    Id(Arc<String>),
    Texpr(Arc<String>, Vec<Type>),
    Tvar(Arc<String>),
    Var(Arc<String>, Arc<String>),
    AnonVar,
}

impl Type {
    pub fn func_types(t: Type) -> (Vec<Type>, Type)
    {
        match t {
            Type::Func(args, result) => (args, *result),
            _ => {
                panic!("Not a func type {:?}", t);
            }
        }
    }

    pub fn is_var(&self) -> bool
    {
        match self {
            &Type::Var(_, _) => true,
            &Type::AnonVar => true,
            _ => false,
        }
    }

    pub fn var_name(&self) -> Arc<String>
    {
        match self {
            &Type::Var(_, ref id) => id.clone(),
            &Type::AnonVar => Arc::new("anon".to_string()),
            _ => {
                panic!("Not a Type::Var {:?}", self);
            }
        }
    }

    pub fn var(vname: Arc<String>) -> Type
    {
        let tname = Arc::new(format!("TypeVar_{}", vname));
        Type::Var(tname, vname)
    }

    pub fn tmp_type(&self) -> Arc<String>
    {
        match self {
            &Type::Var(ref tmptype, _) => tmptype.clone(),
            &Type::AnonVar => Arc::new("anonvar".to_string()),
            _ => {
                panic!("Not a Type::Var {:?}", self);
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
            &Type::Unit => write!(f, "Unit"),
            &Type::Struct(ref name) => write!(f, "Struct"),
            &Type::Enum(ref name) => write!(f, "Enum"),
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
            &Type::Tvar(ref name) => write!(f, "Tvar({})", name),
            &Type::Var(ref tname, ref vname) => {
                write!(f, "TypeVar({}, {})", tname, vname)
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
    CaseExpr,
    IfExpr,
    Comparison,
    LessThan3(bool, bool),
    BooleanAnd,
    BooleanOr,
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
    Unit,
    Failure,
    Sexpr(SexprType, Box<Val>),
    Id(Arc<String>),
    Type(Type),
    Kind(u8),
    Lib(LibVal),
    Future(FutureVal),
    Void,
}

const NIL: Val = Val::Nil;
pub const UNIT: Val = Val::Unit;
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
        if !l.is_list() {
            panic!("Cannot make tuple from not-list: {:?}", l);
        }
        let mut empties: Vec<Val> = vec![];
        let items = list::fold(l, empties, |mut res, item| {
            res.push(item);
            res
        });
        if false && items.len() == 0 {
            Val::Unit
        } else {
            Val::Tuple(items)
        }
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
            &Val::Cons(_, _) => Type::RelaxedList,
            &Val::Type(_) => Type::Kind,
            _ => { panic!("dunno what type {:?}", self) }
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
            (SexprType::IfExpr, ifs) => {
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
            Val::Unit => {
                f.write_str("()")
            }
            Val::Hashtag(ref s) => {
                write!(f, "#{}", s)
            }
            Val::Tuple(ref t) => {
                Val::fmt_tuple(f, t, false)
            }
            Val::Lib(ref lv) => {
                write!(f, "LibVal({:?})", lv.t)
            }
            Val::Failure => {
                f.write_str("Failure")
            }
            Val::Sexpr(ref t, ref head) => {
                Val::fmt_sexpr(*t, head, f, false)
            }
            Val::Id(ref name) => {
                write!(f, "ID({})", name)
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
            Val::Unit => {
                f.write_str("()")
            }
            Val::Hashtag(ref s) => {
                write!(f, "#{}", s)
            }
            Val::Tuple(ref t) => {
                write!(f, "tuple/").ok();
                Val::fmt_tuple(f, t, true).ok();
                write!(f, "/tuple")
            }
            Val::Lib(ref lv) => {
                write!(f, "LibVal({:?})", lv.t)
            }
            Val::Failure => {
                f.write_str("Failure")
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
            Val::Void => {
                write!(f, "Void")
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


impl reg::NumericRegistry for Val {
    fn get_reg_r1(&self, r: i8) -> &Val {
        match self {
            &Val::Tuple(ref tup) => {
                &tup[r as usize]
            }
            _ => {
                panic!("Tuple is only registry value");
            }
        }
    }

    fn set_reg_r1(&mut self, r: i8, v: Val)
    {
        if self.is_list() && !list::is_empty(self) {
            if r == 0 {
                match self {
                    &mut Val::Cons(ref mut head, _) => {
                        *head = Box::new(v);
                    }
                    _ => {
                        panic!("how did this happen?");
                    }
                }
            } else {
                self.set_reg_r1(r-1, v);
            }
        } else if self.is_tuple() {
            match self {
                &mut Val::Tuple(ref mut tup) => {
                    if r as usize >= tup.len() {
                        println!("Reg({}) too big for {:?}"
                            , r, tup);
                    }
                    tup[r as usize] = v;
                }
                _ => {
                    panic!("weird");
                }
            }
        } else {
            panic!("Can't set_reg_r1 on NotTuple");
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
            (&Val::Id(ref a), &Val::Id(ref b)) => {
                PartialOrd::partial_cmp(a, b)
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
            (&Val::Unit, &Val::Unit) => {
                Some(Ordering::Equal)
            }
            (&Val::Void, &Val::Void) => {
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
                    Some(_) => cmp,
                    None => None,
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
            (&Val::Unit, _) => {
                Some(Ordering::Less)
            }
            (_, &Val::Unit) => {
                Some(Ordering::Greater)
            }
            (&Val::Void, _) => {
                Some(Ordering::Less)
            }
            (_, &Val::Void) => {
                Some(Ordering::Greater)
            }
            (&Val::Sexpr(_, _), _) => Some(Ordering::Less),
            (_, &Val::Sexpr(_, _)) => Some(Ordering::Greater),
            //(&Val::Sexpr(_, ref x1), &Val::Sexpr(t2, ref x2)) => {
            _ => {
                println!("can't compare({:?},{:?})", self, other);
                None
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
            &Val::Unit => {
                Val::Unit
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
pub struct Env {
    params: Val,
    result: Option<Val>,
    reg: BTreeMap<i8, Val>,
    kv: BTreeMap<String, Val>,
    error: Val,
}

impl Env {
    pub fn new() -> Env {
        Env{
            params: Val::Unit,
            result: None,
            reg: BTreeMap::new(),
            kv: BTreeMap::new(),
            error: Val::Failure,
        }
    }

    pub fn with_args(v: Val) -> Env {
        Env{
            params: v,
            result: None,
            reg: BTreeMap::new(),
            kv: BTreeMap::new(),
            error: Val::Failure,
        }
    }

    pub fn get_param(&self, p: i8) -> &Val {
        match self.params {
            Val::Tuple(ref ps) => {
                &ps[p as usize]
            }
            _ => {
                panic!("that's not a tuple: {:?}", self.params);
            }
        }
    }

    pub fn get_mut_param(&mut self, p: i8) -> &mut Val {
        match self.params {
            Val::Tuple(ref mut ps) => {
                &mut ps[p as usize]
            }
            _ => {
                panic!("that's not a tuple: {:?}", self.params);
            }
        }
    }

    pub fn set_reg(&mut self, reg: &Reg, v: Val) {
        match reg {
            &Reg::R1(r) => {
                self.set_reg_r1(r, v)
            }
            &Reg::R2(r1,r2) => {
                self.get_mut_reg_r1(r1).set_reg_r1(r2, v);
            }
            &Reg::Result => {
                self.result = Some(v);
            }
            &Reg::Result2(r2) => {
                match self.result {
                    Some(ref mut resultv) => {
                        resultv.set_reg_r1(r2, v);
                    }
                    None => {
                        panic!("Result is None {:?}", reg);
                    }
                }
            }
            _ => {
                panic!("set other reg: {:?}", reg);
            }
        }
    }

    pub fn get_mut_reg_r1(&mut self, r: i8) -> &mut Val
    {
        if self.reg.contains_key(&r) {
            self.reg.get_mut(&r).unwrap()
        } else {
            panic!("register is not set: {}", r);
        }
    }

    pub fn get_reg(&self, reg: &Reg) -> &Val
    {
        match reg {
            &Reg::P1(p) => {
                self.get_param(p)
            }
            &Reg::R1(r) => {
                self.get_reg_r1(r)
            }
            &Reg::R2(r1,r2) => {
                self.get_reg_r1(r1).get_reg_r1(r2)
            }
            &Reg::Result => {
                match self.result {
                    Some(ref rv) => {
                        rv
                    }
                    None => {
                        panic!("No result set!")
                    }
                }
            }
            &Reg::Result2(r2) => {
                match self.result {
                    Some(ref rv) => {
                        rv.get_reg_r1(r2)
                    }
                    None => {
                        panic!("No result set!")
                    }
                }
            }
            _ => {
                println!("register is not set: {:?}", reg);
                &self.error
            }
        }
    }

    pub fn takeResult(&mut self) -> Val {
        match self.result.take() {
            None => {
                Val::Unit
            }
            Some(v) => {
                v
            }
        }
    }
}

impl reg::NumericRegistry for Env {

    fn set_reg_r1(&mut self, r: i8, v: Val) {
        if self.reg.contains_key(&r) {
            println!("register already set: {}", r);
        }
        self.reg.insert(r, v);
    }

    fn get_reg_r1(&self, r: i8) -> &Val {
        if self.reg.contains_key(&r) {
            self.reg.get(&r).unwrap()
        } else {
verbose_out!("{:?} not set in {:?}\n", r, self.reg);
            panic!("register is not set: {}", r);
        }
    }
}


#[cfg(test)]
mod tests {
    use leema::val::{Val, SexprType};
    use leema::list;
    use leema::sexpr;
    use std::collections::{HashMap};
    use std::sync::{Arc};


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
    let body = sexpr::new(SexprType::IfExpr,
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

    let expected = sexpr::new(SexprType::IfExpr,
        list::cons(Val::Bool(true),
        list::cons(Val::Bool(false),
        list::cons(Val::Bool(false),
        Val::Nil,
        ))),
    );
    assert_eq!(expected, result);
}

}
