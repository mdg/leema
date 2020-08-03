use crate::leema::failure::{Failure, Lresult};
use crate::leema::frame::FrameTrace;
use crate::leema::list;
use crate::leema::lmap::{self, LmapNode};
use crate::leema::lstr::Lstr;
use crate::leema::module::{CanonicalMod, ModKey, TypeMod};
use crate::leema::msg;
use crate::leema::reg::{self, Ireg, Iregistry, Reg};
use crate::leema::sendclone;
use crate::leema::struple::{self, Struple2, StrupleItem, StrupleKV};

use std::cmp::{Ordering, PartialEq, PartialOrd};
use std::fmt;
use std::io::Error;
use std::sync::mpsc::Receiver;
use std::sync::{Arc, Mutex};

use mopa::mopafy;


pub const CORE_MOD: TypeMod = canonical_typemod!("/core");

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
pub enum FuncType2
{
    Pure,
    Obs,
    Sys,
    Query,
    Cmd,
    Main, // or Control?
}

#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
#[derive(Eq)]
#[derive(Hash)]
#[derive(Ord)]
pub struct FuncType
{
    pub args: Struple2<Type>,
    pub closed: Struple2<Type>,
    pub result: Box<Type>,
}

impl FuncType
{
    pub fn new(args: Struple2<Type>, result: Type) -> FuncType
    {
        FuncType {
            args,
            closed: vec![],
            result: Box::new(result),
        }
    }

    pub fn new_closure(
        args: Struple2<Type>,
        closed: Struple2<Type>,
        result: Type,
    ) -> FuncType
    {
        FuncType {
            args,
            closed,
            result: Box::new(result),
        }
    }

    pub fn is_open(&self) -> bool
    {
        self.args.iter().any(|a| a.v.is_open())
            || self.closed.iter().any(|c| c.v.is_open())
            || self.result.is_open()
    }

    pub fn is_closed(&self) -> bool
    {
        !self.is_open()
    }

    pub fn call_args(&self) -> Struple2<Val>
    {
        self.args
            .iter()
            .chain(self.closed.iter())
            .map(|a| StrupleItem::new(a.k.clone(), Val::VOID))
            .collect()
    }

    pub fn map<Op>(&self, op: &Op) -> Lresult<FuncType>
    where
        Op: Fn(&Type) -> Lresult<Option<Type>>,
    {
        let mapf = |v: &Type| v.map(op);
        let m_args = struple::map_v(&self.args, mapf)?;
        let m_closed = struple::map_v(&self.closed, mapf)?;
        let m_result = self.result.map(op)?;
        Ok(FuncType::new_closure(m_args, m_closed, m_result))
    }
}

pub type GenericTypes = StrupleKV<&'static str, Type>;
pub type GenericTypeSlice = [StrupleItem<&'static str, Type>];
pub type TypeKey = (CanonicalMod, &'static str);

// #[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
#[derive(Eq)]
#[derive(Hash)]
#[derive(Ord)]
pub enum Type
{
    Tuple(Struple2<Type>),
    Func(FuncType),
    // different from base collection/map interfaces?
    // base interface/type should probably be iterator
    // and then it should be a protocol, not type
    StrictList(Box<Type>),
    User(TypeMod, &'static str),
    Variant(Box<Type>, &'static str),
    /// bool is open
    /// TODO: convert open flag to an enum
    Generic(bool, Box<Type>, GenericTypes),

    RustBlock,
    Kind,
    Any,

    Unknown,
    OpenVar(&'static str),
    LocalVar(Lstr),
}

impl Type
{
    pub const INT: Type = Type::User(CORE_MOD, "Int");
    pub const STR: Type = Type::User(CORE_MOD, "Str");
    pub const BOOL: Type = Type::User(CORE_MOD, "Bool");
    pub const HASHTAG: Type = Type::User(CORE_MOD, "#");
    pub const FAILURE: Type = Type::User(CORE_MOD, "Failure");
    pub const VOID: Type = Type::User(CORE_MOD, "Void");

    pub fn f(inputs: Struple2<Type>, result: Type) -> Type
    {
        Type::Func(FuncType {
            args: inputs,
            closed: vec![],
            result: Box::new(result),
        })
    }

    /**
     * Get the typename including the module
     */
    pub fn full_typename(&self) -> Lstr
    {
        match self {
            &Type::User(ref m, ref name) => lstrf!("{}.{}", m, name),
            &Type::Variant(ref t, ref var) => lstrf!("{}::{}", t, var),
            &Type::OpenVar(name) => Lstr::from(format!("${}", name)),
            &Type::LocalVar(ref name) => Lstr::from(format!("local:{}", name)),
            _ => {
                panic!("no typename for {:?}", self);
            }
        }
    }

    pub fn split_func_ref(t: &Type) -> (&Struple2<Type>, &Type)
    {
        match t {
            &Type::Func(ref ftype) => (&ftype.args, &ftype.result),
            _ => {
                panic!("not a func type {:?}", t);
            }
        }
    }

    pub fn inner(var: &Lstr, i: i16) -> Type
    {
        Type::LocalVar(lstrf!("{}$inner{}", var.str(), i))
    }

    pub fn is_func(&self) -> bool
    {
        match self {
            &Type::Func(_) => true,
            _ => false,
        }
    }

    pub fn is_open(&self) -> bool
    {
        match self {
            Type::Generic(open, _, _) => *open,
            Type::OpenVar(_) => true,
            Type::Tuple(items) => items.iter().any(|i| i.v.is_open()),
            Type::StrictList(inner) => inner.is_open(),
            Type::Unknown => true,
            _ => false,
        }
    }

    pub fn is_closed(&self) -> bool
    {
        !self.is_open()
    }

    pub fn is_failure(&self) -> bool
    {
        match self {
            &Type::User(ref module, "Failure") => {
                module.canonical.str() == "/core"
            }
            _ => false,
        }
    }

    pub fn replace_openvar(&self, id: &str, new_type: &Type) -> Lresult<Type>
    {
        let op = |t: &Type| -> Lresult<Option<Type>> {
            match t {
                Type::OpenVar(ovar) => {
                    let replacement = if *ovar == id {
                        Some(new_type.clone())
                    } else {
                        None
                    };
                    Ok(replacement)
                }
                _ => Ok(None),
            }
        };
        self.map(&op)
    }

    pub fn map<Op>(&self, op: &Op) -> Lresult<Type>
    where
        Op: Fn(&Type) -> Lresult<Option<Type>>,
    {
        if let Some(m_self) = op(self)? {
            return Ok(m_self);
        }

        let res = match self {
            &Type::Tuple(ref items) => {
                let m_items = struple::map_v(items, |i| i.map(op))?;
                Type::Tuple(m_items)
            }
            &Type::StrictList(ref inner) => {
                let m_inner = inner.map(op)?;
                Type::StrictList(Box::new(m_inner))
            }
            &Type::Func(ref ftyp) => {
                let m_ftype = ftyp.map(op)?;
                Type::Func(m_ftype)
            }
            _ => self.clone(),
        };
        Ok(res)
    }

    pub fn list_inner_type(&self) -> Type
    {
        match self {
            &Type::StrictList(ref inner) => (**inner).clone(),
            &Type::OpenVar(_) => Type::Unknown,
            &Type::LocalVar(_) => Type::Unknown,
            &Type::Unknown => Type::Unknown,
            _ => {
                panic!("cannot get inner type of a not list: {:?}", self);
            }
        }
    }
}

impl sendclone::SendClone for Type
{
    type Item = Type;

    fn clone_for_send(&self) -> Type
    {
        match self {
            &Type::Tuple(ref items) => Type::Tuple(items.clone_for_send()),
            &Type::StrictList(ref i) => {
                Type::StrictList(Box::new(i.clone_for_send()))
            }
            &Type::Func(ref ftyp) => Type::Func(ftyp.clone()),
            &Type::Unknown => Type::Unknown,
            &Type::OpenVar(id) => Type::OpenVar(id),
            &Type::LocalVar(ref id) => Type::LocalVar(id.clone_for_send()),
            &Type::Generic(open, ref subt, ref opens) => {
                let subt2 = Box::new(subt.clone_for_send());
                let opens2 = opens.clone_for_send();
                Type::Generic(open, subt2, opens2)
            }
            &Type::User(ref module, typ) => Type::User(module.clone(), typ),
            &Type::Variant(ref t, var) => {
                Type::Variant(Box::new(t.clone_for_send()), var)
            }
            _ => {
                panic!("cannot clone_for_send Type: {:?}", self);
            }
        }
    }
}

impl fmt::Display for FuncType
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "(")?;
        for a in self.args.iter() {
            match a.k {
                Some(ref k) => {
                    write!(f, "{}:{},", k, a.v)?;
                }
                None => {
                    write!(f, "{},", a.v)?;
                }
            }
        }
        if !self.closed.is_empty() {
            write!(f, "...")?;
        }
        for c in self.closed.iter() {
            match c.k {
                Some(ref k) => {
                    write!(f, "{}:{},", k, c.v)?;
                }
                None => {
                    write!(f, "{},", c.v)?;
                }
            }
        }
        write!(f, "):{}", self.result)
    }
}

impl fmt::Display for Type
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self {
            &Type::Tuple(ref items) => {
                write!(f, "(")?;
                for i in items {
                    write!(f, "{}", i)?;
                }
                write!(f, ")")
            }
            &Type::User(ref module, ref id) => write!(f, "{}.{}", module, id),
            &Type::Variant(ref t, ref var) => write!(f, "{}::{}", t, var),
            &Type::Generic(_, ref inner, ref args) => {
                write!(f, "{}{:?}", inner, args)
            }
            &Type::Func(ref ftyp) => write!(f, "F{}", ftyp),
            // different from base collection/map interfaces?
            // base interface/type should probably be iterator
            // and then it should be a protocol, not type
            &Type::StrictList(ref typ) => write!(f, "List<{}>", typ),
            &Type::RustBlock => write!(f, "RustBlock"),
            &Type::Kind => write!(f, "Kind"),
            &Type::Any => write!(f, "Any"),

            &Type::Unknown => write!(f, "TypeUnknown"),
            &Type::OpenVar(ref name) => write!(f, "${}", name),
            &Type::LocalVar(ref name) => write!(f, "local${}", name),
        }
    }
}

impl fmt::Debug for Type
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self {
            &Type::Tuple(ref items) => write!(f, "(T{:?})", items),
            &Type::Func(ref ftyp) => write!(f, "{:?}", ftyp),
            // different from base collection/map interfaces?
            // base interface/type should probably be iterator
            // and then it should be a protocol, not type
            &Type::StrictList(ref typ) => write!(f, "List<{}>", typ),
            &Type::User(ref module, ref id) => write!(f, "{}.{}", module, id),
            &Type::Variant(ref t, ref var) => write!(f, "{}::{}", t, var),
            &Type::Generic(open, ref inner, ref args) => {
                let open_tag = if open { "Open" } else { "Closed" };
                write!(f, "({} ({}){:?})", open_tag, inner, args)
            }
            &Type::RustBlock => write!(f, "RustBlock"),
            &Type::Kind => write!(f, "Kind"),
            &Type::Any => write!(f, "Any"),

            &Type::Unknown => write!(f, "TypeUnknown"),
            &Type::OpenVar(name) => write!(f, "${}", name),
            &Type::LocalVar(ref name) => write!(f, "local${}", name),
        }
    }
}


pub trait LibVal: mopa::Any + fmt::Debug + Send + Sync
{
    fn get_type(&self) -> Type;
}

mopafy!(LibVal);

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
        SrcLoc {
            lineno: l,
            column: c,
        }
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

pub const DEFAULT_SRC_LOC: SrcLoc = SrcLoc {
    lineno: 0,
    column: 0,
};

pub const FAILURE_SUCCESS: i8 = 0;
pub const FAILURE_NOENTRY: i8 = -1;
pub const FAILURE_BADINPUT: i8 = -2;
pub const FAILURE_UNAUTHENTICATED: i8 = -3;
pub const FAILURE_UNAUTHORIZED: i8 = -4;
pub const FAILURE_MISSINGDATA: i8 = -5;
pub const FAILURE_TIMEOUT: i8 = -6;
pub const FAILURE_INTERNAL: i8 = -7;
pub const FAILURE_TYPE: i8 = -8;

#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
#[derive(Eq)]
#[derive(Hash)]
#[derive(Ord)]
pub struct Fref
{
    pub m: ModKey,
    pub f: &'static str,
    pub t: Type,
}

impl Fref
{
    pub fn new(m: ModKey, f: &'static str, t: Type) -> Fref
    {
        Fref { m, f, t }
    }

    pub fn with_modules(m: ModKey, f: &'static str) -> Fref
    {
        Fref {
            m,
            f,
            t: Type::Unknown,
        }
    }
}

impl From<(&'static str, &'static str)> for Fref
{
    fn from(input: (&'static str, &'static str)) -> Fref
    {
        let m = ModKey::from(input.0);
        Fref::with_modules(m, input.1)
    }
}

impl fmt::Display for Fref
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "({}::{} {:?})", self.m, self.f, self.t)
    }
}

impl sendclone::SendClone for Fref
{
    type Item = Fref;

    fn clone_for_send(&self) -> Fref
    {
        Fref {
            m: self.m.clone_for_send(),
            f: self.f,
            t: self.t.clone_for_send(),
        }
    }
}

pub type MsgVal = msg::MsgItem<Val>;

#[derive(Clone)]
pub enum Val
{
    Int(i64),
    Str(Lstr),
    // StrCat(Arc<Val>, Box<Val>),
    Bool(bool),
    Hashtag(Lstr),
    Buffer(Vec<u8>),
    Cons(Box<Val>, Arc<Val>),
    Nil,
    Tuple(Struple2<Val>),
    Struct(Type, Struple2<Val>),
    EnumStruct(Type, Lstr, Struple2<Val>),
    EnumToken(Type, Lstr),
    Token(Type),
    Map(LmapNode),
    Failure2(Box<Failure>),
    Type(Type),
    Construct(Fref),
    Lib(Arc<dyn LibVal>),
    // Fref(Fref),
    Call(Fref, Struple2<Val>),
    ResourceRef(i64),
    RustBlock,
    Future(Arc<Mutex<Receiver<Val>>>),
    Wildcard,
    PatternVar(Reg),
}

const NIL: Val = Val::Nil;
pub const FALSE: Val = Val::Bool(false);
pub const TRUE: Val = Val::Bool(true);

impl Val
{
    pub const VOID: Val = Val::Token(Type::VOID);

    pub fn empty_tuple() -> Val
    {
        Val::new_tuple(0)
    }

    pub fn new_tuple(ref sz: usize) -> Val
    {
        let mut t = Vec::with_capacity(*sz);
        let mut i: usize = *sz;
        while i > 0 {
            t.push(StrupleItem::new(None, Val::VOID));
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
        let empties: Vec<StrupleItem<Option<Lstr>, Val>> =
            Vec::with_capacity(list::len(l));
        let items = list::fold_ref(empties, l, |mut res, item| {
            res.push(StrupleItem::new(None, item.clone()));
            res
        });
        Val::Tuple(items)
    }

    pub fn is_call(&self) -> bool
    {
        match self {
            &Val::Call(_, _) => true,
            _ => false,
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

    pub fn empty_str() -> Val
    {
        Val::Str(Lstr::EMPTY)
    }

    pub fn str(&self) -> &str
    {
        match self {
            &Val::Str(ref s) => s.str(),
            &Val::Hashtag(ref s) => s.str(),
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

    pub fn is_type(&self) -> bool
    {
        match self {
            &Val::Type(_) => true,
            &Val::Construct(_) => true,
            _ => false,
        }
    }

    pub fn future(r: Receiver<Val>) -> Val
    {
        Val::Future(Arc::new(Mutex::new(r)))
    }

    pub fn is_future(&self) -> bool
    {
        match self {
            &Val::Future(_) => true,
            _ => false,
        }
    }

    pub fn is_failure(&self) -> bool
    {
        match self {
            &Val::Failure2(_) => true,
            _ => false,
        }
    }

    pub fn failure(
        tag: Val,
        msg: Val,
        trace: Arc<FrameTrace>,
        status: i8,
    ) -> Val
    {
        Val::Failure2(Box::new(Failure::leema_new(
            tag,
            msg,
            Some(trace),
            status,
        )))
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

    pub fn libval_as<T>(&self) -> Option<&T>
    where
        T: LibVal,
    {
        match self {
            &Val::Lib(ref lvarc) => {
                vout!("lvarc: {:?}\n", lvarc);
                let lvref: &dyn LibVal = &**lvarc;
                vout!("lvref: {:?}\n", lvref);
                lvref.downcast_ref::<T>()
            }
            _ => None,
        }
    }

    pub fn get_type(&self) -> Type
    {
        match self {
            &Val::Bool(_) => Type::BOOL.clone(),
            &Val::Int(_) => Type::INT.clone(),
            &Val::Str(_) => Type::STR.clone(),
            &Val::Hashtag(_) => Type::HASHTAG.clone(),
            &Val::Cons(ref head, _) => {
                let inner = head.get_type();
                Type::StrictList(Box::new(inner))
            }
            &Val::Nil => Type::StrictList(Box::new(Type::Unknown)),
            &Val::Failure2(_) => Type::FAILURE,
            &Val::Type(_) => Type::Kind,
            &Val::Construct(ref f) => f.t.clone(),
            &Val::Wildcard => Type::Unknown,
            &Val::PatternVar(_) => Type::Unknown,
            &Val::RustBlock => Type::RustBlock,
            &Val::Map(_) => lmap::map_type(),
            &Val::Tuple(ref items) if items.len() == 1 => {
                items.get(0).unwrap().v.get_type()
            }
            &Val::Tuple(ref items) => {
                let tuptypes = items
                    .iter()
                    .map(|i| (StrupleItem::new(i.k.clone(), i.v.get_type())))
                    .collect();
                Type::Tuple(tuptypes)
            }
            &Val::Struct(ref typ, _) => typ.clone(),
            &Val::EnumStruct(ref typ, _, _) => typ.clone(),
            &Val::EnumToken(ref typ, _) => typ.clone(),
            &Val::Token(ref typ) => typ.clone(),
            &Val::Buffer(_) => Type::STR,
            &Val::Call(ref fref, _) => fref.t.clone(),
            &Val::Lib(ref lv) => lv.get_type(),
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

    fn _pattern_match(
        assigns: &mut Vec<(Reg, Val)>,
        patt: &Val,
        input: &Val,
    ) -> bool
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
            (&Val::Cons(_, _), &Val::Cons(_, _)) => {
                Val::_pattern_match_list(assigns, patt, input)
            }
            (&Val::Tuple(ref pv), &Val::Tuple(ref iv))
                if pv.len() == iv.len() =>
            {
                pv.iter().zip(iv.iter()).all(|(p_item, i_item)| {
                    Val::_pattern_match(assigns, &p_item.v, &i_item.v)
                })
            }
            (&Val::Struct(ref pt, ref pv), &Val::Struct(ref it, ref iv))
                if pv.len() == iv.len() =>
            {
                // this type check shouldn't be necessary
                // if the type checking was right
                if pt != it {
                    return false;
                }
                pv.iter().zip(iv.iter()).all(|(p_item, i_item)| {
                    Val::_pattern_match(assigns, &p_item.v, &i_item.v)
                })
            }
            (
                &Val::EnumStruct(_, ref pname, ref pv),
                &Val::EnumStruct(_, ref iname, ref iv),
            ) if pv.len() == iv.len() => {
                if pname != iname {
                    return false;
                }
                pv.iter().zip(iv.iter()).all(|(p_item, i_item)| {
                    Val::_pattern_match(assigns, &p_item.v, &i_item.v)
                })
            }
            (
                &Val::EnumToken(ref pt, ref pname),
                &Val::EnumToken(ref it, ref iname),
            ) => pt == it && pname == iname,
            (&Val::Token(ref pt), &Val::Token(ref it)) => pt == it,
            (&Val::Nil, &Val::Nil) => true,
            _ => false,
        }
    }

    fn _pattern_match_list(
        assigns: &mut Vec<(Reg, Val)>,
        patt: &Val,
        input: &Val,
    ) -> bool
    {
        match (patt, input) {
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
        }
    }

    pub fn map<Op>(&self, op: &Op) -> Lresult<Val>
    where
        Op: Fn(&Val) -> Lresult<Option<Val>>,
    {
        if let Some(m_self) = op(self)? {
            return Ok(m_self);
        }

        let m_result = match self {
            &Val::Cons(ref head, ref tail) => {
                let m_head = head.map(op)?;
                let m_tail = tail.map(op)?;
                Val::Cons(Box::new(m_head), Arc::new(m_tail))
            }
            &Val::Tuple(ref flds) => {
                let m_flds = struple::map_v(flds, |f: &Val| f.map(op))?;
                Val::Tuple(m_flds)
            }
            &Val::Struct(ref typ, ref flds) => {
                let m_flds = struple::map_v(flds, |f: &Val| f.map(op))?;
                Val::Struct(typ.clone(), m_flds)
            }
            &Val::EnumStruct(ref typ, ref vname, ref flds) => {
                let m_flds = struple::map_v(flds, |f: &Val| f.map(op))?;
                Val::EnumStruct(typ.clone(), vname.clone(), m_flds)
            }
            &Val::EnumToken(ref typ, ref vname) => {
                Val::EnumToken(typ.clone(), vname.clone())
            }
            &Val::Token(ref typ) => Val::Token(typ.clone()),
            &Val::Call(ref f, ref args) => {
                let m_fref = f.clone();
                let m_args = struple::map_v(args, |a| a.map(op))?;
                Val::Call(m_fref, m_args)
            }
            &Val::Failure2(ref failure) => {
                let m_tag = failure.tag.map(op)?;
                let m_msg = failure.msg.map(op)?;
                Val::Failure2(Box::new(Failure::leema_new(
                    m_tag,
                    m_msg,
                    failure.trace.clone(),
                    failure.code,
                )))
            }
            _ => self.clone(),
        };
        Ok(m_result)
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
            &Val::Wildcard => write!(f, ";_"),
            &Val::PatternVar(_) => write!(f, ";{:?}", l),
            _ => {
                panic!("Not a list: {:?}", l);
            }
        }
    }

    fn fmt_tuple(f: &mut fmt::Formatter, t: &Vec<Val>, dbg: bool)
        -> fmt::Result
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

impl From<&Fref> for Lresult<Val>
{
    fn from(f: &Fref) -> Lresult<Val>
    {
        match &f.t {
            Type::Func(ft) => {
                Ok(Val::Call(
                    f.clone(),
                    struple::map_v(&ft.args, |_| Ok(Val::VOID))?,
                ))
            }
            not_func => {
                Err(rustfail!(
                    "compile_failure",
                    "not a func type: {}",
                    not_func,
                ))
            }
        }
    }
}

impl From<Error> for Val
{
    fn from(_e: Error) -> Val
    {
        Val::VOID
    }
}

impl sendclone::SendClone for Val
{
    type Item = Val;

    fn clone_for_send(&self) -> Val
    {
        match self {
            &Val::Int(i) => Val::Int(i),
            &Val::Str(ref s) => Val::Str(s.clone_for_send()),
            &Val::Bool(b) => Val::Bool(b),
            &Val::Hashtag(ref s) => Val::Hashtag(s.clone_for_send()),
            &Val::Cons(ref head, ref tail) => {
                Val::Cons(Box::new(head.clone_for_send()), tail.clone())
            }
            &Val::Nil => Val::Nil,
            &Val::Tuple(ref flds) => Val::Tuple(flds.clone_for_send()),
            &Val::Struct(ref typ, ref flds) => {
                Val::Struct(typ.clone_for_send(), flds.clone_for_send())
            }
            &Val::EnumStruct(ref typ, ref vname, ref flds) => {
                Val::EnumStruct(
                    typ.clone_for_send(),
                    vname.clone_for_send(),
                    flds.clone_for_send(),
                )
            }
            &Val::EnumToken(ref typ, ref vname) => {
                Val::EnumToken(typ.clone_for_send(), vname.clone_for_send())
            }
            &Val::Token(ref typ) => Val::Token(typ.clone_for_send()),
            &Val::Call(ref f, ref args) => {
                let f2 = f.clone_for_send();
                let args2 = args.clone_for_send();
                Val::Call(f2, args2)
            }
            &Val::Failure2(ref f) => {
                Val::Failure2(Box::new(f.clone_for_send()))
            }
            &Val::Type(ref t) => Val::Type(t.clone_for_send()),
            &Val::Construct(ref t) => Val::Construct(t.clone_for_send()),
            &Val::ResourceRef(r) => Val::ResourceRef(r),
            // &Val::Lib(LibVal),
            // &Val::RustBlock,
            &Val::Future(ref f) => Val::Future(f.clone()),
            &Val::Wildcard => Val::Wildcard,
            &Val::PatternVar(ref r) => Val::PatternVar(r.clone()),
            &Val::Map(_) => {
                panic!("cannot deep clone Map");
            }
            &Val::RustBlock => Val::RustBlock,
            _ => {
                panic!("cannot deep clone val: {:?}", self);
            }
        }
    }
}

impl fmt::Display for Val
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match *self {
            Val::Str(ref s) => write!(f, "{}", s),
            Val::Int(ref i) => write!(f, "{}", i),
            Val::Bool(false) => write!(f, "false"),
            Val::Bool(true) => write!(f, "true"),
            Val::Cons(_, _) => {
                write!(f, "[")
                    .and_then(|_| Val::fmt_list(f, self, false))
                    .and_then(|_| write!(f, "]"))
            }
            Val::Nil => write!(f, "[]"),
            Val::Hashtag(ref s) => f.write_str(s),
            Val::Tuple(ref items) => {
                write!(f, "(")?;
                for i in items {
                    write!(f, "{}", i)?;
                }
                write!(f, ")")
            }
            Val::Struct(ref typename, ref items) => {
                write!(f, "{}(", typename)?;
                for i in items {
                    write!(f, "{}", i)?;
                }
                write!(f, ")")
            }
            Val::EnumStruct(ref tname, ref var, ref items) => {
                write!(f, "{}.{}(", tname, var)?;
                for i in items {
                    write!(f, "{}", i)?;
                }
                write!(f, ")")
            }
            Val::EnumToken(_, ref var_name) => write!(f, "{}", var_name),
            Val::Token(ref typename) => write!(f, "{}", typename),
            Val::Map(ref map) => write!(f, "Map({:?})", map),
            Val::Buffer(ref _buf) => write!(f, "Buffer"),
            Val::Lib(ref lv) => write!(f, "LibVal({:?})", lv),
            Val::ResourceRef(rid) => write!(f, "ResourceRef({})", rid),
            Val::RustBlock => write!(f, "RustBlock"),
            Val::Failure2(ref fail) => write!(f, "Failure({:?})", **fail),
            Val::Type(ref t) => write!(f, "{}", t),
            Val::Construct(ref t) => write!(f, "{}", t),
            Val::Call(ref fref, ref args) => {
                write!(f, "{}::{}({:?}): {}", fref.m, fref.f, args, fref.t)
            }
            Val::Future(_) => write!(f, "Future"),
            Val::PatternVar(ref r) => write!(f, "pvar:{:?}", r),
            Val::Wildcard => write!(f, "_"),
        }
    }
}

impl fmt::Debug for Val
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match *self {
            Val::Str(ref s) => {
                let escaped = s.replace("\n", "\\n");
                write!(f, "Str(\"{}\")", escaped)
            }
            Val::Int(ref i) => write!(f, "Int({})", i),
            Val::Bool(b) => write!(f, "Bool({:?})", b),
            Val::Cons(_, _) => {
                write!(f, "L[")
                    .and_then(|_| Val::fmt_list(f, self, true))
                    .and_then(|_| write!(f, "]"))
            }
            Val::Nil => write!(f, "L[]"),
            Val::Hashtag(ref s) => f.write_str(s),
            Val::Buffer(ref buf) => write!(f, "Buffer<{:?}>", buf),
            Val::Tuple(ref fields) => write!(f, "Tuple {:?}", fields),
            Val::Struct(ref typ, ref fields) => {
                write!(f, "struct({}{:?})", typ, fields)
            }
            Val::EnumStruct(ref name, ref var_name, ref val) => {
                write!(f, "enum({:?}.{}{:?})", name, var_name, val)
            }
            Val::EnumToken(ref typ, ref var_name) => {
                write!(f, "EnumToken({:?}.{:?})", typ, var_name)
            }
            Val::Token(ref name) => write!(f, "Token({:?})", name),
            Val::Map(ref map) => write!(f, "Map({:?})", map),
            Val::Lib(ref lv) => write!(f, "LibVal({:?})", lv),
            Val::ResourceRef(rid) => write!(f, "ResourceRef({})", rid),
            Val::RustBlock => write!(f, "RustBlock"),
            Val::Failure2(ref fail) => write!(f, "Failure({:?})", fail),
            Val::Type(ref t) => write!(f, "TypeVal({:?})", t),
            Val::Construct(ref t) => write!(f, "Construct({:?})", t),
            Val::Call(ref fref, ref args) => {
                write!(f, "({}::{} {:?}: {:?})", fref.m, fref.f, args, fref.t)
            }
            Val::Future(_) => write!(f, "Future"),
            Val::PatternVar(ref r) => write!(f, "pvar:{:?}", r),
            Val::Wildcard => write!(f, "_Wildcard"),
        }
    }
}


impl reg::Iregistry for Val
{
    fn ireg_get(&self, i: Ireg) -> Lresult<&Val>
    {
        match (i, self) {
            // get reg on tuple
            (_, &Val::Tuple(ref items)) => {
                lfailoc!(items.ireg_get(i))
            }
            // get reg on struct
            (_, &Val::Struct(_, ref items)) => {
                lfailoc!(items.ireg_get(i))
            }
            // Get for Functions & Closures
            (_, &Val::Call(_, ref args)) => {
                lfailoc!(args.ireg_get(i))
            }
            // Failures
            (Ireg::Reg(0), &Val::Failure2(ref failure)) => Ok(&failure.tag),
            (Ireg::Reg(1), &Val::Failure2(ref failure)) => Ok(&failure.msg),
            (Ireg::Reg(2), &Val::Failure2(ref failure)) => {
                Err(rustfail!(
                    "leema_failure",
                    "Cannot access frame trace until it is implemented as a leema value {:?}",
                    failure.trace,
                ))
            }
            _ => {
                Err(rustfail!(
                    "leema_failure",
                    "unsupported registry value {:?}{:?}",
                    self,
                    i,
                ))
            }
        }
    }

    fn ireg_set(&mut self, i: Ireg, v: Val)
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
            (Ireg::Reg(0), &mut Val::Cons(ref mut head, _)) => {
                **head = v;
            }
            (Ireg::Sub(0, s), &mut Val::Cons(ref mut head, _)) => {
                head.ireg_set(Ireg::Reg(s), v);
            }
            (_, &mut Val::Cons(_, _)) => {
                panic!("cannot set reg within a list: {}", i);
            }
            (_, &mut Val::Nil) => {
                panic!("cannot set reg on empty list: {}", i);
            }
            // set reg on Fref
            (_, &mut Val::Call(_, ref mut args)) => {
                args.ireg_set(i, v);
            }
            // values that can't act as registries
            (_, dst) => {
                panic!("Can't ireg_set({:?}, {:?})", i, dst);
            }
        }
    }
}

impl Ord for Val
{
    fn cmp(&self, other: &Val) -> Ordering
    {
        PartialOrd::partial_cmp(self, other).expect("values weren't comparable")
    }
}

impl PartialOrd for Val
{
    fn partial_cmp(&self, other: &Val) -> Option<Ordering>
    {
        match (self, other) {
            (&Val::Int(a), &Val::Int(b)) => PartialOrd::partial_cmp(&a, &b),
            (&Val::Str(ref a), &Val::Str(ref b)) => {
                PartialOrd::partial_cmp(a, b)
            }
            (&Val::Bool(false), &Val::Bool(false)) => Some(Ordering::Equal),
            (&Val::Bool(true), &Val::Bool(true)) => Some(Ordering::Equal),
            (&Val::Nil, &Val::Nil) => Some(Ordering::Equal),
            (&Val::Hashtag(ref a), &Val::Hashtag(ref b)) => {
                PartialOrd::partial_cmp(a, b)
            }
            (&Val::Type(ref a), &Val::Type(ref b)) => {
                PartialOrd::partial_cmp(a, b)
            }
            (&Val::Wildcard, &Val::Wildcard) => Some(Ordering::Equal),
            (&Val::Nil, &Val::Cons(_, _)) => Some(Ordering::Less),
            (&Val::Cons(_, _), &Val::Nil) => Some(Ordering::Greater),
            (&Val::Cons(ref h1, ref t1), &Val::Cons(ref h2, ref t2)) => {
                let cmp = PartialOrd::partial_cmp(&*h1, &*h2);
                match cmp {
                    Some(Ordering::Equal) => {
                        PartialOrd::partial_cmp(&*t1, &*t2)
                    }
                    _ => cmp,
                }
            }
            (&Val::RustBlock, &Val::RustBlock) => Some(Ordering::Equal),
            // tuple to tuple comparison
            (&Val::Tuple(ref av), &Val::Tuple(ref bv)) => {
                PartialOrd::partial_cmp(av, bv)
            }
            // struct to struct comparison
            (&Val::Struct(ref at, ref av), &Val::Struct(ref bt, ref bv)) => {
                match PartialOrd::partial_cmp(&*at, &*bt) {
                    Some(Ordering::Equal) => PartialOrd::partial_cmp(av, bv),
                    tcmp => tcmp,
                }
            }
            // enum to enum comparison
            (
                &Val::EnumStruct(ref at, ref an, ref av),
                &Val::EnumStruct(ref bt, ref bn, ref bv),
            ) => {
                Some(
                    PartialOrd::partial_cmp(&*at, &*bt)
                        .unwrap()
                        .then_with(|| PartialOrd::partial_cmp(an, bn).unwrap())
                        .then_with(|| PartialOrd::partial_cmp(av, bv).unwrap()),
                )
            }
            // enumtoken to enumtoken comparison
            (
                &Val::EnumToken(ref at, ref an),
                &Val::EnumToken(ref bt, ref bn),
            ) => {
                Some(
                    PartialOrd::partial_cmp(&*at, &*bt).unwrap().then_with(
                        || PartialOrd::partial_cmp(&*an, &*bn).unwrap(),
                    ),
                )
            }
            // token to token comparison
            (&Val::Token(ref at), &Val::Token(ref bt)) => {
                PartialOrd::partial_cmp(&*at, &*bt)
            }
            // fref to fref comparison
            (&Val::Call(ref f1, ref a1), &Val::Call(ref f2, ref a2)) => {
                Some(
                    PartialOrd::partial_cmp(f1, f2)
                        .unwrap()
                        .then_with(|| PartialOrd::partial_cmp(a1, a2).unwrap()),
                )
            }
            (&Val::ResourceRef(rra), &Val::ResourceRef(rrb)) => {
                PartialOrd::partial_cmp(&rra, &rrb)
            }
            (&Val::Buffer(ref b1), &Val::Buffer(ref b2)) => {
                PartialOrd::partial_cmp(b1, b2)
            }

            // start comparing mixed types
            (&Val::Bool(false), _) => Some(Ordering::Less),
            (_, &Val::Bool(false)) => Some(Ordering::Greater),
            (&Val::Bool(true), _) => Some(Ordering::Less),
            (_, &Val::Bool(true)) => Some(Ordering::Greater),
            (&Val::Int(_), _) => Some(Ordering::Less),
            (_, &Val::Int(_)) => Some(Ordering::Greater),
            (&Val::Str(_), _) => Some(Ordering::Less),
            (_, &Val::Str(_)) => Some(Ordering::Greater),
            (&Val::Hashtag(_), _) => Some(Ordering::Less),
            (_, &Val::Hashtag(_)) => Some(Ordering::Greater),
            (&Val::Type(_), _) => Some(Ordering::Less),
            (_, &Val::Type(_)) => Some(Ordering::Greater),
            (&Val::ResourceRef(_), _) => Some(Ordering::Less),
            (_, &Val::ResourceRef(_)) => Some(Ordering::Greater),
            (&Val::Nil, _) => Some(Ordering::Less),
            (&Val::Cons(_, _), _) => Some(Ordering::Less),
            (_, &Val::Nil) => Some(Ordering::Greater),
            (_, &Val::Cons(_, _)) => Some(Ordering::Greater),
            (&Val::Tuple(_), _) => Some(Ordering::Less),
            (_, &Val::Tuple(_)) => Some(Ordering::Greater),
            (&Val::Struct(_, _), _) => Some(Ordering::Less),
            (_, &Val::Struct(_, _)) => Some(Ordering::Greater),
            (&Val::EnumStruct(_, _, _), _) => Some(Ordering::Less),
            (_, &Val::EnumStruct(_, _, _)) => Some(Ordering::Greater),
            (&Val::EnumToken(_, _), _) => Some(Ordering::Less),
            (_, &Val::EnumToken(_, _)) => Some(Ordering::Greater),
            (&Val::Token(_), _) => Some(Ordering::Less),
            (_, &Val::Token(_)) => Some(Ordering::Greater),
            (&Val::RustBlock, _) => Some(Ordering::Less),
            (_, &Val::RustBlock) => Some(Ordering::Greater),
            (&Val::Wildcard, _) => Some(Ordering::Less),
            (_, &Val::Wildcard) => Some(Ordering::Greater),
            _ => {
                eprintln!("cannot compare({:?},{:?})", self, other);
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

impl Eq for Val {}

impl AsMut<[u8]> for Val
{
    fn as_mut(&mut self) -> &mut [u8]
    {
        match self {
            &mut Val::Buffer(ref mut buf) => buf.as_mut(),
            _ => {
                panic!("Cannot convert val to AsMut<[u8]>: {:?}", self);
            }
        }
    }
}

impl Default for Val
{
    fn default() -> Val
    {
        Val::VOID
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
            &Val::Sxpr(ref s) => {
                Val::Sxpr(s.clone())
            }
            &Val::Type(ref t) => {
                Val::Type(t.clone())
            }
            &Val::Lib(ref lv, ref typ) => {
                Val::Lib((*lv).clone(), typ.clone())
            }
        }
    }
}
*/



#[derive(Debug)]
#[derive(Clone)]
pub struct Env
{
    params: Struple2<Val>,
    result: Option<Val>,
    // locals: StrupleKV<&'static str, Val>,
    // maybe eventually switch locals to init w/ a struple from the func?
    locals: StrupleKV<(), Val>,
    stack: StrupleKV<(), Val>,
}

impl Env
{
    pub fn new() -> Env
    {
        Env {
            params: vec![],
            result: None,
            locals: StrupleKV::new(),
            stack: StrupleKV::new(),
        }
    }

    pub fn with_args(args: Struple2<Val>) -> Env
    {
        Env {
            params: args,
            result: None,
            locals: StrupleKV::new(),
            stack: StrupleKV::new(),
        }
    }

    pub fn set_reg(&mut self, reg: Reg, v: Val)
    {
        match reg {
            Reg::Local(i) => {
                let primary = i.get_primary() as usize;
                if primary >= self.locals.len() {
                    self.locals.resize(primary + 1, Default::default())
                }
                self.locals.ireg_set(i, v)
            }
            Reg::Stack(i) => {
                let primary = i.get_primary() as usize;
                if primary >= self.stack.len() {
                    self.stack.resize(primary + 1, Default::default())
                }
                self.stack.ireg_set(i, v)
            }
            Reg::Void => {
                // do nothing, void reg is like /dev/null
            }
            _ => {
                panic!("set other reg: {:?}", reg);
            }
        }
    }

    pub fn get_reg(&self, reg: Reg) -> Lresult<&Val>
    {
        match reg {
            Reg::Param(r) => lfailoc!(self.params.ireg_get(r)),
            Reg::Local(i) => {
                lfailoc!(self.locals.ireg_get(i))
                    .map_err(|f| f.add_context(lstrf!("get_reg {}", reg)))
            }
            Reg::Stack(i) => lfailoc!(self.stack.ireg_get(i)),
            Reg::Void => {
                Err(rustfail!("leema_failure", "Cannot get Reg::Void",))
            }
            Reg::Lib => {
                Err(rustfail!(
                    "leema_failure",
                    "Please look in application library for Reg::Lib",
                ))
            }
            Reg::Undecided => {
                Err(
                    rustfail!("leema_failure", "Cannot get undecided register",),
                )
            }
        }
    }

    pub fn get_params(&self) -> &Struple2<Val>
    {
        &self.params
    }

    pub fn get_param(&self, reg: i8) -> Lresult<&Val>
    {
        lfailoc!(self.params.ireg_get(Ireg::Reg(reg)))
    }
}


#[cfg(test)]
mod tests
{
    use crate::leema::list;
    use crate::leema::lstr::Lstr;
    use crate::leema::reg::Reg;
    use crate::leema::struple;
    use crate::leema::val::{Type, Val};


    #[test]
    fn test_tuple_from_list()
    {
        let origl = list::cons(Val::Int(4), list::singleton(Val::Int(7)));
        let tuple = Val::tuple_from_list(&origl);
        print!("wtf?({:?})", tuple);
        let exp = Val::Tuple(struple::new_tuple2(Val::Int(4), Val::Int(7)));
        assert_eq!(exp, tuple);
    }

    #[test]
    fn test_equal_int()
    {
        let a = Val::Int(7);
        let b = Val::Int(7);
        assert!(a == b);
    }

    #[test]
    fn test_equal_str()
    {
        let a = Val::Str(Lstr::Sref("hello"));
        let b = Val::Str(Lstr::Sref("hello"));
        assert!(a == b);
    }

    #[test]
    fn test_equal_true()
    {
        let a = Val::Bool(true);
        let b = Val::Bool(true);
        assert!(a == b);
    }

    #[test]
    fn test_equal_false()
    {
        let a = Val::Bool(false);
        let b = Val::Bool(false);
        assert!(a == b);
    }

    #[test]
    fn test_tuple()
    {
        let a = Val::Tuple(struple::new_tuple2(Val::Int(3), Val::Int(7)));
        let b = Val::Tuple(struple::new_tuple2(Val::Int(3), Val::Int(7)));
        assert!(a == b);
    }

    #[test]
    fn test_compare_false_true()
    {
        let f = Val::Bool(false);
        let t = Val::Bool(true);
        assert!(f < t);
    }

    #[test]
    fn test_compare_true_false()
    {
        let f = Val::Bool(false);
        let t = Val::Bool(true);
        assert!(t > f);
    }

    #[test]
    fn test_struct_eq()
    {
        let t = user_type!("foo", "Taco");
        let a = Val::Struct(
            t.clone(),
            struple::new_tuple2(Val::Int(3), Val::Bool(false)),
        );
        let b =
            Val::Struct(t, struple::new_tuple2(Val::Int(3), Val::Bool(false)));
        assert_eq!(a, b);
    }

    #[test]
    fn test_struct_lt_type()
    {
        let a = Val::Struct(
            user_type!("foo", "Burrito"),
            struple::new_tuple2(Val::Int(3), Val::Bool(false)),
        );
        let b = Val::Struct(
            user_type!("foo", "Taco"),
            struple::new_tuple2(Val::Int(3), Val::Bool(false)),
        );
        assert!(a < b);
    }

    #[test]
    fn test_struct_lt_val()
    {
        let typ = user_type!("foo", "Taco");
        let a = Val::Struct(
            typ.clone(),
            struple::new_tuple2(Val::Bool(false), Val::Int(3)),
        );
        let b = Val::Struct(
            typ.clone(),
            struple::new_tuple2(Val::Bool(false), Val::Int(7)),
        );
        assert!(a < b);
    }

    #[test]
    fn test_enum_eq()
    {
        let etype = user_type!("animals", "Animal");

        let a = Val::EnumToken(etype.clone(), Lstr::from("Dog".to_string()));
        let b = Val::EnumToken(etype.clone(), Lstr::Sref("Dog"));
        assert_eq!(a, b);
    }

    #[test]
    fn test_enum_lt_type()
    {
        let typ = user_type!("foo", "Taco");
        let a = Val::EnumToken(typ.clone(), Lstr::Sref("Quesadilla"));
        let b = Val::EnumToken(typ, Lstr::Sref("Torta"));
        assert!(a < b);
    }

    #[test]
    fn test_enum_lt_variant()
    {
        let typ = user_type!("foo", "Taco");
        let a = Val::EnumToken(typ.clone(), Lstr::Sref("Burrito"));
        let b = Val::EnumToken(typ, Lstr::Sref("Torta"));
        assert!(a < b);
    }

    #[test]
    fn test_enum_lt_val()
    {
        let typ = user_type!("foo", "Taco");
        let a = Val::EnumStruct(
            typ.clone(),
            Lstr::Sref("Burrito"),
            struple::new_tuple2(Val::Int(5), Val::Int(8)),
        );
        let b = Val::EnumStruct(
            typ,
            Lstr::Sref("Burrito"),
            struple::new_tuple2(Val::Int(9), Val::Int(8)),
        );
        assert!(a < b);
    }

    #[test]
    fn test_format_struct_empty()
    {
        let typ = user_type!("foo", "Taco");
        let s = Val::Token(typ);

        let s_str = format!("{}", s);
        assert_eq!("foo::Taco", s_str);
    }

    #[test]
    fn test_format_enum_token()
    {
        let typ = user_type!("foo", "Taco");
        let e = Val::EnumToken(typ, Lstr::Sref("Burrito"));

        let e_str = format!("{}", e);
        assert_eq!("Burrito", e_str);
    }

    #[test]
    fn test_format_enum_namedtuple()
    {
        let burrito_str = Lstr::Sref("Burrito");
        let stype = user_type!("tortas", "Taco");
        let s = Val::EnumStruct(
            stype,
            burrito_str.clone(),
            struple::new_tuple2(Val::Int(5), Val::Int(8)),
        );

        let s_str = format!("{}", s);
        assert_eq!("tortas::Taco.Burrito(5,8,)", s_str);
    }

    #[test]
    fn test_compare_across_types()
    {
        let f = Val::Bool(false);
        let t = Val::Bool(true);
        let i = Val::Int(7);
        let s = Val::Str(Lstr::Sref("hello"));
        let stype = user_type!("foo", "Foo");
        let strct = Val::Struct(
            stype,
            struple::new_tuple2(Val::Int(2), Val::Bool(true)),
        );
        let etype = user_type!("foo", "Taco");
        let enm = Val::EnumStruct(
            etype,
            Lstr::Sref("Burrito"),
            struple::new_tuple2(Val::Int(8), Val::Int(6)),
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
        assert_eq!(Type::StrictList(Box::new(Type::INT)), typ);
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
        let patt = Val::Tuple(struple::new_tuple2(Val::Int(1), Val::Wildcard));
        let input = Val::Tuple(struple::new_tuple2(Val::Int(1), Val::Int(4)));
        let pmatch = Val::pattern_match(&patt, &input);
        assert!(pmatch.is_some());
    }
}
