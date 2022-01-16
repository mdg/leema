use crate::leema::canonical::Canonical;
use crate::leema::failure::{self, Failure, Lresult};
use crate::leema::frame::FrameTrace;
use crate::leema::list;
use crate::leema::lmap::{self, LmapNode};
use crate::leema::lstr::Lstr;
use crate::leema::module::ModKey;
use crate::leema::msg;
use crate::leema::reg::{self, Ireg, Reg};
use crate::leema::sendclone;
use crate::leema::struple::{self, Struple2, StrupleItem, StrupleKV};

use std::cmp::{Ordering, PartialEq, PartialOrd};
use std::collections::HashMap;
use std::fmt;
use std::io::Error;
use std::sync::mpsc::Receiver;
use std::sync::{Arc, Mutex};

use mopa::mopafy;


#[macro_export]
macro_rules! leema_type {
    ($t:ident) => {
        crate::leema::val::Type::t(concat!("/leema/", stringify!($t)), vec![])
    };
}

#[macro_export]
macro_rules! core_type {
    ($t:ident) => {
        crate::leema::val::Type::t(concat!("/core/", stringify!($t)), vec![])
    };
}

#[macro_export]
macro_rules! user_type {
    ($ct:literal) => {
        crate::leema::val::Type::t($ct, vec![])
    };
}

const UNNAMED_NAMES: [&'static str; 16] = [
    "__unnamed_0",
    "__unnamed_1",
    "__unnamed_2",
    "__unnamed_3",
    "__unnamed_4",
    "__unnamed_5",
    "__unnamed_6",
    "__unnamed_7",
    "__unnamed_8",
    "__unnamed_9",
    "__unnamed_10",
    "__unnamed_11",
    "__unnamed_12",
    "__unnamed_13",
    "__unnamed_14",
    "__unnamed_15",
];

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

/// Probably switch this to be StrupleItem<Option<Lstr>, Type>
/// so it matches Val and don't have to generate unnamed nonsense above
pub type TypeArg = StrupleItem<Lstr, Type>;
pub type TypeArgs = StrupleKV<Lstr, Type>;
pub type TypeArgSlice = [StrupleItem<Lstr, Type>];
pub type LocalTypeVars = HashMap<Lstr, Type>;

#[derive(Clone)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
#[derive(Eq)]
#[derive(Hash)]
#[derive(Ord)]
pub struct FuncTypeRef<'a>
{
    pub path: &'a str,
    pub type_args: &'a TypeArgSlice,
    pub result: &'a Type,
    pub args: &'a TypeArgSlice,
}

impl<'a> fmt::Display for FuncTypeRef<'a>
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        if !self.type_args.is_empty() {
            write!(f, "<(")?;
        }
        write!(f, "({} ::", self.result)?;
        for a in self.args.iter() {
            if a.v.is_func() {
                write!(f, " ({})", a.v)?;
            } else {
                write!(f, " {}", a.v)?;
            }
        }
        write!(f, ")")?;
        if !self.type_args.is_empty() {
            write!(f, ")")?;
            for ta in self.type_args.iter() {
                write!(f, " {}", ta.v)?;
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}

impl<'a> fmt::Debug for FuncTypeRef<'a>
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        if !self.type_args.is_empty() {
            write!(f, "<(")?;
        }
        write!(f, "{:?} ::", self.result)?;
        for a in self.args.iter() {
            if a.v.is_func() {
                write!(f, " ({:?})", a.v)?;
            } else {
                write!(f, " {:?}", a.v)?;
            }
            if f.alternate() {
                writeln!(f, "")?;
            }
        }
        if !self.type_args.is_empty() {
            write!(f, ")")?;
            for ta in self.type_args.iter() {
                write!(f, " {}:{:?}", ta.k, ta.v)?;
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}

#[derive(Debug)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
#[derive(Eq)]
#[derive(Hash)]
#[derive(Ord)]
pub struct FuncTypeRefMut<'a>
{
    pub path: &'a str,
    pub result: &'a mut Type,
    pub args: &'a mut TypeArgSlice,
}

#[derive(Debug)]
pub struct TypeRef<'a>(pub &'a str, pub &'a TypeArgSlice);
pub struct TypeRefMut<'a>(pub &'a str, pub &'a mut TypeArgSlice);

/// Enum to hold type info
/// does it need to be an enum or could it be flattened
/// to look like this? Type(Canonical, Struple2<Type>)
#[derive(Clone)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
#[derive(Eq)]
#[derive(Hash)]
#[derive(Ord)]
pub struct Type
{
    pub path: Canonical,
    pub args: TypeArgs,
}

impl Type
{
    // core type names to match for special behavior
    pub const PATH_BOOL: &'static str = "/core/Bool";
    /// Canonical path for the closure type
    /// FnType ClosedArgTypes
    pub const PATH_CLOSURE: &'static str = "/core/Closure";
    pub const PATH_CLOSURE_IMPL: &'static str = "/core/ClosureImpl";
    /// Canonical path for the func type
    pub const PATH_FN: &'static str = "/core/Fn";
    pub const PATH_FAILURE: &'static str = "/core/Failure";
    pub const PATH_FREF: &'static str = "/core/Fref";
    pub const PATH_HASHTAG: &'static str = "/core/#";
    pub const PATH_KIND: &'static str = "/core/Kind";
    /// Canonical path for the list type
    pub const PATH_LIST: &'static str = "/core/List";
    pub const PATH_NORETURN: &'static str = "/core/NoReturn";
    pub const PATH_NOTOKEN: &'static str = "/leema/NoToken";
    pub const PATH_INT: &'static str = "/core/Int";
    pub const PATH_OPTION: &'static str = "/core/Option";
    pub const PATH_STR: &'static str = "/core/Str";
    /// Canonical path for the Tuple type
    pub const PATH_TUPLE: &'static str = "/core/Tuple";
    pub const PATH_TYPE: &'static str = "/core/Type";
    pub const PATH_VOID: &'static str = "/core/Void";

    // core types
    pub const BOOL: Type = Type::named(Type::PATH_BOOL);
    pub const FAILURE: Type = Type::named(Type::PATH_FAILURE);
    pub const FREF: Type = Type::named(Type::PATH_FREF);
    pub const HASHTAG: Type = Type::named(Type::PATH_HASHTAG);
    pub const INT: Type = Type::named(Type::PATH_INT);
    pub const KIND: Type = Type::named(Type::PATH_KIND);
    pub const NOTOKEN: Type = Type::named(Type::PATH_NOTOKEN);
    pub const STR: Type = Type::named(Type::PATH_STR);
    pub const TYPE: Type = Type::named(Type::PATH_TYPE);
    pub const VOID: Type = Type::named(Type::PATH_VOID);

    // language implementation types
    pub const PARENT_FRAME: Type = Type::named("/leema/ParentFrame");

    // leema type paths
    // only used for internal compilation, not user code
    /// Type for abstract, unimplemented blocks in trait functions
    /// Matches whatever type is provided
    const PATH_BLOCK_ABSTRACT: &'static str = "/leema/BlockAbstract";
    /// Type assigned to -RUST- code blocks
    /// gets special treatment by the type checker to match any type
    const PATH_BLOCK_RUST: &'static str = "/leema/BlockRust";
    /// identifies a locally defined type variable
    pub const PATH_LOCAL: &'static str = "/leema/Local";
    /// identifies an open type variable
    pub const PATH_OPENVAR: &'static str = "/leema/Open";
    /// identifies an open type call
    pub const PATH_TYPECALL: &'static str = "/leema/TypeCall";
    /// identifies an unknown type
    pub const PATH_UNKNOWN: &'static str = "/leema/Unknown";

    // leema types
    /// Initial type to indicate the type checker doesn't know
    pub const UNKNOWN: Type = Type::named(Type::PATH_UNKNOWN);

    /// function struple key names
    pub const FNKEY_RESULT: Lstr = Lstr::Sref("result");

    /// Create a type w/ the given path name and no type arguments
    pub const fn new(path: Canonical, args: TypeArgs) -> Type
    {
        Type { path, args }
    }

    /// Create a type w/ the given path name
    pub const fn t(path: &'static str, args: TypeArgs) -> Type
    {
        Type {
            path: canonical!(path),
            args,
        }
    }

    /// Create a type w/ the given path name and no type arguments
    pub const fn named(path: &'static str) -> Type
    {
        Type {
            path: canonical!(path),
            args: vec![],
        }
    }

    pub fn f(result: Type, mut args: TypeArgs) -> Type
    {
        args.insert(0, StrupleItem::new(Self::FNKEY_RESULT, result));
        Type::t(Type::PATH_FN, args)
    }

    pub fn generic_f(type_args: TypeArgs, result: Type, args: TypeArgs)
        -> Type
    {
        let ftyp = Type::f(result, args);
        Type::typecall(ftyp, type_args)
    }

    /// Construct the type of a closure implementation function
    /// closed args
    pub fn closure_impl(inner: Type, closed: TypeArgs) -> Type
    {
        Type::t(
            Type::PATH_CLOSURE_IMPL,
            vec![
                StrupleItem::new(Lstr::Sref("F"), inner),
                StrupleItem::new(Lstr::Sref("T"), Type::tuple(closed)),
            ],
        )
    }

    /// Construct the type of a closure implementation function
    pub fn open_closure_impl() -> Type
    {
        Type::t(
            Type::PATH_CLOSURE_IMPL,
            vec![
                StrupleItem::new(Lstr::Sref("F"), Type::UNKNOWN),
                StrupleItem::new(Lstr::Sref("T"), Type::UNKNOWN),
            ],
        )
    }

    pub fn tuple(items: TypeArgs) -> Type
    {
        Type::t(Type::PATH_TUPLE, items)
    }

    /// construct a list type object
    pub fn list(inner: Type) -> Type
    {
        Self::generic_1(Type::PATH_LIST, Some(inner))
    }

    pub fn option(inner: Option<Type>) -> Type
    {
        Self::generic_1(Type::PATH_OPTION, inner)
    }

    /// Generic type. Default the inner type to T if none is provided
    /// Does this need to be a typecall?
    /// Maybe more complex types need to be?
    pub fn generic_1(path: &'static str, inner: Option<Type>) -> Type
    {
        let arg = match inner {
            Some(i) => i,
            None => Type::UNKNOWN,
        };
        let args = vec![StrupleItem::new(Lstr::Sref("T"), arg)];
        Type::t(path, args)
    }

    pub fn local(var: Lstr) -> Type
    {
        Type::t(
            Type::PATH_LOCAL,
            vec![StrupleItem {
                k: var,
                v: Type::VOID,
            }],
        )
    }

    pub fn open(var: Lstr) -> Type
    {
        Type::t(
            Type::PATH_OPENVAR,
            vec![StrupleItem {
                k: var,
                v: Type::UNKNOWN,
            }],
        )
    }

    /// Create a generic typecall
    pub fn typecall(result: Type, mut args: TypeArgs) -> Type
    {
        args.insert(0, StrupleItem::new(Lstr::Sref("result"), result));
        Type::t(Type::PATH_TYPECALL, args)
    }

    pub fn method_type(&self) -> Lresult<Type>
    {
        let fref = self.try_func_ref()?;
        Ok(Type::f(
            fref.result.clone(),
            fref.args[1..].iter().map(|a| a.clone()).collect(),
        ))
    }

    pub fn inner(var: &Lstr, i: i16) -> Type
    {
        Type::local(lstrf!("{}$inner{}", var.str(), i))
    }

    /// Check if this type has generic args
    pub fn is_generic(&self) -> bool
    {
        // can't be generic if no args
        if self.args.is_empty() {
            return false;
        }
        // can't be generic if no args and not a func
        if let Some(f) = self.func_ref() {
            !f.type_args.is_empty()
        } else {
            true
        }
    }

    /// This is used to determine if it's a user-defined type
    /// and used to set fields in a structure
    /// nothing that lives in a /leema/ module is user defined
    pub fn is_user(&self) -> bool
    {
        match self.path.as_str() {
            "/core/Fn" | "/core/Tuple" => false,
            path => !path.starts_with("/leema/"),
        }
    }

    pub fn is_func(&self) -> bool
    {
        match self.path.as_str() {
            "/core/Fn" => true,
            _ => false,
        }
    }

    pub fn is_local(&self) -> bool
    {
        self.path.as_str() == Type::PATH_LOCAL
    }

    pub fn contains_local(&self) -> bool
    {
        self.is_local() || self.args.iter().any(|a| a.v.contains_local())
    }

    pub fn contains_open(&self) -> bool
    {
        self.path.as_str() == Type::PATH_OPENVAR
            || self.args.iter().any(|a| a.v.contains_open())
    }

    pub fn is_closed(&self) -> bool
    {
        !self.contains_open()
    }

    pub fn is_failure(&self) -> bool
    {
        self.path.as_str() == Type::PATH_FAILURE
    }

    pub fn is_untyped_block(&self) -> bool
    {
        match self.path.as_str() {
            Type::PATH_BLOCK_ABSTRACT | Type::PATH_BLOCK_RUST => true,
            _ => false,
        }
    }

    pub fn type_ref<'a>(&'a self) -> TypeRef<'a>
    {
        TypeRef(self.path.as_str(), &self.args)
    }

    pub fn type_ref_mut<'a>(&'a mut self) -> TypeRefMut<'a>
    {
        TypeRefMut(self.path.as_str(), &mut self.args)
    }

    pub fn func_ref<'a>(&'a self) -> Option<FuncTypeRef<'a>>
    {
        match self.type_ref() {
            TypeRef(Type::PATH_FN, args) => {
                let result = &args.first()?.v;
                Some(FuncTypeRef {
                    path: Type::PATH_FN,
                    type_args: &[],
                    result: result,
                    args: &args[1..],
                })
            }
            TypeRef(Type::PATH_TYPECALL, args) => {
                let result = &args.first()?.v;
                let inner = result.func_ref()?;
                Some(FuncTypeRef {
                    path: Type::PATH_FN,
                    type_args: &args[1..],
                    result: &inner.result,
                    args: &inner.args,
                })
            }
            TypeRef(Type::PATH_CLOSURE, [f, _closed]) => {
                let f_func_ref = f.v.func_ref()?;
                Some(FuncTypeRef {
                    path: Type::PATH_CLOSURE,
                    type_args: &f_func_ref.type_args,
                    result: &f_func_ref.result,
                    args: &f_func_ref.args,
                })
            }
            _ => None,
        }
    }

    pub fn func_ref_mut<'a>(&'a mut self) -> Option<FuncTypeRefMut<'a>>
    {
        match self.type_ref_mut() {
            TypeRefMut(Type::PATH_FN, args) => {
                let (result, args) = args.split_at_mut(1);
                Some(FuncTypeRefMut {
                    path: Type::PATH_FN,
                    result: &mut result.first_mut()?.v,
                    args,
                })
            }
            TypeRefMut(Type::PATH_TYPECALL, args) => {
                let result = &mut args.first_mut()?.v;
                let inner = result.func_ref_mut()?;
                Some(FuncTypeRefMut {
                    path: Type::PATH_FN,
                    result: inner.result,
                    args: inner.args,
                })
            }
            _ => None,
        }
    }

    pub fn try_func_ref_mut<'a>(&'a mut self) -> Lresult<FuncTypeRefMut<'a>>
    {
        // sketchy: working around a borrow checker limitation
        // that for some reason keeps this from working. cast self
        // to a pointer, then back to a reference and use the reference
        // instead to avoid double borrow errors
        // Explained in this rust user forum thread:
        // https://users.rust-lang.org/t/solved-borrow-doesnt-drop-returning-this-value-requires-that/24182/6
        let ft = unsafe { &mut (*(self as *mut Type)) };
        if let Some(fref) = ft.func_ref_mut() {
            return Ok(fref);
        }
        Err(lfail!(
            failure::Mode::TypeFailure,
            "type is not a function",
            "type": ldisplay!(self),
        ))
    }

    pub fn try_func_ref<'a>(&'a self) -> Lresult<FuncTypeRef<'a>>
    {
        if let Some(f) = self.func_ref() {
            Ok(f)
        } else {
            Err(rustfail!("leema_failure", "not a func type: {}", self.path))
        }
    }

    /// get the type args from this Type
    pub fn type_args(&self) -> &TypeArgSlice
    {
        if let Some(f) = self.func_ref() {
            &f.type_args
        } else {
            self.args.as_slice()
        }
    }

    pub fn generic_ref<'a>(&'a self) -> Option<TypeRef<'a>>
    {
        if let Some(f) = self.func_ref() {
            if f.type_args.is_empty() {
                None
            } else {
                Some(TypeRef(self.path.as_str(), &f.type_args))
            }
        } else if self.args.is_empty() {
            None
        } else {
            Some(self.type_ref())
        }
    }

    pub fn try_generic_ref<'a>(&'a self) -> Lresult<TypeRef<'a>>
    {
        if let Some(f) = self.func_ref() {
            if f.type_args.is_empty() {
                Err(rustfail!("leema_failure", "function is not generic")
                    .with_context(vec![
                        StrupleItem::new(
                            Lstr::Sref("result"),
                            lstrf!("{}", f.result),
                        ),
                        StrupleItem::new(
                            Lstr::Sref("args"),
                            lstrf!("{:?}", f.args),
                        ),
                    ]))
            } else {
                Ok(TypeRef(self.path.as_str(), &f.type_args))
            }
        } else if self.is_generic() {
            Ok(self.type_ref())
        } else {
            Err(rustfail!("leema_failure", "type is not generic: {}", self,))
        }
    }

    pub fn try_generic_ref_mut<'a>(&'a mut self) -> Lresult<TypeRefMut<'a>>
    {
        if self.path.as_str() == Self::PATH_TYPECALL {
            Ok(TypeRefMut(self.path.as_str(), &mut self.args[1..]))
        } else if !self.args.is_empty() {
            Ok(self.type_ref_mut())
        } else {
            Err(rustfail!("leema_failure", "type is not generic: {}", self,))
        }
    }

    pub fn path_str(&self) -> &str
    {
        self.path.as_str()
    }

    pub fn argc(&self) -> usize
    {
        self.args.len()
    }

    pub fn first_arg(&self) -> Lresult<&TypeArg>
    {
        let first = ltry!(
            self.args.first().ok_or_else(|| {
                rustfail!(
                    "leema_failure",
                    "no first argument to return",
                )
            }),
            "path": self.path.to_lstr(),
        );
        Ok(&first)
    }

    /// clone this type if it's not open
    /// return an Err if it is open
    pub fn clone_closed(&self) -> Lresult<Type>
    {
        if self.contains_open() {
            Err(rustfail!("leema_failure", "unexpected open type: {}", self,))
        } else {
            Ok(self.clone())
        }
    }

    /// apply the closed type parameters to a type call
    pub fn apply_typecall(&self, type_args: &TypeArgSlice) -> Lresult<Type>
    {
        match self.path.as_str() {
            Type::PATH_TYPECALL => {
                let (out, args) = self.args.split_at(1);
                if args.len() != type_args.len() {
                    return Err(lfail!(
                        failure::Mode::TypeFailure,
                        "function type arg mismatch",
                        "type": ldebug!(self),
                        "args": ldebug!(type_args),
                    ));
                }
                let mut result = out
                    .first()
                    .ok_or_else(|| {
                        lfail!(
                            failure::Mode::StaticLeemaFailure,
                            "empty type args for generic function",
                            "type": ldebug!(self),
                            "args": ldebug!(type_args),
                        )
                    })?
                    .v
                    .clone();
                for (k, v) in args.iter().zip(type_args.iter()) {
                    // args.push(StrupleItem::new(k.k.clone(), v.v.clone()));
                    result.replace_openvar(&k.k, &v.v);
                }
                Ok(result)
            }
            Type::PATH_FN => {
                return Err(lfail!(
                    failure::Mode::TypeFailure,
                    "cannot apply types to non-generic function",
                    "type": ldebug!(self),
                    "args": ldebug!(type_args),
                ));
            }
            _ => {
                if self.args.len() != type_args.len() {
                    return Err(lfail!(
                        failure::Mode::TypeFailure,
                        "type arg mismatch",
                        "type": ldebug!(self),
                        "args": ldebug!(type_args),
                    ));
                }
                let args = self
                    .args
                    .iter()
                    .zip(type_args.iter())
                    .map(|p| StrupleItem::new(p.0.k.clone(), p.1.v.clone()))
                    .collect();
                Ok(Type {
                    path: self.path.clone(),
                    args,
                })
            }
        }
    }

    /// replace any open type variables that are already closed
    pub fn close_generics(&mut self, type_args: &TypeArgSlice)
    {
        if self.path.as_str() == Self::PATH_OPENVAR {
            if let Some(open) = self.args.first() {
                if let Some(found) = struple::find(type_args, &open.k) {
                    *self = found.clone();
                }
            } else {
                // else this is an invalid openvar (bad)
                panic!("invalid open type variable {:?}", self);
            }
        } else {
            for a in self.args.iter_mut() {
                a.v.close_generics(type_args);
            }
        }
    }

    /// replace any open variable in this type with the new type
    pub fn localize_generics(&mut self, local_id: &str)
    {
        if self.path.as_str() == Self::PATH_OPENVAR {
            if let Some(open) = self.args.first() {
                *self = Self::local(lstrf!("{}-{}", local_id, open.k));
            } else {
                // else this is an invalid openvar (bad)
                panic!("invalid open type variable");
            }
        } else {
            for a in self.args.iter_mut() {
                a.v.localize_generics(local_id);
            }
        }
    }

    pub fn is_punctuated(name: &str) -> bool
    {
        name.chars().any(|c| c.is_ascii_punctuation() && c != '_')
    }

    // replace an open variable in this type with the new type
    pub fn replace_openvar(&mut self, id: &str, new_type: &Type)
    {
        if self.path.as_str() == Self::PATH_OPENVAR {
            if let Some(open) = self.args.first() {
                if open.k == *id {
                    *self = new_type.clone();
                }
            } // else this is bad
        } else {
            self.args.iter_mut().for_each(|a| {
                a.v.replace_openvar(id, new_type);
            })
        }
    }

    pub fn replace_localvars(&mut self, locals: &LocalTypeVars) -> Lresult<()>
    {
        if self.path.as_str() == Type::PATH_LOCAL {
            *self = {
                let var = &self.first_arg()?.k;
                if let Some(result) = locals.get(var) {
                    result.clone()
                } else {
                    return Err(lfail!(
                        failure::Mode::TypeFailure,
                        "unknown type variable",
                        "variable": var.clone(),
                        "locals": ldebug!(locals),
                    ));
                }
            };
        } else {
            for a in self.args.iter_mut() {
                a.v.replace_localvars(locals)?;
            }
        }
        Ok(())
    }

    pub fn map<Op>(&self, op: &Op) -> Lresult<Type>
    where
        Op: Fn(&Type) -> Lresult<Option<Type>>,
    {
        let m_items = struple::map_v(self.args.as_slice(), |a| {
            match op(a)? {
                Some(t2) => t2.map(op),
                None => a.map(op),
            }
        })?;
        Ok(Type {
            path: self.path.clone(),
            args: m_items,
        })
    }

    pub fn map_v<Op>(&self, op: &Op) -> Lresult<Type>
    where
        Op: Fn(&Type) -> Lresult<Type>,
    {
        let m_items = struple::map_v(self.args.as_slice(), op)?;
        Ok(Type {
            path: self.path.clone(),
            args: m_items,
        })
    }

    pub fn unwrap_name(name: &Option<Lstr>, i: usize) -> Lstr
    {
        match name {
            Some(n) => n.clone(),
            None => {
                match UNNAMED_NAMES.get(i) {
                    Some(un) => Lstr::Sref(un),
                    None => lstrf!("__unnamed_{}", i),
                }
            }
        }
    }

    /// unwrap a field name or return a new one for an unnamed field
    /// return Err if the field index is too high for the static fields
    pub fn unwrap_static_name(
        name: &Option<&'static str>,
        i: usize,
    ) -> Lresult<&'static str>
    {
        match name {
            Some(n) => Ok(n),
            None => {
                match UNNAMED_NAMES.get(i) {
                    Some(un) => Ok(un),
                    None => {
                        Err(lfail!(
                            failure::Mode::TypeFailure,
                            "unnamed name overflow",
                            "field_index": ldisplay!(i),
                        ))
                    }
                }
            }
        }
    }
}

impl From<Canonical> for Type
{
    fn from(path: Canonical) -> Type
    {
        Type { path, args: vec![] }
    }
}

impl sendclone::SendClone for Type
{
    type Item = Type;

    fn clone_for_send(&self) -> Type
    {
        Type {
            path: self.path.clone_for_send(),
            args: self.args.clone_for_send(),
        }
    }
}

impl Default for Type
{
    fn default() -> Type
    {
        Type::UNKNOWN
    }
}

impl fmt::Display for Type
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        if let Some(ft) = self.func_ref() {
            write!(f, "{}", ft)
        } else if self.is_local() {
            let first = self.first_arg().unwrap();
            write!(f, "local:{}", first.k)
        } else {
            match self.path.as_str() {
                Type::PATH_TUPLE => {
                    write!(f, "(")?;
                    for a in self.args.iter() {
                        write!(f, "{},", a)?;
                    }
                    write!(f, ")")
                }
                Type::PATH_LIST => {
                    write!(f, "[")?;
                    write!(f, "{}", self.args.first().unwrap().v)?;
                    write!(f, "]")
                }
                Type::PATH_OPENVAR => {
                    let first = self.first_arg().unwrap();
                    write!(f, "{}", first.k)
                }
                _ => {
                    if self.args.is_empty() {
                        write!(f, "{}", self.path)
                    } else {
                        write!(f, "<{}", self.path)?;
                        for a in self.args.iter() {
                            write!(f, " {}", a)?;
                        }
                        write!(f, ">")
                    }
                }
            }
        }
    }
}

impl fmt::Debug for Type
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        if self.is_local() {
            let first = self.first_arg().unwrap();
            write!(f, "local:{}", first.k)
        } else if let Some(ft) = self.func_ref() {
            if f.alternate() {
                write!(f, "{:#?}", ft)
            } else {
                write!(f, "{:?}", ft)
            }
        } else {
            match self.path.as_str() {
                Type::PATH_TUPLE => {
                    write!(f, "(T ")?;
                    for a in self.args.iter() {
                        write!(f, "{:?},", a)?;
                        if f.alternate() {
                            writeln!(f, "")?;
                        }
                    }
                    write!(f, ")")
                }
                Type::PATH_LIST => {
                    write!(f, "[")?;
                    write!(f, "{:?}", self.args.first().unwrap().v)?;
                    write!(f, "]")
                }
                Type::PATH_OPENVAR => {
                    let first = self.first_arg().unwrap();
                    write!(f, "(open {})", first.k)
                }
                _ => {
                    if self.args.is_empty() {
                        write!(f, "{}", self.path)
                    } else {
                        write!(f, "<{}", self.path)?;
                        for a in self.args.iter() {
                            if f.alternate() {
                                write!(f, " {:#?}", a)?;
                            } else {
                                write!(f, " {:?}", a)?;
                            }
                        }
                        write!(f, ">")
                    }
                }
            }
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
#[derive(PartialEq)]
#[derive(PartialOrd)]
#[derive(Eq)]
#[derive(Hash)]
#[derive(Ord)]
pub struct Fref
{
    pub m: ModKey,
    pub f: &'static str,
    pub t: TypeArgs,
}

impl Fref
{
    /// eventually make this take just TypeArgs
    pub fn new(m: ModKey, f: &'static str, t: Type) -> Fref
    {
        let targs = if let Some(ftref) = t.func_ref() {
            ftref.type_args.to_vec()
        } else {
            vec![]
        };
        Fref { m, f, t: targs }
    }

    pub fn with_modules(m: ModKey, f: &'static str) -> Fref
    {
        Fref { m, f, t: vec![] }
    }

    pub fn is_generic(&self) -> bool
    {
        !self.t.is_empty()
    }

    pub fn contains_open(&self) -> bool
    {
        self.t.iter().any(|t| t.v.contains_open())
    }

    pub fn contains_local(&self) -> bool
    {
        self.t.iter().any(|t| t.v.contains_local())
    }

    pub fn localize_generics(&mut self, local_id: &str)
    {
        for t in self.t.iter_mut() {
            t.v.localize_generics(local_id);
        }
    }

    pub fn replace_localvars(&mut self, locals: &LocalTypeVars) -> Lresult<()>
    {
        for t in self.t.iter_mut() {
            t.v.replace_localvars(locals)?;
        }
        Ok(())
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
        if self.t.is_empty() {
            write!(f, "{}.{}", self.m.name, self.f)
        } else {
            write!(f, "<{}.{}", self.m.name, self.f)?;
            for t in &self.t {
                if t.v == Type::UNKNOWN {
                    write!(f, " {}", t.k)?;
                } else {
                    write!(f, " {}", t.v)?;
                }
            }
            write!(f, ">")
        }
    }
}

impl fmt::Debug for Fref
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "({}.{}: {:?})", self.m.name, self.f, self.t)
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

/// Val objects
///
/// Can it be simplified for call types?
/// CallType | Binding | SubjectArg
/// -------------------------------
///    1     | Static  |    No
///    2     | Static  |    Yes
///    3     | Dynamic |    No
///    4     | Dynamic |    Yes
/// -------------------------------
/// Is 1 just 2 w/ Subject == Void? Same for 3 and 4?
/// Can Call just be:
/// * Struct(FuncT, [Fref, Subject])
/// * Struct(FuncT, [Fref, Binding, Subject])
/// * EnumStruct(FuncT, CallType, [Fref, Subject])
/// Arbirtray type of Subject means subjected SubjectveFuncT is Generic
/// * Fref(Fref)
/// * Struct(FuncT, [Fref])
/// * Struct(<Method T>, [Fref, T])
/// * Struct(<SubjectiveFunc T>, [Fref, T])
/////////////////////////////////////////////
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
    Func(Fref),
    Map(LmapNode),
    Failure2(Box<Failure>),
    Type(Type),
    // LibVal(Box<dyn LibVal>),
    // LibVal(Rc<dyn LibVal>),
    // LibRef(Arc<dyn LibRef>),
    Lib(Arc<dyn LibVal>),
    ResourceRef(i64),
    // can Val::Future just be a Val::Lib?
    Future(Arc<Mutex<Receiver<Val>>>),

    // Special language implementation values
    /// Pattern or if case Wildcard
    Wildcard,

    /// Register for assigning in a pattern
    Reg(Reg),
}

const NIL: Val = Val::Nil;
pub const FALSE: Val = Val::Bool(false);
pub const TRUE: Val = Val::Bool(true);

pub trait Op
{
    fn pre_ref(&mut self, _val: &Val) -> Lresult<()>
    {
        Ok(())
    }

    fn pre_ref_mut(&mut self, _val: &mut Val) -> Lresult<()>
    {
        Ok(())
    }
}

impl Val
{
    pub const FALSE: Val = Val::EnumToken(Type::BOOL, Lstr::Sref("False"));
    pub const TRUE: Val = Val::EnumToken(Type::BOOL, Lstr::Sref("True"));
    pub const VOID: Val = Val::Token(Type::VOID);
    pub const BLOCK_ABSTRACT: Val =
        Val::Token(Type::named(Type::PATH_BLOCK_ABSTRACT));
    pub const BLOCK_RUST: Val = Val::Token(Type::named(Type::PATH_BLOCK_RUST));

    pub const NOTOKEN: Val = Val::Token(Type::NOTOKEN);

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
            _ => false,
        }
    }

    /// Given a type and a path to a constructor, and some args
    /// create a Val that is a Struct or an EnumStruct
    pub fn make_struct_or_enum(
        t: &Type,
        constructor_mod: &Canonical,
        flds: Struple2<Val>,
    ) -> Lresult<Val>
    {
        if *constructor_mod == t.path {
            Ok(Val::Struct(t.clone(), flds))
        } else {
            let split_f = constructor_mod.last_module();
            if let Some(var) = split_f {
                Ok(Val::EnumStruct(t.clone(), var.clone(), flds))
            } else {
                Err(lfail!(
                    failure::Mode::StaticLeemaFailure,
                    "invalid constructor module",
                    "type": ldisplay!(t),
                    "constructor_module": constructor_mod.to_lstr(),
                ))
            }
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
                Type::list(inner)
            }
            &Val::Nil => Type::list(Type::UNKNOWN),
            &Val::Failure2(_) => Type::FAILURE,
            &Val::Type(_) => Type::KIND,
            &Val::Wildcard => Type::UNKNOWN,
            &Val::Reg(_) => Type::UNKNOWN,
            &Val::Map(_) => lmap::map_type(),
            &Val::Tuple(ref items) if items.len() == 1 => {
                items.get(0).unwrap().v.get_type()
            }
            &Val::Tuple(ref items) => {
                let tuptypes = items
                    .iter()
                    .enumerate()
                    .map(|(i, f)| {
                        StrupleItem::new(
                            Type::unwrap_name(&f.k, i),
                            f.v.get_type(),
                        )
                    })
                    .collect();
                Type::tuple(tuptypes)
            }
            &Val::Struct(ref typ, _) => typ.clone(),
            &Val::EnumStruct(ref typ, _, _) => typ.clone(),
            &Val::EnumToken(ref typ, _) => typ.clone(),
            &Val::Token(ref typ) => typ.clone(),
            &Val::Func(_) => Type::FREF,
            &Val::Buffer(_) => Type::STR,
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
            (&Val::Reg(ref dst), _) => {
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
            (&Val::Reg(ref dst), _) => {
                assigns.push((dst.clone(), input.clone()));
                true
            }
            (&Val::Nil, &Val::Nil) => true,
            _ => false,
        }
    }

    /// replace any open type variables that are already closed
    pub fn close_generics(&mut self, type_args: &TypeArgSlice)
    {
        match self {
            Val::Struct(t, flds) => {
                t.close_generics(type_args);
                for f in flds.iter_mut() {
                    f.v.close_generics(type_args);
                }
            }
            Val::Tuple(flds) => {
                for f in flds.iter_mut() {
                    f.v.close_generics(type_args);
                }
            }
            Val::EnumStruct(t, _, flds) => {
                t.close_generics(type_args);
                for f in flds.iter_mut() {
                    f.v.close_generics(type_args);
                }
            }
            Val::EnumToken(t, _) => {
                t.close_generics(type_args);
            }
            Val::Token(t) => {
                t.close_generics(type_args);
            }
            // do nothing for most types
            _ => {}
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

    pub fn walk_ref(&self, op: &mut dyn Op) -> Lresult<()>
    {
        ltry!(op.pre_ref(self));

        match self {
            &Val::Cons(ref head, ref tail) => {
                ltry!(head.walk_ref(op));
                ltry!(tail.walk_ref(op));
            }
            &Val::Tuple(ref flds) => {
                for f in flds {
                    ltry!(f.v.walk_ref(op));
                }
            }
            &Val::Struct(_, ref flds) => {
                for f in flds {
                    ltry!(f.v.walk_ref(op));
                }
            }
            &Val::EnumStruct(_, _, ref flds) => {
                for f in flds {
                    ltry!(f.v.walk_ref(op));
                }
            }
            &Val::Failure2(ref failure) => {
                ltry!(failure.tag.walk_ref(op));
                ltry!(failure.msg.walk_ref(op));
            }
            _ => {}
        }
        Ok(())
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
            &Val::Reg(_) => write!(f, ";{:?}", l),
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
            &Val::Func(ref fref) => Val::Func(fref.clone_for_send()),
            &Val::Failure2(ref f) => {
                Val::Failure2(Box::new(f.clone_for_send()))
            }
            &Val::Type(ref t) => Val::Type(t.clone_for_send()),
            &Val::ResourceRef(r) => Val::ResourceRef(r),
            // &Val::Lib(LibVal),
            &Val::Future(ref f) => Val::Future(f.clone()),
            &Val::Wildcard => Val::Wildcard,
            &Val::Reg(ref r) => Val::Reg(r.clone()),
            &Val::Buffer(ref b) => Val::Buffer(b.clone()),
            &Val::Lib(ref l) => Val::Lib(l.clone()),
            &Val::Map(_) => {
                panic!("cannot deep clone Map");
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
                    write!(f, "{},", i)?;
                }
                write!(f, ")")
            }
            Val::Struct(ref typename, ref items) => {
                write!(f, "{}(", typename)?;
                for i in items {
                    write!(f, "{},", i)?;
                }
                write!(f, ")")
            }
            Val::EnumStruct(ref tname, ref var, ref items) => {
                write!(f, "{}.{}(", tname, var)?;
                for i in items {
                    write!(f, "{},", i)?;
                }
                write!(f, ")")
            }
            Val::EnumToken(_, ref var_name) => write!(f, "{}", var_name),
            Val::Token(ref typename) => write!(f, "{}", typename),
            Val::Func(ref fref) => write!(f, "{}", fref),
            Val::Map(ref map) => write!(f, "Map({:?})", map),
            Val::Buffer(ref _buf) => write!(f, "Buffer"),
            Val::Lib(ref lv) => write!(f, "LibVal({:?})", lv),
            Val::ResourceRef(rid) => write!(f, "ResourceRef({})", rid),
            Val::Failure2(ref fail) => write!(f, "Failure({:?})", **fail),
            Val::Type(ref t) => write!(f, "{}", t),
            Val::Future(_) => write!(f, "Future"),
            Val::Reg(ref r) => write!(f, "reg:{:?}", r),
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
                if f.alternate() {
                    write!(f, "struct({}{:#?})", typ, fields)
                } else {
                    write!(f, "struct({}{:?})", typ, fields)
                }
            }
            Val::EnumStruct(ref name, ref var_name, ref val) => {
                if f.alternate() {
                    write!(f, "enum({:#?}.{}{:#?})", name, var_name, val)
                } else {
                    write!(f, "enum({:?}.{}{:?})", name, var_name, val)
                }
            }
            Val::EnumToken(ref typ, ref var_name) => {
                write!(f, "EnumToken({:?}.{:?})", typ, var_name)
            }
            Val::Token(ref name) => write!(f, "Token({:?})", name),
            Val::Func(ref fref) => write!(f, "Func({:?})", fref),
            Val::Map(ref map) => write!(f, "Map({:?})", map),
            Val::Lib(ref lv) => write!(f, "LibVal({:?})", lv),
            Val::ResourceRef(rid) => write!(f, "ResourceRef({})", rid),
            Val::Failure2(ref fail) => write!(f, "Failure({:?})", fail),
            Val::Type(ref t) => write!(f, "TypeVal({:?})", t),
            Val::Future(_) => write!(f, "Future"),
            Val::Reg(ref r) => write!(f, "reg:{:?}", r),
            Val::Wildcard => write!(f, "_Wildcard"),
        }
    }
}


impl reg::Iregistry for Val
{
    fn ireg_get(&self, i: Ireg) -> Lresult<&Val>
    {
        Ok(match (i, self) {
            // get reg on tuple
            (_, &Val::Tuple(ref items)) => {
                ltry!(items.ireg_get(i))
            }
            // get reg on struct
            (_, &Val::Struct(_, ref items)) => {
                ltry!(items.ireg_get(i))
            }
            // Failures
            (Ireg::Reg(0), &Val::Failure2(ref failure)) => &failure.tag,
            (Ireg::Reg(1), &Val::Failure2(ref failure)) => &failure.msg,
            (Ireg::Reg(2), &Val::Failure2(ref failure)) => {
                return Err(rustfail!(
                    "leema_failure",
                    "Cannot access frame trace until it is implemented as a leema value {:?}",
                    failure.trace,
                ));
            }
            _ => {
                return Err(rustfail!(
                    "leema_failure",
                    "unsupported registry value {:?}{:?}",
                    self,
                    i,
                ));
            }
        })
    }

    fn ireg_set(&mut self, i: Ireg, v: Val) -> Lresult<()>
    {
        match (i, self) {
            // set reg on tuples
            (_, &mut Val::Tuple(ref mut fields)) => fields.ireg_set(i, v),
            // set reg on structs
            (_, &mut Val::Struct(_, ref mut fields)) => fields.ireg_set(i, v),
            (_, &mut Val::EnumStruct(_, _, ref mut fields)) => {
                fields.ireg_set(i, v)
            }
            // set reg on lists
            (Ireg::Reg(0), &mut Val::Cons(ref mut head, _)) => {
                **head = v;
                Ok(())
            }
            (Ireg::Sub(0, s), &mut Val::Cons(ref mut head, _)) => {
                head.ireg_set(Ireg::Reg(s), v)
            }
            (_, &mut Val::Cons(_, _)) => {
                Err(rustfail!(
                    "leema_failure",
                    "cannot set reg within a list: {}",
                    i,
                ))
            }
            (_, &mut Val::Nil) => {
                Err(rustfail!(
                    "leema_failure",
                    "cannot set reg on empty list: {}",
                    i,
                ))
            }
            // values that can't act as registries
            (_, dst) => {
                Err(rustfail!(
                    "leema_failure",
                    "Can't ireg_set({:?}, {:?})",
                    i,
                    dst,
                ))
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
            // func to func comparison
            (&Val::Func(ref f1), &Val::Func(ref f2)) => {
                PartialOrd::partial_cmp(&*f1, &*f2)
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


#[cfg(test)]
mod tests
{
    use super::{Type, Val};
    use crate::leema::list;
    use crate::leema::lstr::Lstr;
    use crate::leema::reg::Reg;
    use crate::leema::struple::{self, StrupleItem};

    #[test]
    fn replace_openvar_func()
    {
        let a = Lstr::Sref("A");
        let b = Lstr::Sref("B");
        let init_type_args = vec![
            StrupleItem::new(a.clone(), Type::open(a.clone())),
            StrupleItem::new(b.clone(), Type::open(b.clone())),
        ];
        let init = Type::generic_f(
            init_type_args,
            Type::open(a.clone()),
            vec![StrupleItem::new(Lstr::Sref("x"), Type::open(b.clone()))],
        );
        let mut actual = init.clone();
        let expected = Type::generic_f(
            vec![
                StrupleItem::new(a.clone(), Type::INT),
                StrupleItem::new(b.clone(), Type::open(b.clone())),
            ],
            Type::INT,
            vec![StrupleItem::new(Lstr::Sref("x"), Type::open(b.clone()))],
        );
        actual.replace_openvar("A", &Type::INT);
        assert_eq!(expected, actual);
    }

    #[test]
    fn replace_openvar_struct()
    {
        let a = Lstr::Sref("A");
        let b = Lstr::Sref("B");
        let init = Type::t(
            "/app/Test",
            vec![
                StrupleItem::new(a.clone(), Type::open(a.clone())),
                StrupleItem::new(b.clone(), Type::open(b.clone())),
            ],
        );
        let mut actual = init.clone();
        let expected = Type::t(
            "/app/Test",
            vec![
                StrupleItem::new(a.clone(), Type::INT),
                StrupleItem::new(b.clone(), Type::open(b.clone())),
            ],
        );
        actual.replace_openvar("A", &Type::INT);
        assert_eq!(expected, actual);
    }

    #[test]
    fn replace_openvar_listmap()
    {
        let a = Lstr::Sref("A");
        let b = Lstr::Sref("B");
        let mut actual = Type::generic_f(
            vec![
                StrupleItem::new(a.clone(), Type::open(a.clone())),
                StrupleItem::new(b.clone(), Type::open(b.clone())),
            ],
            Type::list(Type::open(a.clone())),
            vec![
                StrupleItem::new(
                    Lstr::Sref("i"),
                    Type::list(Type::open(b.clone())),
                ),
                StrupleItem::new(
                    Lstr::Sref("f"),
                    Type::f(
                        Type::open(a.clone()),
                        vec![StrupleItem::new(
                            Lstr::Sref("i"),
                            Type::open(b.clone()),
                        )],
                    ),
                ),
            ],
        );

        let expected = Type::generic_f(
            vec![
                StrupleItem::new(a.clone(), Type::INT),
                StrupleItem::new(b.clone(), Type::open(b.clone())),
            ],
            Type::list(Type::INT),
            vec![
                StrupleItem::new(
                    Lstr::Sref("i"),
                    Type::list(Type::open(b.clone())),
                ),
                StrupleItem::new(
                    Lstr::Sref("f"),
                    Type::f(
                        Type::INT,
                        vec![StrupleItem::new(
                            Lstr::Sref("i"),
                            Type::open(b.clone()),
                        )],
                    ),
                ),
            ],
        );
        actual.replace_openvar("A", &Type::INT);
        assert_eq!(expected, actual);
    }

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
        let t = user_type!("/foo/Taco");
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
            user_type!("/foo/Burrito"),
            struple::new_tuple2(Val::Int(3), Val::Bool(false)),
        );
        let b = Val::Struct(
            user_type!("/foo/Taco"),
            struple::new_tuple2(Val::Int(3), Val::Bool(false)),
        );
        assert!(a < b);
    }

    #[test]
    fn test_struct_lt_val()
    {
        let typ = user_type!("/foo/Taco");
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
        let etype = user_type!("/animals/Animal");

        let a = Val::EnumToken(etype.clone(), Lstr::from("Dog".to_string()));
        let b = Val::EnumToken(etype.clone(), Lstr::Sref("Dog"));
        assert_eq!(a, b);
    }

    #[test]
    fn test_enum_lt_type()
    {
        let typ = user_type!("/foo/Taco");
        let a = Val::EnumToken(typ.clone(), Lstr::Sref("Quesadilla"));
        let b = Val::EnumToken(typ, Lstr::Sref("Torta"));
        assert!(a < b);
    }

    #[test]
    fn test_enum_lt_variant()
    {
        let typ = user_type!("/foo/Taco");
        let a = Val::EnumToken(typ.clone(), Lstr::Sref("Burrito"));
        let b = Val::EnumToken(typ, Lstr::Sref("Torta"));
        assert!(a < b);
    }

    #[test]
    fn test_enum_lt_val()
    {
        let typ = user_type!("/foo/Taco");
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
        let typ = user_type!("/foo/Taco");
        let s = Val::Token(typ);

        let s_str = format!("{}", s);
        assert_eq!("/foo/Taco", s_str);
    }

    #[test]
    fn test_format_enum_token()
    {
        let typ = user_type!("/foo/Taco");
        let e = Val::EnumToken(typ, Lstr::Sref("Burrito"));

        let e_str = format!("{}", e);
        assert_eq!("Burrito", e_str);
    }

    #[test]
    fn test_format_enum_namedtuple()
    {
        let burrito_str = Lstr::Sref("Burrito");
        let stype = user_type!("/tortas/Taco");
        let s = Val::EnumStruct(
            stype,
            burrito_str.clone(),
            struple::new_tuple2(Val::Int(5), Val::Int(8)),
        );

        let s_str = format!("{}", s);
        assert_eq!("/tortas/Taco.Burrito(5,8,)", s_str);
    }

    #[test]
    fn test_compare_across_types()
    {
        let f = Val::Bool(false);
        let t = Val::Bool(true);
        let i = Val::Int(7);
        let s = Val::Str(Lstr::Sref("hello"));
        let stype = user_type!("/foo/Foo");
        let strct = Val::Struct(
            stype,
            struple::new_tuple2(Val::Int(2), Val::Bool(true)),
        );
        let etype = user_type!("/foo/Taco");
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
        assert_eq!(Type::list(Type::UNKNOWN), typ);
    }

    #[test]
    fn test_get_type_int_list()
    {
        let typ = list::from2(Val::Int(3), Val::Int(8)).get_type();
        assert_eq!(Type::list(Type::INT), typ);
    }

    #[test]
    fn test_pattern_match_list_cons_wildcard_head()
    {
        let patt = list::cons(Val::Wildcard, Val::Reg(Reg::local(1)));
        let input = list::from3(Val::Int(1), Val::Int(2), Val::Int(3));
        let pmatch = Val::pattern_match(&patt, &input);
        assert!(pmatch.is_some());
    }

    #[test]
    fn test_pattern_match_list_cons_wildcard_tail()
    {
        let patt = list::cons(Val::Reg(Reg::local(1)), Val::Wildcard);
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

    #[test]
    fn test_type_args_genfunc()
    {
        let ftyp = Type::generic_f(
            vec![StrupleItem::new(Lstr::Sref("T"), Type::STR)],
            Type::open(Lstr::Sref("T")),
            vec![StrupleItem::new(
                Lstr::Sref("a"),
                Type::open(Lstr::Sref("T")),
            )],
        );
        let ftyp_args = ftyp.type_args();
        let expected = [StrupleItem::new(Lstr::Sref("T"), Type::STR)];
        assert_eq!(&expected, ftyp_args);
    }
}
