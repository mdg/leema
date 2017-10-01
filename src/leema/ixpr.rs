
use leema::val::{self, Val, Type};
use leema::log;

use std::io::{stderr, Write};
use std::rc::{Rc};


#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub enum Source
{
    Block(Vec<Ixpr>),
    BooleanAnd(Box<Ixpr>, Box<Ixpr>),
    BooleanOr(Box<Ixpr>, Box<Ixpr>),
    Call(val::FuncCallType, Box<Ixpr>, Box<Ixpr>),
    Constructor(Type),
    ConstVal(Val),
    Fail(Box<Ixpr>, Box<Ixpr>),
    FieldAccess(Box<Ixpr>, Rc<String>),
    Fork(Box<Ixpr>, Box<Ixpr>, Box<Ixpr>),
    Func(Vec<Rc<String>>, Box<Ixpr>),
    Let(Val, Box<Ixpr>),
    MatchExpr(Box<Ixpr>, Box<Ixpr>),
    MatchCase(Val, Box<Ixpr>, Box<Ixpr>),
    ModuleAccess(Rc<String>, Rc<String>),
    RustBlock,
    Id(Rc<String>),
    IfExpr(Box<Ixpr>, Box<Ixpr>, Box<Ixpr>),
    List(Vec<Ixpr>),
    StrMash(Vec<Ixpr>),
    Tuple(Vec<Ixpr>),
    Return(Box<Ixpr>),
}

impl Source
{
    pub fn type_of(src: &Source) -> Type
    {
        match src {
            &Source::ConstVal(ref v) => v.get_type(),
            &Source::Let(_, _) => Type::Void,
            _ => Type::Unknown,
        }
    }
}

#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub struct Ixpr
{
    pub typ: Type,
    pub src: Source,
}

impl Ixpr
{
    pub fn new(src: Source) -> Ixpr
    {
        Ixpr{
            typ: Source::type_of(&src),
            src: src,
        }
    }

    pub fn new_block(code: Vec<Ixpr>) -> Ixpr
    {
        let block_type = match code.last() {
            None => {
                Type::Void
            }
            Some(ix) => {
                ix.typ.clone()
            }
        };
        Ixpr{
            typ: block_type,
            src: Source::Block(code),
        }
    }

    pub fn noop() -> Ixpr
    {
        Ixpr{
            typ: Type::Void,
            src: Source::ConstVal(Val::Void),
        }
    }

    pub fn const_val(src: Val) -> Ixpr
    {
        Ixpr{
            typ: src.get_type(),
            src: Source::ConstVal(src),
        }
    }

    pub fn new_list(items: Vec<Ixpr>) -> Ixpr
    {
        let item_type = items.iter().fold(Type::Unknown, |old_t, new_x| {
            if old_t == Type::Unknown {
                new_x.typ.clone()
            } else if old_t == new_x.typ {
                old_t
            } else {
                panic!("Mixed list types: {:?} != {:?}", old_t, new_x);
            }
        });
        Ixpr{
            typ: Type::StrictList(Box::new(item_type)),
            src: Source::List(items),
        }
    }

    pub fn new_tuple(items: Vec<Ixpr>) -> Ixpr
    {
        let tuptyp = items.iter().map(|i| {
            i.typ.clone()
        }).collect();
        Ixpr{
            typ: Type::Tuple(tuptyp),
            src: Source::Tuple(items),
        }
    }

    fn constructor(t: Type) -> Ixpr
    {
        Ixpr{
            typ: t.clone(),
            src: Source::Constructor(t),
        }
    }

    fn fork(dst: Ixpr, t: Type, f: Ixpr, args: Ixpr) -> Ixpr
    {
        Ixpr{
            typ: t,
            src: Source::Fork(Box::new(dst), Box::new(f), Box::new(args)),
        }
    }

    pub fn new_match_expr(input: Ixpr, cases: Ixpr) -> Ixpr
    {
        Ixpr{
            typ: cases.typ.clone(),
            src: Source::MatchExpr(
                Box::new(input),
                Box::new(cases),
            ),
        }
    }

    pub fn new_match_case(pattern: Val, code: Ixpr, next: Ixpr) -> Ixpr
    {
        Ixpr{
            typ: code.typ.clone(),
            src: Source::MatchCase(
                pattern,
                Box::new(code),
                Box::new(next),
            ),
        }
    }

    pub fn new_if(test: Ixpr, truth: Ixpr, lies: Ixpr, typ: Type) -> Ixpr
    {
        Ixpr{
            typ: typ,
            src: Source::IfExpr(
                Box::new(test),
                Box::new(truth),
                Box::new(lies),
            ),
        }
    }

    pub fn new_field_access(base: Ixpr, fld: Rc<String>) -> Ixpr
    {
        Ixpr{
            typ: Type::Unknown, // new field access
            src: Source::FieldAccess(
                Box::new(base),
                fld,
            ),
        }
    }

    pub fn new_str_mash(items: Vec<Ixpr>) -> Ixpr
    {
        Ixpr{
            typ: Type::Str,
            src: Source::StrMash(items),
        }
    }
}


#[cfg(test)]
mod tests
{
use leema::ixpr::{Ixpr, Source};
use leema::list;
use leema::sxpr;
use leema::val::{Val, SxprType, Type};


#[test]
fn test_new_const_str()
{
    let hello = Val::new_str(String::from("hello"));
    let actual = Ixpr::const_val(hello.clone());
    let expected = Ixpr{
        src: Source::ConstVal(hello),
        typ: Type::Str,
    };
    assert_eq!(expected, actual);
}

#[test]
fn test_new_str_mash_const()
{
    let strs = vec![
        Ixpr::const_val(Val::new_str(String::from("hello"))),
        Ixpr::const_val(Val::new_str(String::from(" mash"))),
    ];
    let strmash = Ixpr::new_str_mash(strs);
    if let Source::StrMash(ss) = strmash.src {
        assert_eq!(2, ss.len());
        // println!("{:?}", ss);
    } else {
        assert_eq!("failed", "strmash pattern");
    }
}

}
