
use leema::val::{Val, Type};
use leema::log;

use std::io::{stderr, Write};
use std::rc::{Rc};


#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub enum Source
{
    Block(Vec<Iexpr>),
    BooleanAnd(Box<Iexpr>, Box<Iexpr>),
    BooleanOr(Box<Iexpr>, Box<Iexpr>),
    Call(Box<Iexpr>, Box<Iexpr>),
    Constructor(Type),
    ConstVal(Val),
    Fail(Box<Iexpr>, Box<Iexpr>),
    FieldAccess(Box<Iexpr>, Rc<String>),
    Fork(Box<Iexpr>, Box<Iexpr>, Box<Iexpr>),
    Func(Box<Iexpr>),
    Let(Val, Box<Iexpr>),
    MatchExpr(Box<Iexpr>, Box<Iexpr>),
    MatchCase(Val, Box<Iexpr>, Box<Iexpr>),
    ModuleAccess(Rc<String>, Rc<String>),
    RustBlock,
    Id(Rc<String>),
    IfExpr(Box<Iexpr>, Box<Iexpr>, Box<Iexpr>),
    List(Vec<Iexpr>),
    StrMash(Vec<Iexpr>),
    Tuple(Vec<Iexpr>),
    Return(Box<Iexpr>),
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
pub struct Iexpr
{
    pub typ: Type,
    pub src: Source,
}

impl Iexpr
{
    pub fn new(src: Source) -> Iexpr
    {
        Iexpr{
            typ: Source::type_of(&src),
            src: src,
        }
    }

    pub fn new_block(code: Vec<Iexpr>) -> Iexpr
    {
vout!("new_block> {:?}\n", code);
        let block_type = match code.last() {
            None => {
                Type::Void
            }
            Some(ix) => {
                ix.typ.clone()
            }
        };
        Iexpr{
            typ: block_type,
            src: Source::Block(code),
        }
    }

    pub fn noop() -> Iexpr
    {
        Iexpr{
            typ: Type::Void,
            src: Source::ConstVal(Val::Void),
        }
    }

    pub fn const_val(src: Val) -> Iexpr
    {
        Iexpr{
            typ: src.get_type(),
            src: Source::ConstVal(src),
        }
    }

    fn new_list(items: Vec<Iexpr>) -> Iexpr
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
        Iexpr{
            typ: Type::StrictList(Box::new(item_type)),
            src: Source::List(items),
        }
    }

    pub fn new_tuple(items: Vec<Iexpr>) -> Iexpr
    {
        let tuptyp = items.iter().map(|i| {
            i.typ.clone()
        }).collect();
        Iexpr{
            typ: Type::Tuple(tuptyp),
            src: Source::Tuple(items),
        }
    }

    pub fn new_call(f: Iexpr, args: Vec<Iexpr>) -> Iexpr
    {
        let args_tup = Iexpr::new_tuple(args);
        Iexpr{
            typ: Type::Unknown,
            src: Source::Call(Box::new(f), Box::new(args_tup)),
        }
    }

    fn constructor(t: Type) -> Iexpr
    {
        Iexpr{
            typ: t.clone(),
            src: Source::Constructor(t),
        }
    }

    fn fork(dst: Iexpr, t: Type, f: Iexpr, args: Iexpr) -> Iexpr
    {
        Iexpr{
            typ: t,
            src: Source::Fork(Box::new(dst), Box::new(f), Box::new(args)),
        }
    }

    pub fn new_match_expr(input: Iexpr, cases: Iexpr) -> Iexpr
    {
        Iexpr{
            typ: Type::Unknown,
            src: Source::MatchExpr(
                Box::new(input),
                Box::new(cases),
            ),
        }
    }

    pub fn new_match_case(pattern: Val, code: Iexpr, next: Iexpr) -> Iexpr
    {
        Iexpr{
            typ: Type::Unknown,
            src: Source::MatchCase(
                pattern,
                Box::new(code),
                Box::new(next),
            ),
        }
    }

    pub fn new_if(test: Iexpr, truth: Iexpr, lies: Iexpr) -> Iexpr
    {
        Iexpr{
            typ: truth.typ.clone(),
            src: Source::IfExpr(
                Box::new(test),
                Box::new(truth),
                Box::new(lies),
            ),
        }
    }

    pub fn new_field_access(base: Iexpr, fld: Rc<String>) -> Iexpr
    {
        Iexpr{
            typ: Type::Unknown, // new field access
            src: Source::FieldAccess(
                Box::new(base),
                fld,
            ),
        }
    }

    pub fn new_str_mash(items: Vec<Iexpr>) -> Iexpr
    {
        Iexpr{
            typ: Type::Str,
            src: Source::StrMash(items),
        }
    }

    fn tuple(items: Vec<Iexpr>) -> Iexpr
    {
        let mut types = vec![];
        for i in &items {
            types.push(i.typ.clone());
        }
        Iexpr{
            typ: Type::Tuple(types),
            src: Source::Tuple(items),
        }
    }
}


#[cfg(test)]
mod tests
{
use leema::iexpr::{Iexpr, Source};
use leema::list;
use leema::sexpr;
use leema::val::{Val, SexprType, Type};


#[test]
fn test_new_const_str()
{
    let hello = Val::new_str(String::from("hello"));
    let actual = Iexpr::const_val(hello.clone());
    let expected = Iexpr{
        src: Source::ConstVal(hello),
        typ: Type::Str,
    };
    assert_eq!(expected, actual);
}

#[test]
fn test_new_str_mash_const()
{
    let strs = vec![
        Iexpr::const_val(Val::new_str(String::from("hello"))),
        Iexpr::const_val(Val::new_str(String::from(" mash"))),
    ];
    let strmash = Iexpr::new_str_mash(strs);
    if let Source::StrMash(ss) = strmash.src {
        assert_eq!(2, ss.len());
        // println!("{:?}", ss);
    } else {
        assert_eq!("failed", "strmash pattern");
    }
}

}
