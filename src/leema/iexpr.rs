
use leema::val::{Val, Type};
use leema::log;

use std::io::{stderr, Write};


#[derive(Debug)]
#[derive(PartialEq)]
pub enum Source
{
    Block(Vec<Iexpr>),
    BooleanAnd(Box<Iexpr>, Box<Iexpr>),
    BooleanOr(Box<Iexpr>, Box<Iexpr>),
    ConstVal(Val),
    Call(Box<Iexpr>, Box<Iexpr>),
    Constructor(Type),
    DefFunc(Box<Iexpr>, Vec<Iexpr>, Box<Iexpr>),
    Fail(Box<Iexpr>, Box<Iexpr>),
    FieldAccess(Box<Iexpr>, i8),
    Fork(Box<Iexpr>, Box<Iexpr>, Box<Iexpr>),
    Let(Box<Iexpr>, Box<Iexpr>),
    MatchExpr(Box<Iexpr>, Box<Iexpr>),
    MatchCase(Box<Iexpr>, Box<Iexpr>, Box<Iexpr>),
    CaseExpr(Box<Iexpr>, Box<Iexpr>, Box<Iexpr>),
    IfStmt(Box<Iexpr>, Box<Iexpr>, Box<Iexpr>),
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
            _ => Type::Unknown,
        }
    }
}

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

    pub fn def_func(name: Iexpr, args: Vec<Iexpr>, body: Iexpr, ftype: Type)
        -> Iexpr
    {
        if let &Type::Func(ref defparams, ref result) = &ftype {
            if args.len() != defparams.len() {
                panic!("Inconsistent argument count");
            }
        } else {
            panic!("Invalid function type: {:?}", ftype);
        }
        Iexpr{
            typ: ftype,
            src: Source::DefFunc(Box::new(name), args, Box::new(body)),
        }
    }

    fn call(t: Type, f: Iexpr, args: Iexpr) -> Iexpr
    {
        Iexpr{
            typ: t,
            src: Source::Call(Box::new(f), Box::new(args)),
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

    fn match_expr(x: Iexpr, cases: Iexpr) -> Iexpr
    {
        Iexpr{
            typ: cases.typ.clone(),
            src: Source::MatchExpr(
                Box::new(x),
                Box::new(cases),
            ),
        }
    }

    fn match_case(pattern: Iexpr, code: Iexpr, next: Iexpr) -> Iexpr
    {
        Iexpr{
            typ: code.typ.clone(),
            src: Source::MatchCase(
                Box::new(pattern),
                Box::new(code),
                Box::new(next),
            ),
        }
    }

    fn case_expr(test: Iexpr, truth: Iexpr, lies: Iexpr) -> Iexpr
    {
        if truth.typ != lies.typ {
            panic!("Mismatched case types: {:?}!={:?}", truth.typ, lies.typ);
        }
        Iexpr{
            typ: truth.typ.clone(),
            src: Source::CaseExpr(
                Box::new(test),
                Box::new(truth),
                Box::new(lies),
            ),
        }
    }

    fn ifstmt(test: Iexpr, truth: Iexpr, lies: Iexpr) -> Iexpr
    {
        Iexpr{
            typ: Type::Void, // if statements are untyped
            src: Source::IfStmt(
                Box::new(test),
                Box::new(truth),
                Box::new(lies)
            ),
        }
    }

    fn str(items: Vec<Iexpr>) -> Iexpr
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

}
