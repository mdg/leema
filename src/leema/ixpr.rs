
use leema::val::{Val, Type};
use leema::log;

use std::collections::HashMap;
use std::io::{Write};
use std::rc::{Rc};


#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub enum Source
{
    Block(Vec<Ixpr>, HashMap<String, Ixpr>, bool),
    BooleanAnd(Box<Ixpr>, Box<Ixpr>),
    BooleanOr(Box<Ixpr>, Box<Ixpr>),
    Call(Box<Ixpr>, Box<Ixpr>),
    Constructor(Type, i8),
    ConstVal(Val),
    EnumConstructor(Type, i16, Box<Ixpr>),
    FieldAccess(Box<Ixpr>, i8),
    Fork(Box<Ixpr>, Box<Ixpr>, Box<Ixpr>),
    Func(Vec<Rc<String>>, Box<Ixpr>),
    Let(Val, Box<Ixpr>, Vec<(Rc<String>, Ixpr)>),
    MatchFailure(Box<Ixpr>, Box<Ixpr>),
    MatchExpr(Box<Ixpr>, Box<Ixpr>),
    MatchCase(Val, Box<Ixpr>, Box<Ixpr>),
    ModuleAccess(Rc<String>, Rc<String>),
    PropagateFailure(Rc<String>, i16),
    RustBlock,
    Id(Rc<String>, i16),
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
            &Source::Let(_, _, _) => Type::Void,
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
    pub line: i16,
}

impl Ixpr
{
    pub fn new(src: Source, line: i16) -> Ixpr
    {
        Ixpr{
            typ: Source::type_of(&src),
            src: src,
            line: line,
        }
    }

    pub fn new_block(code: Vec<Ixpr>, fails: HashMap<String, Ixpr>
        , is_root: bool, line: i16
        ) -> Ixpr
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
            src: Source::Block(code, fails, is_root),
            line: line,
        }
    }

    pub fn noop() -> Ixpr
    {
        Ixpr{
            typ: Type::Void,
            src: Source::ConstVal(Val::Void),
            line: 0,
        }
    }

    pub fn const_val(src: Val, lineno: i16) -> Ixpr
    {
        Ixpr{
            typ: src.get_type(),
            src: Source::ConstVal(src),
            line: lineno,
        }
    }

    pub fn new_list(items: Vec<Ixpr>, line: i16) -> Ixpr
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
            line: line,
        }
    }

    pub fn new_tuple(items: Vec<Ixpr>, mut lineno: i16) -> Ixpr
    {
        let mut set_lineno = false;
        let mut tuptyp = Vec::with_capacity(items.len());
        for i in items.iter() {
            if !set_lineno {
                lineno = i.line;
                set_lineno = true;
            }
            tuptyp.push(i.typ.clone());
        }
        Ixpr{
            typ: Type::Tuple(tuptyp),
            src: Source::Tuple(items),
            line: lineno,
        }
    }

    pub fn constructor(t: Type, nflds: i8, lineno: i16) -> Ixpr
    {
        Ixpr{
            typ: t.clone(),
            src: Source::Constructor(t, nflds),
            line: lineno,
        }
    }

    pub fn enum_constructor(t: Type, idx: i16, varname: &Rc<String>
        , vval: &Val, lineno: i16
        ) -> Ixpr
    {
        let (variant_type, nflds) =
            match *vval {
                Val::Struct(ref vartyp, ref flds) => {
                    (vartyp.clone(), flds.len() as i8)
                }
                Val::NamedTuple(ref vartyp, ref flds) => {
                    (vartyp.clone(), flds.len() as i8)
                }
                _ => {
                    panic!("unknown val for enum: {:?}", vval);
                }
            };
        let construct = Ixpr::constructor(
            variant_type, nflds, lineno
        );
        let src = Source::EnumConstructor(t.clone(), idx, Box::new(construct));

        Ixpr{
            typ: t,
            src: src,
            line: lineno,
        }
    }

    fn fork(dst: Ixpr, t: Type, f: Ixpr, args: Ixpr) -> Ixpr
    {
        let lineno = dst.line;
        Ixpr{
            typ: t,
            src: Source::Fork(Box::new(dst), Box::new(f), Box::new(args)),
            line: lineno,
        }
    }

    pub fn match_failure(var: Ixpr, cases: Ixpr) -> Ixpr
    {
        let lineno = var.line;
        Ixpr{
            typ: cases.typ.clone(),
            src: Source::MatchFailure(
                Box::new(var),
                Box::new(cases),
            ),
            line: lineno,
        }
    }

    pub fn new_match_expr(input: Ixpr, cases: Ixpr) -> Ixpr
    {
        let lineno = input.line;
        Ixpr{
            typ: cases.typ.clone(),
            src: Source::MatchExpr(
                Box::new(input),
                Box::new(cases),
            ),
            line: lineno,
        }
    }

    pub fn new_match_case(pattern: Val, code: Ixpr, next: Ixpr) -> Ixpr
    {
        let lineno = code.line;
        Ixpr{
            typ: code.typ.clone(),
            src: Source::MatchCase(
                pattern,
                Box::new(code),
                Box::new(next),
            ),
            line: lineno,
        }
    }

    pub fn new_if(test: Ixpr, truth: Ixpr, lies: Ixpr, typ: Type) -> Ixpr
    {
        let lineno = test.line;
        Ixpr{
            typ: typ,
            src: Source::IfExpr(
                Box::new(test),
                Box::new(truth),
                Box::new(lies),
            ),
            line: lineno,
        }
    }

    pub fn new_field_access(base: Ixpr, fld_idx: i8, fld_typ: Type) -> Ixpr
    {
        let lineno = base.line;
        Ixpr{
            typ: fld_typ,
            src: Source::FieldAccess(
                Box::new(base),
                fld_idx,
            ),
            line: lineno,
        }
    }

    pub fn new_str_mash(items: Vec<Ixpr>, lineno: i16) -> Ixpr
    {
        Ixpr{
            typ: Type::Str,
            src: Source::StrMash(items),
            line: lineno,
        }
    }
}


#[cfg(test)]
mod tests
{
use leema::ixpr::{Ixpr, Source};
use leema::val::{Val, Type};


#[test]
fn test_new_const_str()
{
    let hello = Val::new_str(String::from("hello"));
    let actual = Ixpr::const_val(hello.clone(), 7);
    let expected = Ixpr{
        src: Source::ConstVal(hello),
        typ: Type::Str,
        line: 7,
    };
    assert_eq!(expected, actual);
}

#[test]
fn test_new_str_mash_const()
{
    let strs = vec![
        Ixpr::const_val(Val::new_str(String::from("hello")), 16),
        Ixpr::const_val(Val::new_str(String::from(" mash")), 17),
    ];
    let strmash = Ixpr::new_str_mash(strs, 18);
    if let Source::StrMash(ss) = strmash.src {
        assert_eq!(2, ss.len());
        // println!("{:?}", ss);
    } else {
        assert_eq!("failed", "strmash pattern");
    }
}

}
