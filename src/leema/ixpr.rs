use leema::lstr::Lstr;
use leema::struple::Struple;
use leema::val::{Type, Val};

use std::collections::HashMap;


#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub enum Source
{
    Block(Vec<Ixpr>, HashMap<String, Ixpr>),
    Call(Box<Ixpr>, Box<Ixpr>),
    Cons(Box<Ixpr>, Box<Ixpr>),
    Construple(Type, Struple<Type>),
    ConstVal(Val),
    EnumConstructor(Type, i16, Box<Ixpr>),
    FieldAccess(Box<Ixpr>, i8),
    Func(Vec<Lstr>, Box<Ixpr>),
    Let(Val, Box<Ixpr>, Vec<(Lstr, Ixpr)>),
    MatchFailure(Box<Ixpr>, Box<Ixpr>),
    MatchExpr(Box<Ixpr>, Box<Ixpr>),
    MatchCase(Val, Box<Ixpr>, Box<Ixpr>),
    PropagateFailure(Lstr, i16),
    RustBlock,
    Id(Lstr, i16),
    IfExpr(Box<Ixpr>, Box<Ixpr>, Option<Box<Ixpr>>),
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
        Ixpr {
            typ: Source::type_of(&src),
            src: src,
            line: line,
        }
    }

    pub fn new_block(
        code: Vec<Ixpr>,
        fails: HashMap<String, Ixpr>,
        line: i16,
    ) -> Ixpr
    {
        let block_type = match code.last() {
            None => Type::Void,
            Some(ix) => ix.typ.clone(),
        };
        Ixpr {
            typ: block_type,
            src: Source::Block(code, fails),
            line: line,
        }
    }

    pub fn noop() -> Ixpr
    {
        Ixpr {
            typ: Type::Void,
            src: Source::ConstVal(Val::Void),
            line: 0,
        }
    }

    pub fn const_val(src: Val, lineno: i16) -> Ixpr
    {
        Ixpr {
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
        Ixpr {
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
            tuptyp.push((None, i.typ.clone()));
        }
        Ixpr {
            typ: Type::Tuple(Struple(tuptyp)),
            src: Source::Tuple(items),
            line: lineno,
        }
    }

    pub fn cons(head: Ixpr, tail: Ixpr, line: i16) -> Ixpr
    {
        Ixpr {
            typ: tail.typ.clone(),
            src: Source::Cons(Box::new(head), Box::new(tail)),
            line: line,
        }
    }

    pub fn construple(t: Type, flds: &Struple<Type>, lineno: i16) -> Ixpr
    {
        Ixpr {
            typ: t.clone(),
            src: Source::Construple(t, flds.clone()),
            line: lineno,
        }
    }

    pub fn match_failure(var: Ixpr, cases: Ixpr) -> Ixpr
    {
        let lineno = var.line;
        Ixpr {
            typ: cases.typ.clone(),
            src: Source::MatchFailure(Box::new(var), Box::new(cases)),
            line: lineno,
        }
    }

    pub fn new_match_expr(input: Ixpr, cases: Ixpr) -> Ixpr
    {
        let lineno = input.line;
        Ixpr {
            typ: cases.typ.clone(),
            src: Source::MatchExpr(Box::new(input), Box::new(cases)),
            line: lineno,
        }
    }

    pub fn new_match_case(pattern: Val, code: Ixpr, next: Ixpr) -> Ixpr
    {
        let lineno = code.line;
        Ixpr {
            typ: code.typ.clone(),
            src: Source::MatchCase(pattern, Box::new(code), Box::new(next)),
            line: lineno,
        }
    }

    pub fn new_if(
        test: Ixpr,
        truth: Ixpr,
        lies: Option<Ixpr>,
    ) -> Ixpr
    {
        let lineno = test.line;
        Ixpr {
            typ: truth.typ.clone(),
            src: Source::IfExpr(
                Box::new(test),
                Box::new(truth),
                lies.map(|l| Box::new(l)),
            ),
            line: lineno,
        }
    }

    pub fn new_field_access(base: Ixpr, fld_idx: i8, fld_typ: Type) -> Ixpr
    {
        let lineno = base.line;
        Ixpr {
            typ: fld_typ,
            src: Source::FieldAccess(Box::new(base), fld_idx),
            line: lineno,
        }
    }

    pub fn new_str_mash(items: Vec<Ixpr>, lineno: i16) -> Ixpr
    {
        Ixpr {
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
    use leema::lstr::Lstr;
    use leema::val::{Type, Val};


    #[test]
    fn test_new_const_str()
    {
        let hello = Val::Str(Lstr::Sref("hello"));
        let actual = Ixpr::const_val(hello.clone(), 7);
        let expected = Ixpr {
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
            Ixpr::const_val(Val::Str(Lstr::Sref("hello")), 16),
            Ixpr::const_val(Val::Str(Lstr::Sref(" mash")), 17),
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
