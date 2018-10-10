use leema::lstr::Lstr;
use leema::struple::Struple;
use leema::val::{Type, Val};


#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub struct MatchFailure
{
    pub var: Lstr,
    pub case: Option<Ixpr>,
    pub line: i16,
}

#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub enum Source
{
    Block(Vec<Ixpr>),
    Call(Box<Ixpr>, Struple<Ixpr>),
    Cons(Box<Ixpr>, Box<Ixpr>),
    Construple(Type, Option<Lstr>, Struple<Type>),
    ConstVal(Val),
    FieldAccess(Box<Ixpr>, Lstr, Option<i8>),
    Fork(Box<Ixpr>),
    Func(Vec<Lstr>, Vec<Lstr>, Vec<Type>, Type, Box<Ixpr>),
    Let(Val, Box<Ixpr>, Vec<MatchFailure>),
    MatchExpr(Box<Ixpr>, Box<Ixpr>),
    MatchCase(Val, Box<Ixpr>, Box<Ixpr>),
    RustBlock(Vec<Type>, Type),
    Id(Lstr, i16),
    IfExpr(Box<Ixpr>, Box<Ixpr>, Option<Box<Ixpr>>),
    List(Vec<Ixpr>),
    StrMash(Vec<Ixpr>),
    Tuple(Struple<Ixpr>),
    Map(Struple<Ixpr>),
    Return(Box<Ixpr>),
}

impl Source
{
    pub fn is_matchcase(&self) -> bool
    {
        if let Source::MatchCase(_, _, _) = self {
            true
        } else {
            false
        }
    }

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
    pub src: Source,
    pub line: i16,
}

impl Ixpr
{
    pub fn new(src: Source, line: i16) -> Ixpr
    {
        Ixpr { src, line }
    }

    pub fn new_block(code: Vec<Ixpr>, line: i16) -> Ixpr
    {
        Ixpr {
            src: Source::Block(code),
            line,
        }
    }

    pub fn noop() -> Ixpr
    {
        Ixpr {
            src: Source::ConstVal(Val::Void),
            line: 0,
        }
    }

    pub fn const_val(src: Val, lineno: i16) -> Ixpr
    {
        Ixpr {
            src: Source::ConstVal(src),
            line: lineno,
        }
    }

    pub fn new_list(items: Vec<Ixpr>, line: i16) -> Ixpr
    {
        Ixpr {
            src: Source::List(items),
            line,
        }
    }

    pub fn new_tuple(items: Struple<Ixpr>, mut lineno: i16) -> Ixpr
    {
        let mut set_lineno = false;
        for i in items.0.iter() {
            if !set_lineno {
                lineno = i.1.line;
                set_lineno = true;
            }
        }
        Ixpr {
            src: Source::Tuple(items),
            line: lineno,
        }
    }

    pub fn new_map(items: Struple<Ixpr>, lineno: i16) -> Ixpr
    {
        Ixpr {
            src: Source::Map(items),
            line: lineno,
        }
    }

    pub fn cons(head: Ixpr, tail: Ixpr, line: i16) -> Ixpr
    {
        Ixpr {
            src: Source::Cons(Box::new(head), Box::new(tail)),
            line,
        }
    }

    pub fn construple(t: Type, variant: Option<Lstr>, flds: &Struple<Type>, line: i16) -> Ixpr
    {
        let src = Source::Construple(t, variant, flds.clone());
        Ixpr { src, line }
    }

    pub fn new_match_expr(input: Ixpr, cases: Ixpr) -> Ixpr
    {
        let lineno = input.line;
        Ixpr {
            src: Source::MatchExpr(Box::new(input), Box::new(cases)),
            line: lineno,
        }
    }

    pub fn new_match_case(pattern: Val, code: Ixpr, next: Ixpr) -> Ixpr
    {
        let lineno = code.line;
        Ixpr {
            src: Source::MatchCase(pattern, Box::new(code), Box::new(next)),
            line: lineno,
        }
    }

    pub fn new_if(test: Ixpr, truth: Ixpr, lies: Option<Ixpr>) -> Ixpr
    {
        let lineno = test.line;
        Ixpr {
            src: Source::IfExpr(
                Box::new(test),
                Box::new(truth),
                lies.map(|l| Box::new(l)),
            ),
            line: lineno,
        }
    }

    pub fn new_field_access(base: Ixpr, sub: Lstr) -> Ixpr
    {
        let lineno = base.line;
        Ixpr {
            src: Source::FieldAccess(Box::new(base), sub, None),
            line: lineno,
        }
    }

    pub fn new_str_mash(items: Vec<Ixpr>, lineno: i16) -> Ixpr
    {
        Ixpr {
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
    use leema::val::Val;


    #[test]
    fn test_new_const_str()
    {
        let hello = Val::Str(Lstr::Sref("hello"));
        let actual = Ixpr::const_val(hello.clone(), 7);
        let expected = Ixpr {
            src: Source::ConstVal(hello),
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
