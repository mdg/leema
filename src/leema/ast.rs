use leema::lri::Lri;
use leema::lstr::Lstr;
use leema::parse::{Parser, Token};
use leema::struple::{Struple2, StrupleItem};
use leema::val::{FuncType, SrcLoc, Type};

use std::collections::LinkedList;
use std::fmt;

/*
#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub enum FuncClass {
    Pure,
    //Query,
    //Cmd,
    //IO,
    //Sys,
}
*/


#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub struct TokenData<T>
{
    pub data: T,
    pub loc: SrcLoc,
}

impl<T> TokenData<T>
{
    pub fn new(d: T, tl: SrcLoc) -> TokenData<T>
    {
        TokenData { data: d, loc: tl }
    }
}


#[derive(Clone)]
#[derive(Copy)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
pub enum DataType
{
    Enum,
    Struple,
}

#[derive(Clone)]
#[derive(Copy)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
pub enum FuncClass
{
    Macro,
    Func,
    Closure,
}

#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub struct FuncDecl
{
    pub name: Ast,
    pub args: LinkedList<Kxpr>,
    pub result: Ast,
    pub loc: SrcLoc,
}

impl fmt::Display for FuncDecl
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "{}(", self.name)?;
        for a in &self.args {
            write!(f, "{},", a.x_ref().unwrap())?;
        }
        write!(f, "):{}", self.result)
    }
}

#[derive(Clone)]
#[derive(Copy)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
pub enum IfType
{
    If,
    Match,
    MatchFailure,
    MatchFunc,
    TypeCast,
}

#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub struct IfCase
{
    pub cond: Ast,
    pub body: Ast,
    pub else_case: Option<Box<IfCase>>,
    pub loc: SrcLoc,
}

impl IfCase
{
    pub fn new(
        cond: Ast,
        body: Ast,
        else_case: Option<IfCase>,
        loc: SrcLoc,
    ) -> IfCase
    {
        IfCase {
            cond,
            body,
            else_case: else_case.map(|ic| Box::new(ic)),
            loc,
        }
    }
}

#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub struct Kxpr
{
    pub k: Option<Lstr>,
    pub x: Option<Box<Ast>>,
}

pub type KxprList = LinkedList<Kxpr>;

impl Kxpr
{
    pub fn new(k: Lstr, x: Ast) -> Kxpr
    {
        Kxpr {
            k: Some(k),
            x: Some(Box::new(x)),
        }
    }

    pub fn new_k(k: Lstr) -> Kxpr
    {
        Kxpr {
            k: Some(k),
            x: None,
        }
    }

    pub fn new_x(x: Ast) -> Kxpr
    {
        Kxpr {
            k: None,
            x: Some(Box::new(x)),
        }
    }

    pub fn k_ref(&self) -> Option<&Lstr>
    {
        self.k.as_ref()
    }

    pub fn x_ref(&self) -> Option<&Ast>
    {
        self.x.as_ref().map(|x| &**x)
    }

    pub fn k_clone(&self) -> Option<Lstr>
    {
        self.k.clone()
    }

    pub fn x_clone(&self) -> Option<Ast>
    {
        self.x.as_ref().map(|ref x| (***x).clone())
    }

    pub fn map_x<F>(&self, op: F) -> Kxpr
    where
        F: FnOnce(&Ast) -> Ast,
    {
        let new_x = self.x.as_ref().map(|bx| Box::new(op(&**bx)));
        Kxpr {
            k: self.k.clone(),
            x: new_x,
        }
    }

    pub fn map_1<F, T>(&self, op: F) -> (Option<Lstr>, T)
    where
        F: FnOnce(&Ast) -> T,
    {
        let x_u = self.x.as_ref().unwrap();
        let x_new = op(x_u);
        (self.k.clone(), x_new)
    }
}

impl fmt::Display for Kxpr
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match (&self.k, &self.x) {
            (&Some(ref ik), &Some(ref ix)) => write!(f, "{}:{}", ik, ix),
            (&None, &Some(ref ix)) => write!(f, "{}", ix),
            (&Some(ref ik), &None) => write!(f, "{}", ik),
            (None, None) => {
                panic!("cannot format kxpr w/ no values");
            }
        }
    }
}


#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub enum Ast
{
    Block(Vec<Ast>),
    Call(Box<Ast>, LinkedList<Kxpr>, SrcLoc),
    Cons(Box<Ast>, Box<Ast>),
    ConstructData(Lri, Option<Lstr>),
    ConstBool(bool),
    ConstHashtag(Lstr),
    ConstInt(i64),
    ConstStr(Lstr),
    ConstVoid,
    DefData(DataType, Box<Ast>, LinkedList<Kxpr>, SrcLoc),
    DefFunc(FuncClass, Box<FuncDecl>, Box<Ast>),
    // dereference another expression
    Deref(Box<Ast>),
    DotAccess(Box<Ast>, Lstr),
    IfExpr(IfType, Box<Ast>, Box<IfCase>, SrcLoc),
    Import(Box<Ast>, SrcLoc),
    Let(Box<Ast>, Box<Ast>, SrcLoc),
    List(LinkedList<Ast>),
    Localid(Lstr, SrcLoc),
    LocalGeneric(Lstr, KxprList, SrcLoc),
    Modid(Lstr, Lstr, SrcLoc),
    Question,
    Return(Box<Ast>, SrcLoc),
    RustBlock,
    StrExpr(Vec<Ast>, SrcLoc),
    Tuple(LinkedList<Kxpr>),
    TypeCall(Box<Ast>, KxprList, SrcLoc),
    TypeAnon,
    TypeBool,
    TypeFailure,
    TypeFunc(Vec<Kxpr>, Box<Ast>, SrcLoc),
    TypeInt,
    TypeHashtag,
    TypeStr,
    TypeVar(Lstr, SrcLoc),
    TypeVoid,
    Wildcard,
    /*
    LessThan3(Box<Ast>, Box<Ast>, Box<Ast>),
    */
}

impl Ast
{
    pub fn binaryop(
        module: &'static str,
        id: &'static str,
        a: Ast,
        b: Ast,
        loc: SrcLoc,
    ) -> Ast
    {
        let name_lri = Ast::Modid(Lstr::Sref(module), Lstr::Sref(id), loc);
        let mut args = LinkedList::new();
        args.push_back(Kxpr::new_x(a));
        args.push_back(Kxpr::new_x(b));
        Ast::Call(Box::new(name_lri), args, loc)
    }

    pub fn type_call(
        module: &'static str,
        local: &'static str,
        typs: Vec<Ast>,
        loc: SrcLoc,
    ) -> Ast
    {
        let mods = Ast::Modid(Lstr::Sref(module), Lstr::Sref(local), loc);
        let tparams = typs.iter().map(|t| Kxpr::new_x(t.clone())).collect();
        Ast::TypeCall(Box::new(mods), tparams, loc)
    }

    pub fn from_lri(l: Lri, loc: &SrcLoc) -> Ast
    {
        let base = match l.modules {
            Some(mods) => Ast::Modid(mods, l.localid, *loc),
            None => Ast::Localid(l.localid, *loc),
        };

        match l.params {
            Some(params) => {
                let kx_params = params
                    .into_iter()
                    .map(|p| Kxpr::new_x(Ast::from_type(p, loc)))
                    .collect();
                Ast::TypeCall(Box::new(base), kx_params, *loc)
            }
            None => base,
        }
    }

    pub fn from_type(t: Type, loc: &SrcLoc) -> Ast
    {
        match t {
            Type::UserDef(lri) => Ast::from_lri(lri, loc),
            Type::Bool => Ast::TypeBool,
            Type::Int => Ast::TypeInt,
            Type::Str => Ast::TypeStr,
            Type::Hashtag => Ast::TypeHashtag,
            Type::Void => Ast::TypeVoid,
            Type::Tuple(items) => {
                let new_items = items
                    .0
                    .into_iter()
                    .map(|it| {
                        let newt = Ast::from_type(it.1, loc);
                        if it.0.is_some() {
                            Kxpr::new(it.0.unwrap(), newt)
                        } else {
                            Kxpr::new_x(newt)
                        }
                    })
                    .collect();
                Ast::Tuple(new_items)
            }
            Type::Func(ftype) => {
                let kx_result = Ast::from_type(*ftype.result, loc);
                let new_params: Vec<Kxpr> = if ftype.args.is_empty() {
                    vec![]
                } else {
                    ftype.args.0
                        .into_iter()
                        .map(|it| {
                            let newt = Ast::from_type(it.v, loc);
                            Kxpr {
                                k: it.k,
                                x: Some(Box::new(newt)),
                            }
                        })
                        .collect()
                };
                Ast::TypeFunc(new_params, Box::new(kx_result), *loc)
            }
            Type::StrictList(inner) => {
                let mut items = LinkedList::new();
                items.push_back(Ast::from_type(*inner, loc));
                Ast::List(items)
            }
            Type::Var(name) => Ast::TypeVar(name, *loc),
            Type::Unknown => Ast::TypeAnon,
            _ => {
                panic!("cannot convert from type to ast");
            }
        }
    }

    pub fn matchfunc_body(cases: IfCase, loc: SrcLoc) -> Ast
    {
        let test = Box::new(Ast::ConstVoid);
        Ast::IfExpr(IfType::MatchFunc, test, Box::new(cases), loc)
    }

    pub fn closure(args: LinkedList<Kxpr>, body: Ast, loc: SrcLoc) -> Ast
    {
        let decl = FuncDecl {
            name: Ast::ConstVoid,
            args,
            result: Ast::TypeAnon,
            loc,
        };
        Ast::DefFunc(FuncClass::Closure, Box::new(decl), Box::new(body))
    }

    pub fn localid_str(&self) -> &Lstr
    {
        match self {
            &Ast::Localid(ref name, _) => name,
            _ => {
                panic!("not a localid: {:?}", self);
            }
        }
    }

    pub fn loc(&self) -> &SrcLoc
    {
        match self {
            &Ast::Localid(_, ref loc) => loc,
            &Ast::Modid(_, _, ref loc) => loc,
            &Ast::TypeCall(_, _, ref loc) => loc,
            _ => {
                panic!("cannot find SrcLoc for: {:?}", self);
            }
        }
    }

    pub fn inner_vec(&self) -> &Vec<Ast>
    {
        match self {
            &Ast::Block(ref items) => items,
            _ => {
                panic!("does not have inner vec: {:?}", self);
            }
        }
    }
}

impl<'a> From<&'a Ast> for Lstr
{
    fn from(a: &'a Ast) -> Lstr
    {
        match a {
            &Ast::Localid(ref ls, _) => ls.clone(),
            _ => {
                panic!("cannot convert to string: {:?}", a);
            }
        }
    }
}

impl<'a> From<&'a Ast> for Lri
{
    fn from(a: &'a Ast) -> Lri
    {
        match a {
            &Ast::Localid(ref id, _) => Lri::new(id.clone()),
            &Ast::Modid(ref mods, ref localid, _) => {
                Lri::with_modules(mods.clone(), localid.clone())
            }
            &Ast::TypeCall(ref base, ref types, _) => {
                let lri_base = match **base {
                    Ast::Localid(ref id, _) => Lri::new(id.clone()),
                    Ast::Modid(ref mods, ref localid, _) => {
                        Lri::with_modules(mods.clone(), localid.clone())
                    }
                    _ => {
                        panic!("unknown base: {:?}", base);
                    }
                };
                let param_array = types
                    .iter()
                    .map(|p| Type::from(p.x_ref().unwrap()))
                    .collect();
                lri_base.replace_params(param_array)
            }
            _ => {
                panic!("cannot convert Ast to Lri: {:?}", a);
            }
        }
    }
}

impl<'a> From<&'a Ast> for Type
{
    fn from(a: &'a Ast) -> Type
    {
        match a {
            &Ast::TypeAnon => Type::Unknown,
            &Ast::TypeInt => Type::Int,
            &Ast::TypeBool => Type::Bool,
            &Ast::TypeFailure => Type::Failure,
            &Ast::TypeHashtag => Type::Hashtag,
            &Ast::TypeStr => Type::Str,
            &Ast::TypeVar(ref v, _) => Type::Var(v.clone()),
            &Ast::TypeVoid => Type::Void,
            &Ast::TypeFunc(ref parts, ref result, _) => {
                let mut ppp: Struple2<Type> = parts
                    .iter()
                    .map(|p| {
                        let pt = Type::from(p.x_ref().unwrap());
                        StrupleItem::new(p.k_clone(), pt)
                    })
                    .collect();
                let result_type = Type::from(&**result);
                let functype = FuncType::new(ppp, result_type);
                Type::Func(functype)
            }
            &Ast::List(ref items) => {
                match items.len() {
                    1 => {
                        let inner_item = items.front().unwrap();
                        let inner_type = Type::from(inner_item);
                        Type::StrictList(Box::new(inner_type))
                    }
                    0 => {
                        panic!("dunno what to do with an empty type list");
                    }
                    _ => {
                        panic!("list types can have only 1 type");
                    }
                }
            }
            &Ast::Tuple(ref items) => {
                let pp_items = items
                    .iter()
                    .map(|i| {
                        let new_k = i.k_ref().map(|kr| kr.clone());
                        (new_k, Type::from(i.x_ref().unwrap()))
                    })
                    .collect();
                Type::Tuple(pp_items)
            }
            &Ast::TypeCall(_, _, _) => Type::UserDef(Lri::from(a)),
            &Ast::Localid(ref id, _) => Type::UserDef(Lri::new(id.clone())),
            &Ast::Modid(ref mods, ref id, _) => {
                Type::UserDef(Lri::with_modules(mods.clone(), id.clone()))
            }
            _ => {
                panic!("cannot convert Ast to Type: {:?}", a);
            }
        }
    }
}

impl fmt::Display for Ast
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self {
            &Ast::Block(ref items) => {
                writeln!(f, "block(")?;
                for i in items {
                    writeln!(f, "\t{}", i)?;
                }
                writeln!(f, ")")
            }
            &Ast::Call(ref callx, ref args, _) => {
                write!(f, "(call {} [", callx)?;
                for a in args {
                    write!(f, "{},", a.x_ref().unwrap())?;
                }
                write!(f, "])")
            }
            &Ast::DotAccess(ref base, ref fld) => {
                write!(f, "({} . {})", base, fld)
            }
            &Ast::Let(ref lhs, ref rhs, _) => {
                write!(f, "(let {} = {})", lhs, rhs)
            }
            &Ast::Localid(ref id, _) => write!(f, "{}", id),
            &Ast::Modid(ref mods, ref id, _) => write!(f, "{}::{}", mods, id),
            &Ast::TypeCall(ref base, ref types, _) => {
                write!(f, "{}[", base)?;
                for t in types.iter() {
                    write!(f, "{},", t)?;
                }
                write!(f, "]")
            }
            &Ast::DefFunc(ft, ref decl, ref body) => {
                writeln!(f, "({:?} {}", ft, decl)?;
                writeln!(f, "\t{})", body)
            }
            &Ast::TypeFunc(ref args, ref result, _) => {
                write!(f, "(Fn ")?;
                for a in args {
                    write!(f, "{},", a)?;
                }
                write!(f, "): {})", result)
            }
            _ => write!(f, "{:?}", self),
        }
    }
}


pub fn parse(toks: Vec<Token>) -> Ast
{
    let e = Err(0);
    let mut p = Parser::new((e, Ast::ConstVoid));
    for t in toks {
        p.parse(t);
    }
    p.parse(Token::EOI);
    p.into_extra().0.unwrap()
}


#[cfg(test)]
mod tests
{
    use leema::ast::{self, Ast, Kxpr, Type};
    use leema::lex::lex;
    use leema::lri::Lri;
    use leema::lstr::Lstr;
    use leema::val::SrcLoc;

    use std::collections::LinkedList;


    fn test_localid(id: &'static str, line: i16, col: i8) -> Ast
    {
        Ast::Localid(Lstr::from(id), SrcLoc::new(line, col))
    }

    fn test_modid(
        m: &'static str,
        item: &'static str,
        line: i16,
        col: i8,
    ) -> Ast
    {
        Ast::Modid(Lstr::from(m), Lstr::from(item), SrcLoc::new(line, col))
    }

    #[test]
    fn test_from_lri_local_only()
    {
        let lri = Lri::new(Lstr::from("Tacos"));
        let ast = Ast::from_lri(lri.clone(), &SrcLoc::default());
        assert_eq!(lri, Lri::from(&ast));
    }

    #[test]
    fn test_from_lri_with_modules()
    {
        let i = Lri::with_modules(Lstr::from("burritos"), Lstr::from("Tacos"));
        let ast = Ast::from_lri(i.clone(), &SrcLoc::default());
        assert_eq!(i, Lri::from(&ast));
    }

    #[test]
    fn test_from_lri_with_params()
    {
        let inner_lri = Lri::full(
            Some(Lstr::from("option")),
            Lstr::from("T"),
            Some(vec![Type::Var(Lstr::from("U"))]),
        );
        let params = vec![
            Type::Int,
            Type::Var(Lstr::from("A")),
            Type::UserDef(inner_lri),
        ];
        let i = Lri::full(None, Lstr::from("Tacos"), Some(params));
        let ast = Ast::from_lri(i.clone(), &SrcLoc::default());
        assert_eq!(i, Lri::from(&ast));
    }

    #[test]
    fn test_from_lri_full()
    {
        let params = vec![Type::Var(Lstr::from("A"))];
        let i = Lri::full(
            Some(Lstr::from("burritos")),
            Lstr::from("Tacos"),
            Some(params),
        );
        let ast = Ast::from_lri(i.clone(), &SrcLoc::default());
        assert_eq!(i, Lri::from(&ast));
    }

    #[test]
    fn test_ast_parse_plus()
    {
        let input = "5 + 3\n";
        let lexed = lex(input);
        let root = ast::parse(lexed);

        let expected = Ast::Block(vec![Ast::Call(
            Box::new(test_modid("prefab", "int_add", 1, 2)),
            vec![Kxpr::new_x(Ast::ConstInt(5)), Kxpr::new_x(Ast::ConstInt(3))]
                .into_iter()
                .collect(),
            SrcLoc::new(1, 2),
        )]);
        assert_eq!(expected, root);
    }

    #[test]
    fn test_ast_parse_strlit()
    {
        let input = "\"taco\"\n";
        let root = ast::parse(lex(input));

        let expected = Ast::Block(vec![Ast::ConstStr(Lstr::from("taco"))]);
        assert_eq!(expected, root);
    }

    #[test]
    fn test_ast_parse_string_id()
    {
        let input = "\"$var\"\n";
        let root = ast::parse(lex(input));

        let expected = Ast::Block(vec![Ast::StrExpr(
            vec![Ast::Localid(Lstr::from("var"), SrcLoc::new(1, 3))],
            SrcLoc::new(1, 1),
        )]);
        assert_eq!(expected, root);
    }

    #[test]
    fn test_ast_parse_string_list()
    {
        let input = "\"hello $name\n\"\n";
        let root = ast::parse(lex(input));

        let part1 = Ast::ConstStr(Lstr::from("hello "));
        let part2 = Ast::Localid(Lstr::from("name"), SrcLoc::new(1, 9));
        let part3 = Ast::ConstStr(Lstr::from("\n"));
        let expected = Ast::Block(vec![Ast::StrExpr(
            vec![part1, part2, part3],
            SrcLoc::new(1, 1),
        )]);
        assert_eq!(expected, root);
    }

    #[test]
    fn test_ast_parse_plus_twice()
    {
        let input = "5 + 3 + 2\n";
        let root = ast::parse(lex(input));

        let inner = Ast::Call(
            Box::new(test_modid("prefab", "int_add", 1, 2)),
            vec![Kxpr::new_x(Ast::ConstInt(5)), Kxpr::new_x(Ast::ConstInt(3))]
                .into_iter()
                .collect(),
            SrcLoc::new(1, 2),
        );
        let outer = Ast::Call(
            Box::new(test_modid("prefab", "int_add", 1, 4)),
            vec![Kxpr::new_x(inner), Kxpr::new_x(Ast::ConstInt(2))]
                .into_iter()
                .collect(),
            SrcLoc::new(1, 4),
        );

        let expected = Ast::Block(vec![outer]);
        assert_eq!(expected, root);
    }

    #[test]
    fn test_ast_parse_call_one_param()
    {
        let input = "inc(~4)\n";
        let root = ast::parse(lex(input));

        let neg4 = Ast::Call(
            Box::new(test_modid("prefab", "int_negate", 1, 5)),
            vec![Kxpr::new_x(Ast::ConstInt(4))].into_iter().collect(),
            SrcLoc::new(1, 5),
        );
        let expected = Ast::Block(vec![Ast::Call(
            Box::new(Ast::Localid(Lstr::from("inc"), SrcLoc::new(1, 1))),
            vec![Kxpr::new_x(neg4)].into_iter().collect(),
            SrcLoc::new(1, 4),
        )]);
        assert_eq!(expected, root);
    }

    #[test]
    fn test_ast_parse_function_call()
    {
        let input = "foo(7, 2)\n";
        let root = ast::parse(lex(input));

        let xargs =
            vec![Kxpr::new_x(Ast::ConstInt(7)), Kxpr::new_x(Ast::ConstInt(2))]
                .into_iter()
                .collect();
        let expected = Ast::Block(vec![Ast::Call(
            Box::new(Ast::Localid(Lstr::from("foo"), SrcLoc::new(1, 1))),
            xargs,
            SrcLoc::new(1, 4),
        )]);
        assert_eq!(expected, root);
    }

    #[test]
    fn test_ast_parse_tuple()
    {
        let input = "(3, \"taco\", true)\n";
        let root = ast::parse(lex(input));

        let xtup = Ast::Tuple(
            vec![
                Kxpr::new_x(Ast::ConstInt(3)),
                Kxpr::new_x(Ast::ConstStr(Lstr::from("taco"))),
                Kxpr::new_x(Ast::ConstBool(true)),
            ]
            .into_iter()
            .collect(),
        );
        let expected = Ast::Block(vec![xtup]);
        assert_eq!(expected, root);
    }

    #[test]
    fn test_ast_parse_list_empty()
    {
        let input = "[]\n";
        let root = ast::parse(lex(input));

        let xlist = Ast::List(vec![].into_iter().collect());
        let expected = Ast::Block(vec![xlist]);
        assert_eq!(expected, root);
    }

    #[test]
    fn test_ast_parse_closure_with_mult()
    {
        let input = "fn(x) x * 3\n";
        let root = ast::parse(lex(input));

        let mut cargs = LinkedList::new();
        cargs.push_back(Kxpr::new_k(Lstr::Sref("x")));
        let multri = Ast::Modid(
            Lstr::Sref("prefab"),
            Lstr::Sref("int_mult"),
            SrcLoc::new(1, 7),
        );
        let mut mult_args = LinkedList::new();
        mult_args.push_back(Kxpr::new_x(Ast::Localid(
            Lstr::Sref("x"),
            SrcLoc::new(1, 6),
        )));
        mult_args.push_back(Kxpr::new_x(Ast::ConstInt(3)));
        let mult_call =
            Ast::Call(Box::new(multri), mult_args, SrcLoc::new(1, 7));
        let clos = Ast::closure(cargs, mult_call, SrcLoc::new(1, 1));
        let expected = Ast::Block(vec![clos]);
        assert_eq!(expected, root);
    }

    #[test]
    fn test_ast_parse_const_list()
    {
        let input = "[1, 2, x]\n";
        let root = ast::parse(lex(input));

        let xlist = Ast::List(
            vec![
                Ast::ConstInt(1),
                Ast::ConstInt(2),
                Ast::Localid(Lstr::from("x"), SrcLoc::new(1, 6)),
            ]
            .into_iter()
            .collect(),
        );
        let expected = Ast::Block(vec![xlist]);
        assert_eq!(expected, root);
    }

    #[test]
    fn test_ast_parse_list_cons()
    {
        let input = "1;2;x\n";
        let root = ast::parse(lex(input));

        let inner = Ast::Cons(
            Box::new(Ast::ConstInt(2)),
            Box::new(Ast::Localid(Lstr::from("x"), SrcLoc::new(1, 5))),
        );
        let outer = Ast::Cons(Box::new(Ast::ConstInt(1)), Box::new(inner));

        let expected = Ast::Block(vec![outer]);
        assert_eq!(expected, root);
    }

    #[test]
    fn test_call_function_plus_comma()
    {
        let input = "
            func main() ->
                foo(x+1, 40)
            --
            ";
        let root = ast::parse(lex(input));
        if let Ast::Block(items) = root {
            assert_eq!(1, items.len());
        } else {
            panic!("func def is not a block");
        }
    }

    #[test]
    fn test_struct_style_func()
    {
        let input = "
            func tacos: Bool
            .filling: Str
            .number: Int
            >>
                foo(x+1, 40)
            --
            ";
        let root = ast::parse(lex(input));
        let funcdef = if let &Ast::Block(ref block_items) = &root {
            &block_items[0]
        } else {
            panic!("func def is not a block");
        };
        if let Ast::DefFunc(fc, _, _) = funcdef {
            assert_eq!(ast::FuncClass::Func, *fc);
        } else {
            panic!("func def is not a func");
        };
    }

    #[test]
    fn test_struct_style_matchfunc()
    {
        let input = "
            func fact: Int
            .i: Int
            >>
            |1 -> 1
            |n -> n * fact(n-1)
            --
            ";
        let root = ast::parse(lex(input));
        let funcdef = if let &Ast::Block(ref block_items) = &root {
            &block_items[0]
        } else {
            panic!("func def is not a block");
        };
        if let Ast::DefFunc(fc, _, _) = funcdef {
            assert_eq!(ast::FuncClass::Func, *fc);
        } else {
            panic!("func def is not a func");
        };
    }

    #[test]
    fn test_parse_empty_tuple()
    {
        let input = "()";
        let root = ast::parse(lex(input));

        let xtuple = Ast::Tuple(LinkedList::new());
        let expected = Ast::Block(vec![xtuple]);
        assert_eq!(expected, root);
    }

    #[test]
    fn test_parse_one_tuple()
    {
        let input = "(5)";
        let root = ast::parse(lex(input));

        let tuple_items =
            vec![Kxpr::new_x(Ast::ConstInt(5))].into_iter().collect();
        let expected = Ast::Block(vec![Ast::Tuple(tuple_items)]);

        assert_eq!(expected, root);
    }

    #[test]
    fn test_parse_match_empty_list()
    {
        let input = "
            func is_empty(l) >>
            |[] -> true
            |_ -> false
            --
            ";
        ast::parse(lex(input));

        // didn't crash
        assert_eq!(2, 2);
    }

    #[test]
    fn test_call_function_comma_plus()
    {
        let input = "
    func main() ->
        foo(40, x+1)
    --
    ";
        ast::parse(lex(input));

        // didn't crash
        assert_eq!(3, 3);
    }

    #[test]
    fn test_parse_multiple_param_func()
    {
        let input = "
    func doubles(x, x2) ->
        x + x == x2
    --

    func main() ->
        doubles(5, 10)
    --
    ";
        ast::parse(lex(input));
    }

    #[test]
    fn test_ast_parse_if()
    {
        let input = "
    if x ->
        y
    else ->
        z
    --
    ";
        ast::parse(lex(input));

        // didn't crash
        assert_eq!(1, 1);
    }

    #[test]
    fn test_ast_parse_if_no_else()
    {
        let input = "if x -> y --";
        let root = ast::parse(lex(input));

        let blocka = ast::IfCase::new(
            Ast::Localid(Lstr::from("x"), SrcLoc::new(1, 3)),
            Ast::Block(vec![Ast::Localid(Lstr::from("y"), SrcLoc::new(1, 6))]),
            None,
            SrcLoc::new(1, 1),
        );

        let expected = Ast::Block(vec![Ast::IfExpr(
            ast::IfType::If,
            Box::new(Ast::ConstVoid),
            Box::new(blocka),
            SrcLoc::new(1, 1),
        )]);
        assert_eq!(expected, root);
    }

    #[test]
    fn test_ast_parse_macro()
    {
        let input = "
            macro mand(a, b) ->
                if
                |a -> b
                |else -> false
                --
            --
            ";
        let root = ast::parse(lex(input));

        if let Ast::Block(lines) = root {
            let f = lines.first().unwrap();
            if let &Ast::DefFunc(ast::FuncClass::Macro, ref decl, _) = f {
                assert_eq!("mand", Lstr::from(&decl.name).str());
                assert_eq!(2, decl.args.len());
            } else {
                panic!("mand is not a macro");
            }
        } else {
            panic!("mand does not have a block");
        }
    }

    #[test]
    fn test_parse_call_function_call_result()
    {
        let input = "(foo(5))(6)";
        let root = ast::parse(lex(input));

        let foo_call = Ast::Tuple(
            vec![Kxpr::new_x(Ast::Call(
                Box::new(test_localid("foo", 1, 2)),
                vec![Kxpr::new_x(Ast::ConstInt(5))].into_iter().collect(),
                SrcLoc::new(1, 5),
            ))]
            .into_iter()
            .collect(),
        );

        let p_call = Ast::Call(
            Box::new(foo_call),
            vec![Kxpr::new_x(Ast::ConstInt(6))].into_iter().collect(),
            SrcLoc::new(1, 9),
        );

        let expected = Ast::Block(vec![p_call]);
        assert_eq!(expected, root);
    }

    #[test]
    fn test_parse_enum_variants()
    {
        let input = "
            enum Animal[A]
            |Dog
            |Cat(Int)
            |Mouse(A)
            |Giraffe
                .height: Int
                .weight: A
            --
            ";
        let root = ast::parse(lex(input));

        if let Ast::Block(lines) = root {
            let first = lines.first().unwrap();
            if let &Ast::DefData(ast::DataType::Enum, ref name, ref vars, _) =
                first
            {
                if let Ast::TypeCall(ref call, ref tvars, _) = **name {
                    assert_eq!(
                        Ast::Localid(Lstr::Sref("Animal"), SrcLoc::new(2, 5)),
                        **call,
                    );
                    assert_eq!(
                        Ast::Localid(Lstr::Sref("A"), SrcLoc::new(2, 12)),
                        **tvars.front().unwrap().x.as_ref().unwrap(),
                    );
                } else {
                    panic!("not a typecall");
                }
                assert_eq!(4, vars.len());
            } else {
                panic!("enum is not an enum: {:?}", first);
            }
        } else {
            panic!("enum is not a block: {:?}", root);
        }
    }

    #[test]
    fn test_parse_defstruple_tuple()
    {
        let input = "
            struct Taco(Int, Str)
            ";
        let root = ast::parse(lex(input));

        let def = Ast::DefData(
            ast::DataType::Struple,
            Box::new(test_localid("Taco", 2, 7)),
            vec![Kxpr::new_x(Ast::TypeInt), Kxpr::new_x(Ast::TypeStr)]
                .into_iter()
                .collect(),
            SrcLoc::new(2, 1),
        );
        assert_eq!(&def, root.inner_vec().get(0).unwrap());
    }

    #[test]
    fn test_parse_defstruple_keyed_params()
    {
        let input = "
    struct Taco(number: Int, style: Str)
    ";
        let root = ast::parse(lex(input));

        let def = Ast::DefData(
            ast::DataType::Struple,
            Box::new(test_localid("Taco", 2, 7)),
            vec![
                Kxpr::new(Lstr::from("number"), Ast::TypeInt),
                Kxpr::new(Lstr::from("style"), Ast::TypeStr),
            ]
            .into_iter()
            .collect(),
            SrcLoc::new(2, 1),
        );
        assert_eq!(&def, root.inner_vec().get(0).unwrap());
    }

    #[test]
    fn test_parse_defstruple_mixed_keys()
    {
        let input = "struct Taco(Int, style: Str)";
        let root = ast::parse(lex(input));

        let def = Ast::DefData(
            ast::DataType::Struple,
            Box::new(test_localid("Taco", 1, 7)),
            vec![
                Kxpr::new_x(Ast::TypeInt),
                Kxpr::new(Lstr::from("style"), Ast::TypeStr),
            ]
            .into_iter()
            .collect(),
            SrcLoc::new(1, 1),
        );
        assert_eq!(&def, root.inner_vec().get(0).unwrap());
    }

    #[test]
    fn test_parse_defstruple_block()
    {
        let input = "
    struct Taco
    .number: Int
    .style: Str
    --
    ";
        let root = ast::parse(lex(input));

        let def = Ast::DefData(
            ast::DataType::Struple,
            Box::new(test_localid("Taco", 2, 7)),
            vec![
                Kxpr::new(Lstr::from("number"), Ast::TypeInt),
                Kxpr::new(Lstr::from("style"), Ast::TypeStr),
            ]
            .into_iter()
            .collect(),
            SrcLoc::new(2, 1),
        );
        assert_eq!(&def, root.inner_vec().get(0).unwrap());
    }

    #[test]
    fn test_parse_match_list()
    {
        let input = "
    match x
    |(h;t) -> h
    |(_) -> false
    --
    ";
        let root = ast::parse(lex(input));

        let match_line = if let &Ast::Block(ref items) = &root {
            items.first().unwrap()
        } else {
            panic!("match is not a block");
        };

        if let &Ast::IfExpr(ast::IfType::Match, ref ifx, ref cases, _) =
            match_line
        {
            assert_eq!(Ast::Localid(Lstr::from("x"), SrcLoc::new(2, 6)), **ifx);
            assert!(cases.else_case.is_some());
        } else {
            panic!("match line not an if statement: {:?}", match_line);
        }
    }

    #[test]
    fn test_parse_constructor_call()
    {
        let input = "Taco(1, 2)";
        let root = ast::parse(lex(input));

        let call = Ast::Call(
            Box::new(test_localid("Taco", 1, 1)),
            vec![Kxpr::new_x(Ast::ConstInt(1)), Kxpr::new_x(Ast::ConstInt(2))]
                .into_iter()
                .collect(),
            SrcLoc::new(1, 5),
        );
        let expected = Ast::Block(vec![call]);
        assert_eq!(expected, root);
    }

    #[test]
    fn test_parse_strlit_field_access()
    {
        let input = "\"hello ${dog.name}\"";
        let root = ast::parse(lex(input));

        let strx = Ast::StrExpr(
            vec![
                Ast::ConstStr(Lstr::from("hello ")),
                Ast::DotAccess(
                    Box::new(test_localid("dog", 1, 8)),
                    Lstr::from("name"),
                ),
            ],
            SrcLoc::new(1, 1),
        );
        let expected = Ast::Block(vec![strx]);
        assert_eq!(expected, root);
    }

    #[test]
    fn test_parse_let_plus_negation()
    {
        let input = "
    let x := 4 + 8
    ~x
    ";
        let root = ast::parse(lex(input));

        if let Ast::Block(lines) = root {
            assert_eq!(2, lines.len());
        } else {
            panic!("root is not a block");
        }
    }

    #[test]
    fn test_parse_fieldaccess_addition()
    {
        let input = "Foo(a.b + c, d)";
        let root = ast::parse(lex(input));

        let result = format!("{}", root);
        /*
        let items = match root {
            Ast::Block(block_items) => {
                assert_eq!(1, block_items.len());
                block_items
            }
            not_block => {
                panic!("root is not a block: {:?}", not_block);
            }
        };
        */
        let expected = "block(\n\t(call Foo [(call prefab::int_add [(a . b),c,]),d,])\n)\n";
        assert_eq!(expected, result);
    }

    #[test]
    fn test_parse_let_plus_tuple()
    {
        let input = "
        let x := 4 + y
        (x, z)
        ";
        let root = ast::parse(lex(input));

        if let Ast::Block(items) = root {
            assert_eq!(2, items.len());
        } else {
            panic!("root is not a block");
        }
    }

    #[test]
    fn test_parse_function_hashtag_tuple()
    {
        let input = "
        func foo(input: [(Int, #)]): [#] ->
            [#tacos, #burritos]
        --
        ";
        ast::parse(lex(input));

        assert!(true); // didn't panic!
    }

    #[test]
    fn test_parse_def_type_param()
    {
        let input = "
        enum Foo[A, B]
        |Bar(A)
        |Baz(B)
        --
        ";
        ast::parse(lex(input));
        assert!(true); // didn't panic!
    }

    #[test]
    fn test_parse_nested_type_param()
    {
        let input = "
        func foo(bar: Map[Int, opt::Option[V]]): opt::Option[V] ->
            option::None
        --
        ";
        ast::parse(lex(input));

        assert!(true); // didn't panic!
    }

    #[test]
    fn test_parse_function_type_param()
    {
        let input = "
            func call_func(i: Int, f: F(Int, Str): Int): F(Str): Int ->
                f(i)
            --
            ";
        ast::parse(lex(input));

        assert!(true); // didn't panic!
    }

}
