use crate::leema::failure::Lresult;
use crate::leema::reg::Reg;
use crate::leema::struple::{Struple2, StrupleKV};
use crate::leema::token::TokenSrc;
use crate::leema::val::{Type, Val};

use std::fmt;


#[derive(Clone)]
#[derive(Copy)]
#[derive(Debug)]
#[derive(PartialEq)]
pub struct Loc
{
    pub lineno: u16,
    pub column: u8,
}

impl Default for Loc
{
    fn default() -> Loc
    {
        Loc {
            lineno: 0,
            column: 0,
        }
    }
}

#[derive(Clone)]
#[derive(Copy)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
pub enum DataType
{
    Struct,
    Union,
}

#[derive(Clone)]
#[derive(Copy)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
pub enum CaseType
{
    If,
    Match,
    MatchFailure,
    TypeCast,
}

#[derive(Clone)]
#[derive(PartialEq)]
pub struct Case<'i>
{
    pub cond: AstNode<'i>,
    pub body: AstNode<'i>,
}

impl<'i> Case<'i>
{
    pub fn new(cond: AstNode<'i>, body: AstNode<'i>) -> Case<'i>
    {
        Case { cond, body }
    }
}

impl<'i> fmt::Debug for Case<'i>
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "(Case {:?} ? {:?})", self.cond, self.body)
    }
}

type Klist<'i> = StrupleKV<&'i str, Option<AstNode<'i>>>;
pub type Xlist<'i> = StrupleKV<Option<&'i str>, AstNode<'i>>;

#[derive(Clone)]
#[derive(PartialEq)]
pub enum Ast<'i>
{
    Block(Vec<AstNode<'i>>),
    Call(AstNode<'i>, Xlist<'i>),
    Case(CaseType, Option<AstNode<'i>>, Vec<Case<'i>>),
    ConstVal(Val),
    DefConst(&'i str, AstNode<'i>),
    DefFunc(AstNode<'i>, Xlist<'i>, AstNode<'i>),
    DefMacro(&'i str, Vec<&'i str>, AstNode<'i>),
    DefType(DataType, AstNode<'i>, Xlist<'i>),
    FuncType(StrupleKV<&'i str, AstNode<'i>>),
    Id1(&'i str),
    Id2(&'i str, &'i str),
    IdGeneric(AstNode<'i>, Klist<'i>),
    Import(&'i str),
    LessThan3(AstNode<'i>, bool, AstNode<'i>, bool, AstNode<'i>),
    Let(AstNode<'i>, AstNode<'i>, AstNode<'i>),
    List(Xlist<'i>),
    Map(StrupleKV<AstNode<'i>, AstNode<'i>>),
    NewStruct(AstNode<'i>, Struple2<AstNode<'i>>),
    NewTuple(StrupleKV<&'i str, AstNode<'i>>),
    NewUnion(AstNode<'i>, &'i str, Struple2<AstNode<'i>>),
    Op1(&'i str, AstNode<'i>),
    Op2(&'i str, AstNode<'i>, AstNode<'i>),
    Return(AstNode<'i>),
    RustBlock,
    StrExpr(Vec<AstNode<'i>>),
    Tuple(Xlist<'i>),
    Type(Type),
    TypeCall(AstNode<'i>, Xlist<'i>),
    Void,
    Wildcard,
}

impl<'i> Ast<'i>
{
    pub fn loc(t: &TokenSrc) -> Loc
    {
        Loc {
            lineno: t.begin.lineno,
            column: t.begin.column,
        }
    }

    pub fn is_const(&self) -> bool
    {
        match self {
            Ast::ConstVal(_) => true,
            Ast::List(items) => items.iter_v().all(|a| a.node.is_const()),
            _ => false,
        }
    }

    pub fn fmt_inner(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self {
            Ast::Block(items) => write!(f, "Block {:?}", items),
            Ast::Call(id, args) => write!(f, "Call {:?} {:?}", id, args),
            Ast::Case(typ, None, args) => write!(f, "{:?} {:?}", typ, args),
            Ast::Case(typ, Some(cond), args) => {
                write!(f, "{:?} {:?} {:?}", typ, cond, args)
            }
            Ast::ConstVal(v) => write!(f, "Const {}", v),
            Ast::DefConst(id, x) => write!(f, "DefConst {} := {:?}", id, x),
            Ast::DefFunc(name, args, body) => {
                write!(f, "DefFunc {:?} {:?} {:?}", name, args, body)
            }
            Ast::DefMacro(name, args, body) => {
                write!(f, "DefMacro {:?} {:?} {:?}", name, args, body)
            }
            Ast::DefType(dtype, name, fields) => {
                write!(f, "DefType {:?} {:?} {:?}", dtype, name, fields)
            }
            // Ast::Def(v) => write!(f, "Def {}", v),
            Ast::Id1(id) => write!(f, "Id {}", id),
            Ast::Id2(id1, id2) => write!(f, "Id {}::{}", id1, id2),
            Ast::IdGeneric(id, args) => {
                write!(f, "IdGeneric {:?} {:?}", id, args)
            }
            Ast::Import(module) => write!(f, "Import {:?}", module),
            Ast::Let(lhp, _lht, rhs) => write!(f, "Let {:?} := {:?}", lhp, rhs),
            Ast::List(items) => write!(f, "List {:?}", items),
            Ast::Op1(op, node) => write!(f, "Op1 {} {:?}", op, node),
            Ast::Op2(op, a, b) => write!(f, "Op2 {} {:?} {:?}", op, a, b),
            Ast::RustBlock => write!(f, "RustBlock"),
            Ast::StrExpr(items) => write!(f, "Str {:?}", items),
            Ast::Tuple(items) => write!(f, "Tuple {:?}", items),
            Ast::Void => write!(f, "Void"),
            Ast::Wildcard => write!(f, "_"),
            // unimplemented
            Ast::FuncType(_) => unimplemented!(),
            Ast::LessThan3(_, _, _, _, _) => unimplemented!(),
            Ast::Map(_) => unimplemented!(),
            Ast::NewStruct(_, _) => unimplemented!(),
            Ast::NewTuple(_) => unimplemented!(),
            Ast::NewUnion(_, _, _) => unimplemented!(),
            Ast::Return(_) => unimplemented!(),
            Ast::Type(_) => unimplemented!(),
            Ast::TypeCall(_, _) => unimplemented!(),
        }
    }
}

impl<'i> fmt::Debug for Ast<'i>
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "(")?;
        self.fmt_inner(f)?;
        write!(f, ")")
    }
}


#[derive(Clone)]
#[derive(PartialEq)]
pub struct AstNode<'i>
{
    pub node: Box<Ast<'i>>,
    pub loc: Loc,
    pub typ: Type,
    pub dst: Reg,
}

pub type AstResult<'input> = Lresult<AstNode<'input>>;

impl<'i> AstNode<'i>
{
    pub fn new(node: Ast<'i>, loc: Loc) -> AstNode<'i>
    {
        AstNode {
            node: Box::new(node),
            loc,
            typ: Type::Unknown,
            dst: Reg::Void,
        }
    }

    pub fn new_constval(v: Val, loc: Loc) -> AstNode<'i>
    {
        let const_type = v.get_type();
        AstNode {
            node: Box::new(Ast::ConstVal(v)),
            loc,
            typ: const_type,
            dst: Reg::Void,
        }
    }

    pub fn void() -> AstNode<'i>
    {
        AstNode {
            node: Box::new(Ast::Void),
            loc: Loc {
                lineno: 0,
                column: 0,
            },
            typ: Type::Unknown,
            dst: Reg::Void,
        }
    }

    pub fn replace(&self, node: Ast<'i>, t: Type) -> AstNode<'i>
    {
        AstNode {
            node: Box::new(node),
            loc: self.loc.clone(),
            typ: t,
            dst: self.dst.clone(),
        }
    }

    pub fn set_dst(&mut self, dst: Reg)
    {
        self.dst = dst;
    }
}

impl<'i> fmt::Debug for AstNode<'i>
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "(")?;
        self.node.fmt_inner(f)?;
        write!(f, " {},{})", self.loc.lineno, self.loc.column)
    }
}
