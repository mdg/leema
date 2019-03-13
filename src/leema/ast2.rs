use leema::token::TokenSrc;
use leema::val::{Type, Val};
use leema::reg::Reg;
use leema::struple::{Struple2, StrupleKV};


#[derive(Clone)]
#[derive(Copy)]
#[derive(Debug)]
#[derive(PartialEq)]
struct Loc {
    lineno: u16,
    column: u8,
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
#[derive(Debug)]
#[derive(PartialEq)]
pub struct Case<'i>
{
    pub cond: AstNode<'i>,
    pub body: AstNode<'i>,
    pub else_case: Option<Box<Case<'i>>>,
    pub loc: Loc,
}

#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub enum Ast<'i>
{
    Block(Vec<AstNode<'i>>),
    Call(AstNode<'i>, StrupleKV<&'i str, AstNode<'i>>),
    Case(CaseType, AstNode<'i>, Case<'i>),
    ConstExpr(Val),
    DefConst(&'i str, AstNode<'i>),
    DefFunc(AstNode<'i>, StrupleKV<Option<&'i str>, AstNode<'i>>, AstNode<'i>),
    DefMacro(AstNode<'i>, StrupleKV<&'i str, AstNode<'i>>, AstNode<'i>),
    DefType(AstNode<'i>, StrupleKV<&'i str, AstNode<'i>>),
    FuncType(StrupleKV<&'i str, AstNode<'i>>),
    Id1(&'i str),
    Id2(&'i str, &'i str),
    IdGeneric(AstNode<'i>, StrupleKV<&'i str, Option<&'i str>>),
    Import(&'i str),
    LessThan3(AstNode<'i>, bool, AstNode<'i>, bool, AstNode<'i>),
    Let(AstNode<'i>, AstNode<'i>, AstNode<'i>),
    List(StrupleKV<&'i str, AstNode<'i>>),
    Map(StrupleKV<AstNode<'i>, AstNode<'i>>),
    NewStruct(AstNode<'i>, Struple2<AstNode<'i>>),
    NewTuple(StrupleKV<&'i str, AstNode<'i>>),
    NewUnion(AstNode<'i>, &'i str, Struple2<AstNode<'i>>),
    Op1(&'i str, AstNode<'i>),
    Op2(&'i str, AstNode<'i>, AstNode<'i>),
    Return(AstNode<'i>),
    RustBlock,
    StrExpr(Vec<AstNode<'i>>),
    Tuple(StrupleKV<&'i str, AstNode<'i>>),
    Type(Type),
    TypeCall(AstNode<'i>, StrupleKV<&'i str, AstNode<'i>>),
}

impl<'input> Ast<'input>
{
    pub fn loc(t: &TokenSrc) -> Loc
    {
        Loc {
            lineno: t.lineno,
            column: t.column,
        }
    }
}

#[derive(Debug)]
pub struct AstNode<'i>
{
    pub node: Box<Ast<'i>>,
    pub loc: Loc,
    pub typ: Type,
    pub dst: Reg,
}

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

