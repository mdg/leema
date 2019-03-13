use leema::token::TokenSrc;
use leema::val::Val;


struct Loc {
    lineno: u16,
    column: u8,
}

impl From<TokenSrc> for Loc
{
    fn from(t: &TokenSrc) -> Loc
    {
        Loc {
            lineno: t.lineno,
            column: t.column,
        }
    }
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

// almost all these vecs should be Struples
enum Ast<'input>
{
    Block(Vec<Val>, Loc),
    Call(Ast, Vec<Ast>, Loc),
    Case(CaseType, Ast, Ast, Ast, Loc),
    Const(Val, Loc),
    DefConst(&'input str, Ast, Loc),
    DefFunc(Ast, Ast, Ast, Loc),
    DefFunc(AstNode, Struple2<AstNode>, AstNode, AstNode),
    DefType(Ast, Vec<Ast>, Loc),
    FuncType(Vec<Ast>, Loc),
    LessThan3(Ast, bool, Ast, bool, Ast, Loc),
    Let(Ast, Ast, Loc),
    List(Vec<Ast>, Loc),
    Localid(&'input str, Loc),
    ModId(&'input str, &'input str, Vec<Ast>, Loc),
    Op1(Ast, Ast, Loc),
    Op2(Ast, Ast, Ast, Loc),
    Tuple(Vec<Ast>, Loc),
}

#[derive(Debug)]
pub enum Ast
{
    Block(Vec<AstNode>),
    Call(AstNode, Struple2<AstNode>),
    Cons(AstNode, AstNode),
    ConstVal(Val),
    DefMacro(AstNode, Struple2<AstNode>, AstNode),
    DefStruct(AstNode, Struple2<AstNode>),
    DefUnion(AstNode, Struple2<AstNode>),
    DotAccess(AstNode, Lstr),
    GenericId(AstNode, Struple2<Lstr>),
    Id(Lstr),
    Ifx(IfType, AstNode, IfCase),
    Import(Lstr),
    Let(AstNode, AstNode, AstNode),
    List(Struple2<AstNode>),
    Map(Struple2<AstNode>),
    ModId(Lstr, Lstr),
    NewStruct(AstNode, Struple2<AstNode>),
    NewTuple(Struple2<AstNode>),
    NewUnion(AstNode, Lstr, Struple2<AstNode>),
    Return(AstNode),
    RustBlock,
    StrExpr(Vec<AstNode>),
    Type(Type),
    TypeCall(AstNode, Struple2<AstNode>),
    Wildcard,
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
pub struct AstNode
{
    node: Box<Ast>,
    loc: SrcLoc,
    typ: Type,
    dst: Reg,
}

impl AstNode
{
    pub fn new(node: Ast, loc: SrcLoc) -> AstNode
    {
        AstNode {
            node: Box::new(node),
            loc,
            typ: Type::Unknown,
            dst: Reg::Void,
        }
    }

    pub fn replace(&self, node: Ast, t: Type) -> AstNode
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

