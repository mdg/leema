use crate::leema::failure::Lresult;
use crate::leema::module::{ImportedMod, ModAlias, ModRelativity};
use crate::leema::reg::Reg;
use crate::leema::struple::{self, StrupleKV};
use crate::leema::token::TokenSrc;
use crate::leema::val::{Type, Val};

use std::collections::HashMap;
use std::fmt;
use std::path::{Component, PathBuf};


#[derive(Clone)]
#[derive(Copy)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
pub struct Loc
{
    pub lineno: u16,
    pub column: u8,
}

impl Loc
{
    pub fn new(lineno: u16, column: u8) -> Loc
    {
        Loc { lineno, column }
    }
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
    MatchFailure,
    TypeCast,
}

#[derive(Clone)]
#[derive(PartialEq)]
pub struct Case
{
    pub cond: AstNode,
    pub body: AstNode,
}

impl Case
{
    pub fn new(cond: AstNode, body: AstNode) -> Case
    {
        Case { cond, body }
    }
}

impl fmt::Debug for Case
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "(Case {:?} ? {:?})", self.cond, self.body)
    }
}

#[derive(Clone)]
#[derive(Copy)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
pub enum ModAction
{
    Export,
    Import,
    Include,
}

#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
pub enum ModTree
{
    Id(&'static str, Loc),
    Block(Vec<ModTree>),
    Sub(&'static str, Box<ModTree>),
    Module(Loc),
    Root(Box<ModTree>),
    Sibling(Box<ModTree>),
    Wildcard(Loc),
}

impl ModTree
{
    pub fn sub(a: &'static str, b: ModTree) -> ModTree
    {
        ModTree::Sub(a, Box::new(b))
    }

    pub fn push_sub(&mut self, tail: ModTree)
    {
        match self {
            ModTree::Id(a, _) => {
                *self = ModTree::Sub(a, Box::new(tail));
            }
            ModTree::Sub(_, ref mut b) => {
                b.push_sub(tail);
            }
            ModTree::Root(ref mut sub) => {
                sub.push_sub(tail);
            }
            ModTree::Sibling(ref mut sub) => {
                sub.push_sub(tail);
            }
            ModTree::Block(_) | ModTree::Module(_) | ModTree::Wildcard(_) => {
                // these can't really contain a sub
                // this should never happen
                unimplemented!();
            }
        }
    }

    pub fn collect(&self, flats: &mut HashMap<&'static str, (ImportedMod, Loc)>) -> Lresult<()>
    {
        let mut paths = PathBuf::new();
        self._collect(flats, ModRelativity::Child, &mut paths)?;
        Ok(())
    }

    pub fn _collect(
        &self,
        flats: &mut HashMap<&'static str, (ImportedMod, Loc)>,
        rel: ModRelativity,
        path: &mut PathBuf,
    ) -> Lresult<bool>
    {
        let dot_module = match self {
            ModTree::Id(id, loc) => {
                path.push(id);
                flats.insert(id, (ImportedMod(path.clone()), *loc));
                path.pop();
                false
            }
            ModTree::Sub(id, subs) => {
                path.push(id);
                let include_this = subs._collect(flats, rel, path)?;
                if include_this {
                    flats.insert(id, (ImportedMod(path.clone()), *loc));
                }
                path.pop();
                false
            }
            ModTree::Block(block) => {
                let include_any = false;
                for item in block.iter() {
                    let include_this = item._collect(flats, rel, path)?;
                    include_any = include_any || include_this;
                }
                include_any
            }
            ModTree::Root(subs) => {
                if !path.as_os_str().is_empty() {
                    panic!("path must be empty for absolute path: {:?}", path);
                }
                path.push(Component::RootDir);
                subs._collect(flats, ModRelativity::Absolute, path)?;
                *path = PathBuf::new();
                false
            }
            ModTree::Sibling(subs) => {
                if !path.as_os_str().is_empty() {
                    panic!("path must be empty for sibling path: {:?}", path);
                }
                path.push(Component::ParentDir);
                subs._collect(flats, ModRelativity::Sibling, path)?;
                *path = PathBuf::new();
                false
            }
            ModTree::Module(loc) => {
                // path.push(Component::CurDir);
                // flats.insert(".", (ImportedMod(path.clone()), *loc));
                // flats.insert(prev.unwrap(), (path.clone(), *loc));
                // path.pop();
                true
            }
            ModTree::Wildcard(_) => {
                // need to do something with this
                false
            }
        };
        Ok(dot_module)
    }
}

pub type Xlist = StrupleKV<Option<&'static str>, AstNode>;

#[derive(Clone)]
#[derive(PartialEq)]
pub enum Ast
{
    Block(Vec<AstNode>),
    Call(AstNode, Xlist),
    ConstVal(Val),
    Copy(AstNode),
    DefConst(&'static str, AstNode),
    DefFunc(AstNode, Xlist, AstNode, AstNode),
    DefMacro(&'static str, Vec<&'static str>, AstNode),
    DefType(DataType, AstNode, Xlist),
    FuncType(Xlist, AstNode),
    Generic(AstNode, Xlist),
    Id1(&'static str),
    Id2(ModAlias, &'static str),
    Ifx(Vec<Case>),
    LessThan3(AstNode, bool, AstNode, bool, AstNode),
    Let(AstNode, AstNode, AstNode),
    List(Xlist),
    Matchx(Option<AstNode>, Vec<Case>),
    ModAction(ModAction, ModTree),
    Op1(&'static str, AstNode),
    Op2(&'static str, AstNode, AstNode),
    Return(AstNode),
    RustBlock,
    StrExpr(Vec<AstNode>),
    Tuple(Xlist),
    Type(Type),
    Void,
    Wildcard,
}

impl Ast
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
            Ast::List(items) => {
                struple::iter_v(items).all(|a| a.node.is_const())
            }
            _ => false,
        }
    }

    pub fn fmt_inner(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self {
            Ast::Block(items) => write!(f, "Block {:?}", items),
            Ast::Call(id, args) => write!(f, "Call {:?} {:?}", id, args),
            Ast::ConstVal(v) => write!(f, "Const {:?}", v),
            Ast::Copy(src) => write!(f, "Copy {:?}", src),
            Ast::DefConst(id, x) => write!(f, "DefConst {} := {:?}", id, x),
            Ast::DefFunc(name, args, result, body) => {
                write!(
                    f,
                    "DefFunc {:?} {:?} / {:?} {:?}",
                    name, args, result, body
                )
            }
            Ast::DefMacro(name, args, body) => {
                write!(f, "DefMacro {:?} {:?} {:?}", name, args, body)
            }
            Ast::DefType(dtype, name, fields) => {
                write!(f, "DefType {:?} {:?} {:?}", dtype, name, fields)
            }
            // Ast::Def(v) => write!(f, "Def {}", v),
            Ast::FuncType(args, result) => {
                write!(f, "FuncType {:?} / {:?}]", args, result)
            }
            Ast::Generic(id, args) => write!(f, "Generic {:?}[{:?}]", id, args),
            Ast::Id1(id) => write!(f, "Id {}", id),
            Ast::Id2(id1, id2) => write!(f, "Id {}::{}", id1, id2),
            Ast::Ifx(args) => write!(f, "If {:?}", args),
            Ast::Let(lhp, _lht, rhs) => write!(f, "Let {:?} := {:?}", lhp, rhs),
            Ast::List(items) => write!(f, "List {:?}", items),
            Ast::Matchx(None, args) => write!(f, "Match None {:?}", args),
            Ast::Matchx(Some(cond), args) => {
                write!(f, "Match {:?} {:?}", cond, args)
            }
            Ast::ModAction(action, tree) => {
                write!(f, "{:?} {:?}", action, tree)
            }
            Ast::Op1(op, node) => write!(f, "Op1 {} {:?}", op, node),
            Ast::Op2(op, a, b) => write!(f, "Op2 {} {:?} {:?}", op, a, b),
            Ast::Return(result) => write!(f, "Return {:?}", result),
            Ast::RustBlock => write!(f, "RustBlock"),
            Ast::StrExpr(items) => write!(f, "Str {:?}", items),
            Ast::Tuple(items) => write!(f, "Tuple {:?}", items),
            Ast::Type(inner) => write!(f, "Type {}", inner),
            Ast::Void => write!(f, "Void"),
            Ast::Wildcard => write!(f, "_"),
            // unimplemented
            Ast::LessThan3(_, _, _, _, _) => unimplemented!(),
        }
    }
}

impl fmt::Debug for Ast
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
pub struct AstNode
{
    pub node: Box<Ast>,
    pub loc: Loc,
    pub typ: Type,
    pub dst: Reg,
}

pub type AstResult = Lresult<AstNode>;

impl AstNode
{
    pub fn new(node: Ast, loc: Loc) -> AstNode
    {
        AstNode {
            node: Box::new(node),
            loc,
            typ: Type::Unknown,
            dst: Reg::Undecided,
        }
    }

    pub fn new_constval(v: Val, loc: Loc) -> AstNode
    {
        let const_type = v.get_type();
        AstNode {
            node: Box::new(Ast::ConstVal(v)),
            loc,
            typ: const_type,
            dst: Reg::Undecided,
        }
    }

    pub fn void() -> AstNode
    {
        AstNode {
            node: Box::new(Ast::Void),
            loc: Loc {
                lineno: 0,
                column: 0,
            },
            typ: Type::Unknown,
            dst: Reg::Undecided,
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

    /// Replace the Ast member in this AstNode
    pub fn replace_node(mut self, node: Ast) -> AstNode
    {
        *self.node = node;
        self
    }

    pub fn set_dst(&mut self, dst: Reg)
    {
        self.dst = dst;
    }
}

impl fmt::Debug for AstNode
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "(")?;
        self.node.fmt_inner(f)?;
        write!(f, " {} {} {})", self.typ, self.loc.lineno, self.dst)
    }
}
