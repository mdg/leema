use crate::leema::failure::Lresult;
use crate::leema::module::ImportedMod;
use crate::leema::reg::Reg;
use crate::leema::struple::{self, StrupleKV};
use crate::leema::token::TokenSrc;
use crate::leema::val::{Type, Val};

use std::collections::HashMap;
use std::fmt;
use std::path::{Path, PathBuf};

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
}

#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
pub enum ModTree
{
    All(Loc),
    Leaf(&'static str, Loc),
    Branch(&'static str, Vec<ModTree>),
}

impl ModTree
{
    pub fn branch(a: &'static str, b: Vec<ModTree>) -> ModTree
    {
        ModTree::Branch(a, b)
    }

    pub fn collect(
        &self,
        flats: &mut HashMap<&'static str, (ImportedMod, Loc)>,
    ) -> Lresult<()>
    {
        let mut path = PathBuf::new();
        match self {
            ModTree::Leaf(id, loc) => {
                let (key, _) = Self::push(&mut path, id);
                flats.insert(key, (ImportedMod(path), *loc));
            }
            ModTree::Branch(base, branches) => {
                let (_, _) = Self::push(&mut path, base);
                for b in branches.iter() {
                    b._collect(flats, &mut path)?;
                }
            }
            ModTree::All(_) => {
                // what? shouldn't happen
                return Err(rustfail!(
                    "compile_failure",
                    "cannot collect imports from All",
                ));
            }
        }
        Ok(())
    }

    pub fn _collect(
        &self,
        flats: &mut HashMap<&'static str, (ImportedMod, Loc)>,
        path: &mut PathBuf,
    ) -> Lresult<()>
    {
        match self {
            ModTree::Leaf(id, loc) => {
                let (key, popper) = Self::push(path, id);
                flats.insert(key, (ImportedMod(path.clone()), *loc));
                popper(path);
            }
            ModTree::Branch(id, branches) => {
                if path.extension().is_some() {
                    panic!("unexpected extension: {:?}", path);
                }
                let (_, popper) = Self::push(path, id);
                for branch in branches.iter() {
                    branch._collect(flats, path)?;
                }
                popper(path);
            }
            ModTree::All(_) => {
                return Err(rustfail!(
                    "compile_failure",
                    "cannot _collect imports from All",
                ));
            }
        }
        Ok(())
    }

    pub fn push(p: &mut PathBuf, id: &'static str) -> (&'static str, Box<dyn Fn(&mut PathBuf)>)
    {
        if id.starts_with(".") && !id.starts_with("..") {
            let ext = &id[1..];
            p.set_extension(ext);
            (ext, Box::new(|pp: &mut PathBuf| {
                pp.set_extension("");
            }))
        } else {
            let nextp = Path::new(id);
            let num_components = nextp.components().count();
            let key = nextp.extension()
                .or_else(|| nextp.file_stem())
                .and_then(|ip| ip.to_str())
                .unwrap();
            p.push(id);
            (key, Box::new(move |pp: &mut PathBuf| {
                Self::pop(pp, num_components);
            }))
        }
    }

    pub fn pop(p: &mut PathBuf, n: usize)
    {
        for _ in 0..n {
            p.pop();
        }
    }
}

pub type Xlist = StrupleKV<Option<&'static str>, AstNode>;

#[derive(Clone)]
#[derive(Debug)]
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
    Export(AstNode),
    FuncType(Xlist, AstNode),
    Generic(AstNode, Xlist),
    Id1(&'static str),
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
            Ast::Export(x) => write!(f, "Export {:?}", x),
            Ast::FuncType(args, result) => {
                write!(f, "FuncType {:?} / {:?}]", args, result)
            }
            Ast::Generic(id, args) => write!(f, "Generic {:?}[{:?}]", id, args),
            Ast::Id1(id) => write!(f, "Id {}", id),
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

#[derive(Clone)]
#[derive(Debug)]
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


macro_rules! steptry {
    ($r:expr) => {
        match $r {
            Ok(NextStep::Ok) => x,
            Ok(NextStep::Rewrite(0)) => {
                return
            }
            Err(f) => {
                return Err(f.loc(file!(), line!()));
            }
        }
    };
}

macro_rules! walktry {
    ($r:expr) => {
        match $r {
            Ok(WalkStep::Ok) => x,
            Ok(WalkStep::Rewrite(0)) => {
                return Ok(NextStep::Rewrite(1));
            }
            Err(f) => {
                return Err(f.loc(file!(), line!()));
            }
        }
    };
}

enum NextWalk
{
    Ok,
    Rewrite,
    Stop,
}

enum NextStep
{
    Ok,
    Rewrite(i16),
    Stop,
}

type WalkResult = Lresult<NextWalk>;
type StepResult = Lresult<NextStep>;

trait Op
{
    fn pre(&mut self, node: &mut AstNode, op: &mut Walker) -> StepResult;
    fn post(&mut self, node: &mut AstNode, op: &mut Walker) -> StepResult;
}

struct Walker
{
}

impl Walker
{
    pub fn walk(&self, node: &mut AstNode, op: &mut dyn Op) -> WalkResult
    {
        steptry!(op.pre(node, self));
        steptry!(self.step(node, op));
        steptry!(op.post((node, self));
        Ok(NextStep::Ok)
    }

    pub fn step(&self, node: &mut AstNode, op: &mut dyn Op) -> StepResult
    {
        match &mut *node.node {
            Ast::Op2(".", ref mut a, ref mut b) => {
                walktry!(self.walk(a, op));
                walktry!(self.walk(b, op));
            }
            _ => {
            }
        }
        Ok(NextStep::Ok)
    }
}
