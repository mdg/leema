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
    Rust,
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
    Module(Xlist),
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
            Ast::Module(items) => write!(f, "Module {:?}", items),
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

    pub fn replace(&mut self, node: Ast, t: Type)
    {
        *self.node = node;
        self.typ = t;
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

impl Default for AstNode
{
    fn default() -> AstNode
    {
        AstNode::void()
    }
}


#[macro_export]
macro_rules! steptry
{
    ($r:expr) => {
        match $r {
            Ok(AstStep::Ok) => {
                // do nothing
            }
            Ok(AstStep::Rewrite) => {
                return Ok(AstStep::Rewrite);
            }
            Ok(AstStep::Stop) => {
                return Ok(AstStep::Ok);

            }
            Err(f) => {
                return Err(f.loc(file!(), line!()));
            }
        }
    };
}

#[derive(Copy)]
#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub enum LocalType
{
    Param,
    Match,
    Let,
}

#[derive(Copy)]
#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
pub enum AstMode
{
    Value,
    Type,
    LetPattern,
    MatchPattern,
}

impl AstMode
{
    pub fn is_pattern(&self) -> bool
    {
        match self {
            AstMode::LetPattern => true,
            AstMode::MatchPattern => true,
            _ => false,
        }
    }

    pub fn get_pattern(&self) -> Option<LocalType>
    {
        match self {
            AstMode::LetPattern => Some(LocalType::Let),
            AstMode::MatchPattern => Some(LocalType::Match),
            _ => None,
        }
    }
}

pub enum AstStep
{
    Ok,
    Rewrite,
    Stop,
}

pub type StepResult = Lresult<AstStep>;

pub trait Op
{
    fn pre(&mut self, _node: &mut AstNode, _mode: AstMode) -> StepResult
    {
        Ok(AstStep::Ok)
    }

    fn post(&mut self, _node: &mut AstNode, _mode: AstMode) -> StepResult
    {
        Ok(AstStep::Ok)
    }
}

pub struct Pipeline<'p>
{
    ops: Vec<&'p mut dyn Op>,
}

impl<'p> Pipeline<'p>
{
    pub fn new(ops: Vec<&'p mut dyn Op>) -> Pipeline<'p>
    {
        Pipeline{ ops }
    }
}

impl<'p> Op for Pipeline<'p>
{
    fn pre(&mut self, node: &mut AstNode, mode: AstMode) -> StepResult
    {
        for op in self.ops.iter_mut() {
            steptry!(op.pre(node, mode));
        }
        Ok(AstStep::Ok)
    }

    fn post(&mut self, node: &mut AstNode, mode: AstMode) -> StepResult
    {
        for op in self.ops.iter_mut() {
            steptry!(op.post(node, mode));
        }
        Ok(AstStep::Ok)
    }
}

struct Walker
{
    mode: AstMode,
}

pub fn walk(mut node: AstNode, op: &mut dyn Op) -> AstResult
{
    let mut w = Walker{ mode: AstMode::Value };
    w.walk(&mut node, op)?;
    Ok(node)
}

impl Walker
{
    fn set_mode(&mut self, mode: AstMode) -> AstMode
    {
        let prev = self.mode;
        self.mode = mode;
        prev
    }

    fn walk(&mut self, node: &mut AstNode, op: &mut dyn Op) -> StepResult
    {
        loop {
            match self.step(node, op)? {
                AstStep::Ok => {
                    break;
                }
                AstStep::Rewrite => {
                    // loop again
                }
                AstStep::Stop => {
                    return Ok(AstStep::Stop);
                }
            }
        }
        Ok(AstStep::Ok)
    }

    fn step(&mut self, node: &mut AstNode, op: &mut dyn Op) -> StepResult
    {
        match ltry!(op.pre(node, self.mode)) {
            AstStep::Ok => {
                steptry!(self.step_in(node, op));
            }
            AstStep::Rewrite => {
                return Ok(AstStep::Rewrite);
            }
            AstStep::Stop => {
                return Ok(AstStep::Ok);
            }
        }
        op.post(node, self.mode)
    }

    fn step_in(&mut self, node: &mut AstNode, op: &mut dyn Op) -> StepResult
    {
        match &mut *node.node {
            Ast::Call(id, args) => {
                steptry!(self.walk(id, op));
                for a in args.iter_mut() {
                    steptry!(self.walk(&mut a.v, op));
                }
            }
            Ast::Op2(".", ref mut a, _b) => {
                steptry!(self.walk(a, op));
                // nowhere to go with b, should be handled in pre or post
                // also it will just cause scope errors
                // maybe the field shouldn't be a regular Id1
                // maybe it's an ast mode?
            }
            Ast::Op2("'", ref mut a, b) => {
                steptry!(self.walk(a, op));
                let prev = self.set_mode(AstMode::Type);
                steptry!(self.walk(b, op));
                self.set_mode(prev);
            }
            Ast::Op2(_ast_op, ref mut a, ref mut b) => {
                steptry!(self.walk(a, op));
                steptry!(self.walk(b, op));
            }
            Ast::Op1(_ast_op, a) => {
                steptry!(self.walk(a, op));
            }
            Ast::StrExpr(items) => {
                for i in items.iter_mut() {
                    steptry!(self.walk(i, op));
                }
            }
            Ast::List(items) => {
                for i in items.iter_mut() {
                    steptry!(self.walk(&mut i.v, op));
                }
            }
            Ast::Tuple(items) => {
                for i in items.iter_mut() {
                    steptry!(self.walk(&mut i.v, op));
                }
            }
            Ast::Block(ref mut children) => {
                for ch in children.iter_mut() {
                    steptry!(self.walk(ch, op));
                }
            }
            Ast::Let(ref mut lhp, _lht, ref mut rhs) => {
                self.set_mode(AstMode::LetPattern);
                steptry!(self.walk(lhp, op));
                self.set_mode(AstMode::Value);
                steptry!(self.walk(rhs, op));
            }
            Ast::Ifx(cases) => {
                for c in cases.iter_mut() {
                    steptry!(self.walk(&mut c.cond, op));
                    steptry!(self.walk(&mut c.body, op));
                }
            }
            Ast::Matchx(None, cases) => {
                for c in cases.iter_mut() {
                    self.set_mode(AstMode::MatchPattern);
                    steptry!(self.walk(&mut c.cond, op));
                    self.set_mode(AstMode::Value);
                    steptry!(self.walk(&mut c.body, op));
                }
            }
            Ast::Matchx(Some(cond), cases) => {
                steptry!(self.walk(cond, op));
                for c in cases.iter_mut() {
                    self.set_mode(AstMode::MatchPattern);
                    steptry!(self.walk(&mut c.cond, op));
                    self.set_mode(AstMode::Value);
                    steptry!(self.walk(&mut c.body, op));
                }
            }
            Ast::Generic(id, args) => {
                steptry!(self.walk(id, op));
                self.set_mode(AstMode::Type);
                for a in args.iter_mut() {
                    steptry!(self.walk(&mut a.v, op));
                }
            }
            Ast::DefFunc(name, args, result, body) => {
                steptry!(self.walk(name, op));
                for a in args.iter_mut() {
                    steptry!(self.walk(&mut a.v, op));
                }
                steptry!(self.walk(result, op));
                steptry!(self.walk(body, op));
            }
            Ast::DefConst(_name, ref mut v) => {
                steptry!(self.walk(v, op));
            }
            Ast::Return(x) => {
                steptry!(self.walk(x, op));
            }
            Ast::ConstVal(_) | Ast::Id1(_) | Ast::RustBlock | Ast::Void => {
                // nowhere else to go
            }
            Ast::Module(_) | Ast::Wildcard => {
                // nowhere else to go
            }
            // these ASTs should already be processed in the proto phase
            Ast::DefMacro(name, _, _) => {
                return Err(rustfail!(
                    "compile_failure",
                    "macro definition must already be processed: {} @ {:?}",
                    name,
                    node.loc,
                ));
            }
            Ast::DefType(_, name, _) => {
                return Err(rustfail!(
                    "compile_failure",
                    "type definition must already be processed: {:?}",
                    name,
                ));
            }
            Ast::ModAction(action, tree) => {
                return Err(rustfail!(
                    "compile_failure",
                    "module action must already be processed: {:?} {:?}",
                    action,
                    tree,
                ));
            }
            _ => {
            }
        }
        Ok(AstStep::Ok)
    }
}
