use crate::leema::canonical::Canonical;
use crate::leema::failure::Lresult;
use crate::leema::lstr::Lstr;
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
    Alias,
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

    pub fn push(
        p: &mut PathBuf,
        id: &'static str,
    ) -> (&'static str, Box<dyn Fn(&mut PathBuf)>)
    {
        if id.starts_with(".") && !id.starts_with("..") {
            let ext = &id[1..];
            p.set_extension(ext);
            (
                ext,
                Box::new(|pp: &mut PathBuf| {
                    pp.set_extension("");
                }),
            )
        } else {
            let nextp = Path::new(id);
            let num_components = nextp.components().count();
            let key = nextp
                .extension()
                .or_else(|| nextp.file_stem())
                .and_then(|ip| ip.to_str())
                .unwrap();
            p.push(id);
            (
                key,
                Box::new(move |pp: &mut PathBuf| {
                    Self::pop(pp, num_components);
                }),
            )
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
    // Type def_alias Type
    Alias(Xlist, Box<AstNode>),

    // _ Block [Stmts]
    Block(Vec<AstNode>),

    // Base CallPath Args
    Call(AstNode, Xlist),

    // Path
    Canonical(Canonical),

    // Val
    ConstVal(Val),

    // ?
    Copy(AstNode),

    // Src Set Args
    CopyAndSet(AstNode, Xlist),

    // Base Field FieldRef
    DataMember(Type, u8),

    // Id def_const Src
    DefConst(&'static str, AstNode),

    // f...
    DefFunc(AstNode, Xlist, AstNode, AstNode),

    // (Trait, DataType) impl Block
    DefImpl(AstNode, AstNode, Vec<AstNode>),

    // Trait def_trait Block
    DefTrait(AstNode, Vec<AstNode>),

    // Name:Args def_macro Block
    DefMacro(&'static str, Vec<&'static str>, AstNode),

    // Type def_struct/enum (Variants|Fields)
    DefType(DataType, AstNode, Xlist),

    // Regular Type?
    FuncType(AstNode, Xlist),

    // Id with_type_args|apply_types Type*
    Generic(AstNode, Xlist),

    // Id, and Id : Type
    Id(&'static str),

    // _ if Cases
    Ifx(Vec<Case>),

    // Pattern(:Type)? assignment Src
    Let(AstNode, AstNode, AstNode),

    // List
    List(Xlist),

    // expr match cases
    Matchx(Option<AstNode>, Vec<Case>),

    // import/export list
    ModAction(ModAction, ModTree),

    // (_ op val) or (val op _)
    Op1(&'static str, AstNode),

    // (a op b)
    Op2(&'static str, AstNode, AstNode),

    // (_ return val)
    Return(AstNode),

    // (base strx list)
    StrExpr(Vec<AstNode>),

    // (_ tuple [items])
    Tuple(Xlist),

    // (path Type args)
    Type(Type),

    // (Id _)
    Wildcard,
}

impl Ast
{
    pub const BLOCK_ABSTRACT: Ast = Ast::ConstVal(Val::BLOCK_ABSTRACT);
    pub const BLOCK_RUST: Ast = Ast::ConstVal(Val::BLOCK_RUST);
    pub const VOID: Ast = Ast::ConstVal(Val::VOID);
    pub const NEWLINE: Ast = Ast::ConstVal(Val::Str(Lstr::Sref("\n")));

    pub const fn canonical(c: &'static str) -> Ast
    {
        Ast::Canonical(Canonical::new(Lstr::Sref(c)))
    }

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

    // check if an Ast object is const VOID
    pub fn is_void(&self) -> bool
    {
        *self == Ast::VOID
    }

    pub fn fmt_inner(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self {
            Ast::Alias(gens, src) => write!(f, "Alias {:?} {:?}", gens, src),
            Ast::Block(items) => write!(f, "Block {:?}", items),
            Ast::Call(id, args) => write!(f, "Call {:?} {:?}", id, args),
            Ast::Canonical(c) => write!(f, "Canonical {:?}", c),
            Ast::ConstVal(v) => write!(f, "Const {:?}", v),
            Ast::Copy(src) => write!(f, "Copy {:?}", src),
            Ast::CopyAndSet(src, flds) => {
                write!(f, "(CopyAndSet {:?} {:?})", src, flds)
            }
            Ast::DataMember(t, i) => write!(f, "DataMember: {} {:?}", i, t),
            Ast::DefConst(id, x) => write!(f, "DefConst {} := {:?}", id, x),
            Ast::DefFunc(name, args, result, body) => {
                write!(
                    f,
                    "DefFunc {:?} {:?} / {:?} {:?}",
                    name, args, result, body
                )
            }
            Ast::DefImpl(typ, iface, funcs) => {
                write!(f, "DefImpl {:?} {:?} {:?}", typ, iface, funcs)
            }
            Ast::DefTrait(name, funcs) => {
                write!(f, "DefTrait {:?} {:?}", name, funcs)
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
            Ast::Id(id) => write!(f, "Id {}", id),
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
            Ast::StrExpr(items) => write!(f, "Str {:?}", items),
            Ast::Tuple(items) => write!(f, "Tuple {:?}", items),
            Ast::Type(inner) => write!(f, "Type {}", inner),
            Ast::Wildcard => write!(f, "_"),
        }
    }
}

impl Default for Ast
{
    fn default() -> Ast
    {
        Ast::VOID
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
            typ: Type::UNKNOWN,
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
            node: Box::new(Ast::ConstVal(Val::VOID)),
            loc: Loc {
                lineno: 0,
                column: 0,
            },
            typ: Type::VOID,
            dst: Reg::Undecided,
        }
    }

    pub fn replace(&mut self, node: Ast, t: Type)
    {
        *self.node = node;
        self.typ = t;
    }

    /// Replace the Ast member in this AstNode
    /// update the type if possible
    pub fn replace_node(&mut self, node: Ast)
    {
        match &node {
            Ast::ConstVal(cv) => {
                self.typ = cv.get_type();
            }
            Ast::Type(_) => {
                self.typ = Type::KIND;
            }
            _ => {} // leave type as-is
        }
        *self.node = node;
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
macro_rules! steptry {
    ($r:expr) => {
        match $r {
            Ok(AstStep::Ok) => {
                // do nothing
            }
            Ok(AstStep::Replace(node, typ)) => {
                return Ok(AstStep::Replace(node, typ));
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
    Replace(Ast, Type),
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
        Pipeline { ops }
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
        for op in self.ops.iter_mut().rev() {
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
    let mut w = Walker {
        mode: AstMode::Value,
    };
    w.walk(&mut node, op)?;
    Ok(node)
}

pub fn walk_ref_mut(node: &mut AstNode, op: &mut dyn Op) -> StepResult
{
    let mut w = Walker {
        mode: AstMode::Value,
    };
    w.walk(node, op)
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
                AstStep::Replace(new_node, typ) => {
                    node.replace(new_node, typ);
                    // loop again
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
        steptry!(op.pre(node, self.mode));
        steptry!(self.step_in(node, op));
        op.post(node, self.mode)
    }

    fn step_in(&mut self, node: &mut AstNode, op: &mut dyn Op) -> StepResult
    {
        match &mut *node.node {
            Ast::Alias(gens, src) => {
                for g in gens.iter_mut() {
                    steptry!(self.walk(&mut g.v, op));
                }
                steptry!(self.walk(src, op));
            }
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
                let prev = self.set_mode(AstMode::Type);
                for a in args.iter_mut() {
                    steptry!(self.walk(&mut a.v, op));
                }
                self.set_mode(prev);
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
            Ast::CopyAndSet(src, flds) => {
                steptry!(self.walk(src, op));
                for f in flds.iter_mut() {
                    steptry!(self.walk(&mut f.v, op));
                }
            }
            Ast::Canonical(_)
            | Ast::ConstVal(_)
            | Ast::DataMember(_, _)
            | Ast::Id(_)
            | Ast::Wildcard => {
                // nowhere else to go
            }
            Ast::Copy(src) => {
                steptry!(self.walk(src, op));
            }
            Ast::FuncType(_, _) | Ast::Type(_) => {
                panic!("func type crawling not implemented");
            }
            // these ASTs should already be processed in the proto phase
            Ast::DefMacro(_, _, _) => {
                // don't do anything w/ macro
            }
            Ast::DefImpl(name, iface, _) => {
                return Err(rustfail!(
                    "compile_failure",
                    "impl definition must already be processed: {:?} as {:?}",
                    name,
                    iface,
                ));
            }
            Ast::DefTrait(name, _) => {
                return Err(rustfail!(
                    "compile_failure",
                    "trait definition must already be processed: {:?}",
                    name,
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
        }
        Ok(AstStep::Ok)
    }
}
