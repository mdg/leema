use crate::leema::ast2::{self, Ast, AstNode, AstResult, Loc};
use crate::leema::failure::Lresult;
use crate::leema::inter::Blockstack;
use crate::leema::lstr::Lstr;
use crate::leema::proto::ProtoLib; // {ProtoLib, ProtoModule};
use crate::leema::reg::RegTable;
use crate::leema::struple::StrupleKV;
use crate::leema::val::Type;

use std::collections::HashMap;
use std::fmt;

/// Stages
/// - prewrite
///   - apply macros
///   - closure collector
/// - analyze
///   - valid code
///   - collect calls
///   - scope check
///   - typecheck
/// - postwrite
///   - optimization / constant folding / code removal
///   - assign registers

pub enum SemanticAction
{
    Keep(AstNode),
    Rewrite(AstNode),
    Remove,
}

pub type SemanticResult = Lresult<SemanticAction>;

pub trait SemanticOp: fmt::Debug
{
    fn f(&mut self, node: AstNode) -> SemanticResult
    {
        Ok(SemanticAction::Keep(node))
    }
    fn pre(&mut self, node: AstNode) -> SemanticResult
    {
        Ok(SemanticAction::Keep(node))
    }
    fn post(&mut self, node: AstNode) -> SemanticResult
    {
        Ok(SemanticAction::Keep(node))
    }
}

#[derive(Debug)]
struct SemanticPipeline<'p>
{
    ops: Vec<&'p mut SemanticOp>,
}

impl<'p> SemanticOp for SemanticPipeline<'p>
{
    fn pre(&mut self, mut node: AstNode) -> SemanticResult
    {
        loop {
            let mut do_loop = false;
            for op in self.ops.iter_mut() {
                match op.pre(node)? {
                    SemanticAction::Keep(knode) => {
                        node = knode;
                    }
                    SemanticAction::Rewrite(rnode) => {
                        node = rnode;
                        do_loop = true;
                        break;
                    }
                    SemanticAction::Remove => {
                        return Ok(SemanticAction::Remove);
                    }
                }
            }
            if !do_loop {
                break;
            }
        }

        Ok(SemanticAction::Keep(node))
    }
}

struct MacroApplication<'l>
{
    proto: &'l ProtoLib,
}

impl<'l> MacroApplication<'l>
{
    fn apply_macro(
        _mac: &Ast,
        _loc: Loc,
        _args: StrupleKV<Option<&'static str>, AstNode>,
    ) -> AstResult
    {
        Ok(AstNode::void())
    }
}

impl<'l> SemanticOp for MacroApplication<'l>
{
    fn f(&mut self, node: AstNode) -> SemanticResult
    {
        if let Ast::Call(callid, args) = *node.node {
            let optmac = match *callid.node {
                Ast::Id1(macroname) => self.proto.get_macro("", macroname)?,
                Ast::Id2(modname, macroname) => {
                    self.proto.get_macro(modname, macroname)?
                }
                _ => None,
            };
            match optmac {
                Some(mac) => {
                    let result = Self::apply_macro(mac, callid.loc, args)?;
                    Ok(SemanticAction::Rewrite(result))
                    // Ok(None)
                }
                None => {
                    let node2 = AstNode::new(Ast::Call(callid, args), node.loc);
                    Ok(SemanticAction::Keep(node2))
                }
            }
        } else {
            Ok(SemanticAction::Keep(node))
        }
    }
}

impl<'l> fmt::Debug for MacroApplication<'l>
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "MacroApplication")
    }
}

#[derive(Debug)]
struct CaseCheck
{
}

impl SemanticOp for CaseCheck
{
    fn pre(&mut self, node: AstNode) -> SemanticResult
    {
        match *node.node {
            Ast::Case(ast2::CaseType::If, Some(cond), _) => {
                return Err(rustfail!(
                    "semantic_failure",
                    "expected no input for if, found {:?}",
                    cond,
                ));
            }
            _ => Ok(SemanticAction::Keep(node)),
        }
    }
}

struct TypeCollector
{
    deftypes: HashMap<&'static str, Type>,
    deffields: HashMap<&'static str, StrupleKV<&'static str, Type>>,
}

struct ClosureCollector
{
    closures: Vec<AstNode>,
}

struct ScopeCheck
{
    blocks: Blockstack,
}

struct TypeCheck {
    // infer: Inferator,
}

struct Registration
{
    reg: RegTable,
}

// 1
// 2
// 3
// 4

// preop: 1, 2, 3, 4
// advance, anterior, befoe, prior, early, prelude

// postop: 4, 3, 2, 1
// after, subsequent, late, postlude

pub struct Semantics
{
    pub types: HashMap<Lstr, HashMap<Lstr, Type>>,

    // pub calls: HashSet<ModLocalId>,
    // pub typecalls: HashSet<SpecialModId>,
    // pub closed: Option<HashSet<Lstr>>,
    pub src: HashMap<Lstr, HashMap<Lstr, AstNode>>,
}

impl Semantics
{
    pub fn new() -> Semantics
    {
        Semantics {
            types: HashMap::new(),
            src: HashMap::new(),
        }
    }

    pub fn compile(
        &mut self,
        proto: &mut ProtoLib,
        module: &str,
    ) -> Lresult<()>
    {
        while let Some(func_ast) = proto.pop_func(module)? {
            let comp_ast = self.compile_func(proto, func_ast)?;
            println!("compiled func to {:?}", comp_ast);
        }
        Ok(())
    }

    pub fn compile_func(
        &mut self,
        proto: &ProtoLib,
        func_ast: AstNode,
    ) -> AstResult
    {
        let mut macs: MacroApplication = MacroApplication { proto };
        let mut pipe = SemanticPipeline {
            ops: vec![&mut macs],
        };

        Self::walk(&mut pipe, func_ast)
    }

    pub fn walk<Op: SemanticOp>(op: &mut Op, node: AstNode) -> AstResult
    {
        let mut prenode = match op.pre(node)? {
            SemanticAction::Keep(inode) => inode,
            SemanticAction::Rewrite(inode) => inode,
            SemanticAction::Remove => {
                return Ok(AstNode::void())
            }
        };

        let new_ast = match *prenode.node {
            Ast::Block(children) => {
                let new_children: Lresult<Vec<AstNode>> = children.into_iter().map(|ch| {
                    Self::walk(op, ch)
                }).collect();
                Ast::Block(new_children?)
            }
            Ast::Call(id, args) => {
                let wid = Self::walk(op, id)?;
                let wargs = args.map_v_into(|v| Self::walk(op, v))?;
                Ast::Call(wid, wargs)
            }
            Ast::Case(typ, None, args) => {
                let new_args: Lresult<Vec<ast2::Case>> = args.into_iter().map(|ch| {
                    let wcond = Self::walk(op, ch.cond)?;
                    let wbody = Self::walk(op, ch.body)?;
                    Ok(ast2::Case::new(wcond, wbody))
                }).collect();
                Ast::Case(typ, None, new_args?)
            }
            Ast::Case(typ, Some(cond), children) => {
                let wcond = Self::walk(op, cond)?;
                let wchildren: Lresult<Vec<ast2::Case>> = children.into_iter().map(|ch| {
                    let wcond = Self::walk(op, ch.cond)?;
                    let wbody = Self::walk(op, ch.body)?;
                    Ok(ast2::Case::new(wcond, wbody))
                }).collect();
                Ast::Case(typ, Some(wcond), wchildren?)
            }
            Ast::ConstVal(v) => {
                // can't walk past on const
                Ast::ConstVal(v)
            }
            Ast::DefConst(_, _) => {
                return Err(rustfail!(
                    "semantic_failure",
                    "cannot define const at {:?}",
                    prenode.loc,
                ));
            }
            ast => ast, // do nothing for everything else
        };
        prenode.node = Box::new(new_ast);

        let postnode = match op.post(prenode)? {
            SemanticAction::Keep(inode) => inode,
            SemanticAction::Rewrite(inode) => inode,
            SemanticAction::Remove => {
                return Ok(AstNode::void())
            }
        };

        Ok(postnode)
    }
}

#[cfg(test)]
mod tests
{
    use crate::leema::loader::Interloader;
    use crate::leema::lstr::Lstr;
    use crate::leema::program;


    #[test]
    fn test_semantics_macro()
    {
        let input = r#"
        macro test_and a b >>
            if
            |a >> b
            |else >> false
            --
        --

        func main >>
            test_and(1 == 1, 2 == 3)
        --
        "#;

        let mut loader = Interloader::new(Lstr::Sref("foo.lma"), "lib");
        loader.set_mod_txt(Lstr::Sref("foo"), input.to_string());
        let mut prog = program::Lib::new(loader);
        prog.read_semantics(&Lstr::from("foo")).unwrap();
    }
}
