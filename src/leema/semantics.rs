use crate::leema::ast2::{Ast, AstNode, AstResult, Loc};
use crate::leema::failure::Lresult;
use crate::leema::infer::Inferator;
use crate::leema::inter::Blockstack;
use crate::leema::lstr::Lstr;
use crate::leema::program;
use crate::leema::proto::ProtoModule;
use crate::leema::reg::RegTable;
use crate::leema::struple::StrupleKV;
use crate::leema::val::Type;

use std::collections::{HashMap, HashSet};
use std::fmt;


type PipelineResult<'i> = Lresult<Option<AstNode<'i>>>;

trait PipelineOp<'i>: fmt::Debug
{
    fn f(&mut self, node: AstNode<'i>) -> PipelineResult<'i>;
}

struct Pipeline<'i, 'p>
{
    ops: Vec<&'p mut PipelineOp<'i>>,
}

struct MacroApplication<'i, 'l>
{
    prog: &'l program::Lib<'i>,
}

impl<'i, 'l> MacroApplication<'i, 'l>
{
    fn apply_macro(mac: &Ast<'i>, args: Vec<AstNode<'i>>, loc: Loc) -> AstResult<'i>
    {
        Ok(AstNode::void())
    }
}

impl<'i, 'l> PipelineOp<'i> for MacroApplication<'i, 'l>
{
    fn f(&mut self, node: AstNode<'i>) -> PipelineResult<'i>
    {
        if let Ast::Call(callid, args) = *node.node {
            let optmac = match *callid.node {
                Ast::Id1(macroname) => {
                    self.prog.get_macro("", macroname)
                }
                Ast::Id2(modname, macroname) => {
                    self.prog.get_macro(modname, macroname)
                }
                _ => {
                    None
                }
            };
            match optmac {
                Some(mac) => {
                    let result = Self::apply_macro(mac, args, callid.loc)?;
                    Ok(Some(result))
                }
                None => {
                    Ok(Some(node))
                }
            }
        } else {
            Ok(Some(node))
        }
    }
}

impl<'i, 'l> fmt::Debug for MacroApplication<'i, 'l>
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "MacroApplication")
    }
}

struct TypeCollector<'i>
{
    deftypes: HashMap<&'i str, Type>,
    deffields: HashMap<&'i str, StrupleKV<&'i str, Type>>,
}

struct ClosureCollector<'i>
{
    closures: Vec<AstNode<'i>>,
}

struct ScopeCheck
{
    blocks: Blockstack,
}

struct TypeCheck<'i>
{
    prog: &'i program::Lib<'i>,
    infer: Inferator<'i>,
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

struct Semantics
{
    pub types: HashMap<Lstr, HashMap<Lstr, Type>>,

    // pub calls: HashSet<ModLocalId>,
    // pub typecalls: HashSet<SpecialModId>,
    pub closed: Option<HashSet<Lstr>>,

    is_closure: bool,
}

pub fn compile(prog: &mut program::Lib, proto: ProtoModule) // -> Semantics
{
    let ops = Pipeline{ops: vec![
        &mut MacroApplication{prog: &prog},
    ]};
}
