use crate::leema::ast2::{Ast, AstNode, AstResult, Loc};
use crate::leema::failure::Lresult;
use crate::leema::inter::Blockstack;
use crate::leema::lstr::Lstr;
use crate::leema::proto::ProtoLib; // {ProtoLib, ProtoModule};
use crate::leema::reg::RegTable;
use crate::leema::struple::StrupleKV;
use crate::leema::val::Type;

use std::collections::HashMap;
use std::fmt;


type PipelineResult = Lresult<Option<AstNode>>;

trait PipelineOp: fmt::Debug
{
    fn f(&mut self, node: AstNode) -> PipelineResult;
}

struct Pipeline<'p>
{
    ops: Vec<&'p mut PipelineOp>,
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

impl<'l> PipelineOp for MacroApplication<'l>
{
    fn f(&mut self, node: AstNode) -> PipelineResult
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
                    Ok(Some(result))
                    // Ok(None)
                }
                None => {
                    let node2 = AstNode::new(Ast::Call(callid, args), node.loc);
                    Ok(Some(node2))
                }
            }
        } else {
            Ok(Some(node))
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

    pub fn compile<'a>(
        &mut self,
        proto: &'a mut ProtoLib,
        module: &str,
    ) -> Lresult<()>
    {
        while let Some(func_ast) = proto.pop_func(module)? {
            let comp_ast = self.compile_func(proto, func_ast)?;
            println!("compiled func to {:?}", comp_ast);
        }
        Ok(())
    }

    pub fn compile_func<'a>(
        &mut self,
        proto: &'a ProtoLib,
        mut func_ast: AstNode,
    ) -> Lresult<AstNode>
    {
        let mut macs: MacroApplication = MacroApplication { proto };
        let mut ops = Pipeline {
            ops: vec![&mut macs],
        };

        for op in ops.ops.iter_mut() {
            func_ast = op.f(func_ast)?.unwrap();
        }

        Ok(func_ast)
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
