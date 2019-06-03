use crate::leema::ast2::{Ast, AstNode, AstResult, Loc};
use crate::leema::failure::Lresult;
use crate::leema::infer::Inferator;
use crate::leema::inter::Blockstack;
use crate::leema::lstr::Lstr;
use crate::leema::proto::ProtoLib; // {ProtoLib, ProtoModule};
use crate::leema::reg::RegTable;
use crate::leema::struple::StrupleKV;
use crate::leema::val::Type;

use std::collections::HashMap;
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
    proto: &'l ProtoLib<'i>,
}

impl<'i, 'l> MacroApplication<'i, 'l>
{
    fn apply_macro(
        _mac: &Ast<'i>,
        _loc: Loc,
        _args: StrupleKV<Option<&'i str>, AstNode<'i>>,
    ) -> AstResult<'i>
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

pub struct Semantics<'i>
{
    pub types: HashMap<Lstr, HashMap<Lstr, Type>>,

    // pub calls: HashSet<ModLocalId>,
    // pub typecalls: HashSet<SpecialModId>,
    // pub closed: Option<HashSet<Lstr>>,
    pub src: HashMap<Lstr, HashMap<Lstr, AstNode<'i>>>,
}

impl<'i> Semantics<'i>
{
    pub fn new() -> Semantics<'i>
    {
        Semantics {
            types: HashMap::new(),
            src: HashMap::new(),
        }
    }

    pub fn compile<'a>(&mut self, proto: &'a mut ProtoLib<'i>, module: &str) -> Lresult<()>
    {
        while let Some(func_ast) = proto.pop_func(module)? {
            let comp_ast = self.compile_func(proto, func_ast)?;
            println!("compiled func to {:?}", comp_ast);
        }
        Ok(())
    }

    pub fn compile_func<'a>(&mut self, proto: &'a ProtoLib<'i>, mut func_ast: AstNode<'i>) -> Lresult<AstNode<'i>>
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
        let mut prog = program::Lib::new(&mut loader);
        prog.read_semantics(&Lstr::from("foo")).unwrap();
    }
}
