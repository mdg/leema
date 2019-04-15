use crate::leema::ast2::{Ast, AstNode, FuncClass};
use crate::leema::failure::Lresult;
use crate::leema::module::ModKey;
use crate::leema::program;
use crate::leema::struple::StrupleKV;
use crate::leema::val::Type;

use std::collections::{HashMap, HashSet};


/// Asts separated into their types of components
#[derive(Debug)]
pub struct ProtoModule<'i>
{
    pub key: ModKey,
    pub imports: HashSet<&'i str>,
    pub macros: HashMap<&'i str, AstNode<'i>>,
    pub constants: Vec<AstNode<'i>>,
    pub types: Vec<AstNode<'i>>,
    pub funcs: Vec<AstNode<'i>>,
}

impl<'i> ProtoModule<'i>
{
    pub fn new(key: ModKey, items: Vec<AstNode<'i>>) -> Lresult<ProtoModule<'i>>
    {
        let mut proto = ProtoModule {
            key,
            imports: HashSet::new(),
            macros: HashMap::new(),
            constants: Vec::new(),
            types: Vec::new(),
            funcs: Vec::new(),
        };

        for i in items {
            match *i.node {
                Ast::DefConst(_, _) => {
                    proto.constants.push(i);
                }
                Ast::DefFunc(FuncClass::Macro, macro_name, args, body) => {
                    if let Ast::Id1(name) = *macro_name.node {
                        let mac = Ast::DefFunc(
                            FuncClass::Macro,
                            macro_name,
                            args,
                            body,
                        );
                        proto.macros.insert(name, AstNode::new(mac, i.loc));
                    } else {
                        return Err(rustfail!(
                            "parse_failure",
                            "expected id for macro name, found {:?}",
                            macro_name,
                        ));
                    }
                }
                Ast::DefFunc(FuncClass::Func, _, _, _) => {
                    proto.funcs.push(i);
                }
                Ast::DefType(_, _, _) => {
                    proto.types.push(i);
                }
                Ast::Import(imp) => {
                    proto.imports.insert(imp);
                }
                _ => {
                    return Err(rustfail!(
                        "parse_failure",
                        "expected module statement, found {:?}",
                        i,
                    ));
                }
            }
        }
        Ok(proto)
    }
}

struct MacroApplication<'i, 'l>(&'l program::Lib<'i>);

struct TypeCollector<'i>
{
    deftypes: HashMap<&'i str, Type>,
    deffields: HashMap<&'i str, StrupleKV<&'i str, Type>>,
}

struct ClosureCollector<'i>
{
    closures: Vec<AstNode<'i>>,
}
