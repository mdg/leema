use crate::leema::ast2::{Ast, AstNode};
use crate::leema::failure::Lresult;
use crate::leema::module::ModKey;

use std::collections::{HashMap, HashSet};


/// Asts separated into their types of components
#[derive(Debug)]
pub struct ProtoModule<'i>
{
    pub key: ModKey,
    pub imports: HashSet<&'i str>,
    pub macros: HashMap<&'i str, Ast<'i>>,
    pub constants: Vec<AstNode<'i>>,
    pub types: Vec<AstNode<'i>>,
    pub funcs: Vec<AstNode<'i>>,
}

impl<'i> ProtoModule<'i>
{
    pub fn new(key: ModKey, items: Vec<AstNode<'i>>)
        -> Lresult<ProtoModule<'i>>
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
                Ast::DefMacro(macro_name, _, _) => {
                    proto.macros.insert(macro_name, *i.node);
                }
                Ast::DefFunc(_, _, _) => {
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
