use crate::leema::ast2::{Ast, AstNode};
use crate::leema::failure::Lresult;
use crate::leema::grammar2::Grammar;
use crate::leema::loader::Interloader;
use crate::leema::lstr::Lstr;
use crate::leema::module::ModKey;
use crate::leema::token::Tokenz;

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

pub struct ProtoLib<'i>
{
    loader: Interloader,
    protos: HashMap<Lstr, ProtoModule<'i>>,
}

impl<'i> ProtoLib<'i>
{
    pub fn new(loader: Interloader) -> ProtoLib<'i>
    {
        ProtoLib{
            loader,
            protos: HashMap::new(),
        }
    }

    pub fn load(&mut self, modname: &Lstr) -> Lresult<()>
    {
        vout!("ProtoLib::load: {}\n", modname);
        if self.protos.contains_key(modname) {
            return Ok(());
        }
        let modtxt = self.loader.read_mod(modname)?;
        let asts = Grammar::new(Tokenz::lexp(modtxt)?).parse_module()?;
        let modkey = ModKey::name_only(modname.clone());
        self.protos.insert(modname.clone(), ProtoModule::new(modkey, asts)?);
        Ok(())
    }

    pub fn get(&self, modname: &str) -> Lresult<&ProtoModule<'i>>
    {
        self.protos.get(modname)
            .ok_or_else(|| {
                rustfail!(
                    "compile_failure",
                    "module not loaded: {}",
                    modname,
                )
            })
    }
}
