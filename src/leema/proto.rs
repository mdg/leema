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
pub struct ProtoModule
{
    pub key: ModKey,
    pub imports: HashSet<&'static str>,
    pub macros: HashMap<&'static str, Ast>,
    pub constants: Vec<AstNode>,
    pub types: Vec<AstNode>,
    pub funcs: Vec<AstNode>,
}

impl ProtoModule
{
    pub fn new(key: ModKey, items: Vec<AstNode>) -> Lresult<ProtoModule>
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

pub struct ProtoLib
{
    protos: HashMap<Lstr, ProtoModule>,
}

impl ProtoLib
{
    pub fn new() -> ProtoLib
    {
        ProtoLib {
            protos: HashMap::new(),
        }
    }

    pub fn load(
        &mut self,
        loader: &mut Interloader,
        modname: &Lstr,
    ) -> Lresult<()>
    {
        vout!("ProtoLib::load({})\n", modname);
        if self.protos.contains_key(modname) {
            return Ok(());
        }
        let modtxt = loader.read_mod(modname)?;
        let asts = Grammar::new(Tokenz::lexp(modtxt)?).parse_module()?;
        let modkey = ModKey::name_only(modname.clone());
        let proto = ProtoModule::new(modkey, asts)?;
        self.protos.insert(modname.clone(), proto);
        Ok(())
    }

    pub fn load_imports(
        &mut self,
        loader: &mut Interloader,
        modname: &Lstr,
    ) -> Lresult<()>
    {
        vout!("ProtoLib::load_imports({})\n", modname);
        let mut imported: Vec<Lstr> = vec![];
        {
            let proto = self.protos.get(modname).ok_or_else(|| {
                rustfail!(
                    "semantic_failure",
                    "an import module does not exist: {}",
                    modname,
                )
            })?;
            for i in proto.imports.iter() {
                if i == &modname {
                    return Err(rustfail!(
                        "semantic_failure",
                        "a module cannot import itself: {}",
                        i,
                    ));
                }
                if self.protos.contains_key(*i) {
                    continue;
                }
                imported.push(Lstr::from(String::from(*i)));
            }
        }
        for i in imported.iter() {
            self.load(loader, i)?;
        }
        Ok(())
    }

    pub fn pop_func(&mut self, module: &str) -> Lresult<Option<AstNode>>
    {
        match self.protos.get_mut(module) {
            Some(protomod) => Ok(protomod.funcs.pop()),
            None => {
                Err(rustfail!(
                    "semantic_failure",
                    "could not find module: {}",
                    module,
                ))
            }
        }
    }


    pub fn get(&self, modname: &str) -> Lresult<&ProtoModule>
    {
        self.protos.get(modname).ok_or_else(|| {
            rustfail!("compile_failure", "module not loaded: {}", modname,)
        })
    }

    pub fn get_macro(
        &self,
        module: &str,
        macroname: &str,
    ) -> Lresult<Option<&Ast>>
    {
        let proto = self.protos.get(module).ok_or_else(|| {
            rustfail!("semantic_failure", "module not loaded: {}", module,)
        })?;
        Ok(proto.macros.get(macroname))
    }
}
