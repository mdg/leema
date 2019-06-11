use crate::leema::ast2::{Ast, AstNode, DataType, Xlist};
use crate::leema::failure::Lresult;
use crate::leema::grammar2::Grammar;
use crate::leema::loader::Interloader;
use crate::leema::lri::Lri;
use crate::leema::lstr::Lstr;
use crate::leema::module::ModKey;
use crate::leema::token::Tokenz;
use crate::leema::val::Type;

use std::collections::{HashMap, HashSet};


/// Asts separated into their types of components
#[derive(Debug)]
pub struct ProtoModule
{
    pub key: ModKey,
    pub imports: HashSet<&'static str>,
    pub macros: HashMap<&'static str, Ast>,
    pub constants: Vec<AstNode>,
    pub types: HashMap<&'static str, Type>,
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
            types: HashMap::new(),
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
                Ast::DefFunc(name, args, body) => {
                    proto.add_func(name, args, body)?;
                }
                Ast::DefType(DataType::Struct, name, fields) => {
                    if fields.is_empty() {
                        proto.add_token(name)?;
                    } else {
                        proto.add_struct(name, fields)?;
                    }
                }
                Ast::DefType(DataType::Union, name, variants) => {
                    proto.add_union(name, variants)?;
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

    fn add_func(&mut self, _name: AstNode, _args: Xlist, _body: AstNode) -> Lresult<()>
    {
        // self.funcs.push(i);
        Ok(())
    }

    fn add_struct(&mut self, _name: AstNode, _fields: Xlist) -> Lresult<()>
    {
        // proto.types.push(i);
        Ok(())
    }

    fn add_token(&mut self, name: AstNode) -> Lresult<()>
    {
        match *name.node {
            Ast::Id1(name_id) => {
                let lri = Lri::new(Lstr::from(name_id));
                self.types.insert(name_id, Type::UserDef(lri));
            }
            invalid_name => {
                return Err(rustfail!(
                    "semantic_failure",
                    "invalid token name: {:?}",
                    invalid_name,
                ));
            }
        }
        Ok(())
    }

    fn add_union(&mut self, _name: AstNode, _variants: Xlist) -> Lresult<()>
    {
        // proto.types.push(i);
        Ok(())
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


#[cfg(test)]
mod tests
{
    use super::ProtoLib;
    use crate::leema::loader::Interloader;
    use crate::leema::lri::Lri;
    use crate::leema::lstr::Lstr;
    use crate::leema::val::Type;

    #[test]
    fn test_proto_token()
    {
        let input = "type Burrito --";
        let mut loader = Interloader::new(Lstr::Sref("foo.lma"), "lib");
        loader.set_mod_txt(Lstr::Sref("foo"), input.to_string());
        let mut lib = ProtoLib::new();
        lib.load(&mut loader, &Lstr::from("foo")).expect("foo load failure");
        let proto = lib.get("foo").expect("no foo ProtoMod");

        let burrito_type = proto.types.get("Burrito").expect("no Burrito type");
        assert_eq!(Type::UserDef(Lri::new(Lstr::from("Burrito"))), *burrito_type);
    }
}
