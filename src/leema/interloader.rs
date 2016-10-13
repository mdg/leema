
use leema::val::{Val};
use leema::compile::{Iexpr};
use leema::module;
use std::collections::{HashMap, HashSet};


/*
calling push leema code from rust
string module
sexpr module block
- raw list, imports, makros, codes

iexpr type0 func interface (and cache)
iexpr type0 func body (and cache)

iexpr type' func interface (and cache)
iexpr type' func body (and cache)

module scope
function scope

read_module ->
    open file, read contents, init Module
--
import module ->
    read_module, assign raw funcs, assign type0 funcs
--
load_module ->
    import module
--
load func(mod, func) ->
    m = self.import_module(mod)
    f0 = m.get_type0_func(func)
    ft = self.resolve_types(f0)
--
*/

pub struct Interloader
{
    module: HashMap<String, Module>,
}

impl Interloader
{
    pub fn new() -> Interloader
    {
        Interloader{
            module: HashMap::new(),
        }
    }

    pub fn load_func(&mut self, module: &str, func: &str) -> Iexpr
    {
        let smod = self.read_module(module);
        let sfunc = self.find_sfunc(module, func);
        let ifunc = self.import_module(sfunc);
        self.interize(sfunc)
    }

    pub fn load_module(&mut self, module: &str)
    {
        self.import_module(module, true);
    }

    fn read_module(&mut self, module: &str, primary: bool) -> &Val
    {
        if self.smod.contains_key(module) {
            return self.smod.get(module).unwrap();
        }

        let mod_fname = module_filename(module);
        let smod = ast::parse(mod_fname);
        self.smod.insert(module, smod);
        self.smod.get(module).unwrap()
    }

    fn import_module(&mut self, smod: &Val)
    {
        let (imports, makros, rem_smod) = Sexpr::split_module(smod);
        for imp in imports {
            if primary {
                self.import_module(imp, false);
            } else {
                self.add_import(imp);
            }
        }
        for makro in makros {
            self.add_macro(module, makro);
        }
        self.smod.insert(module, rem_smod);
    }

    fn import_module(&mut self, module: &str, primary: bool)
    {
        if self.smod.contains_key(module) {
            return self.smod.get(module).unwrap();
        }

        let mod_fname = module_filename(module);
        let smod = ast::parse(mod_fname);

        let (imports, makros, rem_smod) = Sexpr::split_module(smod);
        for imp in imports {
            if primary {
                self.import_module(imp, false);
            } else {
                self.add_import(imp);
            }
        }
        for makro in makros {
            self.add_macro(module, makro);
        }
        self.smod.insert(module, rem_smod);
    }

    fn find_sfunc(&mut self, module: &str, func: &str) -> Option<&Val>
    {
        let opt_smod = self.smod.get(module);
        if opt_smod.is_none() {
            return opt_smod;
        }
        // for f in Sexpr::iter(smod.unwrap()) {}
    }

    fn interize(&mut self, smod: &Val) -> Iexpr
    {
        imod = HashMap::new();
        for func in smod {
            self.scope.push_func_scope();
            let ifunc = vec![];
            for code in func {
                ifunc.push(self.interize(code));
            }
            pop_func_scope();
            imod.insert(func.name, ifunc);
        }
        imod
    }

    pub fn resolve_types(ifunc: Iexpr) -> Iexpr
    {
        for e in ifunc {
            /*
            if e.t resolves to t' {
                e.t = t'
            } else {
                if e is function {
                    inner_ifunc = get_ifunc(e.mod, e.func)
                    resolve_types(inner_ifunc)
                }
            }
            */
        }
    }

    fn add_import(&self, module: &str, import_mod: Val)
    {
    }

    fn add_macro(&self, module: &str, makro: Val)
    {
    }

    fn store_smod(&self, module: &str, smod: Val)
    {
        // self.smod.insert(module, smod);
        // let iftype = Interloader::interface_type(smod);
        // self.store_provisional_interface(module, iftype);
    }

    pub fn module_name(name_or_file: &str) -> String
    {
        name_or_file.to_string()
    }

    pub fn module_filename(name_or_file: &str) -> String
    {
        format!("{}.lma", module_name(name_or_file))
    }
}

