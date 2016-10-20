
use leema::val::{Val};
use leema::compile::{Iexpr, Source};
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
    open file
    return init_module( parse(lex(read(file))))
--
import module(depth) ->
    m = read_module
    assign imports
    assign macros
    assign raw funcs
    assign type0 funcs
--
load_module ->
    if module_loaded {
        return loaded_module
    }
    return import module
--
load_func(mod, func) ->
    m = self.load_module(mod)
    f0 = m.get_type0_func(func)
    ft = self.resolve_types(f0)
--

load_code(mod, func): Code ->
--
load_code_by_type(mod, func, param_types): Code ->
--

typecheck_module(mod) ->
    let m = load_module(mod)
    for import_mod in m.imports {
        typecheck_module(import_mod)
    }
    for f in m.functions {
        self.resolve_types(f)
    }
--
*/

impl Interloader
{
    pub fn load_func(&mut self, module: &str, func: &str) -> Iexpr
    {
        let smod = self.read_module(module);
        let sfunc = self.find_sfunc(module, func);
        let ifunc = self.import_module(sfunc);
        self.interize(sfunc)
    }

    pub fn load_module(&mut self, module: &str) -> &Module
    {
        if !self.module.contains_key(module) {
            self.import_module(module);
        }
        self.module.get(module).unwrap()
    }

    pub fn read_module(&mut self, module: &str) -> &Val
    {
        let mod_fname = module_filename(module);
        let smod = ast::parse(mod_fname);
        self.module.new(module, smod)
    }

    fn import_module(&mut self, module: &str)
    {
        /*
        m = read_module
        assign imports
        assign macros
        assign raw funcs
        assign type0 funcs
        */

        let mod_fname = module_filename(module);
        let smod = ast::parse(mod_fname);

        let (imports, makros, rem_smod) = Sexpr::split_module(smod);
        /*
        for imp in imports {
            if primary {
                self.import_module(imp, false);
            } else {
                self.add_import(imp);
            }
        }
        */
        for makro in makros {
            self.add_macro(module, makro);
        }
    }

    fn find_sfunc(&mut self, module: &str, func: &str) -> Option<&Val>
    {
        /*
        let opt_smod = self.smod.get(module);
        if opt_smod.is_none() {
            return opt_smod;
        }
        */
        // for f in Sexpr::iter(smod.unwrap()) {}
        None
    }

    fn interize(&mut self, smod: &Val) -> Iexpr
    {
        /*
        imod = HashMap::new();
        for func in smod {
            //self.scope.push_func_scope();
            let ifunc = vec![];
            for code in func {
                ifunc.push(self.interize(code));
            }
            //pop_func_scope();
            imod.insert(func.name, ifunc);
        }
        imod
        */
        Iexpr::new(Source::Void)
    }

    pub fn resolve_types(ifunc: Iexpr) -> Iexpr
    {
        /*
        for e in ifunc {
            if e.t resolves to t' {
                e.t = t'
            } else {
                if e is function {
                    inner_ifunc = get_ifunc(e.mod, e.func)
                    resolve_types(inner_ifunc)
                }
            }
        }
            */
        Iexpr::new(Source::Void)
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

