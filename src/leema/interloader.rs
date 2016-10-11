

pub struct Interloader
{
    file: HashMap<String, String>,
    smod: HashMap<String, Smodule>,
    interfunc: HashMap<(String, String), Iexpr>
}

/*
Smodule =
|imports
|makros
|types
|funcs
|repl_stmt
--

Imodule =
|imports
|makros
|types
|func
--
*/

impl Interloader
{
    pub fn new() -> Interloader
    {
        Interloader{
            file: HashMap::new(),
            smod: HashMap::new(),
            interfunc: HashMap::new(),
        }
    }

    pub fn load_func(&mut self, module: &str, func: &str) -> Iexpr
    {
        let smod = self.import_module(module, func, true);
        let sfunc = self.find_sfunc(module, func);
        self.interize(sfunc)
    }

    pub fn load_module(&mut self, module: &str)
    {
        self.import_module(module, true);
    }

    pub fn import_module(&mut self, module: &str, recurse: bool) -> Val
    {
        let fmod = open_file(module);
        let smod = parse(fmod);
        for import in smod.imports {
            if recurse {
                self.import_module(import, false);
            } else {
                self.add_import(import);
            }
        }
        for makro in smod.makros {
            self.add_macro(module, makro);
        }
        self.store_smod(module, smod);
        smod
    }

    fn find_sfunc(&mut self, module: &str, func: &str) -> &Val
    {
        let smod = self.smods.get(module);
        for f in smod {
        }
    }

    fn interize(&mut self, imod: Val) -> Iexpr
    {
        imod = HashMap::new();
        for func in smod {
            push_func_scope()
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
}

