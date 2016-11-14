
use leema::val::{Val};
use leema::iexpr::{Iexpr, Source};
use leema::inter::{Intermod, Version};
use leema::module::{Module, ModKey, ModSrc};
use leema::src;
use leema::parse::{Token};

use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::collections::{HashMap, HashSet};


#[derive(Debug)]
pub struct Interloader
{
    version: Option<Version>,
    pub root_path: PathBuf,
    pub main_mod: String,
    modtxt: HashMap<String, String>,
}

impl Interloader
{
    pub fn new(mainfile: &str) -> Interloader
    {
        let path = Path::new(&mainfile);
        let ext = path.extension();
        if ext.is_none() {
            panic!("Main file has no extension: {}", mainfile);
        }
        if ext.unwrap() != "lma" {
            panic!("Main file extension is not lma: {}", mainfile);
        }
        let modname = path.file_stem();
        if modname.is_none() {
            panic!("Is that not a real file? {}", mainfile);
        }

        Interloader{
            version: None,
            root_path: path.parent().unwrap().to_path_buf(),
            main_mod: modname.unwrap().to_str().unwrap().to_string(),
            modtxt: HashMap::new(),
        }
    }

    pub fn set_mod_txt(&mut self, modname: &str, content: String)
    {
        self.modtxt.insert(String::from(modname), content);
    }

    pub fn load_func(&mut self, module: &str, func: &str) -> Iexpr
    {
        /*
        let smod = self.read_module(module);
        let sfunc = self.find_sfunc(module, func);
        let ifunc = self.import_module(sfunc);
        self.interize(sfunc)
        */
        Iexpr::const_val(Val::Void)
    }

    pub fn mod_name_to_key(&self, mod_name: &str) -> ModKey
    {
        if self.modtxt.contains_key(mod_name) {
            ModKey::name_only(mod_name)
        } else {
            let mut path = PathBuf::new();
            path.push(self.root_path.as_path());
            path.push(mod_name);
            path.set_extension("lma");
            ModKey::new(mod_name, path)
        }
    }

    pub fn init_module(&self, mod_key: ModKey) -> Module
    {
        let txt = if mod_key.file.is_none() {
            self.modtxt.get(&mod_key.name).unwrap().clone()
        } else {
            Interloader::read_file_text(mod_key.file.as_ref().unwrap())
        };
        Module::new(mod_key, txt)
    }

    pub fn read_file_text(path: &Path) -> String
    {
        if !path.exists() {
            panic!("Module file does not exist: {:?}", path);
        }
        if !path.is_file() {
            panic!("Module is not a file: {:?}", path);
        }
        let mut f = File::open(path).ok().unwrap();
        let mut result = String::new();
        f.read_to_string(&mut result);
        result
    }

    fn import_module(&mut self, modname: &str)
    {
        /*
        m = read_module
        assign imports
        assign macros
        assign raw funcs
        assign type0 funcs

        let mod_fname = module::filename(modname);

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
        */
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
        Iexpr::const_val(Val::Void)
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
        Iexpr::const_val(Val::Void)
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
}


#[cfg(test)]
mod tests
{
    use leema::loader::{Interloader};
    use std::path::{Path, PathBuf};

#[test]
fn test_root_path()
{
    let mut i = Interloader::new("hello/world.lma");

    let expected = Path::new("hello");
    assert_eq!(expected, i.root_path);
}

#[test]
fn test_main_mod()
{
    let mut i = Interloader::new("hello/world.lma");

    assert_eq!("world", i.main_mod);
}

}
