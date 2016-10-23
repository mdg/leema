
use leema::val::{Val};
use leema::compile::{Iexpr, Source};
use leema::ast::{Ast};
use leema::lex::{lex};

use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::path::{Path, PathBuf};
use std::io::Read;
use std::borrow::{Cow};


#[derive(PartialEq)]
#[derive(Eq)]
#[derive(PartialOrd)]
#[derive(Ord)]
#[derive(Clone)]
#[derive(Copy)]
#[derive(Debug)]
pub enum Version
{
    Sin,
    Cos,
}


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


#[derive(Debug)]
pub struct Intermod
{
    name: String,
    file: Option<PathBuf>,
    version: Option<Version>,
    srctext: String,
    sexpr: Val,
    imports: HashSet<String>,
    macros: HashMap<String, Val>,
    srcfunc: HashMap<String, Val>,
    interfunc: HashMap<String, Iexpr>,
}

impl Intermod
{
    pub fn new(name: &str, fname: Option<PathBuf>, ver: Option<Version>
        , content: String
    ) -> Intermod
    {
        let tokens = lex(&content);
        let smod_ast = Ast::parse(tokens.clone());
        let smod = Ast::root(smod_ast);
        let imports = HashSet::new();
        let makros = HashMap::new();
        let srcfunc = HashMap::new();
        let interfunc = HashMap::new();

        Intermod{
            name: String::from(name),
            file: fname,
            version: ver,
            srctext: content,
            sexpr: smod,
            imports: imports,
            macros: makros,
            srcfunc: srcfunc,
            interfunc: interfunc,
        }
    }

    pub fn name(name_or_file: &str) -> String
    {
        String::from(name_or_file)
    }

    pub fn filename(module_name: &str) -> String
    {
        format!("{}.lma", module_name)
    }
}


#[derive(Debug)]
pub struct Interloader
{
    version: Option<Version>,
    path: PathBuf,
    modules: HashMap<String, String>,
}

impl Interloader
{
    pub fn new() -> Interloader
    {
        Interloader{
            version: None,
            path: Path::new(".").to_path_buf(),
            modules: HashMap::new(),
        }
    }

    pub fn add_path(&mut self, path: &Path)
    {
        self.path = path.to_path_buf();
    }

    pub fn set_module(&mut self, modname: &str, content: String)
    {
        self.modules.insert(String::from(modname), content);
    }

    pub fn load_func(&mut self, module: &str, func: &str) -> Iexpr
    {
        /*
        let smod = self.read_module(module);
        let sfunc = self.find_sfunc(module, func);
        let ifunc = self.import_module(sfunc);
        self.interize(sfunc)
        */
        Iexpr::new(Source::Void)
    }

    pub fn module_path(&self, mod_name: &str) -> PathBuf
    {
        let mut path = PathBuf::new();
        path.push(self.path.as_path());
        path.push(mod_name);
        path.set_extension("lma");
        path
    }

    pub fn load_module(&self, mod_name: &str) -> Intermod
    {
        let fname;
        let content = if self.modules.contains_key(mod_name) {
            fname = None;
            self.modules.get(mod_name).unwrap().clone()
        } else {
            let path = self.module_path(mod_name);
println!("self path: {:?}", self.path);
            if !path.exists() {
                panic!("Module file does not exist: {:?}", path);
            }
            if !path.is_file() {
                panic!("Module is not a file: {:?}", path);
            }
            fname = Some(path.clone());

            let mut f = File::open(path).ok().unwrap();
            let mut result = String::new();
            f.read_to_string(&mut result);
            result
        };
        Intermod::new(mod_name, fname, self.version, content)
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
}


#[cfg(test)]
mod tests
{
    use leema::inter::{Interloader};
    use std::path::Path;

#[test]
fn test_module_path()
{
    let mut i = Interloader::new();
    i.add_path(Path::new("hello"));
    let mp = i.module_path("world");

    let actual = Path::new("hello/world.lma");
    assert_eq!(actual, mp.as_path());
}

}
