
use leema::val::{Val};
use leema::compile::{Iexpr, Source};
use leema::ast::{Ast};
use leema::lex::{lex};
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::Read;


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


/*
struct module
- name
- list imports
- map str:val makros
- // map str:Val raw_types
- map str:Val raw_func
- map str:Type type0_types
- map str:Iexpr type0_func
- map str:Type typen_types
- map str:Iexpr typen_func
*/
pub struct Intermod
{
    name: String,
    file: String,
    version: Version,
    srctext: String,
    sexpr: Val,
    imports: HashSet<String>,
    macros: HashMap<String, Val>,
    srcfunc: HashMap<String, Val>,
    interfunc: HashMap<String, Iexpr>,
}

impl Intermod
{
    pub fn new(name: &str, fname: &str, ver: Version, content: String) -> Intermod
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
            file: String::from(fname),
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


pub struct Interloader
{
    files: HashMap<String, String>,
}

impl Interloader
{
    pub fn new() -> Interloader
    {
        Interloader{
            files: HashMap::new(),
        }
    }

    pub fn set_file(&mut self, modname: &str, content: String)
    {
        self.files.insert(String::from(modname), content);
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

    pub fn load_module(&mut self, mod_name: &str, ver: Version) -> Intermod
    {
        let mod_fname = Intermod::filename(mod_name);
        let mod_content = self.read_file(&mod_fname);
        Intermod::new(mod_name, &mod_fname, ver, mod_content)
    }

    pub fn read_file(&self, file_name: &str) -> String
    {
        if self.files.contains_key(file_name) {
            self.files.get(file_name).unwrap().clone()
        } else {
            let mut result = String::new();
            let mut f = File::open(file_name).ok().unwrap();
            f.read_to_string(&mut result);
            result
        }
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

