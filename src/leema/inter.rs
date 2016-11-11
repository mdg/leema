
use leema::ast;
use leema::iexpr::{Iexpr, Source};
use leema::lex::{lex};
use leema::module::{ModKey, Module};
use leema::parse::{Token};
use leema::sexpr;
use leema::src;
use leema::val::{self, Val, SexprType};

use std::collections::{HashMap, HashSet};
use std::fmt;
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
    //typedfunc: HashMap<String, Iexpr>,
}

impl Intermod
{
    pub fn new(name: &str, fname: Option<PathBuf>, ver: Option<Version>
        , content: String
    ) -> Intermod
    {
        let tokens = lex(&content);
        let smod = ast::parse(tokens.clone());
        let imports = HashSet::new();
        let makros = HashMap::new();
        let srcfunc = HashMap::new();
        let interfunc = HashMap::new();
        // let prog = split_program(smod.clone());

        Intermod{
            name: String::from(name),
            file: fname,
            version: ver,
            srctext: content,
            sexpr: smod,
            imports: imports, // prog.imports,
            macros: makros, // prog.macros,
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

    fn split_program_val(progval: Val) // -> Modval
    {
        match progval {
            Val::Sexpr(st, sx) => {
                Intermod::split_program_sexpr(st, *sx)
            }
            _ => {
                panic!("Program is not sexpr: {:?}", progval);
            }
        }
    }

    fn split_program_sexpr(st: SexprType, sx: Val)
    {
        match st {
            SexprType::BlockExpr => {
                // split_program_list(sx);
            }
            _ => {
                panic!("Program is not block: {:?}/{:?}", st, sx);
            }
        }
        /*
        Program{
            imports: vec![],
            macros: vec![],
            types: vec![],
            funcs: vec![],
        }
        */
    }
}

impl fmt::Debug for Intermod
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "Intermod{{\n").ok();
        write!(f, "\tname: {}\n", self.name).ok();
        write!(f, "\tfile: {:?}\n", self.file).ok();
        write!(f, "\tversion: {:?}\n", self.version).ok();
        write!(f, "\tsrctext: \"\"\"\n{:?}\"\"\"\n", self.srctext).ok();
        write!(f, "\tsexpr: {:?}\n", self.sexpr).ok();
        write!(f, "\timports: {:?}\n", self.imports).ok();
        write!(f, "\tmacros: {:?}\n", self.macros).ok();
        write!(f, "\tsrcfunc: {:?}\n", self.srcfunc).ok();
        write!(f, "\tinterfunc: {:?}\n", self.interfunc).ok();
        write!(f, "}}\n")
    /*
    imports: HashSet<String>,
    macros: HashMap<String, Val>,
    srcfunc: HashMap<String, Val>,
    interfunc: HashMap<String, Iexpr>,
    */
    }
}


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
        if !path.exists() {
            panic!("Path does not exist: {}", mainfile);
        }
        if !path.is_file() {
            panic!("Path is not a file: {}", mainfile);
        }
        let ext = path.extension();
        if ext.is_none() {
            panic!("Main file has no extension: {}", mainfile);
        }
        if ext.unwrap() == "lma" {
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

    pub fn mod_file_to_key(&self, path: &PathBuf) -> ModKey
    {
        let local_file = path.strip_prefix(&self.root_path).ok();
        println!("{:?}", local_file);
        ModKey::name_only(mod_name)
        /*
        if self.modtxt.contains_key(mod_name) {
        } else {
            let mut path = PathBuf::new();
            path.push(self.path.as_path());
            path.push(mod_name);
            path.set_extension("lma");
            ModKey::new(mod_name, path)
        }
        */
    }

    pub fn init_module(&self, mod_key: ModKey) -> Module
    {
        let txt = if mod_key.file.is_none() {
            self.modtxt.get(&mod_key.name).unwrap().clone()
        } else {
            Interloader::read_file_text(&mod_key.file.unwrap())
        };
        Module::new(mod_key, txt)
    }

    fn read_module(&self, mod_name: &str) -> (Option<PathBuf>, String)
    {
        let mk: ModKey = self.mod_name_to_key(mod_name);
        if mk.file.is_none() {
            (None, self.modtxt.get(mod_name).unwrap().clone())
        } else {
            let path: PathBuf = mk.file.unwrap();
            let txt = Interloader::read_file_text(&path);
            (Some(path), txt)
        }
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

    pub fn read_tokens(m: &mut Module) -> &Vec<Token>
    {
        m.tok = lex(&m.txt);
        m.tok
    }

    pub fn read_ast(m: &mut Module) -> &Val
    {
        m.ast = ast::parse(m.tok);
        &m.ast
    }

    pub fn read_inter(m: &mut Module) -> &Iexpr
    {
        src::compile_mod(m.ast)
    }

    pub fn load_module(&self, mod_name: &str) -> Intermod
    {
        let (filename, txt) = self.read_module(mod_name);
        let toks = lex(&txt);
        Intermod::new(mod_name, filename, self.version, txt)
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
    use leema::inter::{Interloader};
    use std::path::{Path, PathBuf};

#[test]
fn test_module_path()
{
    let mut i = Interloader::new();
    i.add_path(Path::new("hello"));
    let mp = i.module_path("world");

    let actual = Path::new("hello/world.lma");
    assert_eq!(actual, mp.unwrap().as_path());
}

}
