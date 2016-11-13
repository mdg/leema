
use leema::ast;
use leema::iexpr::{Iexpr, Source};
use leema::lex::{lex};
use leema::val::{self, Val, SexprType};

use std::collections::{HashMap, HashSet};
use std::fmt;
use std::path::{PathBuf};
use std::fs::File;
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
