
use leema::ast;
use leema::iexpr::{Iexpr, Source};
use leema::module::{ModKey};
use leema::phase0::{Protomod};
use leema::program;
use leema::val::{self, Val, SexprType};

use std::collections::{HashMap, HashSet};
use std::fmt;
use std::rc::{Rc};
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
    key: Rc<ModKey>,
    interfunc: HashMap<String, Iexpr>,
}

impl Intermod
{
    pub fn new(key: Rc<ModKey>) -> Intermod
    {
        Intermod{
            key: key,
            interfunc: HashMap::new(),
        }
    }

    pub fn name(&self) -> &str
    {
        &self.key.name
    }
}

pub fn compile(proto: &Protomod, prog: &program::Lib) -> Intermod
{
    Intermod::new(proto.key.clone())
}

impl fmt::Debug for Intermod
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "Intermod{{\n").ok();
        write!(f, "\tname: {}\n", self.key.name).ok();
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
