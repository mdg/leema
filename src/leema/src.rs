use leema::val::{Val, Type, SexprType};
use leema::iexpr::{Iexpr, Source};
use leema::lex::{lex};

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};


struct Srcmod
{
    name: String,
    file: Option<PathBuf>,
    // version: Option<Version>,
    srctext: String,
    sexpr: Val,
    imports: HashSet<String>,
    macros: HashMap<String, Val>,
    srcfunc: HashMap<String, Val>,
}

/*
impl Srcmod
{
    pub fn new(name: String, file: Option<PathBuf>, tokens: Vec<Token>)
    {
        let tokens = lex(&text);
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
            // srctext: content,
            sexpr: smod,
            imports: imports, // prog.imports,
            macros: makros, // prog.macros,
            srcfunc: srcfunc,
            interfunc: interfunc,
        }
    }
}
*/

pub fn compile_mod(m: Val) -> Iexpr
{
    match m {
        Val::Sexpr(st, sx) => {
            compile_mod_sexpr(st, *sx)
        }
        Val::Int(i) => {
            Iexpr::const_val(m)
        }
        _ => {
            Iexpr::const_val(Val::Void)
        }
    }
}

pub fn compile_mod_sexpr(st: SexprType, sx: Val) -> Iexpr
{
    Iexpr::const_val(Val::Void)
}
