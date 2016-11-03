use leema::val::{Val, Type, SexprType};
use leema::iexpr::{Iexpr, Source};
use leema::lex::{lex};
use leema::list;

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
            compile_sexpr(st, *sx)
        }
        _ => {
            compile_expr(m)
        }
    }
}

pub fn compile_expr(x: Val) -> Iexpr
{
    match x {
        Val::Int(i) => {
            Iexpr::const_val(x)
        }
        Val::Str(s) => {
            Iexpr::const_val(Val::Str(s))
        }
        Val::Sexpr(st, sx) => {
            compile_sexpr(st, *sx)
        }
        _ => {
            Iexpr::const_val(Val::Void)
        }
    }
}

pub fn compile_sexpr(st: SexprType, sx: Val) -> Iexpr
{
    match st {
        SexprType::DefFunc => {
            let (name, sx2) = list::take(sx);
            let (args, sx3) = list::take(sx2);
            let (result_type, sx4) = list::take(sx3);
            let (body, _sx5) = list::take(sx4);
            compile_def_func(name, args, result_type, body)
        }
        SexprType::Let => {
            let (pattern, sx2) = list::take(sx);
            let (rhs, _sx3) = list::take(sx2);
            compile_let(pattern, rhs)
        }
        SexprType::Fork => { Iexpr::noop() }
        _ => {
            Iexpr::const_val(Val::Void)
        }
    }
}

pub fn compile_def_func(name: Val, args: Val, result_type: Val, body: Val)
    -> Iexpr
{
    Iexpr::const_val(Val::Void)
}

pub fn compile_let(patt: Val, val: Val) -> Iexpr
{
    let lhs = compile_pattern(patt);
    let rhs = compile_expr(val);
    Iexpr::new(Source::Let(Box::new(lhs), Box::new(rhs)))
}

pub fn compile_pattern(patt: Val) -> Iexpr
{
    Iexpr::const_val(patt)
}
