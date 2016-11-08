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


struct SrcState
{
    type_idx: u32,
}

impl SrcState
{
    pub fn new() -> SrcState
    {
        SrcState{
            type_idx: 0,
        }
    }

    pub fn next(&mut self) -> u32
    {
        let result = self.type_idx;
        self.type_idx += 1;
        result
    }
}


pub fn compile_mod(m: Val) -> Iexpr
{
    let ss = SrcState::new();
    let ix = compile_expr(m);
    ix
}

fn compile_expr(x: Val) -> Iexpr
{
    match x {
        Val::Sexpr(st, sx) => {
            compile_sexpr(st, *sx)
        }
        Val::Int(i) => {
            Iexpr::const_val(x)
        }
        Val::Str(s) => {
            Iexpr::const_val(Val::Str(s))
        }
        Val::Hashtag(_) => {
            Iexpr::const_val(x)
        }
        Val::Cons(_, _) => {
            let items = list::map_to_vec(x, compile_expr);
            Iexpr::new(Source::List(items))
        }
        Val::Nil => {
            Iexpr::const_val(x)
        }
        Val::Bool(_) => {
            Iexpr::const_val(x)
        }
        _ => {
            Iexpr::const_val(Val::Void)
        }
    }
}

pub fn compile_sexpr(st: SexprType, sx: Val) -> Iexpr
{
    match st {
        SexprType::BlockExpr => {
            let items = list::map_to_vec(sx, compile_expr);
            Iexpr::new_block(items)
        }
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
    let iname = compile_expr(name);
    let iargs = list::map_to_vec(args, |at| {
        compile_func_arg(at)
    });
    let args_type: Vec<Type> = iargs.iter().map(|a| {
        a.typ.clone()
    }).collect();
    let iresult_type = result_type.to_type();
    let ibody = compile_expr(body);
    let ftype = Type::Func(args_type, Box::new(iresult_type));
    Iexpr::def_func(iname, iargs, ibody, ftype)
}

pub fn compile_func_arg(arg: Val) -> Iexpr
{
    match arg {
        Val::Id(_) => {
            Iexpr::const_val(arg)
        }
        Val::TypedId(_, _) => {
            Iexpr::const_val(arg)
        }
        _ => {
            panic!("arg_type is not arg: {:?}", arg);
        }
    }
}

pub fn compile_let(patt: Val, val: Val) -> Iexpr
{
    let lhs = compile_pattern(patt);
    let rhs = compile_expr(val);
    Iexpr::new(Source::Let(Box::new(lhs), Box::new(rhs)))
}

fn compile_list_to_vec(items: Val) -> Vec<Iexpr>
{
    let mut it = items;
    let mut result = vec![];
    while it != Val::Nil {
        let (head, tail) = list::take(it);
        result.push(compile_expr(head));
        it = tail;
    }
    result
}

pub fn compile_pattern(patt: Val) -> Iexpr
{
    Iexpr::const_val(patt)
}


#[cfg(test)]
mod tests
{
use leema::src;
use leema::iexpr::{Iexpr, Source};
use leema::list;
use leema::sexpr;
use leema::val::{Val, SexprType, Type};

fn test_compile_int()
{
    let actual = src::compile_expr(Val::Int(7));
    let expected = Iexpr{
        src: Source::ConstVal(Val::Int(7)),
        typ: Type::Int,
    };
    assert_eq!(expected, actual);
}

fn test_compile_int_list()
{
    let actual = src::compile_expr(sexpr::new(SexprType::BlockExpr,
        list::cons(Val::Int(4),
        list::cons(Val::Int(9),
        Val::Nil))
    ));
    let expected = Iexpr{
        src: Source::List(vec![
            Iexpr::const_val(Val::Int(3)),
            Iexpr::const_val(Val::Int(9)),
        ]),
        typ: Type::Int,
    };
    assert_eq!(expected, actual);
}

}
