use leema::val::{Val, Type, SexprType};
use leema::iexpr::{Iexpr, Source};
use leema::lex::{lex};
use leema::list;
use leema::module::{Module};


pub fn compile_mod(m: Val) -> Iexpr
{
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
        Val::Tuple(tup) => {
            Iexpr::new_tuple(tup.into_iter().map(|x| {
                compile_expr(x)
            }).collect())
        }
        Val::Id(_) => {
            Iexpr::const_val(x)
        }
        Val::TypedId(_, _) => {
            Iexpr::const_val(x)
        }
        Val::Wildcard => {
            Iexpr::const_val(x)
        }
        Val::Void => {
            Iexpr::const_val(x)
        }
        _ => {
            panic!("Cannot compile: {:?}", x)
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
        SexprType::Call => {
            let (rname, rargs) = list::to_tuple2(sx);
            let name = compile_expr(rname);
            let args = compile_expr(rargs);
            Iexpr::new_call(name, args)
        }
        SexprType::DefFunc => {
            let (name, args, result_type, body) = list::to_tuple4(sx);
            compile_def_func(name, args, result_type, body)
        }
        SexprType::FieldAccess => {
            let (rbase, rfield) = list::to_tuple2(sx);
            let base = compile_expr(rbase);
            Iexpr::new_field_access(base, rfield)
        }
        SexprType::IfStmt => {
            let (rtest, rtruthy, rfalsy) = list::to_tuple3(sx);
            let test = compile_expr(rtest);
            let truthy = compile_expr(rtruthy);
            let falsy = compile_expr(rfalsy);
            Iexpr::new_if(test, truthy, falsy)
        }
        SexprType::Let => {
            let (pattern, sx2) = list::take(sx);
            let (rhs, _sx3) = list::take(sx2);
            compile_let(pattern, rhs)
        }
        SexprType::MatchExpr => {
            let (rinput, rcases) = list::to_tuple2(sx);
            let input = compile_expr(rinput);
            let cases = compile_match_case(rcases);
            Iexpr::new_match_expr(input, cases)
        }
        SexprType::CaseExpr => {
            let (rtest, rtruthy, rfalsy) = list::to_tuple3(sx);
            let test = compile_expr(rtest);
            let truthy = compile_expr(rtruthy);
            let falsy = compile_expr(rfalsy);
            Iexpr::new_when_expr(test, truthy, falsy)
        }
        SexprType::StrExpr => {
            let items = compile_list_to_vec(sx);
            Iexpr::new_str_mash(items)
        }
        SexprType::Import => {
            let modname = list::head(sx);
            Iexpr::new_import(modname)
        }
        SexprType::Fork => {
            panic!("Cannot compile fork: {:?}/{:?}", st, sx);
        }
        _ => {
            panic!("Cannot compile sexpr: {:?}/{:?}", st, sx);
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

pub fn compile_match_case(mcase: Val) -> Iexpr
{
    if mcase == Val::Void {
        return Iexpr::noop();
    }
    let (rpattern, e2) = list::take(mcase);
    let (rbody, e3) = list::take(e2);
    let rnext = list::head_or(e3, Val::Void);

    let pattern = compile_expr(rpattern);
    let body = compile_expr(rbody);
    let next = compile_match_case(rnext);

    Iexpr::new_match_case(pattern, body, next)
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
