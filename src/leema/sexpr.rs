use leema::val::{Val, SexprType, Type};
use leema::list;
use std::fmt;

/**
 * define function
 *
 * define structure
 * define protocol
 * implement protocol
 *
 * type term
 * function type
 * function parameter list
 * code block
 *
 * define macro
 */

/**
 * Val
 * Val -> List
 * Val -> Sexpr
 * Val -> Ast
 *
 * Sexpr
 * Sexpr -> Val
 * Sexpr -> List
 *
 * List -> Val
 * List -> Sexpr
 * List -> Ast
 *
 * Ast -> Val
 * Ast -> Sexpr
 */

pub fn new(t: SexprType, v: Val) -> Val
{
    Val::Sexpr(t, Box::new(v))
}

pub fn is_type(v: &Val, st: SexprType) -> bool
{
    match v {
        &Val::Sexpr(st, _) => true,
        _ => false,
    }
}

pub fn new_block(lst: Val) -> Val
{
    Val::Sexpr(SexprType::BlockExpr, Box::new(lst))
}

pub fn strexpr(strs: Val) -> Val
{
    if list::is_empty(&strs) {
        Val::new_str("".to_string())
    } else if list::is_singleton(&strs) {
        list::head(strs)
    } else {
        new(SexprType::StrExpr, strs)
    }
}

pub fn call(callid: Val, input: Vec<Val>) -> Val
{
    let args = Val::Tuple(input);
    let callargs = list::cons(callid, list::singleton(args));
    Val::Sexpr(SexprType::Call, Box::new(callargs))
}

pub fn binaryop(callname: String, a: Val, b: Val) -> Val
{
    call(Val::id(callname), vec![a, b])
}

pub fn macro_from_func(f: Val) -> Val
{
    match f {
        Val::Sexpr(SexprType::DefFunc, f) => {
            Val::Sexpr(SexprType::DefMacro, f)
        }
        _ => panic!("Cannot create macro from not func"),
    }
}

pub fn casex(cond: Val, truth: Val, lies: Val) -> Val
{
    Val::Sexpr(SexprType::CaseExpr, Box::new(
        list::cons(cond,
        list::cons(truth,
        list::cons(lies,
        Val::Nil
        )))
    ))
}

pub fn match_expr(x: Val, cases: Val) -> Val
{
    Val::Sexpr(SexprType::MatchExpr, Box::new(
        list::cons(x,
        list::cons(cases,
        Val::Nil
        ))
    ))
}

pub fn ifstmt(cond: Val, ifblock: Val, elseblock: Val) -> Val
{
    Val::Sexpr(SexprType::IfStmt, Box::new(
        list::cons(cond,
        list::cons(ifblock,
        list::cons(elseblock,
        Val::Nil
        )))
    ))
}

pub fn defunc(name: Val, args: Val, typ: Val, blk: Val, ps: Val) -> Val
{
    Val::Sexpr(SexprType::DefFunc, Box::new(
        list::cons(name,
        list::cons(args,
        list::cons(typ,
        list::cons(blk,
        list::cons(ps,
        Val::Nil
        )))))
    ))
}

pub fn def_struct(name: Val, fields: Val) -> Val
{
    Val::Sexpr(SexprType::DefStruct, Box::new(
        list::cons(name, fields)
    ))
}

pub fn split(x: Val) -> (SexprType, Val)
{
    match x {
        Val::Sexpr(st, sx) => {
            (st, *sx)
        }
        _ => {
            panic!("Cannot split a not sexpr: {:?}", x);
        }
    }
}


/*
#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
struct TypedId {
    tid: String,
    texpr: Type,
}

impl TypedId {
    pub fn new(id: String, typ: Type) -> TypedId
    {
        TypedId{
            tid: id,
            texpr: typ,
        }
    }
}

*/


#[cfg(test)]
mod tests {
    use leema::val::{Val, SexprType};
    use leema::list;
    use leema::sexpr;
    use std::collections::HashMap;
    use std::rc::Rc;
    use std::sync::Arc;

#[test]
fn test_ast_replace_id()
{
    let str_a = "a".to_string();
    let node = Val::id(str_a.clone());

    let mut idvals = HashMap::new();
    idvals.insert(Arc::new(str_a), Val::Int(5));

    let actual = Val::replace_ids(node, &idvals);
    assert_eq!(Val::Int(5), actual);
}

#[test]
fn test_sexpr_empty_call()
{
    let c = sexpr::call(Val::id("testf".to_string()), vec![]);
    let expected = sexpr::new(SexprType::Call,
        list::cons(Val::id("testf".to_string()),
        list::cons(Val::Tuple(vec![]),
        Val::Nil
    )));

    assert_eq!(expected, c);
}

/*
#[test]
fn test_ast_replace_id_and_macro() {
    let if_case = Sexpr::IfCase(Box::new(Sexpr::Id("a".to_string())),
        Box::new(Sexpr::Id("b".to_string()))
    );
    let else_case = Sexpr::ElseCase(Box::new(Sexpr::val(Val::Bool(false))));
    let if_expr = Sexpr::IfExpr(List::from_sexpr_vec(vec![
        if_case,
        else_case,
    ]));

    let mut idvals = HashMap::new();
    let str_a = "a".to_string();
    idvals.insert(&str_a, Sexpr::val(Val::Bool(true)));
    let str_b = "b".to_string();
    idvals.insert(&str_b, Sexpr::val(Val::Bool(true)));

    let actual = Sexpr::replace_ids(if_expr, &idvals);
    assert_eq!(Val::Int(5), actual);
}
*/

}
