use leema::val::{Val, SxprType, Type, FuncCallType};
use leema::list;

use std::rc::{Rc};

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
 * Val -> Sxpr
 * Val -> Ast
 *
 * Sxpr
 * Sxpr -> Val
 * Sxpr -> List
 *
 * List -> Val
 * List -> Sxpr
 * List -> Ast
 *
 * Ast -> Val
 * Ast -> Sxpr
 */

pub fn new(t: SxprType, v: Val) -> Val
{
    Val::Sxpr(t, Box::new(v))
}

pub fn is_type(v: &Val, st: SxprType) -> bool
{
    match v {
        &Val::Sxpr(st, _) => true,
        _ => false,
    }
}

pub fn new_block(lst: Val) -> Val
{
    Val::Sxpr(SxprType::BlockExpr, Box::new(lst))
}

fn strexpr_mash(merge: Val, next: Val) -> (Option<Val>, Val)
{
    match (merge, next) {
        (Val::Str(merge_str), Val::Str(next_str)) => {
            let new_merge = Val::new_str(format!("{}{}", merge_str, next_str));
            (None, new_merge)
        }
        (merge1, next1) => {
            (Some(merge1), next1)
        }
    }
}

pub fn strexpr(strs: Val) -> Val
{
    if list::is_empty(&strs) {
        Val::new_str("".to_string())
    } else if list::is_singleton(&strs) {
        list::head(strs)
    } else {
        let mashed = list::merge_adjacent(strs, strexpr_mash);
        new(SxprType::StrExpr, mashed)
    }
}

pub fn call(callid: Val, args: Val) -> Val
{
    let callargs = list::cons(callid, args);
    Val::Sxpr(SxprType::Call, Box::new(callargs))
}

pub fn named_param(name: Val, expr: Val) -> Val
{
    let parts = list::from2(name, expr);
    Val::Sxpr(SxprType::NamedParam, Box::new(parts))
}

pub fn binaryop(callname: String, a: Val, b: Val) -> Val
{
    call(Val::id(callname), list::from2(a, b))
}

pub fn macro_from_func(f: Val) -> Val
{
    match f {
        Val::Sxpr(SxprType::DefFunc, f) => {
            Val::Sxpr(SxprType::DefMacro, f)
        }
        _ => panic!("Cannot create macro from not func"),
    }
}

pub fn ifx(cond: Val, truth: Val, lies: Val) -> Val
{
    Val::Sxpr(SxprType::IfExpr, Box::new(
        list::cons(cond,
        list::cons(truth,
        list::cons(lies,
        Val::Nil
        )))
    ))
}

pub fn match_expr(x: Val, cases: Val) -> Val
{
    Val::Sxpr(SxprType::MatchExpr, Box::new(
        list::cons(x,
        list::cons(cases,
        Val::Nil
        ))
    ))
}

pub fn defunc(name: Val, args: Val, typ: Val, blk: Val, ps: Val) -> Val
{
    Val::Sxpr(SxprType::DefFunc, Box::new(
        list::cons(name,
        list::cons(args,
        list::cons(typ,
        list::cons(blk,
        list::cons(ps,
        Val::Nil
        )))))
    ))
}

pub fn defunc_type(defunc: &Val) -> Type
{
    if !defunc.is_sxpr_type(SxprType::DefFunc) {
        panic!("Cannot find function type of not function: {:?}", defunc);
    }
    let (_, sx) = split_ref(defunc);
    let (nameval, argvals, resultval, bodyval) = list::to_ref_tuple4(sx);
    let name = nameval.to_str();
    let argt = list::fold_ref(vec![], argvals, |mut r, a| {
        let typ = a.get_type();
        let argtype = match typ {
            Type::AnonVar => {
                Type::Var(Rc::new(format!("T_{}_{}", name, a.to_str())))
            }
            _ => {
                typ.clone()
            }
        };
        r.push(argtype);
        r
    });
    let tresult = match resultval {
        &Val::Type(Type::AnonVar) => {
            Type::Var(Rc::new(format!("Tresult_{}", name)))
        }
        &Val::Type(ref innert) => {
            innert.clone()
        }
        _ => {
            panic!("Result type of {} is not a type: {:?}", name, resultval);
        }
    };
    let calltype =
        match bodyval {
            &Val::RustBlock => FuncCallType::IoCall,
            _ => FuncCallType::FrameCall,
        };
    Type::Func(calltype, argt, Box::new(tresult))
}

pub fn def_struct(name: Val, fields: Val) -> Val
{
    Val::Sxpr(SxprType::DefStruct, Box::new(
        list::cons(name, fields)
    ))
}

pub fn new_import(name: Val) -> Val
{
    Val::Sxpr(SxprType::Import, Box::new(
        list::singleton(name)
    ))
}

pub fn split(x: Val) -> (SxprType, Val)
{
    match x {
        Val::Sxpr(st, sx) => {
            (st, *sx)
        }
        _ => {
            panic!("Cannot split a not sxpr: {:?}", x);
        }
    }
}

pub fn split_ref(x: &Val) -> (SxprType, &Val)
{
    match x {
        &Val::Sxpr(st, ref sx) => {
            (st, sx)
        }
        _ => {
            panic!("Cannot split a not sxpr: {:?}", x);
        }
    }
}


#[cfg(test)]
mod tests {
    use leema::val::{Val, SxprType};
    use leema::list;
    use leema::sxpr;
    use std::collections::HashMap;
    use std::rc::Rc;

#[test]
fn test_ast_replace_id()
{
    let str_a = "a".to_string();
    let node = Val::id(str_a.clone());

    let mut idvals = HashMap::new();
    idvals.insert(Rc::new(str_a), Val::Int(5));

    let actual = Val::replace_ids(node, &idvals);
    assert_eq!(Val::Int(5), actual);
}

#[test]
fn test_sxpr_empty_call()
{
    let c = sxpr::call(Val::id("testf".to_string()), Val::Nil);
    let expected = sxpr::new(SxprType::Call,
        list::singleton(Val::id("testf".to_string())),
    );

    assert_eq!(expected, c);
}

#[test]
fn test_strexpr_merge_end()
{
    let strs = list::from3(
        Val::id("greeting".to_string()),
        Val::new_str(" world".to_string()),
        Val::new_str("\n".to_string()),
    );
    let strx = sxpr::strexpr(strs);

    let (sxtype, sx) = sxpr::split(strx);
    assert_eq!(SxprType::StrExpr, sxtype);
    assert_eq!(2, list::len(&sx));

    let (h1, t1) = list::take_ref(&sx);
    assert_eq!(Val::id("greeting".to_string()), *h1);
    let (h2, t2) = list::take_ref(t1);
    assert_eq!(Val::new_str(" world\n".to_string()), *h2);
    assert_eq!(Val::Nil, *t2);
}

/*
#[test]
fn test_ast_replace_id_and_macro() {
    let if_case = Sxpr::IfCase(Box::new(Sxpr::Id("a".to_string())),
        Box::new(Sxpr::Id("b".to_string()))
    );
    let else_case = Sxpr::ElseCase(Box::new(Sxpr::val(Val::Bool(false))));
    let if_expr = Sxpr::IfExpr(List::from_sxpr_vec(vec![
        if_case,
        else_case,
    ]));

    let mut idvals = HashMap::new();
    let str_a = "a".to_string();
    idvals.insert(&str_a, Sxpr::val(Val::Bool(true)));
    let str_b = "b".to_string();
    idvals.insert(&str_b, Sxpr::val(Val::Bool(true)));

    let actual = Sxpr::replace_ids(if_expr, &idvals);
    assert_eq!(Val::Int(5), actual);
}
*/

}
