use leema::val::{Val, SxprType, Type, SrcLoc};
use leema::list;
use leema::lri::{Lri};

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

pub fn new(t: SxprType, v: Val, loc: SrcLoc) -> Val
{
    Val::Sxpr(t, Rc::new(v), loc)
}

pub fn replace_loc(sxval: Val, loc: SrcLoc) -> Val
{
    match sxval {
        Val::Sxpr(st, sx, _) => {
            Val::Sxpr(st, sx, loc)
        }
        _ => {
            panic!("cannot replace location in not-sxpr: {:?}@{:?}"
                , sxval, loc);
        }
    }
}

pub fn is_type(v: &Val, st: SxprType) -> bool
{
    match v {
        &Val::Sxpr(vst, _, _) if vst == st => true,
        _ => false,
    }
}

pub fn new_block(stmts: Val, loc: SrcLoc) -> Val
{
    Val::Sxpr(SxprType::BlockExpr, Rc::new(stmts), loc)
}

fn strexpr_mash(merge: &Val, next: &Val) -> Option<Val>
{
    match (merge, next) {
        (&Val::Str(ref merge_str), &Val::Str(ref next_str)) => {
            let new_merge = Val::new_str(format!("{}{}", merge_str, next_str));
            Some(new_merge)
        }
        _ => None,
    }
}

pub fn strexpr(strs: Val, loc: SrcLoc) -> Val
{
    if list::is_empty(&strs) {
        Val::new_str("".to_string())
    } else if list::is_singleton(&strs) {
        list::head(strs)
    } else {
        let mashed = list::merge_adjacent(&strs, strexpr_mash);
        new(SxprType::StrExpr, mashed, loc)
    }
}

pub fn lri(id: Val, loc: SrcLoc) -> Val
{
    Val::Sxpr(SxprType::Lri, Rc::new(list::singleton(id)), loc)
}

pub fn call(callid: Val, args: Val, loc: SrcLoc) -> Val
{
    let callargs = list::cons(callid, args);
    Val::Sxpr(SxprType::Call, Rc::new(callargs), loc)
}

pub fn named_param(name: Val, expr: Val, loc: SrcLoc) -> Val
{
    let parts = list::from2(name, expr);
    Val::Sxpr(SxprType::NamedParam, Rc::new(parts), loc)
}

pub fn binaryop(callname: String, a: Val, b: Val, loc: SrcLoc) -> Val
{
    call(Val::id(callname), list::from2(a, b), loc)
}

pub fn macro_from_func(f: Val) -> Val
{
    match f {
        Val::Sxpr(SxprType::DefFunc, f, loc) => {
            Val::Sxpr(SxprType::DefMacro, f, loc)
        }
        _ => panic!("Cannot create macro from not func"),
    }
}

pub fn ifx(cond: Val, truth: Val, lies: Val, loc: SrcLoc) -> Val
{
    Val::Sxpr(
        SxprType::IfExpr,
        Rc::new(
            list::cons(cond,
            list::cons(truth,
            list::cons(lies,
            Val::Nil
            )))),
        loc,
    )
}

pub fn match_expr(x: Val, cases: Val, loc: SrcLoc) -> Val
{
    Val::Sxpr(
        SxprType::MatchExpr,
        Rc::new(
            list::cons(x,
            list::cons(cases,
            Val::Nil
            ))),
        loc,
    )
}

pub fn match_failed(var: Val, cases: Val, loc: SrcLoc) -> Val
{
    Val::Sxpr(
        SxprType::MatchFailed,
        Rc::new(
            list::cons(var,
            list::cons(cases,
            Val::Nil
            ))
        ),
        loc,
    )
}

pub fn defunc(name: Val, args: Val, typ: Val, blk: Val, loc: SrcLoc) -> Val
{
    Val::Sxpr(
        SxprType::DefFunc,
        Rc::new(
            list::cons(name,
            list::cons(args,
            list::cons(typ,
            list::cons(blk,
            Val::Nil
            ))))),
        loc,
    )
}

pub fn defunc_type(defunc: &Val) -> Type
{
    if !defunc.is_sxpr_type(SxprType::DefFunc) {
        panic!("Cannot find function type of not function: {:?}", defunc);
    }
    let (_, sx, loc) = split_ref(defunc);
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
            panic!("Result type of {} is not a type: {:?} @ {:?}"
                , name, resultval, loc);
        }
    };
    Type::Func(argt, Box::new(tresult))
}

pub fn def_struct(name: Val, fields: Val, loc: SrcLoc) -> Val
{
    if !Val::is_list(&fields) {
        panic!("struct fields must be a list: {:?}", fields);
    }
    Val::Sxpr(
        SxprType::DefStruct,
        Rc::new(
            list::cons(name, fields)
        ),
        loc,
    )
}

pub fn def_enum(name: Lri, fields: Val, loc: SrcLoc) -> Val
{
    Val::Sxpr(
        SxprType::DefEnum,
        Rc::new(
            list::cons(Val::Lri(name), fields)
        ),
        loc,
    )
}

pub fn defnamedtuple(name: Type, fields: Val, loc: SrcLoc) -> Val
{
    Val::Sxpr(
        SxprType::DefNamedTuple,
        Rc::new(
            list::cons(Val::Type(name), fields)
        ),
        loc,
    )
}

pub fn new_import(name: Lri, loc: SrcLoc) -> Val
{
    Val::Sxpr(
        SxprType::Import,
        Rc::new(
            list::singleton(Val::Lri(name))
        ),
        loc,
    )
}

pub fn split(x: Val) -> (SxprType, Rc<Val>, SrcLoc)
{
    match x {
        Val::Sxpr(st, sx, loc) => {
            (st, sx, loc)
        }
        _ => {
            panic!("Cannot split a not sxpr: {:?}", x);
        }
    }
}

pub fn split_ref(x: &Val) -> (SxprType, &Val, &SrcLoc)
{
    match x {
        &Val::Sxpr(st, ref sx, ref loc) => {
            (st, sx, loc)
        }
        _ => {
            panic!("Cannot split a not sxpr: {:?}", x);
        }
    }
}


#[cfg(test)]
mod tests {
    use leema::val::{Val, SxprType, SrcLoc};
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

    let actual = Val::replace_ids(&node, &idvals);
    assert_eq!(Val::Int(5), actual);
}

#[test]
fn test_sxpr_empty_call()
{
    let loc = SrcLoc::new(8, 2);
    let c = sxpr::call(Val::id("testf".to_string()), Val::Nil, loc);
    let expected = sxpr::new(
        SxprType::Call,
        list::singleton(Val::id("testf".to_string())),
        loc,
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
    let strx = sxpr::strexpr(strs, SrcLoc::new(3, 4));

    let (sxtype, sx, loc) = sxpr::split(strx);
    assert_eq!(SxprType::StrExpr, sxtype);
    assert_eq!(2, list::len(&sx));
    assert_eq!(3, loc.lineno);
    assert_eq!(4, loc.column);

    let (h1, t1) = list::take_ref(&sx);
    assert_eq!(Val::id("greeting".to_string()), *h1);
    let (h2, t2) = list::take_ref(t1);
    assert_eq!(Val::new_str(" world\n".to_string()), *h2);
    assert_eq!(Val::Nil, **t2);
}

#[test]
fn test_def_struct_debug()
{
    let ds = sxpr::def_struct(
        Val::id("Taco".to_string()),
        Val::Nil,
        SrcLoc::default(),
    );
    let dbg = format!("{:?}", ds);
    assert_eq!("struct(Taco)", dbg);
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
