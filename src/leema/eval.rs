use leema::val::{Val,List,Env};
use leema::sexpr::{Sexpr};
use std::sync::Arc;


pub fn eval(env: &mut Env, input: &Sexpr) -> Val {
    match input {
        &Sexpr::Val(ref v) => { *v.clone() }
        &Sexpr::Id(ref id) => { Val::Str(Arc::new(id.clone())) }
        &Sexpr::Block(ref b) => {
            eval_block(env, b)
        }
        &Sexpr::Call(ref f, ref args) => {
            let fval = eval(env, f);
            eval_func(env, &fval, args)
        }
        _ => {
            panic!("dunno what to do with {:?}", input);
        }
    }
}

pub fn eval_val(env: &mut Env, input: &Val) -> Val {
    match input {
        &Val::Sexpr(ref x) => { eval(env, x) }
        _ => { input.clone() }
    }
}

pub fn eval_func(env: &mut Env, f: &Val, args: &List) -> Val {
    Val::Int(0)
}

pub fn eval_block(env: &mut Env, block: &List) -> Val {
    match block {
        &List::Nil => {
            // get last result
            Val::Int(0)
        }
        &List::Cons(ref head, ref tail) => {
            eval_val(env, head);
            eval_block(env, tail)
        }
    }
}
