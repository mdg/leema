use leema::program::{Lib};
use leema::val::{SexprType, Val, Type};
use leema::module::{ModulePreface};
use leema::list;
use leema::sexpr;

use std::collections::{HashMap};
use std::sync::{Arc};


#[derive(Debug)]
pub struct Phase0
{
    funcs: HashMap<String, Val>,
    valtypes: HashMap<String, Type>,
    newtypes: HashMap<Type, Val>,
}

impl Phase0
{
    pub fn new() -> Phase0
    {
        Phase0{
            funcs: HashMap::new(),
            valtypes: HashMap::new(),
            newtypes: HashMap::new(),
        }
    }

    pub fn preproc_module_expr(&mut self, prog: &Lib
            , mp: &ModulePreface, x: &Val
    ) {
        match x {
            &Val::Sexpr(SexprType::DefFunc, ref parts) => {
                let (fname, args, fresult, body) = list::to_ref_tuple4(parts);
                let pp_args = Phase0::preproc_list(prog, mp, args);
                let pp_fresult = Phase0::preproc_expr(prog, mp, fresult);
                let pp_body = Phase0::preproc_expr(prog, mp, body);
                let pp_func = sexpr::defunc(
                        fname.clone(), pp_args, pp_fresult, pp_body, Val::Void);
                self.funcs.insert(fname.str().to_string(), pp_func);
            }
            &Val::Sexpr(SexprType::DefMacro, _) => {
                // do nothing. the macro definition will have been handled
                // in the file read
            }
            _ => {
                println!("Cannot phase0: {:?}", x);
            }
        }
    }

    pub fn preproc_expr(prog: &Lib, mp: &ModulePreface, x: &Val) -> Val
    {
        match x {
            &Val::Sexpr(SexprType::Call, ref call_info) => {
                let (ref callx, ref args) = list::take_ref(call_info);
                Phase0::preproc_call(prog, mp, callx, args)
            }
            &Val::Id(_) => {
                x.clone()
            }
            &Val::Sexpr(sexpr_type, ref sx) => {
                Phase0::preproc_sexpr(prog, mp, sexpr_type, sx)
            }
            &Val::Int(i) => {
                Val::Int(i)
            }
            &Val::Str(_) => {
                x.clone()
            }
            &Val::Bool(b) => {
                Val::Bool(b)
            }
            &Val::Type(_) => {
                x.clone()
            }
            &Val::TypedId(_, _) => {
                x.clone()
            }
            _ => {
                println!("preproc_unknown_expr({:?})", x);
                x.clone()
            }
        }
    }

    pub fn preproc_call(prog: &Lib, mp: &ModulePreface
            , callx: &Val, args: &Val) -> Val
    {
        let pp_args = Phase0::preproc_list(prog, mp, args);
        match callx {
            &Val::Id(ref id) => {
                match mp.macros.get(&**id) {
                    Some(&(ref arg_names, ref body)) => {
                        Phase0::apply_macro(&**id, body, arg_names, args)
                    }
                    None => {
                        sexpr::call(callx.clone(), pp_args)
                    }
                }
            }
            _ => {
                let pp_callx = Phase0::preproc_expr(prog, mp, callx);
                sexpr::call(pp_callx, pp_args)
            }
        }
    }

    pub fn apply_macro(macro_name: &str, body: &Val
            , arg_names: &Vec<Arc<String>>, args: &Val) -> Val
    {
        let mut arg_map = HashMap::new();
        let mut arg_it = args;
        for n in arg_names {
            if *arg_it == Val::Nil {
                panic!("Too few arguments passed to macro {}, expected {}"
                        , macro_name, arg_names.len());
            }
            let (arg_val, arg_tail) = list::take_ref(arg_it);
            arg_map.insert(n, arg_val);
            arg_it = arg_tail;
        }
        if *arg_it != Val::Nil {
            panic!("Too many arguments passed to macro {}, expected {}"
                    , macro_name, arg_names.len());
        }
        body.clone()
    }

    pub fn replace_ids(node: &Val, idvals: &HashMap<Arc<String>, &Val>) -> Val
    {
        match node {
            &Val::Cons(_, _) => {
                let f = |v: &Val| -> Val {
                    Phase0::replace_ids(v, idvals)
                };
                list::map_ref(node, f)
            }
            &Val::Tuple(ref t) => {
                let mut result = vec![];
                for tv in t {
                    let rv = Phase0::replace_ids(tv, idvals);
                    result.push(rv);
                }
                Val::Tuple(result)
            }
            &Val::Id(ref name) => {
                match idvals.get(&*name) {
                    Some(newx) => (*newx).clone(),
                    None => Val::Id(name.clone()),
                }
            }
            &Val::Sexpr(stype, ref sdata) => {
                sexpr::new(
                    stype,
                    Phase0::replace_ids(sdata, idvals),
                )
            }
            _ => node.clone(),
        }
    }

    pub fn preproc_sexpr(prog: &Lib, mp: &ModulePreface
            , st: SexprType, sx: &Val) -> Val
    {
        let pp_sx = Phase0::preproc_list(prog, mp, sx);
        sexpr::new(st, pp_sx)
    }

    pub fn preproc_list(prog: &Lib, mp: &ModulePreface, l: &Val) -> Val
    {
        list::map_ref(l, |a| {
            Phase0::preproc_expr(prog, mp, a)
        })
    }
}

pub fn preproc(prog: &mut Lib, mp: &ModulePreface, ast: &Val) -> Phase0
{
    let mut p0 = Phase0::new();
    match ast {
        &Val::Sexpr(SexprType::BlockExpr, ref exprs) => {
            println!("preproc(block, {:?})", exprs);
            list::fold_mut_ref(&mut p0, exprs, |p, x| {
                p.preproc_module_expr(prog, mp, x);
            });
        }
        _ => {
            println!("preproc(something_else, {:?})", ast);
            p0.preproc_module_expr(prog, mp, ast);
        }
    }
    p0
}
