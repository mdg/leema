use leema::program::{Lib};
use leema::val::{SexprType, Val, Type};
use leema::module::{ModulePreface};
use leema::list;
use leema::sexpr;

use std::collections::{HashMap};


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
    ) -> Val
    {
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
            _ => {
                println!("Cannot phase0: {:?}", x);
            }
        }
        x.clone()
    }

    pub fn preproc_expr(prog: &Lib, mp: &ModulePreface, x: &Val) -> Val
    {
        match x {
            &Val::Sexpr(SexprType::Call, ref call_info) => {
                println!("preproc_call({:?})", call_info);
                x.clone()
            }
            &Val::Sexpr(sexpr_type, ref sx) => {
                Phase0::preproc_sexpr(prog, mp, sexpr_type, sx)
            }
            &Val::Int(i) => {
                Val::Int(i)
            }
            &Val::Type(_) => {
                x.clone()
            }
            &Val::TypedId(_, _) => {
                x.clone()
            }
            &Val::Id(_) => {
                x.clone()
            }
            _ => {
                println!("preproc_unknown_expr({:?})", x);
                x.clone()
            }
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
