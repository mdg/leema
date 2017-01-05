use leema::program::{Lib};
use leema::val::{SexprType, Val, Type};
use leema::module::{ModKey, ModulePreface};
use leema::list;
use leema::sexpr;

use std::collections::{HashMap};
use std::rc::Rc;
use std::sync::{Arc};


#[derive(Debug)]
pub struct Protomod
{
    pub key: Rc<ModKey>,
    pub funcsrc: HashMap<String, Val>,
    pub valtypes: HashMap<String, Type>,
    pub newtypes: HashMap<Type, Val>,
}

impl Protomod
{
    pub fn new(mk: Rc<ModKey>) -> Protomod
    {
        Protomod{
            key: mk,
            funcsrc: HashMap::new(),
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
                let strname = String::from(fname.str());
                let pp_args = Protomod::preproc_list(prog, mp, args);
                let pp_fresult = Protomod::preproc_expr(prog, mp, fresult);
                let pp_body = Protomod::preproc_expr(prog, mp, body);
                let pp_func = sexpr::defunc(
                        fname.clone(), pp_args, pp_fresult, pp_body, Val::Void);
                self.funcsrc.insert(strname.clone(), pp_func);

                let ftype = sexpr::defunc_type(x);
                self.valtypes.insert(strname, ftype);
            }
            &Val::Sexpr(SexprType::DefMacro, _) => {
                // do nothing. the macro definition will have been handled
                // in the file read
            }
            &Val::Sexpr(SexprType::Import, _) => {
                // do nothing. imports handled in file read
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
                Protomod::preproc_call(prog, mp, callx, args)
            }
            &Val::Id(_) => {
                x.clone()
            }
            &Val::Sexpr(sexpr_type, ref sx) => {
                Protomod::preproc_sexpr(prog, mp, sexpr_type, sx)
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
        let pp_args = Protomod::preproc_list(prog, mp, args);
        match callx {
            &Val::Id(ref id) => {
                match mp.macros.get(&**id) {
                    Some(&(ref arg_names, ref body)) => {
                        let macrod = Protomod::apply_macro(&**id, body, arg_names, args);
                        // do it again to make sure there's not a wrapped
                        // macro
                        Protomod::preproc_expr(prog, mp, &macrod)
                    }
                    None => {
                        sexpr::call(callx.clone(), pp_args)
                    }
                }
            }
            &Val::Sexpr(SexprType::FieldAccess, ref flds) => {
                match list::to_ref_tuple2(flds) {
                    (&Val::Id(ref modname), &Val::Id(ref modcall)) => {
                        if mp.imports.contains(&**modname) {
                            match prog.get_macro(modname, modcall) {
                                Some(&(ref arg_names, ref body)) => {
                                    let result = Protomod::apply_macro(
                                        &**modcall, body, arg_names, args);
                                    Protomod::preproc_expr(prog, mp, &result)
                                }
                                None => {
                                    panic!("macro undefined: {}.{}",
                                        modname, modcall);
                                }
                            }
                        } else {
                            sexpr::call(callx.clone(), pp_args)
                        }
                    }
                    _ => {
                        let pp_callx = Protomod::preproc_expr(prog, mp, callx);
                        sexpr::call(pp_callx, pp_args)
                    }
                }
            }
            _ => {
                let pp_callx = Protomod::preproc_expr(prog, mp, callx);
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
            arg_map.insert(n.clone(), arg_val);
            arg_it = arg_tail;
        }
        if *arg_it != Val::Nil {
            panic!("Too many arguments passed to macro {}, expected {}"
                    , macro_name, arg_names.len());
        }
        Protomod::replace_ids(body, &arg_map)
    }

    pub fn replace_ids(node: &Val, idvals: &HashMap<Arc<String>, &Val>) -> Val
    {
        match node {
            &Val::Cons(_, _) => {
                let f = |v: &Val| -> Val {
                    Protomod::replace_ids(v, idvals)
                };
                list::map_ref(node, f)
            }
            &Val::Tuple(ref t) => {
                let mut result = vec![];
                for tv in t {
                    let rv = Protomod::replace_ids(tv, idvals);
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
                    Protomod::replace_ids(sdata, idvals),
                )
            }
            _ => node.clone(),
        }
    }

    pub fn preproc_sexpr(prog: &Lib, mp: &ModulePreface
            , st: SexprType, sx: &Val) -> Val
    {
        let pp_sx = Protomod::preproc_list(prog, mp, sx);
        sexpr::new(st, pp_sx)
    }

    pub fn preproc_list(prog: &Lib, mp: &ModulePreface, l: &Val) -> Val
    {
        list::map_ref(l, |a| {
            Protomod::preproc_expr(prog, mp, a)
        })
    }
}

pub fn preproc(prog: &mut Lib, mp: &ModulePreface, ast: &Val) -> Protomod
{
    let mk = Rc::new(ModKey::name_only("tacos"));
    let mut p0 = Protomod::new(mk);
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
