use leema::program::{Lib};
use leema::val::{SxprType, Val, Type};
use leema::module::{ModKey, ModulePreface};
use leema::list;
use leema::log;
use leema::sxpr;

use std::collections::{HashMap};
use std::rc::Rc;
use std::io::{stderr, Write};


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

    pub fn contains_val(&self, valnm: &str) -> bool
    {
        self.valtypes.contains_key(valnm)
    }

    pub fn valtype(&self, valnm: &str) -> Option<&Type>
    {
        self.valtypes.get(valnm)
    }

    pub fn preproc_module_expr(&mut self, prog: &Lib
            , mp: &ModulePreface, x: &Val
    ) {
        match x {
            &Val::Sxpr(SxprType::DefFunc, ref parts) => {
                let (fname, args, fresult, body) = list::to_ref_tuple4(parts);
                let strname = String::from(fname.str());
                let pp_args = Protomod::preproc_list(prog, mp, args);
                let pp_fresult = Protomod::preproc_expr(prog, mp, fresult);
                let pp_body = Protomod::preproc_expr(prog, mp, body);
                let pp_func = sxpr::defunc(
                        fname.clone(), pp_args, pp_fresult, pp_body, Val::Void);

                let ftype = sxpr::defunc_type(&pp_func);

                self.funcsrc.insert(strname.clone(), pp_func);
                self.valtypes.insert(strname, ftype);
            }
            &Val::Sxpr(SxprType::DefMacro, _) => {
                // do nothing. the macro definition will have been handled
                // in the file read
            }
            &Val::Sxpr(SxprType::Import, _) => {
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
            &Val::Sxpr(SxprType::Call, ref call_info) => {
                let (ref callx, ref args) = list::take_ref(call_info);
                Protomod::preproc_call(prog, mp, callx, args)
            }
            &Val::Id(_) => {
                x.clone()
            }
            &Val::Sxpr(sxpr_type, ref sx) => {
                Protomod::preproc_sxpr(prog, mp, sxpr_type, sx)
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
            &Val::Cons(_, _) => {
                list::map_ref(x, |i| { Protomod::preproc_expr(prog, mp, i) })
            }
            &Val::Nil => Val::Nil,
            &Val::Hashtag(ref ht) => Val::Hashtag(ht.clone()),
            &Val::Type(_) => {
                x.clone()
            }
            &Val::TypedId(_, _) => {
                x.clone()
            }
            &Val::Tuple(ref items) => {
                Val::Tuple(items.iter().map(|i| {
                    Protomod::preproc_expr(prog, mp, i)
                }).collect())
            }
            &Val::Wildcard => Val::Wildcard,
            &Val::RustBlock => Val::RustBlock,
            &Val::Void => Val::Void,
            &Val::CallParams => Val::CallParams,
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
                        match prog.get_macro("prefab", &**id) {
                            Some(&(ref arg_names, ref body)) => {
                                let macrod = Protomod::apply_macro(&**id, body, arg_names, args);
                                // do it again to make sure there's not a wrapped
                                // macro
                                Protomod::preproc_expr(prog, mp, &macrod)
                            }
                            None => {
                                sxpr::call(callx.clone(), pp_args)
                            }
                        }
                    }
                }
            }
            &Val::DotAccess(ref outer, ref inner) => {
                match &**outer {
                    &Val::Id(ref outer_id) => {
                        if !mp.imports.contains(&**outer_id) {
                            return sxpr::call(callx.clone(), pp_args);
                        }
                        let mac = prog.get_macro(outer_id, inner);
                        if mac.is_none() {
                            return sxpr::call(callx.clone(), pp_args);
                        }
                        let &(ref arg_names, ref body) = mac.unwrap();
                        let result = Protomod::apply_macro(
                                &**inner, body, arg_names, args);
                        Protomod::preproc_expr(prog, mp, &result)
                    }
                    _ => {
                        sxpr::call(callx.clone(), pp_args)
                    }
                }
            }
            _ => {
                println!("function call is what? {:?}", callx);
                let pp_callx = Protomod::preproc_expr(prog, mp, callx);
                sxpr::call(pp_callx, pp_args)
            }
        }
    }

    pub fn apply_macro(macro_name: &str, body: &Val
            , arg_names: &Vec<Rc<String>>, args: &Val) -> Val
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

    pub fn replace_ids(node: &Val, idvals: &HashMap<Rc<String>, &Val>) -> Val
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
            &Val::Sxpr(stype, ref sdata) => {
                sxpr::new(
                    stype,
                    Protomod::replace_ids(sdata, idvals),
                )
            }
            _ => node.clone(),
        }
    }

    pub fn preproc_sxpr(prog: &Lib, mp: &ModulePreface
            , st: SxprType, sx: &Val) -> Val
    {
        match st {
            SxprType::Let => {
                let (patt, src) = list::to_ref_tuple2(sx);
                let ppatt = Protomod::preproc_pattern(prog, mp, patt);
                let psrc = Protomod::preproc_expr(prog, mp, src);
                sxpr::new(st, list::from2(ppatt, psrc))
            }
            SxprType::MatchExpr => {
                let (mx, cases) = list::to_ref_tuple2(sx);
                let pmx = Protomod::preproc_expr(prog, mp, mx);
                let pcases = Protomod::preproc_matchcase(prog, mp, cases);
                sxpr::new(st, list::from2(pmx, pcases))
            }
            _ => {
                let pp_sx = Protomod::preproc_list(prog, mp, sx);
                sxpr::new(st, pp_sx)
            }
        }
    }

    pub fn preproc_matchcase(prog: &Lib, mp: &ModulePreface, case: &Val) -> Val
    {
        let (patt, t2) = list::take_ref(case);
        let (blk, t3) = list::take_ref(t2);
        let p_patt = Protomod::preproc_pattern(prog, mp, patt);
        let p_blk = Protomod::preproc_expr(prog, mp, blk);

        let p_next = match t3 {
            &Val::Cons(ref next, _) => {
                Protomod::preproc_matchcase(prog, mp, next)
            }
            &Val::Nil => Val::Void,
            _ => {
                panic!("next is not a list: {:?}", *t3);
            }
        };
        list::from3(p_patt, p_blk, p_next)
    }

    pub fn preproc_list(prog: &Lib, mp: &ModulePreface, l: &Val) -> Val
    {
        list::map_ref(l, |a| {
            Protomod::preproc_expr(prog, mp, a)
        })
    }

    pub fn preproc_pattern(prog: &Lib, mp: &ModulePreface, p: &Val) -> Val
    {
        match p {
            &Val::Cons(_, _) => Protomod::preproc_pattern_list(prog, mp, p),
            _ => p.clone(),
        }
    }

    pub fn preproc_pattern_list(prog: &Lib, mp: &ModulePreface, p: &Val) -> Val
    {
        match p {
            &Val::Cons(ref head, ref tail) => {
                let phead = Protomod::preproc_pattern(prog, mp, head);
                let ptail = Protomod::preproc_pattern_list(prog, mp, tail);
                Val::Cons(Box::new(phead), Box::new(ptail))
            }
            &Val::Id(ref id) => Val::Id(id.clone()),
            &Val::Nil => Val::Nil,
            &Val::Wildcard => Val::Wildcard,
            _ => {
                panic!("Not a pattern list: {:?}", p);
            }
        }
    }
}

pub fn preproc(prog: &mut Lib, mp: &ModulePreface, ast: &Val) -> Protomod
{
    let mk = mp.key.clone();
    let mut p0 = Protomod::new(mk);
    match ast {
        &Val::Sxpr(SxprType::BlockExpr, ref exprs) => {
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


#[cfg(test)]
mod tests {
    use leema::log;
    use leema::loader::{Interloader};
    use leema::program;

    use std::rc::{Rc};
    use std::io::{stderr, Write};


#[test]
fn test_preproc_list_pattern()
{
    let input = String::from("

    func foo(a)
    |(h;t) -> cout(\"head: $h, tail: $t\n\")
    --

    func main() -> foo([3, 4, 5]) --
    ");

    let mut loader = Interloader::new("tacos.lma");
    loader.set_mod_txt("tacos", input);
    let mut prog = program::Lib::new(loader);
    let pmod = prog.read_proto("tacos");
}

}
