use leema::program::{Lib};
use leema::val::{SxprType, Val, Type, FuncCallType};
use leema::module::{ModKey, ModulePreface};
use leema::list;
use leema::log;
use leema::sxpr;

use std::collections::{HashMap, HashSet, LinkedList};
use std::rc::Rc;
use std::io::{stderr, Write};


#[derive(Debug)]
pub struct Protomod
{
    pub key: Rc<ModKey>,
    pub funcseq: LinkedList<Rc<String>>,
    pub funcsrc: HashMap<String, Val>,
    pub valtypes: HashMap<String, Type>,
    pub newtypes: HashSet<Type>,
    pub structfields: HashMap<String, Vec<(Rc<String>, Type)>>,
}

impl Protomod
{
    pub fn new(mk: Rc<ModKey>) -> Protomod
    {
        Protomod{
            key: mk,
            funcseq: LinkedList::new(),
            funcsrc: HashMap::new(),
            valtypes: HashMap::new(),
            newtypes: HashSet::new(),
            structfields: HashMap::new(),
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
                let rcname = fname.id_name().clone();
                let strname = String::from(fname.str());
                let pp_args = Protomod::preproc_list(prog, mp, args);
                let pp_fresult = Protomod::preproc_expr(prog, mp, fresult);
                let pp_body = Protomod::preproc_expr(prog, mp, body);
                let pp_func = sxpr::defunc(
                        fname.clone(), pp_args, pp_fresult, pp_body, Val::Void);

                let ftype = sxpr::defunc_type(&pp_func);

                self.funcseq.push_back(rcname);
                self.funcsrc.insert(strname.clone(), pp_func);
                self.valtypes.insert(strname, ftype);
            }
            &Val::Sxpr(SxprType::DefStruct, ref parts) => {
                self.preproc_struct(parts);
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
            &Val::DotAccess(ref base, ref fld) => {
                let ppbase = Protomod::preproc_expr(prog, mp, base);
                Val::DotAccess(Box::new(ppbase), fld.clone())
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
            &Val::ModPrefix(ref prefix, ref inner) => {
                if !mp.imports.contains(&**prefix) {
                    panic!("module not found: {}", prefix);
                }
                let inner_id = inner.to_str();
                let mac = prog.get_macro(prefix, &*inner_id);
                if mac.is_none() {
                    return sxpr::call(callx.clone(), pp_args);
                }
                let &(ref arg_names, ref body) = mac.unwrap();
                let result = Protomod::apply_macro(
                        &**inner_id, body, arg_names, args);
                Protomod::preproc_expr(prog, mp, &result)
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

    pub fn preproc_struct(&mut self, sp: &Val)
    {
        let (ref name, ref src_fields) = list::take_ref(sp);
        let rc_name = name.id_name().clone();

        let field_type_vec = list::map_ref_to_vec(src_fields, |f| {
            let (fname, ftype) = Val::split_typed_id(f);
            ftype.clone()
        });

        let field_id_vec = list::map_ref_to_vec(src_fields, |f| {
            let (fname, _) = Val::split_typed_id(f);
            fname.clone()
        });

        let field_name_vec = list::map_ref_to_vec(src_fields, |f| {
            let (fname, ftype) = Val::split_typed_id(f);
            fname.id_name()
        });

        let struct_fields =
            list::map_ref_to_vec(src_fields, |f| {
                let (fname, ftype) = Val::split_typed_id(f);
                (fname.id_name().clone(), ftype.clone())
            });

        let num_fields = field_type_vec.len() as i8;
        let stype = Type::Struct(rc_name.clone(), num_fields);

        let func_type = Type::Func(FuncCallType::FrameCall,
            field_type_vec, Box::new(stype.clone()));

        let srcblk = Val::Struct(stype.clone(), field_id_vec);
        let srcxpr = sxpr::defunc((*name).clone()
            , (*src_fields).clone()
            , Val::Type(stype.clone())
            , srcblk, Val::Void
            );

        self.funcseq.push_back(rc_name.clone());
        self.funcsrc.insert((*rc_name).clone(), srcxpr);
        self.valtypes.insert((*rc_name).clone(), func_type);
        self.structfields.insert((*rc_name).clone(), struct_fields);
        self.newtypes.insert(stype);
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
    use leema::module::{ModKey};
    use leema::phase0::{Protomod};
    use leema::program;
    use leema::val::{Val, Type};

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

#[test]
fn test_new_struct_newtypes()
{
    let mk = Rc::new(ModKey::name_only("tacos"));
    let mut proto = Protomod::new(mk);
    let struct_name = Rc::new("Burrito".to_string());
    let raw_fields = vec![
        (Rc::new("lettuce".to_string()), Type::Bool),
        (Rc::new("buns".to_string()), Type::Int),
    ];

    proto.new_struct(struct_name.clone(), raw_fields);

    assert!(proto.newtypes.contains(
        &Type::Struct(Rc::new("Burrito".to_string()), 2)
    ));
}

#[test]
fn test_new_struct_fields()
{
    let mk = Rc::new(ModKey::name_only("tacos"));
    let mut proto = Protomod::new(mk);
    let struct_name = Rc::new("Burrito".to_string());
    let raw_fields = vec![
        (Rc::new("lettuce".to_string()), Type::Bool),
        (Rc::new("buns".to_string()), Type::Int),
    ];
    proto.new_struct(struct_name.clone(), raw_fields);

    assert!(proto.structfields.contains_key(&*struct_name));

    let structfields = proto.structfields.get(&*struct_name).unwrap();
    assert_eq!(2, structfields.len());

    let lettuce = structfields.get(0).unwrap();
    assert_eq!("lettuce", &*lettuce.0);
    assert_eq!(Type::Bool, lettuce.1);

    let buns = structfields.get(1).unwrap();
    assert_eq!("buns", &*buns.0);
    assert_eq!(Type::Int, buns.1);
}

#[test]
fn test_new_struct_constructor_valtype()
{
    let mk = Rc::new(ModKey::name_only("tacos"));
    let mut proto = Protomod::new(mk);
    let struct_name = Rc::new("Burrito".to_string());
    let raw_fields = vec![
        (Rc::new("lettuce".to_string()), Type::Bool),
        (Rc::new("buns".to_string()), Type::Int),
    ];
    proto.new_struct(struct_name.clone(), raw_fields);

    assert!(proto.valtypes.contains_key(&*struct_name));

    let constructor = proto.valtypes.get(&*struct_name).unwrap();

    if let &Type::Func(ref calltyp, ref params, ref result) = constructor {
        assert_eq!(2, params.len());
    } else {
        panic!("constructor valtype is not a func");
    }
}

}
