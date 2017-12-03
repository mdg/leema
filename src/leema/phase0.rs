use leema::program::{Lib};
use leema::val::{SxprType, Val, Type, SrcLoc};
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
            &Val::Sxpr(SxprType::DefFunc, ref parts, ref loc) => {
                let (fname, args, fresult, body) = list::to_ref_tuple4(parts);
                let rcname = fname.id_name().clone();
                let strname = String::from(fname.str());
                let pp_args = Protomod::preproc_list(prog, mp, args, loc);
                let pp_fresult = Protomod::preproc_expr(prog, mp, fresult, loc);
                let pp_body = Protomod::preproc_expr(prog, mp, body, loc);
                let pp_func = sxpr::defunc(
                        fname.clone(), pp_args, pp_fresult, pp_body, *loc);

                let ftype = sxpr::defunc_type(&pp_func);

                self.funcseq.push_back(rcname);
                self.funcsrc.insert(strname.clone(), pp_func);
                self.valtypes.insert(strname, ftype);
            }
            &Val::Sxpr(SxprType::DefStruct, ref parts, ref loc) => {
                self.preproc_struct(parts, loc);
            }
            &Val::Sxpr(SxprType::DefMacro, _, ref loc) => {
                // do nothing. the macro definition will have been handled
                // in the file read
            }
            &Val::Sxpr(SxprType::Import, _, _) => {
                // do nothing. imports handled in file read
            }
            _ => {
                println!("Cannot phase0: {:?}", x);
            }
        }
    }

    pub fn preproc_expr(prog: &Lib, mp: &ModulePreface, x: &Val, loc: &SrcLoc
        ) -> Val
    {
        match x {
            &Val::Sxpr(SxprType::Call, ref call_info, ref loc) => {
                let (ref callx, ref args) = list::take_ref(call_info);
                Protomod::preproc_call(prog, mp, callx, args, loc)
            }
            &Val::Id(_) => {
                x.clone()
            }
            &Val::Sxpr(sxpr_type, ref sx, ref sxloc) => {
                Protomod::preproc_sxpr(prog, mp, sxpr_type, sx, sxloc)
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
            &Val::Cons(ref head, ref tail) => {
                Protomod::preproc_cons(prog, mp, head, tail, loc)
            }
            &Val::Nil => Val::Nil,
            &Val::Hashtag(ref ht) => Val::Hashtag(ht.clone()),
            &Val::Type(ref typ) => {
                Val::Type(Protomod::preproc_type(prog, mp, typ))
            }
            &Val::TypedId(ref id, ref typ) => {
                let pptyp = Protomod::preproc_type(prog, mp, typ);
                Val::TypedId(id.clone(), pptyp)
            }
            &Val::Tuple(ref items) if items.len() == 1 => {
                // one tuples are compiled to just the value
                Protomod::preproc_expr(prog, mp, items.get(0).unwrap(), loc)
            }
            &Val::Tuple(ref items) => {
                Val::Tuple(items.iter().map(|i| {
                    Protomod::preproc_expr(prog, mp, i, loc)
                }).collect())
            }
            &Val::DotAccess(ref base, ref fld) => {
                let ppbase = Protomod::preproc_expr(prog, mp, base, loc);
                Val::DotAccess(Box::new(ppbase), fld.clone())
            }
            &Val::ModPrefix(ref prefix, ref name) => {
                if !mp.imports.contains(&**prefix) {
                    panic!("cannot find {} module. maybe import it?");
                }
                let ppname = Protomod::preproc_expr(prog, mp, name, loc);
                Val::ModPrefix(prefix.clone(), Rc::new(ppname))
            }
            &Val::Wildcard => Val::Wildcard,
            &Val::RustBlock => Val::RustBlock,
            &Val::Void => Val::Void,
            &Val::Loc(ref v, ref l) => {
                Protomod::preproc_expr(prog, mp, v, l)
            }
            _ => {
                println!("preproc_unknown_expr({:?})", x);
                x.clone()
            }
        }
    }

    pub fn preproc_cons(prog: &Lib, mp: &ModulePreface
            , head: &Val, tail: &Val, loc: &SrcLoc) -> Val
    {
        let pphead = Protomod::preproc_expr(prog, mp, head, loc);
        match tail {
            &Val::Cons(ref next_head, ref next_tail) => {
                let pptail =
                    Protomod::preproc_cons(prog, mp, next_head, next_tail, loc);
                sxpr::call(
                    Val::id("list_cons".to_string()),
                    list::from2(pphead, pptail),
                    *loc,
                )
            }
            &Val::Nil => {
                list::singleton(pphead)
            }
            &Val::Id(_) => {
                let pptail = Protomod::preproc_expr(prog, mp, tail, loc);
                sxpr::call(
                    Val::id("list_cons".to_string()),
                    list::from2(pphead, pptail),
                    *loc,
                )
            }
            &Val::Loc(ref v, ref lloc) => {
                let v2 = Protomod::preproc_cons(prog, mp, head, v, lloc);
                Val::loc(v2, *loc)
            }
            _ => {
                panic!("cannot preproc cons: {:?};{:?}", head, tail)
            }
        }
    }

    pub fn preproc_call(prog: &Lib, mp: &ModulePreface
            , callx: &Val, args: &Rc<Val>, loc: &SrcLoc) -> Val
    {
        let pp_args = Protomod::preproc_list(prog, mp, &**args, loc);
        match callx {
            &Val::Id(ref id) => {
                match mp.macros.get(&**id) {
                    Some(&(ref arg_names, ref body)) => {
                        let macrod =
                            Protomod::apply_macro(&**id, body, arg_names, args);
                        // do it again to make sure there's not a wrapped
                        // macro
                        Protomod::preproc_expr(prog, mp, &macrod, loc)
                    }
                    None => {
                        match prog.get_macro("prefab", &**id) {
                            Some(&(ref arg_names, ref body)) => {
                                let macrod = Protomod::apply_macro(&**id
                                    , body, arg_names, args);
                                // do it again to make sure there's
                                // not a wrapped macro
                                Protomod::preproc_expr(prog, mp, &macrod, loc)
                            }
                            None => {
                                sxpr::call(callx.clone(), pp_args, *loc)
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
                    return sxpr::call(callx.clone(), pp_args, *loc);
                }
                let &(ref arg_names, ref body) = mac.unwrap();
                let result = Protomod::apply_macro(
                        &**inner_id, body, arg_names, args);
                Protomod::preproc_expr(prog, mp, &result, loc)
            }
            &Val::DotAccess(ref outer, ref inner) => {
                match &**outer {
                    &Val::Id(ref outer_id) => {
                        if !mp.imports.contains(&**outer_id) {
                            return sxpr::call(callx.clone(), pp_args, *loc);
                        }
                        let mac = prog.get_macro(outer_id, inner);
                        if mac.is_none() {
                            return sxpr::call(callx.clone(), pp_args, *loc);
                        }
                        let &(ref arg_names, ref body) = mac.unwrap();
                        let result = Protomod::apply_macro(
                                &**inner, body, arg_names, args);
                        Protomod::preproc_expr(prog, mp, &result, loc)
                    }
                    _ => {
                        sxpr::call(callx.clone(), pp_args, *loc)
                    }
                }
            }
            &Val::Loc(ref v, ref l) => {
                Protomod::preproc_call(prog, mp, v, args, l)
            }
            _ => {
                panic!("function call is what? {:?}@{:?}", callx, loc);
            }
        }
    }

    pub fn apply_macro(macro_name: &str, body: &Val
            , arg_names: &Vec<Rc<String>>, args: &Rc<Val>) -> Val
    {
        let mut arg_map = HashMap::new();
        let mut arg_it = args;
        for n in arg_names {
            if **arg_it == Val::Nil {
                panic!("Too few arguments passed to macro {}, expected {}"
                        , macro_name, arg_names.len());
            }
            let (arg_val, arg_tail) = list::take_ref(&**arg_it);
            arg_map.insert(n.clone(), arg_val);
            arg_it = arg_tail;
        }
        if **arg_it != Val::Nil {
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
                    None => node.clone(),
                }
            }
            &Val::Loc(ref v, ref loc) => {
                let v2 = Protomod::replace_ids(v, idvals);
                Val::loc(v2, *loc)
            }
            &Val::Sxpr(stype, ref sdata, ref loc) => {
                sxpr::new(
                    stype,
                    Protomod::replace_ids(sdata, idvals),
                    *loc,
                )
            }
            _ => node.clone(),
        }
    }

    pub fn preproc_sxpr(prog: &Lib, mp: &ModulePreface
            , st: SxprType, sx: &Val, loc: &SrcLoc) -> Val
    {
        match st {
            SxprType::Let => {
                let (patt, src) = list::to_ref_tuple2(sx);
                let ppatt = Protomod::preproc_pattern(prog, mp, patt);
                let psrc = Protomod::preproc_expr(prog, mp, src, loc);
                sxpr::new(
                    st,
                    list::from2(ppatt, psrc),
                    *loc
                )
            }
            SxprType::MatchExpr => {
                let (mx, cases) = list::to_ref_tuple2(sx);
                let pmx = Protomod::preproc_expr(prog, mp, mx, loc);
                let pcases = Protomod::preproc_matchcase(prog, mp, cases, loc);
                sxpr::new(
                    st,
                    list::from2(pmx, pcases),
                    *loc,
                )
            }
            SxprType::MatchFailed => {
                let (fx, cases) = list::to_ref_tuple2(sx);
                let pfx = Protomod::preproc_expr(prog, mp, fx, loc);
                let pcases = Protomod::preproc_matchcase(prog, mp, cases, loc);
                sxpr::new(
                    SxprType::MatchFailed,
                    list::from2(pfx, pcases),
                    *loc,
                )
            }
            _ => {
                let pp_sx = Protomod::preproc_list(prog, mp, sx, loc);
                sxpr::new(st, pp_sx, *loc)
            }
        }
    }

    pub fn preproc_matchcase(prog: &Lib, mp: &ModulePreface, case: &Val
        , loc: &SrcLoc
        ) -> Val
    {
        let (patt, t2) = list::take_ref(case);
        let (blk, t3) = list::take_ref(&*t2);
        let p_patt = Protomod::preproc_pattern(prog, mp, patt);
        let p_blk = Protomod::preproc_expr(prog, mp, blk, loc);

        let p_next = match &**t3 {
            &Val::Cons(ref next, _) => {
                Protomod::preproc_matchcase(prog, mp, next, loc)
            }
            &Val::Nil => Val::Void,
            _ => {
                panic!("next is not a list: {:?}", *t3);
            }
        };
        list::from3(p_patt, p_blk, p_next)
    }

    pub fn preproc_list(prog: &Lib, mp: &ModulePreface, l: &Val, loc: &SrcLoc
        ) -> Val
    {
        list::map_ref(l, |a| {
            Protomod::preproc_expr(prog, mp, a, loc)
        })
    }

    pub fn preproc_pattern(prog: &Lib, mp: &ModulePreface, p: &Val) -> Val
    {
        match p {
            &Val::Cons(_, _) => Protomod::preproc_pattern_list(prog, mp, p),
            &Val::Nil => Val::Nil,
            &Val::Tuple(ref items) if items.len() == 1 => {
                let first = items.get(0).unwrap();
                Protomod::preproc_pattern(prog, mp, first)
            }
            &Val::Tuple(ref items) => {
                Val::Tuple(
                    items.iter().map(|i| {
                        Protomod::preproc_pattern(prog, mp, i)
                    }).collect()
                )
            }
            &Val::Sxpr(SxprType::Call, ref sx, ref loc) => {
                let (callx, args) = list::take_ref(sx);
                let pp_callx = Protomod::preproc_pattern_call(prog, mp, callx);
                let pp_args = list::map_ref(&**args, |px| {
                    Protomod::preproc_pattern(prog, mp, px)
                });
                let pp_sx = list::cons(pp_callx, pp_args);
                Val::Sxpr(SxprType::Call, Rc::new(pp_sx), *loc)
            }
            &Val::Id(_) => {
                p.clone()
            }
            &Val::Wildcard => Val::Wildcard,
            &Val::Int(i) => Val::Int(i),
            &Val::Bool(b) => Val::Bool(b),
            &Val::Str(ref s) => Val::Str(s.clone()),
            &Val::Hashtag(ref h) => Val::Hashtag(h.clone()),
            &Val::ModPrefix(_, _) => p.clone(),
            &Val::Loc(ref v, _) => {
                Protomod::preproc_pattern(prog, mp, v)
            }
            _ => {
                println!("preproc_pattern what?: {:?}", p);
                p.clone()
            }
        }
    }

    pub fn preproc_pattern_call(prog: &Lib, mp: &ModulePreface, p: &Val) -> Val
    {
        match p {
            &Val::Id(_) => {
                Val::ModPrefix(Rc::new(mp.key.name.clone()), Rc::new(p.clone()))
            }
            &Val::ModPrefix(_, _) => {
                p.clone()
            }
            &Val::Loc(ref v, ref loc) => {
                let v2 = Protomod::preproc_pattern_call(prog, mp, v);
                Val::loc(v2, *loc)
            }
            _ => {
                Protomod::preproc_pattern(prog, mp, p)
            }
        }
    }

    pub fn preproc_pattern_list(prog: &Lib, mp: &ModulePreface, p: &Val) -> Val
    {
        match p {
            &Val::Cons(ref head, ref tail) => {
                let phead = Protomod::preproc_pattern(prog, mp, head);
                let ptail = Protomod::preproc_pattern_list(prog, mp, tail);
                Val::Cons(Box::new(phead), Rc::new(ptail))
            }
            &Val::Id(_) => p.clone(),
            &Val::Nil => Val::Nil,
            &Val::Wildcard => Val::Wildcard,
            &Val::Loc(ref v, ref loc) => {
                let v2 = Protomod::preproc_pattern_list(prog, mp, v);
                Val::loc(v2, *loc)
            }
            _ => {
                panic!("Not a pattern list: {:?}", p);
            }
        }
    }

    pub fn preproc_struct(&mut self, sp: &Val, loc: &SrcLoc)
    {
        let (ref name, ref src_fields) = list::take_ref(sp);
        let rc_name = name.id_name().clone();
        let base_type_id = Type::Id(rc_name.clone());
        let mod_type = Type::ModPrefix(
            Rc::new(self.key.name.clone()),
            Rc::new(base_type_id),
        );
        let mod_typename = mod_type.typename();

        let field_type_vec = list::map_ref_to_vec(&**src_fields, |f| {
            let (fname, ftype) = Val::split_typed_id(f);
            ftype.clone()
        });

        let field_id_vec = list::map_ref_to_vec(&**src_fields, |f| {
            let (fname, _) = Val::split_typed_id(f);
            fname.clone()
        });

        let field_name_vec = list::map_ref_to_vec(&**src_fields, |f| {
            let (fname, ftype) = Val::split_typed_id(f);
            fname.id_name()
        });

        let struct_fields =
            list::map_ref_to_vec(&**src_fields, |f| {
                let (fname, ftype) = Val::split_typed_id(f);
                (fname.id_name().clone(), ftype.clone())
            });

        let stype = Type::Struct(Rc::new(mod_type));

        let func_type = Type::Func(field_type_vec, Box::new(stype.clone()));

        let srcblk = Val::Struct(stype.clone(), field_id_vec);
        let srcxpr = sxpr::defunc((*name).clone()
            , (***src_fields).clone()
            , Val::Type(stype.clone())
            , srcblk
            , *loc
            );

        self.funcseq.push_back(rc_name.clone());
        self.funcsrc.insert((*rc_name).clone(), srcxpr);
        self.valtypes.insert((*rc_name).clone(), func_type);
        self.structfields.insert((*mod_typename).clone(), struct_fields);
        self.newtypes.insert(stype);
    }

    pub fn struct_field_idx(&self, typ: &Type, fld: &str) -> Option<(i8, &Type)>
    {
        let typename = typ.typename();
        let opt_structfields = self.structfields.get(&*typename);
        if opt_structfields.is_none() {
            panic!("cannot find struct fields for: {}", typename);
        }
        let structfields = opt_structfields.unwrap();
        structfields.iter().enumerate().find(|&(_, &(ref fname, _))| {
            &**fname == fld
        })
        .map(|(idx, &(_, ref ftype))| {
            (idx as i8, ftype)
        })
    }

    pub fn preproc_type(prog: &Lib, mp: &ModulePreface, t: &Type) -> Type
    {
        match t {
            &Type::Id(_) => {
                let prefix = Rc::new(mp.key.name.clone());
                Type::ModPrefix(prefix, Rc::new(t.clone()))
            }
            _ => {
                t.clone()
            }
        }
    }
}

pub fn preproc(prog: &mut Lib, mp: &ModulePreface, ast: &Val) -> Protomod
{
    let mk = mp.key.clone();
    let mut p0 = Protomod::new(mk);
    match ast {
        &Val::Sxpr(SxprType::BlockExpr, ref exprs, ref loc) => {
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
    use leema::list;
    use leema::log;
    use leema::loader::{Interloader};
    use leema::module::{ModKey};
    use leema::phase0::{Protomod};
    use leema::program;
    use leema::sxpr;
    use leema::val::{Val, Type, SxprType, SrcLoc};

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

    assert_eq!(2, pmod.funcsrc.len());

    let foo_func = pmod.funcsrc.get("foo").unwrap();
    assert!(sxpr::is_type(foo_func, SxprType::DefFunc));

    let main_func = pmod.funcsrc.get("main").unwrap();
    assert!(sxpr::is_type(main_func, SxprType::DefFunc));
}

#[test]
fn test_new_struct_newtypes()
{
    let mk = Rc::new(ModKey::name_only("tacos"));
    let mut proto = Protomod::new(mk);
    let struct_name = Rc::new("Burrito".to_string());
    let raw_fields = list::from3(
        Val::Id(struct_name.clone()),
        Val::typed_id("lettuce", Type::Bool),
        Val::typed_id("buns", Type::Int),
    );
    let loc = SrcLoc::new(8, 9);
    proto.preproc_struct(&raw_fields, &loc);

    assert!(proto.newtypes.contains(
        &Type::Struct(Rc::new(
            Type::ModPrefix(
                Rc::new("tacos".to_string()),
                Rc::new(Type::Id(Rc::new("Burrito".to_string())))
            )
        ))
    ));
}

#[test]
fn test_new_struct_fields()
{
    let mk = Rc::new(ModKey::name_only("tacos"));
    let mut proto = Protomod::new(mk);
    let struct_name = Rc::new("Burrito".to_string());
    let raw_fields = list::from3(
        Val::Id(struct_name.clone()),
        Val::typed_id("lettuce", Type::Bool),
        Val::typed_id("buns", Type::Int),
    );
    let loc = SrcLoc::new(8, 9);
    proto.preproc_struct(&raw_fields, &loc);

    assert!(proto.structfields.contains_key("tacos::Burrito"));

    let structfields = proto.structfields.get("tacos::Burrito").unwrap();
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
    let raw_fields = list::from3(
        Val::Id(struct_name.clone()),
        Val::typed_id("lettuce", Type::Bool),
        Val::typed_id("buns", Type::Int),
    );
    let loc = SrcLoc::new(8, 9);
    proto.preproc_struct(&raw_fields, &loc);

    assert!(proto.valtypes.contains_key(&*struct_name));

    let constructor = proto.valtypes.get(&*struct_name).unwrap();

    if let &Type::Func(ref params, ref result) = constructor {
        assert_eq!(2, params.len());
    } else {
        panic!("constructor valtype is not a func");
    }
}

}
