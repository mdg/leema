use leema::program::{Lib};
use leema::val::{SxprType, Val, Type, SrcLoc};
use leema::module::{ModKey, ModulePreface};
use leema::list;
use leema::log;
use leema::sxpr;

use std::collections::{HashMap, HashSet, LinkedList};
use std::rc::Rc;
use std::io::{Write};


#[derive(Debug)]
pub struct Protomod
{
    pub key: Rc<ModKey>,
    pub funcseq: LinkedList<Rc<String>>,
    pub funcsrc: HashMap<String, Val>,
    pub valtypes: HashMap<String, Type>,
    pub newtypes: HashSet<Type>,
    pub constants: HashMap<String, Val>,
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
            constants: HashMap::new(),
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
            &Val::Sxpr(SxprType::DefEnum, ref parts, ref loc) => {
                self.preproc_enum(parts, loc);
            }
            &Val::Sxpr(SxprType::DefNamedTuple, ref parts, ref loc) => {
                self.preproc_namedtuple(parts, loc);
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
                    panic!("cannot find {} module. maybe import it?", prefix);
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
                            Protomod::apply_macro(
                                &**id, body, arg_names, args, loc);
                        // do it again to make sure there's not a wrapped
                        // macro
                        Protomod::preproc_expr(prog, mp, &macrod, loc)
                    }
                    None => {
                        match prog.get_macro("prefab", &**id) {
                            Some(&(ref arg_names, ref body)) => {
                                let macrod = Protomod::apply_macro(&**id
                                    , body, arg_names, args, loc);
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
                        &**inner_id, body, arg_names, args, loc);
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
                                &**inner, body, arg_names, args, loc);
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
        , arg_names: &Vec<Rc<String>>, args: &Rc<Val>, loc: &SrcLoc
        ) -> Val
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
        Protomod::replace_ids(body, &arg_map, loc)
    }

    pub fn replace_ids(node: &Val, idvals: &HashMap<Rc<String>, &Val>
        , loc: &SrcLoc
        ) -> Val
    {
        match node {
            &Val::Cons(_, _) => {
                let f = |v: &Val| -> Val {
                    Protomod::replace_ids(v, idvals, loc)
                };
                list::map_ref(node, f)
            }
            &Val::Tuple(ref t) => {
                let mut result = vec![];
                for tv in t {
                    let rv = Protomod::replace_ids(tv, idvals, loc);
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
            &Val::Loc(ref v, ref _oldloc) => {
                Protomod::replace_ids(v, idvals, loc)
            }
            &Val::Sxpr(stype, ref sdata, ref _oldloc) => {
                sxpr::new(
                    stype,
                    Protomod::replace_ids(sdata, idvals, loc),
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
                Val::ModPrefix(mp.key.name.clone(), Rc::new(p.clone()))
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
        if list::is_empty(src_fields) {
            self.preproc_token_struct(name, loc);
        } else {
            self.preproc_struct_with_fields(name, src_fields, loc);
        }
    }

    pub fn preproc_token_struct(&mut self, name: &Val, loc: &SrcLoc)
    {
        let rc_name = name.id_name().clone();
        let local_type = Type::Token(rc_name.clone());
        let mod_name = self.key.name.clone();
        let mod_type = Type::ModPrefix(
            mod_name.clone(),
            Rc::new(local_type.clone()),
        );

        // a token struct is stored as a constant with no constructor
        let constval = Val::Token(mod_type.clone());
        self.constants.insert((*rc_name).clone(), constval);
        self.valtypes.insert((*rc_name).clone(), mod_type.clone());
        self.newtypes.insert(mod_type);
    }

    pub fn preproc_struct_with_fields(&mut self, name: &Val, src_fields: &Val
        , loc: &SrcLoc
        )
    {
        let rc_name = name.id_name().clone();
        let local_type = Type::Struct(rc_name.clone());
        let mod_name = self.key.name.clone();
        let mod_type = Type::ModPrefix(
            mod_name.clone(),
            Rc::new(local_type.clone()),
        );

        self.preproc_struct_fields(&mod_type, &mod_name
            , name, src_fields, loc);
        self.newtypes.insert(local_type);
    }

    pub fn preproc_struct_fields(&mut self, typ: &Type, mod_name: &Rc<String>
        , name: &Val, src_fields: &Val, loc: &SrcLoc
        )
    {
        let rc_name = name.id_name();
        let field_type_vec = list::map_ref_to_vec(src_fields, |f| {
            let (fname, ftype) = Val::split_typed_id(f);
            ftype.clone()
        });

        let field_id_vec = list::map_ref_to_vec(src_fields, |f| {
            let (fname, _) = Val::split_typed_id(f);
            fname.clone()
        });

        let struct_fields =
            list::map_ref_to_vec(src_fields, |f| {
                let (fname, ftype) = Val::split_typed_id(f);
                (fname.id_name().clone(), ftype.clone())
            });

        let func_type = Type::Func(field_type_vec, Box::new(typ.clone()));

        let srcblk = Val::Struct(typ.clone(), field_id_vec);
        let srcxpr = sxpr::defunc((*name).clone()
            , (*src_fields).clone()
            , Val::Type(typ.clone())
            , srcblk
            , *loc
        );

        let funcref =
            Val::FuncRef(mod_name.clone(), rc_name.clone(), func_type.clone());
        self.constants.insert((*rc_name).clone(), funcref);
        self.funcseq.push_back(rc_name.clone());
        self.funcsrc.insert((*rc_name).clone(), srcxpr);
        self.valtypes.insert((*rc_name).clone(), func_type);
        self.structfields.insert((*rc_name).clone(), struct_fields);
    }

    pub fn struct_field_idx(&self, typename: &str, fld: &str
        ) -> Option<(i16, &Type)>
    {
        vout!("field index for struct: {:?}.{}\n", typename, fld);
        let opt_structfields = self.structfields.get(typename);
        if opt_structfields.is_none() {
            panic!("cannot find struct fields for: {} in {:?}"
                , typename, self.structfields);
        }
        let structfields = opt_structfields.unwrap();
        structfields.iter().enumerate().find(|&(_, &(ref fname, _))| {
            &**fname == fld
        })
        .map(|(idx, &(_, ref ftype))| {
            (idx as i16, ftype)
        })
    }

    pub fn preproc_enum(&mut self, sp: &Val, loc: &SrcLoc)
    {
        let (ref name, ref src_variants) = list::take_ref(sp);
        let rc_name = name.id_name().clone();
        let local_type = Type::Enum(rc_name.clone());
        let mod_type = Type::ModPrefix(
            self.key.name.clone(),
            Rc::new(local_type.clone()),
        );

        let mut variant_fields = Vec::with_capacity(list::len(src_variants));
        for (bigi, v) in list::iter(src_variants).enumerate() {
            let i = bigi as i16;
            let variant_name = self.preproc_enum_variant(&mod_type, i, v, loc);
            let vf = (variant_name.clone(), mod_type.clone());
            variant_fields.push(vf);
        }

        self.newtypes.insert(local_type);
        self.structfields.insert((*rc_name).clone(), variant_fields);
        self.constants.insert((*rc_name).clone(), Val::Type(mod_type));
    }

    pub fn preproc_enum_variant(&mut self, typ: &Type, i: i16, var: &Val
        , loc: &SrcLoc
        ) -> Rc<String>
    {
        if let &Val::Loc(ref lvar, ref lloc) = var {
            return self.preproc_enum_variant(typ, i, lvar, lloc);
        }
        let mod_name = self.key.name.clone();
        if sxpr::is_type(var, SxprType::DefStruct) {
            let (st, sx, iloc) = sxpr::split_ref(var);
            let (variant_id, fields) = list::take_ref(sx);
            let variant_name = variant_id.id_name();
            if list::is_empty(&**fields) {
                let var_struct_type = Type::Struct(variant_name.clone());
                let const_val = Val::Enum(typ.clone(), i, variant_name.clone()
                    , Box::new(Val::Void)
                    );
                self.constants.insert((*variant_name).clone(), const_val);
                self.valtypes.insert((*variant_name).clone(), typ.clone());
            } else {
                self.preproc_struct_fields(typ, &mod_name, variant_id
                    , &**fields, loc);
            }
            variant_name.clone()
        } else if sxpr::is_type(var, SxprType::DefNamedTuple) {
            let (st, sx, iloc) = sxpr::split_ref(var);
            let (variant_id, fields) = list::take_ref(sx);
            let variant_name = variant_id.id_name();
            let field_vec = list::iter(fields).map(|f| {
                f.to_type()
            }).collect();
            self.preproc_namedtuple_func(
                typ, variant_name.clone(), field_vec, iloc);
            variant_name
        } else {
            panic!("unknown enum variant: {:?}", var);
        }
    }

    pub fn preproc_namedtuple(&mut self, pieces: &Val, loc: &SrcLoc)
    {
        let (name_id, fields) = list::take_ref(pieces);
        let name_str = name_id.id_name();
        let field_types: Vec<Type> = list::iter(fields).map(|f| {
            f.to_type()
        }).collect();
        let local_type =
            Type::NamedTuple(name_str.clone(), field_types.clone());
        let mod_type = self.modtype(local_type);

        self.preproc_namedtuple_func(&mod_type, name_str, field_types, loc);
        self.newtypes.insert(mod_type);
    }

    pub fn preproc_namedtuple_func(&mut self, mod_type: &Type
        , name_str: Rc<String>, field_types: Vec<Type>, loc: &SrcLoc)
    {
        let func_type =
            Type::Func(field_types.clone(), Box::new(mod_type.clone()));

        let field_name_vec: Vec<Rc<String>> = field_types.iter().enumerate()
            .map(|(i, _)| {
                let idx: i8 = i as i8;
                Rc::new(format!("field_{}", i))
            })
            .collect();
        let field_id_vec = field_name_vec.iter().map(|fname| {
            Val::Id(fname.clone())
        }).collect();
        let src_fields_back = field_name_vec.iter().zip(field_types.iter())
            .map(|(fname, ftyp)| {
                Val::TypedId(fname.clone(), ftyp.clone())
            })
            .fold(Val::Nil, |acc, f| {
                list::cons(f, acc)
            });
        let src_fields = list::reverse(&src_fields_back);
        let srcblk = Val::NamedTuple(mod_type.clone(),
            field_id_vec,
        );
        let srcxpr = sxpr::defunc(Val::Id(name_str.clone())
            , src_fields
            , Val::Type(mod_type.clone())
            , srcblk
            , *loc
        );

        self.constants.insert((*name_str).clone(),
            Val::FuncRef(self.key.name.clone(), name_str.clone(),
                func_type.clone()));
        self.funcsrc.insert((*name_str).clone(), srcxpr);
        self.valtypes.insert((*name_str).clone(), func_type);
        self.funcseq.push_back(name_str);
    }

    pub fn preproc_type(prog: &Lib, mp: &ModulePreface, t: &Type) -> Type
    {
        match t {
            &Type::Id(_) => {
                let prefix = mp.key.name.clone();
                Type::ModPrefix(prefix, Rc::new(t.clone()))
            }
            _ => {
                t.clone()
            }
        }
    }

    fn modtype(&self, t: Type) -> Type
    {
        Type::ModPrefix(self.key.name.clone(), Rc::new(t))
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

    use std::collections::{HashSet};
    use std::rc::{Rc};
    use std::io::{Write};


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
fn test_preproc_enum_colors()
{
    let input = String::from("
    enum PrimaryColor
    |Red
    |Yellow
    |Blue
    --
    ");

    let mut loader = Interloader::new("colors.lma");
    loader.set_mod_txt("colors", input);
    let mut prog = program::Lib::new(loader);
    let pmod = prog.read_proto("colors");

    assert_eq!(0, pmod.funcsrc.len());

    let modname = Rc::new("colors".to_string());
    let local_typename = Rc::new("PrimaryColor".to_string());
    let expected_local_type = Type::Enum(local_typename.clone());
    let expected_full_type =
        Type::ModPrefix(modname.clone(), Rc::new(expected_local_type.clone()));

    assert_eq!(1, pmod.newtypes.len());
    assert!(pmod.newtypes.contains(&expected_local_type));

    let expected_red =
        Val::Enum(expected_full_type.clone(), 0, Rc::new("Red".to_string())
            , Box::new(Val::Void));
    let red = pmod.constants.get("Red").unwrap();
    assert_eq!(expected_red, *red);
    assert!(pmod.constants.get("Yellow").is_some());
    assert!(pmod.constants.get("Blue").is_some());
    assert!(pmod.constants.get("PrimaryColor").is_some());
    assert_eq!(4, pmod.constants.len());

    assert_eq!(1, pmod.structfields.len());
    let color_flds = pmod.structfields.get("PrimaryColor").unwrap();
    assert_eq!(3, color_flds.len());
    let rfld = color_flds.get(0).unwrap();
    let yfld = color_flds.get(1).unwrap();
    let bfld = color_flds.get(2).unwrap();
    assert_eq!("Red", &*rfld.0);
    assert_eq!("Yellow", &*yfld.0);
    assert_eq!("Blue", &*bfld.0);

    let rfld_idx = pmod.struct_field_idx("PrimaryColor", "Red");
}

#[test]
fn test_enum_types()
{
    let input = "
    enum Animal
    |Dog
    |Cat(Int)
    |Mouse($A)
    |Giraffe
        .height: Int
        .weight: $A
    --
    ".to_string();
    let mut loader = Interloader::new("animals.lma");
    loader.set_mod_txt("animals", input);
    let mut prog = program::Lib::new(loader);
    let pmod = prog.read_proto("animals");

    let modname = Rc::new("animals".to_string());
    let local_typename = Rc::new("Animal".to_string());
    let expected_local_type = Type::Enum(local_typename.clone());
    let typevar_a = Type::Var(Rc::new("$A".to_string()));
    let dog_name = Rc::new("Dog".to_string());
    let cat_name = Rc::new("Cat".to_string());
    let mouse_name = Rc::new("Mouse".to_string());
    let giraffe_name = Rc::new("Giraffe".to_string());
    let expected_type =
        Type::ModPrefix(
            Rc::new("animals".to_string()),
            Rc::new(Type::Enum(Rc::new("Animal".to_string()))),
        );
    let cat_func_type =
        Type::Func(
            vec![
                Type::Int,
            ],
            Box::new(expected_type.clone()),
        );
    let mouse_func_type =
        Type::Func(
            vec![typevar_a.clone()],
            Box::new(expected_type.clone()),
        );
    let giraffe_func_type =
        Type::Func(
            vec![
                Type::Int,
                typevar_a.clone(),
            ],
            Box::new(expected_type.clone()),
        );

    // verify constants
    assert_eq!(5, pmod.constants.len());
    let dog_const = pmod.constants.get("Dog").expect("missing constant: Dog");
    let cat_const = pmod.constants.get("Cat").expect("missing constant: Cat");
    let giraffe_const =
        pmod.constants.get("Giraffe").expect("missing constant: Giraffe");

    let exp_dog_const = Val::Enum(expected_type.clone(), 0,
        dog_name.clone(),
        Box::new(Val::Void),
    );
    let exp_cat_const = Val::FuncRef(
        Rc::new("animals".to_string()),
        cat_name.clone(),
        cat_func_type.clone(),
    );
    let exp_giraffe_const = Val::FuncRef(
        Rc::new("animals".to_string()),
        giraffe_name.clone(),
        giraffe_func_type.clone(),
    );
    assert_eq!(exp_dog_const, *dog_const);
    assert_eq!(exp_cat_const, *cat_const);
    assert_eq!(exp_giraffe_const, *giraffe_const);

    // verify constant string formatting
    let dog_str = format!("{}", dog_const);
    assert_eq!("Dog", dog_str);

    // verify newtypes
    assert_eq!(1, pmod.newtypes.len());
    assert!(pmod.newtypes.contains(&expected_local_type));

    // verify function sequence
    assert_eq!(3, pmod.funcseq.len());
    let mut fseq_it = pmod.funcseq.iter();
    assert_eq!("Cat", **fseq_it.next().unwrap());
    assert_eq!("Mouse", **fseq_it.next().unwrap());
    assert_eq!("Giraffe", **fseq_it.next().unwrap());

    // verify function source
    assert!(pmod.funcsrc.get("Dog").is_none());
    assert!(pmod.funcsrc.get("Cat").is_some());
    assert!(pmod.funcsrc.get("Mouse").is_some());
    assert!(pmod.funcsrc.get("Giraffe").is_some());
    assert_eq!(3, pmod.funcsrc.len());

    // verify value types
    assert_eq!("animals::Animal",
        format!("{}", *pmod.valtypes.get("Dog").unwrap()));
    assert_eq!("Int > animals::Animal",
        format!("{}", *pmod.valtypes.get("Cat").unwrap()));
    assert_eq!(*pmod.valtypes.get("Mouse").unwrap(), mouse_func_type);
    assert_eq!(*pmod.valtypes.get("Giraffe").unwrap(), giraffe_func_type);
    assert_eq!(4, pmod.valtypes.len());

    // verify struct fields
    assert_eq!(2, pmod.structfields.len());
    let variants = pmod.structfields.get("Animal").unwrap();
    let variant_dog = variants.get(0).unwrap();
    let variant_cat = variants.get(1).unwrap();
    let variant_mouse = variants.get(2).unwrap();
    let variant_giraffe = variants.get(3).unwrap();
    let giraffe_fields = pmod.structfields.get("Giraffe").unwrap();
    let giraffe_field_height = giraffe_fields.get(0).unwrap();
    let giraffe_field_weight = giraffe_fields.get(1).unwrap();
    assert_eq!(4, variants.len());
    assert_eq!("Dog", *variant_dog.0);
    assert_eq!("Cat", *variant_cat.0);
    assert_eq!("Mouse", *variant_mouse.0);
    assert_eq!("Giraffe", *variant_giraffe.0);
    assert_eq!(expected_type, variant_dog.1);
    assert_eq!(expected_type, variant_cat.1);
    assert_eq!(expected_type, variant_mouse.1);
    assert_eq!(expected_type, variant_giraffe.1);
    assert_eq!(2, giraffe_fields.len());
    assert_eq!("height", *giraffe_field_height.0);
    assert_eq!("weight", *giraffe_field_weight.0);
    assert_eq!(Type::Int, giraffe_field_height.1);
    assert_eq!(typevar_a, giraffe_field_weight.1);
}

#[test]
fn test_preproc_namedtuple()
{
    let input = "
    struct Greeting(Str, Str)
    ".to_string();
    let mut loader = Interloader::new("greet.lma");
    loader.set_mod_txt("greet", input);
    let mut prog = program::Lib::new(loader);
    let pmod = prog.read_proto("greet");

    let greet = Rc::new("greet".to_string());
    let greeting_str = Rc::new("Greeting".to_string());
    let greeting_ntt = Type::NamedTuple(greeting_str.clone(), vec![
        Type::Str, Type::Str]);
    let mod_greeting_ntt = Type::ModPrefix(
        greet.clone(),
        Rc::new(greeting_ntt.clone()),
    );
    let xfunctyp = Type::Func(
        vec![Type::Str, Type::Str],
        Box::new(mod_greeting_ntt.clone()),
    );

    // assert newtypes
    let gnewtype: HashSet<Type> =
        vec![mod_greeting_ntt.clone()].into_iter().collect();
    assert_eq!(gnewtype, pmod.newtypes);
    assert_eq!(1, pmod.newtypes.len());

    // constants
    assert_eq!(
        Val::FuncRef(greet.clone(), greeting_str.clone(), xfunctyp.clone()),
        *pmod.constants.get("Greeting").unwrap()
    );
    assert_eq!(1, pmod.constants.len());

    // assert funcsrc
    assert_eq!(1, pmod.funcsrc.len());

    // assert funcseq
    assert_eq!(1, pmod.funcseq.len());
    assert_eq!("Greeting", **pmod.funcseq.front().unwrap());

    // verify valtypes
    assert_eq!(1, pmod.valtypes.len());
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

    assert!(proto.newtypes.contains(&Type::Struct(struct_name.clone())));
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

    assert!(proto.structfields.contains_key("Burrito"));

    let structfields = proto.structfields.get("Burrito").unwrap();
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

#[test]
fn test_token_type()
{
    let input = String::from("
    struct Burrito --
    ");

    let mut loader = Interloader::new("tok.lma");
    loader.set_mod_txt("tok", input);
    let mut prog = program::Lib::new(loader);
    let pmod = prog.read_proto("tok");

    let name_rc = Rc::new("Burrito".to_string());
    let exptype = Type::ModPrefix(
        Rc::new("tok".to_string()),
        Rc::new(Type::Token(name_rc.clone())),
    );

    // verify newtypes
    let expset: HashSet<Type> = vec![exptype.clone()].into_iter().collect();
    assert_eq!(expset, pmod.newtypes);
    assert!(pmod.newtypes.contains(&exptype));
    assert_eq!(1, pmod.newtypes.len());

    // verify valtypes
    assert_eq!(exptype, *pmod.valtypes.get("Burrito").unwrap());
    assert_eq!(1, pmod.valtypes.len());

    // verify constants
    assert_eq!(Val::Token(exptype.clone())
        , *pmod.constants.get("Burrito").unwrap());
    assert_eq!(1, pmod.constants.len());

    // assert on fields that shouldn't have changed
    assert_eq!(0, pmod.funcseq.len());
    assert_eq!(0, pmod.funcsrc.len());
    assert_eq!(0, pmod.structfields.len());
}

}
