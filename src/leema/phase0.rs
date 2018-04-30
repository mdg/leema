use leema::program::{Lib};
use leema::ast::{self, Ast};
use leema::lstr::{Lstr};
use leema::val::{SxprType, Val, Type, SrcLoc};
use leema::module::{ModKey, ModulePreface, MacroDef};
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
    pub funcsrc: HashMap<String, Ast>,
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
            , mp: &ModulePreface, x: &Ast
    ) {
        match x {
            &Ast::DefFunc(ast::FuncClass::Macro, _, _, _, _, _) => {
                // do nothing. the macro definition will have been handled
                // in the file read
            }
            &Ast::DefFunc(
                fclass, ref name, ref args, ref result_type, ref body, ref loc
            ) => {
                self.preproc_defunc(prog, mp, fclass, name
                    , args, result_type, body, loc);
            }
            &Ast::DefData(data_type, ref name, ref fields, ref loc
            ) => {
                self.preproc_data(data_type, name, fields, loc);
            }
            &Ast::Import(imports, _) => {
                // do nothing. imports handled in file read
            }
            _ => {
                println!("Cannot phase0: {:?}", x);
            }
        }
    }

    pub fn preproc_expr(prog: &Lib, mp: &ModulePreface, x: &Ast, loc: &SrcLoc
        ) -> Ast
    {
        match x {
            &Ast::Block(ref items, ref iloc) => {
                let pp_items = items.iter().map(|i| {
                    Protomod::preproc_expr(prog, mp, i, iloc)
                });
                Ast::Block(pp_items, iloc)
            }
            &Ast::Cons(ref head, ref tail, ref iloc) => {
                let pp_head = Protomod::preproc_expr(prog, mp, head, iloc);
                let pp_tail = Protomod::preproc_expr(prog, mp, tail, iloc);
                Ast::Cons(pp_head, pp_tail, loc)
            }
            &Ast::ConstBool(b) => Ast::ConstBool(b),
            &Ast::ConstHashtag(_) => x.clone(),
            &Ast::ConstInt(i) => Ast::ConstInt(i),
            &Ast::ConstStr(_) => x.clone(),
            &Ast::ConstVoid => Ast::ConstVoid,
            &Ast::DotAccess(ref base, ref fld) => {
                let ppbase = Protomod::preproc_expr(prog, mp, base, loc);
                Ast::DotAccess(Box::new(ppbase), fld.clone())
            }
            &Ast::IfExpr(iftype, ref input, ref case, ref iloc) => {
                let pp_input = Protomod::preproc_expr(prog, mp, input, iloc);
                let pp_case =
                    Protomod::preproc_ifcase(prog, mp, iftype, case, iloc);
                Ast::IfExpr(iftype, pp_input, pp_case, iloc)
            }
            &Ast::Let(let_type, ref left, ref right, ref iloc) => {
                let pp_left = Protomod::preproc_pattern(prog, mp, left, iloc);
                let pp_right = Protomod::preproc_expr(prog, mp, right, iloc);
                Ast::Let(let_type, pp_left, pp_right, iloc)
            }
            &Ast::Call(ref callx, ref args, ref iloc) => {
                Protomod::preproc_call(prog, mp, callx, args, iloc)
            }
            &Ast::Localid(_, _) => {
                x.clone()
            }
            &Ast::Cons(ref head, ref tail) => {
                Protomod::preproc_cons(prog, mp, head, tail, loc)
            }
            &Val::TypedId(ref id, ref typ) => {
                let pptyp = Protomod::preproc_type(prog, mp, typ);
                Val::TypedId(id.clone(), pptyp)
            }
            &Ast::Tuple(ref items) if items.len() == 1 => {
                // one-tuples are compiled to just the value
                Protomod::preproc_expr(prog, mp, items.front().unwrap(), loc)
            }
            &Ast::Tuple(ref items) => {
                let pp_items = items.iter().map(|i| {
                    Protomod::preproc_expr(prog, mp, i, loc)
                }).collect();
                Ast::Tuple(pp_items)
            }
            &Val::ModPrefix(ref prefix, ref name) => {
                if !mp.imports.contains(&**prefix) {
                    panic!("cannot find {} module. maybe import it?", prefix);
                }
                let ppname = Protomod::preproc_expr(prog, mp, name, loc);
                Val::ModPrefix(prefix.clone(), Rc::new(ppname))
            }
            &Ast::RustBlock => Ast::RustBlock,
            &Ast::TypeVoid => Ast::TypeVoid,
            &Ast::Wildcard => Ast::Wildcard,
            _ => {
                println!("preproc_unknown_expr({:?})", x);
                x.clone()
            }
        }
    }

    pub fn preproc_cons(prog: &Lib, mp: &ModulePreface
            , head: &Ast, tail: &Ast, loc: &SrcLoc) -> Ast
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

    pub fn preproc_defunc(&mut self, prog: &Lib, mp: &ModulePreface
        , fclass: ast::FuncClass, name: &Ast, args: &LinkedList<Ast>
        , rtype: &Ast, body: &Ast, loc: &SrcLoc
        )
    {
        let lstr_name = Lstr::from(name);
        let pp_args = args.iter().map(|a| {
            Protomod::preproc_expr(prog, mp, a, loc)
        });
        let pp_rtype = Protomod::preproc_expr(prog, mp, rtype, loc);
        let pp_body = Protomod::preproc_expr(prog, mp, body, loc);
        let pp_func =
            Ast::DefFunc(fclass, name.clone(), pp_args, pp_rtype, pp_body, loc);
        let lstr_name = Lstr::from(name);
        let rcname: Rc<String> = From::from(lstr_name);
        let strname = String::from(rcname);

        let ftype_parts =
            pp_args.iter().map(|a| {
                a.get_type()
            })
            .collect()
            .push(pp_rtype.clone());
        let ftype = Ast::TypeFunc(ftype_parts, loc);

        self.funcseq.push_back(rcname);
        self.funcsrc.insert(strname.clone(), pp_func);
        self.valtypes.insert(strname, ftype);
    }

    pub fn preproc_call(prog: &Lib, mp: &ModulePreface
            , callx: &Ast, args: &LinkedList<Ast>, loc: &SrcLoc) -> Val
    {
        let pp_args = args.iter().map(|arg| {
            Protomod::preproc_expr(prog, mp, arg, loc)
        });
        match callx {
            &Val::Id(ref id) => {
                match mp.macros.get(&**id) {
                    Some(&(ref arg_names, ref body)) => {
                        let macrod =
                            Protomod::apply_macro(
                                callx, body, arg_names, args, loc);
                        // do it again to make sure there's not a wrapped
                        // macro
                        Protomod::preproc_expr(prog, mp, &macrod, loc)
                    }
                    None => {
                        match prog.get_macro("prefab", &**id) {
                            Some(&(ref arg_names, ref body)) => {
                                let macrod = Protomod::apply_macro(callx
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
                        callx, body, arg_names, args, loc);
                Protomod::preproc_expr(prog, mp, &result, loc)
            }
            &Val::Sxpr(SxprType::Lri, ref lri, ref loc) => {
                let mac = Protomod::get_macro(prog, lri);
                if mac.is_none() {
                    let pplri = Protomod::preproc_sxpr(prog, mp
                        , SxprType::Lri, lri, loc);
                    return sxpr::call(pplri, pp_args, *loc);
                }
                let &(ref arg_names, ref body) = mac.unwrap();
                let result = Protomod::apply_macro(
                        lri, body, arg_names, args, loc);
                Protomod::preproc_expr(prog, mp, &result, loc)
            }
            &Val::Sxpr(SxprType::TypeParams, ref params, ref loc) => {
                // nothing with type parameters is going to be a macro. proceed.
                Protomod::preproc_sxpr(prog, mp
                    , SxprType::TypeParams, params, loc)
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
                                callx, body, arg_names, args, loc);
                        Protomod::preproc_expr(prog, mp, &result, loc)
                    }
                    _ => {
                        sxpr::call(callx.clone(), pp_args, *loc)
                    }
                }
            }
            _ => {
                panic!("function call is what? {:?}@{:?}", callx, loc);
            }
        }
    }

    pub fn get_macro<'a, 'b>(prog: &'a Lib, lri_items: &'b Val
        ) -> Option<&'a MacroDef>
    {
        None
    }

    pub fn apply_macro(macro_name: &Val, body: &Val
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

    pub fn preproc_ifcase(prog: &Lib, mp: &ModulePreface, iftype: ast::IfType
        , case: &ast::IfCase, loc: &SrcLoc
        ) -> ast::IfCase
    {
        let pp_cond =
            match iftype {
                ast::IfType::If => {
                    Protomod::preproc_expr(prog, mp, &case.cond, &case.loc)
                }
                ast::IfType::Match => {
                    Protomod::preproc_pattern(prog, mp, &case.cond, &case.loc)
                }
                ast::IfType::MatchFailure => {
                    Protomod::preproc_pattern(prog, mp, &case.cond, &case.loc)
                }
                ast::IfType::TypeCast => {
                    panic!("typecast not ready yet");
                }
            };
        let pp_body = Protomod::preproc_expr(prog, mp, &case.body, &case.loc);
        let pp_else = case.else_case.map(|else_case| {
            Protomod::preproc_ifcase(prog, mp, iftype, &*else_case, &case.loc)
        });
        ast::IfCase::new(pp_cond, pp_body, pp_else, *loc)
    }

    pub fn preproc_pattern(prog: &Lib, mp: &ModulePreface, p: &Ast
        , loc: &SrcLoc
        ) -> Ast
    {
        match p {
            &Ast::Cons(ref head, ref tail) => {
                let phead = Protomod::preproc_pattern(prog, mp, head, loc);
                let ptail = Protomod::preproc_pattern(prog, mp, tail, loc);
                Ast::Cons(Box::new(phead), Box::new(ptail))
            }
            &Ast::Tuple(ref items) if items.len() == 1 => {
                let first = items.front().unwrap();
                Protomod::preproc_pattern(prog, mp, first, loc)
            }
            &Ast::Tuple(ref items) => {
                Ast::Tuple(
                    items.iter().map(|i| {
                        Protomod::preproc_pattern(prog, mp, i, loc)
                    }).collect()
                )
            }
            &Ast::Call(ref name, ref args, ref iloc) => {
                let pp_callx =
                    Protomod::preproc_pattern(prog, mp, name, iloc);
                let pp_args = args.iter().map(|px| {
                    Protomod::preproc_pattern(prog, mp, px, iloc)
                }).collect();
                Ast::Call(Box::new(pp_callx), pp_args, *loc)
            }
            &Ast::Localid(_, _) => {
                p.clone()
            }
            &Ast::Wildcard => Ast::Wildcard,
            &Ast::ConstInt(i) => Ast::ConstInt(i),
            &Ast::ConstBool(b) => Ast::ConstBool(b),
            &Ast::ConstStr(ref s) => Ast::ConstStr(s.clone()),
            &Ast::ConstHashtag(ref h) => Ast::ConstHashtag(h.clone()),
            &Ast::ConstVoid => Ast::ConstVoid,
            _ => {
                println!("preproc_pattern what?: {:?}", p);
                p.clone()
            }
        }
    }

    pub fn preproc_data(&mut self, prog: &Lib, mp: &ModulePreface
        , datatype: ast::DataType, name: &Ast
        , fields: &LinkedList<Ast>, loc: &SrcLoc
        )
    {
        match datatype {
            ast::DataType::Struct => {
                if fields.is_empty() {
                    self.preproc_token_struct(prog, mp, name, loc);
                } else {
                    self.preproc_struct_with_fields(
                        prog, mp, name, fields, loc);
                }
            }
            ast::DataType::Enum => {
                self.preproc_enum(prog, mp, name, fields, loc);
            }
            ast::DataType::NamedTuple => {
                self.preproc_namedtuple(prog, mp, name, name, fields, loc);
            }
        }
    }

    pub fn preproc_token_struct(&mut self, prog: &Lib, mp: &ModulePreface
        , name: &Ast, loc: &SrcLoc
        )
    {
        let name_lstr = Lstr::from(name);
        let rc_name: Rc<String> = From::from(&name_lstr);

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

    pub fn preproc_struct_with_fields(&mut self, prog: &Lib
        , mp: &ModulePreface, name: &Ast
        , src_fields: &LinkedList<Ast>, loc: &SrcLoc
        )
    {
        let name_lstr = Lstr::from(name);
        let rc_name: Rc<String> = From::from(&name_lstr);

        let local_type = Type::Struct(rc_name.clone());
        let mod_name = self.key.name.clone();

        self.preproc_struct_fields(prog, mp, &name, &mod_name
            , name, src_fields, loc);
        self.newtypes.insert(local_type);
    }

    pub fn preproc_struct_fields(&mut self, prog: &Lib, mp: &ModulePreface
        , typename: &Ast
        , mod_name: &Rc<String>
        , name: &Ast, src_fields: &LinkedList<Ast>, loc: &SrcLoc
        )
    {
        let name_lstr = Lstr::from(name);
        let rc_name: Rc<String> = From::from(&name_lstr);

        let struct_fields: Vec<(Rc<String>, Type)> =
            src_fields.iter().map(|f| {
                if let &Ast::KeyedExpr(ref key, ref x, ref loc) = f {
                    let key_rc: Rc<String> = From::from(key);
                    let xtype = Protomod::preproc_type(prog, mp, x);
                    (key_rc, xtype)
                } else {
                    panic!("struct field must have a name and a type: {:?}", f);
                }
            }).collect();

        let field_id_vec: Vec<Ast> =
            struct_fields.iter().map(|&(fname, ft)| {
                Ast::Localid(Lstr::Rc(fname.clone()), SrcLoc::default())
            }).collect();
        let field_type_vec = struct_fields.iter().map(|&(_, ftype)| {
            ftype.clone()
        }).collect();

        let result_type = Protomod::preproc_type(prog, mp, &typename);
        let func_type = Type::Func(field_type_vec, Box::new(result_type));

        let srcblk = Ast::ConstructData(ast::DataType::Struct
            , Box::new(typename.clone()), field_id_vec
            );
        let srcxpr = Ast::DefFunc(ast::FuncClass::Func
            , Box::new((*name).clone())
            , (*src_fields).clone(), Box::new(typename.clone())
            , Box::new(srcblk), *loc);

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

    pub fn preproc_enum(&mut self, prog: &Lib, mp: &ModulePreface
        , name: &Ast, src_variants: &LinkedList<Ast>
        , loc: &SrcLoc)
    {
        let name_lstr = Lstr::from(name);
        let rc_name: Rc<String> = From::from(&name_lstr);
        let local_type = Type::Enum(rc_name.clone());
        let mod_type = Type::ModPrefix(
            self.key.name.clone(),
            Rc::new(local_type.clone()),
        );

        let mut variant_fields = Vec::with_capacity(src_variants.len());
        for (bigi, v) in src_variants.iter().enumerate() {
            let i = bigi as i16;
            if let &Ast::DefData(vdatatype, ref vname, ref fields, ref loc) = v {
                // would be nice if this wasn't necessary
                // maybe make all of the fields LinkedLists?
                let field_vec = fields.iter().map(|f| {
                    f.clone()
                }).collect();
                self.preproc_enum_variant(prog, mp, &name, i
                    , vdatatype, name, &field_vec, loc);
                let variant_lstr = Lstr::from(&**vname);
                let variant_name: Rc<String> = From::from(&variant_lstr);
                let vf = (variant_name, mod_type.clone());
                variant_fields.push(vf);
            } else {
                panic!("variant data is not DefData: {:?}", v);
            }
        }

        self.newtypes.insert(local_type);
        self.structfields.insert((*rc_name).clone(), variant_fields);
        self.constants.insert((*rc_name).clone(), Val::Type(mod_type));
    }

    pub fn preproc_enum_variant(&mut self, prog: &Lib, mp: &ModulePreface
        , typename: &Ast, i: i16, dataclass: ast::DataType
        , name: &Ast, fields: &LinkedList<Ast>, loc: &SrcLoc
        )
    {
        let mod_name = self.key.name.clone();
        let typ = Protomod::preproc_type(prog, mp, typename);
        let variant_name = Lstr::from(name);
        if dataclass == ast::DataType::Struct {
            if fields.is_empty() {
                let variant_name_rc: Rc<String> = From::from(&variant_name);
                let var_struct_type = Type::Struct(variant_name_rc.clone());
                let const_val = Val::Enum(typ.clone(), i, variant_name_rc
                    , Box::new(Val::Void)
                    );
                let variant_name_string = String::from(&variant_name);
                self.constants.insert(variant_name_string.clone(), const_val);
                self.valtypes.insert(variant_name_string, typ);
            } else {
                self.preproc_struct_fields(prog, mp, typename
                    , &mod_name, name, fields, loc);
            }
        } else if dataclass == ast::DataType::NamedTuple {
            let field_vec = fields.iter().map(|f| {
                Protomod::preproc_type(prog, mp, f)
            }).collect();
            self.preproc_namedtuple_func(prog, mp
                , typename, name, fields, loc);
        } else {
            panic!("unknown enum variant type: {:?}", dataclass);
        }
    }

    pub fn preproc_namedtuple(&mut self, prog: &Lib, mp: &ModulePreface
        , typename: &Ast, name: &Ast, fields: &LinkedList<Ast>, loc: &SrcLoc)
    {
        let name_lstr = Lstr::from(name);
        let name_rcstr: Rc<String> = From::from(&name_lstr);
        let field_types: Vec<Type> = fields.iter().map(|f| {
            Protomod::preproc_type(prog, mp, f)
        }).collect();
        let local_type =
            Type::NamedTuple(name_rcstr, field_types.clone());
        let mod_type = self.modtype(local_type);

        self.preproc_namedtuple_func(prog, mp, typename, name
            , fields, loc);
        self.newtypes.insert(mod_type);
    }

    pub fn preproc_namedtuple_func(&mut self, prog: &Lib, mp: &ModulePreface
        , typename: &Ast, name: &Ast, field_types: &LinkedList<Ast>
        , loc: &SrcLoc)
    {
        let name_lstr = Lstr::from(name);
        let name_str: Rc<String> = From::from(&name_lstr);

        let field_id_vec: Vec<Ast> = field_types.iter().enumerate()
            .map(|(i, ft)| {
                let idx: i8 = i as i8;
                let fname = Rc::new(format!("field_{}", i));
                Ast::Localid(Lstr::Rc(fname), *ft.loc())
            })
            .collect();
        let field_type_vec: Vec<Type> = field_types.iter().map(|ft| {
            Protomod::preproc_type(prog, mp, ft)
        }).collect();
        let named_type =
            Type::NamedTuple(name_str.clone(), field_type_vec.clone());
        let func_type =
            Type::Func(field_type_vec, Box::new(named_type.clone()));
        let srcblk = Ast::ConstructData(ast::DataType::NamedTuple
            , Box::new(typename.clone()), field_id_vec);
        let srcxpr = Ast::DefFunc(ast::FuncClass::Func
            , Box::new(name.clone()), field_types.clone()
            , Box::new(typename.clone())
            , Box::new(srcblk), loc.clone()
            );

        self.constants.insert((*name_str).clone(),
            Val::FuncRef(self.key.name.clone(), name_str.clone(),
                func_type.clone()));
        self.funcsrc.insert((*name_str).clone(), srcxpr);
        self.valtypes.insert((*name_str).clone(), func_type);
        self.funcseq.push_back(name_str);
    }

    pub fn preproc_type(prog: &Lib, mp: &ModulePreface, t: &Ast) -> Type
    {
        match t {
            &Ast::TypeInt => Type::Int,
            &Ast::TypeBool => Type::Bool,
            &Ast::TypeHashtag => Type::Hashtag,
            &Ast::TypeStr => Type::Str,
            &Ast::TypeVoid => Type::Void,
            &Ast::Lri(ref names, ref types, ref loc) => {
                Type::Void // Type::Lri(lri.clone()),
            }
            _ => {
                panic!("cannot preproc type: {:?}", t);
            }
        }
    }

    fn modtype(&self, t: Type) -> Type
    {
        Type::ModPrefix(self.key.name.clone(), Rc::new(t))
    }
}

pub fn preproc(prog: &mut Lib, mp: &ModulePreface, ast: &Ast) -> Protomod
{
    let mk = mp.key.clone();
    let mut p = Protomod::new(mk);
    match ast {
        &Ast::Block(ref lines) => {
            for x in lines.iter() {
                p.preproc_module_expr(prog, mp, x);
            }
        }
        _ => {
            println!("preproc(something_else, {:?})", ast);
            p.preproc_module_expr(prog, mp, ast);
        }
    }
    p
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
