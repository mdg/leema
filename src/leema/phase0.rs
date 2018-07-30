use leema::program::{Lib};
use leema::ast::{self, Ast, Kxpr};
use leema::lri::{Lri};
use leema::lstr::{Lstr};
use leema::struple::{Struple};
use leema::types;
use leema::val::{Val, Type, SrcLoc};
use leema::module::{ModKey, ModulePreface};
use leema::list;
use leema::log;

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
    pub constants: HashMap<String, Val>,
    pub deftypes: HashMap<Lstr, Type>,
}

impl Protomod
{
    pub fn new(mk: Rc<ModKey>) -> Protomod
    {
        let mut empty_consts = HashMap::new();
        empty_consts.insert("TYPES".to_string(), Val::Nil);
        Protomod{
            key: mk,
            funcseq: LinkedList::new(),
            funcsrc: HashMap::new(),
            valtypes: HashMap::new(),
            constants: empty_consts,
            deftypes: HashMap::new(),
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
            &Ast::DefData(data_type, ref name, ref fields, ref loc) => {
                self.preproc_data(prog, mp, data_type, name, fields, loc);
            }
            &Ast::Import(ref imports, _) => {
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
            &Ast::Block(ref items) => {
                let pp_items = items.iter().map(|i| {
                    Protomod::preproc_expr(prog, mp, i, loc)
                }).collect();
                Ast::Block(pp_items)
            }
            &Ast::Cons(ref head, ref tail) => {
                let pp_head = Protomod::preproc_expr(prog, mp, head, loc);
                let pp_tail = Protomod::preproc_expr(prog, mp, tail, loc);
                Ast::Cons(Box::new(pp_head), Box::new(pp_tail))
            }
            &Ast::ConstructData(datat, ref name, ref data) => {
                vout!("how did this construct data get here? {:?}", x);
                let ppname = Protomod::preproc_expr(prog, mp, name, loc);
                let ppdata = data.iter().map(|i| {
                    Protomod::preproc_expr(prog, mp, i, loc)
                }).collect();
                Ast::ConstructData(datat, Box::new(ppname), ppdata)
            }
            &Ast::ConstBool(b) => Ast::ConstBool(b),
            &Ast::ConstHashtag(_) => x.clone(),
            &Ast::ConstInt(i) => Ast::ConstInt(i),
            &Ast::ConstStr(_) => x.clone(),
            &Ast::ConstVoid => Ast::ConstVoid,
            &Ast::Deref(ref inner) => {
                Ast::Deref(Box::new(
                    Protomod::preproc_expr(prog, mp, inner, loc)
                ))
            }
            &Ast::DotAccess(ref base, ref fld) => {
                let ppbase = Protomod::preproc_expr(prog, mp, base, loc);
                Ast::DotAccess(Box::new(ppbase), fld.clone())
            }
            &Ast::IfExpr(iftype, ref input, ref case, ref iloc) => {
                let pp_input = Protomod::preproc_expr(prog, mp, input, iloc);
                let pp_case =
                    Protomod::preproc_ifcase(prog, mp, iftype, case, iloc);
                Ast::IfExpr(iftype
                    , Box::new(pp_input), Box::new(pp_case), *iloc)
            }
            &Ast::Let(let_type, ref left, ref right, ref iloc) => {
                let pp_left = Protomod::preproc_pattern(prog, mp, left, iloc);
                let pp_right = Protomod::preproc_expr(prog, mp, right, iloc);
                Ast::Let(let_type, Box::new(pp_left), Box::new(pp_right), *iloc)
            }
            &Ast::Call(ref callx, ref args, ref iloc) => {
                Protomod::preproc_call(prog, mp, callx, args, iloc)
            }
            &Ast::List(ref items) => {
                Ast::List(items.iter().map(|i| {
                    Protomod::preproc_expr(prog, mp, i, loc)
                }).collect())
            }
            &Ast::Localid(_, _) => {
                x.clone()
            }
            &Ast::Lri(ref mods, None, ref iloc) => {
                Protomod::preproc_lri(prog, mp, mods, iloc)
            }
            &Ast::Lri(ref mods, Some(ref typs), ref iloc) => {
                Protomod::preproc_lri_with_types(prog, mp, mods, typs, iloc)
            }
            &Ast::Return(ref x, ref loc) => {
                let px = Protomod::preproc_expr(prog, mp, x, loc);
                Ast::Return(Box::new(px), *loc)
            }
            &Ast::StrExpr(ref xs, ref loc) => {
                let pxs = xs.iter().map(|x| {
                    Protomod::preproc_expr(prog, mp, x, loc)
                }).collect();
                Ast::StrExpr(pxs, *loc)
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
            &Ast::TypeFunc(ref parts, ref loc) => {
                let ppp = parts.iter().map(|p| {
                    Protomod::preproc_func_arg(prog, mp
                        , &Lstr::Sref("anon_func_type"), p, loc)
                }).collect();
                Ast::TypeFunc(ppp, *loc)
            }
            &Ast::RustBlock => Ast::RustBlock,
            &Ast::TypeAnon => Ast::TypeAnon,
            &Ast::TypeBool => Ast::TypeBool,
            &Ast::TypeHashtag => Ast::TypeHashtag,
            &Ast::TypeInt => Ast::TypeInt,
            &Ast::TypeStr => Ast::TypeStr,
            &Ast::TypeVar(ref v, ref loc) => Ast::TypeVar(v.clone(), *loc),
            &Ast::TypeVoid => Ast::TypeVoid,
            &Ast::Wildcard => Ast::Wildcard,
            &Ast::DefData(_, _, _, _) => {
                panic!("cannot preproc: {:?}", x);
            }
            &Ast::DefFunc(_, _, _, _, _, _) => {
                panic!("cannot preproc: {:?}", x);
            }
            &Ast::Import(_, _) => {
                panic!("cannot preproc: {:?}", x);
            }
        }
    }

    pub fn preproc_func_arg(prog: &Lib, mp: &ModulePreface
        , func_name: &Lstr, arg: &Kxpr, loc: &SrcLoc
        ) -> Kxpr
    {
        match (arg.k_ref(), arg.x_ref()) {
            (None, None) => {
                panic!("cannot preproc arg with no id or type: {:?}", loc);
            }
            (None, Some(&Ast::TypeAnon)) => {
                panic!("cannot preproc arg with no id and anonymous type: {:?}"
                    , loc);
            }
            (None, Some(typ)) => {
                let pp_typ = Protomod::preproc_expr(prog, mp, typ, &loc);
                Kxpr::new_x(pp_typ)
            }
            (Some(id), None) => {
                let type_name = Lstr::from(
                    format!("{}_{}_{}", mp.key.name, func_name.str(), id)
                );
                let typ = Ast::TypeVar(type_name, *loc);
                Kxpr::new(id.clone(), typ)
            }
            (Some(id), Some(&Ast::TypeAnon)) => {
                let type_name = Lstr::from(
                    format!("{}_{}_{}", mp.key.name, func_name.str(), id)
                );
                let new_typ = Ast::TypeVar(type_name, *loc);
                Kxpr::new(id.clone(), new_typ)
            }
            (Some(id), Some(typ)) => {
                let pp_typ = Protomod::preproc_expr(prog, mp, typ, &loc);
                Kxpr::new(id.clone(), pp_typ)
            }
        }
    }

    pub fn preproc_defunc(&mut self, prog: &Lib, mp: &ModulePreface
        , fclass: ast::FuncClass, name: &Ast, args: &LinkedList<Kxpr>
        , rtype: &Ast, body: &Ast, loc: &SrcLoc
        )
    {
        let lstr_name = Lstr::from(name);
        let pp_args: LinkedList<Kxpr> = args.iter().map(|a| {
            Protomod::preproc_func_arg(prog, mp, &lstr_name, a, loc)
        }).collect();
        let pp_rtype_ast = Protomod::preproc_expr(prog, mp, rtype, loc);
        let pp_body = Protomod::preproc_expr(prog, mp, body, loc);
        let pp_func =
            Ast::DefFunc(fclass, Box::new(name.clone())
                , pp_args.clone(), Box::new(pp_rtype_ast.clone())
                , Box::new(pp_body), *loc
            );
        let lstr_name = Lstr::from(name);
        let rcname: Rc<String> = From::from(&lstr_name);
        let strname = (*rcname).clone();

        let mut ftype_parts: Vec<Kxpr> =
            pp_args.iter().map(|a| {
                // Protomod::preproc_type(prog, mp, a, loc)
                a.clone()
            })
            .collect();
        ftype_parts.push(Kxpr::new_x(pp_rtype_ast));
        let ftype_ast = Ast::TypeFunc(ftype_parts, *loc);
        let ftype = Type::from(&ftype_ast);

        self.funcseq.push_back(rcname);
        self.funcsrc.insert(strname.clone(), pp_func);
        self.valtypes.insert(strname, ftype);
    }

    pub fn preproc_call(prog: &Lib, mp: &ModulePreface
            , callx: &Ast, args: &LinkedList<Ast>, loc: &SrcLoc) -> Ast
    {
        let pp_args = args.iter().map(|arg| {
            Protomod::preproc_expr(prog, mp, arg, loc)
        }).collect();
        let pp_callx = Protomod::preproc_expr(prog, mp, callx, loc);
        match pp_callx {
            Ast::DefFunc(ast::FuncClass::Macro, mname, margs, _
                    , body, _
            ) => {
                vout!("apply_macro({:?}, {:?})\n", mname, args);
                let macrod =
                    Protomod::apply_macro(
                        &mname, &body, &margs, args, loc);
                // do it again to make sure there's not a wrapped macro
                Protomod::preproc_expr(prog, mp, &macrod, loc)
            }
            _ => {
                Ast::Call(Box::new(pp_callx), pp_args, *loc)
            }
        }
    }

    pub fn apply_macro(macro_name: &Ast, body: &Ast
        , arg_names: &LinkedList<Kxpr>, args: &LinkedList<Ast>, loc: &SrcLoc
        ) -> Ast
    {
        let mut arg_map = HashMap::new();
        match (arg_names.len(), args.len()) {
            (a, b) if a < b => {
                panic!("Too many arguments passed to macro {:?}, expected {}"
                    , macro_name, a);
            }
            (a, b) if a > b => {
                panic!("Too few arguments passed to macro {:?}, expected {}"
                    , macro_name, a);
            }
            _ => {
                // a == b. cool, proceed
            }
        }
        for (n, arg_val) in arg_names.iter().zip(args.iter()) {
            let n_opt = n.k_ref();
            if n_opt.is_none() {
                panic!("macro has unnamed args: {:?} -> {:?}"
                    , macro_name, arg_names);
            }
            let n_lstr = n_opt.unwrap().clone();
            arg_map.insert(n_lstr, arg_val);
        }
        vout!("replace_ids({:?})\n", arg_map);
        Protomod::replace_ids(body, &arg_map, loc)
    }

    pub fn replace_ids(node: &Ast, idvals: &HashMap<Lstr, &Ast>
        , loc: &SrcLoc
        ) -> Ast
    {
        match node {
            &Ast::Block(ref items) => {
                let m_items = items.iter().map(|i| {
                    Protomod::replace_ids(i, idvals, loc)
                }).collect();
                Ast::Block(m_items)
            }
            &Ast::Cons(ref head, ref tail) => {
                let rhead = Protomod::replace_ids(head, idvals, loc);
                let rtail = Protomod::replace_ids(tail, idvals, loc);
                Ast::Cons(Box::new(rhead), Box::new(rtail))
            }
            &Ast::Tuple(ref t) => {
                let result = t.iter().map(|tv| {
                    Protomod::replace_ids(tv, idvals, loc)
                }).collect();
                Ast::Tuple(result)
            }
            &Ast::IfExpr(ift, ref input, ref if_case, _) => {
                let m_input = Protomod::replace_ids(input, idvals, loc);
                let m_case = Protomod::replace_ifcase_ids(if_case, idvals, loc);
                Ast::IfExpr(ift, Box::new(m_input), Box::new(m_case), *loc)
            }
            &Ast::Localid(ref name, ref iloc) => {
                match idvals.get(&*name) {
                    Some(newx) => (*newx).clone(),
                    None => node.clone(),
                }
            }
            &Ast::Lri(ref names, ref types, _) => {
                node.clone()
            }
            _ => {
                node.clone()
            }
        }
    }

    pub fn replace_ifcase_ids(case: &ast::IfCase, idvals: &HashMap<Lstr, &Ast>
        , loc: &SrcLoc
        ) -> ast::IfCase
    {
        let m_cond = Protomod::replace_ids(&case.cond, idvals, loc);
        let m_body = Protomod::replace_ids(&case.body, idvals, loc);
        let m_else = case.else_case.as_ref().map(|else_case| {
            Protomod::replace_ifcase_ids(&else_case, idvals, loc)
        });
        ast::IfCase::new(m_cond, m_body, m_else, *loc)
    }

    pub fn preproc_lri(prog: &Lib, mp: &ModulePreface, mods: &Vec<Lstr>
        , loc: &SrcLoc
        ) -> Ast
    {
        let mod_name = mods.first().unwrap();
        if mod_name.str() != *mp.key.name
                && !mp.imports.contains(mod_name.str())
        {
            panic!("module not found: {:?}", mods);
        }
        let val_name = mods.last().unwrap();
        match prog.get_macro(mod_name, val_name) {
            Some(mac) => {
                mac.clone()
            }
            None => {
                Ast::Lri(mods.clone(), None, *loc)
            }
        }
    }

    pub fn preproc_lri_with_types(prog: &Lib, mp: &ModulePreface
        , mods: &Vec<Lstr>, typs: &LinkedList<Ast>, loc: &SrcLoc
        ) -> Ast
    {
        let pp_types = typs.iter().map(|t| {
            Protomod::preproc_expr(prog, mp, t, loc)
        }).collect();
        Ast::Lri(mods.clone(), Some(pp_types), *loc)
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
        let pp_else = case.else_case.as_ref().map(|else_case| {
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
            &Ast::List(ref items) => {
                Ast::List(items.iter().map(|i| {
                    Protomod::preproc_pattern(prog, mp, i, loc)
                }).collect())
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

    pub fn preproc_type(prog: &Lib, mp: &ModulePreface
        , opt_type_params: Option<&Vec<Type>>, typ: &Ast, loc: &SrcLoc
        ) -> Type
    {
        let pp_x = Protomod::preproc_expr(prog, mp, typ, loc);
        match (&pp_x, opt_type_params) {
            (&Ast::Localid(ref name, ref iloc), None) => {
                Type::UserDef(Lri::new(name.clone()))
            }
            (&Ast::Localid(ref name, ref iloc), Some(ref type_params)) => {
                Protomod::find_type_param(type_params, name)
                .map(|i| {
                    // Type::Param(i)
                    Type::Var(name.clone())
                })
                .unwrap_or_else(|| {
                    Type::UserDef(Lri::new(name.clone()))
                })
            }
            (&Ast::TypeVar(ref name, ref iloc), _) => {
                Type::UserDef(Lri::new(name.clone()))
            }
            _ => {
                Type::from(&pp_x)
            }
        }
    }

    pub fn find_type_param(params: &Vec<Type>, name: &str) -> Option<i8>
    {
        for (i, p) in params.iter().enumerate() {
            let p_rc = p.local_typename();
            if &**p_rc == name {
                return Some(i as i8)
            }
        }
        None
    }

    pub fn preproc_data(&mut self, prog: &Lib, mp: &ModulePreface
        , datatype: ast::DataType, name_ast: &Ast
        , fields: &LinkedList<Kxpr>, loc: &SrcLoc
        )
    {
        let name = Lri::from(name_ast);
        if name.mod_ref().is_some() {
            panic!("no modules in data definitions: {}"
                , name);
        }

        match datatype {
            ast::DataType::Struple => {
                if fields.is_empty() {
                    self.preproc_struple_token(prog, mp, name, loc);
                } else {
                    self.preproc_struple_with_fields(
                        prog, mp, name, fields, loc);
                }
            }
            ast::DataType::Enum => {
                self.preproc_enum(prog, mp, name_ast, fields, loc);
            }
        }
    }

    pub fn preproc_struple_token(&mut self, prog: &Lib, mp: &ModulePreface
        , name: Lri, loc: &SrcLoc
        )
    {
        if name.param_ref().is_some() {
            panic!("no type params for tokens: {}", name);
        }
        let name_lstr = name.local_ref();
        let mod_lstr = Lstr::Rc(mp.key.name.clone());
        let full_lri = name.add_modules(mod_lstr);
        let type_name = Type::UserDef(full_lri.clone());

        // a token struct is stored as a constant with no constructor
        let constval = Val::Token(full_lri);
        self.constants.insert(String::from(name_lstr.str()), constval);
        self.valtypes.insert(String::from(name_lstr.str()), type_name);
    }

    pub fn preproc_struple_with_fields(&mut self, prog: &Lib
        , mp: &ModulePreface, name: Lri
        , src_fields: &LinkedList<Kxpr>, loc: &SrcLoc
        )
    {
        let name_lstr = name.local_ref().clone();
        let mod_lstr = Lstr::Rc(self.key.name.clone());

        let type_lri = Lri::with_modules(mod_lstr.clone(), name_lstr.clone());

        self.preproc_struple_fields(prog, mp, name
            , mod_lstr, name_lstr, src_fields, loc);
    }

    pub fn preproc_struple_fields(&mut self, prog: &Lib, mp: &ModulePreface
        , local_type: Lri, mod_name: Lstr, local_name: Lstr
        , src_fields: &LinkedList<Kxpr>, loc: &SrcLoc
        )
    {
        let struple_fields: Vec<(Option<Lstr>, Type)> =
            src_fields.iter()
            .map(|f| {
                // struple fields are made w/ an x_list,
                // x_ref will always be there
                let pp_type = Protomod::preproc_type(prog, mp
                    , local_type.param_ref(), f.x_ref().unwrap(), loc);
                (f.k_clone(), pp_type)
            }).collect();
        let field_type_vec = struple_fields.iter().map(|&(_, ref ftype)| {
            ftype.clone()
        }).collect();

        let struple_lri = local_type.add_modules(mod_name.clone());
        let full_type = Type::UserDef(struple_lri.clone());
        let func_type = Type::Func(field_type_vec, Box::new(full_type.clone()));

        let src_typename = Ast::from_lri(struple_lri.clone(), loc);
        let localid_ast = Ast::Localid(local_name.clone(), *loc);
        let srcblk = Ast::ConstructData(ast::DataType::Struple
            , Box::new(localid_ast), Vec::with_capacity(0)
            );
        let srcxpr = Ast::DefFunc(ast::FuncClass::Func
            , Box::new(Ast::Localid(local_name.clone(), *loc))
            , (*src_fields).clone(), Box::new(src_typename)
            , Box::new(srcblk), *loc);

        let struct_type_val = types::new_type_val(struple_lri.clone(), &struple_fields);
        let new_types_list = list::cons(struct_type_val
            , self.constants.remove("TYPES").expect("missing TYPES constant").clone());
        self.constants.insert(String::from("TYPES"), new_types_list);

        let funcref =
            Val::FuncRef(mod_name.rc(), local_name.rc(), func_type.clone());
        self.constants.insert(String::from(&local_name), funcref);

        self.funcseq.push_back(local_name.rc());
        self.funcsrc.insert(String::from(&local_name), srcxpr);
        self.valtypes.insert(String::from(&local_name), func_type);
        self.deftypes.insert(struple_lri.local_ref().clone(), full_type);
    }

    pub fn struple_field_idx(&self, typename: &str, fld: &str
        ) -> Option<(i16, &Type)>
    {
        vout!("field index for struple: {:?}.{}\n", typename, fld);
        let opt_typ = self.constants.get(typename);
        if opt_typ.is_none() {
            panic!("cannot find struple type: {} in {:?}"
                , typename, self.constants);
        }
        match opt_typ.unwrap() {
            &Val::Struct(_, ref items) => {
                panic!("need to get the struct field still");
            }
            what => {
                panic!("cannot get fields from not struple: {:?}", what);
            }
        }
    }

    pub fn func_result_type(&self, func_name: &Lstr) -> Type
    {
        let func_type = self.valtypes.get(func_name.str())
            .expect("cannot find type for value");
        match func_type {
            Type::Func(_, ref result_type) => {
                (**result_type).clone()
            }
            _ => {
                panic!("cannot get result type from not func: {:?}", func_type);
            }
        }
    }

    pub fn preproc_enum(&mut self, prog: &Lib, mp: &ModulePreface
        , name_ast: &Ast, src_variants: &LinkedList<Kxpr>
        , loc: &SrcLoc)
    {
        let local_name = Lri::from(name_ast);
        let enum_lri = local_name.add_modules(Lstr::Rc(self.key.name.clone()));
        let name_lstr = Lstr::from(name_ast);
        let rc_name: Rc<String> = From::from(&name_lstr);
        let mod_type = Type::UserDef(enum_lri.clone());

        let type_params: HashSet<Lstr> = HashSet::new();
        let mut variant_fields = Vec::with_capacity(src_variants.len());
        for (bigi, kx) in src_variants.iter().enumerate() {
            let i = bigi as i16;
            let v = kx.x_ref().unwrap();
            if let &Ast::DefData(vdatatype, ref vname, ref fields, ref loc) = v {
                self.preproc_enum_variant(prog, mp, &name_ast, i
                    , vdatatype, vname, &type_params, fields, loc);
                let variant_lstr = Lstr::from(&**vname);
                let variant_name: Rc<String> = From::from(&variant_lstr);
                let vf = (variant_name, mod_type.clone());
                variant_fields.push(vf);
            } else {
                panic!("variant data is not DefData: {:?}", v);
            }
        }

        self.constants.insert((*rc_name).clone(), Val::Type(mod_type));
    }

    pub fn preproc_enum_variant(&mut self, prog: &Lib, mp: &ModulePreface
        , typename: &Ast, i: i16, dataclass: ast::DataType
        , name: &Ast, type_params: &HashSet<Lstr>
        , fields: &LinkedList<Kxpr>, loc: &SrcLoc
        )
    {
        let mod_name = self.key.name.clone();
        let mod_lstr = Lstr::Rc(mod_name.clone());
        let typ_lri = Lri::from(typename);
        let full_lri = typ_lri.add_modules(mod_lstr.clone());
        let typ = Type::UserDef(full_lri.clone());
        let type_lstr = Lstr::from(typename);
        let variant_name = Lstr::from(name);
        vout!("preproc_enum_variant({}::{}::{})\n"
            , mod_lstr, type_lstr, variant_name);
        if dataclass == ast::DataType::Struple {
            if fields.is_empty() {
                let variant_lri =
                    Lri::with_modules(mod_lstr.clone(), variant_name.clone());
                let var_struct_type = Type::UserDef(variant_lri);
                let const_val =
                    Val::EnumToken(full_lri, variant_name.clone());
                let variant_name_string = String::from(&variant_name);
                self.constants.insert(variant_name_string.clone(), const_val);
                self.valtypes.insert(variant_name_string, typ);
            } else {
                self.preproc_struple_fields(prog, mp, typ_lri
                    , mod_lstr, variant_name, fields, loc);
            }
        } else {
            panic!("unknown enum variant type: {:?}", dataclass);
        }
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
    use leema::ast::{self, Ast};
    use leema::list;
    use leema::lri::{Lri};
    use leema::lstr::{Lstr};
    use leema::log;
    use leema::loader::{Interloader};
    use leema::module::{ModKey};
    use leema::phase0::{Protomod};
    use leema::program;
    use leema::types;
    use leema::val::{Val, Type, SrcLoc};

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
    if let &Ast::DefFunc(foo_ft, ref foo_name, _, _, _, _) = foo_func {
        assert_eq!(ast::FuncClass::Func, foo_ft);
        assert_eq!("foo", Lstr::from(&**foo_name).str());
    } else {
        panic!("foo is not a function definition");
    }

    let main_func = pmod.funcsrc.get("main").unwrap();
    if let &Ast::DefFunc(main_ft, ref main_name, _, _, _, _) = main_func {
        assert_eq!(ast::FuncClass::Func, main_ft);
        assert_eq!("main", Lstr::from(&**main_name).str());
    } else {
        panic!("main is not a function definition");
    }
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
    let type_lri = Lri::with_modules(
        Lstr::Rc(modname.clone()),
        Lstr::Rc(local_typename.clone()),
    );
    let expected_type = Type::UserDef(type_lri.clone());

    let expected_red =
        Val::EnumToken(type_lri.clone(), Lstr::Sref("Red"));
    let red = pmod.constants.get("Red").unwrap();
    assert_eq!(expected_red, *red);
    assert!(pmod.constants.get("Yellow").is_some());
    assert!(pmod.constants.get("Blue").is_some());
    assert!(pmod.constants.get("PrimaryColor").is_some());
    assert_eq!(5, pmod.constants.len());
}

#[test]
fn test_enum_types()
{
    let input = "
    enum Animal[A]
    |Dog
    |Cat(Int)
    |Mouse(A)
    |Giraffe
        .height: Int
        .weight: A
    --
    ".to_string();
    let mut loader = Interloader::new("animals.lma");
    loader.set_mod_txt("animals", input);
    let mut prog = program::Lib::new(loader);
    let pmod = prog.read_proto("animals");

    let type_lri = Lri::full(
        Some(Lstr::Sref("animals")),
        Lstr::Sref("Animal"),
        Some(vec![Type::UserDef(Lri::new(Lstr::Sref("A")))]),
    );
    let expected_type = Type::UserDef(type_lri.clone());
    let typevar_a = Type::Var(Lstr::Sref("A"));
    let typeparam_a = Type::Param(0);
    let dog_name = Lstr::Sref("Dog");
    let cat_name = Lstr::Sref("Cat");
    let mouse_name = Rc::new("Mouse".to_string());
    let giraffe_name = Rc::new("Giraffe".to_string());
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
    assert_eq!(6, pmod.constants.len());
    let dog_const = pmod.constants.get("Dog").expect("missing constant: Dog");
    let cat_const = pmod.constants.get("Cat").expect("missing constant: Cat");
    let giraffe_const =
        pmod.constants.get("Giraffe").expect("missing constant: Giraffe");

    let exp_dog_const = Val::EnumToken(type_lri.clone(), dog_name);
    let exp_cat_const = Val::FuncRef(
        Rc::new("animals".to_string()),
        cat_name.rc(),
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
    assert_eq!("animals::Animal[A,]",
        format!("{}", *pmod.valtypes.get("Dog").unwrap()));
    assert_eq!("Int => animals::Animal[A,]",
        format!("{}", *pmod.valtypes.get("Cat").unwrap()));
    assert_eq!(mouse_func_type, *pmod.valtypes.get("Mouse").unwrap());
    assert_eq!(giraffe_func_type, *pmod.valtypes.get("Giraffe").unwrap());
    assert_eq!(4, pmod.valtypes.len());
}

#[test]
fn test_preproc_namedtuple()
{
    let input = "
    struple Greeting(Str, Str)
    ".to_string();
    let mut loader = Interloader::new("greet.lma");
    loader.set_mod_txt("greet", input);
    let mut prog = program::Lib::new(loader);
    let pmod = prog.read_proto("greet");

    let greet = Rc::new("greet".to_string());
    let greeting_str = Rc::new("Greeting".to_string());
    let greeting_lstr = Lstr::Rc(greeting_str.clone());
    let greeting_local = Lri::new(greeting_lstr.clone());
    let greeting_fullri = Lri::with_modules(
        Lstr::Rc(greet.clone()), greeting_lstr);
    let greeting_typref = Type::UserDef(greeting_fullri);
    let xfunctyp = Type::Func(
        vec![Type::Str, Type::Str],
        Box::new(greeting_typref.clone()),
    );

    // constants
    assert_eq!(
        Val::FuncRef(greet.clone(), greeting_str.clone(), xfunctyp.clone()),
        *pmod.constants.get("Greeting").unwrap()
    );
    assert_eq!(2, pmod.constants.len());

    // assert funcsrc
    assert_eq!(1, pmod.funcsrc.len());

    // assert funcseq
    assert_eq!(1, pmod.funcseq.len());
    assert_eq!("Greeting", **pmod.funcseq.front().unwrap());

    // verify valtypes
    assert_eq!(1, pmod.valtypes.len());
}

#[test]
fn preproc_defstruple_mixed_keys()
{
    let input = "
    struple Burrito(Bool, buns: Int)
    ".to_string();
    let mut loader = Interloader::new("tacos.lma");
    loader.set_mod_txt("tacos", input);
    let mut prog = program::Lib::new(loader);
    let pmod = prog.read_proto("tacos");

    // assert valtypes
    assert!(pmod.valtypes.contains_key("Burrito"));
    let constructor = pmod.valtypes.get("Burrito").unwrap();
    if let &Type::Func(ref params, ref result) = constructor {
        assert_eq!(2, params.len());
        let exp_result = Type::UserDef(Lri::with_modules(
            Lstr::from("tacos"), Lstr::from("Burrito")
        ));
        assert_eq!(exp_result, **result);
    } else {
        panic!("constructor valtype is not a func");
    }

    let xtyperef = Type::UserDef(Lri::with_modules(
        Lstr::from("tacos"),
        Lstr::from("Burrito"),
    ));
    let xfunctype = Type::Func(
        vec![Type::Bool, Type::Int],
        Box::new(xtyperef.clone()),
    );

    // assert constants
    let funcref = pmod.constants.get("Burrito").unwrap();
    if let &Val::FuncRef(ref mod_nm, ref func_nm, ref ftype) = funcref {
        assert_eq!("tacos", &**mod_nm);
        assert_eq!("Burrito", &**func_nm);
        assert_eq!(xfunctype, *ftype);
    } else {
        panic!("Burrito constant is not a FuncRef: {:?}", funcref);
    }

    // assert funcseq contents
    assert_eq!("Burrito", **pmod.funcseq.front().unwrap());
    assert_eq!(1, pmod.funcseq.len());

    // assert valtypes
    assert_eq!(xfunctype, *pmod.valtypes.get("Burrito").unwrap());
    assert_eq!(1, pmod.valtypes.len());

    // assert funcsrc
    assert!(pmod.funcsrc.contains_key("Burrito"));
    assert_eq!(1, pmod.funcsrc.len());
}

#[test]
fn preproc_defstruple_keyed()
{
    let input = "
    struple Burrito
    .filling: Str
    .number: Int
    --
    ".to_string();
    let mut loader = Interloader::new("tacos.lma");
    loader.set_mod_txt("tacos", input);
    let mut prog = program::Lib::new(loader);
    let pmod = prog.read_proto("tacos");

    // assert valtypes
    assert!(pmod.valtypes.contains_key("Burrito"));
    let constructor = pmod.valtypes.get("Burrito").unwrap();
    if let &Type::Func(ref params, ref result) = constructor {
        assert_eq!(2, params.len());
        let exp_result = Type::UserDef(Lri::with_modules(
            Lstr::from("tacos"), Lstr::from("Burrito")
        ));
        assert_eq!(exp_result, **result);
    } else {
        panic!("constructor valtype is not a func");
    }

    let xtyperef = Type::UserDef(Lri::with_modules(
        Lstr::from("tacos"),
        Lstr::from("Burrito"),
    ));
    let xfunctype = Type::Func(
        vec![Type::Str, Type::Int],
        Box::new(xtyperef.clone()),
    );

    // assert constants
    let funcref = pmod.constants.get("Burrito").unwrap();
    if let &Val::FuncRef(ref mod_nm, ref func_nm, ref ftype) = funcref {
        assert_eq!("tacos", &**mod_nm);
        assert_eq!("Burrito", &**func_nm);
        assert_eq!(xfunctype, *ftype);
    } else {
        panic!("Burrito constant is not a FuncRef: {:?}", funcref);
    }
    let type_vals = pmod.constants.get("TYPES").unwrap();
    assert_eq!(1, list::len(type_vals));
    let burrito_typeval = list::head_ref(type_vals);
    if let &Val::Struct(ref stype, ref sfields) = burrito_typeval {
        assert_eq!("types::TypeVal", format!("{}", stype));
    } else {
        panic!("Burrito constant is not a struct: {:?}", burrito_typeval);
    }
    assert_eq!(2, pmod.constants.len());

    // assert burrito field types
    let burrito_filling_type =
        types::get_field_type(burrito_typeval, &Lstr::Sref("filling"))
        .expect("cannot find Burrito filling field");
    let burrito_number_type =
        types::get_field_type(burrito_typeval, &Lstr::Sref("number"))
        .expect("cannot find Burrito number field");
    assert_eq!(Type::Str, *burrito_filling_type.1);
    assert_eq!(Type::Int, *burrito_number_type.1);
    assert_eq!(0, burrito_filling_type.0);
    assert_eq!(1, burrito_number_type.0);

    // assert funcseq contents
    assert_eq!("Burrito", **pmod.funcseq.front().unwrap());
    assert_eq!(1, pmod.funcseq.len());

    // assert valtypes
    assert_eq!(xfunctype, *pmod.valtypes.get("Burrito").unwrap());
    assert_eq!(1, pmod.valtypes.len());

    // assert funcsrc
    assert!(pmod.funcsrc.contains_key("Burrito"));
    assert_eq!(1, pmod.funcsrc.len());
}

#[test]
fn preproc_defstruple_token()
{
    let input = String::from("
    struple Burrito --
    ");

    let mut loader = Interloader::new("tok.lma");
    loader.set_mod_txt("tok", input);
    let mut prog = program::Lib::new(loader);
    let pmod = prog.read_proto("tok");

    let name_rc = Rc::new("Burrito".to_string());
    let exptype_lri = Lri::with_modules(
        Lstr::from("tok"),
        Lstr::from("Burrito"),
    );
    let exptype = Type::UserDef(exptype_lri.clone());

    // verify valtypes
    assert_eq!(exptype, *pmod.valtypes.get("Burrito").unwrap());
    assert_eq!(1, pmod.valtypes.len());

    // verify constants
    assert_eq!(Val::Token(exptype_lri.clone())
        , *pmod.constants.get("Burrito").unwrap());
    pmod.constants.get("TYPES").expect("tok constants not found");
    assert_eq!(2, pmod.constants.len());

    // assert on fields that shouldn't have changed
    assert_eq!(0, pmod.funcseq.len());
    assert_eq!(0, pmod.funcsrc.len());
}

#[test]
#[should_panic]
fn test_old_token_type()
{
    // empty structs are no longer supported
    let input = String::from("
    struct Burrito --
    ");

    let mut loader = Interloader::new("tok.lma");
    loader.set_mod_txt("tok", input);
    let mut prog = program::Lib::new(loader);
    prog.read_proto("tok");
}

}
