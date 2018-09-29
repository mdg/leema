use leema::ast::{self, Ast, Kxpr};
use leema::list;
use leema::log;
use leema::lri::Lri;
use leema::lstr::Lstr;
use leema::module::{ModKey, ModulePreface};
use leema::program::Lib;
use leema::struple::Struple;
use leema::types;
use leema::val::{SrcLoc, Type, Val};

use std::collections::{HashMap, LinkedList};
use std::fmt;


#[derive(Debug)]
pub struct Protomod
{
    pub key: ModKey,
    pub closures: LinkedList<Lstr>,
    pub funcseq: LinkedList<Lstr>,
    pub funcsrc: HashMap<Lstr, Ast>,
    pub valtypes: HashMap<Lstr, Type>,
    pub constants: HashMap<Lstr, Val>,
    pub deftypes: HashMap<Lstr, Type>,
    pub struple_fields: HashMap<Lstr, Struple<Type>>,
}

impl Protomod
{
    pub fn new(mk: ModKey) -> Protomod
    {
        let mut empty_consts = HashMap::new();
        empty_consts.insert(Lstr::Sref("TYPES"), Val::Nil);
        Protomod {
            key: mk,
            closures: LinkedList::new(),
            funcseq: LinkedList::new(),
            funcsrc: HashMap::new(),
            valtypes: HashMap::new(),
            constants: empty_consts,
            deftypes: HashMap::new(),
            struple_fields: HashMap::new(),
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

    pub fn preproc_module_expr(
        &mut self,
        prog: &Lib,
        mp: &ModulePreface,
        x: &Ast,
    )
    {
        match x {
            &Ast::DefFunc(ast::FuncClass::Macro, _, _, _, _, _) => {
                // do nothing. the macro definition will have been handled
                // in the file read
            }
            &Ast::DefFunc(
                fclass,
                ref name,
                ref args,
                ref result_type,
                ref body,
                ref loc,
            ) => {
                self.preproc_defunc(
                    prog,
                    mp,
                    fclass,
                    name,
                    args,
                    result_type,
                    body,
                    loc,
                );
            }
            &Ast::DefData(data_type, ref name, ref fields, ref loc) => {
                self.preproc_data(prog, mp, data_type, name, fields, loc);
            }
            &Ast::Import(ref _imports, _) => {
                // do nothing. imports handled in file read
            }
            _ => {
                println!("Cannot phase0: {:?}", x);
            }
        }
    }

    pub fn preproc_expr(
        &mut self,
        prog: &Lib,
        mp: &ModulePreface,
        x: &Ast,
        loc: &SrcLoc,
    ) -> Ast
    {
        match x {
            &Ast::Block(ref items) => {
                let pp_items = items
                    .iter()
                    .map(|i| Protomod::preproc_expr(self, prog, mp, i, loc))
                    .collect();
                Ast::Block(pp_items)
            }
            &Ast::Call(ref callx, ref args, ref iloc) => {
                Protomod::preproc_call(self, prog, mp, callx, args, iloc)
            }
            &Ast::DefFunc(
                ast::FuncClass::Closure,
                _,
                ref args,
                _,
                ref body,
                ref iloc,
            ) => Protomod::preproc_closure(self, prog, mp, args, body, iloc),
            &Ast::Cons(ref head, ref tail) => {
                let pp_head = Protomod::preproc_expr(self, prog, mp, head, loc);
                let pp_tail = Protomod::preproc_expr(self, prog, mp, tail, loc);
                Ast::Cons(Box::new(pp_head), Box::new(pp_tail))
            }
            &Ast::ConstructData(datat, ref name) => {
                let ppname = Protomod::preproc_expr(self, prog, mp, name, loc);
                Ast::ConstructData(datat, Box::new(ppname))
            }
            &Ast::ConstBool(b) => Ast::ConstBool(b),
            &Ast::ConstHashtag(_) => x.clone(),
            &Ast::ConstInt(i) => Ast::ConstInt(i),
            &Ast::ConstStr(_) => x.clone(),
            &Ast::ConstVoid => Ast::ConstVoid,
            &Ast::Deref(ref inner) => {
                Ast::Deref(Box::new(Protomod::preproc_expr(
                    self, prog, mp, inner, loc,
                )))
            }
            &Ast::DotAccess(ref base, ref fld) => {
                let ppbase = Protomod::preproc_expr(self, prog, mp, base, loc);
                Ast::DotAccess(Box::new(ppbase), fld.clone())
            }
            &Ast::Fork(ref fx) => {
                let ppfx = Protomod::preproc_expr(self, prog, mp, fx, loc);
                Ast::Fork(Box::new(ppfx))
            }
            &Ast::IfExpr(
                ast::IfType::MatchFailure,
                ref input,
                ref case,
                ref iloc,
            ) => {
                if let Ast::Localid(_, _) = **input {
                    // this localid shouldn't need further processing
                } else {
                    panic!("match failed input must be a variable name");
                }
                let pp_case = Protomod::preproc_ifcase(
                    self,
                    prog,
                    mp,
                    ast::IfType::MatchFailure,
                    case,
                    iloc,
                );
                Ast::IfExpr(
                    ast::IfType::MatchFailure,
                    input.clone(),
                    Box::new(pp_case),
                    *iloc,
                )
            }
            &Ast::IfExpr(iftype, ref input, ref case, ref iloc) => {
                let pp_input =
                    Protomod::preproc_expr(self, prog, mp, input, iloc);
                let pp_case = Protomod::preproc_ifcase(
                    self, prog, mp, iftype, case, iloc,
                );
                Ast::IfExpr(
                    iftype,
                    Box::new(pp_input),
                    Box::new(pp_case),
                    *iloc,
                )
            }
            &Ast::Let(ref left, ref right, ref iloc) => {
                let pp_left = Protomod::preproc_pattern(prog, mp, left, iloc);
                let pp_right =
                    Protomod::preproc_expr(self, prog, mp, right, iloc);
                Ast::Let(Box::new(pp_left), Box::new(pp_right), *iloc)
            }
            &Ast::List(ref items) => {
                Ast::List(
                    items
                        .iter()
                        .map(|i| Protomod::preproc_expr(self, prog, mp, i, loc))
                        .collect(),
                )
            }
            &Ast::Localid(ref id, ref iloc) => {
                Protomod::preproc_localid(prog, mp, id, iloc)
            }
            &Ast::Lri(ref mods, None, ref iloc) => {
                Protomod::preproc_lri(prog, mp, mods, iloc)
            }
            &Ast::Lri(ref mods, Some(ref typs), ref iloc) => {
                Protomod::preproc_lri_with_types(
                    self, prog, mp, mods, typs, iloc,
                )
            }
            &Ast::Map(ref items) => {
                let pp_items = items
                    .iter()
                    .map(|i| {
                        i.map_x(|x| {
                            Protomod::preproc_expr(self, prog, mp, x, loc)
                        })
                    })
                    .collect();
                Ast::Map(pp_items)
            }
            &Ast::Return(ref x, ref loc) => {
                let px = Protomod::preproc_expr(self, prog, mp, x, loc);
                Ast::Return(Box::new(px), *loc)
            }
            &Ast::StrExpr(ref xs, ref loc) => {
                let pxs = xs
                    .iter()
                    .map(|x| Protomod::preproc_expr(self, prog, mp, x, loc))
                    .collect();
                Ast::StrExpr(pxs, *loc)
            }
            &Ast::Tuple(ref items) if items.len() == 1 => {
                // one-tuples are compiled to just the value
                let first_x = items.front().unwrap().x_ref().unwrap();
                Protomod::preproc_expr(self, prog, mp, first_x, loc)
            }
            &Ast::Tuple(ref items) => {
                let pp_items = items
                    .iter()
                    .map(|i| {
                        i.map_x(|x| {
                            Protomod::preproc_expr(self, prog, mp, x, loc)
                        })
                    })
                    .collect();
                Ast::Tuple(pp_items)
            }
            &Ast::TypeFunc(ref parts, ref loc) => {
                let ppp = parts
                    .iter()
                    .map(|p| {
                        Protomod::preproc_func_arg(
                            self,
                            prog,
                            mp,
                            &Lstr::Sref("anon_func_type"),
                            p,
                            loc,
                        )
                    })
                    .collect();
                Ast::TypeFunc(ppp, *loc)
            }
            &Ast::Question => Ast::Question,
            &Ast::RustBlock => Ast::RustBlock,
            &Ast::TypeAnon => Ast::TypeAnon,
            &Ast::TypeBool => Ast::TypeBool,
            &Ast::TypeFailure => Ast::TypeFailure,
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

    pub fn preproc_func_arg(
        &mut self,
        prog: &Lib,
        mp: &ModulePreface,
        func_name: &Lstr,
        arg: &Kxpr,
        loc: &SrcLoc,
    ) -> Kxpr
    {
        match (arg.k_ref(), arg.x_ref()) {
            (None, None) => {
                panic!("cannot preproc arg with no id or type: {:?}", loc);
            }
            (None, Some(&Ast::TypeAnon)) => {
                panic!(
                    "cannot preproc arg with no id and anonymous type: {:?}",
                    loc
                );
            }
            (Some(id), None) => {
                let type_name = Lstr::from(format!(
                    "T_{}_{}_{}",
                    mp.key.name,
                    func_name.str(),
                    id
                ));
                let typ = Ast::TypeVar(type_name, *loc);
                Kxpr::new(id.clone(), typ)
            }
            (Some(id), Some(&Ast::TypeAnon)) => {
                let type_name = Lstr::from(format!(
                    "T_{}_{}_{}",
                    mp.key.name,
                    func_name.str(),
                    id
                ));
                let new_typ = Ast::TypeVar(type_name, *loc);
                Kxpr::new(id.clone(), new_typ)
            }
            (_, Some(_)) => {
                arg.map_x(|typ| {
                    Protomod::preproc_expr(self, prog, mp, typ, &loc)
                })
            }
        }
    }

    pub fn preproc_func_result(
        &mut self,
        prog: &Lib,
        mp: &ModulePreface,
        result_type: &Ast,
        loc: &SrcLoc,
    ) -> Ast
    {
        match result_type {
            Ast::TypeAnon => Ast::TypeVar(Lstr::Sref("T_result"), *loc),
            _ => Protomod::preproc_expr(self, prog, mp, result_type, &loc),
        }
    }

    pub fn preproc_defunc(
        &mut self,
        prog: &Lib,
        mp: &ModulePreface,
        fclass: ast::FuncClass,
        name: &Ast,
        args: &LinkedList<Kxpr>,
        rtype: &Ast,
        body: &Ast,
        loc: &SrcLoc,
    )
    {
        let name_lri = Lri::from(name);
        let full_lri = name_lri.add_modules(mp.key.name.clone());
        let lstr_name = Lstr::from(name);
        let pp_args: LinkedList<Kxpr> = args
            .iter()
            .map(|a| {
                Protomod::preproc_func_arg(self, prog, mp, &lstr_name, a, loc)
            })
            .collect();
        let pp_rtype_ast =
            Protomod::preproc_func_result(self, prog, mp, rtype, loc);
        let pp_body = Protomod::preproc_expr(self, prog, mp, body, loc);
        let pp_func = Ast::DefFunc(
            fclass,
            Box::new(name.clone()),
            pp_args.clone(),
            Box::new(pp_rtype_ast.clone()),
            Box::new(pp_body),
            *loc,
        );
        let lstr_name = Lstr::from(name);

        let mut ftype_parts: Vec<Kxpr> =
            pp_args.iter().map(|a| a.clone()).collect();
        let (ftype_part_types, rtype): (Vec<Type>, Type) = {
            let lri_params = full_lri.params.as_ref();
            let pp_ftype_parts = ftype_parts
                .iter()
                .map(|argt| {
                    self.preproc_type(
                        prog,
                        mp,
                        lri_params,
                        argt.x_ref().unwrap(),
                        loc,
                    )
                })
                .collect();
            let pp_rtype =
                self.preproc_type(prog, mp, lri_params, &pp_rtype_ast, loc);
            (pp_ftype_parts, pp_rtype)
        };
        ftype_parts.push(Kxpr::new_x(pp_rtype_ast));
        let ftype = Type::Func(ftype_part_types, Box::new(rtype));
        let fref_args =
            pp_args.iter().map(|a| (a.k_clone(), Val::Void)).collect();

        let funcref = Val::FuncRef(full_lri, fref_args, ftype.clone());

        self.funcseq.push_back(lstr_name.clone());
        self.funcsrc.insert(lstr_name.clone(), pp_func);
        self.valtypes.insert(lstr_name.clone(), ftype);
        self.constants.insert(lstr_name, funcref);
    }

    pub fn preproc_closure(
        &mut self,
        prog: &Lib,
        mp: &ModulePreface,
        args: &LinkedList<Kxpr>,
        body: &Ast,
        loc: &SrcLoc,
    ) -> Ast
    {
        let closure_key =
            Protomod::make_closure_key(&mp.key.name, loc.lineno, &args);
        let pp_args: LinkedList<Kxpr> = args
            .iter()
            .map(|a| self.preproc_func_arg(prog, mp, &closure_key, a, loc))
            .collect();
        let pp_body = self.preproc_expr(prog, mp, body, loc);
        let pp_result = self.preproc_func_result(prog, mp, &Ast::TypeAnon, loc);

        let arg_types: Vec<Type> = pp_args
            .iter()
            .map(|a| {
                let x_ref = a.x_ref();
                if x_ref.is_none() {
                    panic!("closure arg type is None: {}", closure_key);
                }
                let atype = x_ref.unwrap();
                self.preproc_type(prog, mp, None, atype, loc)
            })
            .collect();
        let result_type = self.preproc_type(prog, mp, None, &pp_result, loc);
        let ftype = Type::Func(arg_types, Box::new(result_type));

        let fref_args =
            pp_args.iter().map(|a| (a.k_clone(), Val::Void)).collect();

        let pp_func = Ast::DefFunc(
            ast::FuncClass::Closure,
            Box::new(Ast::Lri(vec![closure_key.clone()], None, *loc)),
            pp_args,
            Box::new(pp_result.clone()),
            Box::new(pp_body),
            *loc,
        );

        let closuri =
            Lri::with_modules(mp.key.name.clone(), closure_key.clone());
        let fref_struple = Struple(fref_args);
        let funcref = Val::FuncRef(closuri, fref_struple, ftype.clone());

        self.closures.push_back(closure_key.clone());
        self.funcseq.push_back(closure_key.clone());
        self.funcsrc.insert(closure_key.clone(), pp_func);
        self.valtypes.insert(closure_key.clone(), ftype);
        self.constants.insert(closure_key.clone(), funcref);
        Ast::Localid(closure_key, *loc)
    }

    /// generate a key for referencing the closure when actually
    /// trying to call it
    pub fn make_closure_key(
        modname: &str,
        lineno: i16,
        args: &LinkedList<Kxpr>,
    ) -> Lstr
    {
        let mut key = String::with_capacity(128);
        key.push_str(modname);
        key.push('_');
        key.push_str(&format!("{}", lineno));
        for a in args.iter() {
            key.push('_');
            let arg_name = a.k_ref().unwrap_or_else(|| {
                panic!("closure missing arg name at {}:{}", modname, lineno);
            });
            key.push_str(arg_name);
        }
        Lstr::from(key)
    }

    pub fn preproc_call(
        &mut self,
        prog: &Lib,
        mp: &ModulePreface,
        callx: &Ast,
        args: &LinkedList<Kxpr>,
        loc: &SrcLoc,
    ) -> Ast
    {
        let pp_args: LinkedList<Kxpr> = args
            .iter()
            .map(|arg| {
                arg.map_x(|x| Protomod::preproc_expr(self, prog, mp, x, loc))
            })
            .collect();
        let pp_callx = Protomod::preproc_expr(self, prog, mp, callx, loc);

        match pp_callx {
            Ast::DefFunc(ast::FuncClass::Macro, mname, margs, _, body, _) => {
                vout!("apply_macro({:?}, {:?})\n", mname, args);
                let macrod =
                    Protomod::apply_macro(&mname, &body, &margs, &pp_args, loc);
                // do it again to make sure there's not a wrapped macro
                return self.preproc_expr(prog, mp, &macrod, loc);
            }
            _ => {
                // fall through
            }
        }
        let is_curried = args.iter().any(|a| a.x_ref() == Some(&Ast::Question));
        if is_curried {
            self.preproc_curry(prog, mp, &pp_callx, &pp_args, loc)
        } else {
            Ast::Call(Box::new(pp_callx), pp_args, *loc)
        }
    }

    pub fn preproc_curry(
        &mut self,
        prog: &Lib,
        mp: &ModulePreface,
        callx: &Ast,
        args: &LinkedList<Kxpr>,
        loc: &SrcLoc,
    ) -> Ast
    {
        // set arguments passed to curry in a new block
        // to be later included in the closure
        let mut new_block = Vec::new();
        let mut outer_args: LinkedList<Kxpr> = LinkedList::new();
        let mut inner_args = LinkedList::new();
        for (i, a) in args.iter().enumerate() {
            if Some(&Ast::Question) == a.x_ref() {
                let argn = Lstr::from(format!(
                    "curryarg_{}_{}__{}",
                    mp.key.name, loc.lineno, i
                ));
                outer_args.push_back(Kxpr::new_k(argn.clone()));
                inner_args.push_back(Kxpr::new_x(Ast::Localid(argn, *loc)));
            } else {
                let argn = Lstr::from(format!(
                    "curryclosed_{}_{}__{}",
                    mp.key.name, loc.lineno, i
                ));
                let argn_ast = Ast::Localid(argn, *loc);
                let new_x = Box::new(a.x_clone().unwrap());
                new_block.push(Ast::Let(
                    Box::new(argn_ast.clone()),
                    new_x,
                    *loc,
                ));
                inner_args.push_back(Kxpr::new_x(argn_ast));
            }
        }
        // make closure
        let clbody = Ast::Call(Box::new(callx.clone()), inner_args, *loc);
        let clos = Ast::closure(outer_args, clbody, *loc);
        // add closure as result of block
        new_block.push(clos);
        self.preproc_expr(prog, mp, &Ast::Block(new_block), loc)
    }

    pub fn apply_macro(
        macro_name: &Ast,
        body: &Ast,
        arg_names: &LinkedList<Kxpr>,
        args: &LinkedList<Kxpr>,
        loc: &SrcLoc,
    ) -> Ast
    {
        let mut arg_map = HashMap::new();
        match (arg_names.len(), args.len()) {
            (a, b) if a < b => {
                panic!(
                    "Too many arguments passed to macro {:?}, expected {}",
                    macro_name, a
                );
            }
            (a, b) if a > b => {
                panic!(
                    "Too few arguments passed to macro {:?}, expected {}",
                    macro_name, a
                );
            }
            _ => {
                // a == b. cool, proceed
            }
        }
        for (n, arg_val) in arg_names.iter().zip(args.iter()) {
            let n_opt = n.k_ref();
            if n_opt.is_none() {
                panic!(
                    "macro has unnamed args: {:?} -> {:?}",
                    macro_name, arg_names
                );
            }
            let n_lstr = n_opt.unwrap().clone();
            arg_map.insert(n_lstr, arg_val.x_ref().unwrap());
        }
        vout!("replace_ids({:?})\n", arg_map);
        Protomod::replace_ids(body, &arg_map, loc)
    }

    pub fn replace_ids(
        node: &Ast,
        idvals: &HashMap<Lstr, &Ast>,
        loc: &SrcLoc,
    ) -> Ast
    {
        match node {
            &Ast::Block(ref items) => {
                let m_items = items
                    .iter()
                    .map(|i| Protomod::replace_ids(i, idvals, loc))
                    .collect();
                Ast::Block(m_items)
            }
            &Ast::Cons(ref head, ref tail) => {
                let rhead = Protomod::replace_ids(head, idvals, loc);
                let rtail = Protomod::replace_ids(tail, idvals, loc);
                Ast::Cons(Box::new(rhead), Box::new(rtail))
            }
            &Ast::Tuple(ref t) => {
                let result = t
                    .iter()
                    .map(|tv| {
                        tv.map_x(|x| Protomod::replace_ids(x, idvals, loc))
                    })
                    .collect();
                Ast::Tuple(result)
            }
            &Ast::IfExpr(ift, ref input, ref if_case, _) => {
                let m_input = Protomod::replace_ids(input, idvals, loc);
                let m_case = Protomod::replace_ifcase_ids(if_case, idvals, loc);
                Ast::IfExpr(ift, Box::new(m_input), Box::new(m_case), *loc)
            }
            &Ast::Call(ref callx, ref args, _) => {
                let new_callx = Protomod::replace_ids(callx, idvals, loc);
                let new_args = args
                    .iter()
                    .map(|arg| {
                        arg.map_x(|x| Protomod::replace_ids(x, idvals, loc))
                    })
                    .collect();
                Ast::Call(Box::new(new_callx), new_args, *loc)
            }
            &Ast::Localid(ref name, ref _iloc) => {
                match idvals.get(&*name) {
                    Some(newx) => (*newx).clone(),
                    None => Ast::Localid(name.clone(), *loc),
                }
            }
            &Ast::Return(ref result, _) => {
                let new_result = Protomod::replace_ids(result, idvals, loc);
                Ast::Return(Box::new(new_result), *loc)
            }
            &Ast::Lri(_, _, _) => node.clone(),
            &Ast::ConstBool(b) => Ast::ConstBool(b),
            &Ast::ConstVoid => Ast::ConstVoid,
            &Ast::Wildcard => Ast::Wildcard,
            _ => {
                println!("cannot replace_ids for expression: {:?}", node);
                node.clone()
            }
        }
    }

    pub fn replace_ifcase_ids(
        case: &ast::IfCase,
        idvals: &HashMap<Lstr, &Ast>,
        loc: &SrcLoc,
    ) -> ast::IfCase
    {
        let m_cond = Protomod::replace_ids(&case.cond, idvals, loc);
        let m_body = Protomod::replace_ids(&case.body, idvals, loc);
        let m_else = case.else_case.as_ref().map(|else_case| {
            Protomod::replace_ifcase_ids(&else_case, idvals, loc)
        });
        ast::IfCase::new(m_cond, m_body, m_else, *loc)
    }

    pub fn preproc_localid(
        prog: &Lib,
        mp: &ModulePreface,
        id: &Lstr,
        loc: &SrcLoc,
    ) -> Ast
    {
        let mac = prog
            .get_macro(&mp.key.name, id)
            .or_else(|| prog.get_macro(&Lstr::Sref("prefab"), id));
        match mac {
            Some(imac) => imac.clone(),
            None => Ast::Localid(id.clone(), *loc),
        }
    }

    pub fn preproc_lri(
        prog: &Lib,
        mp: &ModulePreface,
        mods: &Vec<Lstr>,
        loc: &SrcLoc,
    ) -> Ast
    {
        let mod_name = mods.first().unwrap();
        if *mod_name != *mp.key.name && !mp.imports.contains(mod_name) {
            panic!("module not found: {:?}", mods);
        }
        let val_name = mods.last().unwrap();
        match prog.get_macro(mod_name, val_name) {
            Some(mac) => mac.clone(),
            None => Ast::Lri(mods.clone(), None, *loc),
        }
    }

    pub fn preproc_lri_with_types(
        &mut self,
        prog: &Lib,
        mp: &ModulePreface,
        mods: &Vec<Lstr>,
        typs: &LinkedList<Kxpr>,
        loc: &SrcLoc,
    ) -> Ast
    {
        let pp_types = typs
            .iter()
            .map(|t| {
                t.map_x(|tx| Protomod::preproc_expr(self, prog, mp, tx, loc))
            })
            .collect();
        Ast::Lri(mods.clone(), Some(pp_types), *loc)
    }

    pub fn preproc_ifcase(
        &mut self,
        prog: &Lib,
        mp: &ModulePreface,
        iftype: ast::IfType,
        case: &ast::IfCase,
        loc: &SrcLoc,
    ) -> ast::IfCase
    {
        let pp_cond = match iftype {
            ast::IfType::If => {
                Protomod::preproc_expr(self, prog, mp, &case.cond, &case.loc)
            }
            ast::IfType::Match => {
                Protomod::preproc_pattern(prog, mp, &case.cond, &case.loc)
            }
            ast::IfType::MatchFailure => {
                match &case.cond {
                    &Ast::ConstHashtag(ref ht) => Ast::ConstHashtag(ht.clone()),
                    &Ast::Wildcard => Ast::Wildcard,
                    _ => {
                        panic!("match failure case pattern must be a hashtag or an underscore: {:?}", case.cond);
                    }
                }
            }
            ast::IfType::TypeCast => {
                panic!("typecast not ready yet");
            }
        };
        let pp_body =
            Protomod::preproc_expr(self, prog, mp, &case.body, &case.loc);
        let pp_else = case.else_case.as_ref().map(|else_case| {
            Protomod::preproc_ifcase(
                self,
                prog,
                mp,
                iftype,
                &*else_case,
                &case.loc,
            )
        });
        ast::IfCase::new(pp_cond, pp_body, pp_else, *loc)
    }

    pub fn preproc_pattern(
        prog: &Lib,
        mp: &ModulePreface,
        p: &Ast,
        loc: &SrcLoc,
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
                Protomod::preproc_pattern(prog, mp, first.x_ref().unwrap(), loc)
            }
            &Ast::Tuple(ref items) => {
                Ast::Tuple(
                    items
                        .iter()
                        .map(|i| {
                            i.map_x(|x| {
                                Protomod::preproc_pattern(prog, mp, x, loc)
                            })
                        })
                        .collect(),
                )
            }
            &Ast::List(ref items) => {
                Ast::List(
                    items
                        .iter()
                        .map(|i| Protomod::preproc_pattern(prog, mp, i, loc))
                        .collect(),
                )
            }
            &Ast::Call(ref name, ref args, ref iloc) => {
                let pp_callx = Protomod::preproc_pattern(prog, mp, name, iloc);
                let pp_args = args
                    .iter()
                    .map(|px| {
                        px.map_x(|x| {
                            Protomod::preproc_pattern(prog, mp, x, iloc)
                        })
                    })
                    .collect();
                Ast::Call(Box::new(pp_callx), pp_args, *loc)
            }
            &Ast::Localid(_, _) => p.clone(),
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

    pub fn preproc_type(
        &mut self,
        prog: &Lib,
        mp: &ModulePreface,
        opt_type_params: Option<&Vec<Type>>,
        typ: &Ast,
        loc: &SrcLoc,
    ) -> Type
    {
        let pp_x = Protomod::preproc_expr(self, prog, mp, typ, loc);
        let local_type = Type::from(&pp_x);
        self.replace_typeids(opt_type_params, local_type)
    }

    pub fn replace_typeids(
        &self,
        type_params: Option<&Vec<Type>>,
        t: Type,
    ) -> Type
    {
        // rewrite the modules if necessary
        match t {
            Type::UserDef(id) => {
                if id.local_only() && Protomod::find_type_param(
                    type_params,
                    &id.localid,
                )
                .is_some()
                {
                    Type::Var(id.localid.clone())
                } else if !id.has_modules()
                    && self.deftypes.contains_key(&id.localid)
                {
                    Type::UserDef(id.add_modules(self.key.name.clone()))
                } else {
                    Type::UserDef(id)
                }
            }
            Type::Func(mut args, result) => {
                let args2 = args
                    .drain(..)
                    .map(|a| self.replace_typeids(type_params, a))
                    .collect();
                let result2 = self.replace_typeids(type_params, *result);
                Type::Func(args2, Box::new(result2))
            }
            Type::StrictList(subtype) => {
                let sub2 = self.replace_typeids(type_params, *subtype);
                Type::StrictList(Box::new(sub2))
            }
            Type::Tuple(mut items) => {
                let items2 = items
                    .0
                    .drain(..)
                    .map(|mut i| {
                        i.1 = self.replace_typeids(type_params, i.1);
                        i
                    })
                    .collect();
                Type::Tuple(Struple(items2))
            }
            Type::Map => Type::Map,
            // primitive types
            Type::Bool => Type::Bool,
            Type::Hashtag => Type::Hashtag,
            Type::Failure => Type::Failure,
            Type::Int => Type::Int,
            Type::Str => Type::Str,
            id => id,
        }
    }

    pub fn find_type_param(params: Option<&Vec<Type>>, name: &str)
        -> Option<i8>
    {
        if params.is_none() {
            return None;
        }
        for (i, p) in params.unwrap().iter().enumerate() {
            if let &Type::Var(ref pname) = p {
                if pname == name {
                    return Some(i as i8);
                }
            }
        }
        None
    }

    pub fn preproc_data(
        &mut self,
        prog: &Lib,
        mp: &ModulePreface,
        datatype: ast::DataType,
        name_ast: &Ast,
        fields: &LinkedList<Kxpr>,
        loc: &SrcLoc,
    )
    {
        let base_name = Lri::from(name_ast);
        if base_name.mod_ref().is_some() {
            panic!("no modules in data definitions: {}", base_name);
        }
        let name = Protomod::replace_type_names_with_vars(
            base_name.add_modules(mp.key.name.clone()),
        );

        match datatype {
            ast::DataType::Struple => {
                if fields.is_empty() {
                    self.preproc_struple_token(name, loc);
                } else {
                    self.preproc_struple_with_fields(
                        prog, mp, name, fields, loc,
                    );
                }
            }
            ast::DataType::Enum => {
                self.preproc_enum(prog, mp, name_ast, fields);
            }
        }
    }

    pub fn replace_type_names_with_vars(mut i: Lri) -> Lri
    {
        if !i.has_params() {
            return i;
        }
        let var_params = i
            .params
            .take()
            .unwrap()
            .into_iter()
            .map(|p| {
                match p {
                    Type::Var(_) => p,
                    Type::UserDef(lri) => {
                        if lri.local_only() {
                            Type::Var(lri.localid)
                        } else {
                            Type::UserDef(lri)
                        }
                    }
                    _ => p,
                }
            })
            .collect();
        i.params = Some(var_params);
        i
    }

    pub fn preproc_struple_token(&mut self, full_lri: Lri, _loc: &SrcLoc)
    {
        if full_lri.param_ref().is_some() {
            panic!("no type params for tokens: {}", full_lri);
        }
        let name_lstr = full_lri.localid.clone();
        let type_name = Type::UserDef(full_lri.clone());

        // a token struct is stored as a constant with no constructor
        let constval = Val::Token(full_lri);
        self.constants.insert(name_lstr.clone(), constval);
        self.deftypes.insert(name_lstr.clone(), type_name.clone());
        self.valtypes.insert(name_lstr, type_name);
    }

    pub fn preproc_struple_with_fields(
        &mut self,
        prog: &Lib,
        mp: &ModulePreface,
        type_lri: Lri,
        src_fields: &LinkedList<Kxpr>,
        loc: &SrcLoc,
    )
    {
        let name_lstr = type_lri.local_ref().clone();

        self.preproc_struple_fields(
            prog,
            mp,
            type_lri.clone(),
            name_lstr.clone(),
            src_fields,
            loc,
        );
        self.deftypes
            .insert(name_lstr.clone(), Type::UserDef(type_lri));
    }

    pub fn preproc_struple_fields(
        &mut self,
        prog: &Lib,
        mp: &ModulePreface,
        struple_lri: Lri,
        local_name: Lstr,
        src_fields: &LinkedList<Kxpr>,
        loc: &SrcLoc,
    )
    {
        let struple_fields: Vec<(Option<Lstr>, Type)> = src_fields
            .iter()
            .map(|f| {
                // struple fields are made w/ an x_list,
                // x_ref will always be there
                let pp_type = self.preproc_type(
                    prog,
                    mp,
                    struple_lri.param_ref(),
                    f.x_ref().unwrap(),
                    loc,
                );
                (f.k_clone(), pp_type)
            })
            .collect();
        let field_type_vec = struple_fields
            .iter()
            .map(|&(_, ref ftype)| ftype.clone())
            .collect();
        let fref_args = Struple(
            struple_fields
                .iter()
                .map(|f| (f.0.clone(), Val::Void))
                .collect(),
        );

        let full_type = Type::UserDef(struple_lri.clone());
        let func_type = Type::Func(field_type_vec, Box::new(full_type.clone()));

        let full_type_ast = Ast::from_lri(struple_lri.clone(), loc);
        let srcblk = Ast::ConstructData(
            ast::DataType::Struple,
            Box::new(full_type_ast.clone()),
        );

        let srcxpr = Ast::DefFunc(
            ast::FuncClass::Func,
            Box::new(Ast::Localid(local_name.clone(), *loc)),
            (*src_fields).clone(),
            Box::new(full_type_ast),
            Box::new(srcblk),
            *loc,
        );

        let struct_type_val =
            types::new_type_val(struple_lri.clone(), &struple_fields);
        let new_types_list = list::cons(
            struct_type_val,
            self.constants
                .remove("TYPES")
                .expect("missing TYPES constant")
                .clone(),
        );
        self.constants.insert(Lstr::Sref("TYPES"), new_types_list);
        self.struple_fields
            .insert(local_name.clone(), Struple(struple_fields));

        let funcref = Val::FuncRef(struple_lri, fref_args, func_type.clone());
        self.constants.insert(local_name.clone(), funcref);

        self.funcseq.push_back(local_name.clone());
        self.funcsrc.insert(local_name.clone(), srcxpr);
        self.valtypes.insert(local_name, func_type);
    }

    pub fn struple_field_idx(
        &self,
        typename: &str,
        fld: &str,
    ) -> Option<(i16, &Type)>
    {
        vout!("field index for struple: {:?}.{}\n", typename, fld);
        let types_list = self
            .constants
            .get("TYPES")
            .expect("cannot find TYPES constant");
        for typ in list::iter(types_list) {
            let opt_iter_name =
                types::get_named_struct_field(typ, &Lstr::Sref("name"));
            if opt_iter_name.is_none() {
                continue;
            }
            let iter_name = opt_iter_name.unwrap();
            if typename != iter_name.1.str() {
                continue;
            }
            return types::get_field_type(typ, fld);
        }
        None
    }

    pub fn get_struple_fields(&self, typename: &Lstr) -> &Struple<Type>
    {
        self.struple_fields.get(typename).unwrap()
    }

    pub fn func_result_type(&self, func_name: &Lstr) -> Option<Type>
    {
        self.valtypes.get(func_name.str()).and_then(|func_type| {
            if let Type::Func(_, ref result_type) = func_type {
                Some((**result_type).clone())
            } else {
                None
            }
        })
    }

    pub fn preproc_enum(
        &mut self,
        prog: &Lib,
        mp: &ModulePreface,
        name_ast: &Ast,
        src_variants: &LinkedList<Kxpr>,
    )
    {
        let local_name = Lri::from(name_ast);
        let enum_lri = local_name.add_modules(self.key.name.clone());
        let mod_type = Type::UserDef(enum_lri.clone());

        let mut variant_fields = Vec::with_capacity(src_variants.len());
        for kx in src_variants.iter() {
            let v = kx.x_ref().unwrap();
            if let &Ast::DefData(vdatatype, ref vname, ref fields, ref iloc) = v
            {
                self.preproc_enum_variant(
                    prog, mp, &name_ast, vdatatype, vname, fields, iloc,
                );
                let variant_lstr = Lstr::from(&**vname);
                let vf = (variant_lstr, mod_type.clone());
                variant_fields.push(vf);
            } else {
                panic!("variant data is not DefData: {:?}", v);
            }
        }

        // self.constants.insert(name_lstr.clone(), Val::Type(mod_type.clone()));
        self.deftypes
            .insert(enum_lri.local_ref().clone(), mod_type.clone());
    }

    pub fn preproc_enum_variant(
        &mut self,
        prog: &Lib,
        mp: &ModulePreface,
        typename: &Ast,
        dataclass: ast::DataType,
        name: &Ast,
        fields: &LinkedList<Kxpr>,
        loc: &SrcLoc,
    )
    {
        let typ_lri = Lri::from(typename);
        let full_lri = typ_lri.add_modules(self.key.name.clone());
        let typ = Type::UserDef(full_lri.clone());
        let type_lstr = Lstr::from(typename);
        let variant_name = Lstr::from(name);
        vout!(
            "preproc_enum_variant({}::{}::{})\n",
            self.key.name,
            type_lstr,
            variant_name
        );
        if dataclass == ast::DataType::Struple {
            if fields.is_empty() {
                let const_val = Val::EnumToken(full_lri, variant_name.clone());
                self.constants.insert(variant_name.clone(), const_val);
                self.valtypes.insert(variant_name, typ);
            } else {
                self.preproc_struple_fields(
                    prog,
                    mp,
                    typ_lri,
                    variant_name,
                    fields,
                    loc,
                );
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

impl fmt::Display for Protomod
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "Protomod: {}\n.funcseq:", self.key)?;
        for i in self.funcseq.iter() {
            write!(f, " {}", i)?;
        }
        // funcsrc
        write!(f, "\n.funcsrc:")?;
        for i in self.funcsrc.keys() {
            write!(f, " {}", i)?;
        }
        // valtypes
        writeln!(f, "\n.valtypes:")?;
        for (k, v) in self.valtypes.iter() {
            writeln!(f, "   {}: {}", k, v)?;
        }
        // constants
        writeln!(f, ".constants:")?;
        for (k, v) in self.constants.iter() {
            writeln!(f, "   {}: {}", k, v)?;
        }
        // deftypes
        writeln!(f, ".deftypes:")?;
        for (k, v) in self.deftypes.iter() {
            writeln!(f, "   {}: {}", k, v)?;
        }
        writeln!(f, ".struple_fields: {:?}", self.struple_fields)
    }
}


#[cfg(test)]
mod tests
{
    use leema::ast::{self, Ast};
    use leema::list;
    use leema::loader::Interloader;
    use leema::lri::Lri;
    use leema::lstr::Lstr;
    use leema::program;
    use leema::struple::Struple;
    use leema::types;
    use leema::val::{Type, Val};


    #[test]
    fn test_preproc_list_pattern()
    {
        let input = String::from(
            "
            func foo(a)
            |(h;t) -> print(\"head: $h, tail: $t\n\")
            --

            func main() -> foo([3, 4, 5]) --
            ",
        );

        let mut loader = Interloader::new(Lstr::Sref("tacos.lma"));
        loader.set_mod_txt(Lstr::Sref("tacos"), input);
        let mut prog = program::Lib::new(loader);
        let pmod = prog.read_proto(&Lstr::Sref("tacos"));

        assert_eq!(2, pmod.funcsrc.len());

        let foo_func = pmod.funcsrc.get("foo").unwrap();
        if let &Ast::DefFunc(foo_ft, ref foo_name, _, _, _, _) = foo_func {
            assert_eq!(ast::FuncClass::Func, foo_ft);
            assert_eq!("foo", Lstr::from(&**foo_name).str());
        } else {
            panic!("foo is not a function definition");
        }
        pmod.constants.get("foo").unwrap();

        let main_func = pmod.funcsrc.get("main").unwrap();
        if let &Ast::DefFunc(main_ft, ref main_name, _, _, _, _) = main_func {
            assert_eq!(ast::FuncClass::Func, main_ft);
            assert_eq!("main", Lstr::from(&**main_name).str());
        } else {
            panic!("main is not a function definition");
        }
        pmod.constants.get("main").unwrap();
    }

    #[test]
    fn test_preproc_prepend_module_name()
    {
        let input = "struct Foo --

            func open_foo(): Foo -RUST-
            func close_foo(f: Foo): Void -RUST-
            "
        .to_string();

        let foo_str = Lstr::Sref("foo");
        let mut loader = Interloader::new(Lstr::Sref("foo.lma"));
        loader.set_mod_txt(foo_str.clone(), input);
        let mut prog = program::Lib::new(loader);
        let pmod = prog.read_proto(&foo_str);

        let open_valtype = pmod.valtypes.get("open_foo").unwrap();
        if let Type::Func(open_args, open_result_type) = open_valtype {
            assert!(open_args.is_empty());
            assert_eq!("foo::Foo", &open_result_type.full_typename());
        } else {
            panic!("open_valtype is not a func type: {:?}", open_valtype);
        }

        let _close_valtype = pmod.valtypes.get("close_foo").unwrap();
    }

    #[test]
    fn test_preproc_closures()
    {
        let input = "
            func main() ->
                let items := [1, 2]
                let items2 := map(items, fn(i) i * 2)
            --
            "
        .to_string();

        let foo_str = Lstr::Sref("foo");
        let mut loader = Interloader::new(Lstr::Sref("foo.lma"));
        loader.set_mod_txt(foo_str.clone(), input);
        let mut prog = program::Lib::new(loader);
        let pmod = prog.read_proto(&foo_str);

        let closure0 = pmod.closures.front().unwrap();
        assert_eq!("foo_4_i", closure0);
        assert_eq!(1, pmod.closures.len());
    }

    #[test]
    fn test_preproc_enum_colors()
    {
        let input = String::from(
            "
            enum PrimaryColor
            |Red
            |Yellow
            |Blue
            --
            ",
        );

        let colors_str = Lstr::Sref("colors");
        let mut loader = Interloader::new(Lstr::Sref("colors.lma"));
        loader.set_mod_txt(colors_str.clone(), input);
        let mut prog = program::Lib::new(loader);
        let pmod = prog.read_proto(&colors_str);

        assert_eq!(0, pmod.funcsrc.len());

        let local_typename = Lstr::Sref("PrimaryColor");
        let type_lri =
            Lri::with_modules(colors_str.clone(), local_typename.clone());

        let expected_red = Val::EnumToken(type_lri.clone(), Lstr::Sref("Red"));
        let red = pmod.constants.get("Red").unwrap();
        assert_eq!(expected_red, *red);
        assert!(pmod.constants.get("Yellow").is_some());
        assert!(pmod.constants.get("Blue").is_some());
        assert_eq!(4, pmod.constants.len());
    }

    #[test]
    #[ignore] // unignore this once enums are working again
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
        "
        .to_string();
        let animals_str = Lstr::Sref("animals");
        let mut loader = Interloader::new(Lstr::Sref("animals.lma"));
        loader.set_mod_txt(animals_str.clone(), input);
        let mut prog = program::Lib::new(loader);
        let pmod = prog.read_proto(&animals_str);

        let type_lri = Lri::full(
            Some(animals_str.clone()),
            Lstr::Sref("Animal"),
            Some(vec![Type::UserDef(Lri::new(Lstr::Sref("A")))]),
        );
        let expected_type = Type::UserDef(type_lri.clone());
        let typevar_a = Type::Var(Lstr::Sref("A"));
        let dog_name = Lstr::Sref("Dog");
        let cat_func_type =
            Type::Func(vec![Type::Int], Box::new(expected_type.clone()));
        let mouse_func_type = Type::Func(
            vec![typevar_a.clone()],
            Box::new(expected_type.clone()),
        );
        let giraffe_func_type = Type::Func(
            vec![Type::Int, typevar_a.clone()],
            Box::new(expected_type.clone()),
        );

        // verify constants
        assert_eq!(6, pmod.constants.len());
        let dog_const =
            pmod.constants.get("Dog").expect("missing constant: Dog");
        let cat_const =
            pmod.constants.get("Cat").expect("missing constant: Cat");
        let giraffe_const = pmod
            .constants
            .get("Giraffe")
            .expect("missing constant: Giraffe");

        let exp_dog_const = Val::EnumToken(type_lri.clone(), dog_name);
        let exp_cat_const = Val::FuncRef(
            Lri::with_modules(animals_str.clone(), Lstr::Sref("Cat")),
            Struple(vec![(None, Val::Void)]),
            cat_func_type.clone(),
        );
        let exp_giraffe_const = Val::FuncRef(
            Lri::with_modules(animals_str.clone(), Lstr::Sref("Giraffe")),
            Struple(vec![
                (Some(Lstr::Sref("height")), Val::Void),
                (Some(Lstr::Sref("weight")), Val::Void),
            ]),
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
        assert_eq!("Cat", fseq_it.next().unwrap().str());
        assert_eq!("Mouse", fseq_it.next().unwrap().str());
        assert_eq!("Giraffe", fseq_it.next().unwrap().str());

        // verify function source
        assert!(pmod.funcsrc.get("Dog").is_none());
        assert!(pmod.funcsrc.get("Cat").is_some());
        assert!(pmod.funcsrc.get("Mouse").is_some());
        assert!(pmod.funcsrc.get("Giraffe").is_some());
        assert_eq!(3, pmod.funcsrc.len());

        // verify value types
        assert_eq!(
            "animals::Animal[A,]",
            format!("{}", *pmod.valtypes.get("Dog").unwrap())
        );
        assert_eq!(
            "Int => animals::Animal[A,]",
            format!("{}", *pmod.valtypes.get("Cat").unwrap())
        );
        assert_eq!(mouse_func_type, *pmod.valtypes.get("Mouse").unwrap());
        assert_eq!(giraffe_func_type, *pmod.valtypes.get("Giraffe").unwrap());
        assert_eq!(4, pmod.valtypes.len());
    }

    #[test]
    fn test_preproc_namedtuple()
    {
        let input = "
    struct Greeting(Str, Str)
    "
        .to_string();
        let greet = Lstr::Sref("greet");
        let mut loader = Interloader::new(Lstr::Sref("greet.lma"));
        loader.set_mod_txt(greet.clone(), input);
        let mut prog = program::Lib::new(loader);
        let pmod = prog.read_proto(&greet);

        let greeting_lstr = Lstr::Sref("Greeting");
        let greeting_fullri = Lri::with_modules(greet.clone(), greeting_lstr);
        let greeting_typref = Type::UserDef(greeting_fullri);
        let xfunctyp = Type::Func(
            vec![Type::Str, Type::Str],
            Box::new(greeting_typref.clone()),
        );

        // constants
        match pmod.constants.get("Greeting").unwrap() {
            Val::FuncRef(ref actual_fri, ref actual_args, ref actual_types) => {
                assert_eq!("greet", actual_fri.safe_mod().str());
                assert_eq!("Greeting", actual_fri.localid.str());
                assert_eq!(2, actual_args.0.len());
                assert!(actual_fri.params.is_none());
                assert_eq!(xfunctyp, *actual_types);
            }
            _ => {
                panic!("greeting not a function");
            }
        }
        assert_eq!(2, pmod.constants.len());

        // assert funcsrc
        assert_eq!(1, pmod.funcsrc.len());

        // assert funcseq
        assert_eq!(1, pmod.funcseq.len());
        assert_eq!("Greeting", pmod.funcseq.front().unwrap().str());

        // verify valtypes
        assert_eq!(1, pmod.valtypes.len());
    }

    #[test]
    fn preproc_defstruple_mixed_keys()
    {
        let input = "
            struct Burrito(Bool, buns: Int)
            "
        .to_string();

        let mut loader = Interloader::new(Lstr::Sref("tacos.lma"));
        loader.set_mod_txt(Lstr::Sref("tacos"), input);
        let mut prog = program::Lib::new(loader);
        let pmod = prog.read_proto(&Lstr::Sref("tacos"));

        // assert valtypes
        assert!(pmod.valtypes.contains_key("Burrito"));
        let constructor = pmod.valtypes.get("Burrito").unwrap();
        if let &Type::Func(ref params, ref result) = constructor {
            assert_eq!(2, params.len());
            let exp_result = Type::UserDef(Lri::with_modules(
                Lstr::from("tacos"),
                Lstr::from("Burrito"),
            ));
            assert_eq!(exp_result, **result);
        } else {
            panic!("constructor valtype is not a func");
        }

        let xtyperef = Type::UserDef(Lri::with_modules(
            Lstr::from("tacos"),
            Lstr::from("Burrito"),
        ));
        let xfunctype =
            Type::Func(vec![Type::Bool, Type::Int], Box::new(xtyperef.clone()));

        // assert constants
        let funcref = pmod.constants.get("Burrito").unwrap();
        if let &Val::FuncRef(ref fri, ref args, ref ftype) = funcref {
            assert_eq!("tacos", fri.modules.as_ref().unwrap().str());
            assert_eq!("Burrito", fri.localid.str());
            // args content
            assert_eq!(None, args.0[0].0);
            assert_eq!(Val::Void, args.0[0].1);
            assert_eq!(Some(Lstr::Sref("buns")), args.0[1].0);
            assert_eq!(Val::Void, args.0[1].1);
            assert_eq!(2, args.0.len());
            // function type
            assert_eq!(xfunctype, *ftype);
        } else {
            panic!("Burrito constant is not a FuncRef: {:?}", funcref);
        }

        // assert funcseq contents
        assert_eq!("Burrito", &*pmod.funcseq.front().unwrap());
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
            struct Burrito
            .filling: Str
            .number: Int
            --
            "
        .to_string();
        let mut loader = Interloader::new(Lstr::Sref("tacos.lma"));
        loader.set_mod_txt(Lstr::Sref("tacos"), input);
        let mut prog = program::Lib::new(loader);
        let pmod = prog.read_proto(&Lstr::Sref("tacos"));

        // assert valtypes
        assert!(pmod.valtypes.contains_key("Burrito"));
        let constructor = pmod.valtypes.get("Burrito").unwrap();
        if let &Type::Func(ref params, ref result) = constructor {
            assert_eq!(2, params.len());
            let exp_result = Type::UserDef(Lri::with_modules(
                Lstr::from("tacos"),
                Lstr::from("Burrito"),
            ));
            assert_eq!(exp_result, **result);
        } else {
            panic!("constructor valtype is not a func");
        }

        let xtyperef = Type::UserDef(Lri::with_modules(
            Lstr::from("tacos"),
            Lstr::from("Burrito"),
        ));
        let xfunctype =
            Type::Func(vec![Type::Str, Type::Int], Box::new(xtyperef.clone()));

        // assert constants
        let funcref = pmod.constants.get("Burrito").unwrap();
        if let &Val::FuncRef(ref funcri, ref args, ref ftype) = funcref {
            // func lri
            assert_eq!("tacos", funcri.mod_ref().unwrap().str());
            assert_eq!("Burrito", funcri.localid.str());
            // func args
            assert_eq!("filling", args.0[0].0.as_ref().unwrap());
            assert_eq!("number", args.0[1].0.as_ref().unwrap());
            assert_eq!(Val::Void, args.0[0].1);
            assert_eq!(Val::Void, args.0[1].1);
            assert_eq!(2, args.0.len());
            // func type
            assert_eq!(xfunctype, *ftype);
        } else {
            panic!("Burrito constant is not a FuncRef: {:?}", funcref);
        }
        let type_vals = pmod.constants.get("TYPES").unwrap();
        assert_eq!(1, list::len(type_vals));
        let burrito_typeval = list::head_ref(type_vals);
        if let &Val::Struct(ref stype, ref sfields) = burrito_typeval {
            assert_eq!("types::TypeVal", format!("{}", stype));
            assert_eq!(2, sfields.0.len());
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
        assert_eq!("Burrito", *&pmod.funcseq.front().unwrap());
        assert_eq!(1, pmod.funcseq.len());

        // assert valtypes
        assert_eq!(xfunctype, *pmod.valtypes.get("Burrito").unwrap());
        assert_eq!(1, pmod.valtypes.len());

        // assert funcsrc
        assert!(pmod.funcsrc.contains_key("Burrito"));
        assert_eq!(1, pmod.funcsrc.len());

        // field indexes
        let burrito_filling_idx = pmod.struple_field_idx("Burrito", "filling");
        let burrito_number_idx = pmod.struple_field_idx("Burrito", "number");
        burrito_filling_idx.expect("burrito filling idx");
        burrito_number_idx.expect("burrito number idx");
    }

    #[test]
    fn preproc_defstruple_token()
    {
        let input = "
            struct Burrito --
            "
        .to_string();

        let mut loader = Interloader::new(Lstr::Sref("tok.lma"));
        loader.set_mod_txt(Lstr::Sref("tok"), input);
        let mut prog = program::Lib::new(loader);
        let pmod = prog.read_proto(&Lstr::Sref("tok"));

        let exptype_lri =
            Lri::with_modules(Lstr::from("tok"), Lstr::from("Burrito"));
        let exptype = Type::UserDef(exptype_lri.clone());

        // verify valtypes
        assert_eq!(exptype, *pmod.valtypes.get("Burrito").unwrap());
        assert_eq!(1, pmod.valtypes.len());

        // verify constants
        assert_eq!(
            Val::Token(exptype_lri.clone()),
            *pmod.constants.get("Burrito").unwrap()
        );
        pmod.constants
            .get("TYPES")
            .expect("tok constants not found");
        assert_eq!(2, pmod.constants.len());

        // assert on fields that shouldn't have changed
        assert_eq!(0, pmod.funcseq.len());
        assert_eq!(0, pmod.funcsrc.len());
    }
}
