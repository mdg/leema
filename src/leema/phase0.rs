use leema::ast::{self, Ast, Kxpr, KxprList};
use leema::failure::{Failure, Lresult};
use leema::list;
use leema::lri::{Lri, ModLocalId};
use leema::lstr::Lstr;
use leema::module::{ModKey, ModulePreface};
use leema::program::Lib;
use leema::struple::{Struple, Struple2, StrupleItem, StrupleKV};
use leema::types;
use leema::val::{FuncType, SrcLoc, Type, Val};

use std::collections::{HashMap, LinkedList};
use std::fmt;
use std::iter::FromIterator;


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

    pub fn constant(&self, valnm: &str) -> Option<&Val>
    {
        self.constants.get(valnm)
    }

    pub fn preproc_module_expr(
        &mut self,
        prog: &Lib,
        mp: &ModulePreface,
        x: &Ast,
    )
    {
        match x {
            &Ast::DefFunc(ast::FuncClass::Func, ref decl, ref body) => {
                match decl.name {
                    Ast::Localid(ref name, _) => {
                        self.preproc_defunc(
                            prog,
                            mp,
                            &name,
                            &decl.args,
                            &decl.result,
                            body,
                            &decl.loc,
                        );
                    }
                    Ast::LocalGeneric(ref name, ref typ_params, _) => {
                        self.preproc_generic_func(
                            prog,
                            mp,
                            &name,
                            typ_params,
                            &decl.args,
                            &decl.result,
                            body,
                            &decl.loc,
                        )
                        .unwrap();
                    }
                    _ => {
                        panic!("cannot preproc function: {}", decl);
                    }
                }
            }
            &Ast::DefData(data_type, ref name, ref fields, ref loc) => {
                self.preproc_data(prog, mp, data_type, name, fields, loc);
            }
            &Ast::Import(ref _imports, _) => {
                // do nothing. imports handled in file read
            }
            &Ast::DefFunc(ast::FuncClass::Macro, _, _) => {
                // do nothing. the macro definition will have been handled
                // in the file read
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
            &Ast::DefFunc(ast::FuncClass::Closure, ref decl, ref body) => {
                Protomod::preproc_closure(
                    self, prog, mp, &decl.args, body, &decl.loc,
                )
            }
            &Ast::Cons(ref head, ref tail) => {
                let pp_head = Protomod::preproc_expr(self, prog, mp, head, loc);
                let pp_tail = Protomod::preproc_expr(self, prog, mp, tail, loc);
                Ast::Cons(Box::new(pp_head), Box::new(pp_tail))
            }
            &Ast::ConstructData(ref name, ref variant) => {
                Ast::ConstructData(name.clone(), variant.clone())
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
            &Ast::Let(ref left, ref ltype, ref right, ref iloc) => {
                let pp_left = Protomod::preproc_pattern(prog, mp, left, iloc);
                let pp_ltype = self.preproc_expr(prog, mp, ltype, iloc);
                let pp_right = self.preproc_expr(prog, mp, right, iloc);
                Ast::Let(
                    Box::new(pp_left),
                    Box::new(pp_ltype),
                    Box::new(pp_right),
                    *iloc,
                )
            }
            &Ast::List(ref items) => {
                Ast::List(
                    items
                        .iter()
                        .map(|i| Protomod::preproc_expr(self, prog, mp, i, loc))
                        .collect(),
                )
            }
            &Ast::LocalGeneric(_, _, _) => {
                // nothing to do for generics really,
                // since it's all just strings
                x.clone()
            }
            &Ast::Localid(ref id, ref iloc) => {
                Protomod::preproc_localid(prog, mp, id, iloc)
            }
            &Ast::Modid(ref module, ref localid, ref iloc) => {
                Protomod::preproc_modid(prog, mp, module, localid, iloc)
            }
            &Ast::TypeCall(ref base, ref typs, ref iloc) => {
                self.preproc_typecall(prog, mp, base, typs, iloc)
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
            &Ast::TypeFunc(ref parts, ref result, ref loc) => {
                let anon_type = Lstr::Sref("anon_func_type");
                let ppp = parts
                    .iter()
                    .map(|p| {
                        Protomod::preproc_func_arg(
                            self,
                            prog,
                            mp,
                            &anon_type,
                            p.k_ref(),
                            p.x_ref(),
                            loc,
                        )
                    })
                    .collect();
                let pp_result = self.preproc_func_result(prog, mp, result, loc);
                Ast::TypeFunc(ppp, Box::new(pp_result), *loc)
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
            &Ast::DefFunc(_, _, _) => {
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
        arg_name: Option<&Lstr>,
        arg_type: Option<&Ast>,
        loc: &SrcLoc,
    ) -> Kxpr
    {
        match (arg_name, arg_type) {
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
            (ref k, Some(ref argt)) => {
                Kxpr {
                    k: k.map(|ik| ik.clone()),
                    x: Some(Box::new(Protomod::preproc_expr(
                        self, prog, mp, argt, &loc,
                    ))),
                }
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
        name: &Lstr,
        args: &LinkedList<Kxpr>,
        rtype: &Ast,
        body: &Ast,
        loc: &SrcLoc,
    )
    {
        let modid = ModLocalId::new(mp.key.name.clone(), name.clone());
        let pp_args: LinkedList<Kxpr> = args
            .iter()
            .map(|a| {
                Protomod::preproc_func_arg(
                    self,
                    prog,
                    mp,
                    &name,
                    a.k_ref(),
                    a.x_ref(),
                    loc,
                )
            })
            .collect();
        let pp_rtype_ast =
            Protomod::preproc_func_result(self, prog, mp, rtype, loc);
        let pp_body = match body {
            // normal function body
            Ast::Block(_) => Protomod::preproc_expr(self, prog, mp, body, loc),
            // match func body
            Ast::IfExpr(ast::IfType::MatchFunc, _, ref cases, ref iloc) => {
                let match_args = pp_args
                    .iter()
                    .map(|a| {
                        let argn = a.k_clone().unwrap();
                        Kxpr::new_x(Ast::Localid(argn, *iloc))
                    })
                    .collect();
                let match_arg_tuple = Box::new(Ast::Tuple(match_args));
                let ifx = &Ast::IfExpr(
                    ast::IfType::Match,
                    match_arg_tuple,
                    cases.clone(),
                    *iloc,
                );
                self.preproc_expr(prog, mp, ifx, iloc)
            }
            Ast::RustBlock => {
                if let &Ast::TypeAnon = rtype {
                    panic!(
                        "return type must be defined for Rust functions: {}",
                        modid
                    );
                }
                Ast::RustBlock
            }
            _ => {
                panic!("invalid function body: {:?}", body);
            }
        };
        let decl = ast::FuncDecl {
            name: Ast::Localid(name.clone(), *loc),
            args: pp_args.clone(),
            result: pp_rtype_ast.clone(),
            loc: *loc,
        };
        let pp_func = Ast::DefFunc(
            ast::FuncClass::Func,
            Box::new(decl),
            Box::new(pp_body),
        );

        let mut ftype_parts: Vec<Kxpr> =
            pp_args.iter().map(|a| a.clone()).collect();
        let (ftype_part_types, rtype): (Struple2<Type>, Type) = {
            let pp_ftypr: Vec<
                Lresult<StrupleItem<Option<Lstr>, Type>>,
            >;
            pp_ftypr = ftype_parts
                .iter()
                .map(|argt| {
                    let typ = self.preproc_type(
                        prog,
                        mp,
                        &vec![],
                        argt.x_ref().unwrap(),
                        loc,
                    )?;
                    Ok(StrupleItem::new(argt.k_clone(), typ))
                })
                .collect();
            let pp_args: Vec<StrupleItem<Option<Lstr>, Type>>;
            pp_args = Lresult::from_iter(pp_ftypr).unwrap();
            let pp_rtype = self
                .preproc_type(prog, mp, &vec![], &pp_rtype_ast, loc)
                .unwrap();
            (StrupleKV::from_vec(pp_args), pp_rtype)
        };
        ftype_parts.push(Kxpr::new_x(pp_rtype_ast));
        let functype = FuncType::new(ftype_part_types, rtype);
        let ftype = Type::Func(functype);
        let fref_args =
            pp_args.iter().map(|a| (a.k_clone(), Val::Void)).collect();

        let full_lri =
            Lri::with_modules(modid.module.clone(), modid.local.clone());
        let funcref = Val::FuncRef(full_lri, fref_args, ftype.clone());

        self.funcseq.push_back(name.clone());
        self.funcsrc.insert(name.clone(), pp_func);
        self.valtypes.insert(name.clone(), ftype);
        self.constants.insert(name.clone(), funcref);
    }

    pub fn preproc_generic_func(
        &mut self,
        prog: &Lib,
        mp: &ModulePreface,
        name: &Lstr,
        type_args: &KxprList,
        args: &KxprList,
        rtype: &Ast,
        body: &Ast,
        loc: &SrcLoc,
    ) -> Lresult<()>
    {
        // for each type arg, make sure type IDs are converted to type vars
        let type_var_name_result: Vec<Lresult<Lstr>> = type_args
            .iter()
            .map(|targ| {
                match targ {
                    Kxpr {
                        k: None,
                        x: Some(ref id),
                    } => {
                        match **id {
                            Ast::Localid(ref argn, _) => Ok(argn.clone()),
                            _ => {
                                Err(Failure::new(
                                    "code_err",
                                    Lstr::from(format!(
                                        "unrecognized type arg: {:?}",
                                        targ
                                    )),
                                ))
                            }
                        }
                    }
                    _ => {
                        Err(Failure::new(
                            "code_err",
                            Lstr::from(format!(
                                "unrecognized type arg: {:?}",
                                targ
                            )),
                        ))
                    }
                }
            })
            .collect();
        let type_var_names: Vec<Lstr> =
            Lresult::from_iter(type_var_name_result)?;
        let type_vars: Vec<Type> = type_var_names
            .iter()
            .map(|tvn| Type::Var(tvn.clone()))
            .collect();
        let pp_args: LinkedList<Kxpr> = args
            .iter()
            .map(|a| {
                Protomod::preproc_func_arg(
                    self,
                    prog,
                    mp,
                    &name,
                    a.k_ref(),
                    a.x_ref(),
                    loc,
                )
            })
            .collect();
        let pp_rtype_ast =
            Protomod::preproc_func_result(self, prog, mp, rtype, loc);
        let pp_body = self.preproc_func_body(prog, mp, args, body, loc)?;
        if Ast::RustBlock == pp_body && Ast::TypeAnon == *rtype {
            return Err(Failure::new(
                "bad_type",
                Lstr::from(format!(
                    "return type must be defined for Rust functions: {}",
                    name
                )),
            ));
        }
        let decl = ast::FuncDecl {
            name: Ast::Localid(name.clone(), *loc),
            args: pp_args.clone(),
            result: pp_rtype_ast.clone(),
            loc: *loc,
        };
        let pp_func = Ast::DefFunc(
            ast::FuncClass::Func,
            Box::new(decl),
            Box::new(pp_body),
        );

        let mut ftype_parts: Vec<Kxpr> =
            pp_args.iter().map(|a| a.clone()).collect();
        let (ftype_part_types, rtype): (Struple2<Type>, Type) = {
            let pp_ftype_parts = ftype_parts
                .iter()
                .map(|argt| {
                    StrupleItem::new(
                        argt.k_clone(),
                        self.preproc_type(
                            prog,
                            mp,
                            &type_var_names,
                            argt.x_ref().unwrap(),
                            loc,
                        )
                        .unwrap(),
                    )
                })
                .collect();
            let pp_rtype = self
                .preproc_type(prog, mp, &type_var_names, &pp_rtype_ast, loc)
                .unwrap();
            (StrupleKV::from_vec(pp_ftype_parts), pp_rtype)
        };
        ftype_parts.push(Kxpr::new_x(pp_rtype_ast));
        let functype = FuncType::new(ftype_part_types, rtype);
        let ftype = Type::GenericFunc(type_var_names.clone(), functype);
        let fref_args =
            pp_args.iter().map(|a| (a.k_clone(), Val::Void)).collect();

        let funcri = Lri::full(
            Some(mp.key.name.clone()),
            name.clone(),
            Some(type_vars.clone()),
        );
        let funcref = Val::FuncRef(funcri, fref_args, ftype.clone());

        self.funcseq.push_back(name.clone());
        self.funcsrc.insert(name.clone(), pp_func);
        self.valtypes.insert(name.clone(), ftype);
        self.constants.insert(name.clone(), funcref);
        Ok(())
    }

    pub fn preproc_func_body(
        &mut self,
        prog: &Lib,
        mp: &ModulePreface,
        args: &KxprList,
        body: &Ast,
        loc: &SrcLoc,
    ) -> Lresult<Ast>
    {
        let result = match body {
            // normal function body
            Ast::Block(_) => Protomod::preproc_expr(self, prog, mp, body, loc),
            // match func body
            Ast::IfExpr(ast::IfType::MatchFunc, _, ref cases, ref iloc) => {
                let match_args = args
                    .iter()
                    .map(|a| {
                        let argn = a.k_clone().unwrap();
                        Kxpr::new_x(Ast::Localid(argn, *iloc))
                    })
                    .collect();
                let match_arg_tuple = Box::new(Ast::Tuple(match_args));
                let ifx = &Ast::IfExpr(
                    ast::IfType::Match,
                    match_arg_tuple,
                    cases.clone(),
                    *iloc,
                );
                self.preproc_expr(prog, mp, ifx, iloc)
            }
            Ast::RustBlock => Ast::RustBlock,
            _ => {
                return Err(Failure::new(
                    "code_error",
                    Lstr::from(format!("invalid function body: {:?}", body)),
                ));
            }
        };
        Ok(result)
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
            .map(|a| {
                self.preproc_func_arg(
                    prog,
                    mp,
                    &closure_key,
                    a.k_ref(),
                    a.x_ref(),
                    loc,
                )
            })
            .collect();
        let pp_body = self.preproc_expr(prog, mp, body, loc);
        let pp_result = self.preproc_func_result(prog, mp, &Ast::TypeAnon, loc);

        let arg_types: Vec<StrupleItem<Option<Lstr>, Type>> = pp_args
            .iter()
            .map(|a| {
                let x_ref = a.x_ref();
                if x_ref.is_none() {
                    panic!("closure arg type is None: {}", closure_key);
                }
                let atype = x_ref.unwrap();
                StrupleItem::new(
                    a.k_clone(),
                    self.preproc_type(prog, mp, &vec![], atype, loc).unwrap(),
                )
            })
            .collect();
        let result_type = self
            .preproc_type(prog, mp, &vec![], &pp_result, loc)
            .unwrap();
        let func_type = FuncType::new(StrupleKV::from(arg_types), result_type);
        let ftype = Type::Func(func_type);
        // let type_vars = ftype.collect_typevars();

        let fref_args =
            pp_args.iter().map(|a| (a.k_clone(), Val::Void)).collect();

        let decl = ast::FuncDecl {
            name: Ast::Localid(closure_key.clone(), *loc),
            args: pp_args,
            result: pp_result.clone(),
            loc: *loc,
        };
        let pp_func = Ast::DefFunc(
            ast::FuncClass::Closure,
            Box::new(decl),
            Box::new(pp_body),
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
            Ast::DefFunc(ast::FuncClass::Macro, decl, body) => {
                vout!("apply_macro({:?}, {:?})\n", decl.name, decl.args);
                let macrod = Protomod::apply_macro(
                    &decl.name, &body, &decl.args, &pp_args, loc,
                );
                // do it again to make sure there's not a wrapped macro
                return self.preproc_expr(prog, mp, &macrod, &decl.loc);
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
                    Box::new(Ast::TypeAnon),
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
            &Ast::TypeCall(ref base, ref tparams, _) => {
                let base2 = Protomod::replace_ids(base, idvals, loc);
                let tparams2 = tparams
                    .iter()
                    .map(|tp| {
                        tp.map_x(|x| Protomod::replace_ids(x, idvals, loc))
                    })
                    .collect();
                Ast::TypeCall(Box::new(base2), tparams2, *loc)
            }
            &Ast::Modid(_, _, _) => node.clone(),
            &Ast::Localid(ref name, _) => {
                match idvals.get(&*name) {
                    Some(newx) => (*newx).clone(),
                    None => Ast::Localid(name.clone(), *loc),
                }
            }
            &Ast::StrExpr(ref items, _) => {
                let new_items = items
                    .iter()
                    .map(|i| Protomod::replace_ids(i, idvals, loc))
                    .collect();
                Ast::StrExpr(new_items, *loc)
            }
            &Ast::DefFunc(fc, ref decl, ref body) => {
                let r_name = Protomod::replace_ids(&decl.name, idvals, loc);
                let r_args = decl
                    .args
                    .iter()
                    .map(|a| {
                        a.map_x(|ia| Protomod::replace_ids(ia, idvals, loc))
                    })
                    .collect();
                let r_result = Protomod::replace_ids(&decl.result, idvals, loc);
                let r_decl = ast::FuncDecl {
                    name: r_name,
                    args: r_args,
                    result: r_result,
                    loc: *loc,
                };
                let r_body = Protomod::replace_ids(body, idvals, loc);
                Ast::DefFunc(fc, Box::new(r_decl), Box::new(r_body))
            }
            &Ast::Return(ref result, _) => {
                let new_result = Protomod::replace_ids(result, idvals, loc);
                Ast::Return(Box::new(new_result), *loc)
            }
            &Ast::ConstBool(b) => Ast::ConstBool(b),
            &Ast::ConstInt(i) => Ast::ConstInt(i),
            &Ast::ConstHashtag(ref s) => Ast::ConstHashtag(s.clone()),
            &Ast::ConstStr(ref s) => Ast::ConstStr(s.clone()),
            &Ast::ConstVoid => Ast::ConstVoid,
            &Ast::TypeAnon => Ast::TypeAnon,
            &Ast::Wildcard => Ast::Wildcard,
            _ => {
                panic!("cannot replace_ids for expression: {:?}", node);
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

    pub fn preproc_modid(
        prog: &Lib,
        mp: &ModulePreface,
        mod_name: &Lstr,
        localid: &Lstr,
        loc: &SrcLoc,
    ) -> Ast
    {
        if *mod_name != *mp.key.name && !mp.imports.contains(mod_name) {
            panic!(
                "module not found: '{}::{}' in {} @ {}",
                mod_name, localid, mp.key.name, loc,
            );
        }
        match prog.get_macro(mod_name, localid) {
            Some(mac) => mac.clone(),
            None => Ast::Modid(mod_name.clone(), localid.clone(), *loc),
        }
    }

    pub fn preproc_typecall(
        &mut self,
        prog: &Lib,
        mp: &ModulePreface,
        base: &Ast,
        typs: &LinkedList<Kxpr>,
        loc: &SrcLoc,
    ) -> Ast
    {
        let pp_base = self.preproc_expr(prog, mp, base, loc);
        let pp_types = typs
            .iter()
            .map(|t| {
                t.map_x(|tx| {
                    match tx {
                        Ast::Localid(ref id, ref iloc) => {
                            if self.deftypes.contains_key(id) {
                                Ast::Modid(
                                    mp.key.name.clone(),
                                    id.clone(),
                                    *iloc,
                                )
                            } else {
                                Ast::TypeVar(id.clone(), *iloc)
                            }
                        }
                        Ast::TypeVar(_, _) => tx.clone(),
                        _ => self.preproc_expr(prog, mp, tx, loc),
                    }
                })
            })
            .collect();
        Ast::TypeCall(Box::new(pp_base), pp_types, *loc)
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
            ast::IfType::MatchFunc => {
                panic!("cannot have matchfunc in normal code");
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
            &Ast::TypeCall(ref name, ref params, ref iloc) => {
                let pp_call = Protomod::preproc_pattern(prog, mp, name, iloc);
                let pp_params = params
                    .iter()
                    .map(|pkx| {
                        pkx.map_x(|p| {
                            Protomod::preproc_pattern(prog, mp, p, iloc)
                        })
                    })
                    .collect();
                Ast::TypeCall(Box::new(pp_call), pp_params, *iloc)
            }
            &Ast::Modid(_, _, _) => p.clone(),
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
        type_params: &Vec<Lstr>,
        typ: &Ast,
        loc: &SrcLoc,
    ) -> Lresult<Type>
    {
        let pp_x = Protomod::preproc_expr(self, prog, mp, typ, loc);
        let local_type = Type::from(&pp_x);
        self.replace_typeids(type_params, local_type)
    }

    pub fn replace_typeids(
        &self,
        type_params: &Vec<Lstr>,
        t: Type,
    ) -> Lresult<Type>
    {
        // rewrite the modules if necessary
        let result = match t {
            Type::UserDef(id) => {
                if id.local_only()
                    && Protomod::find_type_param(type_params, &id.localid)
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
            Type::Func(ftype) => {
                let args2 = ftype
                    .args
                    .map_v_into(|a| self.replace_typeids(type_params, a))?;
                let closed2 = ftype
                    .closed
                    .map_v_into(|a| self.replace_typeids(type_params, a))?;
                let result2 =
                    self.replace_typeids(type_params, *ftype.result)?;
                Type::Func(FuncType::new_closure(args2, closed2, result2))
            }
            Type::StrictList(subtype) => {
                let sub2 = self.replace_typeids(type_params, *subtype)?;
                Type::StrictList(Box::new(sub2))
            }
            Type::Tuple(mut items) => {
                let items2: Vec<Lresult<(Option<Lstr>, Type)>> = items
                    .0
                    .drain(..)
                    .map(|mut i| {
                        i.1 = self.replace_typeids(type_params, i.1)?;
                        Ok(i)
                    })
                    .collect();
                Type::Tuple(Struple(Lresult::from_iter(items2)?))
            }
            Type::Var(varname) => Type::Var(varname),
            // primitive types
            Type::Bool => Type::Bool,
            Type::Hashtag => Type::Hashtag,
            Type::Failure => Type::Failure,
            Type::Int => Type::Int,
            Type::Str => Type::Str,
            Type::Void => Type::Void,
            typ => {
                return Err(Failure::new(
                    "type_err",
                    Lstr::from(format!("cannot replace_typeids for: {}", typ)),
                ));
            }
        };
        Ok(result)
    }

    pub fn find_type_param(params: &Vec<Lstr>, name: &str) -> Option<i8>
    {
        for (i, p) in params.iter().enumerate() {
            if name == p {
                return Some(i as i8);
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

        let type_name = Type::UserDef(name.clone());
        self.deftypes.insert(base_name.localid, type_name);

        match datatype {
            ast::DataType::Struple => {
                if fields.is_empty() {
                    self.preproc_struple_token(name, loc);
                } else {
                    self.preproc_struple_with_fields(
                        prog,
                        mp,
                        name_ast.clone(),
                        name,
                        fields,
                        loc,
                    );
                }
            }
            ast::DataType::Enum => {
                self.preproc_enum(prog, mp, name_ast, &name, fields);
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
        let name_lstr = full_lri.localid.clone();
        let type_name = Type::UserDef(full_lri.clone());

        // a token struct is stored as a constant with no constructor
        let constval = Val::Token(full_lri);
        self.constants.insert(name_lstr.clone(), constval);
        self.valtypes.insert(name_lstr, type_name);
    }

    pub fn preproc_struple_with_fields(
        &mut self,
        prog: &Lib,
        mp: &ModulePreface,
        type_ast: Ast,
        type_lri: Lri,
        src_fields: &LinkedList<Kxpr>,
        loc: &SrcLoc,
    )
    {
        self.preproc_struple_fields(
            prog,
            mp,
            type_ast,
            type_lri.clone(),
            None,
            src_fields,
            loc,
        );
    }

    pub fn preproc_struple_fields(
        &mut self,
        prog: &Lib,
        mp: &ModulePreface,
        type_ast: Ast,
        struple_lri: Lri,
        variant: Option<Lstr>,
        src_fields: &LinkedList<Kxpr>,
        loc: &SrcLoc,
    )
    {
        let opt_variant = variant.clone();
        let local_func_name = variant.unwrap_or(struple_lri.localid.clone());
        let type_vars = struple_lri.type_var_names();

        let struple_fields: Vec<(Option<Lstr>, Type)> = src_fields
            .iter()
            .map(|f| {
                // struple fields are made w/ an x_list,
                // x_ref will always be there
                let pp_type = self
                    .preproc_type(prog, mp, &type_vars, f.x_ref().unwrap(), loc)
                    .unwrap();
                (f.k_clone(), pp_type)
            })
            .collect();
        let fref_args = Struple(
            struple_fields
                .iter()
                .map(|f| (f.0.clone(), Val::Void))
                .collect(),
        );

        let full_type = Type::UserDef(struple_lri.clone());
        let func_args: Struple2<Type> = struple_fields
            .iter()
            .map(|item| StrupleItem::new(item.0.clone(), item.1.clone()))
            .collect();
        let ifunc_type = FuncType::new(func_args, full_type.clone());
        let func_type = if type_vars.is_empty() {
            Type::Func(ifunc_type)
        } else {
            Type::GenericFunc(type_vars.clone(), ifunc_type)
        };
        let construct_ri = struple_lri.replace_local(local_func_name.clone());

        let srcblk = Ast::ConstructData(struple_lri.clone(), opt_variant);

        let decl = ast::FuncDecl {
            name: Ast::Localid(local_func_name.clone(), *loc),
            args: (*src_fields).clone(),
            result: type_ast,
            loc: *loc,
        };
        let srcxpr = Ast::DefFunc(
            ast::FuncClass::Func,
            Box::new(decl),
            Box::new(srcblk),
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
            .insert(local_func_name.clone(), Struple(struple_fields));

        let funcref = Val::FuncRef(construct_ri, fref_args, func_type.clone());
        self.constants.insert(local_func_name.clone(), funcref);

        self.funcseq.push_back(local_func_name.clone());
        self.funcsrc.insert(local_func_name.clone(), srcxpr);

        self.valtypes.insert(local_func_name, func_type);
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
            match func_type {
                Type::Func(ref ftype) => Some((*ftype.result).clone()),
                Type::GenericFunc(_, ref ftype) => {
                    Some((*ftype.result).clone())
                }
                Type::SpecialFunc(_, ref ftype) => {
                    Some((*ftype.result).clone())
                }
                _ => None,
            }
        })
    }

    pub fn preproc_enum(
        &mut self,
        prog: &Lib,
        mp: &ModulePreface,
        type_ast: &Ast,
        enum_lri: &Lri,
        src_variants: &LinkedList<Kxpr>,
    )
    {
        let mod_type = Type::UserDef(enum_lri.clone());

        let mut variant_fields = Vec::with_capacity(src_variants.len());
        for kx in src_variants.iter() {
            let v = kx.x_ref().unwrap();
            if let &Ast::DefData(_, ref vname, ref fields, ref iloc) = v {
                self.preproc_enum_variant(
                    prog,
                    mp,
                    type_ast.clone(),
                    enum_lri.clone(),
                    vname,
                    fields,
                    iloc,
                );
                let variant_lstr = Lstr::from(&**vname);
                let vf = (variant_lstr, mod_type.clone());
                variant_fields.push(vf);
            } else {
                panic!("variant data is not DefData: {:?}", v);
            }
        }

        // self.constants.insert(name_lstr.clone(), Val::Type(mod_type.clone()));
    }

    pub fn preproc_enum_variant(
        &mut self,
        prog: &Lib,
        mp: &ModulePreface,
        type_ast: Ast,
        full_lri: Lri,
        name: &Ast,
        fields: &LinkedList<Kxpr>,
        loc: &SrcLoc,
    )
    {
        let typ = Type::UserDef(full_lri.clone());
        let variant_name = Lstr::from(name);
        vout!("preproc_enum_variant({}.{})\n", full_lri, variant_name);

        if fields.is_empty() {
            let const_val =
                Val::EnumToken(full_lri.clone(), variant_name.clone());
            self.constants.insert(variant_name.clone(), const_val);
            self.valtypes.insert(variant_name, typ);
        } else {
            self.preproc_struple_fields(
                prog,
                mp,
                type_ast,
                full_lri.clone(),
                Some(variant_name),
                fields,
                loc,
            );
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
    use leema::struple::{Struple, StrupleItem, StrupleKV};
    use leema::types;
    use leema::val::{FuncType, Type, Val};


    #[test]
    fn test_preproc_list_pattern()
    {
        let input = String::from(
            "
            func foo(a) >>
            |(h;t) -> print(\"head: $h, tail: $t\n\")
            --

            func main() -> foo([3, 4, 5]) --
            ",
        );

        let mut loader = Interloader::new(Lstr::Sref("tacos.lma"), "lib");
        loader.set_mod_txt(Lstr::Sref("tacos"), input);
        let mut prog = program::Lib::new(loader);
        let pmod = prog.read_proto(&Lstr::Sref("tacos"));

        assert_eq!(2, pmod.funcsrc.len());

        let foo_func = pmod.funcsrc.get("foo").unwrap();
        if let &Ast::DefFunc(foo_ft, ref foo_decl, _) = foo_func {
            assert_eq!(ast::FuncClass::Func, foo_ft);
            assert_eq!("foo", &Lstr::from(&foo_decl.name));
        } else {
            panic!("foo is not a function definition");
        }
        pmod.constants.get("foo").unwrap();

        let main_func = pmod.funcsrc.get("main").unwrap();
        if let &Ast::DefFunc(main_ft, ref main_decl, _) = main_func {
            assert_eq!(ast::FuncClass::Func, main_ft);
            assert_eq!("main", &Lstr::from(&main_decl.name));
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
        let mut loader = Interloader::new(Lstr::Sref("foo.lma"), "lib");
        loader.set_mod_txt(foo_str.clone(), input);
        let mut prog = program::Lib::new(loader);
        let pmod = prog.read_proto(&foo_str);

        let open_valtype = pmod.valtypes.get("open_foo").unwrap();
        if let Type::Func(open_ft) = open_valtype {
            assert!(open_ft.args.is_empty());
            assert_eq!("foo::Foo", &open_ft.result.full_typename());
        } else {
            panic!("open_valtype is not a func type: {:?}", open_valtype);
        }

        let _close_valtype = pmod.valtypes.get("close_foo").unwrap();
    }

    #[test]
    fn test_preproc_func_returns_func()
    {
        let input = "
            func foo(): F(x: Int): F(): Bool -RUST-
            func bar(): (F(x: Int): (F(): Bool)) -RUST-
            "
        .to_string();

        let foo_str = Lstr::Sref("foo");
        let mut loader = Interloader::new(Lstr::Sref("foo.lma"), "lib");
        loader.set_mod_txt(foo_str.clone(), input);
        let mut prog = program::Lib::new(loader);
        let pmod = prog.read_proto(&foo_str);

        let type_foo = pmod.valtypes.get("foo").unwrap();
        let type_bar = pmod.valtypes.get("bar").unwrap();
        assert_eq!(type_foo, type_bar);
    }

    #[test]
    fn test_preproc_generic_func()
    {
        let input = "
            func swap[A, B](x: A, y: B): (B, A) >>
                (b, a)
            --
            "
        .to_string();

        let foo_str = Lstr::Sref("foo");
        let mut loader = Interloader::new(Lstr::Sref("foo.lma"), "lib");
        loader.set_mod_txt(foo_str.clone(), input);
        let mut prog = program::Lib::new(loader);
        let pmod = prog.read_proto(&foo_str);

        let _foo_type = pmod.valtypes.get("swap").unwrap();
        let _foo_const = pmod.constants.get("swap").unwrap();
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
        let mut loader = Interloader::new(Lstr::Sref("foo.lma"), "lib");
        loader.set_mod_txt(foo_str.clone(), input);
        let mut prog = program::Lib::new(loader);
        let pmod = prog.read_proto(&foo_str);

        let closure0 = pmod.closures.front().unwrap();
        assert_eq!("foo_4_i", closure0);
        assert_eq!(1, pmod.closures.len());
    }

    #[test]
    fn test_preproc_enum_tree()
    {
        let input = String::from(
            "
            enum Tree
            |Node(Tree, Int, Tree)
            |Empty
            --
            ",
        );

        let tacos_str = Lstr::Sref("tacos");
        let mut loader = Interloader::new(Lstr::Sref("tacos.lma"), "lib");
        loader.set_mod_txt(tacos_str.clone(), input);
        let mut prog = program::Lib::new(loader);
        let pmod = prog.read_proto(&tacos_str);

        let local_typename = Lstr::Sref("Tree");
        let type_lri =
            Lri::with_modules(tacos_str.clone(), local_typename.clone());
        let tree_type = Type::UserDef(type_lri.clone());
        let node_funct = Type::Func(FuncType::new(
            StrupleKV::from(vec![
                tree_type.clone(),
                Type::Int,
                tree_type.clone(),
            ]),
            tree_type.clone(),
        ));

        // closures are empty
        assert!(pmod.closures.is_empty());

        // constants
        let exp_empty = Val::EnumToken(type_lri.clone(), Lstr::Sref("Empty"));
        let empty = pmod.constants.get("Empty").unwrap();
        assert_eq!(exp_empty, *empty);
        let node_const = pmod.constants.get("Node").unwrap();
        if let Val::FuncRef(node_fri, node_args, act_node_ftype) = node_const {
            assert_eq!("tacos", node_fri.modules.as_ref().unwrap());
            assert_eq!("Node", &node_fri.localid);
            assert_eq!(node_funct, *act_node_ftype);
            assert_eq!(Val::Void, node_args.0[0].1);
        } else {
            panic!("Node func const is not a FuncRef: {}", node_const);
        }
        assert_eq!(3, pmod.constants.len());

        // type constants; these aren't really used I think
        let types = pmod.constants.get("TYPES").unwrap();
        assert_eq!(1, list::len(types));

        // deftypes
        assert_eq!(tree_type, *pmod.deftypes.get("Tree").unwrap());
        assert_eq!(1, pmod.deftypes.len());

        // funcseq has Node
        assert_eq!("Node", pmod.funcseq.front().unwrap());
        assert_eq!(1, pmod.funcseq.len());

        // funcsrc
        assert!(pmod.funcsrc.get("Node").is_some());
        assert_eq!(1, pmod.funcsrc.len());

        // struple fields for Node
        let node_flds = pmod.struple_fields.get("Node").unwrap();
        assert_eq!(tree_type, node_flds.0[0].1);
        assert_eq!(Type::Int, node_flds.0[1].1);
        assert_eq!(tree_type, node_flds.0[2].1);
        assert_eq!(3, node_flds.0.len());
        assert_eq!(1, pmod.struple_fields.len());

        // valtypes
        assert_eq!(tree_type, *pmod.valtypes.get("Empty").unwrap());
        assert_eq!(node_funct, *pmod.valtypes.get("Node").unwrap());
        assert_eq!(2, pmod.valtypes.len());
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
        let mut loader = Interloader::new(Lstr::Sref("colors.lma"), "lib");
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
        let mut loader = Interloader::new(Lstr::Sref("animals.lma"), "lib");
        loader.set_mod_txt(animals_str.clone(), input);
        let mut prog = program::Lib::new(loader);
        let pmod = prog.read_proto(&animals_str);

        let type_lri = Lri::full(
            Some(animals_str.clone()),
            Lstr::Sref("Animal"),
            Some(vec![Type::Var(Lstr::Sref("A"))]),
        );
        let animal_type = Type::UserDef(type_lri.clone());
        let typevar_a = Type::Var(Lstr::Sref("A"));
        let dog_name = Lstr::from("Dog".to_string());
        let cat_func_type = Type::GenericFunc(
            vec![Lstr::Sref("A")],
            FuncType::new(
                StrupleKV::from(vec![Type::Int]),
                animal_type.clone(),
            ),
        );
        let mouse_func_type = Type::GenericFunc(
            vec![Lstr::Sref("A")],
            FuncType::new(
                StrupleKV::from(vec![typevar_a.clone()]),
                animal_type.clone(),
            ),
        );
        let giraffe_func_type = Type::GenericFunc(
            vec![Lstr::Sref("A")],
            FuncType::new(
                StrupleKV::from_vec(vec![
                    StrupleItem::new(Some(Lstr::Sref("height")), Type::Int),
                    StrupleItem::new(
                        Some(Lstr::Sref("weight")),
                        typevar_a.clone(),
                    ),
                ]),
                animal_type.clone(),
            ),
        );

        // no closures
        assert!(pmod.closures.is_empty());

        // verify constants
        let dog_const =
            pmod.constants.get("Dog").expect("missing constant: Dog");
        let cat_const =
            pmod.constants.get("Cat").expect("missing constant: Cat");
        let mouse_const = pmod
            .constants
            .get("Mouse")
            .expect("missing constant: Mouse");
        assert!(mouse_const.is_funcref());
        let giraffe_const = pmod
            .constants
            .get("Giraffe")
            .expect("missing constant: Giraffe");
        assert_eq!(5, pmod.constants.len());

        let exp_dog_const = Val::EnumToken(type_lri.clone(), dog_name);
        let exp_cat_const = Val::FuncRef(
            type_lri.replace_local(Lstr::Sref("Cat")),
            Struple(vec![(None, Val::Void)]),
            cat_func_type.clone(),
        );
        let exp_giraffe_const = Val::FuncRef(
            type_lri.replace_local(Lstr::Sref("Giraffe")),
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

        // verify deftypes
        assert_eq!(animal_type, *pmod.deftypes.get("Animal").unwrap());
        assert_eq!(1, pmod.deftypes.len());

        // verify function sequence
        let mut fseq_it = pmod.funcseq.iter();
        assert_eq!("Cat", fseq_it.next().unwrap().str());
        assert_eq!("Mouse", fseq_it.next().unwrap().str());
        assert_eq!("Giraffe", fseq_it.next().unwrap().str());
        assert_eq!(3, pmod.funcseq.len());

        // verify function source
        assert!(pmod.funcsrc.get("Cat").is_some());
        assert!(pmod.funcsrc.get("Mouse").is_some());
        assert!(pmod.funcsrc.get("Giraffe").is_some());
        assert_eq!(3, pmod.funcsrc.len());

        // verify struple fields
        // cat fields
        let cat_flds = pmod.struple_fields.get("Cat").unwrap();
        assert!(cat_flds.0[0].0.is_none());
        assert_eq!(Type::Int, cat_flds.0[0].1);
        assert_eq!(1, cat_flds.0.len());
        // mouse fields
        let mouse_flds = pmod.struple_fields.get("Mouse").unwrap();
        assert!(mouse_flds.0[0].0.is_none());
        assert_eq!(Type::Var(Lstr::Sref("A")), mouse_flds.0[0].1);
        assert_eq!(1, mouse_flds.0.len());
        // giraffe fields
        let giraffe_flds = pmod.struple_fields.get("Giraffe").unwrap();
        assert_eq!(Some(Lstr::Sref("height")), giraffe_flds.0[0].0);
        assert_eq!(Type::Int, giraffe_flds.0[0].1);
        assert_eq!(Some(Lstr::Sref("weight")), giraffe_flds.0[1].0);
        assert_eq!(Type::Var(Lstr::Sref("A")), giraffe_flds.0[1].1);
        assert_eq!(2, giraffe_flds.0.len());
        assert_eq!(3, pmod.struple_fields.len());

        // verify value types
        assert_eq!(
            "animals::Animal[$A,]",
            format!("{}", *pmod.valtypes.get("Dog").unwrap())
        );
        assert_eq!(
            "GF[A,](Int,/):animals::Animal[$A,]",
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
        let mut loader = Interloader::new(Lstr::Sref("greet.lma"), "lib");
        loader.set_mod_txt(greet.clone(), input);
        let mut prog = program::Lib::new(loader);
        let pmod = prog.read_proto(&greet);

        let greeting_lstr = Lstr::Sref("Greeting");
        let greeting_fullri = Lri::with_modules(greet.clone(), greeting_lstr);
        let greeting_typref = Type::UserDef(greeting_fullri);
        let xfunctyp = Type::Func(FuncType::new(
            StrupleKV::from_vec(vec![
                StrupleItem::new(None, Type::Str),
                StrupleItem::new(None, Type::Str),
            ]),
            greeting_typref.clone(),
        ));

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

        let mut loader = Interloader::new(Lstr::Sref("tacos.lma"), "lib");
        loader.set_mod_txt(Lstr::Sref("tacos"), input);
        let mut prog = program::Lib::new(loader);
        let pmod = prog.read_proto(&Lstr::Sref("tacos"));

        // assert valtypes
        assert!(pmod.valtypes.contains_key("Burrito"));
        let constructor = pmod.valtypes.get("Burrito").unwrap();
        if let &Type::Func(ref ft) = constructor {
            assert_eq!(2, ft.args.len());
            let exp_result = Type::UserDef(Lri::with_modules(
                Lstr::from("tacos"),
                Lstr::from("Burrito"),
            ));
            assert_eq!(exp_result, *ft.result);
        } else {
            panic!("constructor valtype is not a func");
        }

        let xtyperef = Type::UserDef(Lri::with_modules(
            Lstr::from("tacos"),
            Lstr::from("Burrito"),
        ));
        let xfunctype = Type::Func(FuncType::new(
            StrupleKV::from_vec(vec![
                StrupleItem::new(None, Type::Bool),
                StrupleItem::new(Some(Lstr::Sref("buns")), Type::Int),
            ]),
            xtyperef.clone(),
        ));

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
        let mut loader = Interloader::new(Lstr::Sref("tacos.lma"), "lib");
        loader.set_mod_txt(Lstr::Sref("tacos"), input);
        let mut prog = program::Lib::new(loader);
        let pmod = prog.read_proto(&Lstr::Sref("tacos"));

        // assert valtypes
        assert!(pmod.valtypes.contains_key("Burrito"));
        let constructor = pmod.valtypes.get("Burrito").unwrap();
        if let &Type::Func(ref ft) = constructor {
            assert_eq!(2, ft.args.len());
            let exp_result = Type::UserDef(Lri::with_modules(
                Lstr::from("tacos"),
                Lstr::from("Burrito"),
            ));
            assert_eq!(exp_result, *ft.result);
        } else {
            panic!("constructor valtype is not a func");
        }

        let xtyperef = Type::UserDef(Lri::with_modules(
            Lstr::from("tacos"),
            Lstr::from("Burrito"),
        ));
        let xfunctype = Type::Func(FuncType::new(
            StrupleKV::from_vec(vec![
                StrupleItem::new(Some(Lstr::Sref("filling")), Type::Str),
                StrupleItem::new(Some(Lstr::Sref("number")), Type::Int),
            ]),
            xtyperef.clone(),
        ));

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

        let mut loader = Interloader::new(Lstr::Sref("tok.lma"), "lib");
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
