use crate::leema::failure::Lresult;
use crate::leema::infer::{Inferator, TypeSet};
use crate::leema::ixpr::{Ixpr, Source};
use crate::leema::lmap;
use crate::leema::lri::Lri;
use crate::leema::lstr::Lstr;
use crate::leema::phase0::Protomod;
use crate::leema::struple::{Struple, StrupleItem, StrupleKV};
use crate::leema::val::{FuncType, Type, Val};

use std::collections::{HashMap, LinkedList};

#[derive(Debug)]
pub enum CallOp
{
    LocalCall(Lstr),
    ExternalCall(Lri),
}

#[derive(Debug)]
pub struct CallFrame<'a>
{
    fri: &'a Lri,
    infer: Inferator<'a>,
    pub calls: LinkedList<CallOp>,
}

impl<'a> CallFrame<'a>
{
    pub fn new(fri: &'a Lri) -> CallFrame<'a>
    {
        CallFrame {
            fri,
            infer: Inferator::new(fri),
            calls: LinkedList::new(),
        }
    }

    pub fn push_call(&mut self, call: CallOp)
    {
        self.calls.push_back(call);
    }

    pub fn pop_call(&mut self) -> Option<CallOp>
    {
        self.calls.pop_front()
    }

    pub fn collect_calls<'b>(&mut self, ix: &'b Ixpr)
    {
        match ix.src {
            Source::Call(ref callx, ref args) => {
                self.collect_callexpr(callx);
                for a in args.0.iter() {
                    self.collect_calls(&a.1);
                }
            }
            Source::Block(ref expressions) => {
                self.collect_calls_vec(expressions);
            }
            Source::Let(ref _lhs, _, ref rhs, ref failed) => {
                self.collect_calls(rhs);
                // would be better to pass the iterator directly instead
                // of creating a new vector, but I don't know how to do
                // that right now and I'm only at this cafe for so long today
                for f in failed.iter() {
                    if f.case.is_none() {
                        continue;
                    }
                    self.collect_calls(f.case.as_ref().unwrap())
                }
            }
            Source::StrMash(ref items) => {
                self.collect_calls_vec(items);
            }
            Source::IfExpr(ref cond, ref truth, ref lies) => {
                self.collect_calls(cond);
                self.collect_calls(truth);
                if lies.is_some() {
                    self.collect_calls(lies.as_ref().unwrap());
                }
            }
            Source::List(ref items) => {
                for i in items {
                    self.collect_calls(i);
                }
            }
            Source::Tuple(ref items) => {
                for i in items.0.iter() {
                    self.collect_calls(&i.1);
                }
            }
            Source::Map(ref items) => {
                for i in items.0.iter() {
                    self.collect_calls(&i.1);
                }
            }
            Source::ConstVal(Val::FuncRef(ref lri, _, _)) => {
                self.push_call(CallOp::ExternalCall(lri.clone()));
            }
            Source::ConstVal(_) => {
                // nothing to do. constants aren't calls.
            }
            Source::Fork(ref sub) => {
                self.collect_calls(sub);
            }
            Source::Id(ref _id, _) => {
                // nothing to do. not calls.
            }
            Source::MatchExpr(ref sample, ref cases) => {
                self.collect_calls(sample);
                self.collect_calls(cases);
            }
            Source::MatchCase(_, ref truth, ref lies) => {
                self.collect_calls(truth);
                self.collect_calls(lies);
            }
            Source::Return(ref result) => {
                self.collect_calls(result);
            }
            Source::Func(ref _args, _, _, _, ref body) => {
                self.collect_calls(body);
            }
            Source::Cons(ref head, ref tail) => {
                self.collect_calls(head);
                self.collect_calls(tail);
            }
            Source::FieldAccess(ref base, _, _) => {
                self.collect_calls(base);
            }
            // nothing to do for these, not calls.
            Source::RustBlock(_, _) => {}
            Source::Construple(_, _, _) => {}
        }
    }

    pub fn collect_callexpr<'b>(&mut self, callx: &'b Ixpr)
    {
        match callx.src {
            Source::Id(ref _callname, _) => {
                // this is probably a local variable that is a function
                // self.push_call(CallOp::LocalCall(callname.clone()));
            }
            Source::ConstVal(ref val) => {
                match val {
                    &Val::Str(ref name) => {
                        print!("where does this local call come from? ");
                        println!("it seems suspect: {}", name);
                        self.push_call(CallOp::LocalCall(name.clone()));
                    }
                    &Val::FuncRef(ref i, _, _) => {
                        self.push_call(CallOp::ExternalCall(i.clone()));
                    }
                    _ => {
                        panic!("const val is not a call: {:?}", val);
                    }
                }
            }
            _ => {
                panic!("Unsupported call type: {:?}", callx);
            }
        }
    }

    pub fn collect_calls_vec<'b>(&mut self, xvec: &'b Vec<Ixpr>)
    {
        for x in xvec.iter() {
            self.collect_calls(x);
        }
    }
}

#[derive(Copy)]
#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
pub enum Depth
{
    Inter,
    One,
    Full,
}

impl Depth
{
    pub fn one_deeper(&self) -> bool
    {
        match self {
            &Depth::Inter => false,
            &Depth::One => true,
            &Depth::Full => true,
        }
    }

    pub fn next(&self) -> Depth
    {
        match self {
            &Depth::One => Depth::Inter,
            &Depth::Full => Depth::Full,
            &Depth::Inter => panic!("cannot get next for Depth::Inter"),
        }
    }
}

/**
 * Typemod
 *
 * Function scoped type checking
 * Type from preface
 * Type from first compilation
 * Type from one deep compilations
 * Type from full depth compilation
 */
#[derive(Debug)]
pub struct Typemod
{
    pub modname: Lstr,
    functions: HashMap<Lstr, Type>,
}

impl Typemod
{
    pub fn new(modname: Lstr) -> Typemod
    {
        Typemod {
            modname,
            functions: HashMap::new(),
        }
    }

    pub fn name(&self) -> &str
    {
        self.modname.str()
    }

    pub fn set_function_type(&mut self, fname: Lstr, typ: Type)
    {
        self.functions.insert(fname, typ);
    }

    pub fn get_function_type(&self, fname: &str) -> Option<&Type>
    {
        self.functions.get(fname)
    }
}

#[derive(Debug)]
pub struct Typescope<'a, 'b>
{
    pub fname: &'b str,
    fri: &'b Lri,
    proto: &'a Protomod,
    inter: &'a Typemod,
    imports: &'a HashMap<Lstr, &'a Typemod>,
    infer: Inferator<'b>,
    typeset: &'a TypeSet<'a>,
}

impl<'a, 'b> Typescope<'a, 'b>
{
    pub fn new(
        inter: &'a Typemod,
        proto: &'a Protomod,
        func: &'b Lri,
        imps: &'a HashMap<Lstr, &'a Typemod>,
        typeset: &'a TypeSet<'a>,
    ) -> Typescope<'a, 'b>
    {
        Typescope {
            fname: &func.localid,
            fri: func,
            proto,
            inter,
            imports: imps,
            infer: Inferator::new(func),
            typeset,
        }
    }

    pub fn is_typevar(&self, t: &Type) -> bool
    {
        match t {
            &Type::Var(_) => true,
            &Type::Unknown => true,
            &Type::UserDef(ref tri) if tri.local_only() => {
                self.infer.is_typevar(&tri.localid)
            }
            // if a UserDef is not local only, it's definitely not a typevar
            &Type::UserDef(ref tri) if tri.has_params() => {
                tri.params
                    .as_ref()
                    .unwrap()
                    .iter()
                    .any(|t| self.is_typevar(t))
            }
            &Type::Tuple(ref items) => {
                items.0.iter().any(|i| self.is_typevar(&i.1))
            }
            &Type::Func(ref ftype) => {
                let is_typevar = |it| self.is_typevar(it);
                ftype.args.iter_v().any(is_typevar)
                    || ftype.closed.iter_v().any(is_typevar)
                    || self.is_typevar(&ftype.result)
            }
            &Type::StrictList(ref inner) => self.is_typevar(inner),
            _ => false,
        }
    }

    pub fn typecheck_matchcase(
        &mut self,
        valtype: &Type,
        case: &mut Ixpr,
    ) -> Lresult<Type>
    {
        // this is_matchcase check is a borrow-checker workaround
        // b/c i had trouble calling typecheck_expr in the
        // else clause b/c case was still held
        // as being borrowed from the if check
        if case.src.is_matchcase() {
            if let &mut Source::MatchCase(
                ref patt,
                ref mut truth,
                ref mut lies,
            ) = &mut case.src
            {
                self.infer.match_pattern(
                    &self.typeset,
                    patt,
                    valtype,
                    case.line,
                )?;
                let ttype = typecheck_expr(self, truth).map_err(|f| {
                    f.add_context(Lstr::from("typecheck_matchcase"))
                })?;
                if lies.src != Source::ConstVal(Val::Void) {
                    let ftype =
                        self.typecheck_matchcase(valtype, lies).unwrap();
                    self.infer.merge_types(&ttype, &ftype)
                } else {
                    Ok(ttype)
                }
            } else {
                panic!("not a matchcase somehow");
            }
        } else {
            typecheck_expr(self, case)
        }
    }

    pub fn typecheck_call_func(&mut self, src: &Source) -> Lresult<Type>
    {
        match src {
            &Source::ConstVal(Val::FuncRef(ref fri, _, ref typ)) => {
                self.typecheck_funcref(fri, typ)
            }
            &Source::Id(ref id, _) => {
                let local_type = self.infer.vartype(id)?;
                if !self.is_typevar(&local_type) {
                    return Ok(local_type.clone());
                }
                Ok(local_type)
            }
            _ => {
                panic!("whatever is that in typecheck_call? {:?}", src);
            }
        }
    }

    /// typecheck a function call with args
    pub fn typecheck_call(
        &mut self,
        func: &mut Ixpr,
        args: &mut Struple<Ixpr>,
    ) -> Lresult<Type>
    {
        let mut targs = vec![];
        for a in &mut args.0 {
            let atype = typecheck_expr(self, &mut a.1).map_err(|e| {
                e.add_context(Lstr::from(format!(
                    "function args for: {:?} on line {}",
                    func.src, func.line
                )))
            })?;
            targs.push(atype);
        }

        match &mut func.src {
            Source::ConstVal(Val::FuncRef(ref mut fri, _, ref mut typ)) => {
                if fri.has_params() {
                    let special_type = match typ {
                        Type::GenericFunc(ref gen_vars, ref mut ft) => {
                            self.specialize_generic_func(ft, gen_vars, &targs)?
                        }
                        Type::SpecialFunc(_, _) => typ.clone(),
                        not_func => {
                            return Err(rustfail!(
                                "type_err",
                                "not a func: {}",
                                not_func
                            ));
                        }
                    };
                    *typ = special_type;
                    if let Type::SpecialFunc(ref mut spec_vars, _) = typ {
                        let new_types =
                            spec_vars.iter_v().map(|t| t.clone()).collect();
                        *fri = fri.specialize_params(new_types)?;
                    }
                } else {
                    // fine, do nothing
                }
            }
            Source::Id(_, _) => {
                // ids are fine, do nothing
            }
            unexpected => {
                println!("unexpected func source: {:?}", unexpected);
                // should probably do something else here to handle
                // function arguments and convert them too
            }
        }

        let tfunc = self.typecheck_call_func(&func.src).map_err(|e| {
            e.add_context(Lstr::from(format!("function: {:?}", func.src)))
        })?;

        let mut targs_ref = vec![];
        for ta in targs.iter() {
            targs_ref.push(ta);
        }
        let full_call_type =
            self.infer.make_call_type(&tfunc, &targs_ref).unwrap();
        let (_, call_result) = Type::split_func_ref(&full_call_type);
        Ok(call_result.clone())
    }

    fn specialize_generic_func(
        &mut self,
        ft: &FuncType,
        gen_args: &Vec<Lstr>,
        arg_types: &Vec<Type>,
    ) -> Lresult<Type>
    {
        let mut var_map: HashMap<Lstr, &Type> = HashMap::new();
        for (var, argtype) in ft.args.iter().zip(arg_types.iter()) {
            match var.v {
                Type::Var(ref varname) => {
                    if var_map.contains_key(varname) {
                        let prev_type = var_map.get(varname).unwrap();
                        self.infer.merge_types(&argtype, prev_type)?;
                    } else {
                        var_map.insert(varname.clone(), argtype);
                    }
                }
                _ => {
                    // not a typevar, ignore it
                }
            }
        }

        let special_args: StrupleKV<Lstr, Type> = gen_args
            .iter()
            .map(|ga| {
                let var_type = var_map.get(ga);
                let new_type =
                    var_type.map(|t| (*t).clone()).unwrap_or(Type::Unknown);
                StrupleItem::new(ga.clone(), new_type)
            })
            .collect();

        let special_ft = ft.map(&|t| Type::replace_typevars(t, &var_map))?;

        Ok(Type::SpecialFunc(special_args, special_ft))
    }

    pub fn typecheck_funcref(&mut self, fri: &Lri, typ: &Type)
        -> Lresult<Type>
    {
        let typed = self.functype(&fri)?;
        let result = self.infer.merge_types(typ, &typed)?;
        match result {
            Type::Func(ref _func_type) => {
                // do nothing here for now
            }
            Type::GenericFunc(ref _var_types, ref _func_type) => {
                // do nothing here for now
            }
            Type::SpecialFunc(ref _var_types, ref _func_type) => {
                // do nothing here for now?
            }
            not_func => {
                return Err(rustfail!(
                    "type_err",
                    "FuncRef type is not a func: {:?}",
                    not_func,
                ));
            }
        }
        Ok(result)
    }

    pub fn functype(&self, fri: &Lri) -> Lresult<Type>
    {
        let modname = fri.mod_ref().unwrap();
        let ftype = if modname == self.inter.name() {
            self.inter
                .get_function_type(&fri.localid)
                .expect("missing typed object for module")
                .clone()
        } else {
            let m = self.imports.get(modname).expect(&format!(
                "cannot find module {} in {:?}",
                modname, self.imports
            ));
            m.get_function_type(&fri.localid)
                .expect(&format!("cannot find function {} in {:?}", fri, m))
                .clone()
        };
        if let Type::Func(ref _i_ftype) = &ftype {
            /*
            if !Lri::nominal_eq(selfri, fri) {
            return Err(TypeErr::Failure(Failure::new(
                "type_error",
                Lstr::from(format!("what to match this Lri to: {}", fri)),
            )));
            */
        }
        Ok(ftype)
    }
}

pub fn typecheck_expr(scope: &mut Typescope, ix: &mut Ixpr) -> Lresult<Type>
{
    match &mut ix.src {
        &mut Source::Call(ref mut func, ref mut args) => {
            scope.typecheck_call(func, args)
        }
        &mut Source::Cons(ref mut head, ref mut tail) => {
            let head_t = typecheck_expr(scope, head).unwrap();
            let tail_t = typecheck_expr(scope, tail).unwrap();
            let head_list_t = Type::StrictList(Box::new(head_t));
            scope.infer.merge_types(&head_list_t, &tail_t)
        }
        &mut Source::ConstVal(Val::FuncRef(ref fri, _, ref typ)) => {
            scope.typecheck_funcref(fri, typ)
        }
        &mut Source::ConstVal(ref val) => Ok(val.get_type()),
        &mut Source::Let(ref lhs, ref ltype, ref mut rhs, ref mut fails) => {
            scope
                .infer
                .match_pattern(&scope.typeset, lhs, ltype, ix.line)?;
            let rhs_type = typecheck_expr(scope, rhs)?;
            scope.infer.match_pattern(
                &scope.typeset,
                lhs,
                &rhs_type,
                ix.line,
            )?;
            for f in fails {
                if f.case.is_none() {
                    continue;
                }
                let recovery_type = scope.typecheck_matchcase(
                    &Type::Hashtag,
                    f.case.as_mut().unwrap(),
                )?;
                // check that whatever type comes out of the
                // failed block matches what was supposed to go into
                // the variable originally
                if recovery_type != Type::Failure {
                    scope.infer.merge_types(&rhs_type, &recovery_type)?;
                }
            }
            Ok(Type::Void)
        }
        &mut Source::Block(ref mut elems) => {
            let mut last_type = Type::Void;
            for e in elems {
                last_type = typecheck_expr(scope, e)?;
            }
            Ok(last_type)
        }
        &mut Source::FieldAccess(ref mut x, ref sub, ref mut subidx) => {
            let xtyp = typecheck_expr(scope, x)?;
            typecheck_field_access(scope, subidx, &xtyp, sub)
        }
        &mut Source::Fork(ref mut x) => {
            let xtyp = typecheck_expr(scope, x)?;
            Ok(Type::future(xtyp))
        }
        &mut Source::Id(ref id, _) => scope.infer.vartype(id),
        &mut Source::List(ref mut items) => {
            let mut last_type = Type::Unknown;
            for i in items {
                let this_type = typecheck_expr(scope, i)?;
                last_type = scope.infer.merge_types(&last_type, &this_type)?;
            }
            Ok(Type::StrictList(Box::new(last_type)))
        }
        &mut Source::Construple(ref typ, _, _) => Ok(typ.clone()),
        &mut Source::Map(_) => Ok(lmap::MAP_TYPE),
        &mut Source::Tuple(ref mut items) => {
            let mut item_types = Vec::with_capacity(items.0.len());
            for i in &mut items.0 {
                let xt = typecheck_expr(scope, &mut i.1)?;
                item_types.push((i.0.clone(), xt));
            }
            Ok(Type::Tuple(Struple(item_types)))
        }
        &mut Source::IfExpr(ref mut cond, ref mut truth, ref mut lies) => {
            let cond_t = typecheck_expr(scope, cond).unwrap();
            scope.infer.merge_types(&cond_t, &Type::Bool)?;

            let truth_result = typecheck_expr(scope, truth);
            match lies {
                &mut None => truth_result,
                &mut Some(ref mut some_lies) => {
                    let lies_result = typecheck_expr(scope, some_lies);
                    let truth_t = truth_result.unwrap();
                    let lies_t = lies_result.unwrap();
                    scope.infer.merge_types(&truth_t, &lies_t)
                }
            }
        }
        &mut Source::StrMash(ref mut items) => {
            for i in items {
                typecheck_expr(scope, i)?;
                // TODO: check that this supports the stringification interface
                // let it = typecheck_expr(scope, i);
                // scope.infer.match_types(&it, &Type::Str);
            }
            Ok(Type::Str)
        }
        &mut Source::MatchExpr(ref mut subject, ref mut cases) => {
            typecheck_expr(scope, subject).and_then(|subject_type| {
                scope.typecheck_matchcase(&subject_type, cases)
            })
        }
        &mut Source::Return(ref mut result) => typecheck_expr(scope, result),
        &mut Source::MatchCase(ref pattern, _, _) => {
            panic!("cannot directly typecheck matchcase: {}", pattern);
        }
        &mut Source::Func(ref _args, _, _, _, ref _body) => {
            Err(rustfail!("leema_fail", "unexpected func in typecheck",))
        }
        src => {
            panic!("could not typecheck_expr({:?})", src);
        }
    }
}

pub fn typecheck_function(scope: &mut Typescope, ix: &mut Ixpr)
    -> Lresult<Type>
{
    let pretype = scope.inter.get_function_type(scope.fname).unwrap();
    match &mut ix.src {
        &mut Source::Func(
            ref arg_names,
            ref closed_vars,
            ref mut arg_types,
            ref mut declared_result_type,
            ref mut body,
        ) => {
            let zips = arg_names.iter().zip(arg_types.iter());
            for (i, (an, at)) in zips.enumerate() {
                scope.infer.init_param(i as i16, an, at, ix.line)?;
            }
            let ci = arg_names.len();
            for an in closed_vars.iter() {
                let at = Type::Var(Lstr::from(format!("T_{}_{}", an, ci)));
                scope.infer.init_param(ci as i16, an, &at, ix.line)?;
            }
            vout!("f({:?}) =>\n{:?}\n", arg_names, body);
            let result_type = typecheck_expr(scope, &mut *body)
                .map_err(|e| {
                    let err_msg = format!(
                        "function result type error in: {}",
                        scope.fname
                    );
                    e.add_context(Lstr::from(err_msg))
                })
                .unwrap();

            vout!("type is: {}\n", result_type);
            vout!("vars:");
            for var in scope.infer.vars() {
                let typ = scope.infer.vartype(var);
                vout!("\t{}: {}\n", var, typ.unwrap());
            }
            let final_args_vec = arg_names
                .iter()
                .zip(arg_types.iter())
                .map(|(an, at)| {
                    let at2 = scope.infer.inferred_type(at).clone();
                    StrupleItem::new(Some(an.clone()), at2)
                })
                .collect();
            let final_args = StrupleKV::from_vec(final_args_vec);
            let final_result = scope
                .infer
                .merge_types(&result_type, declared_result_type)?;
            match pretype {
                Type::Func(_) => Ok(Type::f(final_args, final_result)),
                Type::GenericFunc(ref pre_tvars, _) => {
                    Ok(Type::GenericFunc(
                        pre_tvars.clone(),
                        FuncType::new(final_args, final_result),
                    ))
                }
                Type::SpecialFunc(_, _) => {
                    Err(rustfail!(
                        "leema_incomplete",
                        "cannot typecheck a special func yet",
                    ))
                }
                not_func => {
                    Err(rustfail!(
                        "leema_fail",
                        "pre type is not a func: {}",
                        not_func
                    ))
                }
            }
        }
        &mut Source::RustBlock(ref arg_types, ref result_type) => {
            match *result_type {
                Type::Unknown => {
                    vout!("rust function result types must be known");
                    return Err(rustfail!(
                        "type_err",
                        "unknowable type as rust block result",
                    ));
                }
                _ => {} // everything's fine
            }
            scope.infer.validate_rust_args(arg_types, result_type)?;

            match pretype {
                Type::Func(_) => {
                    Ok(Type::f(arg_types.clone(), result_type.clone()))
                }
                Type::GenericFunc(ref pre_tvars, _) => {
                    Ok(Type::GenericFunc(
                        pre_tvars.clone(),
                        FuncType::new(arg_types.clone(), result_type.clone()),
                    ))
                }
                Type::SpecialFunc(_, _) => {
                    Err(rustfail!(
                        "leema_incomplete",
                        "cannot typecheck a special rust func yet",
                    ))
                }
                not_func => {
                    Err(rustfail!(
                        "leema_fail",
                        "pre type is not a func: {}",
                        not_func
                    ))
                }
            }
        }
        ref mut src => {
            panic!("Cannot typecheck_function a not function: {:?}", src);
        }
    }
    /*
    scope.add_parameters(fix)
    */
}

pub fn typecheck_field_access(
    scope: &mut Typescope,
    fldidx: &mut Option<i8>,
    xtyp: &Type,
    fld: &Lstr,
) -> Lresult<Type>
{
    vout!("typecheck_field_access({:?}.{})\n", xtyp, fld);
    match xtyp {
        &Type::UserDef(ref name) => {
            match scope.typeset.get_typedef(name) {
                Result::Ok(ref ityp) => {
                    ityp.find(fld)
                        .map(|(i, ft)| {
                            *fldidx = Some(i as i8);
                            ft.clone()
                        })
                        .ok_or_else(|| {
                            rustfail!("type_err", "invalid field index")
                        })
                }
                Result::Err(ref e) => {
                    panic!("cannot find defined type for: {}", e);
                }
            }
        }
        _ => {
            panic!("cannot access field for type: {:?}.{}", xtyp, fld);
        }
    }
}


#[cfg(test)]
mod tests
{
    use crate::leema::loader::Interloader;
    use crate::leema::lri::Lri;
    use crate::leema::lstr::Lstr;
    use crate::leema::program;
    use crate::leema::struple::{Struple, StrupleItem, StrupleKV};
    use crate::leema::typecheck::Depth;
    use crate::leema::val::{FuncType, Type};


    #[test]
    #[ignore]
    fn test_pattern_field_inferred()
    {
        let input = r#"
            import map

            enum Foo
            |Object(map::T[Str, Foo])
            |Nothing
            --

            func bar(f: Foo): map::T[Str, Foo]
            >>
                match f
                |Object(flds) >> flds
                |Nothing >> fail(#nothing, "not an object")
                --
            --
            "#
        .to_string();

        let mut loader = Interloader::new(Lstr::Sref("tacos.lma"), "lib");
        loader.set_mod_txt(Lstr::Sref("tacos"), input);
        let mut prog = program::Lib::new(&mut loader);
        let fri = Lri::with_modules(Lstr::from("tacos"), Lstr::from("bar"));
        prog.typecheck(&fri, Depth::Full);
    }

    #[test]
    #[should_panic]
    fn test_pattern_type_inferred_mismatch()
    {
        let input = "
            ## foo should take [#] and return a #
            func foo(inputs) >>
            |[] -> #empty
            |#whatever;more -> #whatever
            |_;more -> foo(more)
            --

            func main() ->
                foo([5, 3, 4])
            --
            "
        .to_string();

        let mut loader = Interloader::new(Lstr::Sref("tacos.lma"), "lib");
        loader.set_mod_txt(Lstr::Sref("tacos"), input);
        let mut prog = program::Lib::new(&mut loader);
        let fri = Lri::with_modules(Lstr::from("tacos"), Lstr::from("main"));
        prog.typecheck(&fri, Depth::Full);
    }

    #[test]
    #[should_panic]
    fn test_let_type_mismatch()
    {
        let input = r#"
            func main() ->
                let x: Str := 1 + 2
                print("$x")
            --
            "#
        .to_string();

        let mut loader = Interloader::new(Lstr::Sref("tacos.lma"), "lib");
        loader.set_mod_txt(Lstr::Sref("tacos"), input);
        let mut prog = program::Lib::new(&mut loader);
        let fri = Lri::with_modules(Lstr::from("tacos"), Lstr::from("main"));
        prog.typecheck(&fri, Depth::Full);
    }

    #[test]
    #[should_panic]
    fn test_typevar_parameter_mismatch()
    {
        let input = r#"
            func swap[T, U](a: T, b: U): (U, T) >>
                (b, a)
            --

            func main() >>
                let (a, b) := swap[Int, #]("x", #y)
                let (c, d) := swap[Int, Str](8, true)
                print("swapped: $a $b $c $d\n")
            --
            "#
        .to_string();

        let mut loader = Interloader::new(Lstr::Sref("tacos.lma"), "lib");
        loader.set_mod_txt(Lstr::Sref("tacos"), input);
        let mut prog = program::Lib::new(&mut loader);
        let fri = Lri::with_modules(Lstr::from("tacos"), Lstr::from("main"));
        prog.typecheck(&fri, Depth::Full);
    }

    #[test]
    #[ignore]
    fn test_typevar_used_two_ways()
    {
        let input = r#"
            func swap[T, U](a: T, b: U): (U, T) >>
                (b, a)
            --

            func main() >>
                let (a, b) := swap[Str, #]("x", #y)
                let (c, d) := swap[Int, Bool](8, true)
                print("swapped: $a $b $c $d\n")
            --
            "#
        .to_string();

        let mut loader = Interloader::new(Lstr::Sref("tacos.lma"), "lib");
        loader.set_mod_txt(Lstr::Sref("tacos"), input);
        let mut prog = program::Lib::new(&mut loader);
        let fri = Lri::with_modules(Lstr::from("tacos"), Lstr::from("main"));
        let main_type = prog.typecheck(&fri, Depth::Full);
        let main_ft = FuncType::new(StrupleKV::none(), Type::Void);
        assert_eq!(Type::Func(main_ft), main_type);

        let swapi = Lri::with_modules(Lstr::from("tacos"), Lstr::from("swap"));
        let swap_type = prog.typecheck(&swapi, Depth::Full);
        let func_vars = vec![Lstr::Sref("T"), Lstr::Sref("U")];
        let func_type = FuncType::new(
            StrupleKV::from_vec(vec![
                StrupleItem::new(
                    Some(Lstr::Sref("a")),
                    Type::Var(Lstr::Sref("T")),
                ),
                StrupleItem::new(
                    Some(Lstr::Sref("b")),
                    Type::Var(Lstr::Sref("U")),
                ),
            ]),
            Type::Tuple(Struple(vec![
                (None, Type::Var(Lstr::Sref("U"))),
                (None, Type::Var(Lstr::Sref("T"))),
            ])),
        );
        assert_eq!(Type::GenericFunc(func_vars, func_type), swap_type);
    }
}

/*
thing that reads files

thing that holds imported files, ntyped functions

view of visible naively, unferred typed stuff

thing that holds complete type info

view of visible type-complete stuff

library of typed code
*/
