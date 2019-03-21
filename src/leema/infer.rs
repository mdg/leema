use crate::leema::failure::Lresult;
use crate::leema::lri::Lri;
use crate::leema::lstr::Lstr;
use crate::leema::struple::{Struple, Struple2, StrupleItem, StrupleKV};
use crate::leema::val::{FuncType, Type, Val};

use std::collections::hash_map::Keys;
use std::collections::HashMap;
use std::iter::FromIterator;


macro_rules! match_err {
    ($a:expr, $b:expr) => {
        Err(rustfail!("type_err", "type mismatch\n   {}\n!= {}", $a, $b,))
    };
}


#[derive(Debug)]
pub struct TypeSet<'b>
{
    typedef: HashMap<Lstr, &'b HashMap<Lstr, Struple<Type>>>,
}

impl<'b> TypeSet<'b>
{
    pub fn new() -> TypeSet<'b>
    {
        TypeSet {
            typedef: HashMap::new(),
        }
    }

    pub fn get_typedef(&self, i: &Lri) -> Lresult<&Struple<Type>>
    {
        // switch all this to use Option/Result combinators
        if !i.has_modules() {
            return Result::Err(rustfail!(
                "leema_fail",
                "cannot get type without including module name: {}",
                i
            ));
        }
        let i_modname = i.modules.as_ref().unwrap();
        let opt_modtypes = self.typedef.get(i_modname);
        if opt_modtypes.is_none() {
            return Err(rustfail!(
                "missing_import",
                "module {} is not imported",
                i_modname
            ));
        }
        opt_modtypes.unwrap().get(&i.localid).ok_or_else(|| {
            rustfail!(
                "undefined_type",
                "cannot find type {} in module {}",
                i.localid,
                i_modname,
            )
        })
    }

    pub fn import_user_types(
        &mut self,
        modname: Lstr,
        types: &'b HashMap<Lstr, Struple<Type>>,
    )
    {
        self.typedef.insert(modname, types);
    }
}

#[derive(Debug)]
pub struct Inferator<'b>
{
    funcname: &'b str,
    fri: &'b Lri,
    typevars: HashMap<&'b Lstr, bool>,
    vartypes: HashMap<Lstr, Type>,
    inferences: HashMap<Lstr, Type>,
}

impl<'b> Inferator<'b>
{
    pub fn new(funcri: &'b Lri) -> Inferator<'b>
    {
        let mut typevars = HashMap::new();
        if funcri.params.is_some() {
            for fp in funcri.params.as_ref().unwrap().iter() {
                let tvar = match fp {
                    &Type::Var(ref vname) => vname,
                    &Type::UserDef(ref vri) if vri.local_only() => &vri.localid,
                    _ => continue,
                };
                typevars.insert(tvar, false);
            }
        }

        Inferator {
            funcname: &funcri.localid,
            fri: funcri,
            typevars,
            vartypes: HashMap::new(),
            inferences: HashMap::new(),
        }
    }

    pub fn vars(&self) -> Keys<Lstr, Type>
    {
        self.vartypes.keys()
    }

    pub fn vartype(&self, argn: &str) -> Lresult<Type>
    {
        match self.vartypes.get(argn) {
            None => {
                Err(rustfail!(
                    "leema_type_fail",
                    "no type for unknown var: {}",
                    argn
                ))
            }
            Some(&Type::Unknown) => {
                Err(rustfail!(
                    "leema_type_fail",
                    "cannot infer unknown type for var: {}",
                    argn
                ))
            }
            Some(ref argt) => Ok(self.inferred_type(argt)),
        }
    }

    pub fn is_typevar(&self, t: &Lstr) -> bool
    {
        self.typevars.contains_key(t)
    }

    /**
     * Are these args valid for a Rust function that has limited
     * ability to infer data types and needs more specific type inputs
     */
    pub fn validate_rust_args(
        &mut self,
        args: &Struple2<Type>,
        result: &Type,
    ) -> Lresult<Type>
    {
        if self.typevars.is_empty() {
            // if there are no type parameters, then this should be fine
            return Ok(Type::Void);
        }

        for a in args.iter_v() {
            self.mark_used_typevars(a)?;
        }
        self.mark_used_typevars(result)?;
        for tv in self.typevars.values() {
            if !tv {
                vout!("rust function type params must be used in arguments\n");
                return Err(rustfail!(
                    "unknowable_type",
                    "rust function type params must be used in arguments",
                ));
            }
        }
        Ok(Type::Void)
    }

    pub fn mark_used_typevars(&mut self, arg: &Type) -> Lresult<Type>
    {
        match arg {
            &Type::Var(ref vname) => {
                let mv = self.typevars.get_mut(vname);
                if mv.is_none() {
                    Err(rustfail!("type_error", "undefined type: {}", vname))
                } else {
                    *mv.unwrap() = true;
                    Ok(Type::Void)
                }
            }
            &Type::Unknown => {
                vout!("rust typevars must be identifiable");
                Err(rustfail!(
                    "unknowable_type",
                    "rust typevars must be identifiable",
                ))
            }
            &Type::UserDef(ref tri) if tri.local_only() => {
                let mv = self.typevars.get_mut(&tri.localid);
                if mv.is_none() {
                    Err(rustfail!("type_error", "undefined type: {}", tri))
                } else {
                    *mv.unwrap() = true;
                    Ok(Type::Void)
                }
            }
            // if a UserDef is not local only, it's definitely not a typevar
            &Type::UserDef(ref tri) if tri.has_params() => {
                for p in tri.params.as_ref().unwrap().iter() {
                    self.mark_used_typevars(p)?;
                }
                Ok(Type::Void)
            }
            &Type::Tuple(ref items) => {
                for i in items.0.iter() {
                    self.mark_used_typevars(&i.1)?;
                }
                Ok(Type::Void)
            }
            &Type::Func(ref ftype) => {
                for a in ftype.args.iter_v() {
                    self.mark_used_typevars(a)?;
                }
                for c in ftype.closed.iter_v() {
                    self.mark_used_typevars(c)?;
                }
                self.mark_used_typevars(&ftype.result)
            }
            &Type::StrictList(ref inner) => self.mark_used_typevars(inner),
            _ => Ok(Type::Void),
        }
    }

    pub fn init_param(
        &mut self,
        argi: i16,
        argn: &Lstr,
        argt: &Type,
        line: i16,
    ) -> Lresult<Type>
    {
        vout!(
            "init_param({}, #{} {:?}: {:?} #{})\n",
            self.funcname,
            argi,
            argn,
            argt,
            line,
        );

        let realt = match argt {
            Type::Unknown => Type::new_var(argn),
            _ => argt.clone(),
        };
        if self.vartypes.contains_key(argn.str()) {
            let oldargt = self.vartypes.get(argn).unwrap();
            return Inferator::mash(
                &mut self.inferences,
                &self.typevars,
                oldargt,
                &realt,
            );
        }

        self.vartypes.insert(argn.clone(), realt.clone());
        Ok(realt)
    }

    pub fn bind_vartype(
        &mut self,
        argn: &Lstr,
        argt: &Type,
        line: i16,
    ) -> Lresult<Type>
    {
        vout!(
            "bind_vartype({}, {}: {:?}, {})\n",
            self.funcname,
            argn,
            argt,
            line
        );

        let realt = match argt {
            &Type::Unknown => Type::new_var(argn),
            _ => argt.clone(),
        };
        if !self.vartypes.contains_key(argn) {
            self.vartypes.insert(argn.clone(), realt.clone());
            return Ok(realt);
        }

        let oldargt = self.vartypes.get(argn).unwrap();
        Inferator::mash(&mut self.inferences, &self.typevars, oldargt, &realt)
    }

    pub fn merge_types(&mut self, a: &Type, b: &Type) -> Lresult<Type>
    {
        Inferator::mash(&mut self.inferences, &self.typevars, a, b)
    }

    pub fn match_pattern(
        &mut self,
        typeset: &TypeSet,
        patt: &Val,
        valtype: &Type,
        lineno: i16,
    ) -> Lresult<Type>
    {
        match (patt, valtype) {
            (&Val::Id(ref id), _) => self.bind_vartype(id, valtype, lineno),
            (&Val::Wildcard, _) => Ok(Type::Unknown),
            (&Val::Nil, _) => {
                self.merge_types(
                    &Type::StrictList(Box::new(Type::Unknown)),
                    valtype,
                )
            }
            (&Val::Cons(_, _), &Type::StrictList(ref subt)) => {
                self.match_list_pattern(typeset, patt, subt, lineno)
            }
            (&Val::Cons(_, _), &Type::Var(ref tvar_name)) => {
                let tvar_inner_name = format!("{}_inner", tvar_name);
                let tvar_inner = Type::Var(Lstr::from(tvar_inner_name));
                self.match_list_pattern(typeset, patt, &tvar_inner, lineno)?;
                self.merge_types(
                    &valtype,
                    &Type::StrictList(Box::new(tvar_inner.clone())),
                )
            }
            (&Val::Tuple(ref flds1), &Type::Tuple(ref item_types)) => {
                if flds1.0.len() != item_types.0.len() {
                    panic!(
                        "pattern tuple size mismatch: {} != {}",
                        patt, valtype
                    );
                }
                let mut tfld_types = vec![];
                for (fp, ft) in flds1.0.iter().zip(item_types.0.iter()) {
                    tfld_types.push((
                        ft.0.clone(),
                        self.match_pattern(typeset, &fp.1, &ft.1, lineno)?,
                    ));
                }
                Ok(Type::Tuple(Struple(tfld_types)))
            }
            (
                &Val::Struct(ref typ1, ref flds1),
                &Type::UserDef(ref typename2),
            ) => {
                if typ1 != typename2 {
                    panic!(
                        "struct type mismatch: {:?} != {:?}",
                        typ1, typename2
                    );
                }
                let flds2 = match typeset.get_typedef(typename2) {
                    Result::Err(e) => {
                        panic!("{}", e);
                    }
                    Result::Ok(r_type_struple) => r_type_struple,
                };
                if flds1.0.len() > flds2.0.len() {
                    panic!("too many fields in pattern for: {}", typ1);
                }
                let mut tparams = vec![];
                for (fp, ft) in flds1.0.iter().zip(flds2.0.iter()) {
                    tparams.push(
                        self.match_pattern(typeset, &fp.1, &ft.1, lineno)?,
                    );
                }
                Ok(Type::UserDef(typ1.replace_params(tparams)))
            }
            (
                &Val::EnumStruct(ref typ1, ref var1, ref flds1),
                &Type::UserDef(ref typename2),
            ) => {
                if !Lri::nominal_eq(typ1, typename2) {
                    return match_err!(Type::UserDef(typ1.clone()), valtype);
                }
                let new_type =
                    if typ1.params.is_some() && typename2.params.is_some() {
                        let iter1 = typ1.params.as_ref().unwrap().iter();
                        let iter2 = typename2.params.as_ref().unwrap().iter();
                        let mut tparams = vec![];
                        for (t1, t2) in iter1.zip(iter2) {
                            tparams.push(self.merge_types(t1, t2)?);
                        }
                        typ1.replace_params(tparams)
                    } else {
                        typ1.clone()
                    };

                // look at the struct fields
                let variant_lri = new_type.replace_local(var1.clone());
                let flds2 = match typeset.get_typedef(&variant_lri) {
                    Result::Err(e) => {
                        panic!("{}", e);
                    }
                    Result::Ok(r_type_struple) => r_type_struple,
                };
                if flds1.0.len() > flds2.0.len() {
                    panic!("too many fields in pattern for: {}", new_type);
                }
                for (fp, ft) in flds1.0.iter().zip(flds2.0.iter()) {
                    self.match_pattern(typeset, &fp.1, &ft.1, lineno)?;
                }
                Ok(Type::UserDef(new_type))
            }
            (
                &Val::EnumToken(ref typ1, ref _var1),
                &Type::UserDef(ref typename2),
            ) => {
                if !Lri::nominal_eq(typ1, typename2) {
                    return match_err!(Type::UserDef(typ1.clone()), valtype);
                }
                if typ1.params.is_none() && typename2.params.is_none() {
                    return Ok(valtype.clone());
                }
                let iter1 = typ1.params.as_ref().unwrap().iter();
                let iter2 = typename2.params.as_ref().unwrap().iter();
                let mut tparams = vec![];
                for (t1, t2) in iter1.zip(iter2) {
                    tparams.push(self.merge_types(t1, t2)?);
                }
                Ok(Type::UserDef(typ1.replace_params(tparams)))
            }
            (&Val::Token(ref typ1), &Type::UserDef(ref typename2)) => {
                if typ1 != typename2 {
                    Err(rustfail!(
                        "type_err",
                        "token type mismatch: {:?} != {:?}",
                        typ1,
                        typename2,
                    ))
                } else {
                    Ok(valtype.clone())
                }
            }
            _ => {
                let ptype = patt.get_type();
                self.merge_types(&ptype, valtype).map_err(|e| {
                    e.add_context(lstrf!(
                        "pattern type mismatch: {:?} != {:?}",
                        patt,
                        valtype
                    ))
                })
            }
        }
    }

    pub fn match_list_pattern(
        &mut self,
        typeset: &TypeSet,
        l: &Val,
        inner_type: &Type,
        lineno: i16,
    ) -> Lresult<Type>
    {
        match l {
            &Val::Cons(ref head, ref tail) => {
                self.match_pattern(typeset, head, inner_type, lineno)?;
                self.match_list_pattern(typeset, tail, inner_type, lineno)
            }
            &Val::Id(ref idname) => {
                let ltype = Type::StrictList(Box::new(inner_type.clone()));
                self.bind_vartype(idname, &ltype, lineno)
            }
            &Val::Nil => Ok(Type::StrictList(Box::new(Type::Unknown))),
            &Val::Wildcard => Ok(Type::Unknown),
            _ => {
                Err(rustfail!(
                    "type_err",
                    "match_list_pattern on not a list: {:?}",
                    l,
                ))
            }
        }
    }

    pub fn inferred_type<'a>(&'a self, typ: &'a Type) -> Type
    {
        match typ {
            &Type::Var(ref varname) => {
                match self.inferences.get(&**varname) {
                    Some(ref other_type) => self.inferred_type(other_type),
                    None => typ.clone(),
                }
            }
            &Type::StrictList(ref inner) => {
                Type::StrictList(Box::new(self.inferred_type(inner)))
            }
            &Type::Tuple(ref inners) => {
                let infers = inners
                    .0
                    .iter()
                    .map(|i| (i.0.clone(), self.inferred_type(&i.1)))
                    .collect();
                Type::Tuple(infers)
            }
            &Type::UserDef(ref udlri) if udlri.params.is_some() => {
                let iparams = udlri
                    .params
                    .as_ref()
                    .unwrap()
                    .iter()
                    .map(|p| self.inferred_type(&p))
                    .collect();
                Type::UserDef(udlri.replace_params(iparams))
            }
            _ => typ.clone(),
        }
    }

    fn mash(
        inferences: &mut HashMap<Lstr, Type>,
        typevars: &HashMap<&Lstr, bool>,
        oldt: &Type,
        newt: &Type,
    ) -> Lresult<Type>
    {
        if oldt == newt {
            // all good
            return Ok(oldt.clone());
        }

        vout!("mash({:?}, {:?}) where {:?}\n", oldt, newt, typevars);
        let mtype: Lresult<Type> = match (oldt, newt) {
            // anything is better than Unknown
            (&Type::Unknown, _) => Ok(newt.clone()),
            (_, &Type::Unknown) => Ok(oldt.clone()),
            // failures aren't typechecked, keep the other type
            (&Type::Failure, _) => Ok(newt.clone()),
            (_, &Type::Failure) => Ok(oldt.clone()),
            // handle variables
            (&Type::Var(ref oldtname), &Type::Var(ref newtname)) => {
                if oldtname < newtname {
                    inferences.insert(newtname.clone(), oldt.clone());
                    Ok(oldt.clone())
                } else {
                    inferences.insert(oldtname.clone(), newt.clone());
                    Ok(newt.clone())
                }
            }
            (&Type::StrictList(ref oldit), &Type::StrictList(ref newit)) => {
                Inferator::mash(inferences, typevars, oldit, newit)
                    .and_then(|t| Ok(Type::StrictList(Box::new(t))))
            }
            (&Type::Var(ref oldtname), _) => {
                inferences.insert(oldtname.clone(), newt.clone());
                Ok(newt.clone())
            }
            (_, &Type::Var(ref newtname)) => {
                inferences.insert(newtname.clone(), oldt.clone());
                Ok(oldt.clone())
            }
            (&Type::UserDef(ref oldtname), _)
                if oldtname.local_only()
                    && typevars.contains_key(&oldtname.localid) =>
            {
                inferences.insert(oldtname.localid.clone(), newt.clone());
                Ok(newt.clone())
            }
            (_, &Type::UserDef(ref newtname))
                if newtname.local_only()
                    && typevars.contains_key(&newtname.localid) =>
            {
                inferences.insert(newtname.localid.clone(), oldt.clone());
                Ok(oldt.clone())
            }
            (&Type::Func(ref oldftype), &Type::Func(ref newftype)) => {
                let mashftype = Inferator::mash_ft(
                    inferences, typevars, oldftype, newftype,
                )?;
                Ok(Type::Func(mashftype))
            }
            (&Type::SpecialFunc(_, _), &Type::GenericFunc(_, _)) => {
                Inferator::mash(inferences, typevars, newt, oldt)
            }
            (
                &Type::GenericFunc(ref argnames, ref genftype),
                &Type::SpecialFunc(ref argtypes, ref specftype),
            ) => {
                let genlen = argnames.len();
                let speclen = argtypes.len();
                if genlen != speclen {
                    return match_err!(oldt, newt);
                }
                let mut tparams = HashMap::new();
                for (argn, argt) in argnames.iter().zip(argtypes.iter()) {
                    if *argn != argt.k {
                        return match_err!(oldt, newt);
                    }
                    tparams.insert(argn.clone(), &argt.v);
                }
                let replaced_genft =
                    genftype.map(&|t: &Type| t.replace_typevars(&tparams))?;
                let mashed_ft = Inferator::mash_ft(
                    inferences,
                    typevars,
                    &replaced_genft,
                    specftype,
                )?;
                Ok(Type::SpecialFunc(argtypes.clone(), mashed_ft))
            }
            (&Type::Tuple(ref olditems), &Type::Tuple(ref newitems)) => {
                let oldlen = olditems.0.len();
                let newlen = newitems.0.len();
                if oldlen != newlen {
                    return match_err!(oldt, newt);
                }
                let mut mashitems = Vec::with_capacity(oldlen);
                for (oi, ni) in olditems.0.iter().zip(newitems.0.iter()) {
                    let mi =
                        Inferator::mash(inferences, typevars, &oi.1, &ni.1)?;
                    mashitems.push((None, mi));
                }
                Ok(Type::Tuple(Struple(mashitems)))
            }
            (&Type::UserDef(ref oldlri), &Type::UserDef(ref newlri)) => {
                if oldlri.modules != newlri.modules {
                    return match_err!(oldt, newt);
                }
                if oldlri.localid != newlri.localid {
                    return match_err!(oldt, newt);
                }
                if oldlri.params.is_none() || oldlri.params.is_none() {
                    return match_err!(oldt, newt);
                }
                let oldparams = oldlri.params.as_ref().unwrap();
                let newparams = newlri.params.as_ref().unwrap();
                let oldlen = oldparams.len();
                if oldlen != newparams.len() {
                    return match_err!(oldt, newt);
                }
                let mut mashparams = Vec::with_capacity(oldlen);
                for (op, np) in oldparams.iter().zip(newparams.iter()) {
                    let mp = Inferator::mash(inferences, typevars, &op, &np)?;
                    mashparams.push(mp);
                }
                let mashlri = Lri::full(
                    oldlri.modules.clone(),
                    oldlri.localid.clone(),
                    Some(mashparams),
                );
                Ok(Type::UserDef(mashlri))
            }
            (_, _) => match_err!(oldt, newt),
        };
        vout!("\tmashed -> {:?}\n", mtype);
        mtype
    }

    fn mash_ft(
        inferences: &mut HashMap<Lstr, Type>,
        typevars: &HashMap<&Lstr, bool>,
        oldt: &FuncType,
        newt: &FuncType,
    ) -> Lresult<FuncType>
    {
        let oldlen = oldt.args.len();
        let newlen = newt.args.len();
        if oldlen != newlen {
            return match_err!(oldt, newt);
        }
        let mut masht = Vec::with_capacity(oldlen);
        let mashit = oldt.args.iter().zip(newt.args.iter());
        for (oldit, newit) in mashit {
            let mashit =
                Inferator::mash(inferences, typevars, &oldit.v, &newit.v)?;
            masht.push(StrupleItem::new(oldit.k.clone(), mashit));
        }

        let mashresult =
            Inferator::mash(inferences, typevars, &oldt.result, &newt.result)?;
        Ok(FuncType::new(StrupleKV::from(masht), mashresult))
    }


    pub fn make_call_type(
        &mut self,
        ftype: &Type,
        argst: &Vec<&Type>,
    ) -> Lresult<Type>
    {
        vout!("make_call_type({}, {:?})\n", ftype, argst);
        let (defargst, defresult) = Type::split_func_ref(ftype);

        let defargslen = defargst.len();
        let argslen = argst.len();
        if argslen > defargslen {
            panic!("too many args passed to {:?}: {:?}", ftype, argst);
        }
        if argslen < defargslen {
            return Err(rustfail!(
                "type_error",
                "passed too few args to {}",
                ftype
            ));
        }

        let mash_result: Vec<Lresult<StrupleItem<Option<Lstr>, Type>>>;
        mash_result = defargst
            .iter()
            .zip(argst.iter())
            .map(|(defargt, argt)| {
                let mash_arg = Inferator::mash(
                    &mut self.inferences,
                    &self.typevars,
                    &defargt.v,
                    argt,
                )
                .map_err(|e| {
                    e.add_context(Lstr::from(format!(
                        "expected function args in {}: {:?} found {:?}",
                        self.funcname, defargst, argst,
                    )))
                })?;
                Ok(StrupleItem::new(defargt.k.clone(), mash_arg))
            })
            .collect();
        let mashed_args = Result::from_iter(mash_result)?;
        Ok(Type::f(mashed_args, self.inferred_type(defresult)))
    }
}


#[cfg(test)]
mod tests
{
    use crate::leema::infer::{Inferator, TypeSet};
    use crate::leema::list;
    use crate::leema::lri::Lri;
    use crate::leema::lstr::Lstr;
    use crate::leema::struple::{Struple, StrupleKV};
    use crate::leema::val::{Type, Val};

    use std::collections::HashMap;


    #[test]
    fn test_add_and_find()
    {
        let fri = Lri::new(Lstr::Sref("burritos"));
        let mut t = Inferator::new(&fri);
        t.bind_vartype(&Lstr::Sref("a"), &Type::Int, 18).unwrap();
        assert_eq!(Type::Int, t.vartype("a").unwrap());
    }

    #[test]
    fn test_merge_strict_list_unknown()
    {
        let fri = Lri::new(Lstr::Sref("burritos"));
        let mut t = Inferator::new(&fri);
        let mtype = t.merge_types(
            &Type::StrictList(Box::new(Type::Unknown)),
            &Type::StrictList(Box::new(Type::Int)),
        );

        assert_eq!(Ok(Type::StrictList(Box::new(Type::Int))), mtype);
    }

    #[test]
    fn test_merge_types_via_tvar()
    {
        let fri = Lri::new(Lstr::Sref("burritos"));
        let mut t = Inferator::new(&fri);
        let intlist = Type::StrictList(Box::new(Type::Int));
        let unknownlist = Type::StrictList(Box::new(Type::Unknown));
        let tvar = Type::Var(Lstr::Sref("Taco"));

        let mtype0 = t.merge_types(&unknownlist, &tvar);
        assert_eq!(Ok(unknownlist), mtype0);

        let mtype1 = t.merge_types(&intlist, &tvar);
        assert_eq!(Ok(intlist), mtype1);
    }

    #[test]
    fn test_make_call_type_with_vars()
    {
        let fri = Lri::new(Lstr::Sref("burritos"));
        let mut t = Inferator::new(&fri);
        let defargst = Type::f(
            StrupleKV::from(vec![Type::Var(Lstr::from("A")), Type::Int]),
            Type::Var(Lstr::from("A")),
        );
        let argvalt = vec![&Type::Hashtag, &Type::Int];

        let mct = t.make_call_type(&defargst, &argvalt).unwrap();

        let (func_args, func_result) = Type::split_func_ref(&mct);
        assert_eq!(2, func_args.len());
        assert_eq!(Type::Hashtag, func_args[0].v);
        assert_eq!(Type::Int, func_args[1].v);
        assert_eq!(Type::Hashtag, *func_result);
    }

    #[test]
    fn test_match_pattern_empty_list()
    {
        let fri = Lri::new(Lstr::Sref("burritos"));
        let mut t = Inferator::new(&fri);
        let tvar = Type::Var(Lstr::Sref("Taco"));
        let ts = TypeSet::new();
        t.match_pattern(&ts, &Val::Nil, &tvar, 55).unwrap();

        assert_eq!(
            Type::StrictList(Box::new(Type::Unknown)),
            t.inferred_type(&tvar)
        );
    }

    #[test]
    fn test_match_pattern_empty_and_full_lists()
    {
        let fri = Lri::new(Lstr::Sref("burritos"));
        let mut t = Inferator::new(&fri);
        let tvar = Type::Var(Lstr::Sref("Taco"));
        let ts = TypeSet::new();
        t.match_pattern(&ts, &Val::Nil, &tvar, 32).unwrap();
        t.match_pattern(&ts, &list::singleton(Val::Int(5)), &tvar, 99)
            .unwrap();

        assert_eq!(
            Type::StrictList(Box::new(Type::Int)),
            t.inferred_type(&tvar)
        );
    }

    #[test]
    fn test_match_pattern_hashtag_list_inside_tuple()
    {
        let fri = Lri::new(Lstr::Sref("burritos"));
        let mut t = Inferator::new(&fri);
        let tvar =
            Type::Tuple(Struple(vec![(None, Type::Var(Lstr::Sref("Taco")))]));
        let ilistpatt = list::cons(
            Val::Hashtag(Lstr::Sref("leema")),
            Val::Id(Lstr::Sref("tail")),
        );
        let listpatt = Val::Tuple(Struple(vec![(None, ilistpatt)]));
        let ts = TypeSet::new();
        t.match_pattern(&ts, &listpatt, &tvar, 14).unwrap();

        let exp = Type::Tuple(Struple(vec![(
            None,
            Type::StrictList(Box::new(Type::Hashtag)),
        )]));
        assert_eq!(exp, t.inferred_type(&tvar));
    }

    #[test]
    #[should_panic]
    fn test_match_pattern_tuple_size_mismatch()
    {
        let fri = Lri::new(Lstr::Sref("burritos"));
        let mut t = Inferator::new(&fri);
        let tvar =
            Type::Tuple(Struple(vec![(None, Type::Var(Lstr::Sref("Taco")))]));
        let listpatt = Val::Tuple(Struple::new_tuple2(
            Val::Hashtag(Lstr::Sref("leema")),
            Val::Id(Lstr::Sref("tail")),
        ));
        let ts = TypeSet::new();
        t.match_pattern(&ts, &listpatt, &tvar, 14).unwrap();
    }

    #[test]
    fn test_mash_tuples_containing_vars()
    {
        let mut inferences = HashMap::new();
        let typevars = HashMap::new();
        let oldt = Type::Tuple(Struple(vec![
            (None, Type::Var(Lstr::Sref("A"))),
            (None, Type::Var(Lstr::Sref("B"))),
        ]));
        let newt =
            Type::Tuple(Struple(vec![(None, Type::Int), (None, Type::Str)]));

        let result = Inferator::mash(&mut inferences, &typevars, &oldt, &newt);
        result.unwrap();
    }

    #[test]
    fn test_mash_userdefs_containing_vars()
    {
        let mut inferences = HashMap::new();
        let tvars = HashMap::new();
        let oldt = Type::UserDef(Lri::full(
            Some(Lstr::from("option")),
            Lstr::from("T"),
            Some(vec![Type::Var(Lstr::from("A"))]),
        ));
        let newt = Type::UserDef(Lri::full(
            Some(Lstr::from("option")),
            Lstr::from("T"),
            Some(vec![Type::Int]),
        ));

        let result = Inferator::mash(&mut inferences, &tvars, &oldt, &newt);
        result.unwrap();
    }

}
