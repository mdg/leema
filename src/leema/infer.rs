use leema::lri::Lri;
use leema::lstr::Lstr;
use leema::struple::Struple;
use leema::val::{Type, TypeErr, TypeResult, Val};

use std::collections::hash_map::Keys;
use std::collections::{HashMap, HashSet};


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

    pub fn get_typedef(&self, i: &Lri) -> Result<&Struple<Type>, TypeErr>
    {
        // switch all this to use Option/Result combinators
        if !i.has_modules() {
            return Result::Err(TypeErr::Error(Lstr::from(format!(
                "cannot get type without including module name: {}",
                i
            ))));
        }
        let i_modname = i.modules.as_ref().unwrap();
        let opt_modtypes = self.typedef.get(i_modname);
        if opt_modtypes.is_none() {
            return Result::Err(TypeErr::Error(Lstr::from(format!(
                "module {} is not imported",
                i_modname
            ))));
        }
        opt_modtypes.unwrap().get(&i.localid).ok_or_else(|| {
            TypeErr::Error(Lstr::from(format!(
                "cannot find type {} in module {}",
                i.localid, i_modname
            )))
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
    typevars: HashSet<Lstr>,
    vartypes: HashMap<Lstr, Type>,
    inferences: HashMap<Lstr, Type>,
}

impl<'b> Inferator<'b>
{
    pub fn new(funcname: &'b str) -> Inferator<'b>
    {
        Inferator {
            funcname,
            typevars: HashSet::new(),
            vartypes: HashMap::new(),
            inferences: HashMap::new(),
        }
    }

    pub fn vars(&self) -> Keys<Lstr, Type>
    {
        self.vartypes.keys()
    }

    pub fn vartype(&self, argn: &str) -> TypeResult
    {
        match self.vartypes.get(argn) {
            None => {
                Err(TypeErr::Error(Lstr::from(format!(
                    "no type for unknown var: {}",
                    argn
                ))))
            }
            Some(&Type::AnonVar) => {
                Err(TypeErr::Error(Lstr::from(format!(
                    "cannot infer AnonVar for var: {}",
                    argn
                ))))
            }
            Some(ref argt) => Ok(self.inferred_type(argt)),
        }
    }

    pub fn is_typevar(&self, t: &Lstr) -> bool
    {
        self.typevars.contains(t)
    }

    pub fn init_param(
        &mut self,
        argi: i16,
        argn: &Lstr,
        argt: &Type,
        line: i16,
    ) -> TypeResult
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
            Type::Unknown => {
                let arg_typename = format!("T_param_{}", argi);
                Type::Var(Lstr::from(arg_typename))
            }
            Type::AnonVar => {
                let arg_typename = format!("T_param_{}", argi);
                Type::Var(Lstr::from(arg_typename))
            }
            _ => argt.clone(),
        };
        if self.vartypes.contains_key(argn.str()) {
            let oldargt = self.vartypes.get(argn).unwrap();
            return Inferator::mash(&mut self.inferences, oldargt, &realt);
        }

        self.vartypes.insert(argn.clone(), realt.clone());
        Ok(realt)
    }

    pub fn bind_vartype(
        &mut self,
        argn: &Lstr,
        argt: &Type,
        line: i16,
    ) -> TypeResult
    {
        vout!(
            "bind_vartype({}, {}: {:?}, {})\n",
            self.funcname,
            argn,
            argt,
            line
        );

        let realt = match argt {
            &Type::Unknown => {
                let arg_typename = format!("T_local_{}", argn);
                Type::Var(Lstr::from(arg_typename))
            }
            &Type::AnonVar => {
                panic!("cannot bind var to anonymous type: {}", argn);
            }
            _ => argt.clone(),
        };
        if !self.vartypes.contains_key(argn) {
            self.vartypes.insert(argn.clone(), realt.clone());
            return Ok(realt);
        }

        let oldargt = self.vartypes.get(argn).unwrap();
        Inferator::mash(&mut self.inferences, oldargt, &realt)
    }

    pub fn merge_types(&mut self, a: &Type, b: &Type) -> TypeResult
    {
        Inferator::mash(&mut self.inferences, a, b)
    }

    pub fn match_pattern(
        &mut self,
        typeset: &TypeSet,
        patt: &Val,
        valtype: &Type,
        lineno: i16,
    ) -> Result<(), TypeErr>
    {
        match (patt, valtype) {
            (_, &Type::AnonVar) => {
                panic!("pattern value type cannot be anonymous: {:?}", patt);
            }
            (&Val::Id(ref id), _) => {
                self.bind_vartype(id, valtype, lineno).map(|_| ())
            }
            (&Val::Wildcard, _) => Ok(()),
            (&Val::Nil, _) => {
                self.merge_types(
                    &Type::StrictList(Box::new(Type::Unknown)),
                    valtype,
                )
                .map(|_| ())
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
                .map(|_| ())
            }
            (&Val::Tuple(ref flds1), &Type::Tuple(ref item_types)) => {
                if flds1.0.len() != item_types.0.len() {
                    panic!(
                        "pattern tuple size mismatch: {} != {}",
                        patt, valtype
                    );
                }
                for (fp, ft) in flds1.0.iter().zip(item_types.0.iter()) {
                    self.match_pattern(typeset, &fp.1, &ft.1, lineno)?;
                }
                Ok(())
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
                for (fp, ft) in flds1.0.iter().zip(flds2.0.iter()) {
                    self.match_pattern(typeset, &fp.1, &ft.1, lineno)?;
                }
                Ok(())
            }
            (
                &Val::EnumStruct(ref typ1, ref _var1, ref _flds1),
                &Type::UserDef(ref typename2),
            ) => {
                if typ1 != typename2 {
                    panic!(
                        "enum struct type mismatch: {:?} != {:?}",
                        typ1, typename2
                    );
                }
                Ok(())
            }
            (
                &Val::EnumToken(ref typ1, ref _var1),
                &Type::UserDef(ref typename2),
            ) => {
                if typ1 != typename2 {
                    panic!(
                        "enum token type mismatch: {:?} != {:?}",
                        typ1, typename2
                    );
                }
                Ok(())
            }
            (&Val::Token(ref typ1), &Type::UserDef(ref typename2)) => {
                if typ1 != typename2 {
                    panic!(
                        "token type mismatch: {:?} != {:?}",
                        typ1, typename2
                    );
                }
                Ok(())
            }
            _ => {
                let ptype = patt.get_type();
                self.merge_types(&ptype, valtype).map(|_| ()).map_err(|e| {
                    e.add_context(Lstr::from(format!(
                        "pattern type mismatch: {:?} != {:?}",
                        patt, valtype
                    )))
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
    ) -> Result<(), TypeErr>
    {
        match l {
            &Val::Cons(ref head, ref tail) => {
                self.match_pattern(typeset, head, inner_type, lineno)?;
                self.match_list_pattern(typeset, tail, inner_type, lineno)
            }
            &Val::Id(ref idname) => {
                let ltype = Type::StrictList(Box::new(inner_type.clone()));
                self.bind_vartype(idname, &ltype, lineno).map(|_| ())
            }
            &Val::Nil => Ok(()),
            &Val::Wildcard => Ok(()),
            _ => {
                panic!("match_list_pattern on not a list: {:?}", l);
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
        oldt: &Type,
        newt: &Type,
    ) -> TypeResult
    {
        if oldt == newt {
            // all good
            return Ok(oldt.clone());
        }
        let match_err = || Err(TypeErr::Mismatch(oldt.clone(), newt.clone()));

        vout!("mash({:?}, {:?})\n", oldt, newt);
        let mtype: TypeResult = match (oldt, newt) {
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
                Inferator::mash(inferences, oldit, newit)
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
            (
                &Type::Func(ref oldargs, ref oldresult),
                &Type::Func(ref newargs, ref newresult),
            ) => {
                let oldlen = oldargs.len();
                let newlen = newargs.len();
                if oldlen != newlen {
                    return match_err();
                }
                let mut masht = Vec::with_capacity(oldlen);
                for (oldit, newit) in oldargs.iter().zip(newargs.iter()) {
                    let mashit = Inferator::mash(inferences, oldit, newit)?;
                    masht.push(mashit);
                }
                let mashresult =
                    Inferator::mash(inferences, oldresult, newresult)?;
                Ok(Type::Func(masht, Box::new(mashresult)))
            }
            (
                &Type::Closure(ref oldargs, ref oldclosed, ref oldresult),
                &Type::Closure(ref newargs, ref newclosed, ref newresult),
            ) => {
                let oldlen = oldargs.len();
                let newlen = newargs.len();
                if oldlen != newlen {
                    return match_err();
                }
                let coldlen = oldclosed.len();
                let cnewlen = newclosed.len();
                if coldlen != cnewlen {
                    return match_err();
                }
                let mut masht = Vec::with_capacity(oldlen);
                for (oldit, newit) in oldargs.iter().zip(newargs.iter()) {
                    let mashit = Inferator::mash(inferences, oldit, newit)?;
                    masht.push(mashit);
                }
                let mut mashclosed = Vec::with_capacity(oldlen);
                for (oldit, newit) in oldclosed.iter().zip(newclosed.iter()) {
                    let mashit = Inferator::mash(inferences, oldit, newit)?;
                    mashclosed.push(mashit);
                }
                let mashresult =
                    Inferator::mash(inferences, oldresult, newresult)?;
                Ok(Type::Closure(masht, mashclosed, Box::new(mashresult)))
            }
            (
                &Type::Func(ref oldargs, ref oldresult),
                &Type::Closure(ref newargs, _, ref newresult),
            ) => {
                let oldlen = oldargs.len();
                let newlen = newargs.len();
                if oldlen != newlen {
                    return match_err();
                }
                let mut masht = Vec::with_capacity(oldlen);
                for (oldit, newit) in oldargs.iter().zip(newargs.iter()) {
                    let mashit = Inferator::mash(inferences, oldit, newit)?;
                    masht.push(mashit);
                }
                let mashresult =
                    Inferator::mash(inferences, oldresult, newresult)?;
                Ok(Type::Func(masht, Box::new(mashresult)))
            }
            (
                &Type::Closure(ref oldargs, _, ref oldresult),
                &Type::Func(ref newargs, ref newresult),
            ) => {
                let oldlen = oldargs.len();
                let newlen = newargs.len();
                if oldlen != newlen {
                    return match_err();
                }
                let mut masht = Vec::with_capacity(oldlen);
                for (oldit, newit) in oldargs.iter().zip(newargs.iter()) {
                    let mashit = Inferator::mash(inferences, oldit, newit)?;
                    masht.push(mashit);
                }
                let mashresult =
                    Inferator::mash(inferences, oldresult, newresult)?;
                Ok(Type::Func(masht, Box::new(mashresult)))
            }
            (&Type::Tuple(ref olditems), &Type::Tuple(ref newitems)) => {
                let oldlen = olditems.0.len();
                let newlen = newitems.0.len();
                if oldlen != newlen {
                    return match_err();
                }
                let mut mashitems = Vec::with_capacity(oldlen);
                for (oi, ni) in olditems.0.iter().zip(newitems.0.iter()) {
                    let mi = Inferator::mash(inferences, &oi.1, &ni.1)?;
                    mashitems.push((None, mi));
                }
                Ok(Type::Tuple(Struple(mashitems)))
            }
            (&Type::UserDef(ref oldlri), &Type::UserDef(ref newlri)) => {
                if oldlri.modules != newlri.modules {
                    return match_err();
                }
                if oldlri.localid != newlri.localid {
                    return match_err();
                }
                if oldlri.params.is_none() || oldlri.params.is_none() {
                    return match_err();
                }
                let oldparams = oldlri.params.as_ref().unwrap();
                let newparams = newlri.params.as_ref().unwrap();
                let oldlen = oldparams.len();
                if oldlen != newparams.len() {
                    return match_err();
                }
                let mut mashparams = Vec::with_capacity(oldlen);
                for (op, np) in oldparams.iter().zip(newparams.iter()) {
                    let mp = Inferator::mash(inferences, &op, &np)?;
                    mashparams.push(mp);
                }
                let mashlri = Lri::full(
                    oldlri.modules.clone(),
                    oldlri.localid.clone(),
                    Some(mashparams),
                );
                Ok(Type::UserDef(mashlri))
            }
            (_, _) => match_err(),
        };
        vout!("\tmashed -> {:?}\n", mtype);
        mtype
    }


    pub fn make_call_type(
        &mut self,
        ftype: &Type,
        argst: &Vec<&Type>,
    ) -> TypeResult
    {
        vout!("make_call_type({}, {:?})\n", ftype, argst);
        let (defargst, defresult) = Type::split_func_ref(ftype);

        let defargslen = defargst.len();
        let argslen = argst.len();
        if argslen > defargslen {
            panic!("too many args passed to {:?}: {:?}", ftype, argst);
        }
        if argslen < defargslen {
            panic!("it's so much fun to curry, but not supported yet");
        }

        let mashed_args = defargst
            .iter()
            .zip(argst.iter())
            .map(|(defargt, argt)| {
                Inferator::mash(&mut self.inferences, defargt, argt)
                    .map_err(|e| {
                        e.add_context(Lstr::from(format!(
                            "expected function args in {}: {:?} found {:?}",
                            self.funcname, defargst, argst,
                        )))
                    })
                    .unwrap()
            })
            .collect();
        Ok(Type::f(mashed_args, self.inferred_type(defresult)))
    }
}


#[cfg(test)]
mod tests
{
    use leema::infer::{Inferator, TypeSet};
    use leema::list;
    use leema::lri::Lri;
    use leema::lstr::Lstr;
    use leema::struple::Struple;
    use leema::val::{Type, Val};

    use std::collections::HashMap;


    #[test]
    fn test_add_and_find()
    {
        let mut t = Inferator::new("burritos");
        t.bind_vartype(&Lstr::Sref("a"), &Type::Int, 18).unwrap();
        assert_eq!(Type::Int, t.vartype("a").unwrap());
    }

    #[test]
    fn test_merge_strict_list_unknown()
    {
        let mut t = Inferator::new("burritos");
        let mtype = t.merge_types(
            &Type::StrictList(Box::new(Type::Unknown)),
            &Type::StrictList(Box::new(Type::Int)),
        );

        assert_eq!(Ok(Type::StrictList(Box::new(Type::Int))), mtype);
    }

    #[test]
    fn test_merge_types_via_tvar()
    {
        let mut t = Inferator::new("burritos");
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
        let mut t = Inferator::new("burritos");
        let defargst = Type::f(
            vec![Type::Var(Lstr::from("A")), Type::Int],
            Type::Var(Lstr::from("A")),
        );
        let argvalt = vec![&Type::Hashtag, &Type::Int];

        let mct = t.make_call_type(&defargst, &argvalt).unwrap();

        let (func_args, func_result) = Type::split_func(mct);
        assert_eq!(2, func_args.len());
        assert_eq!(Type::Hashtag, func_args[0]);
        assert_eq!(Type::Int, func_args[1]);
        assert_eq!(Type::Hashtag, func_result);
    }

    #[test]
    fn test_match_pattern_empty_list()
    {
        let mut t = Inferator::new("burritos");
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
        let mut t = Inferator::new("burritos");
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
        let mut t = Inferator::new("burritos");
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
        let mut t = Inferator::new("burritos");
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
        let oldt = Type::Tuple(Struple(vec![
            (None, Type::Var(Lstr::Sref("A"))),
            (None, Type::Var(Lstr::Sref("B"))),
        ]));
        let newt =
            Type::Tuple(Struple(vec![(None, Type::Int), (None, Type::Str)]));

        let result = Inferator::mash(&mut inferences, &oldt, &newt);
        result.unwrap();
    }

    #[test]
    fn test_mash_userdefs_containing_vars()
    {
        let mut inferences = HashMap::new();
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

        let result = Inferator::mash(&mut inferences, &oldt, &newt);
        result.unwrap();
    }

}
