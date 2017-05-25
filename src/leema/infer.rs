
use leema::log;
use leema::val::{Val, Type};

use std::collections::{HashMap};
use std::io::{stderr, Write};
use std::rc::{Rc};


#[derive(Debug)]
pub struct Inferator
{
    T: HashMap<String, Type>,
    inferences: HashMap<Rc<String>, Type>,
}

impl Inferator
{
    pub fn new() -> Inferator
    {
        Inferator{
            T: HashMap::new(),
            inferences: HashMap::new(),
        }
    }

    pub fn vartype(&self, argn: &str) -> Option<&Type>
    {
        match self.T.get(argn) {
            None => None,
            Some(&Type::AnonVar) => {
                panic!("Can't infer AnonVar");
            }
            Some(ref argt) => {
                Some(self.inferred_type(argt))
            }
        }
    }

    pub fn bind_vartype(&mut self, argn: &str, argt: &Type) -> Option<Type>
    {
        if !self.T.contains_key(argn) {
            self.T.insert(String::from(argn), argt.clone());
            return Some(argt.clone())
        }

        let oldargt = self.T.get(argn).unwrap();
        Inferator::mash(&mut self.inferences, oldargt, argt)
    }

    pub fn merge_types(&mut self, a: &Type, b: &Type) -> Option<Type>
    {
        Inferator::mash(&mut self.inferences, a, b)
    }

    pub fn match_pattern(&mut self, patt: &Val, valtype: &Type)
    {
        match (patt, valtype) {
            (_, &Type::AnonVar) => {
                panic!("pattern value type cannot be anonymous: {:?}"
                        , patt);
            }
            (&Val::Id(ref id), _) => {
                self.bind_vartype(id, valtype);
            }
            (&Val::Tuple(ref p_items), &Type::Tuple(ref t_items)) => {
                if p_items.len() != t_items.len() {
                    panic!("tuple pattern size mismatch: {:?} != {:?}"
                        , p_items, t_items);
                }
                for (pi, ti) in p_items.iter().zip(t_items.iter()) {
                    self.match_pattern(pi, ti);
                }
            }
            (&Val::Nil, _) => {
                self.merge_types(
                    &Type::StrictList(Box::new(Type::Unknown)),
                    valtype,
                );
            }
            _ => {
                let ptype = patt.get_type();
                let mtype = self.merge_types(&ptype, valtype);
                if mtype.is_none() {
                    panic!("pattern type mismatch: {:?} != {:?}"
                        , patt, valtype);
                }
            }
        }
    }

    pub fn inferred_type<'a>(&'a self, typ: &'a Type) -> &Type
    {
        if !typ.is_var() {
            return typ;
        }
        let varname = typ.var_name();
        match self.inferences.get(&varname) {
            Some(ref other_type) => {
                self.inferred_type(other_type)
            }
            None => typ,
        }
    }

    fn mash(inferences: &mut HashMap<Rc<String>, Type>
            , oldt: &Type, newt: &Type) -> Option<Type>
    {
        if oldt == newt {
            // all good
            return Some(oldt.clone());
        }
        match (oldt, newt) {
            (&Type::Var(ref oldtname), &Type::Var(ref newtname)) => {
                if oldtname < newtname {
                    inferences.insert(newtname.clone(), oldt.clone());
                    Some(oldt.clone())
                } else {
                    inferences.insert(oldtname.clone(), newt.clone());
                    Some(newt.clone())
                }
            }
            (&Type::StrictList(ref oldit), &Type::StrictList(ref newit)) => {
                Inferator::mash(inferences, oldit, newit).and_then(|t| {
                    Some(Type::StrictList(Box::new(t)))
                })
            }
            (&Type::StrictList(_), &Type::RelaxedList) => Some(oldt.clone()),
            (&Type::RelaxedList, &Type::StrictList(_)) => Some(newt.clone()),
            (&Type::Var(ref oldtname), _) => {
                inferences.insert(oldtname.clone(), newt.clone());
                Some(newt.clone())
            }
            (_, &Type::Var(ref newtname)) => {
                inferences.insert(newtname.clone(), oldt.clone());
                Some(oldt.clone())
            }
            (&Type::Tuple(ref oldi), &Type::Tuple(ref newi)) => {
                if oldi.len() != newi.len() {
                    panic!("tuple size mismatch: {:?}!={:?}", oldt, newt);
                }
                let mut masht = vec![];
                for (oldit, newit) in oldi.iter().zip(newi.iter()) {
                    match Inferator::mash(inferences, oldit, newit) {
                        Some(mashit) => {
                            masht.push(mashit);
                        }
                        None => {
                            panic!("tuple type mismatch: {:?} != {:?}",
                                oldt, newt);
                        }
                    }
                }
                Some(Type::Tuple(masht))
            }
            // nothing to mash for unknown types
            (&Type::Unknown, _) => Some(newt.clone()),
            (_, &Type::Unknown) => Some(oldt.clone()),
            (_, _) => {
                println!("type mismatch: {:?} != {:?}", oldt, newt);
                None
            }
        }
    }



    pub fn make_call_type(&mut self, ftype: &Type, argst: &Vec<&Type>) -> Type
    {
        let (defargst, defresult) = Type::split_func(ftype);

        let defargslen = defargst.len();
        let argslen = argst.len();
        if argslen > defargslen {
            panic!("too many args passed to {:?}", ftype);
        }
        if argslen < defargslen {
            panic!("it's so much fun to curry, but not supported yet");
        }

        for (defargt, argt) in defargst.iter().zip(argst.iter()) {
            Inferator::mash(&mut self.inferences, defargt, argt);
        }
        self.inferred_type(defresult).clone()
    }
}


#[cfg(test)]
mod tests {
    use leema::infer::{Inferator};
    use leema::list;
    use leema::log;
    use leema::module::{ModKey};
    use leema::val::{Val, Type};

    use std::rc::{Rc};
    use std::io::{stderr, Write};
    use std::collections::{HashMap};


#[test]
fn test_add_and_find()
{
    let mut t = Inferator::new();
    t.bind_vartype("a", &Type::Int);
    assert_eq!(Type::Int, *t.vartype("a").unwrap());
}

#[test]
fn test_merge_strict_list_unknown()
{
    let mut t = Inferator::new();
    let mtype = t.merge_types(
        &Type::StrictList(Box::new(Type::Unknown)),
        &Type::StrictList(Box::new(Type::Int)),
    );

    assert_eq!(Some(Type::StrictList(Box::new(Type::Int))), mtype);
}

#[test]
fn test_merge_types_via_tvar()
{
    let mut t = Inferator::new();
    let intlist = Type::StrictList(Box::new(Type::Int));
    let unknownlist = Type::StrictList(Box::new(Type::Unknown));
    let tvar = Type::Var(Rc::new("Taco".to_string()));

    let mtype0 = t.merge_types(&unknownlist, &tvar);
    assert_eq!(Some(unknownlist), mtype0);

    let mtype1 = t.merge_types(&intlist, &tvar);
    assert_eq!(Some(intlist), mtype1);
}

#[test]
fn test_match_pattern_empty_list()
{
    let mut t = Inferator::new();
    let tvar = Type::Var(Rc::new("Taco".to_string()));
    t.match_pattern(&Val::Nil, &tvar);

    assert_eq!(&Type::StrictList(Box::new(Type::Unknown)),
        t.inferred_type(&tvar));
}

#[test]
fn test_match_pattern_empty_and_full_lists()
{
    let mut t = Inferator::new();
    let tvar = Type::Var(Rc::new("Taco".to_string()));
    t.match_pattern(&Val::Nil, &tvar);
    t.match_pattern(&list::singleton(Val::Int(5)), &tvar);

    assert_eq!(&Type::StrictList(Box::new(Type::Int)),
        t.inferred_type(&tvar));
}

}
