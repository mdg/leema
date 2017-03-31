
use leema::val::{Val, Type};

use std::collections::{HashMap};
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

    pub fn bind_vartype(&mut self, argn: &str, argt: &Type)
    {
        if !self.T.contains_key(argn) {
            self.T.insert(String::from(argn), argt.clone());
            return;
        }

        let oldargt = self.T.get(argn).unwrap();
        Inferator::mash(&mut self.inferences, oldargt, argt);
    }

    pub fn merge_types(&mut self, a: &Type, b: &Type)
    {
        Inferator::mash(&mut self.inferences, a, b);
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

    fn mash(inferences: &mut HashMap<Rc<String>
            , Type>, oldt: &Type, newt: &Type)
    {
        if oldt == newt {
            // all good
            return;
        }
        match (oldt, newt) {
            (&Type::Var(ref oldtname), &Type::Var(ref newtname)) => {
                if oldtname < newtname {
                    inferences.insert(newtname.clone(), oldt.clone());
                } else {
                    inferences.insert(oldtname.clone(), newt.clone());
                }
            }
            (&Type::Var(ref oldtname), _) => {
                inferences.insert(oldtname.clone(), newt.clone());
            }
            (_, &Type::Var(ref newtname)) => {
                inferences.insert(newtname.clone(), oldt.clone());
            }
            // nothing to mash for unknown types
            (&Type::Unknown, _) => {}
            (_, &Type::Unknown) => {}
            (_, _) => {
                panic!("cannot mash types: {:?} <> {:?}", oldt, newt);
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
            Inferator::mash(&mut self.inferences, defargt, argt)
        }
        self.inferred_type(defresult).clone()
    }



    /**
     * mark typevar as inferred, and which type.
     *
     * panic if it was already inferred w/ a different type.
     * do nothing if it was already inferred w/ the same type.
     */
    pub fn match_types(&mut self, a: &Type, b: &Type)
    {
print!("infer.match_types({:?}, {:?})\n", a, b);

        match (a, b) {
            (&Type::StrictList(ref innera), &Type::StrictList(ref innerb)) => {
                self.match_types(innera, innerb);
            }
            (&Type::Tuple(ref ta), &Type::Tuple(ref tb)) => {
                self.match_type_vectors(ta, tb);
            }
            (&Type::Func(ref pa, ref ra), &Type::Func(ref pb, ref rb)) => {
                self.match_type_vectors(pa, pb);
                self.match_types(ra, rb);
            }
            (_, _) if a == b => {}
            (_, _) => {
                panic!("these types don't match {:?}<>{:?}", a, b);
            }
        }
    }

    fn match_type_vectors(&mut self,
        passed_types: &Vec<Type>,
        defined_types: &Vec<Type>,
    ) {
        let passed_len = passed_types.len();
        let defined_types_len = defined_types.len();
        if passed_len != defined_types_len {
            panic!("tuple size mismatch: {:?} != {:?}",
                passed_types, defined_types);
        }
        for z in passed_types.iter().zip(defined_types.iter()) {
            let (p, d) = z;
            self.match_types(p, d);
        }
    }
}
