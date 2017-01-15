
use leema::val::{Val, Type};

use std::collections::{HashMap};
use std::sync::{Arc};


#[derive(Debug)]
pub struct Inferator
{
    T: HashMap<String, Type>,
    inferences: HashMap<Arc<String>, Type>,
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
            Some(ref argt) if argt.is_var() => {
                self.inferred_type(argt)
            }
            Some(ref argt) => {
                Some(argt)
            }
        }
    }

    pub fn bind_vartype(&mut self, argn: &str, argt: &Type)
    {
        if !self.T.contains_key(argn) {
            self.T.insert(String::from(argn), argt.clone());
            return;
        }

        self.mash(argn, argt);
    }

    fn inferred_type<'a>(&'a self, typ: &'a Type) -> Option<&Type>
    {
        let varname = typ.var_name();
        match self.inferences.get(&varname) {
            Some(ref other_type) => {
                self.inferred_type(other_type)
            }
            None => Some(typ),
        }
    }

    fn mash(&mut self, oldn: &str, newt: &Type)
    {
        let oldt = self.T.get(oldn).unwrap();
        if oldt == newt {
            // all good
            return;
        }
        match (oldt, newt) {
            (&Type::Var(ref oldtname), &Type::Var(ref newtname)) => {
                if oldtname < newtname {
                    self.inferences.insert(newtname.clone(), oldt.clone());
                } else {
                    self.inferences.insert(oldtname.clone(), newt.clone());
                }
            }
            (&Type::Var(ref oldtname), _) => {
                self.inferences.insert(oldtname.clone(), newt.clone());
            }
            (_, &Type::Var(ref newtname)) => {
                self.inferences.insert(newtname.clone(), oldt.clone());
            }
            (_, _) => {
                panic!("cannot mash types: {:?} <> {:?}", oldt, newt);
            }
        }
    }



    pub fn make_call_type(&mut self, ftype: &Type, argst: &Vec<Type>) -> Type
    {
        let (defargst, defresult) = Type::split_func(ftype);

        // let args = defargst.iter().zip(pargst).map(|defargt, pargt| {
            // Type::mash(defargt, pargt)
        // });
        defresult.clone()
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
