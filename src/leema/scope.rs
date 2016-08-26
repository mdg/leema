use leema::val::{Val,SexprType,Type};
use leema::list;
use leema::log;
use leema::reg::{Reg, Ireg};
use leema::sexpr;
use std::collections::{HashMap, LinkedList};
use std::sync::Arc;
use std::io::{stderr, Write};
use std::mem;

#[derive(Debug)]
pub struct Scope
{
    parent: Option<Box<Scope>>,
    scope_name: Option<String>,
    // macros
    m: HashMap<String, (Vec<Arc<String>>, Val)>,
    // registers of locally defined labels
    E: HashMap<String, Reg>,
    // types of locally defined labels
    T: HashMap<String, Type>,
    // probably should move this to its own data structre and maybe module
    // to handle updates and multiple possible type options
    inferred: HashMap<String, Type>,
    // type definitins in type namespace
    K: HashMap<String, Type>,
    // labels that have failure checks
    checked_failures: HashMap<Arc<String>, Val>,
    // the failed label currently being checked
    failed: Option<String>,
    _nextreg: i8,
}

impl Scope
{
    pub fn new(scope_nm: String) -> Scope
    {
        Scope{
            parent: None,
            scope_name: Some(scope_nm),
            m: HashMap::new(),
            E: HashMap::new(),
            T: HashMap::new(),
            inferred: HashMap::new(),
            K: HashMap::new(),
            checked_failures: HashMap::new(),
            failed: None,
            _nextreg: 0,
        }
    }

    pub fn push_call_scope(parent: &mut Scope, func_nm: &String, ps: Val)
    {
        let mut tmp_scope = Scope{
            parent: None,
            scope_name: Some(func_nm.clone()),
            m: HashMap::new(),
            E: HashMap::new(),
            T: HashMap::new(),
            inferred: HashMap::new(),
            K: HashMap::new(),
            checked_failures: Scope::collect_ps_index(ps),
            failed: None,
            _nextreg: 0,
        };
        mem::swap(parent, &mut tmp_scope);
        parent.parent = Some(Box::new(tmp_scope));
    }

    pub fn push_block_scope(parent: &mut Scope)
    {
        let mut tmp_scope = Scope{
            parent: None,
            scope_name: None,
            m: HashMap::new(),
            E: HashMap::new(),
            T: HashMap::new(),
            inferred: HashMap::new(),
            K: HashMap::new(),
            checked_failures: HashMap::new(),
            failed: None,
            _nextreg: 0,
        };
        mem::swap(parent, &mut tmp_scope);
        parent.parent = Some(Box::new(tmp_scope));
    }

    pub fn push_failed_scope(parent: &mut Scope, var: &String)
    {
        let mut tmp_scope = Scope{
            parent: None,
            scope_name: None,
            m: HashMap::new(),
            E: HashMap::new(),
            T: HashMap::new(),
            inferred: HashMap::new(),
            K: HashMap::new(),
            checked_failures: HashMap::new(),
            failed: Some(var.clone()),
            _nextreg: 0,
        };
        mem::swap(parent, &mut tmp_scope);
        parent.parent = Some(Box::new(tmp_scope));
    }

    pub fn pop_scope(s: &mut Scope)
    {
        let mut tmp = None;
        mem::swap(&mut s.parent, &mut tmp);
        if tmp.is_none() {
            panic!("No scope left to pop {}");
        }
        mem::replace(s, *tmp.unwrap());
    }

    pub fn get_scope_name(&self) -> String
    {
        match (&self.parent, &self.scope_name) {
            (&None, &None) => {
                "".to_string()
            }
            (&None, &Some(ref scope_nm)) => {
                scope_nm.clone()
            }
            (&Some(ref p), &None) => {
                p.get_scope_name()
            }
            (&Some(ref p), &Some(ref scope_nm)) => {
                format!("{}.{}", p.get_scope_name(), scope_nm)
            }
        }
    }

    pub fn define_macro(&mut self, name: &String, args: Vec<Arc<String>>, body: Val)
    {
        if self.is_label(name) {
            panic!("label already assigned: {}", name);
        }
        if self.is_macro(name) {
            panic!("macro already defined: {}", name);
        }
        self.m.insert(name.clone(), (args, body));
    }

    pub fn is_macro(&self, name: &String) -> bool
    {
        self.get_macro(name).is_some()
    }

    pub fn get_macro<'a>(&'a self, name: &String) -> Option<&'a (Vec<Arc<String>>, Val)>
    {
        let m = self.m.get(name);
        if m.is_some() || self.parent.is_none() {
            return m;
        }
        match self.parent {
            Some(ref p) => p.get_macro(name),
            None => None,
        }
    }

    pub fn assign_label(&mut self, r: Reg, name: &String, typ: Type)
    {
        if self.is_label(name) {
            panic!("label already assigned: {}", name);
        }
        if self.is_macro(name) {
            panic!("macro already defined: {}", name);
        }
        self.E.insert(name.clone(), r);
        self.T.insert(name.clone(), typ);
    }

    pub fn is_label(&self, name: &String) -> bool
    {
        self.lookup_label(name).is_some()
    }

    pub fn lookup_label(&self, name: &String) -> Option<(&Reg, &Type)>
    {
        let regopt = self.E.get(name);
        let typopt = self.T.get(name);
        match (regopt, typopt) {
            (Some(ref r), Some(t)) => Some((r, t)),
            (None, None) => {
                match self.parent {
                    Some(ref p) => p.lookup_label(name),
                    None => {
                        None
                    }
                }
            }
            _ => {
                panic!("mixed lookup_label result");
            }
        }
    }

    pub fn is_failed(&self, name: &String) -> bool
    {
        match self.failed {
            Some(ref scope_failed_name) if scope_failed_name == name => true,
            _ => {
                match self.parent {
                    Some(ref p) => p.is_failed(name),
                    None => false,
                }
            }
        }
    }

    pub fn apply_call_types(&mut self, fname: &String, input_tuple: &Type) -> Type
    {
vout!("apply_call_types({}, {:?})\n", fname, input_tuple);
        let input_args = match input_tuple {
            &Type::Tuple(ref input_vec) => input_vec,
            _ => {
                panic!("call args not a tuple: {:?}", input_tuple);
            }
        };
        let input_len = input_args.len();

        let (infers, result_type) = {
            let func_label = self.lookup_label(fname);
            if func_label.is_none() {
                panic!("function is undefined: {}", fname);
            }
            let (_, defined_type) = func_label.unwrap();
vout!("split_func {}({:?})", fname, defined_type);
            let (defined_args, defined_result) = Type::split_func(defined_type);

            let defined_args_len = defined_args.len();
            if input_len < defined_args_len {
                panic!("too few args: f{:?} called with {:?}",
                    defined_args, input_args);
            }
            if input_len > defined_args_len {
                panic!("too many args: f{:?} called with {:?}",
                    defined_args, input_args);
            }

            let mut bad_types = false;
            let mut infers: Vec<(String, Type)> = vec![];
            for a in input_args.iter().zip(defined_args.iter()) {
                match a {
                    (&Type::Var(ref i), &Type::Var(ref d)) => {
                        vout!("can we infer types when both sides are vars? {:?}={:?}", i, d);
                        infers.push(((**d).clone(), Type::Var(i.clone())))
                    }
                    (&Type::Unknown, _) => {
                        panic!("input arg is unknown!");
                    }
                    (_, &Type::Unknown) => {
                        panic!("defined arg is unknown!");
                    }
                    (&Type::Var(ref i), d) => {
                        infers.push(((**i).clone(), d.clone()));
                    }
                    (i, &Type::Var(ref d)) => {
                        infers.push(((**d).clone(), i.clone()));
                    }
                    (i, d) if i == d => {
                        // types matched, we're good
                    }
                    (i, d) => {
                        vout!("wrong arg {:?} != {:?}\n", i, d);
                        bad_types = true;
                    }
                }
            }
            if bad_types {
                panic!("wrong types: f{:?} called with {:?}",
                    defined_args, input_args);
            }
            (infers, defined_result.clone())
        };
        for infer in infers {
            let (i, d) = infer;
            self.inferred.insert(i, d);
        }
        result_type
    }

    pub fn inferred_type(&self, typevar: Type) -> Type
    {
        match typevar {
            Type::Var(ref name) => {
                match self.inferred.get(&**name) {
                    Some(t) => t.clone(),
                    None => {
                        panic!("Could not infer type: {}", name);
                    }
                }
            }
            Type::AnonVar => {
                panic!("Cannot infer an anonymous type");
            }
            _ => {
                typevar
            }
        }
    }

    /**
     * mark typevar as inferred, and which type.
     *
     * panic if it was already inferred w/ a different type.
     * do nothing if it was already inferred w/ the same type.
     */
    pub fn infer_type(&mut self, typevar: &Type, newtype: &Type)
    {
vout!("infer_type({}, {:?}) for {:?}", typevar, newtype, self.scope_name);
        let tvname = match typevar {
            &Type::Var(ref tvnm) => tvnm,
            _ => {
                panic!("cannot infer not a typevar: {:?}", typevar);
            }
        };
        // avoiding patterns here to avoid borrow overlaps
        if self.inferred.contains_key(&**tvname) {
            let old_type = self.inferred.get(&**tvname).unwrap();
            if old_type != newtype {
                panic!("typevar previously inferred: {:?} now {:?}",
                    old_type, newtype); 
            }
        } else {
            self.inferred.insert((**tvname).clone(), newtype.clone());
        }
    }

    pub fn define_type(&mut self, name: &String, typ: &Type)
    {
        if self.is_type(name) {
            panic!("Type is already defined: {}", name);
        }
        self.K.insert(name.clone(), typ.clone());
    }

    pub fn is_type(&self, name: &String) -> bool
    {
        self.get_type(name).is_some()
    }

    pub fn get_type(&self, name: &String) -> Option<&Type>
    {
        let t = self.K.get(name);
        if t.is_some() {
            return t;
        }
        match self.parent {
            Some(ref p) => p.get_type(name),
            None => None,
        }
    }


    pub fn nextreg(&mut self) -> i8
    {
        let r = self._nextreg;
        self._nextreg += 1;
        r
    }

    pub fn collect_ps_index(ps: Val) -> HashMap<Arc<String>, Val>
    {
        let base = HashMap::new();
        if ps == Val::Void {
            return base;
        }
        list::fold(ps, base, |mut acc, f| {
            let id = match &f {
                &Val::Sexpr(SexprType::MatchFailed, ref mf) => {
                    let (x, _) = list::take_ref(mf);
                    x.id_name().clone()
                }
                _ => {
                    panic!("Postscript blocks may only contain failures: {:?}",
                        f);
                }
            };
            acc.insert(id, f);
            acc
        })
    }

    pub fn take_failure_check(&mut self, name: &String) -> Option<Val>
    {
        self.checked_failures.remove(name)
    }
}


#[cfg(test)]
mod tests {
    use leema::val::{Val, SexprType, Type};
    use leema::sexpr;
    use leema::scope::{Scope};
    use leema::reg::{Reg};

#[test]
fn test_push_pop()
{
    let mut s = Scope::new("hello".to_string());

    Scope::push_scope(&mut s, Some(&"world".to_string()));
    assert_eq!("hello.world".to_string(), s.get_scope_name());

    Scope::pop_scope(&mut s);
    assert_eq!("hello".to_string(), s.get_scope_name());
}

#[test]
fn test_macro_defined()
{
    let mut s = Scope::new("hello".to_string());
    let macro_name = "world".to_string();
    s.define_macro(&macro_name, vec![], Val::Void);
    assert!(s.is_macro(&macro_name));

    Scope::push_scope(&mut s, Some(&"foo".to_string()));
    assert!(s.is_macro(&macro_name));
}

#[test]
fn test_label_assigned()
{
    let mut s = Scope::new("hello".to_string());
    let label_name = "world".to_string();
    s.assign_label(Reg::new_param(2), &label_name, Type::Int);
    assert!(s.is_label(&label_name));

    Scope::push_scope(&mut s, Some(&"world".to_string()));
    assert!(s.is_label(&label_name));
}

}
