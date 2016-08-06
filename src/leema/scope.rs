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
    pub scope_name: String,
    pub func_name: String,
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
    _nextreg: i8,
}

impl Scope
{
    pub fn new(scope_nm: String) -> Scope
    {
        Scope{
            parent: None,
            scope_name: scope_nm,
            func_name: "".to_string(),
            m: HashMap::new(),
            E: HashMap::new(),
            T: HashMap::new(),
            inferred: HashMap::new(),
            K: HashMap::new(),
            _nextreg: 0,
        }
    }

    pub fn push_scope(parent: &mut Scope, func_nm: String)
    {
        let new_scope_nm = format!("{}.{}", parent.scope_name, func_nm);
        let mut tmp_scope = Scope{
            parent: None,
            scope_name: new_scope_nm,
            func_name: func_nm,
            m: HashMap::new(),
            E: HashMap::new(),
            T: HashMap::new(),
            inferred: HashMap::new(),
            K: HashMap::new(),
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

    pub fn define_macro(&mut self, name: &String, args: Vec<Arc<String>>, body: Val)
    {
        if self.m.contains_key(name) {
            panic!("macro is already defined: {}", name);
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
        //this work? self.parent.and_then(|p| p.get_macro(name))
        match self.parent {
            Some(ref p) => p.get_macro(name),
            None => None,
        }
    }

    pub fn assign_label(&mut self, r: Reg, name: &String, typ: Type)
    {
        if self.E.contains_key(name) || self.T.contains_key(name) {
            panic!("label already assigned: {}", name);
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
            (None, None) => None,
            _ => {
                panic!("mixed lookup_label result");
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

        let defined_type_opt = self.T.get(fname);
        if defined_type_opt.is_none() {
            panic!("function is undefined: {}", fname);
        }
        let defined_type = defined_type_opt.unwrap();
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
        for a in input_args.iter().zip(defined_args.iter()) {
            match a {
                (&Type::Var(_), &Type::Var(_)) => {
                    panic!("can't infer types");
                }
                (&Type::Unknown, _) => {
                    panic!("input arg is unknown!");
                }
                (_, &Type::Unknown) => {
                    panic!("defined arg is unknown!");
                }
                (&Type::Var(ref i), d) => {
                    self.inferred.insert((**i).clone(), d.clone());
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
        defined_result.clone()
    }

    /**
     * mark typevar as inferred, and which type.
     *
     * panic if it was already inferred w/ a different type.
     * do nothing if it was already inferred w/ the same type.
     */
    pub fn infer_type(&mut self, typevar: &Type, newtype: &Type)
    {
vout!("infer_type({}, {:?}) for {}", typevar, newtype, self.scope_name);
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

    pub fn is_type(&self, name: &String) -> bool
    {
        self.get_type(name).is_some()
    }

    pub fn get_type(&self, name: &String) -> Option<&Type>
    {
        let t = self.T.get(name);
        if t.is_some() {
            return t;
        }
        match self.parent {
            Some(ref p) => p.get_type(name),
            None => None,
        }
    }
}


#[cfg(test)]
mod tests {
    use leema::val::{Val, SexprType, Type};
    use leema::sexpr;
    use leema::scope::{Scope};

#[test]
fn test_push_pop()
{
    let mut s = Scope::new("hello".to_string());

    Scope::push_scope(&mut s, "world".to_string());
    assert_eq!("hello.world".to_string(), s.scope_name);

    Scope::pop_scope(&mut s);
    assert_eq!("hello".to_string(), s.scope_name);
}

#[test]
fn test_macro_defined()
{
    let mut s = Scope::new("hello".to_string());
    let macro_name = "world".to_string();
    s.define_macro(&macro_name, vec![], Val::Void);
    assert!(s.is_macro(&macro_name));

    Scope::push_scope(&mut s, "world".to_string());
    assert!(s.is_macro(&macro_name));
}

}
