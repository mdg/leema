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
#[derive(PartialEq)]
#[derive(Clone)]
#[derive(Copy)]
enum ScopeType
{
    Module,
    Function,
    Block,
}

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
    inferred: HashMap<Type, Type>,
    // type definitins in type namespace
    K: HashMap<String, Type>,
    // labels that have failure checks
    checked_failures: HashMap<Arc<String>, Val>,
    // the failed label currently being checked
    failed: Option<String>,
    typ: ScopeType,
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
            typ: ScopeType::Module,
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
            typ: ScopeType::Function,
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
            typ: ScopeType::Block,
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
            typ: ScopeType::Block,
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

    pub fn new_pattern_reg(&mut self, dst: &Reg) -> Reg
    {
        match dst {
            &Reg::Param(_) => dst.clone(),
            &Reg::Undecided => Reg::new_reg(self.nextreg()),
            _ => {
                panic!("Unexpected reg param: {:?}", dst);
            }
        }
    }

    pub fn assign_pattern(&mut self, patt: Val, typ: &Type, dst: &Reg) -> Val
    {
        match patt {
            Val::Id(name) => {
                let dstreg = self.new_pattern_reg(dst);
                self.assign_label(dstreg.clone(), &*name, typ.clone());
                Val::PatternVar(dstreg)
            }
            Val::Cons(phead, ptail) => {
                match typ {
                    &Type::StrictList(ref innert) => {
                        let chead = self.assign_pattern(*phead, innert, dst);
                        let ctail = self.assign_pattern(*ptail, typ, dst);
                        Val::Cons(Box::new(chead), Box::new(ctail))
                    }
                    &Type::Var(_) => {
                        let innert =
                            Type::Var(Arc::new("TypeVar_G".to_string()));
                        let chead = self.assign_pattern(*phead, &innert, dst);
                        let outert = Type::StrictList(Box::new(innert));
                        let ctail = self.assign_pattern(*ptail, &outert, dst);
                        self.infer_type(typ, &outert);
                        Val::Cons(Box::new(chead), Box::new(ctail))
                    }
                    &Type::RelaxedList => {
                        panic!("is relaxed list really necessary?");
                    }
                    _ => {
                        panic!("pattern is a list, expected list input as well, instead {:?}", typ);
                    }
                }
            }
            Val::Tuple(items) => {
                match typ {
                    &Type::Tuple(ref ttypes) => {
                        let mut atup = vec![];
                        let mut i = 0;
                        for p in items {
                            let itemtype = ttypes.get(i).unwrap();
                            let tp = self.assign_pattern(p, &itemtype, dst);
                            atup.push(tp);
                            i += 1;
                        }
                        Val::Tuple(atup)
                    }
                    &Type::Var(_) => {
                        let mut tup_typ_vars = vec![];
                        let mut atup = vec![];
                        let mut i = 0;
                        for p in items {
                            let new_type_name = format!("TypeVar_Item{}", i);
                            let new_t = Type::Var(Arc::new(new_type_name));
                            atup.push(self.assign_pattern(p, &new_t, dst));
                            tup_typ_vars.push(new_t);
                            i += 1;
                        }
                        self.infer_type(typ, &Type::Tuple(tup_typ_vars));
                        Val::Tuple(atup)
                    }
                    _ => {
                        panic!("pattern is a tuple, expected tuple input, not {:?}", typ);
                    }
                }
            }
            Val::Int(_) => {
                self.infer_type(typ, &Type::Int);
                patt
            }
            Val::Str(_) => {
                self.infer_type(typ, &Type::Str);
                patt
            }
            Val::Bool(_) => {
                self.infer_type(typ, &Type::Bool);
                patt
            }
            Val::Hashtag(_) => {
                self.infer_type(typ, &Type::Hashtag);
                patt
            }
            Val::Nil => {
                // should really make this list of type var
                // need a good way to generate unique type vars
                self.infer_type(typ, &Type::RelaxedList);
                patt
            }
            Val::Wildcard => {
                // nothing to infer here
                patt
            }
            _ => {
                panic!("That's not a pattern! {:?}", patt);
            }
        }
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

    pub fn apply_call_types(&mut self, fname: &String, input_tuple: &Type)
        -> Type
    {
vout!("apply_call_types({}, {:?})\n", fname, input_tuple);
        let (mut result_type, mut infers) = {
            let input_args = match input_tuple {
                &Type::Tuple(ref input_vec) => input_vec,
                _ => {
                    panic!("call args not a tuple: {:?}", input_tuple);
                }
            };
            let result = Type::Var(Arc::new("TypeVar_Result".to_string()));
            let mut called_type = Type::Func(
                input_args.clone(),
                Box::new(result.clone()),
            );
            let func_label = self.lookup_label(fname);
            if func_label.is_none() {
                panic!("function is undefined: {}", fname);
            }
            let (_, defined_type) = func_label.unwrap();
vout!("split_func {}({:?})", fname, defined_type);

            let mut tmp_infers = vec![];
            Scope::smash_types(&mut tmp_infers, &called_type, &defined_type);
            (result, tmp_infers)
        };

        /*
        let mut bad_types = false;
        if bad_types {
            panic!("wrong types: f{:?} called with {:?}",
                defined_args, input_args);
        }
        */
        for infer in infers {
            let (var, inf) = infer;
            if var == result_type {
                result_type = inf.clone();
            }
            self.inferred.insert(var, inf);
        }
        result_type
    }

    pub fn smash_types(
        infers: &mut Vec<(Type, Type)>,
        passed_type: &Type,
        defined_type: &Type,
    ) {
        match (passed_type, defined_type) {
            (&Type::Var(ref p), &Type::Var(ref d)) => {
                vout!("can we infer types when both sides are vars? {:?}={:?}", p, d);
                infers.push((passed_type.clone(), defined_type.clone()));
            }
            (&Type::Var(_), _) => {
                infers.push((passed_type.clone(), defined_type.clone()));
            }
            (_, &Type::Var(_)) => {
                infers.push((defined_type.clone(), passed_type.clone()));
            }
            (&Type::Unknown, _) => {
                panic!("passed type is unknown!");
            }
            (_, &Type::Unknown) => {
                panic!("defined type is unknown!");
            }
            (&Type::Func(ref pargs, ref presult), &Type::Func(ref defargs, ref defresult)) => {
                Scope::smash_type_vector(infers, pargs, defargs);
                Scope::smash_types(infers, presult, defresult);
            }
            (ref p, ref d) if p == d => {
                // types matched, we're good
            }
            (ref p, ref d) => {
                panic!("wrong type {:?} != {:?}\n", p, d);
            }
        }
    }

    fn smash_type_vector(
        infers: &mut Vec<(Type, Type)>,
        passed_types: &Vec<Type>,
        defined_types: &Vec<Type>,
    ) {
        let passed_len = passed_types.len();
        let defined_types_len = defined_types.len();
        if passed_len < defined_types_len {
            panic!("too few values: {:?} != {:?}",
                passed_types, defined_types);
        }
        if passed_len > defined_types_len {
            panic!("too many values: {:?} != {:?}",
                passed_types, defined_types);
        }
        for z in passed_types.iter().zip(defined_types.iter()) {
            let (p, d) = z;
            Scope::smash_types(infers, p, d);
        }
    }

    pub fn inferred_type(&self, typevar: Type) -> Type
    {
        match typevar {
            Type::Var(_) => {
                match self.inferred.get(&typevar) {
                    Some(t) => t.clone(),
                    None => {
                        panic!("Could not infer type: {}", typevar);
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
        // don't put inferences at the block level
        match (&self.typ, &mut self.parent) {
            (&ScopeType::Block, &mut Some(ref mut par)) => {
                par.infer_type(typevar, newtype);
                return;
            }
            _ => {} // something else, infer here
        }
vout!("infer_type({}, {:?}) for {:?}\n",
typevar, newtype, self.get_scope_name());
        match typevar {
            &Type::Var(_) => {}
            _ => {
                // nothing
                if typevar != newtype {
                    panic!("type mismatch, found {:?}, expected {:?}",
                        typevar, newtype);
                }
                return;
            }
        }
        // avoiding patterns here to avoid borrow overlaps
        if self.inferred.contains_key(&typevar) {
            let old_type = self.inferred.get(&typevar).unwrap();
            if old_type != newtype {
                panic!("typevar previously inferred: {:?} now {:?}",
                    old_type, newtype); 
            }
        } else {
            self.inferred.insert(typevar.clone(), newtype.clone());
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

    Scope::push_call_scope(&mut s, &"world".to_string(), Val::Void);
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

    Scope::push_call_scope(&mut s, &"foo".to_string(), Val::Void);
    assert!(s.is_macro(&macro_name));
}

#[test]
fn test_label_assigned()
{
    let mut s = Scope::new("hello".to_string());
    let label_name = "world".to_string();
    s.assign_label(Reg::new_param(2), &label_name, Type::Int);
    assert!(s.is_label(&label_name));

    Scope::push_call_scope(&mut s, &"world".to_string(), Val::Void);
    assert!(s.is_label(&label_name));
}

}
