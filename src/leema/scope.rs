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
pub struct Inferator
{
    inferences: HashMap<Type, LinkedList<Type>>,
    typevar_index: i16,
}

impl Inferator
{
    pub fn new() -> Inferator
    {
        Inferator{
            inferences: HashMap::new(),
            typevar_index: 0,
        }
    }

    pub fn next_typevar_index(&mut self) -> i16
    {
        let t = self.typevar_index;
        self.typevar_index += 1;
        t
    }

    /**
     * mark typevar as inferred, and which type.
     *
     * panic if it was already inferred w/ a different type.
     * do nothing if it was already inferred w/ the same type.
     */
    pub fn match_types(&mut self, a: &Type, b: &Type)
    {
vout!("infer.match_types({:?}, {:?})\n", a, b);

        match (a, b) {
            (&Type::Var(_), &Type::Var(_)) => {
                self.mapvar(a, b);
                self.mapvar(b, a);
            }
            (&Type::Var(_), _) => {
                self.mapvar(a, b);
            }
            (_, &Type::Var(_)) => {
                self.mapvar(b, a);
            }
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

    fn mapvar(&mut self, key: &Type, option: &Type)
    {
        if !self.inferences.contains_key(key) {
            self.inferences.insert(key.clone(), LinkedList::new());
        }
        let types = self.inferences.get_mut(key).unwrap();
        for t in types.iter() {
            if option == t {
                // don't store dupes, we're done
                return;
            }
            if !Type::is_var(t) && !Type::is_var(option) {
                panic!("Mismatched types: {:?} != {:?}", option, t);
            }
        }
        types.push_front(option.clone());
    }

    pub fn find_inferred_type<'a>(&'a self, key: &'a Type) -> &'a Type
    {
        let mut result = key;
        match key {
            &Type::Var(_) => {
                match self.inferences.get(&key) {
                    Some(ref types) => {
                        for t in types.iter() {
                            if !t.is_var() {
                                result = t;
                                break;
                            } else if t < result {
                                result = t;
                            }
                        }
                    }
                    None => {
                        panic!("Could not infer type: {}", key);
                    }
                }
            }
            &Type::AnonVar => {
                panic!("Cannot infer an anonymous type");
            }
            _ => {
                // do nothing, leave result = key
            }
        }
        result
    }
}

#[derive(Debug)]
pub struct BlockScope
{
    parent: Option<Box<BlockScope>>,
    // registers of locally defined labels
    E: HashMap<String, Reg>,
    nextreg: i8,
}

impl BlockScope
{
    pub fn new(nr: i8) -> BlockScope
    {
        BlockScope{
            parent: None,
            E: HashMap::new(),
            nextreg: nr,
        }
    }

    pub fn nextreg(&mut self) -> i8
    {
        let r = self.nextreg;
        self.nextreg += 1;
        r
    }

    pub fn find_label_reg(&self, name: &String) -> Option<&Reg>
    {
        let lbl = self.E.get(name);
        match (lbl, &self.parent) {
            (Some(ref t), _) => Some(t),
            (None, &Some(ref p)) => p.find_label_reg(name),
            (None, &None) => None,
        }
    }
}

#[derive(Debug)]
pub struct FunctionScope
{
    parent: Option<Box<FunctionScope>>,
    name: String,
    function_param_types: Option<Vec<Type>>,
    blk: BlockScope,
    checked_failures: HashMap<Arc<String>, Val>,
    // types of locally defined labels
    T: HashMap<String, Type>,
}

impl FunctionScope
{
    pub fn new(name: &String) -> FunctionScope
    {
        FunctionScope{
            parent: None,
            name: name.clone(),
            function_param_types: Some(vec![]),
            blk: BlockScope::new(0),
            T: HashMap::new(),
            checked_failures: HashMap::new(),
        }
    }

    pub fn push_function(parent: &mut FunctionScope, name: &String,
        failures: Val
    ) {
        let new_name = format!("{}.{}", parent.name, name);
        let cfails = Scope::collect_ps_index(failures);
        let mut tmp_scope = FunctionScope{
            parent: None,
            name: new_name,
            function_param_types: None,
            blk: BlockScope::new(0),
            T: HashMap::new(),
            checked_failures: cfails,
        };
        mem::swap(parent, &mut tmp_scope);
        parent.parent = Some(Box::new(tmp_scope));
    }

    pub fn pop_function(s: &mut FunctionScope)
    {
        let mut tmp: Option<Box<FunctionScope>> = None;
        mem::swap(&mut s.parent, &mut tmp);
        if tmp.is_none() {
            panic!("No scope left to pop {}");
        }
        mem::replace(s, *tmp.unwrap());
    }

    pub fn push_block(&mut self)
    {
        let mut tmp_scope = BlockScope{
            parent: None,
            E: HashMap::new(),
            nextreg: self.blk.nextreg,
        };
        mem::swap(&mut self.blk, &mut tmp_scope);
        self.blk.parent = Some(Box::new(tmp_scope));
    }

    pub fn pop_block(&mut self)
    {
        let mut tmp: Option<Box<BlockScope>> = None;
        mem::swap(&mut self.blk.parent, &mut tmp);
        if tmp.is_none() {
            panic!("No scope left to pop {}");
        }
        mem::replace(&mut self.blk, *tmp.unwrap());
    }

    pub fn is_label(&self, name: &String) -> bool
    {
        self.find_label_type(name).is_some()
    }

    pub fn find_label_reg(&self, name: &String) -> Option<&Reg>
    {
        let lbl = self.blk.find_label_reg(name);
        match (lbl, &self.parent) {
            (Some(ref t), _) => Some(t),
            (None, &Some(ref p)) => p.find_label_reg(name),
            (None, &None) => None,
        }
    }

    pub fn find_label_type(&self, name: &String) -> Option<&Type>
    {
        let lbl = self.T.get(name);
        match (lbl, &self.parent) {
            (Some(ref t), _) => Some(t),
            (None, &Some(ref p)) => p.find_label_type(name),
            (None, &None) => None,
        }
    }

    pub fn lookup_label(&self, name: &String) -> Option<(&Reg, &Type)>
    {
        let regopt = self.find_label_reg(name);
        let typopt = self.find_label_type(name);
        match (regopt, typopt) {
            (Some(ref r), Some(ref t)) => Some((r, t)),
            (None, None) => None,
            _ => {
                panic!("mixed lookup_label result {:?}", (regopt, typopt));
            }
        }
    }
}

#[derive(Debug)]
pub struct ModuleScope
{
    name: String,
    // macros
    _macros: HashMap<String, (Vec<Arc<String>>, Val)>,
    // type definitins in type namespace
    _type_defs: HashMap<String, Type>,
}

impl ModuleScope
{
    pub fn new(name: &String) -> ModuleScope
    {
        ModuleScope{
            name: name.clone(),
            _macros: HashMap::new(),
            _type_defs: HashMap::new(),
        }
    }

    pub fn is_macro(&self, name: &String) -> bool
    {
        self._macros.contains_key(name)
    }

    pub fn get_type(&self, name: &String) -> Option<&Type>
    {
        self._type_defs.get(name)
    }
}

#[derive(Debug)]
pub struct Scope
{
    _module: ModuleScope,
    _infer: Inferator,
    _function: FunctionScope,
    _failed: Option<String>,
}

impl Scope
{
    pub fn new(scope_nm: &String) -> Scope
    {
        Scope{
            _module: ModuleScope::new(scope_nm),
            _infer: Inferator::new(),
            _function: FunctionScope::new(&"__script__".to_string()),
            _failed: None,
        }
    }

    pub fn push_function_scope(&mut self, func_nm: &String, ps: Val)
    {
        FunctionScope::push_function(&mut self._function, func_nm, ps);
    }

    pub fn pop_function_scope(&mut self)
    {
        FunctionScope::pop_function(&mut self._function)
    }

    pub fn push_block_scope(&mut self)
    {
        self._function.push_block();
    }

    pub fn pop_block_scope(&mut self)
    {
        self._function.pop_block();
    }

    pub fn push_failed_scope(&mut self, var: &String)
    {
        if self._failed.is_some() {
            panic!("Cannot nest failed scopes: {}", var);
        }
        self._failed = Some(var.clone());
    }

    pub fn pop_failed_scope(&mut self) -> String
    {
        if self._failed.is_none() {
            panic!("Cannot pop empty failed scope");
        }
        self._failed.take().unwrap()
    }

    pub fn get_scope_name(&self) -> String
    {
        format!("{}.{}", self._module.name, self._function.name)
    }

    pub fn define_macro(&mut self, name: &String, args: Vec<Arc<String>>, body: Val)
    {
        if self.is_label(name) {
            panic!("label already assigned: {}", name);
        }
        if self._module.is_macro(name) {
            panic!("macro already defined: {}", name);
        }
        self._module._macros.insert(name.clone(), (args, body));
    }

    pub fn is_macro(&self, name: &String) -> bool
    {
        self.get_macro(name).is_some()
    }

    pub fn get_macro<'a>(&'a self, name: &String) -> Option<&'a (Vec<Arc<String>>, Val)>
    {
        self._module._macros.get(name)
    }

    pub fn assign_label(&mut self, r: Reg, name: &String, typ: Type)
    {
        if self.is_macro(name) {
            panic!("macro already defined: {}", name);
        }
        match self._function.lookup_label(name) {
            None => {} // undefined, proceed below
            Some((old_reg, old_type)) => {
                if *old_reg != r {
                    panic!("Cannot move label to new reg: {}", name);
                }
                self._infer.match_types(old_type, &typ);
                return;
            }
        }
        self._function.blk.E.insert(name.clone(), r);
        self._function.T.insert(name.clone(), typ);
    }

    pub fn new_pattern_reg(&mut self, dst: &Reg, idx: Option<i8>) -> Reg
    {
        match (dst, idx) {
            (&Reg::Param(_), None) => dst.clone(),
            (&Reg::Param(_), Some(_)) => Reg::Undecided,
            (&Reg::Params, Some(i)) => Reg::new_param(i),
            (&Reg::Undecided, _) => Reg::new_reg(self.nextreg()),
            (&Reg::Params, None) => {
                panic!("Cannot make new pattern reg for Reg::Params, None");
            }
            _ => {
                panic!("Unexpected reg param: {:?}", dst);
            }
        }
    }

    pub fn assign_pattern(&mut self, patt: Val, typ: &Type, dst: &Reg) -> Val
    {
        match patt {
            Val::Id(name) => {
                let dstreg = self.new_pattern_reg(dst, None);
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
                        let innert = self.new_typevar();
                        let chead = self.assign_pattern(*phead, &innert, dst);
                        let outert = Type::StrictList(Box::new(innert));
                        let ctail = self.assign_pattern(*ptail, &outert, dst);
                        self._infer.match_types(typ, &outert);
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
            Val::Nil => {
                let innert = self.new_typevar();
                self._infer.match_types(
                    &Type::StrictList(Box::new(innert)), typ);
                patt
            }
            Val::Tuple(items) => {
                match typ {
                    &Type::Tuple(ref ttypes) => {
                        let mut atup = vec![];
                        let mut i = 0;
                        for p in items {
                            let preg = self.new_pattern_reg(dst, Some(i));
                            let itemtype = ttypes.get(i as usize).unwrap();
                            let tp = self.assign_pattern(p, &itemtype, &preg);
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
                            let preg = self.new_pattern_reg(dst, Some(i));
                            let new_t = self.new_typevar();
                            atup.push(self.assign_pattern(p, &new_t, &preg));
                            tup_typ_vars.push(new_t);
                            i += 1;
                        }
                        self._infer.match_types(
                            typ, &Type::Tuple(tup_typ_vars));
                        Val::Tuple(atup)
                    }
                    _ => {
                        panic!("pattern is a tuple, expected tuple input, not {:?}", typ);
                    }
                }
            }
            Val::Int(_) => {
                self._infer.match_types(typ, &Type::Int);
                patt
            }
            Val::Str(_) => {
                self._infer.match_types(typ, &Type::Str);
                patt
            }
            Val::Bool(_) => {
                self._infer.match_types(typ, &Type::Bool);
                patt
            }
            Val::Hashtag(_) => {
                self._infer.match_types(typ, &Type::Hashtag);
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
        self._function.is_label(name)
    }

    pub fn lookup_label(&self, name: &String) -> Option<(&Reg, &Type)>
    {
        self._function.lookup_label(name)
    }

    pub fn is_failed(&self, name: &String) -> bool
    {
        match self._failed {
            Some(ref f) => f == name,
            None => false,
        }
    }

    pub fn apply_call_types(&mut self, fname: &String, input_tuple: &Type)
        -> Type
    {
vout!("apply_call_types({}, {:?})\n", fname, input_tuple);
        let mut result_type = {
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
            let func_label = self._function.lookup_label(fname);
            if func_label.is_none() {
                panic!("function is undefined: {}", fname);
            }
            let (_, defined_type) = func_label.unwrap();
vout!("split_func {}({:?})", fname, defined_type);

            self._infer.match_types(&called_type, &defined_type);
            result
        };
println!("result_type = {:?}", result_type);

        result_type
    }

    pub fn find_inferred_type<'a>(&'a self, typevar: &'a Type) -> &'a Type
    {
        self._infer.find_inferred_type(typevar)
    }

    pub fn set_function_param_types(&mut self, fpt: &Vec<Type>)
    {
        self._function.function_param_types = Some(fpt.clone());
    }

    pub fn function_param_types(&self) -> &Vec<Type>
    {
        self._function.function_param_types.as_ref().unwrap()
    }

    pub fn define_type(&mut self, name: &String, typ: &Type)
    {
        if self.is_type(name) {
            panic!("Type is already defined: {}", name);
        }
        self._module._type_defs.insert(name.clone(), typ.clone());
    }

    pub fn is_type(&self, name: &String) -> bool
    {
        self._module.get_type(name).is_some()
    }

    pub fn get_type<'a>(&'a self, name: &String) -> Option<&'a Type>
    {
        self._module.get_type(name)
    }


    pub fn nextreg(&mut self) -> i8
    {
        self._function.blk.nextreg()
    }

    pub fn new_typevar(&mut self) -> Type
    {
        let t = self._infer.next_typevar_index();
        let tname = format!("TypeVar_{}_{}", self.get_scope_name(), t);
        Type::Var(Arc::new(tname))
    }

    pub fn named_typevar(&mut self, var: &String) -> Type
    {
        let t = self._infer.next_typevar_index();
        let tname = format!("TypeVar_{}_{}_{}", self.get_scope_name(), t, var);
        Type::Var(Arc::new(tname))
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
        self._function.checked_failures.remove(name)
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
    let mut s = Scope::new(&"hello".to_string());

    Scope::push_function_scope(&mut s, &"world".to_string(), Val::Void);
    assert_eq!("hello.__script__.world".to_string(), s.get_scope_name());

    Scope::pop_function_scope(&mut s);
    assert_eq!("hello.__script__".to_string(), s.get_scope_name());
}

#[test]
fn test_macro_defined()
{
    let mut s = Scope::new(&"hello".to_string());
    let macro_name = "world".to_string();
    s.define_macro(&macro_name, vec![], Val::Void);
    assert!(s.is_macro(&macro_name));

    Scope::push_function_scope(&mut s, &"foo".to_string(), Val::Void);
    assert!(s.is_macro(&macro_name));
}

#[test]
fn test_label_assigned()
{
    let mut s = Scope::new(&"hello".to_string());
    let label_name = "world".to_string();
    s.assign_label(Reg::new_param(2), &label_name, Type::Int);
    assert!(s.is_label(&label_name));

    Scope::push_function_scope(&mut s, &"world".to_string(), Val::Void);
    assert!(s.is_label(&label_name));
    assert!(s.lookup_label(&label_name).is_some());
}

}
