use leema::val::{Val,SexprType,Type};
use leema::list;
use leema::log;
use leema::reg::{Reg, Ireg};
use leema::sexpr;
use std::collections::{HashMap};
use std::sync::Arc;
use std::io::{stderr, Write};
use std::mem;

#[derive(Debug)]
pub struct Scope
{
    parent: Option<Box<Scope>>,
    scope_name: String,
    func_name: String,
    // macros
    m: HashMap<Arc<String>, (Vec<Arc<String>>, Val)>,
    // registers of locally defined labels
    E: HashMap<Arc<String>, Reg>,
    // types of locally defined labels
    T: HashMap<Arc<String>, Type>,
    // probably should move this to its own data structre and maybe module
    // to handle updates and multiple possible type options
    inferred: HashMap<Arc<String>, Type>,
    // type definitins in type namespace
    typespace: HashMap<Arc<String>, Type>,
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
            typespace: HashMap::new(),
            _nextreg: 0,
        }
    }

    pub fn push_child(parent: &mut Scope, func_nm: String)
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
            typespace: HashMap::new(),
            _nextreg: 0,
        };
        mem::swap(parent, &mut tmp_scope);
        parent.parent = Some(Box::new(tmp_scope));
    }
}
