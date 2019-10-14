use crate::leema::lstr::Lstr;

use std::collections::{HashMap, HashSet};


#[derive(Debug)]
pub struct Blockscope
{
    vars: HashSet<Lstr>,
}

impl Blockscope
{
    pub fn new() -> Blockscope
    {
        Blockscope {
            vars: HashSet::new(),
        }
    }
}

#[derive(Copy)]
#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub enum LocalType
{
    Param,
    Match,
    Let,
}

#[derive(Debug)]
pub struct LocalVar
{
    name: Lstr,
    var_type: LocalType,
    num_scopes: i16,
    num_reassignments: i16,
    first_assign: i16,
    first_access: Option<i16>,
    last_assign: i16,
    last_access: Option<i16>,
}

impl LocalVar
{
    pub fn new(name: Lstr, vt: LocalType) -> LocalVar
    {
        LocalVar {
            name,
            var_type: vt,
            num_scopes: 1,
            num_reassignments: 0,
            first_assign: 0,
            first_access: None,
            last_assign: 0,
            last_access: None,
        }
    }
}

#[derive(Debug)]
pub struct Blockstack
{
    stack: Vec<Blockscope>,
    locals: HashMap<Lstr, LocalVar>,
    in_failed: bool,
}

impl Blockstack
{
    pub fn new() -> Blockstack
    {
        Blockstack {
            stack: vec![Blockscope::new()],
            locals: HashMap::new(),
            in_failed: false,
        }
    }

    pub fn push_blockscope(&mut self)
    {
        self.stack.push(Blockscope::new());
    }

    pub fn pop_blockscope(&mut self)
    {
        self.stack.pop();
    }

    pub fn current_block_mut(&mut self) -> &mut Blockscope
    {
        self.stack.last_mut().unwrap()
    }

    pub fn assign_var(&mut self, id: &Lstr, vt: LocalType)
    {
        if self.var_in_scope(id) {
            let var_data = self.locals.get_mut(id).unwrap();
            match (var_data.var_type, vt) {
                (LocalType::Let, LocalType::Let) => {
                    var_data.num_reassignments += 1;
                }
                (LocalType::Param, LocalType::Let) => {
                    panic!("cannot reassign a function parameter: {}", id);
                }
                (LocalType::Match, LocalType::Let) => {
                    panic!("cannot reassign a pattern variable: {}", id);
                }
                _ => {
                    // matching on an existing variable, that's cool
                }
            }
        } else {
            let new_var = LocalVar::new(id.clone(), vt);
            self.locals.insert(id.clone(), new_var);
            self.current_block_mut().vars.insert(id.clone());
        }
    }

    pub fn access_var(&mut self, id: &Lstr, lineno: i16)
    {
        let opt_local = self.locals.get_mut(id);
        if opt_local.is_none() {
            panic!("cannot access undefined var: {}", id);
        }
        let vi = opt_local.unwrap();
        if vi.first_access.is_none() {
            vi.first_access = Some(lineno)
        }
        vi.last_access = Some(lineno)
    }

    pub fn var_in_scope(&self, id: &Lstr) -> bool
    {
        self.stack.iter().any(|bs| bs.vars.contains(id))
    }
}
