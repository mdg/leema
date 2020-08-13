use crate::leema::ast2::LocalType;
use crate::leema::failure::Lresult;
use crate::leema::reg::{Reg, RegTab};

use std::collections::HashMap;


#[derive(Debug)]
pub struct Blockscope
{
    vars: RegTab,
}

impl Blockscope
{
    pub fn new() -> Blockscope
    {
        Blockscope {
            vars: RegTab::new(vec![]),
        }
    }
}

#[derive(Debug)]
pub struct LocalVar
{
    name: &'static str,
    var_type: LocalType,
    num_reassignments: i16,
    first_assign: i16,
    first_access: Option<i16>,
    last_assign: i16,
    last_access: Option<i16>,
}

impl LocalVar
{
    pub fn new(name: &'static str, vt: LocalType) -> LocalVar
    {
        LocalVar {
            name,
            var_type: vt,
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
    locals: HashMap<&'static str, LocalVar>,
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

    pub fn assign_var(&mut self, id: &'static str, vt: LocalType) -> Lresult<Reg>
    {
        if let Some(r) = self.stack.last().unwrap().vars.with_name(id) {
            // since this var is already in the current stack
            // reassign to the current register
            return Ok(r);
        }

        if let Some(var_data) = self.locals.get_mut(id) {
            if var_data.var_type == LocalType::Param {
                return Err(rustfail!(
                    "compile_error",
                    "cannot reassign a function parameter: {}",
                    id,
                ));
            }
            var_data.num_reassignments += 1;
            return Ok(self.var_in_scope(id).unwrap());
        }

        let new_var = LocalVar::new(id.clone(), vt);
        self.locals.insert(id.clone(), new_var);
        Ok(self.current_block_mut().vars.new_name(id))
    }

    pub fn access_var(&mut self, id: &'static str, lineno: i16)
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

    pub fn var_in_scope(&self, id: &'static str) -> Option<Reg>
    {
        for s in self.stack.iter() {
            if let Some(r) = s.vars.with_name(id) {
                return Some(r);
            }
        }
        None
    }
}
