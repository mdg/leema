use crate::leema::ast2::LocalType;
use crate::leema::failure::{self, Lresult};
use crate::leema::lstr::Lstr;
use crate::leema::reg::{Reg, RegTab};

use std::collections::HashMap;

#[derive(Debug)]
pub struct Blockscope
{
    vars: RegTab,
}

impl Blockscope
{
    pub fn push(&self) -> Blockscope
    {
        Blockscope {
            vars: self.vars.push(),
        }
    }

    pub fn with_args(args: Vec<&'static str>) -> Blockscope
    {
        Blockscope {
            vars: RegTab::with_args(args),
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
    not_in_scope: HashMap<&'static str, i16>,
    in_failed: bool,
}

impl Blockstack
{
    pub fn with_args(args: Vec<&'static str>) -> Blockstack
    {
        Blockstack {
            stack: vec![Blockscope::with_args(args)],
            locals: HashMap::new(),
            not_in_scope: HashMap::new(),
            in_failed: false,
        }
    }

    pub fn push_blockscope(&mut self)
    {
        let new_blockscope = self.stack.last().unwrap().push();
        self.stack.push(new_blockscope);
    }

    pub fn pop_blockscope(&mut self)
    {
        self.stack.pop();
    }

    pub fn current_block_mut(&mut self) -> &mut Blockscope
    {
        self.stack.last_mut().unwrap()
    }

    /// all the variables used out of scope
    pub fn out_of_scope(&self) -> &HashMap<&'static str, i16>
    {
        &self.not_in_scope
    }

    pub fn assign_var(
        &mut self,
        id: &'static str,
        vt: LocalType,
    ) -> Lresult<Reg>
    {
        if self.not_in_scope.contains_key(id) {
            return Err(lfail!(
                failure::Mode::Scope,
                "cannot re-assign variable",
                "variable": ldisplay!(id),
            ));
        }

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
            return self.var_in_scope(id).ok_or_else(|| {
                lfail!(
                    failure::Mode::Scope,
                    "var not in scope",
                    "var": Lstr::Sref(id),
                )
            });
        }

        let new_var = LocalVar::new(id, vt);
        self.locals.insert(id, new_var);
        Ok(self.current_block_mut().vars.new_name(id))
    }

    pub fn access_var(&mut self, id: &'static str, lineno: i16)
    {
        let opt_local = self.locals.get_mut(id);
        if opt_local.is_none() {
            self.not_in_scope.entry(id).or_insert(lineno);
            return;
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

#[cfg(test)]
mod tests
{
    use super::Blockstack;
    use crate::leema::ast2::LocalType;

    #[test]
    fn collect_access_out_of_scope()
    {
        let mut b = Blockstack::with_args(vec![]);
        b.access_var("a", 5);
        b.access_var("b", 6);
        b.access_var("a", 7);
        let out = b.out_of_scope();
        assert_eq!(5, *out.get("a").unwrap());
        assert_eq!(6, *out.get("b").unwrap());
        assert_eq!(2, out.len());
    }

    #[test]
    fn assign_not_in_scope()
    {
        let mut b = Blockstack::with_args(vec![]);
        b.access_var("a", 5);
        let result = b.assign_var("a", LocalType::Let);
        assert!(result.is_err());
    }
}
