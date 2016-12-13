use leema::inter::{Version, Intermod};
use leema::module::{Module};
use leema::loader::{Interloader};

use std::collections::{HashMap};


pub struct Lib
{
    loader: Interloader,
    module: HashMap<String, Module>,
}

impl Lib
{
    pub fn new(l: Interloader) -> Lib
    {
        Lib{
            loader: l,
            module: HashMap::new(),
        }
    }

    pub fn has_mod(&mut self, modname: &str) -> bool
    {
        self.module.contains_key(modname)
    }

    pub fn add_mod(&mut self, m: Module) -> &mut Module
    {
        // self.module.insert(m.key.name.clone(), m);
        self.module.entry(m.key.name.clone()).or_insert(m)
    }

    pub fn get_mod(&self, modname: &str) -> &Module
    {
        self.module.get(modname).unwrap()
    }

    pub fn get_mod_mut(&mut self, modname: &str) -> &mut Module
    {
        self.module.get_mut(modname).unwrap()
    }
}
