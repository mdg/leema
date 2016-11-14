use leema::inter::{Version, Intermod};
use leema::module::{Module};
use std::collections::{HashMap};


pub struct Lib
{
    module: HashMap<String, Module>,
}

impl Lib
{
    pub fn new() -> Lib
    {
        Lib{
            module: HashMap::new(),
        }
    }

    pub fn add_mod(&mut self, m: Module)
    {
        self.module.insert(m.key.name.clone(), m);
    }
}
