use leema::inter::{Version, Intermod};
use leema::module::{Module};
use leema::loader::{Interloader};

use std::rc::{Rc};
use std::collections::{HashMap};


pub struct Lib
{
    loader: Interloader,
    module: HashMap<String, Rc<Module>>,
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

    pub fn load_module(&mut self, modname: &str) -> Rc<Module>
    {
        if ! self.module.contains_key(modname) {
            self.read_module(modname);
        }
        self.load_imports(modname);
        self.module.get(modname).unwrap().clone()
    }

    pub fn read_module(&mut self, modname: &str)
    {
        let modkey = self.loader.mod_name_to_key(modname);
        let mut new_mod = self.loader.init_module(modkey);
        new_mod.load();
        self.module.insert(String::from(modname), Rc::new(new_mod));
    }

    pub fn load_imports(&mut self, modname: &str)
    {
        let m = self.module.get(modname).unwrap();
    }

    pub fn add_mod(&mut self, m: Module) -> Rc<Module>
    {
        // self.module.insert(m.key.name.clone(), m);
        let rcm = Rc::new(m);
        if !self.module.contains_key(&rcm.key.name) {
            self.module.insert(rcm.key.name.clone(), rcm.clone());
        }
        rcm
    }

    pub fn get_mod(&self, modname: &str) -> Rc<Module>
    {
        self.module.get(modname).unwrap().clone()
    }
}
