use leema::inter::{Version, Intermod};
use leema::module::{ModuleSource, ModuleInterface};
use leema::loader::{Interloader};

use std::rc::{Rc};
use std::collections::{HashMap};


pub struct Lib
{
    loader: Interloader,
    modsrc: HashMap<String, Option<ModuleSource>>,
    modifc: HashMap<String, Rc<ModuleInterface>>,
}

impl Lib
{
    pub fn new(l: Interloader) -> Lib
    {
        Lib{
            loader: l,
            modsrc: HashMap::new(),
            modifc: HashMap::new(),
        }
    }

    pub fn load_module(&mut self, modname: &str) -> ModuleSource
    {
        if !self.modsrc.contains_key(modname) {
            let mut m = self.read_module(modname);
            self.load_imports(&mut m);
            self.modsrc.insert(String::from(modname), None);
            m
        } else {
            let msrc = match self.modsrc.get_mut(modname);
            match msrc {
                Some(_) => {
                    msrc.take()
                }
                None => {
                    panic!("Module already in use: {}", modname);
                }
            }
        }
    }

    pub fn read_module(&mut self, modname: &str) -> ModuleSource
    {
        let modkey = self.loader.mod_name_to_key(modname);
        let mut new_mod = self.loader.init_module(modkey);
        new_mod.load();
        new_mod
    }

    pub fn load_imports(&mut self, m: &mut Module)
    {
        if m.imports_loaded {
            return;
        }
        for i in m.src.imports.iter() {
            if *i == m.key.name {
                panic!("A module cannot import itself: {}", i);
            }
            let im = self.read_module(i);
            self.module.insert(i.clone(), Rc::new(im));
        }
        m.imports_loaded = true;
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
