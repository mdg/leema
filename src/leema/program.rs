use leema::inter::{Version, Intermod};
use leema::module::{ModuleSource, ModuleInterface, ModulePreface};
use leema::loader::{Interloader};
use leema::phase0;

use std::rc::{Rc};
use std::collections::{HashMap, HashSet};


pub struct Lib
{
    loader: Interloader,
    modsrc: HashMap<String, Option<ModuleSource>>,
    modpre: HashMap<String, Rc<ModulePreface>>,
    modifc: HashMap<String, Rc<ModuleInterface>>,
}

impl Lib
{
    pub fn new(l: Interloader) -> Lib
    {
        Lib{
            loader: l,
            modsrc: HashMap::new(),
            modpre: HashMap::new(),
            modifc: HashMap::new(),
        }
    }

    pub fn main_module(&self) -> &str
    {
        &self.loader.main_mod
    }

    pub fn load_module(&mut self, modname: &str)
    {
        if !self.modsrc.contains_key(modname) {
            let m = self.read_module(modname);
            let pref = ModulePreface::new(&m);
            self.load_imports(modname, &pref.imports);
            let p0 = phase0::preproc(self, &pref, &m.ast);
            println!("phase0: {:?}", p0);
            self.modsrc.insert(String::from(modname), Some(m));
            self.modpre.insert(String::from(modname), Rc::new(pref));
        }
        /*
        else {
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
        */
    }

    fn read_module(&self, modname: &str) -> ModuleSource
    {
        let modkey = self.loader.mod_name_to_key(modname);
        let modtxt = self.loader.read_module(&modkey);
        ModuleSource::new(modkey, modtxt)
    }

    fn load_imports(&mut self, modname: &str, imports: &HashSet<String>)
    {
        for i in imports {
            if i == modname {
                panic!("A module cannot import itself: {}", i);
            }
            if self.modpre.contains_key(i) {
                continue;
            }
            let im = self.read_module(i);
            let pref = ModulePreface::new(&im);
            self.modsrc.insert(i.clone(), Some(im));
            self.modpre.insert(i.clone(), Rc::new(pref));
        }
    }
}
