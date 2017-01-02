use leema::inter::{Version, Intermod};
use leema::module::{ModuleSource, ModuleInterface, ModulePreface, MacroDef};
use leema::loader::{Interloader};
use leema::phase0::{self, Protomod};

use std::rc::{Rc};
use std::collections::{HashMap, HashSet};


pub struct Lib
{
    loader: Interloader,
    modsrc: HashMap<String, ModuleSource>,
    preface: HashMap<String, Rc<ModulePreface>>,
    proto: HashMap<String, Rc<Protomod>>,
}

impl Lib
{
    pub fn new(l: Interloader) -> Lib
    {
        Lib{
            loader: l,
            modsrc: HashMap::new(),
            preface: HashMap::new(),
            proto: HashMap::new(),
        }
    }

    pub fn main_module(&self) -> &str
    {
        &self.loader.main_mod
    }

    pub fn load_module(&mut self, modname: &str)
    {
        if !self.proto.contains_key(modname) {
            let m = self.read_module(modname);
            let pref = ModulePreface::new(&m);
            self.load_imports(modname, &pref.imports);
            let p0 = phase0::preproc(self, &pref, &m.ast);
            self.modsrc.insert(String::from(modname), m);
            self.preface.insert(String::from(modname), Rc::new(pref));
            self.proto.insert(String::from(modname), Rc::new(p0));
        }
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
            if self.preface.contains_key(i) {
                continue;
            }
            let im = self.read_module(i);
            let pref = ModulePreface::new(&im);
            self.modsrc.insert(i.clone(), im);
            self.preface.insert(i.clone(), Rc::new(pref));
        }
    }

    pub fn get_macro(&self, modname: &str, macname: &str)
            -> Option<&MacroDef>
    {
        match self.preface.get(modname) {
            Some(pref) => pref.macros.get(macname),
            None => None,
        }
    }
}
