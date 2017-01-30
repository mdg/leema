use leema::iexpr::{Iexpr};
use leema::inter::{self, Version, Intermod};
use leema::module::{ModuleSource, ModuleInterface, ModulePreface, MacroDef};
use leema::loader::{Interloader};
use leema::phase0::{self, Protomod};
use leema::prefab;
use leema::typecheck::{self, CallOp, CallFrame, Typescope};

use std::rc::{Rc};
use std::collections::{HashMap, HashSet};


pub struct Lib
{
    loader: Interloader,
    modsrc: HashMap<String, ModuleSource>,
    preface: HashMap<String, Rc<ModulePreface>>,
    proto: HashMap<String, Rc<Protomod>>,
    inter: HashMap<String, Rc<Intermod>>,
    typed: HashMap<(String, String), Iexpr>,
}

impl Lib
{
    pub fn new(mut l: Interloader) -> Lib
    {
        l.set_mod_txt("prefab", String::from(prefab::source_code()));
        let mut proglib = Lib{
            loader: l,
            modsrc: HashMap::new(),
            preface: HashMap::new(),
            proto: HashMap::new(),
            inter: HashMap::new(),
            typed: HashMap::new(),
        };
        proglib.load_module("prefab");
        proglib
    }

    pub fn main_module(&self) -> &str
    {
        &self.loader.main_mod
    }

    pub fn typecheck_main(&mut self, modname: &str, funcname: &str)
    {
            /*
        if !self.typed.contains_key(&(modname, funcname)) {
            self.deep_typecheck(modname, funcname);
            self.typed.insert(
                (String::from(modname), String::from(funcname)),
                ifunc,
            );
        }
            */
    }

    pub fn load_module(&mut self, modname: &str)
    {
        self.load_inter(modname)
    }

    pub fn load_inter(&mut self, modname: &str)
    {
        if !self.inter.contains_key(modname) {
            let inter = self.read_inter(modname);
            self.inter.insert(String::from(modname), Rc::new(inter));
        }
    }

    pub fn load_proto(&mut self, modname: &str)
    {
        if !self.proto.contains_key(modname) {
            let proto = self.read_proto(modname);
            self.proto.insert(String::from(modname), Rc::new(proto));
        }
    }

    pub fn load_preface(&mut self, modname: &str)
    {
        if !self.preface.contains_key(modname) {
            let (msrc, mpref) = self.read_preface(modname);
            self.modsrc.insert(String::from(modname), msrc);
            self.preface.insert(String::from(modname), Rc::new(mpref));
        }
    }

    pub fn read_modsrc(&self, modname: &str) -> ModuleSource
    {
        let modkey = self.loader.mod_name_to_key(modname);
        let modtxt = self.loader.read_module(&modkey);
        ModuleSource::new(modkey, modtxt)
    }

    pub fn read_preface(&self, modname: &str) -> (ModuleSource, ModulePreface)
    {
        let ms = self.read_modsrc(modname);
        let pref = ModulePreface::new(&ms);
        (ms, pref)
    }

    pub fn read_proto(&mut self, modname: &str) -> Protomod
    {
        let (ms, pref) = self.read_preface(modname);
        self.load_imports(modname, &pref.imports);
        let proto = phase0::preproc(self, &pref, &ms.ast);
        self.modsrc.insert(String::from(modname), ms);
        self.preface.insert(String::from(modname), Rc::new(pref));
        proto
    }

    pub fn read_inter(&mut self, modname: &str) -> Intermod
    {
        self.load_proto(modname);
        let preface = self.preface.get(modname).unwrap().clone();
        let imports = self.import_protos(modname, &preface.imports);
        let proto = self.proto.get(modname).unwrap();
        Intermod::compile(&proto, &imports)
    }

    pub fn deep_typecheck<'a>(&mut self, modname: &'a str, funcname: &'a str)
    {
        self.load_inter(modname);

        let inter = self.inter.get(modname).unwrap().clone();
        let fix = inter.interfunc.get(funcname).unwrap();
        let mut cf = CallFrame::new(modname, funcname);
        cf.collect_calls(fix);
        for c in cf.calls.iter() {
            match c {
                &CallOp::LocalCall(ref call_name) => {
                    if inter.interfunc.contains_key(&**call_name) {
                        if *funcname != **call_name {
                            self.deep_typecheck(modname, call_name);
                        }
                    } else {
                        self.deep_typecheck("prefab", call_name);
                    }
                }
                &CallOp::ExternalCall(ref extmod, ref extfunc) => {
                    self.deep_typecheck(extmod, extfunc);
                }
            }
        }

        let pref = self.preface.get(modname).unwrap().clone();
        let imports = {
            let mut imps = HashMap::new();
            let prefab = self.inter.get("prefab").unwrap().clone();
            imps.insert(String::from("prefab"), prefab);
            for i in pref.imports.iter() {
                self.load_inter(i);
                imps.insert(i.clone(), self.inter.get(i).unwrap().clone());
            }
        };
    }

    /*
    pub fn typecheck_deeper(scope: &mut Typescope)
    {
        if let call = scope.pop_call() {
            match call {
                &CallOp::LocalCall(funcnm) => {
                }
                &CallOp::ExternalCall(modnm, funcnm) => {
                }
            }
        }
    }
    */

    fn load_imports(&mut self, modname: &str, imports: &HashSet<String>)
    {
        for i in imports {
            if i == modname {
                panic!("A module cannot import itself: {}", i);
            }
            if self.preface.contains_key(i) {
                continue;
            }
            let im = self.read_modsrc(i);
            let pref = ModulePreface::new(&im);
            self.modsrc.insert(i.clone(), im);
            self.preface.insert(i.clone(), Rc::new(pref));
        }
    }

    fn import_protos(&mut self, modname: &str, imports: &HashSet<String>)
            -> HashMap<String, Rc<Protomod>>
    {
        let mut imported_protos = HashMap::new();
        imported_protos.insert(String::from("prefab")
                , self.proto.get("prefab").unwrap().clone());
        for i in imports {
            self.load_proto(i);
            let p = self.proto.get(i).unwrap().clone();
            imported_protos.insert(i.clone(), p);
        }
        imported_protos
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
