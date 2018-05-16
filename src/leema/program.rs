use leema::code::{self, Code};
use leema::ixpr::{Ixpr, Source};
use leema::inter::{Intermod};
use leema::module::{ModuleSource, ModulePreface, MacroDef};
use leema::loader::{Interloader};
use leema::log;
use leema::lstr::{Lstr};
use leema::phase0::{self, Protomod};
use leema::{prefab, file, udp, tcp};
use leema::typecheck::{self, Typemod, CallFrame, CallOp, Typescope};
use leema::val::{Type};
use leema::{lib_str};

use std::io::{Write};
use std::rc::{Rc};
use std::collections::{HashMap, HashSet};


pub struct Lib
{
    loader: Interloader,
    modsrc: HashMap<String, ModuleSource>,
    preface: HashMap<String, Rc<ModulePreface>>,
    proto: HashMap<String, Rc<Protomod>>,
    interfunc: HashMap<(Lstr, Lstr), Ixpr>,
    typed: HashMap<String, Typemod>,
    rust_load: HashMap<String, fn(&str) -> Option<code::Code>>,
    code: HashMap<String, HashMap<String, Code>>,
}

impl Lib
{
    pub fn new(mut l: Interloader) -> Lib
    {
        let mut proglib = Lib{
            loader: l,
            modsrc: HashMap::new(),
            preface: HashMap::new(),
            proto: HashMap::new(),
            interfunc: HashMap::new(),
            typed: HashMap::new(),
            rust_load: HashMap::new(),
            code: HashMap::new(),
        };
        proglib.rust_load.insert("prefab".to_string(), prefab::load_rust_func);
        proglib.load_proto("prefab");
        proglib.rust_load.insert("file".to_string(), file::load_rust_func);
        proglib.rust_load.insert("str".to_string(), lib_str::load_rust_func);
        proglib.rust_load.insert("tcp".to_string(), tcp::load_rust_func);
        proglib.rust_load.insert("udp".to_string(), udp::load_rust_func);
        proglib
    }

    /**
     * Idempotent function to initialize a Typemod for a given module
     */
    pub fn init_typed(&mut self, modname: Lstr)
    {
        if !self.typed.contains_key(modname.str()) {
            self.typed.insert(
                String::from(modname.str()),
                Typemod::new(modname),
            );
        }
    }

    pub fn main_module(&self) -> &str
    {
        &self.loader.main_mod
    }

    pub fn load_code(&mut self, modname: &str, funcname: &str) -> &Code
    {
        let (has_mod, has_func) = if self.code.contains_key(modname) {
            let old_mod = self.code.get(modname).unwrap();
            (true, old_mod.contains_key(funcname))
        } else {
            (false, false)
        };

        if !has_func {
            let new_code = self.read_code(modname, funcname);
            if modname == "prefab" {
                vout!("code for {}.{} is {:?}\n", modname, funcname, new_code);
            }

            if has_mod {
                let mut old_mod = self.code.get_mut(modname).unwrap();
                old_mod.insert(String::from(funcname), new_code);
            } else {
                let mut new_mod = HashMap::new();
                new_mod.insert(String::from(funcname), new_code);
                self.code.insert(String::from(modname), new_mod);
            }
        }

        self.code.get(modname).unwrap().get(funcname).unwrap()
    }

    pub fn find_preface(&self, modname: &str) -> Option<&Rc<ModulePreface>>
    {
        self.preface.get(modname)
    }

    pub fn find_interfunc(&self, modname: &Lstr, funcname: &Lstr
        ) -> Option<&Ixpr>
    {
        let modfunc = (modname.clone(), funcname.clone());
        self.interfunc.get(&modfunc)
    }

    pub fn load_inter(&mut self, modname: &str, funcname: &str)
    {
        let mod_lstr = Lstr::from(String::from(modname));
        let modfunc = (mod_lstr.clone(), Lstr::from(String::from(funcname)));
        if !self.interfunc.contains_key(&modfunc) {
            let fix = self.read_inter(modname, funcname);
            self.interfunc.insert(modfunc, fix);

            self.init_typed(Lstr::from(String::from(modname)));
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

    pub fn read_inter(&mut self, modname: &str, funcname: &str) -> Ixpr
    {
        vout!("read_inter({}.{})\n", modname, funcname);
        self.load_proto(modname);
        let preface = self.preface.get(modname).unwrap().clone();
        let imports = self.import_protos(modname, &preface.imports);
        let proto = self.proto.get(modname).unwrap();
        Intermod::compile(&proto, &imports, funcname)
    }

    pub fn read_code(&mut self, modname: &str, funcname: &str) -> Code
    {
        vout!("read_code({}.{})\n", modname, funcname);
        self.load_inter(modname, funcname);

        let modfunc = (
            Lstr::from(String::from(modname)),
            Lstr::from(String::from(funcname)),
        );
        let fix = self.interfunc.get(&modfunc)
            .or_else(|| {
                panic!("Cannot find function {}::{}", modname, funcname);
            })
            .unwrap();
        if modname == "prefab" {
            vout!("prefab.{} fix: {:?}\n", funcname, fix);
        }
        if fix.src == Source::RustBlock {
            let rust_loader = self.rust_load.get(modname);
            if rust_loader.is_none() {
                panic!("no rust loader for: {}", modname);
            }
            let rustfunc = rust_loader.unwrap()(funcname);
            if rustfunc.is_none() {
                panic!("no rust function for: {}::{}", modname, funcname);
            }
            rustfunc.unwrap()
        } else {
            let ops = code::make_ops(fix);
            if modname == "prefab" {
                vout!("prefab.{} ops: {:?}\n", funcname, ops);
            }
            Code::Leema(ops)
        }
    }

    pub fn deep_typecheck(&mut self
        , modname: &str, funcname: &str
        )
    {
        vout!("deep_");

        println!("typecheck {}::{}", modname, funcname);
        self.load_inter(modname, funcname);

        let modlstr = Lstr::from(String::from(modname));
        let funclstr = Lstr::from(String::from(funcname));
        let fix = self.find_interfunc(&modlstr, &funclstr).unwrap().clone();
        let mut cf = CallFrame::new(modname, funcname);
        cf.collect_calls(&fix);
        for c in cf.calls.iter() {
            match c {
                &CallOp::LocalCall(ref call_name) => {
                    let callstr = Lstr::from(&**call_name);
                    if self.find_interfunc(&modlstr, &callstr).is_some() {
                        if *funcname != **call_name {
                            self.deep_typecheck(modname, call_name);
                        }
                    } else {
                        self.deep_typecheck("prefab", &**call_name);
                    }
                }
                &CallOp::ExternalCall(ref extmod, ref extfunc)
                    if modname == &**extmod && funcname == &**extfunc
                => {
                    // do nothing, it's recursive, we're already doing it
                }
                &CallOp::ExternalCall(ref extmod, ref extfunc) => {
                    self.deep_typecheck(extmod, extfunc);
                }
            }
        }

        self.typed.get_mut(modname).unwrap().set_type(
            funclstr.clone(), typecheck::Depth::D1, fix.typ.clone());

        let ftype = self.deep_typecheck_function(modname, funcname);
        let mutyped = self.typed.get_mut(modname).unwrap();
        mutyped.set_type(funclstr, typecheck::Depth::Full, ftype);
    }

    pub fn deep_typecheck_function(&mut self
        , modname: &str, funcname: &str
        ) -> Type
    {
        vout!("deep_typecheck_function({}::{})\n", modname, funcname);
        let modlstr = Lstr::from(String::from(modname));
        let funclstr = Lstr::from(String::from(funcname));
        let fix = self.find_interfunc(&modlstr, &funclstr).unwrap();
        let typed = self.typed.get(modname).unwrap();

        let pref = self.find_preface(modname).unwrap().clone();
        let prefab = self.typed.get("prefab");
        let mut imports: HashMap<String, &Typemod> = HashMap::new();
        if prefab.is_some() {
            imports.insert(String::from("prefab"), prefab.unwrap());
        }
        for i in pref.imports.iter() {
            let iii: Option<&Typemod> = self.typed.get(i);
            if iii.is_none() {
                panic!("cannot find intermod in imports: {}", i);
            }
            imports.insert(i.clone(), iii.unwrap());
        }

        let mut scope = Typescope::new(typed, funcname, &imports);
        typecheck::typecheck_function(&mut scope, fix).unwrap()
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
