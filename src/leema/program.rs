use leema::ast::Ast;
use leema::code::{self, Code};
use leema::inter::Intermod;
use leema::ixpr::Source;
use leema::lib_str;
use leema::loader::Interloader;
use leema::log;
use leema::lri::Lri;
use leema::lstr::Lstr;
use leema::module::{ModulePreface, ModuleSource};
use leema::phase0::{self, Protomod};
use leema::typecheck::{self, CallFrame, CallOp, Typemod, Typescope};
use leema::val::Type;
use leema::{file, prefab, tcp, udp};

use std::collections::{HashMap, HashSet};
use std::io::Write;
use std::rc::Rc;


pub struct Lib
{
    loader: Interloader,
    modsrc: HashMap<String, ModuleSource>,
    preface: HashMap<String, Rc<ModulePreface>>,
    proto: HashMap<String, Rc<Protomod>>,
    inter: HashMap<Lstr, Intermod>,
    typed: HashMap<String, Typemod>,
    rust_load: HashMap<String, fn(&str) -> Option<code::Code>>,
    code: HashMap<String, HashMap<String, Code>>,
}

impl Lib
{
    pub fn new(l: Interloader) -> Lib
    {
        let mut proglib = Lib {
            loader: l,
            modsrc: HashMap::new(),
            preface: HashMap::new(),
            proto: HashMap::new(),
            inter: HashMap::new(),
            typed: HashMap::new(),
            rust_load: HashMap::new(),
            code: HashMap::new(),
        };
        proglib
            .rust_load
            .insert("prefab".to_string(), prefab::load_rust_func);
        proglib.load_inter("prefab");
        proglib
            .rust_load
            .insert("file".to_string(), file::load_rust_func);
        proglib
            .rust_load
            .insert("str".to_string(), lib_str::load_rust_func);
        proglib
            .rust_load
            .insert("tcp".to_string(), tcp::load_rust_func);
        proglib
            .rust_load
            .insert("udp".to_string(), udp::load_rust_func);
        proglib
    }

    /**
     * Idempotent function to initialize a Typemod for a given module
     */
    pub fn init_typed(&mut self, inter: &Intermod)
    {
        if !self.typed.contains_key(inter.modname.str()) {
            self.typed.insert(
                String::from(&inter.modname),
                Typemod::new(inter.modname.clone()),
            );
        }
        let typed = self.typed.get_mut(inter.modname.str()).unwrap();

        for (name, fix) in inter.interfunc.iter() {
            let name_lstr = Lstr::from(name.clone());
            typed.set_type(name_lstr, typecheck::Depth::Inter, fix.typ.clone());
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
                vout!("code for {}::{} is {:?}\n", modname, funcname, new_code);
            }
            let modlstr = Lstr::from(String::from(modname));
            let funclstr = Lstr::from(String::from(funcname));
            let funcri = Lri::with_modules(modlstr, funclstr);
            self.typecheck(&funcri, typecheck::Depth::One);

            if has_mod {
                let old_mod = self.code.get_mut(modname).unwrap();
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

    pub fn load_inter(&mut self, modname: &str)
    {
        if !self.inter.contains_key(modname) {
            let inter = self.read_inter(modname);
            let mod_lstr = Lstr::from(String::from(modname));
            self.init_typed(&inter);
            self.inter.insert(mod_lstr.clone(), inter);
        }
    }

    pub fn load_proto(&mut self, modname: &str)
    {
        if !self.proto.contains_key(modname) {
            let proto = self.read_proto(modname);

            let mut tmod = Typemod::new(Lstr::from(String::from(modname)));
            tmod.import_phase0(&proto);
            self.typed.insert(String::from(modname), tmod);

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
        vout!("read_inter({})\n", modname);
        self.load_proto(modname);
        let preface = self.preface.get(modname).unwrap().clone();
        let imports = self.import_protos(&preface.imports);
        let proto = self.proto.get(modname).unwrap();
        let mod_lstr = Lstr::from(String::from(modname));
        let mut typed = Typemod::new(mod_lstr.clone());
        let inter = Intermod::compile(&proto, &imports, &mut typed);
        self.typed
            .insert(String::from(modname), Typemod::new(mod_lstr.clone()));
        inter
    }

    pub fn read_code(&mut self, modname: &str, funcname: &str) -> Code
    {
        vout!("read_code({}::{})\n", modname, funcname);
        self.load_inter(modname);

        let inter = self
            .inter
            .get(modname)
            .or_else(|| {
                panic!("cannot compile missing module {}", modname);
            }).unwrap();
        let fix = inter
            .interfunc
            .get(funcname)
            .or_else(|| {
                panic!(
                    "cannot compile missing function {}::{}",
                    modname, funcname
                );
            }).unwrap();
        if modname == "prefab" {
            vout!("prefab::{} fix: {:?}\n", funcname, fix);
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
                vout!("prefab::{} ops: {:?}\n", funcname, ops);
            }
            Code::Leema(ops)
        }
    }

    pub fn typecheck(&mut self, funcri: &Lri, depth: typecheck::Depth)
    {
        vout!("typecheck({}, {:?})\n", funcri, depth);
        self.load_inter(
            funcri.mod_ref().expect("no typecheck module name").str(),
        );
        if depth.one_deeper() {
            self.deeper_typecheck(funcri, depth);
        }

        let ftype = self.local_typecheck(funcri);
        let modstr = funcri.mod_ref().unwrap().str();
        let mutyped = self.typed.get_mut(modstr).unwrap();
        mutyped.set_type(funcri.localid.clone(), depth, ftype);
        vout!("\tfinish typecheck({})\n", funcri);
    }

    pub fn deeper_typecheck(&mut self, funcri: &Lri, depth: typecheck::Depth)
    {
        let cf = {
            let mod_str = funcri.mod_ref().expect("typecheck module name");
            let mut icf = CallFrame::new(mod_str, funcri.localid.str());
            let inter = self.inter.get(mod_str).unwrap();
            let fix = inter.interfunc.get(funcri.localid.str()).unwrap();
            icf.collect_calls(&fix);
            icf
        };
        for c in cf.calls.iter() {
            match c {
                &CallOp::LocalCall(ref call_name) => {
                    let contains_local = {
                        let local_inter =
                            self.inter.get(funcri.localid.str()).unwrap();
                        local_inter.interfunc.contains_key(&**call_name)
                    };
                    if contains_local {
                        if funcri.localid.str() != &**call_name {
                            self.typecheck(
                                &Lri::full(
                                    funcri.modules.clone(),
                                    call_name.clone(),
                                    None,
                                ),
                                depth.next(),
                            );
                        }
                    } else {
                        self.typecheck(
                            &Lri::with_modules(
                                Lstr::Sref("prefab"),
                                call_name.clone(),
                            ),
                            depth.next(),
                        );
                    }
                }
                &CallOp::ExternalCall(ref ext) => {
                    if funcri == ext {
                        // do nothing, it's recursive, we're already doing it
                    } else {
                        self.typecheck(&ext, depth.next());
                    }
                }
            }
        }
    }

    pub fn local_typecheck(&mut self, funcri: &Lri) -> Type
    {
        vout!("local_typecheck({})\n", funcri);
        let modlstr = funcri.mod_ref().unwrap();
        let funclstr = &funcri.localid;
        let opt_inter = self.inter.get(modlstr);
        if opt_inter.is_none() {
            panic!("cannot find inter for {}", funcri);
        }
        let inter = opt_inter.unwrap();
        let fix = inter.interfunc.get(funclstr.str()).unwrap();
        let typed = self.typed.get(modlstr.str()).unwrap();

        let pref = self.find_preface(modlstr).unwrap().clone();
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

        let opt_proto = self.proto.get(modlstr.str());
        let mut scope =
            Typescope::new(typed, opt_proto.unwrap(), funclstr.str(), &imports);
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

    fn import_protos(
        &mut self,
        imports: &HashSet<String>,
    ) -> HashMap<String, Rc<Protomod>>
    {
        let mut imported_protos = HashMap::new();
        imported_protos.insert(
            String::from("prefab"),
            self.proto.get("prefab").unwrap().clone(),
        );
        for i in imports {
            self.load_proto(i);
            let p = self.proto.get(i).unwrap().clone();
            imported_protos.insert(i.clone(), p);
        }
        imported_protos
    }

    pub fn get_macro<'a>(
        &'a self,
        modname: &str,
        macname: &str,
    ) -> Option<&'a Ast>
    {
        match self.preface.get(modname) {
            Some(pref) => pref.macros.get(macname),
            None => None,
        }
    }
}
