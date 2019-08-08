use crate::leema::ast::Ast;
use crate::leema::ast2;
use crate::leema::code::{self, Code};
use crate::leema::failure::Lresult;
use crate::leema::inter::Intermod;
use crate::leema::ixpr::Source;
use crate::leema::lib_map;
use crate::leema::lib_str;
use crate::leema::loader::Interloader;
use crate::leema::lstr::Lstr;
use crate::leema::module::{ModKey, ModulePreface, ModuleSource};
use crate::leema::phase0::{self, Protomod};
use crate::leema::proto::{ProtoLib, ProtoModule};
use crate::leema::semantics::Semantics;
use crate::leema::typecheck::Typemod;
use crate::leema::val::Type;
use crate::leema::{
    file, lib_hyper, lib_io, lib_json, lib_list, lib_task, prefab, tcp, udp,
};

use std::collections::{HashMap, HashSet};
use std::rc::Rc;


pub struct Lib
{
    loader: Interloader,
    modsrc: HashMap<Lstr, ModuleSource>,
    preface: HashMap<Lstr, Rc<ModulePreface>>,
    proto: HashMap<Lstr, Rc<Protomod>>,
    protos: ProtoLib,
    semantics: Semantics,
    inter: HashMap<Lstr, Intermod>,
    typed: HashMap<Lstr, Typemod>,
    rust_load: HashMap<Lstr, fn(&str) -> Option<code::Code>>,
    code: HashMap<Lstr, HashMap<Lstr, Code>>,
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
            protos: ProtoLib::new(),
            semantics: Semantics::new(),
            inter: HashMap::new(),
            typed: HashMap::new(),
            rust_load: HashMap::new(),
            code: HashMap::new(),
        };
        proglib
            .rust_load
            .insert(Lstr::Sref("prefab"), prefab::load_rust_func);
        proglib
            .rust_load
            .insert(Lstr::Sref("file"), file::load_rust_func);
        proglib
            .rust_load
            .insert(Lstr::Sref("hyper_client"), lib_hyper::load_client_func);
        proglib
            .rust_load
            .insert(Lstr::Sref("hyper_server"), lib_hyper::load_rust_func);
        proglib
            .rust_load
            .insert(Lstr::Sref("io"), lib_io::load_rust_func);
        proglib
            .rust_load
            .insert(Lstr::Sref("json"), lib_json::load_rust_func);
        proglib
            .rust_load
            .insert(Lstr::Sref("list"), lib_list::load_rust_func);
        proglib
            .rust_load
            .insert(Lstr::Sref("map"), lib_map::load_rust_func);
        proglib
            .rust_load
            .insert(Lstr::Sref("str"), lib_str::load_rust_func);
        proglib
            .rust_load
            .insert(Lstr::Sref("task"), lib_task::load_rust_func);
        proglib
            .rust_load
            .insert(Lstr::Sref("tcp"), tcp::load_rust_func);
        proglib
            .rust_load
            .insert(Lstr::Sref("udp"), udp::load_rust_func);

        proglib
    }

    pub fn main_module(&self) -> &str
    {
        &self.loader.main_mod
    }

    pub fn load_code(&mut self, modname: &Lstr, funcname: &Lstr) -> Lresult<&Code>
    {
        let (has_mod, has_func) = if self.code.contains_key(modname) {
            let old_mod = self.code.get(modname).unwrap();
            (true, old_mod.contains_key(funcname))
        } else {
            (false, false)
        };

        if !has_func {
            let new_code = self.read_code(modname, funcname)?;

            if has_mod {
                let old_mod = self.code.get_mut(modname).unwrap();
                old_mod.insert(funcname.clone(), new_code);
            } else {
                let mut new_mod = HashMap::new();
                new_mod.insert(funcname.clone(), new_code);
                self.code.insert(modname.clone(), new_mod);
            }
        }

        let module = self.code.get(modname).ok_or_else(|| {
            rustfail!(
                "codefail",
                "cannot find module for code: {}",
                modname,
            )
        })?;
        module.get(funcname).ok_or_else(|| {
            rustfail!(
                "codefail",
                "cannot find code for function: {}",
                funcname,
            )
        })
    }

    pub fn find_preface(&self, modname: &str) -> Option<&Rc<ModulePreface>>
    {
        self.preface.get(modname)
    }

    pub fn find_proto(&self, modname: &str) -> Option<&Protomod>
    {
        self.proto.get(modname).map(|p| &**p)
    }

    pub fn load_inter(&mut self, modname: &Lstr)
    {
        if !self.inter.contains_key(modname) {
            let inter = self.read_inter(modname);
            self.inter.insert(modname.clone(), inter);
        }
    }

    pub fn init_typemod(&mut self, modname: &Lstr)
    {
        if !self.typed.contains_key(modname) {
            self.typed
                .insert(modname.clone(), Typemod::new(modname.clone()));
        }

        let typmod = self.typed.get_mut(modname).unwrap();
        let proto = self.proto.get(modname).unwrap();
        for (fname, ftype) in proto.valtypes.iter() {
            typmod.set_function_type(fname.clone(), ftype.clone());
        }
    }

    pub fn load_proto(&mut self, modname: &Lstr)
    {
        if !self.proto.contains_key(modname) {
            let proto = self.read_proto(modname);
            self.proto.insert(modname.clone(), Rc::new(proto));
            self.init_typemod(modname);
        }
    }

    pub fn load_preface(&mut self, modname: &Lstr)
    {
        if !self.preface.contains_key(modname) {
            let (msrc, mpref) = self.read_preface(modname);
            self.modsrc.insert(modname.clone(), msrc);
            self.preface.insert(modname.clone(), Rc::new(mpref));
        }
    }

    pub fn read_astmod(
        loader: &mut Interloader,
        modname: &Lstr,
    ) -> Lresult<ProtoModule>
    {
        vout!("read_modast: {}\n", modname);
        let modtxt = loader.read_mod(modname)?;
        let modkey = ModKey::name_only(modname.clone());
        ProtoModule::new(modkey, modtxt)
    }

    pub fn load_proto2(&mut self, modname: &Lstr) -> Lresult<()>
    {
        vout!("load_proto2: {}\n", modname);
        self.protos.load(&mut self.loader, modname)?;
        Ok(())
    }

    pub fn read_semantics(&mut self, modname: &Lstr, funcname: &Lstr) -> Lresult<Type>
    {
        self.load_proto_and_imports(modname)?;
        let result = self.semantics.compile_call(&mut self.protos, modname.str(), funcname.str())?;
        Ok(result.typ)
    }

    pub fn read_modsrc(&mut self, modname: &Lstr) -> ModuleSource
    {
        vout!("read_modsrc: {}\n", modname);
        let modtxt = self.loader.read_module(modname).unwrap();
        let modkey = ModKey::name_only(modname.clone());
        ModuleSource::new(modkey, modtxt)
    }

    pub fn read_preface(
        &mut self,
        modname: &Lstr,
    ) -> (ModuleSource, ModulePreface)
    {
        let ms = self.read_modsrc(modname);
        let pref = ModulePreface::new(&ms);
        (ms, pref)
    }

    pub fn read_proto(&mut self, modname: &Lstr) -> Protomod
    {
        let (ms, pref) = self.read_preface(modname);
        self.load_imports(modname, &pref.imports);
        let proto = phase0::preproc(self, &pref, &ms.ast);
        self.modsrc.insert(modname.clone(), ms);
        self.preface.insert(modname.clone(), Rc::new(pref));
        proto
    }

    pub fn read_inter(&mut self, modname: &Lstr) -> Intermod
    {
        vout!("read_inter({})\n", modname);
        self.load_proto(modname);
        let preface = self.preface.get(modname).unwrap().clone();
        let imports = self.import_protos(&preface.imports);
        let proto = self.proto.get(modname).unwrap();
        Intermod::compile(&proto, &imports)
    }

    pub fn read_code(&mut self, modname: &Lstr, funcname: &Lstr) -> Lresult<Code>
    {
        vout!("read_code({}::{})\n", modname, funcname);
        self.load_inter(modname);

        let inter = self
            .inter
            .get(modname)
            .ok_or_else(|| {
                rustfail!(
                    "codefail",
                    "cannot compile missing module {}",
                    modname,
                )
            })?;
        let fix = inter
            .interfunc
            .get(funcname)
            .ok_or_else(|| {
                rustfail!(
                    "codefail",
                    "cannot compile missing function {}::{}",
                    modname,
                    funcname,
                )
            })?;
        if modname == "prefab" {
            vout!("prefab::{} fix: {:?}\n", funcname, fix);
        }

        if let Source::RustBlock(_, _) = fix.src {
            let rust_loader = self.rust_load.get(modname);
            if rust_loader.is_none() {
                panic!("no rust loader for: {}", modname);
            }
            let rustfunc = rust_loader.unwrap()(funcname);
            if rustfunc.is_none() {
                panic!("no rust function for: {}::{}", modname, funcname);
            }
            Ok(rustfunc.unwrap())
        } else {
            let ops = code::make_ops(fix);
            Ok(Code::Leema(ops))
        }
    }

    fn load_proto_and_imports(&mut self, modname: &Lstr) -> Lresult<()>
    {
        self.protos.load(&mut self.loader, modname)?;
        self.protos.load_imports(&mut self.loader, modname)
    }

    fn load_imports(&mut self, modname: &Lstr, imports: &HashSet<Lstr>)
    {
        for i in imports {
            if i == modname {
                panic!("A module cannot import itself: {}", i);
            }
            if !self.preface.contains_key(i) {
                let im = self.read_modsrc(i);
                let pref = ModulePreface::new(&im);
                self.modsrc.insert(i.clone(), im);
                self.preface.insert(i.clone(), Rc::new(pref));
            }
        }
    }

    fn import_protos(
        &mut self,
        imports: &HashSet<Lstr>,
    ) -> HashMap<Lstr, Rc<Protomod>>
    {
        let mut imported_protos: HashMap<Lstr, Rc<Protomod>> = HashMap::new();
        imported_protos.insert(
            Lstr::Sref("prefab"),
            self.proto.get("prefab").unwrap().clone(),
        );
        for i in imports {
            self.load_proto(i);
            let p = self.proto.get(i).unwrap().clone();
            imported_protos.insert(i.clone(), p);
        }
        imported_protos
    }

    pub fn get_macro2<'a>(
        &'a self,
        modname: &str,
        macname: &str,
    ) -> Lresult<Option<&'a ast2::Ast>>
    {
        let proto = self.protos.get(modname)?;
        Ok(proto.macros.get(macname))
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
