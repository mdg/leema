use leema::ast::Ast;
use leema::code::{self, Code};
use leema::infer::TypeSet;
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
use leema::{file, prefab, tcp, udp, lib_hyper};

use std::collections::{HashMap, HashSet};
use std::io::Write;
use std::rc::Rc;


pub struct Lib
{
    loader: Interloader,
    modsrc: HashMap<Lstr, ModuleSource>,
    preface: HashMap<Lstr, Rc<ModulePreface>>,
    proto: HashMap<Lstr, Rc<Protomod>>,
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
            inter: HashMap::new(),
            typed: HashMap::new(),
            rust_load: HashMap::new(),
            code: HashMap::new(),
        };
        proglib
            .rust_load
            .insert(Lstr::Sref("prefab"), prefab::load_rust_func);
        proglib.load_inter(&Lstr::Sref("prefab"));
        proglib
            .rust_load
            .insert(Lstr::Sref("file"), file::load_rust_func);
        proglib
            .rust_load
            .insert(Lstr::Sref("str"), lib_str::load_rust_func);
        proglib
            .rust_load
            .insert(Lstr::Sref("tcp"), tcp::load_rust_func);
        proglib
            .rust_load
            .insert(Lstr::Sref("udp"), udp::load_rust_func);
        proglib
            .rust_load
            .insert(Lstr::Sref("hyper_server"), lib_hyper::load_rust_func);

        proglib
    }

    pub fn main_module(&self) -> &str
    {
        &self.loader.main_mod
    }

    pub fn load_code(&mut self, modname: &Lstr, funcname: &Lstr) -> &Code
    {
        let (has_mod, has_func) = if self.code.contains_key(modname) {
            let old_mod = self.code.get(modname).unwrap();
            (true, old_mod.contains_key(funcname))
        } else {
            (false, false)
        };

        if !has_func {
            let new_code = self.read_code(modname, funcname);

            if has_mod {
                let old_mod = self.code.get_mut(modname).unwrap();
                old_mod.insert(funcname.clone(), new_code);
            } else {
                let mut new_mod = HashMap::new();
                new_mod.insert(funcname.clone(), new_code);
                self.code.insert(modname.clone(), new_mod);
            }
        }

        self.code.get(modname).unwrap().get(funcname).unwrap()
    }

    pub fn find_preface(&self, modname: &Lstr) -> Option<&Rc<ModulePreface>>
    {
        self.preface.get(modname)
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

    pub fn read_modsrc(&self, modname: &Lstr) -> ModuleSource
    {
        let modkey = self.loader.mod_name_to_key(modname.clone());
        let modtxt = self.loader.read_module(&modkey);
        ModuleSource::new(modkey, modtxt)
    }

    pub fn read_preface(&self, modname: &Lstr)
        -> (ModuleSource, ModulePreface)
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

    pub fn read_code(&mut self, modname: &Lstr, funcname: &Lstr) -> Code
    {
        vout!("read_code({}::{})\n", modname, funcname);
        self.load_inter(modname);

        let funcri = Lri::with_modules(modname.clone(), funcname.clone());
        self.typecheck(&funcri, typecheck::Depth::One);

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

        if let Source::RustBlock(_, _) = fix.src {
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

    pub fn typecheck(&mut self, funcri: &Lri, depth: typecheck::Depth) -> Type
    {
        vout!("typecheck({}, {:?})\n", funcri, depth);
        self.load_inter(funcri.mod_ref().expect("no typecheck module name"));
        if depth.one_deeper() {
            self.deeper_typecheck(funcri, depth);
        }

        let ftype = self.local_typecheck(funcri);
        let modstr = funcri.mod_ref().unwrap().str();
        let mutyped = self.typed.get_mut(modstr).unwrap();
        mutyped.set_function_type(funcri.localid.clone(), ftype.clone());
        vout!("\tfinish typecheck({})\n", funcri);
        ftype
    }

    pub fn deeper_typecheck(&mut self, funcri: &Lri, depth: typecheck::Depth)
    {
        let cf = {
            let mod_str = funcri.mod_ref().expect("typecheck module name");
            let mut icf = CallFrame::new(mod_str, funcri.localid.str());
            let inter = self.inter.get(mod_str).unwrap();
            let fix = inter
                .interfunc
                .get(funcri.localid.str())
                .or_else(|| {
                    panic!("cannot find function inter: {}", funcri);
                }).unwrap();
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
        let opt_inter = self.inter.get_mut(modlstr);
        if opt_inter.is_none() {
            panic!("cannot find inter for {}", funcri);
        }
        let inter = opt_inter.unwrap();
        let mut fix = inter.interfunc.get_mut(funclstr).unwrap();
        if !self.typed.contains_key(modlstr) {
            self.typed
                .insert(modlstr.clone(), Typemod::new(modlstr.clone()));
        }
        {
            let mutyped = self.typed.get_mut(modlstr).unwrap();
            if mutyped.get_function_type(funclstr).is_none() {
                let proto = self.proto.get(modlstr).unwrap();
                let decltype = proto.valtypes.get(funclstr).unwrap();
                mutyped.set_function_type(funclstr.clone(), decltype.clone());
            }
        }
        let typed = self.typed.get(modlstr).unwrap();

        let mut typeset = TypeSet::new();
        if self.proto.contains_key("prefab") {
            typeset.import_user_types(
                Lstr::Sref("prefab"),
                &self.proto.get("prefab").unwrap().struple_fields,
            );
        }

        let pref = self.preface.get(modlstr).unwrap().clone();
        let mut imports: HashMap<Lstr, &Typemod> = HashMap::new();
        let prefab_typed = self.typed.get("prefab");
        if prefab_typed.is_some() {
            imports.insert(Lstr::Sref("prefab"), prefab_typed.unwrap());
        }
        for i in pref.imports.iter() {
            let iii: Option<&Typemod> = self.typed.get(i);
            if iii.is_none() {
                panic!("cannot find intermod in imports: {}", i);
            }
            imports.insert(i.clone(), iii.unwrap());

            let improto = self.proto.get(i).unwrap();
            typeset.import_user_types(i.clone(), &improto.struple_fields);
        }

        let opt_proto = self.proto.get(modlstr);
        typeset.import_user_types(
            modlstr.clone(),
            &opt_proto.unwrap().struple_fields,
        );
        let mut scope = Typescope::new(
            typed,
            opt_proto.unwrap(),
            funclstr.str(),
            &imports,
            &typeset,
        );
        typecheck::typecheck_function(&mut scope, &mut fix).unwrap()
    }

    fn load_imports(&mut self, modname: &Lstr, imports: &HashSet<Lstr>)
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
