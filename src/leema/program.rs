use crate::leema::ast2::Ast;
use crate::leema::code::{self, Code};
use crate::leema::failure::Lresult;
use crate::leema::lib_map;
use crate::leema::lib_str;
use crate::leema::loader::Interloader;
use crate::leema::lstr::Lstr;
use crate::leema::proto::{ProtoLib, ProtoModule};
use crate::leema::semantics::Semantics;
use crate::leema::{
    file, lib_hyper, lib_io, lib_json, lib_list, lib_task, prefab, tcp, udp,
};

use std::collections::HashMap;


pub struct Lib
{
    loader: Interloader,
    protos: ProtoLib,
    semantics: Semantics,
    rust_load: HashMap<Lstr, fn(&str) -> Option<code::Code>>,
    code: HashMap<Lstr, HashMap<Lstr, Code>>,
}

impl Lib
{
    pub fn new(l: Interloader) -> Lib
    {
        let mut proglib = Lib {
            loader: l,
            protos: ProtoLib::new(),
            semantics: Semantics::new(),
            rust_load: HashMap::new(),
            code: HashMap::new(),
        };

        lfailoc!(proglib.protos.load(&mut proglib.loader, &Lstr::Sref("prefab"))).unwrap();

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

    pub fn load_code(
        &mut self,
        modname: &Lstr,
        funcname: &Lstr,
    ) -> Lresult<&Code>
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
            rustfail!("codefail", "cannot find module for code: {}", modname,)
        })?;
        module.get(funcname).ok_or_else(|| {
            rustfail!(
                "codefail",
                "cannot find code for function: {}",
                funcname,
            )
        })
    }

    pub fn find_proto(&self, modname: &str) -> Lresult<&ProtoModule>
    {
        self.protos.get(modname)
    }

    pub fn load_proto2(&mut self, modname: &Lstr) -> Lresult<()>
    {
        vout!("load_proto2: {}\n", modname);
        self.protos.load(&mut self.loader, modname)?;
        Ok(())
    }

    pub fn read_semantics(
        &mut self,
        modname: &Lstr,
        funcname: &Lstr,
    ) -> Lresult<Semantics>
    {
        self.load_proto_and_imports(modname)?;
        let mut semantics = Semantics::new();
        let result = semantics.compile_call(
            &mut self.protos,
            modname.str(),
            funcname.str(),
        )?;
        semantics.src = result;
        Ok(semantics)
    }

    pub fn read_code(
        &mut self,
        modname: &Lstr,
        funcname: &Lstr,
    ) -> Lresult<Code>
    {
        vout!("read_code({}::{})\n", modname, funcname);
        let semantics = self.read_semantics(modname, funcname)?;

        if let Ast::RustBlock = &*semantics.src.node {
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
            let mut semantics_ast = semantics.src;
            code::assign_registers(&mut semantics_ast, semantics.args)?;
            let ops = code::make_ops2(semantics_ast);
            Ok(Code::Leema(ops))
        }
    }

    fn load_proto_and_imports(&mut self, modname: &Lstr) -> Lresult<()>
    {
        self.protos.load(&mut self.loader, modname)?;
        self.protos.load_imports(&mut self.loader, modname)
    }

    pub fn get_macro2<'a>(
        &'a self,
        modname: &str,
        macname: &str,
    ) -> Lresult<Option<&'a Ast>>
    {
        let proto = self.protos.get(modname)?;
        Ok(proto.macros.get(macname))
    }
}
