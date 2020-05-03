use crate::leema::ast2::Ast;
use crate::leema::code::{self, Code};
use crate::leema::failure::Lresult;
use crate::leema::lib_map;
use crate::leema::lib_str;
use crate::leema::loader::Interloader;
use crate::leema::module::CanonicalMod;
use crate::leema::proto::{ProtoLib, ProtoModule};
use crate::leema::semantics::Semantics;
use crate::leema::val::Fref;
use crate::leema::{
    file, lib_core, lib_hyper, lib_io, lib_json, lib_list, lib_math, lib_task,
    prefab, tcp, udp,
};

use std::collections::HashMap;
use std::path::Path;


pub struct Lib
{
    loader: Interloader,
    protos: ProtoLib,
    semantics: Semantics,
    rust_load: HashMap<CanonicalMod, fn(&str) -> Option<code::Code>>,
    code: HashMap<Fref, Code>,
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

        // automatically load both core and prefab
        // eventually will move everything to core and delete prefab
        lfailoc!(proglib
            .protos
            .load_absolute(&mut proglib.loader, Path::new("/core")))
        .unwrap();
        lfailoc!(proglib
            .protos
            .load_absolute(&mut proglib.loader, Path::new("/prefab")))
        .unwrap();

        proglib
            .rust_load
            .insert(canonical_mod!("/core"), lib_core::load_rust_func);
        proglib
            .rust_load
            .insert(canonical_mod!("/prefab"), prefab::load_rust_func);
        proglib
            .rust_load
            .insert(canonical_mod!("/file"), file::load_rust_func);
        proglib
            .rust_load
            .insert(canonical_mod!("/hyper_client"), lib_hyper::load_client_func);
        proglib
            .rust_load
            .insert(canonical_mod!("/hyper_server"), lib_hyper::load_rust_func);
        proglib
            .rust_load
            .insert(canonical_mod!("/io"), lib_io::load_rust_func);
        proglib
            .rust_load
            .insert(canonical_mod!("/json"), lib_json::load_rust_func);
        proglib
            .rust_load
            .insert(canonical_mod!("/list"), lib_list::load_rust_func);
        proglib
            .rust_load
            .insert(canonical_mod!("/map"), lib_map::load_rust_func);
        proglib
            .rust_load
            .insert(canonical_mod!("/math"), lib_math::load_rust_func);
        proglib
            .rust_load
            .insert(canonical_mod!("/str"), lib_str::load_rust_func);
        proglib
            .rust_load
            .insert(canonical_mod!("/task"), lib_task::load_rust_func);
        proglib
            .rust_load
            .insert(canonical_mod!("/tcp"), tcp::load_rust_func);
        proglib
            .rust_load
            .insert(canonical_mod!("/udp"), udp::load_rust_func);

        proglib
    }

    pub fn load_code(&mut self, f: &Fref) -> Lresult<&Code>
    {
        if !self.code.contains_key(f) {
            let new_code = ltry!(self.read_code(f));
            self.code.insert(f.clone(), new_code);
        }

        self.code
            .get(f)
            .ok_or_else(|| rustfail!("codefail", "cannot find code for: {}", f))
    }

    pub fn find_proto(&self, path: &CanonicalMod) -> Lresult<&ProtoModule>
    {
        self.protos.path_proto(&path)
    }

    pub fn read_semantics(&mut self, f: &Fref) -> Lresult<Semantics>
    {
        ltry!(self.load_proto_and_imports(&f.m.name));
        Semantics::compile_call(&mut self.protos, f)
    }

    pub fn read_code(&mut self, f: &Fref) -> Lresult<Code>
    {
        vout!("read_code({})\n", f);
        let start = start_timer!();
        let semantics = ltry!(self.read_semantics(f));

        if let Ast::RustBlock = &*semantics.src.node {
            let rust_loader = self.rust_load.get(&f.m.name);
            if rust_loader.is_none() {
                panic!("no rust loader for: {}", f.m);
            }
            let rustfunc = rust_loader.unwrap()(f.f);
            if rustfunc.is_none() {
                panic!("no rust function for: {}", f);
            }
            Ok(rustfunc.unwrap())
        } else {
            let mut semantics_ast = semantics.src;
            code::assign_registers(&mut semantics_ast, semantics.args)?;
            let ops = code::make_ops2(semantics_ast);
            log_timer!(start, "read_code {}", f);
            Ok(Code::Leema(ops))
        }
    }

    pub fn load_proto_and_imports(
        &mut self,
        cmod: &CanonicalMod,
    ) -> Lresult<()>
    {
        let modpath = cmod.mod_path();
        ltry!(self.protos.load_absolute(&mut self.loader, modpath));
        Ok(ltry!(self.protos.load_imports(&mut self.loader, modpath)))
    }
}
