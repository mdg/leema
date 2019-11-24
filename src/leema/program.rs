use crate::leema::ast2::Ast;
use crate::leema::code::{self, Code};
use crate::leema::failure::Lresult;
use crate::leema::lib_map;
use crate::leema::lib_str;
use crate::leema::loader::Interloader;
use crate::leema::module::{self, Chain};
use crate::leema::proto::{ProtoLib, ProtoModule};
use crate::leema::semantics::Semantics;
use crate::leema::val::Fref;
use crate::leema::{
    file, lib_hyper, lib_io, lib_json, lib_list, lib_task, prefab, tcp, udp,
};

use std::collections::HashMap;


pub struct Lib
{
    loader: Interloader,
    protos: ProtoLib,
    semantics: Semantics,
    rust_load: HashMap<module::Chain, fn(&str) -> Option<code::Code>>,
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
        lfailoc!(proglib.protos.load_absolute(&mut proglib.loader, module::Chain::from("core"))).unwrap();
        lfailoc!(proglib.protos.load_absolute(&mut proglib.loader, module::Chain::from("prefab"))).unwrap();

        proglib
            .rust_load
            .insert(Chain::from("prefab"), prefab::load_rust_func);
        proglib
            .rust_load
            .insert(Chain::from("file"), file::load_rust_func);
        proglib
            .rust_load
            .insert(Chain::from("hyper_client"), lib_hyper::load_client_func);
        proglib
            .rust_load
            .insert(Chain::from("hyper_server"), lib_hyper::load_rust_func);
        proglib
            .rust_load
            .insert(Chain::from("io"), lib_io::load_rust_func);
        proglib
            .rust_load
            .insert(Chain::from("json"), lib_json::load_rust_func);
        proglib
            .rust_load
            .insert(Chain::from("list"), lib_list::load_rust_func);
        proglib
            .rust_load
            .insert(Chain::from("map"), lib_map::load_rust_func);
        proglib
            .rust_load
            .insert(Chain::from("str"), lib_str::load_rust_func);
        proglib
            .rust_load
            .insert(Chain::from("task"), lib_task::load_rust_func);
        proglib
            .rust_load
            .insert(Chain::from("tcp"), tcp::load_rust_func);
        proglib
            .rust_load
            .insert(Chain::from("udp"), udp::load_rust_func);

        proglib
    }

    pub fn load_code(
        &mut self,
        f: &Fref,
    ) -> Lresult<&Code>
    {
        if !self.code.contains_key(f) {
            let new_code = ltry!(self.read_code(f));
            self.code.insert(f.clone(), new_code);
        }

        self.code.get(f).ok_or_else(|| {
            rustfail!("codefail", "cannot find code for: {}", f)
        })
    }

    pub fn find_proto(&self, path: &module::Chain) -> Lresult<&ProtoModule>
    {
        self.protos.path_proto(&path)
    }

    pub fn read_semantics(
        &mut self,
        f: &Fref,
    ) -> Lresult<Semantics>
    {
        self.load_proto_and_imports(&f.m.chain)?;
        Semantics::compile_call(&mut self.protos, f)
    }

    pub fn read_code(
        &mut self,
        f: &Fref,
    ) -> Lresult<Code>
    {
        vout!("read_code({})\n", f);
        let semantics = ltry!(self.read_semantics(f));

        if let Ast::RustBlock = &*semantics.src.node {
            let rust_loader = self.rust_load.get(&f.m.chain);
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
            Ok(Code::Leema(ops))
        }
    }

    pub fn load_proto_and_imports(&mut self, modpath: &module::Chain) -> Lresult<()>
    {
        self.protos.load_absolute(&mut self.loader, modpath.clone())?;
        self.protos.load_imports(&mut self.loader, modpath)
    }

    pub fn get_macro2<'a>(
        &'a self,
        modpath: &module::Chain,
        macname: &str,
    ) -> Lresult<Option<&'a Ast>>
    {
        let proto = self.protos.path_proto(&modpath)?;
        Ok(proto.macros.get(macname))
    }
}
