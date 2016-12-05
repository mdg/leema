
use leema::loader::{Interloader};
use leema::program;
use leema::log;

use std::path::Path;
use std::io::{stderr, Write};


pub fn program(prog: &mut program::Lib, inter: &Interloader)
{
    {
        let mut m = if !prog.has_mod(&inter.main_mod) {
            let mkey = inter.mod_name_to_key(&inter.main_mod);
            prog.add_mod(inter.init_module(mkey))
        } else {
            prog.get_mod_mut(&inter.main_mod)
        };
        m.load();
    }
    module(prog, inter, &inter.main_mod);
}

/*
mod -> prog
imps -> libs

load_func(mn, fn) {
    if !mod_sourced(mn)
        source_mod(mn)
    if !mod_loaded(mn)
        load_mod(mn)
    mod = get_mod(mn)
    src_func = get_func(mod, fn)
    pfunc = preprocess(src_func)
    tfunc = type_func(pfunc)
    if !func_preprocessed(mod, fn)
        src_func
        preprocess(src_func)
    if !func_processed(mod, fn)
}

for f in mod.funcs {
    typecheck_func(f)
}

typecheck_call(m, f, args) {
    tfd = typecheck_func(m, f)
    typecheck(args, tfd.args)
}

typecheck_func(m, f) {
    fd = getf(m, f)
    for e in fd {
        typecheck_expr(e)
    }
}

typecheck_expr(e) {
    if e == call {
        typecheck_call(call.module, call.func, call.args)
    }
}
*/

pub fn module(prog: &mut program::Lib, inter: &Interloader, modname: &str)
{
    vout!("typecheck.module({})\n", modname);
    let imported_mods = {
        let m = prog.get_mod_mut(modname);
        println!("loaded module:\n{:?}\n", m);

        let mut imods = Vec::with_capacity(m.src.imports.len());
        for i in &m.src.imports {
            println!("import({})", i);
            let ikey = inter.mod_name_to_key(&i);
            let mut im = inter.init_module(ikey);
            im.load();
            imods.push(im);
        }
        m.imports_loaded = true;
        imods
    };

    for i in imported_mods {
        println!("imported({:?})", i);
        prog.add_mod(i);
    }

    let funcs: Vec<String> = {
        prog.get_mod(modname).src.funcs.keys().map(|im| {
            im.clone()
        }).collect()
    };
    // let mut ts = new TypeSpace();
    // ts.add_imports(&m.src.imports);

    for fname in funcs {
        func(prog, inter, &fname);
    }
}

pub fn func(prog: &mut program::Lib, inter: &Interloader, fname: &str)
{
    println!("\ntypecheck::func({:?})", fname);
    // prog.get_func(fname);
}

/*
thing that reads files

thing that holds imported files, ntyped functions

view of visible naively, unferred typed stuff

thing that holds complete type info

view of visible type-complete stuff

library of typed code


pub fn function(prog: &mut program::Lib, inter: &Interloader,
        modname: &str, fname: &str) -> Iexpr
{
    let mut m = prog.load_module(modname);
    let mut tf = m.typed_func(fname);
    if tf.is_none() {
        sf = m.src_func(fname);
        tf = preprocess(sf, prog);
    }
    match m.typed_func(fname) {
        None => j
    }
    let tfunc = prog.typed_func(
    let fsrc = prog.get_func(modname, fname);
}
*/
