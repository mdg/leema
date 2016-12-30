
use leema::loader::{Interloader};
use leema::list;
use leema::program::{Lib};
use leema::scope::{Scope};
use leema::val::{Val, Type};
use leema::log;

use std::path::Path;
use std::io::{stderr, Write};


pub fn program(scope: Scope, prog: &mut Lib)
{
    let main_mod = prog.main_module().to_string();
    let prog_type = function(scope, prog, &main_mod, "main");
    println!("\nprogram type: {:?}", prog_type);
}

pub fn function(mut scope: Scope, prog: &mut Lib, modnm: &str, funcnm: &str) -> Type
{
    println!("\ntypecheck::function({}:{})", modnm, funcnm);
    scope.push_function(prog, modnm, funcnm);
    println!("\nscope: {:?}", scope);
    // function_code
    {
        /*
        let func_src = &scope._function.src;
        let (def_fname, def_args, def_result, body) =
                list::to_ref_tuple4(func_src);
        println!("\nfunc: {}({:?}) -> {:?} {{\n{:?}\n}}\n",
                def_fname, def_args, def_result, body);
        let result = compile_expr(scope, prog, body);
        */
    }
    scope.pop_function(prog);
    Type::Void
}

pub fn compile_expr(mut scope: Scope, prog: &mut Lib, expr: &Val) -> Type
{
    match expr {
        &Val::Int(i) => {
            // Iexpr::const_val(expr.clone());
        }
        what => {
            println!("Couldn't match expr: {:?}", expr);
        }
    }
    Type::Void
}

/*
mod -> prog
imps -> libs

load_func(mn, fn) {
    mod = prog.load_module(prog, mn)
    import_mods(mod)
    if !mod_sourced(mn)
        source_mod(mn)
    if !mod_loaded(mn)
        load_mod(mn)
    mod = get_mod(mn)
    if !mod.imports_loaded
        load_imports(mod.imports)
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

/*
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
*/

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
