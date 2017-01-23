
use leema::loader::{Interloader};
use leema::list;
use leema::iexpr::{Iexpr};
use leema::inter::{Intermod};
use leema::infer::{Inferator};
use leema::program::{self, Lib};
use leema::scope::{Scope};
use leema::val::{Val, Type};
use leema::log;

use std::collections::{HashMap};
use std::path::Path;
use std::io::{stderr, Write};
use std::rc::{Rc};


#[derive(Debug)]
struct CallFrame<'a>
{
    fname: &'a str,
    fix: &'a Iexpr,
    T: Inferator,
}

impl <'a> CallFrame<'a>
{
    pub fn new(fname: &'a str) -> CallFrame<'a>
    {
        CallFrame{
            parent: None,
            fname: fname,
            T: Inferator::new(),
        }
    }

    pub fn pop_call(frame: &mut CallFrame)
    {
        // let mut tmpf = None;
        // mem::swap(tmpf, frame.parent);
        // mem::replace(frame, frame.parent);
    }
}

#[derive(Debug)]
struct ModScope<'a>
{
    inter: &'a Intermod,
    typed: Intermod,
    callstack: Vec<CallFrame<'a>>,
}

impl<'a> ModScope<'a>
{
    pub fn new(inter: &'a Intermod) -> ModScope<'a>
    {
        ModScope
        {
            inter: inter,
            typed: Intermod::new(inter.key.clone()),
        }
    }
}

#[derive(Debug)]
struct Typescope<'a>
{
    prog: &'a program::Lib,
    typed: HashMap<&'a str, Option<ModScope<'a>>>,
    modstack: Vec<ModScope<'a>>,
}

impl<'a> Typescope<'a>
{
    pub fn new(prog: &'a program::Lib) -> Typescope<'a>
    {
        Typescope{
            prog: prog,
            typed: HashMap::new(),
            modstack: vec![],
        }
    }

    pub fn push_call(scope: &mut Typescope, modnm: &'a str, funcnm: &'a str)
    {
    }

    pub fn pop_call(scope: &mut Typescope) -> Option<Intermod>
    {
        None
    }
}

pub fn check_main(prog: &Lib)
        -> Intermod
{
    let scope = Typescope::new(prog, "main");
    check_function(&mut scope)
}

fn check(scope: &mut Typescope, fix: &Iexpr)
{
    // let func = scope.inter.interfunc.get(scope.call.fname).unwrap();
    scope.add_parameters()
    for x in fix {
        check(x)
    }
}

pub fn module(inter: &Intermod)
        -> Intermod
{
    scope = new scope(inter);
    for funcs in inter {
        func = check_function(scope, func)
        scope.add(func);
    }
}

CallFrame {
}

pub fn typecheck(f: Frame, ix: &Iexpr) -> Iexpr
{
    match ix {
        Call(func, args) => {
            tfunc = typecheck(func)
            targs = typecheck(args)
            newf = new Frame(args)
            new_fix = what?
            typecheck(f, ix)
        }
    }
}

pub fn function(f: Frame, fix: &Iexpr) -> Iexpr
{
    for i in fix {
        typecheck(f, i);
    }
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
