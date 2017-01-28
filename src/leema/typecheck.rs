
use leema::loader::{Interloader};
use leema::list;
use leema::iexpr::{Iexpr, Source};
use leema::inter::{Intermod};
use leema::infer::{Inferator};
use leema::scope::{Scope};
use leema::val::{Val, Type};
use leema::log;

use std::collections::{HashMap, LinkedList};
use std::path::Path;
use std::io::{stderr, Write};
use std::rc::{Rc};
use std::sync::{Arc};


#[derive(Debug)]
pub enum CallOp
{
    LocalCall(Arc<String>),
    ExternalCall(Arc<String>, Arc<String>),
}

#[derive(Debug)]
pub struct CallFrame<'a>
{
    modname: &'a str,
    fname: &'a str,
    T: Inferator,
    pub calls: LinkedList<CallOp>,
}

impl<'a> CallFrame<'a>
{
    pub fn new(modname: &'a str, fname: &'a str) -> CallFrame<'a>
    {
        CallFrame{
            modname: modname,
            fname: fname,
            T: Inferator::new(),
            calls: LinkedList::new(),
        }
    }

    pub fn push_call(&'a mut self, call: CallOp)
    {
        self.calls.push_back(call);
    }

    pub fn pop_call(&mut self) -> Option<CallOp>
    {
        self.calls.pop_front()
    }

    pub fn collect_calls<'b>(&'a mut self, ix: &'b Iexpr)
    {
        match ix.src {
            Source::Call(ref callx, ref args) => {
                { self.collect_calls_vec(args); }
                { self.collect_callexpr(callx); }
            }
            Source::Block(ref expressions) => {
                self.collect_calls_vec(expressions);
            }
            Source::Let(ref lhs, ref rhs) => {
                self.collect_calls(rhs);
            }
            Source::StrMash(ref items) => {
                self.collect_calls_vec(items);
            }
            Source::ConstVal(ref val) => {
                // nothing to do. constants aren't calls.
            }
            Source::ValExpr(ref val) => {
                // nothing to do. not calls.
            }
            Source::Func(ref body) => {
                self.collect_calls(body);
            }
            _ => {
                panic!("Cannot collect calls: {:?}", ix);
            }
        }
    }

    pub fn collect_callexpr<'b>(&'a mut self, callx: &'b Iexpr)
    {
        match callx.src {
            Source::ValExpr(Val::Id(ref callname)) => {
                self.push_call(CallOp::LocalCall(callname.clone()));
            }
            Source::ModuleAccess(ref modname, ref callname) => {
                self.push_call(
                    CallOp::ExternalCall(modname.clone(), callname.clone()));
            }
            _ => {
                panic!("Unsupported call type: {:?}", callx);
            }
        }
    }

    pub fn collect_calls_vec<'b>(&'a mut self, xvec: &'b Vec<Iexpr>)
    {
        for x in xvec {
            self.collect_calls(x);
        }
    }
}

#[derive(Debug)]
pub struct Typescope<'a>
{
    pub fname: &'a str,
    inter: &'a Intermod,
    // imports: &'a HashMap<&'a str, &'a Intermod>,
    T: Inferator,
}

impl<'a> Typescope<'a>
{
    pub fn new(inter: &'a Intermod, func: &'a str) -> Typescope<'a>
    {
        Typescope
        {
            fname: func,
            inter: inter,
            // imports: HashMap::new(),
            T: Inferator::new(),
        }
    }

    pub fn pop_call(&'a mut self)
    {
    }
}

pub fn typecheck_expr(scope: &mut Typescope, ix: &Iexpr) -> Iexpr
{
    match &ix.src {
        &Source::Call(ref func, ref args) => {
            // let tfunc = typecheck_expr(scope, func);
            // let targs = typecheck_expr(scope, args);
            // let newf = new Frame(args)
            // new_fix = what?
            // typecheck(f, ix)
        }
        _ => {
        }
    }
    Iexpr::noop()
}

pub fn typecheck_function(scope: &mut Typescope)
{
    println!("check_function({:?})", scope.fname);
    /*
    scope.add_parameters()

    typecheck_expr(scope, fix);
    for i in fix {
        typecheck_expr(f, i);
    }
    */
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
