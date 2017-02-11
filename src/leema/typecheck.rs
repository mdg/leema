
use leema::iexpr::{Iexpr, Source};
use leema::inter::{Intermod};
use leema::infer::{Inferator};
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

    pub fn push_call(&mut self, call: CallOp)
    {
        self.calls.push_back(call);
    }

    pub fn pop_call(&mut self) -> Option<CallOp>
    {
        self.calls.pop_front()
    }

    pub fn collect_calls<'b>(&mut self, ix: &'b Iexpr)
    {
        match ix.src {
            Source::Call(ref callx, ref args) => {
                if let Source::Tuple(ref argsix) = args.src {
                    self.collect_calls_vec(argsix);
                    self.collect_callexpr(callx);
                }
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
            Source::IfExpr(ref cond, ref truth, ref lies) => {
                self.collect_calls(cond);
                self.collect_calls(truth);
                self.collect_calls(lies);
            }
            Source::ConstVal(ref val) => {
                // nothing to do. constants aren't calls.
            }
            Source::Id(ref id) => {
                // nothing to do. not calls.
            }
            Source::RustBlock => {
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

    pub fn collect_callexpr<'b>(&mut self, callx: &'b Iexpr)
    {
        match callx.src {
            Source::Id(ref callname) => {
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

    pub fn collect_calls_vec<'b>(&mut self, xvec: &'b Vec<Iexpr>)
    {
        for x in xvec {
            self.collect_calls(x);
        }
    }
}

#[derive(Debug)]
pub struct Typescope<'a, 'b>
{
    pub fname: &'b str,
    inter: &'a Intermod,
    imports: &'a HashMap<String, &'a Intermod>,
    T: Inferator,
}

impl<'a, 'b> Typescope<'a, 'b>
{
    pub fn new(inter: &'a Intermod, func: &'b str
            , imps: &'a HashMap<String, &'a Intermod>
            ) -> Typescope<'a, 'b>
    {
        Typescope
        {
            fname: func,
            inter: inter,
            imports: imps,
            T: Inferator::new(),
        }
    }

    pub fn typecheck_pattern(&mut self, patt: &Val, valtype: &Type)
    {
        match (patt, valtype) {
            (_, &Type::AnonVar) => {
                panic!("pattern value type cannot be anonymous: {:?}"
                        , patt);
            }
            (&Val::Id(ref id), _) => {
                self.T.bind_vartype(id, valtype);
            }
            _ => {
                panic!("cannot typecheck pattern match: {:?} := {:?}"
                        , patt, valtype);
            }
        }
    }
}

pub fn typecheck_expr(scope: &mut Typescope, ix: &Iexpr) -> Type
{
    match &ix.src {
        &Source::Call(ref func, ref args) => {
            let tfunc = typecheck_expr(scope, func);
            let mut targs = vec![];
            if let Source::Tuple(ref argstup) = args.src {
                for a in argstup {
                    targs.push(typecheck_expr(scope, a));
                }
            }
            let mut targs_ref = vec![];
            for ta in targs.iter() {
                targs_ref.push(ta);
            }
            scope.T.make_call_type(&tfunc, &targs_ref)
        }
        &Source::ConstVal(ref cv) => {
            ix.typ.clone()
        }
        &Source::Let(ref lhs, ref rhs) => {
            let rhs_type = typecheck_expr(scope, rhs);
            scope.typecheck_pattern(lhs, &rhs_type);
            Type::Void
        }
        &Source::Block(ref elems) => {
            let mut last_type = Type::Void;
            for e in elems {
                last_type = typecheck_expr(scope, e);
            }
            last_type
        }
        &Source::Id(_) => {
            ix.typ.clone()
        }
        &Source::IfExpr(ref cond, ref truth, ref lies) => {
            let cond_t = typecheck_expr(scope, cond);
            scope.T.match_types(&cond_t, &Type::Bool);

            let truth_t = typecheck_expr(scope, truth);
            let lies_t = typecheck_expr(scope, lies);
            scope.T.match_types(&truth_t, &lies_t);
            truth_t
        }
        &Source::StrMash(ref items) => {
            for i in items {
                typecheck_expr(scope, i);
                // TODO: check that this supports the stringification interface
                // let it = typecheck_expr(scope, i);
                // scope.T.match_types(&it, &Type::Str);
            }
            Type::Str
        }
        &Source::Func(ref body) => {
            // typecheck_expr(scope, body)
            panic!("unexpected func in typecheck: {:?}", body);
        }
        _ => {
            panic!("Could not typecheck_expr({:?})", ix);
        }
    }
}

pub fn typecheck_function(scope: &mut Typescope, ix: &Iexpr) -> Type
{
    println!("check_function({:?})", scope.fname);
    match &ix.src {
        &Source::Func(ref body) => {
            typecheck_expr(scope, &*body)
        }
        &Source::RustBlock => {
            ix.typ.clone()
        }
        _ => {
            panic!("Cannot typecheck_function a not function: {:?}", ix);
        }
    }
    /*
    scope.add_parameters(fix)
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
