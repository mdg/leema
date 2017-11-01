
use leema::ixpr::{Ixpr, Source};
use leema::infer::{Inferator};
use leema::module::{ModKey};
use leema::val::{Val, Type};
use leema::log;

use std::collections::{HashMap, LinkedList};
use std::path::Path;
use std::io::{stderr, Write};
use std::rc::{Rc};


#[derive(Debug)]
pub enum CallOp
{
    LocalCall(Rc<String>),
    ExternalCall(Rc<String>, Rc<String>),
}

#[derive(Debug)]
pub struct CallFrame<'a>
{
    modname: &'a str,
    fname: &'a str,
    T: Inferator<'a>,
    pub calls: LinkedList<CallOp>,
}

impl<'a> CallFrame<'a>
{
    pub fn new(modname: &'a str, fname: &'a str) -> CallFrame<'a>
    {
        CallFrame{
            modname: modname,
            fname: fname,
            T: Inferator::new(fname),
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

    pub fn collect_calls<'b>(&mut self, ix: &'b Ixpr)
    {
        match ix.src {
            Source::Call(ref _funcmode, ref callx, ref args) => {
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
            Source::List(ref items) => {
                for i in items {
                    self.collect_calls(i);
                }
            }
            Source::Tuple(ref items) => {
                for i in items {
                    self.collect_calls(i);
                }
            }
            Source::ConstVal(ref val) => {
                // nothing to do. constants aren't calls.
            }
            Source::Id(ref id) => {
                // nothing to do. not calls.
            }
            Source::MatchExpr(ref sample, ref cases) => {
                self.collect_calls(sample);
                self.collect_calls(cases);
            }
            Source::MatchCase(_, ref truth, ref lies) => {
                // self.collect_calls(truth);
                // self.collect_calls(lies);
            }
            Source::RustBlock => {
                // nothing to do. not calls.
            }
            Source::Func(ref _args, ref body) => {
                self.collect_calls(body);
            }
            _ => {
                panic!("Cannot collect calls: {:?}", ix);
            }
        }
    }

    pub fn collect_callexpr<'b>(&mut self, callx: &'b Ixpr)
    {
        match callx.src {
            Source::Id(ref callname) => {
                self.push_call(CallOp::LocalCall(callname.clone()));
            }
            Source::ModuleAccess(ref modname, ref callname) => {
                self.push_call(
                    CallOp::ExternalCall(modname.clone(), callname.clone()));
            }
            Source::ConstVal(ref val) => {
                match val {
                    &Val::Str(ref name) => {
                        self.push_call(CallOp::LocalCall(name.clone()));
                    }
                    &Val::Tuple(ref modfunc) if modfunc.len() == 2 => {
                        self.push_call(CallOp::ExternalCall(
                            modfunc.get(0).unwrap().to_str(),
                            modfunc.get(1).unwrap().to_str(),
                        ));
                    }
                    _ => {
                        panic!("Const val is not a call: {:?}", val);
                    }
                }
            }
            _ => {
                panic!("Unsupported call type: {:?}", callx);
            }
        }
    }

    pub fn collect_calls_vec<'b>(&mut self, xvec: &'b Vec<Ixpr>)
    {
        for x in xvec {
            self.collect_calls(x);
        }
    }
}

#[derive(Debug)]
pub struct Typemod
{
    pub key: Rc<ModKey>,
    pub func: HashMap<String, Type>,
}

impl Typemod
{
    pub fn new(key: Rc<ModKey>) -> Typemod
    {
        Typemod{
            key: key,
            func: HashMap::new(),
        }
    }

    pub fn name(&self) -> &str
    {
        &self.key.name
    }

    pub fn function_type(&self, fname: &str) -> Option<&Type>
    {
        self.func.get(fname)
    }
}

#[derive(Debug)]
pub struct Typescope<'a, 'b>
{
    pub fname: &'b str,
    inter: &'a Typemod,
    imports: &'a HashMap<String, &'a Typemod>,
    T: Inferator<'b>,
}

impl<'a, 'b> Typescope<'a, 'b>
{
    pub fn new(inter: &'a Typemod, func: &'b str
            , imps: &'a HashMap<String, &'a Typemod>
            ) -> Typescope<'a, 'b>
    {
        Typescope
        {
            fname: func,
            inter: inter,
            imports: imps,
            T: Inferator::new(func),
        }
    }

    pub fn typecheck_matchcase(&mut self, valtype: &Type, case: &Ixpr) -> Type
    {
        match &case.src {
            &Source::MatchCase(ref patt, ref truth, ref lies) => {
                self.T.match_pattern(patt, valtype);
                let ttype = typecheck_expr(self, truth);
                let ftype = self.typecheck_matchcase(valtype, lies);

                match self.T.merge_types(&ttype, &ftype) {
                    Some(rtype) => rtype,
                    None => {
                        panic!("match case type mismatch: {:?} != {:?}"
                            , truth, lies);
                    }
                }
            }
            &Source::ConstVal(Val::Void) => Type::Unknown,
            _ => {
                typecheck_expr(self, case)
            }
        }
    }

    pub fn typecheck_call_func(&mut self, src: &Source) -> Type
    {
        match src {
            &Source::Tuple(ref items) => {
                if items.len() != 2 {
                    panic!("call tuples should have 2 items: {:?}", items);
                }
                let ref modname = items[0];
                let ref funcname = items[1];
                Type::Void
            }
            &Source::ConstVal(ref fval) => {
                match fval {
                    &Val::Tuple(ref items) => {
                        let ref modname = items[0];
                        let ref funcname = items[1];
                        self.functype(modname.str(), funcname.str())
                    }
                    &Val::Str(ref strname) => {
                        Type::Void
                    }
                    _ => {
                        panic!("whateval is in typecheck_call? {:?}", fval);
                    }
                }
            }
            _ => {
                panic!("whatever is that in typecheck_call? {:?}", src);
            }
        }
    }

    pub fn functype(&self, modname: &str, funcname: &str) -> Type
    {
        if modname == self.inter.key.name {
            self.inter.func.get(funcname)
            .unwrap()
            .clone()
        } else {
            self.imports.get(modname)
            .expect(&format!("cannot find module {}", modname))
            .func.get(funcname)
            .expect(&format!("cannot find function {}::{}", modname, funcname))
            .clone()
        }
    }
}

pub fn typecheck_expr(scope: &mut Typescope, ix: &Ixpr) -> Type
{
    match &ix.src {
        &Source::Call(ref _mode, ref func, ref args) => {
            let tfunc = scope.typecheck_call_func(&func.src);
            let mut targs = vec![];
            if let Source::Tuple(ref argstup) = args.src {
                for a in argstup {
                    targs.push(typecheck_expr(scope, a));
                }
            } else {
                println!("args are not a tuple");
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
            scope.T.match_pattern(lhs, &rhs_type);
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
        &Source::List(ref items) => {
            for i in items {
                typecheck_expr(scope, i);
            }
            ix.typ.clone()
        }
        &Source::Tuple(ref items) => {
            let mut tuptyp = vec![];
            for i in items {
                tuptyp.push(typecheck_expr(scope, i));
            }
            Type::Tuple(tuptyp)
        }
        &Source::IfExpr(ref cond, ref truth, ref lies) => {
            let cond_t = typecheck_expr(scope, cond);
            scope.T.merge_types(&cond_t, &Type::Bool);

            let truth_t = typecheck_expr(scope, truth);
            let lies_t = typecheck_expr(scope, lies);
            scope.T.merge_types(&truth_t, &lies_t);
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
        &Source::Func(ref _args, ref body) => {
            // typecheck_expr(scope, body)
            panic!("unexpected func in typecheck: {:?}", body);
        }
        &Source::MatchExpr(ref subject, ref cases) => {
            let subject_type = typecheck_expr(scope, subject);
            scope.typecheck_matchcase(&subject_type, cases)
        }
        &Source::MatchCase(_, _, _) => {
            panic!("typecheck matchcase in a specific function: {:?}", ix);
        }
        _ => {
            panic!("Could not typecheck_expr({:?})", ix);
        }
    }
}

pub fn typecheck_function(scope: &mut Typescope, ix: &Ixpr) -> Type
{
    println!("check_function({:?}: {:?})", scope.fname, ix.typ);
    match (&ix.src, &ix.typ) {
        (&Source::Func(ref arg_names, ref body)
                , &Type::Func(ref calltype, ref arg_types
                    , ref declared_result_type)) =>
        {
            for (an, at) in arg_names.iter().zip(arg_types.iter()) {
                scope.T.bind_vartype(an, at);
            }
            println!("f({:?}) =>\n{:?}", arg_names, body);
            let result_type = typecheck_expr(scope, &*body);
            println!("type is: {}", result_type);
            println!("vars:");
            for var in scope.T.vars() {
                let typ = scope.T.vartype(var);
                println!("\t{}: {}", var, typ.unwrap());
            }
            let final_args = arg_types.iter().map(|at| {
                scope.T.inferred_type(at).clone()
            }).collect();
            let final_result = scope.T
                .merge_types(&result_type, declared_result_type)
                .unwrap();
            Type::Func(calltype.clone(), final_args, Box::new(final_result))
        }
        (&Source::RustBlock, _) => {
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


#[cfg(test)]
mod tests {
    use leema::program;
    use leema::loader::{Interloader};
    use leema::log;
    use leema::module::{ModKey};
    use leema::val::{Val, Type};

    use std::io::{stderr, Write};
    use std::collections::{HashMap};


#[test]
#[should_panic]
fn test_pattern_type_inferred_mismatch()
{
    let input = String::from("

    ## foo should take [#] and return a #
    func foo(inputs)
    |([]) -> #empty
    |(#whatever;more) -> #whatever
    |(_;more) -> foo(more)
    --

    func main() ->
        foo([5, 3, 4])
    --
    ");

    let mut loader = Interloader::new("tacos.lma");
    loader.set_mod_txt("tacos", input);
    let mut prog = program::Lib::new(loader);
    prog.deep_typecheck("tacos", "main");
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
        modname: &str, fname: &str) -> Ixpr
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
