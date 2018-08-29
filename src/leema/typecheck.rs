use leema::infer::{Inferator, TypeSet};
use leema::ixpr::{Ixpr, Source};
use leema::log;
use leema::lri::Lri;
use leema::lstr::Lstr;
use leema::phase0::Protomod;
use leema::struple::Struple;
use leema::val::{Type, TypeErr, TypeResult, Val};

use std::collections::{HashMap, LinkedList};
use std::io::Write;

#[derive(Debug)]
pub enum CallOp
{
    LocalCall(Lstr),
    ExternalCall(Lri),
}

#[derive(Debug)]
pub struct CallFrame<'a>
{
    modname: &'a str,
    fname: &'a str,
    infer: Inferator<'a>,
    pub calls: LinkedList<CallOp>,
}

impl<'a> CallFrame<'a>
{
    pub fn new(modname: &'a str, fname: &'a str) -> CallFrame<'a>
    {
        CallFrame {
            modname: modname,
            fname: fname,
            infer: Inferator::new(fname),
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
            Source::Call(ref callx, ref args) => {
                self.collect_callexpr(callx);
                self.collect_calls(args);
            }
            Source::Block(ref expressions) => {
                self.collect_calls_vec(expressions);
            }
            Source::Let(ref _lhs, ref rhs, ref failed) => {
                self.collect_calls(rhs);
                // would be better to pass the iterator directly instead
                // of creating a new vector, but I don't know how to do
                // that right now and I'm only at this cafe for so long today
                for f in failed.iter() {
                    if f.case.is_none() {
                        continue;
                    }
                    self.collect_calls(f.case.as_ref().unwrap())
                }
            }
            Source::StrMash(ref items) => {
                self.collect_calls_vec(items);
            }
            Source::IfExpr(ref cond, ref truth, ref lies) => {
                self.collect_calls(cond);
                self.collect_calls(truth);
                if lies.is_some() {
                    self.collect_calls(lies.as_ref().unwrap());
                }
            }
            Source::List(ref items) => {
                for i in items {
                    self.collect_calls(i);
                }
            }
            Source::Tuple(ref items) => {
                for i in items.0.iter() {
                    self.collect_calls(&i.1);
                }
            }
            Source::ConstVal(_) => {
                // nothing to do. constants aren't calls.
            }
            Source::Id(ref _id, _) => {
                // nothing to do. not calls.
            }
            Source::MatchExpr(ref sample, ref cases) => {
                self.collect_calls(sample);
                self.collect_calls(cases);
            }
            Source::MatchCase(_, ref truth, ref lies) => {
                self.collect_calls(truth);
                self.collect_calls(lies);
            }
            Source::Return(ref result) => {
                self.collect_calls(result);
            }
            Source::Func(ref _args, ref body) => {
                self.collect_calls(body);
            }
            Source::Cons(ref head, ref tail) => {
                self.collect_calls(head);
                self.collect_calls(tail);
            }
            Source::FieldAccess(ref base, _, _) => {
                self.collect_calls(base);
            }
            // nothing to do for these, not calls.
            Source::RustBlock => {}
            Source::Construple(_, _) => {}
            Source::EnumConstructor(_, _, _) => {}
        }
    }

    pub fn collect_callexpr<'b>(&mut self, callx: &'b Ixpr)
    {
        match callx.src {
            Source::Id(ref callname, _) => {
                self.push_call(CallOp::LocalCall(callname.clone()));
            }
            Source::ConstVal(ref val) => {
                match val {
                    &Val::Str(ref name) => {
                        self.push_call(CallOp::LocalCall(name.clone()));
                    }
                    &Val::FuncRef(ref i, _) => {
                        self.push_call(CallOp::ExternalCall(i.clone()));
                    }
                    _ => {
                        panic!("const val is not a call: {:?}", val);
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
        for x in xvec.iter() {
            self.collect_calls(x);
        }
    }
}

#[derive(Copy)]
#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
pub enum Depth
{
    Phase0,
    Inter,
    One,
    Full,
}

impl Depth
{
    pub fn one_deeper(&self) -> bool
    {
        match self {
            &Depth::Inter => false,
            &Depth::One => true,
            &Depth::Full => true,
            &Depth::Phase0 => {
                panic!("cannot check one_deeper for Depth::Phase0")
            }
        }
    }

    pub fn next(&self) -> Depth
    {
        match self {
            &Depth::One => Depth::Inter,
            &Depth::Full => Depth::Full,
            &Depth::Phase0 => panic!("cannot get next for Depth::Phase0"),
            &Depth::Inter => panic!("cannot get next for Depth::Inter"),
        }
    }
}

/**
 * Typemod
 *
 * Function scoped type checking
 * Type from preface
 * Type from first compilation
 * Type from one deep compilations
 * Type from full depth compilation
 */
#[derive(Debug)]
pub struct Typemod
{
    pub modname: Lstr,
    functions: HashMap<Lstr, Type>,
}

impl Typemod
{
    pub fn new(modname: Lstr) -> Typemod
    {
        Typemod {
            modname,
            functions: HashMap::new(),
        }
    }

    pub fn name(&self) -> &str
    {
        self.modname.str()
    }

    pub fn set_function_type(&mut self, fname: Lstr, typ: Type)
    {
        self.functions.insert(fname, typ);
    }

    pub fn get_function_type(&self, fname: &str) -> Option<&Type>
    {
        self.functions.get(fname)
    }
}

#[derive(Debug)]
pub struct Typescope<'a, 'b>
{
    pub fname: &'b str,
    proto: &'a Protomod,
    inter: &'a Typemod,
    imports: &'a HashMap<Lstr, &'a Typemod>,
    infer: Inferator<'b>,
    typeset: &'a TypeSet<'a>,
}

impl<'a, 'b> Typescope<'a, 'b>
{
    pub fn new(
        inter: &'a Typemod,
        proto: &'a Protomod,
        func: &'b str,
        imps: &'a HashMap<Lstr, &'a Typemod>,
        typeset: &'a TypeSet<'a>,
    ) -> Typescope<'a, 'b>
    {
        Typescope {
            fname: func,
            proto,
            inter,
            imports: imps,
            infer: Inferator::new(func),
            typeset,
        }
    }

    pub fn typecheck_matchcase(
        &mut self,
        valtype: &Type,
        case: &mut Ixpr,
    ) -> TypeResult
    {
        // this is_matchcase check is a borrow-checker workaround
        // b/c i had trouble calling typecheck_expr in the
        // else clause b/c case was still held
        // as being borrowed from the if check
        if case.src.is_matchcase() {
            if let &mut Source::MatchCase(
                ref patt,
                ref mut truth,
                ref mut lies,
            ) = &mut case.src
            {
                self.infer.push_block(HashMap::new());
                self.infer.match_pattern(
                    &self.typeset,
                    patt,
                    valtype,
                    case.line,
                )?;
                let ttype = typecheck_expr(self, truth).unwrap();
                self.infer.pop_block();
                if lies.src != Source::ConstVal(Val::Void) {
                    let ftype =
                        self.typecheck_matchcase(valtype, lies).unwrap();
                    self.infer.merge_types(&ttype, &ftype)
                } else {
                    Ok(ttype)
                }
            } else {
                panic!("not a matchcase somehow");
            }
        } else {
            typecheck_expr(self, case)
        }
    }

    pub fn typecheck_call_func(&mut self, src: &Source) -> TypeResult
    {
        match src {
            &Source::Tuple(ref items) => {
                println!("tuples as calls are deprecated: {:?}", items);
                if items.0.len() != 2 {
                    panic!("call tuples should have 2 items: {:?}", items);
                }
                let ref _modname = items.0[0];
                let ref _funcname = items.0[1];
                Ok(Type::Void)
            }
            &Source::ConstVal(ref fval) => {
                match fval {
                    &Val::Str(_) => Ok(Type::Void),
                    &Val::FuncRef(ref fri, ref typ) => {
                        let typed =
                            self.functype(fri.mod_ref().unwrap(), &fri.localid);
                        self.infer.merge_types(typ, &typed)
                    }
                    _ => {
                        panic!("what val is in typecheck_call? {:?}", fval);
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
        if modname == self.inter.name() {
            self.inter
                .get_function_type(funcname)
                .expect("missing typed object for module")
                .clone()
        } else {
            let m = self.imports.get(modname).expect(&format!(
                "cannot find module {} in {:?}",
                modname, self.imports
            ));
            m.get_function_type(funcname)
                .expect(&format!(
                    "cannot find function {}::{} in {:?}",
                    modname, funcname, m
                )).clone()
        }
    }
}

pub fn typecheck_expr(scope: &mut Typescope, ix: &mut Ixpr) -> TypeResult
{
    match &mut ix.src {
        &mut Source::Call(ref mut func, ref mut args) => {
            let tfunc = scope.typecheck_call_func(&func.src).unwrap();
            let mut targs = vec![];
            if let Source::Tuple(ref mut argstup) = args.src {
                for mut a in &mut argstup.0 {
                    targs.push(typecheck_expr(scope, &mut a.1).unwrap());
                }
            } else {
                println!("args are not a tuple");
            }
            let mut targs_ref = vec![];
            for ta in targs.iter() {
                targs_ref.push(ta);
            }
            let full_call_type =
                scope.infer.make_call_type(&tfunc, &targs_ref).unwrap();
            let (_, call_result) = Type::split_func(full_call_type);
            Ok(call_result.clone())
        }
        &mut Source::Cons(ref mut head, ref mut tail) => {
            let head_t = typecheck_expr(scope, head).unwrap();
            let tail_t = typecheck_expr(scope, tail).unwrap();
            let head_list_t = Type::StrictList(Box::new(head_t));
            scope.infer.merge_types(&head_list_t, &tail_t)
        }
        &mut Source::ConstVal(_) => Ok(ix.typ.clone()),
        &mut Source::Let(ref lhs, ref mut rhs, ref mut fails) => {
            let rhs_type = typecheck_expr(scope, rhs)?;
            for mut f in fails {
                if f.case.is_none() {
                    continue;
                }
                typecheck_expr(scope, f.case.as_mut().unwrap())?;
            }
            scope
                .infer
                .match_pattern(&scope.typeset, lhs, &rhs_type, ix.line)
                .map(|_| Type::Void)
        }
        &mut Source::Block(ref mut elems) => {
            let mut last_type = Ok(Type::Void);
            for e in elems {
                last_type = typecheck_expr(scope, e);
            }
            last_type
        }
        &mut Source::FieldAccess(ref mut x, ref sub, ref mut subidx) => {
            let xtyp = typecheck_expr(scope, x)?;
            typecheck_field_access(scope, subidx, &xtyp, sub)
        }
        &mut Source::Id(ref id, _) => {
            let result = scope.infer.vartype(id);
            if result.is_ok() {
                ix.typ = result.as_ref().unwrap().clone();
            }
            result
        }
        &mut Source::List(ref mut items) => {
            for i in items {
                typecheck_expr(scope, i)?;
            }
            Ok(ix.typ.clone())
        }
        &mut Source::Construple(ref typ, _) => Ok(typ.clone()),
        &mut Source::Tuple(ref mut items) => {
            let mut item_types = Vec::with_capacity(items.0.len());
            for mut i in &mut items.0 {
                let xt = typecheck_expr(scope, &mut i.1)?;
                item_types.push((i.0.clone(), xt));
            }
            Ok(Type::Tuple(Struple(item_types)))
        }
        &mut Source::IfExpr(ref mut cond, ref mut truth, ref mut lies) => {
            let cond_t = typecheck_expr(scope, cond).unwrap();
            scope.infer.merge_types(&cond_t, &Type::Bool)?;

            let truth_result = typecheck_expr(scope, truth);
            match lies {
                &mut None => truth_result,
                &mut Some(ref mut some_lies) => {
                    let lies_result = typecheck_expr(scope, some_lies);
                    let truth_t = truth_result.unwrap();
                    let lies_t = lies_result.unwrap();
                    scope.infer.merge_types(&truth_t, &lies_t)
                }
            }
        }
        &mut Source::StrMash(ref mut items) => {
            for i in items {
                typecheck_expr(scope, i)?;
                // TODO: check that this supports the stringification interface
                // let it = typecheck_expr(scope, i);
                // scope.infer.match_types(&it, &Type::Str);
            }
            Ok(Type::Str)
        }
        &mut Source::Func(ref _args, ref body) => {
            // typecheck_expr(scope, body)
            Err(TypeErr::Error(Lstr::from(format!(
                "unexpected func in typecheck: {:?}",
                body
            ))))
        }
        &mut Source::MatchExpr(ref mut subject, ref mut cases) => {
            typecheck_expr(scope, subject).and_then(|subject_type| {
                scope.typecheck_matchcase(&subject_type, cases)
            })
        }
        &mut Source::Return(ref mut result) => typecheck_expr(scope, result),
        &mut Source::MatchCase(ref pattern, _, _) => {
            panic!("cannot directly typecheck matchcase: {}", pattern);
        }
        src => {
            panic!("could not typecheck_expr({:?})", src);
        }
    }
}

pub fn typecheck_function(scope: &mut Typescope, ix: &mut Ixpr) -> TypeResult
{
    vout!("typecheck function({:?}: {:?})", scope.fname, ix.typ);
    vout!("typescope: {:?}\n", scope);
    match (&mut ix.src, &ix.typ) {
        (
            &mut Source::Func(ref arg_names, ref mut body),
            &Type::Func(ref arg_types, ref declared_result_type),
        ) => {
            let zips = arg_names.iter().zip(arg_types.iter());
            for (i, (an, at)) in zips.enumerate() {
                scope.infer.init_param(i as i16, an, at, ix.line)?;
            }
            vout!("f({:?}) =>\n{:?}", arg_names, body);
            let result_type = typecheck_expr(scope, &mut *body)
                .map_err(|e| {
                    let err_msg = format!(
                        "function result type error for: {}",
                        scope.fname
                    );
                    e.add_context(Lstr::from(err_msg))
                }).unwrap();

            vout!("type is: {}", result_type);
            vout!("vars:");
            for var in scope.infer.vars() {
                let typ = scope.infer.vartype(var);
                vout!("\t{}: {}", var, typ.unwrap());
            }
            let final_args = arg_types
                .iter()
                .map(|at| scope.infer.inferred_type(at).clone())
                .collect();
            scope
                .infer
                .merge_types(&result_type, declared_result_type)
                .map(|final_type| Type::f(final_args, final_type))
        }
        (&mut Source::RustBlock, _) => Ok(ix.typ.clone()),
        (ref mut src, _) => {
            panic!("Cannot typecheck_function a not function: {:?}", src);
        }
    }
    /*
    scope.add_parameters(fix)
    */
}

pub fn typecheck_field_access(
    scope: &mut Typescope,
    fldidx: &mut Option<i8>,
    xtyp: &Type,
    fld: &Lstr,
) -> TypeResult
{
    vout!("typecheck_field_access({:?}.{})\n", xtyp, fld);
    match xtyp {
        &Type::UserDef(ref name) => {
            match scope.typeset.get_typedef(name) {
                Result::Ok(ref ityp) => {
                    ityp.find(fld)
                        .map(|(i, ft)| {
                            *fldidx = Some(i as i8);
                            ft.clone()
                        }).ok_or_else(|| {
                            TypeErr::Error(Lstr::Sref("invalid field index"))
                        })
                }
                Result::Err(ref e) => {
                    panic!("cannot find defined type for: {}", e);
                }
            }
        }
        _ => {
            panic!("cannot access field for type: {:?}.{}", xtyp, fld);
        }
    }
}


#[cfg(test)]
mod tests
{
    use leema::loader::Interloader;
    use leema::lri::Lri;
    use leema::lstr::Lstr;
    use leema::program;
    use leema::typecheck::Depth;


    #[test]
    #[should_panic]
    #[ignore] // to fix when typechecking is restored
    fn test_pattern_type_inferred_mismatch()
    {
        let input = "
            ## foo should take [#] and return a #
            func foo(inputs)
            |([]) -> #empty
            |(#whatever;more) -> #whatever
            |(_;more) -> foo(more)
            --

            func main() ->
                foo([5, 3, 4])
            --
            ".to_string();

        let mut loader = Interloader::new(Lstr::Sref("tacos.lma"));
        loader.set_mod_txt(Lstr::Sref("tacos"), input);
        let mut prog = program::Lib::new(loader);
        let fri = Lri::with_modules(Lstr::from("tacos"), Lstr::from("main"));
        prog.typecheck(&fri, Depth::Full);
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
