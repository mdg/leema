
use leema::ixpr::{Ixpr, Source};
use leema::infer::{Inferator};
use leema::list;
use leema::log;
use leema::module::{ModKey};
use leema::phase0::{Protomod};
use leema::sxpr;
use leema::val::{Val, SxprType, Type};

use std::collections::{HashMap};
use std::fmt;
use std::io::{stderr, Write};
use std::rc::{Rc};


#[derive(PartialEq)]
#[derive(Eq)]
#[derive(PartialOrd)]
#[derive(Ord)]
#[derive(Clone)]
#[derive(Copy)]
#[derive(Debug)]
pub enum Version
{
    Sin,
    Cos,
}


/*
calling push leema code from rust
string module
sxpr module block
- raw list, imports, makros, codes

ixpr type0 func interface (and cache)
ixpr type0 func body (and cache)

ixpr type' func interface (and cache)
ixpr type' func body (and cache)

module scope
function scope

read_module ->
    open file
    return init_module( parse(lex(read(file))))
--
import module(depth) ->
    m = read_module
    assign imports
    assign macros
    assign raw funcs
    assign type0 funcs
--
load_module ->
    if module_loaded {
        return loaded_module
    }
    return import module
--
load_func(mod, func) ->
    m = self.load_module(mod)
    f0 = m.get_type0_func(func)
    ft = self.resolve_types(f0)
--

load_code(mod, func): Code ->
--
load_code_by_type(mod, func, param_types): Code ->
--

typecheck_module(mod) ->
    let m = load_module(mod)
    for import_mod in m.imports {
        typecheck_module(import_mod)
    }
    for f in m.functions {
        self.resolve_types(f)
    }
--
*/


pub struct Intermod
{
    pub key: Rc<ModKey>,
    pub interfunc: HashMap<String, Ixpr>,
}

impl Intermod
{
    pub fn new(key: Rc<ModKey>) -> Intermod
    {
        Intermod{
            key: key,
            interfunc: HashMap::new(),
        }
    }

    pub fn name(&self) -> &str
    {
        &self.key.name
    }

    pub fn compile(proto: &Protomod, imports: &HashMap<String
            , Rc<Protomod>>) -> Intermod
    {
        let mut inter = Intermod::new(proto.key.clone());
        let mut locals: HashMap<String, Type> = HashMap::new();
        for fname in proto.funcseq.iter() {
            let opt_defunc = proto.funcsrc.get(&**fname);
            if opt_defunc.is_none() {
                panic!("No function source found for {}::{}",
                    proto.key.name, fname);
            }
            let defunc = opt_defunc.unwrap();
            let (args, body) = split_func_args_body(defunc);
            let ftype = proto.valtypes.get(&**fname).unwrap();
            let ifunc = compile_function(proto, imports, &locals
                    , fname, ftype, &args, body);
            locals.insert(fname.to_string(), ifunc.typ.clone());
            inter.interfunc.insert(fname.to_string(), ifunc);
        }
        inter
    }
}

#[derive(Debug)]
#[derive(PartialEq)]
pub enum ScopeLevel
{
    Local,
    Module,
    External,
}

#[derive(Debug)]
pub struct Interscope<'a>
{
    fname: &'a str,
    proto: &'a Protomod,
    imports: &'a HashMap<String, Rc<Protomod>>,
    // types of functions already compiled in this module
    mod_locals: &'a HashMap<String, Type>,
    // types of locally defined labels
    T: Inferator<'a>,
    argnames: Vec<Rc<String>>,
    argt: Type,
}

impl<'a> Interscope<'a>
{
    pub fn new(proto: &'a Protomod, imports: &'a HashMap<String, Rc<Protomod>>
            , locals: &'a HashMap<String, Type>
            , fname: &'a str, args: &Vec<Rc<String>>, argt: &Vec<Type>
            ) -> Interscope<'a>
    {
        let mut t = Inferator::new(fname);
        for (an, at) in args.iter().zip(argt) {
            if let None = t.bind_vartype(an, at) {
                panic!("args type mismatch: {:?} != {:?}", args, argt);
            }
        }

        Interscope{
            fname: fname,
            proto: proto,
            imports: imports,
            mod_locals: locals,
            T: t,
            argnames: args.clone(),
            argt: Type::Tuple(argt.clone()),
        }
    }

    pub fn vartype(&self, name: &str) -> Option<(ScopeLevel, Type)>
    {
        let local = self.T.vartype(name);
        if local.is_some() && self.T.var_is_in_scope(name) {
            return Some((ScopeLevel::Local, local.unwrap()));
        }
        if let Some(local_valtype) = self.mod_locals.get(name) {
            return Some((ScopeLevel::Module, local_valtype.clone()));
        }
        if let Some(modtyp) = self.proto.valtype(name) {
            return Some((ScopeLevel::Module, modtyp.clone()));
        }
        match self.imports.get("prefab") {
            Some(ref proto) => {
                let valtype_opt = proto.valtype(name);
                if valtype_opt.is_none() {
                    vout!("cannot find variable: {} in {:?}\n",
                        name, self.T);
                    panic!("undefined variable: {}", name);
                }
                Some((ScopeLevel::External, valtype_opt.unwrap().clone()))
            }
            None => None,
        }
    }

    pub fn import_vartype(&self, modnm: &str, valnm: &str) -> Option<&Type>
    {
        match self.imports.get(modnm) {
            None => None,
            Some(ref proto) => {
                proto.valtype(valnm)
            }
        }
    }

    pub fn contains_id(&self, name: &str) -> bool
    {
        if self.T.var_is_in_scope(name) {
            true
        } else if self.proto.contains_val(name) {
            true
        } else {
            match self.imports.get("prefab") {
                Some(ref proto) => {
                    proto.contains_val(name)
                }
                None => false,
            }
        }
    }

    pub fn imports_module(&self, name: &str) -> bool
    {
        self.imports.contains_key(name)
    }

    pub fn struct_field_idx(&self, typ: &Type, fld: &str) -> Option<(i8, &Type)>
    {
        self.proto.struct_field_idx(typ, fld)
    }
}


pub fn compile_function<'a>(proto: &'a Protomod
        , imports: &'a HashMap<String, Rc<Protomod>>
        , locals: &'a HashMap<String, Type>, fname: &'a str
        , ftype: &Type, args: &Vec<Rc<String>>, body: &Val) -> Ixpr
{
    if *body == Val::RustBlock {
        return Ixpr{
            typ: ftype.clone(),
            src: Source::RustBlock,
        }
    }
    let (argt, _result) = Type::split_func(ftype);
    let mut scope = Interscope::new(proto, imports, locals, fname, args, argt);
    let mut ibody = compile_expr(&mut scope, body);
    let argt2: Vec<Type> = argt.iter().map(|a| {
        scope.T.inferred_type(a).clone()
    }).collect();
    let ibody2 = Ixpr{
        typ: scope.T.inferred_type(&ibody.typ).clone(),
        src: ibody.src,
    };
    let final_ftype = Type::Func(argt2, Box::new(ibody2.typ.clone()));
    vout!("compile function {}: {}\n", fname, ftype);
    for v in scope.T.vars() {
        let vtyp = scope.T.vartype(v);
        vout!("\t{}: {}\n", v, vtyp.unwrap());
    }
    vout!("\t<result>: {}\n", ibody2.typ);
    vout!("inferences: {:?}\n", scope.T);
    Ixpr{
        typ: final_ftype,
        src: Source::Func(args.clone(), Box::new(ibody2)),
    }
}

pub fn compile_expr(scope: &mut Interscope, x: &Val) -> Ixpr
{
    match x {
        &Val::Id(ref id) => {
            match scope.vartype(id) {
                Some((ScopeLevel::Local, typ)) => {
                    Ixpr{
                        src: Source::Id(id.clone()),
                        typ: typ.clone(),
                    }
                }
                Some((ScopeLevel::Module, typ)) => {
                    Ixpr{
                        src: Source::ConstVal(Val::Tuple(vec![
                            Val::Str(Rc::new(scope.proto.key.name.clone())),
                            Val::Str(id.clone()),
                        ])),
                        typ: typ.clone(),
                    }
                }
                Some((ScopeLevel::External, typ)) => {
                    // if it's external and no module prefix,
                    // it's almost certainly prefab. probably
                    // a better way to make this work
                    Ixpr{
                        src: Source::ConstVal(
                            Val::Tuple(vec![
                                Val::Str(Rc::new("prefab".to_string())),
                                Val::Str(id.clone()),
                            ])
                        ),
                        typ: typ.clone(),
                    }
                }
                None => {
                    panic!("untyped variable: {} not in {:?}", id, scope);
                }
            }
        }
        &Val::ModPrefix(ref prefix, ref inner) => {
            if !scope.imports_module(prefix) {
                panic!("module not found: {}", prefix);
            }
            let inner_id = inner.to_str();
            let opt_itype = scope.import_vartype(prefix, &*inner_id);
            if opt_itype.is_none() {
                panic!("module var not found: {:?}", x);
            }
            let itype = opt_itype.unwrap();
            Ixpr{
                typ: itype.clone(),
                src: Source::ModuleAccess(
                    prefix.clone(), inner_id.clone()
                ),
            }
        }
        &Val::DotAccess(ref outer, ref inner) => {
            let ix_outer = compile_expr(scope, outer);
            if let Some((inner_idx, inner_typ)) =
                scope.struct_field_idx(&ix_outer.typ, inner)
            {
                Ixpr::new_field_access(ix_outer, inner_idx, inner_typ.clone())
            } else {
                panic!("no field: {:?}.{}", outer, inner);
            }
        }
        &Val::Bool(b) => {
            Ixpr::const_val(Val::Bool(b))
        }
        &Val::Int(i) => {
            Ixpr::const_val(Val::Int(i))
        }
        &Val::Str(ref s) => {
            Ixpr::const_val(Val::Str(s.clone()))
        }
        &Val::Hashtag(ref s) => {
            Ixpr::const_val(Val::Hashtag(s.clone()))
        }
        &Val::Cons(_, _) => {
            let items = list::map_ref_to_vec(x, |ref mut i| {
                compile_expr(scope, i)
            });
            Ixpr::new_list(items)
        }
        &Val::Nil => {
            Ixpr::new_list(vec![])
        }
        &Val::Tuple(ref items) => {
            let c_items = items.iter().map(|i| {
                compile_expr(scope, i)
            }).collect();
            Ixpr::new_tuple(c_items)
        }
        &Val::Sxpr(st, ref sx) => compile_sxpr(scope, st, sx),
        &Val::Struct(ref typ, ref flds) => {
            Ixpr::constructor(typ.clone(), flds.len() as i8)
        }
        &Val::Void => Ixpr::noop(),
        _ => {
            panic!("Cannot compile expr: {:?}", x);
        }
    }
}

pub fn compile_sxpr(scope: &mut Interscope, st: SxprType, sx: &Val) -> Ixpr
{
    match st {
        SxprType::BlockExpr => {
            scope.T.push_block();
            let iblk = compile_block(scope, sx);
            scope.T.pop_block();
            iblk
        }
        SxprType::Call => {
            let (callx, args) = list::take_ref(sx);
            let icall = compile_expr(scope, callx);
            let iargs = compile_list_to_vec(scope, &*args);
            let ftype = {
                let iargst: Vec<&Type> = iargs.iter().map(|ia| {
                    &ia.typ
                }).collect::<Vec<&Type>>();
                scope.T.make_call_type(&icall.typ, &iargst)
            };
            if ftype.is_error() {
                panic!("type error in function call: {} {:?}", callx, ftype);
            }
            let argsix = Ixpr::new_tuple(iargs);
            Ixpr{
                typ: ftype,
                src: Source::Call(Box::new(icall), Box::new(argsix)),
            }
        }
        SxprType::IfExpr => {
            let (ifx, truth, lies) = list::to_ref_tuple3(sx);
            let ifix = compile_expr(scope, ifx);
            let itruth = compile_expr(scope, truth);
            let ilies = compile_expr(scope, lies);
            let iftyp = scope.T.merge_types(&itruth.typ, &ilies.typ);
            if iftyp.is_none() {
                panic!("if/else types do not match: {:?} <> {:?}",
                    itruth.typ, ilies.typ);
            }
            Ixpr::new_if(ifix, itruth, ilies, iftyp.unwrap())
        }
        SxprType::Let => {
            let (lhs_patt, rhs_val) = list::to_ref_tuple2(sx);
            let irhs = compile_expr(scope, rhs_val);
            let cpatt = compile_pattern(scope, lhs_patt);
            scope.T.match_pattern(&cpatt, &irhs.typ);
            Ixpr::new(Source::Let(cpatt, Box::new(irhs)))
        }
        SxprType::MatchExpr => {
            let (mx, cases) = list::to_ref_tuple2(sx);
            let imx = compile_expr(scope, mx);
            let icases = compile_matchcase(scope, cases, &imx.typ);
            Ixpr::new_match_expr(imx, icases)
        }
        SxprType::MatchFailed => {
            let fx = list::head_ref(sx);
            panic!("Cannot use MatchFailed as an expression for: {}", fx);
        }
        SxprType::StrExpr => {
            let strvec = compile_list_to_vec(scope, sx);
            Ixpr::new_str_mash(strvec)
        }
        SxprType::Return => {
            let head = list::head_ref(sx);
            let chead = compile_expr(scope, head);
            Ixpr::new(Source::Return(Box::new(chead)))
        }
        _ => {
            panic!("Cannot compile sxpr: {:?} {:?}", st, sx);
        }
    }
}

pub fn compile_matchcase(scope: &mut Interscope
        , case: &Val, xtyp: &Type) -> Ixpr
{
    let (patt, t2) = list::take_ref(case);
    let (blk, t3) = list::take_ref(&*t2);

    scope.T.push_block();
    let cpatt = compile_pattern(scope, patt);
    scope.T.match_pattern(&cpatt, xtyp);
    let iblk = compile_expr(scope, blk);
    scope.T.pop_block();
    let inext = match &**t3 {
        &Val::Cons(ref next, _) if **next == Val::Void => {
            Ixpr::noop()
        }
        &Val::Cons(ref next, _) => {
            let inxt_inner = compile_matchcase(scope, next, xtyp);
            scope.T.merge_types(&iblk.typ, &inxt_inner.typ);
            inxt_inner
        }
        &Val::Nil => {
            Ixpr::noop()
        }
        _ => {
            panic!("next is not a list: {:?}", *t3);
        }
    };
    Ixpr::new_match_case(cpatt, iblk, inext)
}

pub fn compile_pattern(scope: &mut Interscope, patt: &Val) -> Val
{
    match patt {
        &Val::Id(ref name) => {
            Val::Id(name.clone())
        }
        &Val::Cons(ref head, ref tail) => {
            let chead = compile_pattern(scope, head);
            let ctail = match &**tail {
                &Val::Id(_) => {
                    compile_pattern(scope, &**tail)
                }
                &Val::Wildcard => Val::Wildcard,
                &Val::Nil => Val::Nil,
                &Val::Cons(_, _) => {
                    compile_pattern(scope, &**tail)
                }
                _ => {
                    panic!("invalid pattern tail: {:?}", tail);
                }
            };
            Val::Cons(Box::new(chead), Rc::new(ctail))
        }
        &Val::Nil => {
            Val::Nil
        }
        &Val::Tuple(ref items) => {
            let citems = items.iter().map(|i| {
                compile_pattern(scope, i)
            }).collect();
            Val::Tuple(citems)
        }
        &Val::Int(i) => {
            Val::Int(i)
        }
        &Val::Bool(b) => {
            Val::Bool(b)
        }
        &Val::Str(ref s) => {
            Val::Str(s.clone())
        }
        &Val::Hashtag(ref h) => {
            Val::Hashtag(h.clone())
        }
        &Val::Wildcard => {
            Val::Wildcard
        }
        &Val::Sxpr(SxprType::Call, ref callx) => {
            compile_pattern_call(scope, callx)
        }
        _ => {
            panic!("invalid pattern: {:?}", patt);
        }
    }
}

pub fn compile_pattern_call(scope: &mut Interscope, patt: &Val) -> Val
{
    let (callx, args) = list::take_ref(patt);
    let struct_flds = match callx {
        &Val::Id(ref name) => {
            let flds = scope.proto.structfields.get(callx.str());
            if flds.is_none() {
                panic!("Unknown type: {}", callx.str());
            }
            flds.unwrap()
        }
        &Val::ModPrefix(ref prefix, ref name) => {
            let opt_flds = if scope.proto.key.name == **prefix {
                let iflds = scope.proto.structfields.get(name.str());
                if iflds.is_some() {
                    iflds
                } else {
                    let prefix_name = callx.to_str();
                    scope.proto.structfields.get(&*prefix_name)
                }
            } else {
                let opt_imp = scope.imports.get(&**prefix);
                if opt_imp.is_none() {
                    panic!("missing import: {}", prefix);
                }
                let imp = opt_imp.unwrap();
                imp.structfields.get(name.str())
            };
            if opt_flds.is_none() {
                panic!("unknown type: {}", name.str());
            }
            opt_flds.unwrap()
        }
        _ => {
            panic!("cannot match pattern call: {:?}", patt);
        }
    };
    let args_vec = list::map_ref_to_vec(&*args, |a| {
        compile_pattern(scope, a)
    });
    if args_vec.len() < struct_flds.len() {
        panic!("too few fields in struct pattern for: {}", callx.str());
    }
    if args_vec.len() > struct_flds.len() {
        panic!("too many fields in struct pattern for: {}", callx.str());
    }
    for (arg, fld) in args_vec.iter().zip(struct_flds.iter()) {
        let &(_, ref fldtype) = fld;
        scope.T.bind_vartype(arg.str(), fldtype);
    }
    let calltyp = Rc::new(callx.to_type());
    let struct_type = Type::Struct(calltyp);
    Val::Struct(struct_type, args_vec.clone())
}

pub fn compile_block(scope: &mut Interscope, l: &Val) -> Ixpr
{
    let block_len = list::len(l);
    let mut result = Vec::with_capacity(block_len);
    let mut fails = HashMap::new();
    list::fold_mut_ref(&mut (&mut result, &mut fails, scope), l
        , |&mut (ref mut ilines, ref mut ifails, ref mut iscope), line| {
            compile_block_stmt(ilines, ifails, iscope, line);
        });
    Ixpr::new_block(result, fails)
}

pub fn compile_block_stmt(istmts: &mut Vec<Ixpr>
    , ifails: &mut HashMap<String, Ixpr>
    , scope: &mut Interscope
    , stmt: &Val
    )
{
    match stmt {
        &Val::Sxpr(SxprType::MatchFailed, ref sx) => {
            scope.T.mark_failing();
            compile_failed_stmt(ifails, scope, sx);
        }
        _ => {
            istmts.push(compile_expr(scope, stmt));
        }
    }
}

pub fn compile_list_to_vec(scope: &mut Interscope, l: &Val) -> Vec<Ixpr>
{
    let mut result = Vec::with_capacity(list::len(l));
    list::fold_mut_ref(&mut (&mut result, scope), l,
        |&mut (ref mut dst, ref mut scp), x| {
            dst.push(compile_expr(*scp, x));
        }
    );
    result
}

pub fn compile_failed_stmt(fails: &mut HashMap<String, Ixpr>
    , scope: &mut Interscope
    , sx: &Val
    )
{
    let (fvar, cases) = list::to_ref_tuple2(sx);
    let varname = match fvar {
        &Val::Id(ref name) => {
            if fails.contains_key(&**name) {
                panic!("cannot redefine failure handler for: {}", name);
            }
            (**name).clone()
        }
        _ => {
            panic!("the test expression in a failed statement must be an identifier");
        }
    };

    let ifx = compile_expr(scope, fvar);
    let icases = compile_matchcase(scope, cases, &ifx.typ);
    let imatchfailed = Ixpr::new_match_expr(ifx, icases);
    fails.insert(varname, imatchfailed);
}

pub fn split_func_args_body(defunc: &Val) -> (Vec<Rc<String>>, &Val)
{
    let (st, sx) = sxpr::split_ref(defunc);
    let (_, args, _, body) = list::to_ref_tuple4(sx);
    let arg_names = list::map_ref_to_vec(args, |a| {
        a.to_str()
    });
    (arg_names, body)
}

impl fmt::Debug for Intermod
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "Intermod{{\n").ok();
        write!(f, "\tname: {}\n", self.key.name).ok();
        write!(f, "\tinterfunc:\n").ok();
        for (fname, fix) in self.interfunc.iter() {
            write!(f, "\t\t{}: {:?}\n", fname, fix).ok();
        }
        write!(f, "}}\n")
    /*
    imports: HashSet<String>,
    macros: HashMap<String, Val>,
    srcfunc: HashMap<String, Val>,
    interfunc: HashMap<String, Ixpr>,
    */
    }
}


#[cfg(test)]
mod tests {
    use leema::inter::{ScopeLevel, Interscope};
    use leema::log;
    use leema::loader::{Interloader};
    use leema::module::{ModKey};
    use leema::phase0::{Protomod};
    use leema::program;
    use leema::val::{Type};

    use std::rc::{Rc};
    use std::io::{stderr, Write};
    use std::collections::{HashMap};


#[test]
fn test_scope_add_vartype()
{
    let mk = Rc::new(ModKey::name_only("tacos"));
    let proto = Protomod::new(mk);
    let imps = HashMap::new();
    let args = vec![];
    let argt = vec![];
    let locals = HashMap::new();
    let mut scope = Interscope::new(&proto, &imps, &locals
        , "foo", &args, &argt);
    scope.T.bind_vartype("hello", &Type::Int);

    let (scope_lvl, typ) = scope.vartype("hello").unwrap();
    assert_eq!(ScopeLevel::Local, scope_lvl);
    assert_eq!(Type::Int, typ);
}

#[test]
fn test_scope_push_block()
{
    let mk = Rc::new(ModKey::name_only("tacos"));
    let proto = Protomod::new(mk);
    let imps = HashMap::new();
    let args = vec![];
    let argt = vec![];
    let locals = HashMap::new();
    let mut scope = Interscope::new(&proto, &imps, &locals
        , "foo", &args, &argt);
    scope.T.bind_vartype("hello", &Type::Int);
    println!("add_var(hello) -> {:?}", scope);

    {
        let (hello_lvl, hello_typ) = scope.vartype("hello").unwrap();
        assert_eq!(ScopeLevel::Local, hello_lvl);
        assert_eq!(Type::Int, hello_typ);
    }

    scope.T.push_block();
    scope.T.bind_vartype("world", &Type::Str);
    println!("push_block().add_var(world) -> {:?}", scope);

    {
        let (world_lvl, world_typ) = scope.vartype("world").unwrap();
        assert_eq!(ScopeLevel::Local, world_lvl);
        assert_eq!(Type::Str, world_typ);

        let (hello_lvl, hello_typ) = scope.vartype("hello").unwrap();
        assert_eq!(ScopeLevel::Local, hello_lvl);
        assert_eq!(Type::Int, hello_typ);
    }

    scope.T.pop_block();

    assert_eq!(None, scope.vartype("world"));
    assert!(scope.vartype("hello").is_some());
}

#[test]
#[should_panic]
fn test_too_many_args()
{
    let input = String::from("

    func sum(a, b) -> a + b --
    func main() -> sum(3, 4, 5) --
    ");

    let mut loader = Interloader::new("tacos.lma");
    loader.set_mod_txt("tacos", input);
    let mut prog = program::Lib::new(loader);
    let imod = prog.read_inter("tacos");
}

#[test]
fn test_pattern_declaration()
{
    let input = String::from("

    func foo(inputs: [#])
    |[] -> #empty
    |#whatever;more -> #whatever
    |_;more -> foo(more)
    --

    func main() ->
        foo([#a, #b, #c])
    --
    ");

    let mut loader = Interloader::new("tacos.lma");
    loader.set_mod_txt("tacos", input);
    let mut prog = program::Lib::new(loader);
    let imod = prog.read_inter("tacos");
    assert!(true); // didn't panic earlier
}

#[test]
#[should_panic]
fn test_pattern_type_explicit_mismatch()
{
    let input = String::from("

    func foo(inputs: [#])
    |([]) -> #empty
    |([#whatever;more]) -> #whatever
    |([_;more]) -> foo(more)
    --

    func main() ->
        foo([5, 3, 4])
    --
    ");

    let mut loader = Interloader::new("tacos.lma");
    loader.set_mod_txt("tacos", input);
    let mut prog = program::Lib::new(loader);
    let imod = prog.read_inter("tacos");
}

#[test]
#[should_panic]
fn test_pattern_type_inferred_mismatch()
{
    let input = String::from("

    ## foo should return a #
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
    let imod = prog.read_inter("tacos");
}

}
