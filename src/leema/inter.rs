
use leema::ixpr::{Ixpr, Source};
use leema::infer::{Inferator};
use leema::list;
use leema::log;
use leema::module::{ModKey};
use leema::phase0::{Protomod};
use leema::sxpr;
use leema::val::{Val, SxprType, Type};

use std::collections::{HashMap, HashSet};
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

    pub fn compile(proto: &Protomod, imports: &HashMap<String, Rc<Protomod>>)
            -> Intermod
    {
        let mut inter = Intermod::new(proto.key.clone());
        for (fname, defunc) in proto.funcsrc.iter() {
            let (args, body) = split_func_args_body(defunc);
            let ftype = proto.valtypes.get(fname).unwrap();
            let ifunc = compile_function(proto, imports
                    , fname, ftype, &args, body);
            inter.interfunc.insert(fname.to_string(), ifunc);
        }
        inter
    }
}

#[derive(Debug)]
struct Blockscope
{
    // set of locally defined labels
    E: HashSet<String>,
}

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
    blkstk: Vec<Blockscope>,
    // types of locally defined labels
    T: Inferator,
}

impl<'a> Interscope<'a>
{
    pub fn new(proto: &'a Protomod, imports: &'a HashMap<String, Rc<Protomod>>
            , fname: &'a str, args: &Vec<Rc<String>>, argt: &Vec<Type>
            ) -> Interscope<'a>
    {
        let mut e = HashSet::new();
        let mut t = Inferator::new();

        for (an, at) in args.iter().zip(argt) {
            e.insert((&**an).clone());
            t.bind_vartype(an, at);
        }

        let blk = Blockscope{
            E: e,
        };
        Interscope{
            fname: fname,
            proto: proto,
            imports: imports,
            blkstk: vec![blk],
            T: t,
        }
    }

    pub fn push_block(&mut self)
    {
        self.blkstk.push(Blockscope{
            E: HashSet::new()
        });
    }

    pub fn pop_block(&mut self)
    {
        self.blkstk.pop();
    }

    pub fn blk(&self) -> &Blockscope
    {
        self.blkstk.last().unwrap()
    }

    pub fn add_var(&mut self, name: &str, typ: &Type)
    {
        if self.blk().E.contains(name) {
            panic!("variable is already declared: {}", name);
        }
        self.T.bind_vartype(name, typ);
        self.blkstk.last_mut().unwrap().E.insert(String::from(name));
    }

    pub fn vartype(&self, name: &str) -> Option<(ScopeLevel, &Type)>
    {
        let local = self.T.vartype(name);
        if local.is_some() {
            return Some((ScopeLevel::Local, local.unwrap()));
        }
        let modtyp = self.proto.valtype(name);
        if modtyp.is_some() {
            return Some((ScopeLevel::Module, modtyp.unwrap()));
        }
        match self.imports.get("prefab") {
            Some(ref proto) => {
                let valtype_opt = proto.valtype(name);
                if valtype_opt.is_none() {
                    panic!("could not find {} in prefab", name);
                }
                Some((ScopeLevel::External, valtype_opt.unwrap()))
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
        if self.contains_local(name) {
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

    pub fn contains_local(&self, name: &str) -> bool
    {
        for b in self.blkstk.iter() {
            if b.E.contains(name) {
                return true;
            }
        }
        false
    }

    pub fn imports_module(&self, name: &str) -> bool
    {
        self.imports.contains_key(name)
    }
}


pub fn compile_function<'a>(proto: &'a Protomod
        , imports: &'a HashMap<String, Rc<Protomod>>, fname: &'a str
        , ftype: &Type, args: &Vec<Rc<String>>, body: &Val) -> Ixpr
{
    if *body == Val::RustBlock {
        return Ixpr{
            typ: ftype.clone(),
            src: Source::RustBlock,
        }
    }
    let (argt, result) = Type::split_func(ftype);
    let mut scope = Interscope::new(proto, imports, fname, args, argt);
    let ibody = compile_expr(&mut scope, body);
    let argt2: Vec<Type> = argt.iter().map(|a| {
        scope.T.inferred_type(a).clone()
    }).collect();
    let final_ftype = Type::Func(argt2, Box::new(ibody.typ.clone()));
    Ixpr{
        typ: final_ftype,
        src: Source::Func(args.clone(), Box::new(ibody)),
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
        &Val::DotAccess(ref outer, ref inner) => {
            match &**outer {
                &Val::Id(ref outer_id) => {
                    if scope.contains_local(outer_id) {
                        Ixpr::new(Source::FieldAccess(
                            Box::new(Ixpr::new(Source::Id(outer_id.clone()))),
                            inner.clone(),
                        ))
                    } else if scope.imports_module(outer_id) {
                        let opt_itype = scope.import_vartype(outer_id, inner);
                        if opt_itype.is_none() {
                            panic!("module var not found: {:?}", x);
                        }
                        let itype = opt_itype.unwrap();
                        Ixpr{
                            typ: itype.clone(),
                            src: Source::ModuleAccess(
                                outer_id.clone(), inner.clone()
                            ),
                        }
                    } else {
                        panic!("unknown identifier: {}", outer_id);
                    }
                }
                _ => {
                    panic!("dot access unsupported: {:?}", outer);
                }
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
        &Val::Sxpr(st, ref sx) => compile_sxpr(scope, st, sx),
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
            scope.push_block();
            let iblk = compile_list_to_vec(scope, sx);
            scope.pop_block();
            Ixpr::new_block(iblk)
        }
        SxprType::Call => {
            let (callx, args) = list::take_ref(sx);
            let icall = compile_expr(scope, callx);
            let iargs = compile_list_to_vec(scope, args);
            let ftype = {
                let iargst: Vec<&Type> = iargs.iter().map(|ia| {
                    &ia.typ
                }).collect::<Vec<&Type>>();
                scope.T.make_call_type(&icall.typ, &iargst)
            };
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
            scope.T.merge_types(&itruth.typ, &ilies.typ);
            Ixpr::new_if(ifix, itruth, ilies)
        }
        SxprType::Let => {
            let (lhs_patt, rhs_val) = list::to_ref_tuple2(sx);
            let irhs = compile_expr(scope, rhs_val);
            compile_pattern(scope, lhs_patt, &irhs.typ);
            Ixpr::new(Source::Let(lhs_patt.clone(), Box::new(irhs)))
        }
        SxprType::MatchExpr => {
            let (mx, cases) = list::to_ref_tuple2(sx);
            let imx = compile_expr(scope, mx);
            let icases = compile_matchcase(scope, cases, &imx.typ);
            Ixpr::new_match_expr(imx, icases)
        }
        SxprType::StrExpr => {
            let strvec = compile_list_to_vec(scope, sx);
            Ixpr::new_str_mash(strvec)
        }
        _ => {
            panic!("Cannot compile sxpr: {:?} {:?}", st, sx);
        }
    }
}

pub fn compile_matchcase(scope: &mut Interscope, case: &Val, xtyp: &Type
) -> Ixpr
{
    let (patt, t2) = list::take_ref(case);
    let (blk, t3) = list::take_ref(t2);
    scope.push_block();
    compile_pattern(scope, patt, xtyp);
    let iblk = compile_expr(scope, blk);
    scope.pop_block();
    let inext = match t3 {
        &Val::Cons(ref next, _) => {
            compile_matchcase(scope, next, xtyp)
        }
        &Val::Nil => {
            Ixpr::noop()
        }
        _ => {
            panic!("next is not a list: {:?}", *t3);
        }
    };
    Ixpr::new_match_case(patt.clone(), iblk, inext)
}

pub fn compile_list_to_vec(scope: &mut Interscope, l: &Val) -> Vec<Ixpr>
{
    let mut result = vec![];
    list::fold_mut_ref(&mut (&mut result, scope), l,
        |&mut (ref mut dst, ref mut scp), x| {
            dst.push(compile_expr(*scp, x));
        }
    );
    result
}

pub fn compile_pattern(scope: &mut Interscope, p: &Val, srctyp: &Type)
{
    match p {
        &Val::Id(ref id) => {
            scope.add_var(&id, srctyp);
        }
        &Val::Int(_) => {
            scope.T.merge_types(&Type::Int, srctyp);
        }
        &Val::Str(_) => {
            scope.T.merge_types(&Type::Str, srctyp);
        }
        &Val::Bool(_) => {
            scope.T.merge_types(&Type::Bool, srctyp);
        }
        &Val::Hashtag(_) => {
            scope.T.merge_types(&Type::Hashtag, srctyp);
        }
        &Val::Wildcard => {
            // matches, but nothing to do
        }
        _ => {
            panic!("Unsupported pattern: {:?}", p);
        }
    }
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
