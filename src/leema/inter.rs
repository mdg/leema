
use leema::ast;
use leema::ixpr::{Ixpr, Source};
use leema::infer::{Inferator};
use leema::list;
use leema::module::{ModKey};
use leema::phase0::{Protomod};
use leema::sxpr;
use leema::val::{Val, SxprType, Type};

use std::collections::{HashMap, HashSet};
use std::fmt;
use std::rc::{Rc};
use std::path::{PathBuf};
use std::fs::File;
use std::borrow::{Cow};


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
    parent: Option<Box<Blockscope>>,
    // registers of locally defined labels
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
    blk: Blockscope,
    // types of locally defined labels
    T: Inferator,
}

impl<'a> Interscope<'a>
{
    pub fn new(proto: &'a Protomod, imports: &'a HashMap<String, Rc<Protomod>>
            , fname: &'a str, args: &Vec<String>, argt: &Vec<Type>
            ) -> Interscope<'a>
    {
        let mut e = HashSet::new();
        let mut t = Inferator::new();

        for (an, at) in args.iter().zip(argt) {
            e.insert(an.clone());
            t.bind_vartype(an, at);
        }

        let blk = Blockscope{
            parent: None,
            E: e,
        };
        Interscope{
            fname: fname,
            proto: proto,
            imports: imports,
            blk: blk,
            T: t,
        }
    }

    pub fn push_block(&mut self)
    {
    }

    pub fn pop_block(&mut self)
    {
    }

    pub fn add_var(&mut self, name: &str, typ: &Type)
    {
        if self.blk.E.contains(name) {
            panic!("variable is already declared: {}", name);
        }
        self.T.bind_vartype(name, typ);
        self.blk.E.insert(String::from(name));
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
                Some((ScopeLevel::External, proto.valtype(name).unwrap()))
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
        if self.blk.E.contains(name) {
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
        self.blk.E.contains(name)
    }

    pub fn imports_module(&self, name: &str) -> bool
    {
        self.imports.contains_key(name)
    }
}


pub fn compile_function<'a>(proto: &'a Protomod
        , imports: &'a HashMap<String, Rc<Protomod>>, fname: &'a str
        , ftype: &Type, args: &Vec<String>, body: &Val) -> Ixpr
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
        src: Source::Func(Box::new(ibody)),
    }
}

pub fn compile_expr(scope: &mut Interscope, x: &Val) -> Ixpr
{
    match x {
        &Val::Sxpr(SxprType::BlockExpr, ref blk) => {
            scope.push_block();
            let iblk = compile_list_to_vec(scope, blk);
            scope.pop_block();
            Ixpr::new_block(iblk)
        }
        &Val::Sxpr(SxprType::Call, ref callinfo) => {
            let (callx, args) = list::take_ref(callinfo);
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
        &Val::Sxpr(SxprType::IfExpr, ref ifinfo) => {
            let (ifx, truth, lies) = list::to_ref_tuple3(ifinfo);
            let ifix = compile_expr(scope, ifx);
            let itruth = compile_expr(scope, truth);
            let ilies = compile_expr(scope, lies);
            scope.T.merge_types(&itruth.typ, &ilies.typ);
            Ixpr::new_if(ifix, itruth, ilies)
        }
        &Val::Sxpr(SxprType::Let, ref letx) => {
            let (lhs_patt, rhs_val) = list::to_ref_tuple2(letx);
            let irhs = compile_expr(scope, rhs_val);
            compile_pattern(scope, lhs_patt, &irhs.typ);
            Ixpr::new(Source::Let(lhs_patt.clone(), Box::new(irhs)))
        }
        &Val::Sxpr(SxprType::StrExpr, ref strlist) => {
            let strvec = compile_list_to_vec(scope, strlist);
            Ixpr::new_str_mash(strvec)
        }
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
                        src: Source::ConstVal(Val::Str(id.clone())),
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
        _ => {
            panic!("Cannot compile expr: {:?}", x);
        }
    }
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
        _ => {
            panic!("Unsupported pattern: {:?}", p);
        }
    }
}

pub fn split_func_args_body(defunc: &Val) -> (Vec<String>, &Val)
{
    let (st, sx) = sxpr::split_ref(defunc);
    let (_, args, _, body) = list::to_ref_tuple4(sx);
    let arg_names = list::map_ref_to_vec(args, |a| {
        String::from(a.str())
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
