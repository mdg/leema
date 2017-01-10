
use leema::ast;
use leema::iexpr::{Iexpr, Source};
use leema::list;
use leema::module::{ModKey};
use leema::phase0::{Protomod};
use leema::sexpr;
use leema::val::{Val, SexprType, Type};

use std::collections::{HashMap, HashSet};
use std::fmt;
use std::rc::{Rc};
use std::sync::{Arc};
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
sexpr module block
- raw list, imports, makros, codes

iexpr type0 func interface (and cache)
iexpr type0 func body (and cache)

iexpr type' func interface (and cache)
iexpr type' func body (and cache)

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
    key: Rc<ModKey>,
    interfunc: HashMap<String, Iexpr>,
    imports: HashMap<String, Rc<Protomod>>,
}

impl Intermod
{
    pub fn new(key: Rc<ModKey>, imports: HashMap<String, Rc<Protomod>>)
            -> Intermod
    {
        Intermod{
            key: key,
            interfunc: HashMap::new(),
            imports: imports,
        }
    }

    pub fn name(&self) -> &str
    {
        &self.key.name
    }

    pub fn compile(proto: Rc<Protomod>, imports: HashMap<String, Rc<Protomod>>)
            -> Intermod
    {
        let mut inter = Intermod::new(proto.key.clone(), imports);
        for (fname, defunc) in proto.funcsrc.iter() {
            let (args, body) = split_func_args_body(defunc);
            let ftype = proto.valtypes.get(fname).unwrap();
            let ifunc = compile_function(proto, fname, ftype, &args, body);
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

#[derive(Debug)]
pub struct Interscope<'a>
{
    proto: &'a Protomod,
    blk: Blockscope,
    // types of locally defined labels
    T: HashMap<String, Type>,
}

impl<'a> Interscope<'a>
{
    pub fn new(proto: &'a Protomod, args: &Vec<String>, argt: &Vec<Type>
            ) -> Interscope<'a>
    {
        let mut e = HashSet::new();
        let mut t = HashMap::new();

        for (an, at) in args.iter().zip(argt) {
            e.insert(an.clone());
            t.insert(an.clone(), at.clone());
        }

        let blk = Blockscope{
            parent: None,
            E: e,
        };
        Interscope{
            proto: proto,
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

    pub fn add_var(&mut self, name: &str)
    {
        if self.blk.E.contains(name) {
            panic!("variable is already declared: {}", name);
        }
        self.blk.E.insert(String::from(name));
    }

    pub fn contains_var(&self, name: &str) -> bool
    {
        self.blk.E.contains(name)
            || self.proto.contains_val(name)
            || self.proto.contains_import_val("prefab", name)
    }
}


pub fn compile_function(proto: &Protomod, fname: &str, ftype: &Type,
        args: &Vec<String>, body: &Val) -> Iexpr
{
    let (argt, result) = Type::split_func(ftype);
    let mut scope = Interscope::new(proto, args, argt);
    compile_expr(&mut scope, proto, body)
}

pub fn compile_expr(scope: &mut Interscope, proto: &Protomod, x: &Val) -> Iexpr
{
    match x {
        &Val::Sexpr(SexprType::BlockExpr, ref blk) => {
            scope.push_block();
            let mut iblk = vec![];
            let mut blk_head: &Val = &**blk;
            while *blk_head != Val::Nil {
                let (bx, tail) = list::take_ref(blk_head);
                iblk.push(compile_expr(scope, proto, bx));
                blk_head = tail;
            }
            scope.pop_block();
            Iexpr::new_block(iblk)
        }
        &Val::Sexpr(SexprType::Call, ref callinfo) => {
            let (callx, args) = list::to_ref_tuple2(callinfo);
            let icall = compile_expr(scope, proto, callx);
            let iargs = compile_expr(scope, proto, args);
            Iexpr::new_call(icall, iargs)
        }
        &Val::Sexpr(SexprType::IfExpr, ref ifinfo) => {
            let (ifx, truth, lies) = list::to_ref_tuple3(ifinfo);
            let ifix = compile_expr(scope, proto, ifx);
            let itruth = compile_expr(scope, proto, truth);
            let ilies = compile_expr(scope, proto, lies);
            Iexpr::new_if(ifix, itruth, ilies)
        }
        &Val::Sexpr(SexprType::Let, ref letx) => {
            let (lhs_patt, rhs_val) = list::to_ref_tuple2(letx);
            let ilhs = compile_pattern(scope, proto, lhs_patt);
            let irhs = compile_expr(scope, proto, rhs_val);
            Iexpr::new(Source::Let(Box::new(ilhs), Box::new(irhs)))
        }
        &Val::Id(ref id) => {
            if !scope.contains_var(id) {
                panic!("undeclared variable: {}", id);
            }
            Iexpr::valx(Val::Id(id.clone()))
        }
        &Val::Bool(b) => {
            Iexpr::const_val(Val::Bool(b))
        }
        &Val::Int(i) => {
            Iexpr::const_val(Val::Int(i))
        }
        _ => {
            panic!("Cannot compile expr: {:?}", x);
        }
    }
}

pub fn compile_pattern(scope: &mut Interscope, proto: &Protomod, p: &Val
        ) -> Iexpr
{
    match p {
        &Val::Id(ref id) => {
            scope.add_var(&id);
            Iexpr::new(Source::Pattern(p.clone()))
        }
        _ => {
            panic!("Unsupported pattern: {:?}", p);
        }
    }
}

pub fn split_func_args_body(defunc: &Val) -> (Vec<String>, &Val)
{
    let (st, sx) = sexpr::split_ref(defunc);
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
        write!(f, "\tinterfunc: {:?}\n", self.interfunc).ok();
        write!(f, "}}\n")
    /*
    imports: HashSet<String>,
    macros: HashMap<String, Val>,
    srcfunc: HashMap<String, Val>,
    interfunc: HashMap<String, Iexpr>,
    */
    }
}
