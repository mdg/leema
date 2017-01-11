
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

#[derive(Debug)]
pub struct Interscope<'a>
{
    fname: &'a str,
    proto: &'a Protomod,
    imports: &'a HashMap<String, Rc<Protomod>>,
    blk: Blockscope,
    // types of locally defined labels
    T: HashMap<String, Type>,
}

impl<'a> Interscope<'a>
{
    pub fn new(proto: &'a Protomod, imports: &'a HashMap<String, Rc<Protomod>>
            , fname: &'a str, args: &Vec<String>, argt: &Vec<Type>
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
        if !self.T.contains_key(name) {
            self.T.insert(String::from(name), typ.clone());
        } else {
            let scope_type = self.T.get(name).unwrap();
            if scope_type != typ {
                panic!("variable {} already declared type: {:?}"
                        , name, scope_type);
            }
        }
        self.blk.E.insert(String::from(name));
    }

    pub fn vartype(&self, name: &str) -> Option<&Type>
    {
        let local = self.T.get(name);
        if local.is_some() {
            return local;
        }
        let modtyp = self.proto.valtype(name);
        if modtyp.is_some() {
            return modtyp;
        }
        match self.imports.get("prefab") {
            Some(ref proto) => {
                proto.valtype(name)
            }
            None => None,
        }
    }

    pub fn contains_var(&self, name: &str) -> bool
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
}


pub fn compile_function<'a>(proto: &'a Protomod
        , imports: &'a HashMap<String, Rc<Protomod>>, fname: &'a str
        , ftype: &Type, args: &Vec<String>, body: &Val) -> Iexpr
{
    if *body == Val::RustBlock {
        return Iexpr::valx(Val::RustBlock);
    }
    let (argt, result) = Type::split_func(ftype);
    let mut scope = Interscope::new(proto, imports, fname, args, argt);
    compile_expr(&mut scope, body)
}

pub fn compile_expr(scope: &mut Interscope, x: &Val) -> Iexpr
{
    match x {
        &Val::Sexpr(SexprType::BlockExpr, ref blk) => {
            scope.push_block();
            let mut iblk = vec![];
            let mut blk_head: &Val = &**blk;
            while *blk_head != Val::Nil {
                let (bx, tail) = list::take_ref(blk_head);
                iblk.push(compile_expr(scope, bx));
                blk_head = tail;
            }
            scope.pop_block();
            Iexpr::new_block(iblk)
        }
        &Val::Sexpr(SexprType::Call, ref callinfo) => {
            let (callx, args) = list::to_ref_tuple2(callinfo);
            let icall = compile_expr(scope, callx);
            let iargs = compile_expr(scope, args);
            Iexpr::new_call(icall, iargs)
        }
        &Val::Sexpr(SexprType::IfExpr, ref ifinfo) => {
            let (ifx, truth, lies) = list::to_ref_tuple3(ifinfo);
            let ifix = compile_expr(scope, ifx);
            let itruth = compile_expr(scope, truth);
            let ilies = compile_expr(scope, lies);
            Iexpr::new_if(ifix, itruth, ilies)
        }
        &Val::Sexpr(SexprType::Let, ref letx) => {
            let (lhs_patt, rhs_val) = list::to_ref_tuple2(letx);
            let irhs = compile_expr(scope, rhs_val);
            let ilhs = compile_pattern(scope, lhs_patt, &irhs.typ);
            Iexpr::new(Source::Let(Box::new(ilhs), Box::new(irhs)))
        }
        &Val::Sexpr(SexprType::StrExpr, ref strlist) => {
            let mut strvec = vec![];
            list::fold_mut_ref(&mut (&mut strvec, scope), strlist,
                |&mut (ref mut sv, ref mut scp), x| {
                    sv.push(compile_expr(*scp, x));
                }
            );
            Iexpr::new_str_mash(strvec)
        }
        &Val::Id(ref id) => {
            match scope.vartype(id) {
                Some(typ) => {
                    Iexpr{
                        src: Source::ValExpr(Val::Id(id.clone())),
                        typ: typ.clone(),
                    }
                }
                None => {
                    panic!("undeclared variable: {}", id);
                }
            }
        }
        &Val::Bool(b) => {
            Iexpr::const_val(Val::Bool(b))
        }
        &Val::Int(i) => {
            Iexpr::const_val(Val::Int(i))
        }
        &Val::Str(ref s) => {
            Iexpr::const_val(Val::Str(s.clone()))
        }
        _ => {
            panic!("Cannot compile expr: {:?}", x);
        }
    }
}

pub fn compile_pattern(scope: &mut Interscope, p: &Val, srctyp: &Type) -> Iexpr
{
    match p {
        &Val::Id(ref id) => {
            scope.add_var(&id, srctyp);
            Iexpr{
                src: Source::Pattern(p.clone()),
                typ: srctyp.clone(),
            }
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
