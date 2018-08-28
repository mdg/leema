use leema::ast::{self, Ast, Kxpr};
use leema::infer::{Inferator, TypeSet};
use leema::ixpr::{Ixpr, Source};
use leema::list;
use leema::log;
use leema::lri::Lri;
use leema::lstr::Lstr;
use leema::phase0::Protomod;
use leema::struple::Struple;
use leema::typecheck::{self, Typemod};
use leema::val::{SrcLoc, Type, TypeErr, Val};

use std::collections::{HashMap, LinkedList};
use std::fmt;
use std::io::Write;
use std::rc::Rc;


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
    pub modname: Lstr,
    pub interfunc: HashMap<Lstr, Ixpr>,
}

impl Intermod
{
    pub fn new(modname: Lstr) -> Intermod
    {
        Intermod {
            modname: modname,
            interfunc: HashMap::new(),
        }
    }

    pub fn name(&self) -> &str
    {
        &self.modname.str()
    }

    pub fn compile(
        proto: &Protomod,
        imports: &HashMap<Lstr, Rc<Protomod>>,
        typed: &mut Typemod,
    ) -> Intermod
    {
        let mod_lstr = &proto.key.name;
        let mut inter = Intermod::new(mod_lstr.clone());
        for fname in proto.funcseq.iter() {
            let opt_defunc = proto.funcsrc.get(fname);
            if opt_defunc.is_none() {
                panic!(
                    "No function source found for {}::{}",
                    proto.key.name, fname
                );
            }
            let defunc = opt_defunc.unwrap();
            let (args, body, loc) = split_func_args_body(defunc);
            let ftype = proto.valtypes.get(fname).unwrap();
            let ifunc = compile_function(
                proto,
                imports,
                &typed,
                fname.str(),
                ftype,
                &args,
                body,
                loc,
            );
            typed.set_type(
                fname.clone(),
                typecheck::Depth::Inter,
                ifunc.typ.clone(),
            );
            inter.interfunc.insert(fname.clone(), ifunc);
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
    imports: &'a HashMap<Lstr, Rc<Protomod>>,
    typed: &'a Typemod,
    // types of locally defined labels
    infer: Inferator<'a>,
    typeset: TypeSet<'a>,
    argnames: LinkedList<Kxpr>,
    argt: Type,
}

impl<'a> Interscope<'a>
{
    pub fn new(
        proto: &'a Protomod,
        imports: &'a HashMap<Lstr, Rc<Protomod>>,
        typed: &'a Typemod,
        fname: &'a str,
        lineno: i16,
        args: &LinkedList<Kxpr>,
    ) -> Interscope<'a>
    {
        let mut t = Inferator::new(fname);
        let mut ts = TypeSet::new();
        let mut argt = Vec::new();
        for (i, a) in args.iter().enumerate() {
            vout!("bind func param as: #{} {:?}\n", i, a);
            let at = Type::from(a.x_ref().unwrap());
            let at2 = t
                .init_param(i as i16, a.k_ref(), at, lineno)
                .map_err(|_| {
                    TypeErr::Error(Lstr::from(format!(
                        "args type mismatch: {:?}",
                        args
                    )))
                }).unwrap();
            let opt_k = a.k_ref().map(|k| k.clone());
            argt.push((opt_k, at2));
        }

        ts.import_user_types(&proto.key.name, &proto.struple_fields);
        for (_, imp) in imports.iter() {
            ts.import_user_types(&imp.key.name, &imp.struple_fields);
        }

        Interscope {
            fname,
            proto,
            imports,
            typed,
            infer: t,
            typeset: ts,
            argnames: args.clone(),
            argt: Type::Tuple(Struple(argt)),
        }
    }

    pub fn vartype(&self, name: &str) -> Option<(ScopeLevel, Type)>
    {
        let local = self.infer.vartype(name);
        if local.is_some() && self.infer.var_is_in_scope(name) {
            return Some((ScopeLevel::Local, local.unwrap()));
        }
        let ftype = self.typed.function_type(&Lstr::from(String::from(name)));
        if ftype.is_some() {
            return Some((ScopeLevel::Module, ftype.unwrap().clone()));
        }
        if let Some(modtyp) = self.proto.valtype(name) {
            return Some((ScopeLevel::Module, modtyp.clone()));
        }
        match self.imports.get("prefab") {
            Some(ref proto) => {
                let valtype_opt = proto.valtype(name);
                if valtype_opt.is_none() {
                    vout!(
                        "cannot find variable: {} in\n{:?}\n{:?}\n",
                        name,
                        self.infer,
                        self.proto
                    );
                    panic!("undefined variable: {} in {:?}", name, self.infer);
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
            Some(ref proto) => proto.valtype(valnm),
        }
    }

    pub fn contains_id(&self, name: &str) -> bool
    {
        if self.infer.var_is_in_scope(name) {
            true
        } else if self.proto.contains_val(name) {
            true
        } else {
            match self.imports.get("prefab") {
                Some(ref proto) => proto.contains_val(name),
                None => false,
            }
        }
    }

    pub fn imports_module(&self, name: &str) -> bool
    {
        self.imports.contains_key(name)
    }

    pub fn struple_field_idx(
        &self,
        typ: &Type,
        fld: &str,
    ) -> Option<(i16, &Type)>
    {
        let proto = self.type_module(typ);
        if let &Type::UserDef(ref typi) = typ {
            proto.struple_field_idx(&typi.localid, fld)
        } else {
            panic!("cannot get field index for not user def");
        }
    }

    pub fn type_module(&self, typ: &Type) -> &Protomod
    {
        match typ {
            &Type::UserDef(ref i) => {
                i.mod_ref()
                    .and_then(|mods| {
                        if *mods == self.proto.key.name {
                            return None;
                        }
                        let imp = self.imports.get(mods.str());
                        if imp.is_none() {
                            panic!("module for type cannot be found: {}", typ);
                        }
                        Some(&**imp.unwrap())
                    }).unwrap_or(self.proto)
            }
            _ => self.proto,
        }
    }
}


pub fn compile_function<'a>(
    proto: &'a Protomod,
    imports: &'a HashMap<Lstr, Rc<Protomod>>,
    typed: &Typemod,
    fname: &'a str,
    ftype: &Type,
    args: &LinkedList<Kxpr>,
    body: &Ast,
    loc: &SrcLoc,
) -> Ixpr
{
    vout!("compile {}({:?}): {:?}\n", fname, args, ftype);
    if *body == Ast::RustBlock {
        return Ixpr {
            typ: ftype.clone(),
            src: Source::RustBlock,
            line: loc.lineno,
        };
    }
    let (argt, _result) = Type::split_func_ref(ftype);
    let mut scope =
        Interscope::new(proto, imports, typed, fname, loc.lineno, args);
    let ibody = compile_expr(&mut scope, body, loc);
    let argt2: Vec<Type> = argt
        .iter()
        .map(|a| scope.infer.inferred_type(a).clone())
        .collect();
    let ibody2 = Ixpr {
        typ: scope.infer.inferred_type(&ibody.typ).clone(),
        src: ibody.src,
        line: loc.lineno,
    };
    let final_ftype = Type::Func(argt2, Box::new(ibody2.typ.clone()));
    vout!("compile function {}: {}\n", fname, final_ftype);
    for v in scope.infer.vars() {
        let vtyp = scope.infer.vartype(v);
        vout!("\t{}: {}\n", v, vtyp.unwrap());
    }
    vout!("\t<result>: {}\n", ibody2.typ);
    vout!("inferences: {:?}\n", scope.infer);
    let rc_args = args
        .iter()
        .enumerate()
        .map(|(argi, a)| {
            a.k_clone()
                .unwrap_or_else(|| Lstr::from(format!("T_param_{}", argi)))
        }).collect();
    Ixpr {
        typ: final_ftype,
        src: Source::Func(rc_args, Box::new(ibody2)),
        line: loc.lineno,
    }
}

pub fn compile_expr(scope: &mut Interscope, x: &Ast, loc: &SrcLoc) -> Ixpr
{
    match x {
        &Ast::Block(ref lines) => compile_block(scope, lines, loc),
        &Ast::Localid(ref id, ref loc) => compile_local_id(scope, id, loc),
        &Ast::Lri(ref names, None, ref loc) => compile_lri(scope, names, loc),
        &Ast::Lri(_, ref _types, _) => {
            panic!("cannot handle typed lri");
        }
        &Ast::DotAccess(ref outer, ref inner) => {
            compile_dot_access(scope, outer, inner, loc)
        }
        &Ast::Call(ref callx, ref args, ref iloc) => {
            compile_call(scope, callx, args, iloc)
        }
        &Ast::ConstBool(b) => Ixpr::const_val(Val::Bool(b), loc.lineno),
        &Ast::ConstInt(i) => Ixpr::const_val(Val::Int(i), loc.lineno),
        &Ast::ConstStr(ref s) => {
            Ixpr::const_val(Val::Str(s.clone()), loc.lineno)
        }
        &Ast::ConstHashtag(ref s) => {
            Ixpr::const_val(Val::Hashtag(s.clone()), loc.lineno)
        }
        &Ast::ConstVoid => Ixpr::const_val(Val::Void, loc.lineno),
        &Ast::Cons(ref head, ref tail) => {
            let chead = compile_expr(scope, head, loc);
            let ctail = compile_expr(scope, tail, loc);
            let list_type = scope
                .infer
                .merge_types(
                    &Type::StrictList(Box::new(chead.typ.clone())),
                    &ctail.typ,
                ).map_err(|e| {
                    e.add_context(Lstr::Sref("const types do not match"))
                });
            Ixpr::cons(chead, ctail, list_type.unwrap(), loc.lineno)
        }
        &Ast::IfExpr(_, _, _, _) => compile_ifx(scope, x),
        &Ast::StrExpr(ref items, ref iloc) => {
            let strvec =
                items.iter().map(|i| compile_expr(scope, i, iloc)).collect();
            Ixpr::new_str_mash(strvec, iloc.lineno)
        }
        &Ast::List(ref items) => {
            let c_items =
                items.iter().map(|i| compile_expr(scope, i, loc)).collect();
            Ixpr::new_list(c_items, loc.lineno)
        }
        &Ast::Tuple(ref items) => {
            let c_items = items
                .iter()
                .map(|i| compile_expr(scope, i.x_ref().unwrap(), loc))
                .collect();
            Ixpr::new_tuple(c_items, loc.lineno)
        }
        &Ast::ConstructData(ast::DataType::Struple, ref ast_typ) => {
            let type_lri = Lri::from(&**ast_typ);
            let opt_full_type = scope.proto.func_result_type(&type_lri.localid);
            if opt_full_type.is_none() {
                panic!(
                    "cannot find full type for: {:?} in {:?}",
                    type_lri.localid, scope.proto.valtypes
                );
            }
            let full_type = opt_full_type.unwrap();
            let flds = scope.typeset.get_typedef(&type_lri).unwrap();
            Ixpr::construple(full_type.clone(), flds, loc.lineno)
        }
        &Ast::Let(ltype, ref lhs, ref rhs, ref iloc) => {
            compile_let_stmt(scope, ltype, lhs, rhs, iloc)
        }
        _ => {
            panic!("Cannot compile expr: {:?}", x);
        }
    }
}

pub fn compile_lri(
    scope: &mut Interscope,
    names: &Vec<Lstr>,
    loc: &SrcLoc,
) -> Ixpr
{
    if names.len() != 2 {
        panic!("too many modules: {:?}", names);
    }
    let modname = names.first().unwrap();
    if !scope.imports_module(modname) {
        panic!("module not found: {:?}", names);
    }
    let id = names.last().unwrap();
    let opt_vartype = scope.import_vartype(modname, id);
    if opt_vartype.is_none() {
        panic!("failure for import_vartype({}, {})", modname, id);
    }
    let vartype = opt_vartype.unwrap();
    let lri = Lri::with_modules(modname.clone(), id.clone());

    let fref = Val::FuncRef(lri, vartype.clone());
    Ixpr::const_val(fref, loc.lineno)
}

pub fn compile_local_id(scope: &mut Interscope, id: &Lstr, loc: &SrcLoc)
    -> Ixpr
{
    vout!("compile_local_id({})\n", id);
    match scope.vartype(id.str()) {
        Some((ScopeLevel::Local, typ)) => {
            scope.infer.mark_usage(id.str(), loc);
            Ixpr {
                src: Source::Id(id.clone(), loc.lineno),
                typ: typ.clone(),
                line: loc.lineno,
            }
        }
        Some((ScopeLevel::Module, typ)) => {
            if typ.is_func() {
                let fref = Val::FuncRef(
                    Lri::with_modules(scope.proto.key.name.clone(), id.clone()),
                    typ.clone(),
                );
                Ixpr {
                    src: Source::ConstVal(fref),
                    typ: typ.clone(),
                    line: loc.lineno,
                }
            } else {
                let c = scope.proto.constants.get(id.str()).unwrap();
                Ixpr {
                    src: Source::ConstVal(c.clone()),
                    typ: typ.clone(),
                    line: loc.lineno,
                }
            }
        }
        Some((ScopeLevel::External, typ)) => {
            // if it's external and no module prefix,
            // it's almost certainly prefab. probably
            // a better way to make this work
            let fref = Val::FuncRef(
                Lri::with_modules(Lstr::from("prefab"), id.clone()),
                typ.clone(),
            );
            Ixpr {
                src: Source::ConstVal(fref),
                typ: typ.clone(),
                line: loc.lineno,
            }
        }
        None => {
            panic!("untyped variable: {} not in {:?}", id, scope);
        }
    }
}

pub fn compile_call(
    scope: &mut Interscope,
    callx: &Ast,
    args: &LinkedList<Kxpr>,
    loc: &SrcLoc,
) -> Ixpr
{
    let mut icall = compile_expr(scope, callx, loc);
    let iargs: Vec<Ixpr> = args
        .iter()
        .map(|i| compile_expr(scope, i.x_ref().unwrap(), loc))
        .collect();
    let ftype = {
        let iargst: Vec<&Type> = iargs.iter().map(|ia| &ia.typ).collect();
        scope
            .infer
            .make_call_type(&icall.typ, &iargst)
            .map_err(|e| {
                e.add_context(Lstr::from(format!(
                    "type error in function call: {:?}",
                    callx
                )))
            }).unwrap()
    };
    icall.typ = ftype.clone();
    let (_, ftype_result) = Type::split_func(ftype);
    let argsix = Ixpr::new_tuple(iargs, loc.lineno);
    Ixpr {
        typ: ftype_result.clone(),
        src: Source::Call(Box::new(icall), Box::new(argsix)),
        line: loc.lineno,
    }
}

pub fn compile_let_stmt(
    scope: &mut Interscope,
    _lettype: ast::LetType,
    lhs: &Ast,
    rhs: &Ast,
    loc: &SrcLoc,
) -> Ixpr
{
    vout!("compile let {:?} := {:?}\n", lhs, rhs);
    let irhs = compile_expr(scope, rhs, loc);
    let mut new_vars = Vec::new();
    let cpatt = compile_pattern(scope, &mut new_vars, lhs);
    vout!("new vars in let: {:?} = {:?} = {:?}\n", new_vars, lhs, irhs);
    scope
        .infer
        .match_pattern(&scope.typeset, &cpatt, &irhs.typ, loc.lineno)
        .unwrap();
    let failed: Vec<(Lstr, Ixpr)> = new_vars
        .iter()
        .map(|v| (v.clone(), compile_failed_var(scope, v, loc)))
        .collect();
    Ixpr::new(Source::Let(cpatt, Box::new(irhs), failed), loc.lineno)
}

pub fn compile_dot_access(
    scope: &mut Interscope,
    base_val: &Ast,
    field: &Lstr,
    loc: &SrcLoc,
) -> Ixpr
{
    let ix_base = compile_expr(scope, base_val, loc);
    if let Some((field_idx, field_typ)) =
        scope.struple_field_idx(&ix_base.typ, field.str())
    {
        Ixpr::new_field_access(ix_base, field_idx as i8, field_typ.clone())
    } else {
        panic!("no field: {:?}.{}", base_val, field);
    }
}

pub fn compile_ifx(scope: &mut Interscope, ifx: &Ast) -> Ixpr
{
    scope.infer.push_block(HashMap::new());
    let ix = match ifx {
        &Ast::IfExpr(ast::IfType::If, ref const_void, ref case, ref iloc) => {
            if **const_void != Ast::ConstVoid {
                panic!("if input is not void? {:?} @ {}", const_void, iloc);
            }
            compile_if_case(scope, case)
        }
        &Ast::IfExpr(ast::IfType::Match, ref x, ref ifcase, ref iloc) => {
            let ix = compile_expr(scope, x, iloc);
            let ixcase = compile_match_case(scope, ifcase, &ix.typ);
            Ixpr::new_match_expr(ix, ixcase)
        }
        &Ast::IfExpr(ast::IfType::MatchFailure, _, _, _) => {
            panic!("what's a MatchFailed doing here?");
        }
        _ => {
            panic!("not an expected if expression: {:?}", ifx);
        }
    };
    scope.infer.pop_block();
    ix
}

pub fn compile_if_case(scope: &mut Interscope, case: &ast::IfCase) -> Ixpr
{
    let ix = if case.cond == Ast::Wildcard {
        if case.else_case.is_some() {
            panic!("cannot have else case for else case: {:?}", case);
        }
        Ixpr::const_val(Val::Bool(true), case.loc.lineno)
    } else {
        compile_expr(scope, &case.cond, &case.loc)
    };
    scope.infer.push_block(HashMap::new());
    let ibody = compile_expr(scope, &case.body, &case.loc);
    scope.infer.pop_block();
    let (if_result_type, inext) = match case.else_case.as_ref() {
        Some(else_case) => {
            let iinext = compile_if_case(scope, &*else_case);
            let mtype = scope
                .infer
                .merge_types(&ibody.typ, &iinext.typ)
                .map_err(|e| {
                    e.add_context(Lstr::Sref("if/else types do not match"))
                });
            (mtype.unwrap(), Some(iinext))
        }
        None => (ibody.typ.clone(), None),
    };
    Ixpr::new_if(ix, ibody, inext, if_result_type)
}

pub fn compile_match_case(
    scope: &mut Interscope,
    case: &ast::IfCase,
    xtyp: &Type,
) -> Ixpr
{
    let mut new_vars = Vec::new();
    scope.infer.push_block(HashMap::new());
    let cpatt = compile_pattern(scope, &mut new_vars, &case.cond);
    scope
        .infer
        .match_pattern(&scope.typeset, &cpatt, xtyp, case.loc.lineno)
        .unwrap();
    let iblk = compile_expr(scope, &case.body, &case.loc);
    scope.infer.pop_block();
    let inext = case.else_case.as_ref().map_or(Ixpr::noop(), |else_case| {
        compile_match_case(scope, &else_case, xtyp)
    });
    Ixpr::new_match_case(cpatt, iblk, inext)
}

pub fn compile_pattern(
    scope: &mut Interscope,
    new_vars: &mut Vec<Lstr>,
    patt: &Ast,
) -> Val
{
    match patt {
        &Ast::Localid(ref name, _) => {
            if !scope.infer.var_is_in_scope(name.str()) {
                new_vars.push(name.clone());
            }
            Val::Id(name.clone())
        }
        &Ast::Cons(ref head, ref tail) => {
            let chead = compile_pattern(scope, new_vars, head);
            let ctail = match &**tail {
                &Ast::Localid(_, _) => {
                    compile_pattern(scope, new_vars, &**tail)
                }
                &Ast::Wildcard => Val::Wildcard,
                &Ast::Cons(_, _) => compile_pattern(scope, new_vars, &**tail),
                &Ast::List(_) => compile_pattern(scope, new_vars, &**tail),
                _ => {
                    panic!("invalid pattern tail: {:?}", tail);
                }
            };
            Val::Cons(Box::new(chead), Rc::new(ctail))
        }
        &Ast::List(ref items) => {
            let c_items: Vec<Val> = items
                .iter()
                .map(|i| compile_pattern(scope, new_vars, i))
                .collect();
            list::from_vec(&c_items)
        }
        &Ast::Tuple(ref items) => {
            let citems = items
                .iter()
                .map(|i| compile_pattern(scope, new_vars, i.x_ref().unwrap()))
                .collect();
            Val::Tuple(Struple::new_indexed(citems))
        }
        &Ast::ConstInt(i) => Val::Int(i),
        &Ast::ConstBool(b) => Val::Bool(b),
        &Ast::ConstStr(ref s) => Val::Str(s.clone()),
        &Ast::ConstHashtag(ref h) => Val::Hashtag(h.clone()),
        &Ast::Wildcard => Val::Wildcard,
        &Ast::Call(ref callx, ref args, ref iloc) => {
            compile_pattern_call(scope, new_vars, callx, args, iloc)
        }
        _ => {
            panic!("invalid pattern: {:?}", patt);
        }
    }
}

pub fn compile_pattern_call(
    scope: &mut Interscope,
    new_vars: &mut Vec<Lstr>,
    callx: &Ast,
    args: &LinkedList<Kxpr>,
    _loc: &SrcLoc,
) -> Val
{
    let args_vec: Vec<(Option<Lstr>, Val)> = args
        .iter()
        .map(|a| {
            (
                a.k_clone(),
                compile_pattern(scope, new_vars, a.x_ref().unwrap()),
            )
        }).collect();

    let mut struct_lri = Lri::from(callx);
    if !struct_lri.has_modules() {
        struct_lri = struct_lri.add_modules(scope.proto.key.name.clone());
    }
    Val::Struct(struct_lri, Struple(args_vec))
}

pub fn push_block(scope: &mut Interscope, stmts: &Vec<Ast>) -> Vec<Ast>
{
    let mut keyed_failures: HashMap<Lstr, Ast> = HashMap::new();
    let mut lines: Vec<Ast> = Vec::with_capacity(stmts.len());
    for s in stmts.iter() {
        if let &Ast::IfExpr(
            ast::IfType::MatchFailure,
            ref name,
            ref _case,
            _iloc,
        ) = s
        {
            let name_lstr = Lstr::from(&**name);
            keyed_failures.insert(name_lstr, s.clone());
        } else {
            lines.push(s.clone());
        }
    }

    scope.infer.push_block(keyed_failures);
    lines
}

pub fn compile_block(
    scope: &mut Interscope,
    blk: &Vec<Ast>,
    loc: &SrcLoc,
) -> Ixpr
{
    let stmts = push_block(scope, blk);
    let block_len = stmts.len();
    let mut result: Vec<Ixpr> = Vec::with_capacity(block_len);
    let mut fails = HashMap::new();
    for line in stmts.iter() {
        compile_block_stmt(&mut result, &mut fails, scope, line, loc);
    }
    scope.infer.pop_block();
    Ixpr::new_block(result, fails, loc.lineno)
}

pub fn compile_block_stmt(
    istmts: &mut Vec<Ixpr>,
    _ifails: &mut HashMap<String, Ixpr>,
    scope: &mut Interscope,
    stmt: &Ast,
    loc: &SrcLoc,
)
{
    match stmt {
        &Ast::IfExpr(ast::IfType::MatchFailure, ref input, _, _) => {
            panic!("MatchFailed should be removed from blocks: {:?}", input);
        }
        &Ast::Return(ref result, ref iloc) => {
            let cresult = compile_expr(scope, result, iloc);
            let ret = Ixpr::new(Source::Return(Box::new(cresult)), iloc.lineno);
            istmts.push(ret);
        }
        _ => {
            istmts.push(compile_expr(scope, stmt, loc));
        }
    }
}

pub fn compile_failed_var(
    scope: &mut Interscope,
    v: &Lstr,
    loc: &SrcLoc,
) -> Ixpr
{
    if scope.infer.handles_failure(&**v) {
        vout!("compile failure handling for {}\n", v);
        scope.infer.push_block(HashMap::new());
        let ixfailure = {
            let failure = scope.infer.get_failure(&**v).unwrap().clone();
            compile_ifx(scope, &failure)
        };
        scope.infer.pop_block();
        ixfailure
    } else {
        Ixpr::new(Source::PropagateFailure(v.clone(), loc.lineno), loc.lineno)
    }
}

pub fn split_func_args_body(defunc: &Ast)
    -> (&LinkedList<Kxpr>, &Ast, &SrcLoc)
{
    if let &Ast::DefFunc(
        ast::FuncClass::Func,
        ref name,
        ref args,
        ref _result,
        ref body,
        ref loc,
    ) = defunc
    {
        vout!("split_func_args({:?})\n", name);
        (args, body, loc)
    } else {
        panic!("func is not a func: {:?}", defunc);
    }
}

impl fmt::Debug for Intermod
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "Intermod{{\n").ok();
        write!(f, "\tname: {}\n", self.modname).ok();
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
mod tests
{
    use leema::ast::Ast;
    use leema::inter::{self, Interscope, ScopeLevel};
    use leema::loader::Interloader;
    use leema::lstr::Lstr;
    use leema::module::ModKey;
    use leema::phase0::Protomod;
    use leema::program;
    use leema::typecheck::Typemod;
    use leema::val::{SrcLoc, Type};

    use std::collections::{HashMap, LinkedList};


    #[test]
    fn test_scope_add_vartype()
    {
        let mk = ModKey::name_only(Lstr::Sref("tacos"));
        let typed = Typemod::new(mk.name.clone());
        let proto = Protomod::new(mk);
        let imps = HashMap::new();
        let args = LinkedList::new();
        let mut scope =
            Interscope::new(&proto, &imps, &typed, "foo", 105, &args);
        scope
            .infer
            .bind_vartype(&Lstr::Sref("hello"), &Type::Int, 17)
            .unwrap();

        let (scope_lvl, typ) = scope.vartype("hello").unwrap();
        assert_eq!(ScopeLevel::Local, scope_lvl);
        assert_eq!(Type::Int, typ);
    }

    #[test]
    fn test_scope_push_block()
    {
        let mk = ModKey::name_only(Lstr::Sref("tacos"));
        let typed = Typemod::new(mk.name.clone());
        let proto = Protomod::new(mk);
        let imps = HashMap::new();
        let args = LinkedList::new();
        let mut scope =
            Interscope::new(&proto, &imps, &typed, "foo", 104, &args);
        scope
            .infer
            .bind_vartype(&Lstr::Sref("hello"), &Type::Int, 18)
            .unwrap();
        println!("add_var(hello) -> {:?}", scope);

        {
            let (hello_lvl, hello_typ) = scope.vartype("hello").unwrap();
            assert_eq!(ScopeLevel::Local, hello_lvl);
            assert_eq!(Type::Int, hello_typ);
        }

        scope.infer.push_block(HashMap::new());
        scope
            .infer
            .bind_vartype(&Lstr::Sref("world"), &Type::Str, 33)
            .unwrap();
        println!("push_block().add_var(world) -> {:?}", scope);

        {
            let (world_lvl, world_typ) = scope.vartype("world").unwrap();
            assert_eq!(ScopeLevel::Local, world_lvl);
            assert_eq!(Type::Str, world_typ);

            let (hello_lvl, hello_typ) = scope.vartype("hello").unwrap();
            assert_eq!(ScopeLevel::Local, hello_lvl);
            assert_eq!(Type::Int, hello_typ);
        }

        scope.infer.pop_block();

        assert_eq!(None, scope.vartype("world"));
        assert!(scope.vartype("hello").is_some());
    }

    #[test]
    fn test_new_vars_from_id_pattern()
    {
        let mk = ModKey::name_only(Lstr::Sref("tacos"));
        let proto = Protomod::new(mk.clone());
        let typed = Typemod::new(mk.name.clone());
        let imps = HashMap::new();
        let args = LinkedList::new();
        let mut scope =
            Interscope::new(&proto, &imps, &typed, "foo", 103, &args);

        let mut new_vars = Vec::default();
        let patt = Ast::Localid(Lstr::from("x"), SrcLoc::default());

        inter::compile_pattern(&mut scope, &mut new_vars, &patt);

        assert_eq!(1, new_vars.len());
        assert_eq!("x", &**(new_vars.first().unwrap()));
    }

    #[test]
    fn test_compile_matched_if_branches()
    {
        let input = String::from(
            "
    func factf(i): Int ->
        if
        |i == 1 -> 1
        |else -> i * factf(i-1)
        --
    --
    ",
        );

        let mut loader = Interloader::new(Lstr::Sref("fact.lma"));
        loader.set_mod_txt(Lstr::Sref("fact"), input);
        let mut prog = program::Lib::new(loader);
        prog.read_inter(&Lstr::Sref("fact"));
        // assert that it didn't panic
        assert!(true);
    }

    #[test]
    #[should_panic]
    fn test_too_many_args()
    {
        let input = String::from(
            "

    func sum(a, b) -> a + b --
    func main() -> sum(3, 4, 5) --
    ",
        );

        let mut loader = Interloader::new(Lstr::Sref("tacos.lma"));
        loader.set_mod_txt(Lstr::Sref("tacos"), input);
        let mut prog = program::Lib::new(loader);
        prog.read_inter(&Lstr::Sref("tacos"));
    }

    #[test]
    fn test_pattern_declaration()
    {
        let input = String::from(
            "

    func foo(inputs: [#])
    |[] -> #empty
    |#whatever;more -> #whatever
    |_;more -> foo(more)
    --

    func main() ->
        foo([#a, #b, #c])
    --
    ",
        );

        let mut loader = Interloader::new(Lstr::Sref("tacos.lma"));
        loader.set_mod_txt(Lstr::Sref("tacos"), input);
        let mut prog = program::Lib::new(loader);
        prog.read_inter(&Lstr::Sref("tacos"));
        assert!(true); // didn't panic earlier
    }

    #[test]
    fn test_named_tuple_constructor()
    {
        let input = String::from(
            "
            struct Greeting(Str, Str)

            func main() ->
                let g := Greeting(\"hello\", \"world\")
            --
            ",
        );

        let greeting_str = Lstr::Sref("greeting");
        let mut loader = Interloader::new(Lstr::Sref("greeting.lma"));
        loader.set_mod_txt(greeting_str.clone(), input);
        let mut prog = program::Lib::new(loader);
        prog.read_inter(&greeting_str);
        assert!(true); // didn't panic earlier
    }

    #[test]
    #[ignore] // unignore this once enums are working again
    fn test_enum_constructors()
    {
        let input = "
            enum Animal
            |Dog
            |Cat(Int)
            |Mouse
                .whiskers: Int
                .color: Str
            --

            func main() ->
                let d := Dog
                let c := Cat(3)
                let m := Mouse(9, \"red\")
            --
            ".to_string();

        let mut loader = Interloader::new(Lstr::Sref("animals.lma"));
        loader.set_mod_txt(Lstr::Sref("animals"), input);
        let mut prog = program::Lib::new(loader);
        prog.read_inter(&Lstr::Sref("animals"));
        assert!(true); // didn't panic earlier
    }

    #[test]
    #[should_panic]
    fn test_pattern_type_explicit_mismatch()
    {
        let input = String::from(
            "
            func foo(inputs: [#])
            |([]) -> #empty
            |([#whatever;more]) -> #whatever
            |([_;more]) -> foo(more)
            --

            func main() ->
                foo([5, 3, 4])
            --
            ",
        );

        let mut loader = Interloader::new(Lstr::Sref("tacos.lma"));
        loader.set_mod_txt(Lstr::Sref("tacos"), input);
        let mut prog = program::Lib::new(loader);
        prog.read_inter(&Lstr::Sref("tacos"));
    }

    #[test]
    #[should_panic]
    fn test_pattern_type_mismatch_not_inferred()
    {
        let input = String::from(
            "
            ## foo should return a #
            func foo(inputs)
            |([]) -> #empty
            |(#whatever;more) -> #whatever
            |(_;more) -> foo(more)
            --

            func main() ->
                foo([5, 3, 4])
            --
            ",
        );

        let mut loader = Interloader::new(Lstr::Sref("tacos.lma"));
        loader.set_mod_txt(Lstr::Sref("tacos"), input);
        let mut prog = program::Lib::new(loader);
        prog.read_inter(&Lstr::Sref("tacos"));
    }

}
