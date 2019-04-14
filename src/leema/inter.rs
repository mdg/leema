use crate::leema::ast::{self, Ast, IfCase, Kxpr};
use crate::leema::failure::{Failure, Lresult};
use crate::leema::ixpr::{Ixpr, MatchFailure, Source};
use crate::leema::list;
use crate::leema::lri::Lri;
use crate::leema::lstr::Lstr;
use crate::leema::phase0::Protomod;
use crate::leema::struple::{Struple, StrupleItem, StrupleKV};
use crate::leema::val::{FuncType, SrcLoc, Type, Val};

use std::collections::{HashMap, HashSet, LinkedList};
use std::fmt;
use std::iter::FromIterator;
use std::rc::Rc;
use std::sync::Arc;


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
    closed_vars: HashMap<Lstr, Vec<Lstr>>,
}

impl Intermod
{
    pub fn new(modname: Lstr) -> Intermod
    {
        Intermod {
            modname,
            interfunc: HashMap::new(),
            closed_vars: HashMap::new(),
        }
    }

    pub fn name(&self) -> &str
    {
        &self.modname.str()
    }

    pub fn get_closed_vars(&self, name: &str) -> Option<&Vec<Lstr>>
    {
        self.closed_vars.get(name)
    }

    pub fn compile(
        proto: &Protomod,
        imports: &HashMap<Lstr, Rc<Protomod>>,
    ) -> Intermod
    {
        let mod_lstr = &proto.key.name;
        let mut inter = Intermod::new(mod_lstr.clone());

        // compile regular functions next
        for fname in proto.funcseq.iter() {
            let ifunc = inter.compile_function(proto, imports, fname);
            inter.interfunc.insert(fname.clone(), ifunc.unwrap());
        }
        inter
    }

    pub fn compile_function<'a>(
        &mut self,
        proto: &'a Protomod,
        imports: &'a HashMap<Lstr, Rc<Protomod>>,
        fname: &'a Lstr,
    ) -> Lresult<Ixpr>
    {
        vout!("compile {}()\n", fname);
        let opt_defunc = proto.funcsrc.get(fname);
        if opt_defunc.is_none() {
            panic!(
                "No function source found for {}::{}",
                proto.key.name, fname
            );
        }
        let defunc = opt_defunc.unwrap();
        let (fclass, args, body, loc) = split_func_args_body(defunc);
        let is_closure = fclass == ast::FuncClass::Closure;
        let ftype = proto.valtypes.get(fname).unwrap();
        vout!("\t{}({:?}): {:?}\n", fname, args, ftype);

        let (argt, result_type) = Type::split_func_ref(ftype);
        let argt_vec = argt.iter_v().map(|x| x.clone()).collect();
        if *body == Ast::RustBlock {
            return Ok(Ixpr {
                src: Source::RustBlock(argt.clone(), result_type.clone()),
                line: loc.lineno,
            });
        }
        let (ibody, closed_vars) = {
            let mut scope = Interscope::new(
                proto,
                imports,
                &self.closed_vars,
                fname,
                args,
                is_closure,
            );
            let inner_body = compile_expr(&mut scope, body, loc)?;
            (inner_body, scope.take_closed())
        };
        if !closed_vars.is_empty() {
            self.closed_vars.insert(fname.clone(), closed_vars.clone());
        }
        let ibody2 = Ixpr {
            src: ibody.src,
            line: loc.lineno,
        };
        vout!("compile function {}({:?}): {}\n", fname, argt, result_type);
        let rc_args = args
            .iter()
            .enumerate()
            .map(|(argi, a)| {
                a.k_clone()
                    .unwrap_or_else(|| Lstr::from(format!("T_param_{}", argi)))
            })
            .collect();
        let src = Source::Func(
            rc_args,
            closed_vars,
            argt_vec,
            result_type.clone(),
            Box::new(ibody2),
        );
        Ok(Ixpr {
            src,
            line: loc.lineno,
        })
    }
}

#[derive(Debug)]
pub struct Blockscope
{
    failures: HashMap<Lstr, IfCase>,
    vars: HashSet<Lstr>,
}

impl Blockscope
{
    pub fn new() -> Blockscope
    {
        Blockscope {
            failures: HashMap::new(),
            vars: HashSet::new(),
        }
    }

    pub fn add_failure(&mut self, var: &Lstr, cases: IfCase)
    {
        self.failures.insert(var.clone(), cases);
    }
}

#[derive(Copy)]
#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub enum LocalType
{
    Param,
    Match,
    Let,
}

#[derive(Debug)]
pub struct LocalVar
{
    name: Lstr,
    var_type: LocalType,
    num_scopes: i16,
    num_reassignments: i16,
    first_assign: i16,
    first_access: Option<i16>,
    last_assign: i16,
    last_access: Option<i16>,
}

impl LocalVar
{
    pub fn new(name: Lstr, vt: LocalType) -> LocalVar
    {
        LocalVar {
            name,
            var_type: vt,
            num_scopes: 1,
            num_reassignments: 0,
            first_assign: 0,
            first_access: None,
            last_assign: 0,
            last_access: None,
        }
    }
}

#[derive(Debug)]
pub struct Blockstack
{
    stack: Vec<Blockscope>,
    locals: HashMap<Lstr, LocalVar>,
    in_failed: bool,
}

impl Blockstack
{
    pub fn new() -> Blockstack
    {
        Blockstack {
            stack: vec![Blockscope::new()],
            locals: HashMap::new(),
            in_failed: false,
        }
    }

    pub fn push_blockscope(&mut self)
    {
        self.stack.push(Blockscope::new());
    }

    pub fn pop_blockscope(&mut self)
    {
        self.stack.pop();
    }

    pub fn current_block_mut(&mut self) -> &mut Blockscope
    {
        self.stack.last_mut().unwrap()
    }

    pub fn assign_var(&mut self, id: &Lstr, vt: LocalType)
    {
        if self.var_in_scope(id) {
            let var_data = self.locals.get_mut(id).unwrap();
            match (var_data.var_type, vt) {
                (LocalType::Let, LocalType::Let) => {
                    var_data.num_reassignments += 1;
                }
                (LocalType::Param, LocalType::Let) => {
                    panic!("cannot reassign a function parameter: {}", id);
                }
                (LocalType::Match, LocalType::Let) => {
                    panic!("cannot reassign a pattern variable: {}", id);
                }
                _ => {
                    // matching on an existing variable, that's cool
                }
            }
        } else {
            let new_var = LocalVar::new(id.clone(), vt);
            self.locals.insert(id.clone(), new_var);
            self.current_block_mut().vars.insert(id.clone());
        }
    }

    pub fn access_var(&mut self, id: &Lstr, lineno: i16)
    {
        let opt_local = self.locals.get_mut(id);
        if opt_local.is_none() {
            panic!("cannot access undefined var: {}", id);
        }
        let vi = opt_local.unwrap();
        if vi.first_access.is_none() {
            vi.first_access = Some(lineno)
        }
        vi.last_access = Some(lineno)
    }

    pub fn var_in_scope(&self, id: &Lstr) -> bool
    {
        self.stack.iter().any(|bs| bs.vars.contains(id))
    }

    pub fn get_failure(&self, name: &Lstr) -> Option<&IfCase>
    {
        for b in self.stack.iter().rev() {
            if b.failures.contains_key(name) {
                return b.failures.get(name);
            }
        }
        None
    }
}

#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub enum ScopeLevel
{
    Local,
    Module(Val),
}

#[derive(Debug)]
pub struct Interscope<'a>
{
    fname: &'a Lstr,
    proto: &'a Protomod,
    imports: &'a HashMap<Lstr, Rc<Protomod>>,
    closure_vars: &'a HashMap<Lstr, Vec<Lstr>>,
    blocks: Blockstack,
    argnames: LinkedList<Kxpr>,
    argt: Type,
    closed: Option<HashSet<Lstr>>,
    is_closure: bool,
}

impl<'a> Interscope<'a>
{
    pub fn new(
        proto: &'a Protomod,
        imports: &'a HashMap<Lstr, Rc<Protomod>>,
        closure_vars: &'a HashMap<Lstr, Vec<Lstr>>,
        fname: &'a Lstr,
        args: &LinkedList<Kxpr>,
        is_closure: bool,
    ) -> Interscope<'a>
    {
        let mut blocks = Blockstack::new();
        let mut argt = Vec::new();
        for (i, a) in args.iter().enumerate() {
            vout!("bind func param as: #{} {:?}\n", i, a);
            let at = Type::from(a.x_ref().unwrap());
            let opt_k = a.k_ref().map(|k| {
                blocks.assign_var(k, LocalType::Param);
                k.clone()
            });
            if opt_k.is_some() {}
            argt.push((opt_k, at));
        }

        let closed = if is_closure {
            Some(HashSet::new())
        } else {
            None
        };

        Interscope {
            fname,
            proto,
            imports,
            closure_vars,
            blocks,
            argnames: args.clone(),
            argt: Type::Tuple(Struple(argt)),
            closed,
            is_closure,
        }
    }

    pub fn import_constval(&self, modnm: &str, valnm: &str) -> Option<&Val>
    {
        if modnm == &self.proto.key.name {
            return self.proto.constant(valnm);
        }
        match self.imports.get(modnm) {
            None => None,
            Some(ref proto) => proto.constant(valnm),
        }
    }

    pub fn module(&self, opt_mod: &Option<Lstr>) -> &Protomod
    {
        match opt_mod {
            None => &self.proto,
            Some(ref self_mod) if self_mod == &self.proto.key.name => {
                &self.proto
            }
            Some(ref import) => self.imports.get(import).unwrap(),
        }
    }

    pub fn import_vartype(&self, modnm: &str, valnm: &str) -> Option<&Type>
    {
        if modnm == &self.proto.key.name {
            return self.proto.valtype(valnm);
        }
        match self.imports.get(modnm) {
            None => None,
            Some(ref proto) => proto.valtype(valnm),
        }
    }

    pub fn imports_module(&self, name: &str) -> bool
    {
        self.imports.contains_key(name)
    }

    pub fn push_blockscope<'b>(&'b mut self) -> NewBlockscope<'a, 'b>
    {
        self.blocks.push_blockscope();
        NewBlockscope::new(self)
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
                    })
                    .unwrap_or(self.proto)
            }
            _ => self.proto,
        }
    }

    pub fn scope_level(&self, id: &Lstr) -> Option<ScopeLevel>
    {
        if self.blocks.var_in_scope(id) {
            Some(ScopeLevel::Local)
        } else {
            self.proto
                .constants
                .get(id)
                .or_else(|| {
                    self.imports
                        .get("prefab")
                        .and_then(|prefab| prefab.constants.get(id))
                })
                .map(|val| ScopeLevel::Module(val.clone()))
        }
    }

    /// get closed variables for the named closure
    pub fn get_closed_vars(&self, name: &str) -> Option<&'a Vec<Lstr>>
    {
        self.closure_vars.get(name)
    }

    pub fn is_closed_empty(&self) -> bool
    {
        self.closed.as_ref().map(|s| s.is_empty()).unwrap_or(true)
    }

    pub fn take_closed(&mut self) -> Vec<Lstr>
    {
        self.closed
            .take()
            .map(|mut s| s.drain().collect())
            .unwrap_or_else(|| Vec::with_capacity(0))
    }
}

pub struct NewBlockscope<'a, 'b>
where
    'a: 'b,
{
    pub scope: &'b mut Interscope<'a>,
}

impl<'a, 'b> NewBlockscope<'a, 'b>
{
    pub fn new(scope: &'b mut Interscope<'a>) -> NewBlockscope<'a, 'b>
    {
        NewBlockscope { scope }
    }

    pub fn collect_failures<'c>(&mut self, stmt: &'c Ast) -> Option<&'c Ast>
    {
        match stmt {
            &Ast::IfExpr(
                ast::IfType::MatchFailure,
                ref input,
                ref cases,
                ref _iloc,
            ) => {
                if let Ast::Localid(ref name, ref _loc2) = **input {
                    let b = self.scope.blocks.current_block_mut();
                    b.add_failure(name, (**cases).clone());
                } else {
                    panic!(
                        "match failure input must be a local variable: {:?}",
                        input
                    );
                }
                None
            }
            _ => Some(stmt),
        }
    }
}

impl<'a, 'b> Drop for NewBlockscope<'a, 'b>
{
    fn drop(&mut self)
    {
        self.scope.blocks.pop_blockscope()
    }
}


pub fn compile_expr(
    scope: &mut Interscope,
    x: &Ast,
    loc: &SrcLoc,
) -> Lresult<Ixpr>
{
    match x {
        &Ast::Block(ref lines) => compile_block(scope, lines, loc),
        &Ast::Localid(ref id, ref loc) => compile_local_id(scope, id, loc),
        &Ast::Modid(ref modules, ref id, ref loc) => {
            compile_modid(scope, modules, id, loc)
        }
        &Ast::DotAccess(ref outer, ref inner) => {
            compile_dot_access(scope, outer, inner, loc)
        }
        &Ast::Call(ref callx, ref args, ref iloc) => {
            compile_call(scope, callx, args, iloc)
        }
        &Ast::TypeCall(ref callx, ref args, ref iloc) => {
            compile_typecall(scope, callx, args, iloc)
        }
        &Ast::ConstBool(b) => Ok(Ixpr::const_val(Val::Bool(b), loc.lineno)),
        &Ast::ConstInt(i) => Ok(Ixpr::const_val(Val::Int(i), loc.lineno)),
        &Ast::ConstStr(ref s) => {
            Ok(Ixpr::const_val(Val::Str(s.clone()), loc.lineno))
        }
        &Ast::ConstHashtag(ref s) => {
            Ok(Ixpr::const_val(Val::Hashtag(s.clone()), loc.lineno))
        }
        &Ast::ConstVoid => Ok(Ixpr::const_val(Val::Void, loc.lineno)),
        &Ast::Cons(ref head, ref tail) => {
            let chead = compile_expr(scope, head, loc)?;
            let ctail = compile_expr(scope, tail, loc)?;
            Ok(Ixpr::cons(chead, ctail, loc.lineno))
        }
        &Ast::IfExpr(_, _, _, _) => compile_ifx(scope, x),
        &Ast::StrExpr(ref items, ref iloc) => {
            let strvec: Lresult<Vec<Ixpr>> =
                items.iter().map(|i| compile_expr(scope, i, iloc)).collect();
            Ok(Ixpr::new_str_mash(strvec?, iloc.lineno))
        }
        &Ast::List(ref items) => {
            let m_items = items.iter().map(|i| compile_expr(scope, i, loc));
            let c_items = Lresult::from_iter(m_items)?;
            Ok(Ixpr::new_list(c_items, loc.lineno))
        }
        &Ast::Tuple(ref items) => {
            let m_items = items.iter().map(|i| {
                let ix = compile_expr(scope, i.x_ref().unwrap(), loc)?;
                Ok((i.k_clone(), ix))
            });
            let c_items = Lresult::from_iter(m_items)?;
            Ok(Ixpr::new_tuple(Struple(c_items), loc.lineno))
        }
        &Ast::ConstructData(ref type_lri, None) => {
            let opt_full_type = scope.proto.func_result_type(&type_lri.localid);
            if opt_full_type.is_none() {
                panic!(
                    "cannot find full type for: {:?} in {:?}",
                    type_lri.localid, scope.proto.valtypes
                );
            }
            let full_type = opt_full_type.unwrap();
            let fields = scope.proto.get_struple_fields(&type_lri.localid);
            Ok(Ixpr::construple(
                full_type.clone(),
                None,
                fields,
                loc.lineno,
            ))
        }
        &Ast::ConstructData(ref type_lri, Some(ref variant)) => {
            let opt_full_type = scope.proto.func_result_type(variant);
            if opt_full_type.is_none() {
                return Err(Failure::new(
                    "type_error",
                    Lstr::from(format!(
                        "cannot find full type for: {:?} in {:?}",
                        type_lri.localid, scope.proto.valtypes
                    )),
                ));
            }
            let full_type = opt_full_type.unwrap();
            let fields = scope.proto.get_struple_fields(variant);
            Ok(Ixpr::construple(
                full_type.clone(),
                Some(variant.clone()),
                fields,
                loc.lineno,
            ))
        }
        &Ast::Let(ref lhs, ref ltype, ref rhs, ref iloc) => {
            compile_let_stmt(scope, lhs, ltype, rhs, iloc)
        }
        _ => {
            Err(Failure::new(
                "compile_error",
                Lstr::from(format!("cannot compile expr: {:?}", x)),
            ))
        }
    }
}

pub fn compile_modid(
    scope: &mut Interscope,
    modname: &Lstr,
    localid: &Lstr,
    loc: &SrcLoc,
) -> Lresult<Ixpr>
{
    vout!("compile_modid({}::{})\n", modname, localid);
    if !scope.imports_module(modname) && *modname != scope.proto.key.name {
        panic!("module not found: {:?}", modname);
    }

    let opt_constval: Option<&Val> = scope.import_constval(modname, localid);
    let constval: &Val = opt_constval.ok_or_else(|| {
        Failure::new(
            "missing_import",
            Lstr::from(format!(
                "cannot find imported value: {}::{}",
                modname, localid
            )),
        )
    })?;
    Ok(Ixpr::const_val(constval.clone(), loc.lineno))
}

pub fn compile_lri(
    scope: &mut Interscope,
    names: &Vec<Lstr>,
    tvars: &Option<LinkedList<Kxpr>>,
    loc: &SrcLoc,
) -> Lresult<Ixpr>
{
    vout!("compile_lri(");
    for n in names.iter() {
        vout!("{},", n);
    }
    vout!(")\n");
    if names.len() > 2 {
        panic!("too many modules: {:?}", names);
    }
    let modname: &Lstr = if names.len() == 2 {
        names.first().as_ref().unwrap().clone()
    } else if tvars.is_some() {
        &scope.proto.key.name
    } else {
        panic!("cannot compile this Lri: {:?}/{:?}", names, tvars);
    };
    if !scope.imports_module(modname) && *modname != scope.proto.key.name {
        panic!("module not found: {:?}", names);
    }
    let id = names.last().unwrap();
    let ctvars: Option<Vec<Type>> = tvars.as_ref().map(|itvars| {
        itvars
            .iter()
            .map(|itv| compile_type(scope, itv.x_ref().unwrap(), loc))
            .collect()
    });
    let lri = Lri::full(Some(modname.clone()), id.clone(), ctvars);

    let opt_constval: Option<&Val> = scope.import_constval(modname, id);
    if opt_constval.is_none() {
        panic!("cannot find imported value: {}::{}", modname, id);
    }
    let constval: &Val = opt_constval.unwrap();
    let special: Val = if lri.has_params() {
        constval.map(&|v| Val::specialize_lri_params(v, &lri))?
    } else {
        // no params to override with, skip the specialization
        constval.clone()
    };
    Ok(Ixpr::const_val(special, loc.lineno))
}

pub fn compile_type(_scope: &mut Interscope, typ: &Ast, _loc: &SrcLoc) -> Type
{
    // this should probably be more, but good enough for now
    Type::from(typ)
}

pub fn compile_local_id(
    scope: &mut Interscope,
    id: &Lstr,
    loc: &SrcLoc,
) -> Lresult<Ixpr>
{
    let ix = match scope.scope_level(id) {
        Some(ScopeLevel::Local) => {
            vout!("compile_local_id_local({})\n", id);
            scope.blocks.access_var(id, loc.lineno);
            Ixpr {
                src: Source::Id(id.clone(), loc.lineno),
                line: loc.lineno,
            }
        }
        Some(ScopeLevel::Module(val)) => {
            vout!("compile_local_id_module({})\n", id);
            match val {
                // Val::FuncRef(fri, ftype) => {
                Val::FuncRef(fri, args, ftype) => {
                    let cval = match scope.get_closed_vars(&fri.localid) {
                        Some(ref vars) if !vars.is_empty() => {
                            let cvar_it = vars.iter().map(|v| {
                                scope.blocks.access_var(v, loc.lineno);
                                (Some(v.clone()), Val::Void)
                            });
                            let fr_args =
                                args.0.into_iter().chain(cvar_it).collect();
                            let cvartype_vec: Vec<
                                StrupleItem<Option<Lstr>, Type>,
                            >;
                            cvartype_vec = vars
                                .iter()
                                .map(|v| {
                                    let tvar = format!("T_cvar_{}", v);
                                    StrupleItem::new(
                                        None,
                                        Type::Var(Lstr::from(tvar)),
                                    )
                                })
                                .collect();
                            let cftype = if let Type::Func(i_ftype) = ftype {
                                Type::Func(FuncType {
                                    args: i_ftype.args,
                                    closed: StrupleKV::from(cvartype_vec),
                                    result: i_ftype.result,
                                })
                            } else {
                                panic!("closure type not a func: {:?}", ftype);
                            };
                            Val::FuncRef(fri, Struple(fr_args), cftype)
                        }
                        _ => Val::FuncRef(fri, args, ftype),
                    };
                    Ixpr {
                        src: Source::ConstVal(cval),
                        line: loc.lineno,
                    }
                }
                _ => {
                    Ixpr {
                        src: Source::ConstVal(val),
                        line: loc.lineno,
                    }
                }
            }
        }
        None => {
            if !scope.is_closure {
                vout!(
                    "undefined variable {} in {:?}\n",
                    id,
                    scope.proto.constants
                );
                panic!("undefined variable: {}", id);
            }
            vout!("closure variable {}\n", id);
            scope.closed.as_mut().unwrap().insert(id.clone());
            Ixpr {
                src: Source::Id(id.clone(), loc.lineno),
                line: loc.lineno,
            }
        }
    };
    Ok(ix)
}

pub fn compile_call(
    scope: &mut Interscope,
    callx: &Ast,
    args: &LinkedList<Kxpr>,
    loc: &SrcLoc,
) -> Lresult<Ixpr>
{
    let icall = compile_expr(scope, callx, loc)?;
    let margs = args.iter().map(|i| {
        let ix = compile_expr(scope, i.x_ref().unwrap(), loc)?;
        Ok((i.k_clone(), ix))
    });
    let iargs = Lresult::from_iter(margs)?;
    Ok(Ixpr {
        src: Source::Call(Box::new(icall), Struple(iargs)),
        line: loc.lineno,
    })
}

pub fn compile_typecall(
    scope: &mut Interscope,
    callx: &Ast,
    args: &LinkedList<Kxpr>,
    loc: &SrcLoc,
) -> Lresult<Ixpr>
{
    let icall = compile_expr(scope, callx, loc)?;
    let margs: Vec<Type> = args
        .iter()
        .map(|i| compile_type(scope, i.x_ref().unwrap(), loc))
        .collect();

    let new_ix = match icall.src {
        Source::ConstVal(Val::FuncRef(fri, fref_args, typ)) => {
            let new_type = match typ {
                Type::GenericFunc(tvars, argt) => {
                    // should specialize this somehow
                    // does this happen in typecheck?
                    // these need to consolidate again already
                    let spec_vars: StrupleKV<Lstr, Type> = tvars
                        .into_iter()
                        .zip(margs.into_iter())
                        .map(|(n, t)| StrupleItem::new(n, t))
                        .collect();
                    Type::SpecialFunc(spec_vars, argt)
                }
                Type::SpecialFunc(_, _) => {
                    return Err(rustfail!(
                        "type_err",
                        "cannot give type params to specialized func: {}{:?}",
                        fri,
                        args,
                    ));
                }
                Type::Func(_) => {
                    return Err(rustfail!(
                        "type_err",
                        "cannot provide type params to normal func: {}{:?}",
                        fri,
                        args,
                    ));
                }
                not_func => {
                    return Err(rustfail!(
                        "leema_fail",
                        "func ref type not a func {}",
                        not_func,
                    ));
                }
            };
            let new_fref = Val::FuncRef(fri, fref_args, new_type);
            let new_src = Source::ConstVal(new_fref);
            Ixpr::new(new_src, icall.line)
        }
        not_func => Ixpr::new(not_func, icall.line),
    };
    Ok(new_ix)
}

pub fn compile_let_stmt(
    scope: &mut Interscope,
    lhs: &Ast,
    ltype: &Ast,
    rhs: &Ast,
    loc: &SrcLoc,
) -> Lresult<Ixpr>
{
    vout!("compile let {:?} := {:?}\n", lhs, rhs);
    let irhs = compile_expr(scope, rhs, loc)?;
    let mut new_vars = Vec::new();
    let cpatt = compile_pattern(scope, &mut new_vars, lhs)?;
    let ctype = compile_type(scope, ltype, loc);
    vout!("new vars in let: {:?} = {:?} = {:?}\n", new_vars, lhs, irhs);
    let m_failures = new_vars.iter().map(|v| {
        scope.blocks.assign_var(v, LocalType::Let);
        compile_failed_var(scope, v, loc)
    });
    let failures: Vec<MatchFailure> = Lresult::from_iter(m_failures)?;
    Ok(Ixpr::new(
        Source::Let(cpatt, ctype, Box::new(irhs), failures),
        loc.lineno,
    ))
}

pub fn compile_dot_access(
    scope: &mut Interscope,
    base_val: &Ast,
    field: &Lstr,
    loc: &SrcLoc,
) -> Lresult<Ixpr>
{
    let ix_base = compile_expr(scope, base_val, loc)?;
    Ok(Ixpr::new_field_access(ix_base, field.clone()))
}

pub fn compile_ifx(scope: &mut Interscope, ifx: &Ast) -> Lresult<Ixpr>
{
    match ifx {
        &Ast::IfExpr(ast::IfType::If, ref const_void, ref case, ref iloc) => {
            if **const_void != Ast::ConstVoid {
                panic!("if input is not void? {:?} @ {}", const_void, iloc);
            }
            compile_if_case(scope, case)
        }
        &Ast::IfExpr(ast::IfType::Match, ref x, ref ifcase, ref iloc) => {
            let ix = compile_expr(scope, x, iloc)?;
            let ixcase = compile_match_case(scope, ifcase)?;
            Ok(Ixpr::new_match_expr(ix, ixcase))
        }
        &Ast::IfExpr(ast::IfType::MatchFailure, _, _, _) => {
            panic!("what's a MatchFailed doing here?");
        }
        _ => {
            panic!("not an expected if expression: {:?}", ifx);
        }
    }
}

pub fn compile_if_case(
    scope: &mut Interscope,
    case: &ast::IfCase,
) -> Lresult<Ixpr>
{
    let ix = if case.cond == Ast::Wildcard {
        if case.else_case.is_some() {
            panic!("cannot have else case for else case: {:?}", case);
        }
        Ixpr::const_val(Val::Bool(true), case.loc.lineno)
    } else {
        compile_expr(scope, &case.cond, &case.loc)?
    };
    let ibody = compile_expr(scope, &case.body, &case.loc)?;
    let mnext: Option<Lresult<Ixpr>> = case
        .else_case
        .as_ref()
        .map(|else_case| compile_if_case(scope, else_case));
    let inext = Failure::transpose(mnext)?;
    Ok(Ixpr::new_if(ix, ibody, inext))
}

pub fn compile_match_case(
    scope: &mut Interscope,
    case: &ast::IfCase,
) -> Lresult<Ixpr>
{
    let (patt, iblk) = {
        let new_block = scope.push_blockscope();
        let mut new_vars = Vec::new();
        let cpatt =
            compile_pattern(new_block.scope, &mut new_vars, &case.cond)?;
        for nv in new_vars.iter() {
            new_block.scope.blocks.assign_var(nv, LocalType::Match);
        }
        (cpatt, compile_expr(new_block.scope, &case.body, &case.loc)?)
    };
    let inext = case
        .else_case
        .as_ref()
        .map_or(Ok(Ixpr::noop()), |else_case| {
            compile_match_case(scope, &else_case)
        })?;
    Ok(Ixpr::new_match_case(patt, iblk, inext))
}

pub fn compile_pattern(
    scope: &mut Interscope,
    new_vars: &mut Vec<Lstr>,
    patt: &Ast,
) -> Lresult<Val>
{
    let cpatt = match patt {
        &Ast::Localid(ref name, _) => {
            new_vars.push(name.clone());
            Val::Id(name.clone())
        }
        &Ast::Cons(ref head, ref tail) => {
            let chead = compile_pattern(scope, new_vars, head)?;
            let ctail = match &**tail {
                &Ast::Localid(_, _) => {
                    compile_pattern(scope, new_vars, &**tail)?
                }
                &Ast::Wildcard => Val::Wildcard,
                &Ast::Cons(_, _) => compile_pattern(scope, new_vars, &**tail)?,
                &Ast::List(_) => compile_pattern(scope, new_vars, &**tail)?,
                _ => {
                    return Err(Failure::new(
                        "invalid_pattern",
                        Lstr::from(format!("invalid pattern tail: {:?}", tail)),
                    ));
                }
            };
            Val::Cons(Box::new(chead), Arc::new(ctail))
        }
        &Ast::List(ref items) => {
            let m_items =
                items.iter().map(|i| compile_pattern(scope, new_vars, i));
            let c_items = Lresult::from_iter(m_items)?;
            list::from_vec(&c_items)
        }
        &Ast::Tuple(ref items) => {
            let mitems = items
                .iter()
                .map(|i| compile_pattern(scope, new_vars, i.x_ref().unwrap()));
            let citems = Lresult::from_iter(mitems)?;
            Val::Tuple(Struple::new_indexed(citems))
        }
        &Ast::ConstInt(i) => Val::Int(i),
        &Ast::ConstBool(b) => Val::Bool(b),
        &Ast::ConstStr(ref s) => Val::Str(s.clone()),
        &Ast::ConstHashtag(ref h) => Val::Hashtag(h.clone()),
        &Ast::Wildcard => Val::Wildcard,
        &Ast::Call(ref callx, ref args, ref iloc) => {
            compile_pattern_call(scope, new_vars, callx, args, iloc)?
        }
        &Ast::Modid(_, _, _) => {
            // this should be a const token
            let tokenri = Lri::from(patt);
            let constval = scope
                .import_constval(&tokenri.modules.unwrap(), &tokenri.localid);
            if constval.is_none() {
                panic!("unknown module value in pattern: {}", patt)
            }
            (*constval.as_ref().unwrap()).clone()
        }
        _ => {
            return Err(Failure::new(
                "invalid_pattern",
                Lstr::from(format!("invalid pattern: {:?}", patt)),
            ));
        }
    };
    Ok(cpatt)
}

pub fn compile_pattern_call(
    scope: &mut Interscope,
    new_vars: &mut Vec<Lstr>,
    callx: &Ast,
    args: &LinkedList<Kxpr>,
    _loc: &SrcLoc,
) -> Lresult<Val>
{
    let args_vec: Vec<(Option<Lstr>, Val)> = {
        let m_args = args.iter().map(|a| {
            let cp = compile_pattern(scope, new_vars, a.x_ref().unwrap())?;
            Ok((a.k_clone(), cp))
        });
        Lresult::from_iter(m_args)?
    };

    let mut struct_lri = Lri::from(callx);
    if !struct_lri.has_modules() {
        struct_lri = struct_lri.add_modules(scope.proto.key.name.clone());
    }
    let dtype = scope
        .module(&struct_lri.modules)
        .deftypes
        .get(&struct_lri.localid);
    if dtype.is_some() {
        return Ok(Val::Struct(struct_lri, Struple(args_vec)));
    }
    let ftype = scope
        .module(&struct_lri.modules)
        .func_result_type(&struct_lri.localid)
        .ok_or_else(|| {
            rustfail!("leema_fail", "cannot find type: {}", struct_lri,)
        })?;
    if let Type::UserDef(ref ftype_ri) = ftype {
        Ok(Val::EnumStruct(
            ftype_ri.clone(),
            struct_lri.localid.clone(),
            Struple(args_vec),
        ))
    } else {
        Err(Failure::new(
            "leema_failure",
            Lstr::from(format!(
                "constructor type is not a structure: {:?}",
                ftype
            )),
        ))
    }
}

pub fn push_block(_scope: &mut Interscope, stmts: &Vec<Ast>) -> Vec<Ast>
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

    lines
}

pub fn compile_block(
    scope: &mut Interscope,
    blk: &Vec<Ast>,
    loc: &SrcLoc,
) -> Lresult<Ixpr>
{
    let mut new_block = scope.push_blockscope();
    let non_failures: Vec<&Ast> = blk
        .iter()
        .filter_map(|stmt| new_block.collect_failures(stmt))
        .collect();
    let mixs = non_failures
        .iter()
        .map(|stmt| compile_block_stmt(&mut new_block.scope, stmt, loc));
    let ixs = Lresult::from_iter(mixs)?;
    Ok(Ixpr::new_block(ixs, loc.lineno))
}

pub fn compile_block_stmt(
    scope: &mut Interscope,
    stmt: &Ast,
    loc: &SrcLoc,
) -> Lresult<Ixpr>
{
    match stmt {
        &Ast::IfExpr(ast::IfType::MatchFailure, ref input, _, _) => {
            panic!("MatchFailed should be removed from blocks: {:?}", input);
        }
        &Ast::Return(ref result, ref iloc) => {
            let cresult = compile_expr(scope, result, iloc)?;
            Ok(Ixpr::new(Source::Return(Box::new(cresult)), iloc.lineno))
        }
        _ => compile_expr(scope, stmt, loc),
    }
}

pub fn compile_failed_var(
    scope: &mut Interscope,
    v: &Lstr,
    loc: &SrcLoc,
) -> Lresult<MatchFailure>
{
    let var_failure = scope.blocks.get_failure(v).map(|f| f.clone());
    let mf = match var_failure {
        Some(fail_case) => {
            vout!("compile failure handling for {}\n", v);
            Some(compile_match_case(scope, &fail_case)?)
        }
        None => None,
    };
    Ok(MatchFailure {
        var: v.clone(),
        case: mf,
        line: loc.lineno,
    })
}

pub fn split_func_args_body(
    defunc: &Ast,
) -> (ast::FuncClass, &LinkedList<Kxpr>, &Ast, &SrcLoc)
{
    match defunc {
        Ast::DefFunc(fc, ref decl, ref body) => {
            vout!("split_func_args({:?})\n", decl.name);
            (*fc, &decl.args, body, &decl.loc)
        }
        _ => {
            panic!("func is not a func: {:?}", defunc);
        }
    }
}

impl fmt::Debug for Intermod
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        writeln!(f, "Intermod{{")?;
        writeln!(f, "\tname: {}", self.modname)?;
        writeln!(f, "\tinterfunc:")?;
        for (fname, fix) in self.interfunc.iter() {
            writeln!(f, "\t\t{}: {:?}", fname, fix)?;
        }
        writeln!(f, "}}")
    }
}


#[cfg(test)]
mod tests
{
    use crate::leema::ast::Ast;
    use crate::leema::inter::{self, Intermod, Interscope, ScopeLevel};
    use crate::leema::loader::Interloader;
    use crate::leema::lstr::Lstr;
    use crate::leema::module::ModKey;
    use crate::leema::phase0::Protomod;
    use crate::leema::program;
    use crate::leema::val::SrcLoc;

    use std::collections::{HashMap, LinkedList};


    #[test]
    fn test_scope_add_vartype()
    {
        let mk = ModKey::name_only(Lstr::Sref("tacos"));
        let proto = Protomod::new(mk);
        let imps = HashMap::new();
        let closed_vars = HashMap::new();
        let args = LinkedList::new();
        let mut scope = Interscope::new(
            &proto,
            &imps,
            &closed_vars,
            &Lstr::Sref("foo"),
            &args,
            false,
        );
        scope
            .blocks
            .assign_var(&Lstr::Sref("hello"), inter::LocalType::Param);

        let scope_lvl = scope.scope_level(&Lstr::from("hello")).unwrap();
        assert_eq!(ScopeLevel::Local, scope_lvl);
    }

    #[test]
    fn test_scope_push_block()
    {
        let mk = ModKey::name_only(Lstr::Sref("tacos"));
        let proto = Protomod::new(mk);
        let imps = HashMap::new();
        let closed_vars = HashMap::new();
        let args = LinkedList::new();
        let mut scope = Interscope::new(
            &proto,
            &imps,
            &closed_vars,
            &Lstr::Sref("foo"),
            &args,
            false,
        );
        scope
            .blocks
            .assign_var(&Lstr::Sref("hello"), inter::LocalType::Let);

        assert!(scope.blocks.var_in_scope(&Lstr::from("hello")));
        let hello_lvl = scope.scope_level(&Lstr::from("hello")).unwrap();
        assert_eq!(ScopeLevel::Local, hello_lvl);

        {
            let new_block = scope.push_blockscope();
            new_block
                .scope
                .blocks
                .assign_var(&Lstr::Sref("world"), inter::LocalType::Let);

            assert!(new_block.scope.blocks.var_in_scope(&Lstr::Sref("world")));
            let world_lvl =
                new_block.scope.scope_level(&Lstr::from("world")).unwrap();
            assert_eq!(ScopeLevel::Local, world_lvl);

            assert!(new_block.scope.blocks.var_in_scope(&Lstr::from("hello")));
            let hello_lvl =
                new_block.scope.scope_level(&Lstr::from("hello")).unwrap();
            assert_eq!(ScopeLevel::Local, hello_lvl);
        }

        assert!(!scope.blocks.var_in_scope(&Lstr::from("world")));
        assert_eq!(None, scope.scope_level(&Lstr::from("world")));
    }

    #[test]
    fn test_new_vars_from_id_pattern()
    {
        let mk = ModKey::name_only(Lstr::Sref("tacos"));
        let proto = Protomod::new(mk.clone());
        let imps = HashMap::new();
        let closed_vars = HashMap::new();
        let args = LinkedList::new();
        let mut scope = Interscope::new(
            &proto,
            &imps,
            &closed_vars,
            &Lstr::Sref("foo"),
            &args,
            false,
        );

        let mut new_vars = Vec::default();
        let patt = Ast::Localid(Lstr::from("x"), SrcLoc::default());

        inter::compile_pattern(&mut scope, &mut new_vars, &patt).unwrap();

        assert_eq!(1, new_vars.len());
        assert_eq!("x", &**(new_vars.first().unwrap()));
    }

    #[test]
    #[should_panic]
    fn test_compile_function_undefined_var()
    {
        let input = "
            func foo(x) ->
                \"($x:$y)\"
            --
            "
        .to_string();

        let foo_str = Lstr::Sref("foo");
        let mut loader = Interloader::new(Lstr::Sref("foo.lma"), "lib");
        loader.set_mod_txt(foo_str.clone(), input);
        let mut prog = program::Lib::new(&mut loader);
        // y is undefined so this should crash b/c it's not a closure
        prog.read_inter(&foo_str);
    }

    #[test]
    fn test_compile_function_closure_def()
    {
        let input = "
            func foo(i) ->
                fn(x) \"($x:$i)\"
            --
            "
        .to_string();

        let foo_str = Lstr::Sref("foo");
        let mut loader = Interloader::new(Lstr::Sref("foo.lma"), "lib");
        loader.set_mod_txt(foo_str.clone(), input);
        let mut prog = program::Lib::new(&mut loader);
        let proto = prog.read_proto(&foo_str);
        let mut inter = Intermod::new(foo_str.clone());
        let closure_name = proto.closures.front().unwrap();
        inter
            .compile_function(&proto, &HashMap::new(), closure_name)
            .unwrap();

        let closed = inter.get_closed_vars(closure_name).unwrap();
        assert_eq!("i", &closed[0]);
        assert_eq!(1, closed.len());
    }

    #[test]
    fn test_compile_anon_func()
    {
        let input = "
            func foo() ->
                let double := fn(x) x * 2
                let ten := double(5)
                \"5 * 2 == $ten\"
            --
            "
        .to_string();

        let mut loader = Interloader::new(Lstr::Sref("foo.lma"), "lib");
        loader.set_mod_txt(Lstr::Sref("foo"), input);
        let mut prog = program::Lib::new(&mut loader);
        let inter = prog.read_inter(&Lstr::Sref("foo"));
        let proto = prog.find_proto("foo").unwrap();
        let closure_name = proto.closures.front().unwrap();

        assert!(inter.get_closed_vars(closure_name).is_none());
        assert!(inter.interfunc.contains_key(closure_name));
        assert!(inter.interfunc.contains_key("foo"));
        assert_eq!(2, inter.interfunc.len());
    }

    #[test]
    fn test_compile_closure()
    {
        let input = "
            func foo(i) ->
                let times_i := fn(x) -> x * i --
                let result := times_i(5)
                \"result := $result\"
            --
            "
        .to_string();

        let mut loader = Interloader::new(Lstr::Sref("foo.lma"), "lib");
        loader.set_mod_txt(Lstr::Sref("foo"), input);
        let mut prog = program::Lib::new(&mut loader);
        let inter = prog.read_inter(&Lstr::Sref("foo"));
        let proto = prog.find_proto("foo").unwrap();
        let closure_name = proto.closures.front().unwrap();

        let closed_vars = inter.get_closed_vars(closure_name).unwrap();
        assert_eq!("i", &closed_vars[0]);
        assert_eq!(1, closed_vars.len());
        assert!(inter.interfunc.contains_key(closure_name));
        assert!(inter.interfunc.contains_key("foo"));
        assert_eq!(2, inter.interfunc.len());
    }

    #[test]
    #[should_panic]
    fn test_compile_undefined_closure_var()
    {
        let input = "
            func foo(j) ->
                let times_i := fn(x) -> x * i --
                let result := times_i(5)
                \"result := $result\"
            --
            "
        .to_string();

        let mut loader = Interloader::new(Lstr::Sref("foo.lma"), "lib");
        loader.set_mod_txt(Lstr::Sref("foo"), input);
        let mut prog = program::Lib::new(&mut loader);
        prog.read_inter(&Lstr::Sref("foo"));
    }

    #[test]
    fn test_compile_matched_if_branches()
    {
        let input = "
            func factf(i): Int ->
                if
                |i == 1 -> 1
                |else -> i * factf(i-1)
                --
            --
            "
        .to_string();

        let mut loader = Interloader::new(Lstr::Sref("fact.lma"), "lib");
        loader.set_mod_txt(Lstr::Sref("fact"), input);
        let mut prog = program::Lib::new(&mut loader);
        prog.read_inter(&Lstr::Sref("fact"));
        // assert that it didn't panic
        assert!(true);
    }

    #[test]
    fn test_pattern_declaration()
    {
        let input = "
            func foo(inputs: [#]) >>
            |[] -> #empty
            |#whatever;more -> #whatever
            |_;more -> foo(more)
            --

            func main() ->
                foo([#a, #b, #c])
            --
            "
        .to_string();

        let mut loader = Interloader::new(Lstr::Sref("tacos.lma"), "lib");
        loader.set_mod_txt(Lstr::Sref("tacos"), input);
        let mut prog = program::Lib::new(&mut loader);
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
        let mut loader = Interloader::new(Lstr::Sref("greeting.lma"), "lib");
        loader.set_mod_txt(greeting_str.clone(), input);
        let mut prog = program::Lib::new(&mut loader);
        prog.read_inter(&greeting_str);
        assert!(true); // didn't panic earlier
    }

    #[test]
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
            "
        .to_string();

        let mut loader = Interloader::new(Lstr::Sref("animals.lma"), "lib");
        loader.set_mod_txt(Lstr::Sref("animals"), input);
        let mut prog = program::Lib::new(&mut loader);
        prog.read_inter(&Lstr::Sref("animals"));
        assert!(true); // didn't panic earlier
    }

    #[test]
    fn test_compile_match_existing_var()
    {
        let input = "
            func foo(): Int ->
                let a := 5
                let b := 8
                match b
                |a -> a + 1
                |_ -> a - 1
                --
            --
            "
        .to_string();

        let mut loader = Interloader::new(Lstr::Sref("foo.lma"), "lib");
        loader.set_mod_txt(Lstr::Sref("foo"), input);
        let mut prog = program::Lib::new(&mut loader);
        prog.read_inter(&Lstr::Sref("foo"));
        assert!(true); // assert that it didn't panic
    }
}
