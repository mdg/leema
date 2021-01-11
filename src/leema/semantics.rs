use crate::leema::ast2::{
    self, Ast, AstMode, AstNode, AstResult, AstStep, Loc, StepResult, Xlist,
};
use crate::leema::canonical::Canonical;
use crate::leema::failure::{self, Failure, Lresult};
use crate::leema::inter::Blockstack;
use crate::leema::lstr::Lstr;
use crate::leema::proto::{self, ProtoLib, ProtoModule};
use crate::leema::struple::{self, StrupleItem, StrupleKV};
use crate::leema::val::{
    Fref, FuncTypeRef, FuncTypeRefMut, Type, TypeArgSlice, TypeArgs, TypeRef,
    TypeRefMut, Val,
};

use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::mem;


/// Stages
/// - prewrite
///   - apply macros
///   - closure collector
/// - analyze
///   - valid code
///   - scope check
///   - collect calls
///   - typecheck
/// - postwrite
///   - optimization / constant folding / code removal
///   - assign registers

const SEMFAIL: &'static str = "semantic_failure";
const TYPEFAIL: &'static str = "type_failure";

struct MacroApplication<'l>
{
    lib: &'l ProtoLib,
    local: &'l ProtoModule,
    ftype: &'l FuncTypeRef<'l>,
    closed: &'l TypeArgs,
}

impl<'l> MacroApplication<'l>
{
    pub fn new(
        lib: &'l ProtoLib,
        local: &'l ProtoModule,
        ftype: &'l FuncTypeRef<'l>,
        closed: &'l TypeArgs,
    ) -> MacroApplication<'l>
    {
        MacroApplication {
            lib,
            local,
            ftype,
            closed,
        }
    }

    fn apply_macro(mac: &Ast, loc: Loc, args: &Xlist) -> AstResult
    {
        let (macro_name, arg_names, body) =
            if let Ast::DefMacro(iname, idefargs, ibody) = mac {
                (iname, idefargs, ibody)
            } else {
                return Err(rustfail!(
                    SEMFAIL,
                    "invalid macro definition: {:?}",
                    mac,
                ));
            };
        match (arg_names.len(), args.len()) {
            (a, b) if a < b => {
                return Err(rustfail!(
                    SEMFAIL,
                    "too many arguments passed to macro {}, expected {}, found {:?}",
                    macro_name,
                    a,
                    args,
                ));
            }
            (a, b) if a > b => {
                return Err(rustfail!(
                    SEMFAIL,
                    "too few arguments passed to macro {}, expected {}",
                    macro_name,
                    a
                ));
            }
            _ => {
                // a == b. cool, proceed
            }
        }

        let mut arg_map: HashMap<&'static str, &AstNode> = HashMap::new();
        for (n, arg_val) in arg_names.iter().zip(args.iter()) {
            arg_map.insert(n, &arg_val.v);
        }
        vout!("replace_ids({:?})\n", arg_map);
        let mut macro_replace = MacroReplacement { arg_map, loc };
        Ok(ast2::walk(body.clone(), &mut macro_replace)?)
    }

    fn op_to_call1(
        func: &'static str,
        a: &mut AstNode,
        b: &mut AstNode,
        loc: Loc,
    ) -> AstNode
    {
        let callx = AstNode::new(Ast::Id(func), loc);
        let new_a = mem::take(a);
        let new_b = mem::take(b);
        let args: Xlist = struple::new_tuple2(new_a, new_b);
        AstNode::new(Ast::Call(callx, args), loc)
    }

    fn find_macro_1(&self, macroname: &str) -> Lresult<Option<&Ast>>
    {
        Ok(self.local.find_macro(macroname))
    }
}

impl<'l> ast2::Op for MacroApplication<'l>
{
    fn pre(&mut self, node: &mut AstNode, mode: AstMode) -> StepResult
    {
        match &mut *node.node {
            Ast::Call(callid, args) => {
                match &mut *callid.node {
                    Ast::Id(macroname) => {
                        if let Some(mac) = self.find_macro_1(macroname)? {
                            *node = Self::apply_macro(mac, callid.loc, args)?;
                            return Ok(AstStep::Rewrite);
                        } // else not a call, that's fine
                    }
                    Ast::Op2(".", base, call) => {
                        if let Ast::ConstVal(callval) = &*call.node {
                            if let Val::Call(_, _) = callval {
                                let base2 = mem::take(base);
                                *callid = mem::take(call);
                                args.insert(0, StrupleItem::new_v(base2));
                            }
                        }
                    }
                    mac @ Ast::DefMacro(_, _, _) => {
                        *node = Self::apply_macro(mac, node.loc, args)?;
                        return Ok(AstStep::Rewrite);
                    }
                    Ast::ConstVal(Val::Call(_, _)) => {
                        // already what it needs to be. continue.
                    }
                    Ast::Generic(_, _) => {
                        // what's happening with generics here?
                        eprintln!(
                            "why generic call at line {}",
                            callid.loc.lineno
                        );
                    }
                    other => {
                        // is something else, like a method call maybe
                        // but what is it?
                        return Err(Failure::static_leema(
                            failure::Mode::StaticLeemaFailure,
                            lstrf!("unexpected call expression: {:?}", other),
                            self.local.key.name.to_lstr(),
                            callid.loc.lineno,
                        ));
                    }
                }
            }
            Ast::Op2("+", a, b) => {
                *node = Self::op_to_call1("int_add", a, b, node.loc);
                return Ok(AstStep::Rewrite);
            }
            Ast::Op2("-", a, b) => {
                *node = Self::op_to_call1("int_sub", a, b, node.loc);
                return Ok(AstStep::Rewrite);
            }
            Ast::Op2("*", a, b) => {
                *node = Self::op_to_call1("int_mult", a, b, node.loc);
                return Ok(AstStep::Rewrite);
            }
            Ast::Op2("/", a, b) => {
                *node = Self::op_to_call1("int_div", a, b, node.loc);
                return Ok(AstStep::Rewrite);
            }
            Ast::Op2("modulo", a, b) => {
                *node = Self::op_to_call1("int_mod", a, b, node.loc);
                return Ok(AstStep::Rewrite);
            }
            Ast::Op2("mod", a, b) => {
                *node = Self::op_to_call1("int_mod", a, b, node.loc);
                return Ok(AstStep::Rewrite);
            }
            Ast::Op2("and", a, b) => {
                *node = Self::op_to_call1("boolean_and", a, b, node.loc);
                return Ok(AstStep::Rewrite);
            }
            Ast::Op2("or", a, b) => {
                *node = Self::op_to_call1("boolean_or", a, b, node.loc);
                return Ok(AstStep::Rewrite);
            }
            Ast::Op2("==", a, b) => {
                *node = Self::op_to_call1("int_equal", a, b, node.loc);
                return Ok(AstStep::Rewrite);
            }
            Ast::Op2("!=", a, b) => {
                *node = Self::op_to_call1("not_equal", a, b, node.loc);
                return Ok(AstStep::Rewrite);
            }
            Ast::Op2("<", a, b) => {
                *node = Self::op_to_call1("int_less_than", a, b, node.loc);
                return Ok(AstStep::Rewrite);
            }
            Ast::Op2(">", a, b) => {
                *node = Self::op_to_call1("int_gt", a, b, node.loc);
                return Ok(AstStep::Rewrite);
            }
            Ast::Op2("<=", a, b) => {
                *node = Self::op_to_call1("int_lteq", a, b, node.loc);
                return Ok(AstStep::Rewrite);
            }
            Ast::Op2(">=", a, b) => {
                *node = Self::op_to_call1("int_gteq", a, b, node.loc);
                return Ok(AstStep::Rewrite);
            }
            Ast::Op2(";", a, b) => {
                if !mode.is_pattern() {
                    // if not a pattern, convert to a call
                    let callx = AstNode::new(Ast::Id("cons"), node.loc);
                    let args = struple::new_tuple2(mem::take(a), mem::take(b));
                    *node = AstNode::new(Ast::Call(callx, args), node.loc);
                    return Ok(AstStep::Rewrite);
                }
            }
            Ast::Op1("not", x) => {
                let callx = AstNode::new(Ast::Id("boolean_not"), node.loc);
                let arg = vec![StrupleItem::new_v(mem::take(x))];
                *node = AstNode::new(Ast::Call(callx, arg), node.loc);
                return Ok(AstStep::Rewrite);
            }
            Ast::Op1("-", x) => {
                let callx = AstNode::new(Ast::Id("int_negate"), node.loc);
                let arg = vec![StrupleItem::new_v(mem::take(x))];
                *node = AstNode::new(Ast::Call(callx, arg), node.loc);
                return Ok(AstStep::Rewrite);
            }
            Ast::Op1("\\n", x) => {
                let newline =
                    AstNode::new_constval(Val::Str(Lstr::Sref("\n")), node.loc);
                let strx = Ast::StrExpr(vec![mem::take(x), newline]);
                *node = AstNode::new(strx, node.loc);
                return Ok(AstStep::Rewrite);
            }
            Ast::ConstVal(Val::Str(s)) => {
                match s.str() {
                    "\\n" => {
                        *node = AstNode::new(
                            Ast::ConstVal(Val::Str(Lstr::Sref("\n"))),
                            node.loc,
                        );
                    }
                    "\\\"" => {
                        *node = AstNode::new(
                            Ast::ConstVal(Val::Str(Lstr::Sref("\""))),
                            node.loc,
                        );
                    }
                    _ => {} // nothing
                }
            }
            Ast::Id(id) => {
                let nloc = node.loc;
                if let Some(ctype) = struple::find(self.closed, &id) {
                    let type_ast = Ast::Type(ctype.clone());
                    *node = AstNode::new(type_ast, nloc);
                    return Ok(AstStep::Rewrite);
                }
            }
            Ast::Matchx(None, cases) => {
                let node_loc = node.loc;
                let args: Lresult<Xlist> = self
                    .ftype
                    .args
                    .iter()
                    .map(|arg| {
                        if let Lstr::Sref(argname) = arg.k {
                            let argnode =
                                AstNode::new(Ast::Id(argname), node_loc);
                            Ok(StrupleItem::new_v(argnode))
                        } else {
                            return Err(rustfail!(
                                SEMFAIL,
                                "arg name is not a static str: {:?}",
                                arg.k,
                            ));
                        }
                    })
                    .collect();
                let match_input = AstNode::new(Ast::Tuple(args?), node.loc);
                let matchx = Ast::Matchx(Some(match_input), mem::take(cases));
                *node = AstNode::new(matchx, node.loc);
                return Ok(AstStep::Rewrite);
            }
            // for single element tuples, just take the single item
            Ast::Tuple(items) => {
                if items.len() == 1 {
                    if items.first().unwrap().k.is_none() {
                        *node = items.pop().unwrap().v;
                        return Ok(AstStep::Rewrite);
                    }
                }
            }
            _ => {}
        }
        Ok(AstStep::Ok)
    }
}

impl<'l> fmt::Debug for MacroApplication<'l>
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "MacroApplication")
    }
}

#[derive(Debug)]
struct MacroReplacement<'a>
{
    arg_map: HashMap<&'static str, &'a AstNode>,
    loc: Loc,
}

impl<'a> MacroReplacement<'a>
{
    fn expand_args(&self, args: &mut Xlist) -> StepResult
    {
        let args2: Lresult<Xlist> =
            args.drain(..).try_fold(vec![], |mut acc, a| {
                if let Ast::Op1("*", expansion_node) = &*a.v.node {
                    match &*expansion_node.node {
                        Ast::Id(expansion) => {
                            if let Some(newval) = self.arg_map.get(expansion) {
                                match &*newval.node {
                                    Ast::Tuple(items) => {
                                        acc.extend_from_slice(&items[..]);
                                    }
                                    other => {
                                        return Err(rustfail!(
                                            "macro_error",
                                            "cannot expand {:?}",
                                            other,
                                        ));
                                    }
                                }
                            } else {
                                return Err(rustfail!(
                                    "macro_error",
                                    "undefined macro parameter: {}",
                                    expansion,
                                ));
                            }
                        }
                        what => {
                            return Err(rustfail!(
                                "macro_error",
                                "expected id, found {:?}",
                                what,
                            ));
                        }
                    }
                } else {
                    acc.push(a);
                }
                Ok(acc)
            });

        mem::swap(args, &mut args2?);
        Ok(AstStep::Ok)
    }
}

impl<'a> ast2::Op for MacroReplacement<'a>
{
    fn pre(&mut self, node: &mut AstNode, mode: AstMode) -> StepResult
    {
        match &mut *node.node {
            Ast::Id(idname) => {
                if let Some(newval) = self.arg_map.get(idname) {
                    *node = (*newval).clone();
                    return Ok(AstStep::Rewrite);
                }
            }
            Ast::Call(_callx, ref mut args) if mode == AstMode::Value => {
                self.expand_args(args)?;
            }
            _ => {} // nothing, all good
        }
        // if not replacing the id, replace the location of the call
        // so everything in the macro body traces back to the macro name
        node.loc = self.loc;
        Ok(AstStep::Ok)
    }
}

struct ScopeCheck<'p>
{
    lib: &'p ProtoLib,
    local_mod: &'p ProtoModule,
    blocks: Blockstack,
}

impl<'p> ScopeCheck<'p>
{
    pub fn new(
        lib: &'p ProtoLib,
        local_mod: &'p ProtoModule,
        ftyp: &'p FuncTypeRef<'p>,
    ) -> Lresult<ScopeCheck<'p>>
    {
        let mut args: Vec<&'static str> = Vec::new();
        for a in ftyp.args.iter() {
            match a.k {
                Lstr::Sref(arg) => {
                    args.push(arg);
                }
                _ => {
                    panic!("function argument missing name: {:?}", ftyp);
                }
            }
        }
        Ok(ScopeCheck {
            lib,
            local_mod,
            blocks: Blockstack::with_args(args),
        })
    }
}

impl<'p> ast2::Op for ScopeCheck<'p>
{
    fn pre(&mut self, node: &mut AstNode, mode: AstMode) -> StepResult
    {
        let loc = node.loc;
        match &mut *node.node {
            Ast::Block(_) => {
                self.blocks.push_blockscope();
            }
            Ast::Id(id) if mode.is_pattern() => {
                let local_type = mode.get_pattern().unwrap();
                // make sure this doesn't duplicate an import
                // why not just add ModAliases to the blocks?
                if self.local_mod.imports.contains_key(id) {
                    return Err(Failure::static_leema(
                        failure::Mode::CompileFailure,
                        lstrf!("{} is already used as a module name", id),
                        self.local_mod.key.best_path(),
                        node.loc.lineno,
                    ));
                }
                node.dst = self.blocks.assign_var(id, local_type)?;
            }
            Ast::Id(id) if mode == AstMode::Type => {
                let found = self.local_mod.find_type(id);
                if let Some(typ) = found {
                    node.replace(
                        Ast::ConstVal(Val::Type(typ.clone())),
                        Type::KIND,
                    );
                }
            }
            Ast::Id(id) => {
                if let Some(r) = self.blocks.var_in_scope(id) {
                    node.dst = r;
                } else if let Some(me) = self.local_mod.find_modelem(id) {
                    node.replace((*me.node).clone(), me.typ.clone());
                    return Ok(AstStep::Rewrite);
                } else if let Some(b) = proto::find_builtin(id) {
                    node.replace_node(b.clone());
                    return Ok(AstStep::Rewrite);
                } else {
                    return Err(rustfail!(
                        SEMFAIL,
                        "var not in scope: {} @ {:?}",
                        id,
                        loc,
                    ));
                }
            }
            Ast::Call(ref mut callx, _) => {
                // go depth first on the call expression
                steptry!(ast2::walk_ref_mut(callx, self));
                return Ok(AstStep::Ok);
            }
            Ast::Canonical(c) => {
                if let Ok(proto) = self.lib.path_proto(c) {
                    // check for type and find constructor
                    let optcons = proto.find_modelem(proto::MODNAME_CONSTRUCT);
                    if optcons.is_none() {
                        return Err(rustfail!(
                            "compile_error",
                            "replace {} with constructor",
                            c,
                        ));
                    }
                    let cons = optcons.unwrap();
                    node.replace((*cons.node).clone(), cons.typ.clone());
                    return Ok(AstStep::Rewrite);
                } else if let Some((parent, id)) = c.split_function() {
                    match self.lib.exported_elem(&parent, id.as_str(), node.loc)
                    {
                        Ok(f) => {
                            node.replace((*f.node).clone(), f.typ.clone());
                            return Ok(AstStep::Rewrite);
                        }
                        Err(_) => {
                            return Err(Failure::static_leema(
                                failure::Mode::CompileFailure,
                                lstrf!("undefined: {}", c),
                                self.local_mod.key.best_path(),
                                node.loc.lineno,
                            )
                            .with_context(vec![
                                StrupleItem::new(
                                    Lstr::Sref("rustfile"),
                                    Lstr::Sref(file!()),
                                ),
                                StrupleItem::new(
                                    Lstr::Sref("rustline"),
                                    lstrf!("{}", line!()),
                                ),
                            ]));
                        }
                    }
                } else {
                    return Err(rustfail!(
                        "semantic_error",
                        "undefined: {}",
                        c,
                    ));
                }
            }
            Ast::Op2(".", base_node, sub_node) => {
                if let (Ast::Id(id), Ast::Id(sub)) =
                    (&*base_node.node, &*sub_node.node)
                {
                    if self.blocks.var_in_scope(id).is_none() {
                        if let Some(me) = self.local_mod.find_modelem(id) {
                            match &*me.node {
                                Ast::Canonical(can) => {
                                    let node2 = self.lib.exported_elem(
                                        can,
                                        sub,
                                        base_node.loc,
                                    )?;
                                    node.replace(
                                        (*node2.node).clone(),
                                        node2.typ.clone(),
                                    );
                                }
                                _ => {
                                    return Err(rustfail!(
                                        SEMFAIL,
                                        "sub {} from id {} as {:?}",
                                        sub,
                                        id,
                                        me,
                                    ));
                                }
                            }
                        } else {
                            return Err(rustfail!(
                                SEMFAIL,
                                "what's it? {}.{} in {}",
                                id,
                                sub,
                                self.local_mod.key.name,
                            ));
                        }
                    }
                    // else is a regular var in scope
                }
                // else it's probably (hopefully?) a method or something
            }
            Ast::Generic(_base, _args) => {
                // what's happening w/ generics here?
                /*
                return Err(Failure::static_leema(
                    failure::Mode::LeemaTodoFailure,
                    Lstr::Sref("generics are not yet implemented here"),
                    self.local_mod.key.name.0.clone(),
                    loc.lineno,
                ));
                */
            }
            _ => {
                // do nothing otherwise
            }
        }
        Ok(AstStep::Ok)
    }

    fn post(&mut self, node: &mut AstNode, _mode: AstMode) -> StepResult
    {
        match &*node.node {
            Ast::Block(_) => {
                self.blocks.pop_blockscope();
            }
            _ => {
                // do nothing, keep walking
            }
        }
        Ok(AstStep::Ok)
    }
}

impl<'l> fmt::Debug for ScopeCheck<'l>
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "ScopeCheck({})", self.local_mod.key.name)
    }
}

/// Check types within a function call
///
/// Normally match types w/ self.match_type(t0, t1)
/// If there's a generic call, put the arg types in scope
/// how to differentiate between regular type vars and function type vars?
/// Need to collect which generic funcs have types applied
/// Function type parameters should be converted to real types
/// before typechecking
struct TypeCheck<'p>
{
    lib: &'p ProtoLib,
    local_mod: &'p ProtoModule,
    result: Type,
    vartypes: HashMap<&'static str, Type>,
    infers: HashMap<Lstr, Type>,
    calls: Vec<Fref>,
}

impl<'p> TypeCheck<'p>
{
    pub fn new(
        lib: &'p ProtoLib,
        local_mod: &'p ProtoModule,
        ftyp: &'p FuncTypeRef<'p>,
    ) -> Lresult<TypeCheck<'p>>
    {
        let mut check = TypeCheck {
            lib,
            local_mod,
            result: Type::VOID,
            vartypes: HashMap::new(),
            infers: HashMap::new(),
            calls: vec![],
        };

        for arg in ftyp.args.iter() {
            let argname = arg.k.sref()?;
            check.vartypes.insert(argname, arg.v.clone());
        }
        check.result = (*ftyp.result).clone();
        Ok(check)
    }

    pub fn inferred_local(&self, local_tvar: &Lstr) -> Type
    {
        self.infers
            .get(local_tvar.as_ref())
            .map(|t| t.clone())
            .unwrap_or_else(|| Type::local(local_tvar.clone()))
    }

    pub fn inferred_type(&self, t: &Type, opens: &TypeArgSlice)
        -> Lresult<Type>
    {
        let newt = match t.type_ref() {
            TypeRef(Type::PATH_LOCAL, [StrupleItem { k: local, .. }, ..]) => {
                self.inferred_local(local)
            }
            TypeRef(Type::PATH_OPENVAR, [StrupleItem { k: open, .. }, ..]) => {
                struple::find(opens, open)
                    .map(|item| item.clone())
                    .unwrap_or_else(|| Type::open(open.clone()))
            }
            _ => t.map_v(&|a| self.inferred_type(a, opens))?,
        };
        Ok(newt)
    }

    /// match one type to another
    /// 1. List(OpenVarA) == LocalX
    ///    OpenVarA == LocalX.Inner
    ///    nah, use 2 instead
    /// 2. match List(OpenVarA) == LocalX
    ///    infer LocalX = List(LocalX.Inner0)
    ///    match List(OpenVarA) == List(LocalX.Inner0)
    /// 3. match List(LocalX) == OpenVarA
    ///    close_generic(OpenVarA, List(LocalX)
    /// 4. match Tuple(OpenVarA, OpenVarB) == LocalX
    ///    Tuple(OpenVarA, OpenVarB) == Tuple(LocalX.Inner0, LocalX.Inner1)
    pub fn match_type(
        &mut self,
        t0: &Type,
        t1: &Type,
        opens: &mut TypeArgSlice,
    ) -> Lresult<Type>
    {
        match (t0.path_str(), t1.path_str()) {
            // unknown and failure cases
            // failure defaults to the other type b/c possible failure
            // is implicit in types
            (Type::PATH_FAILURE, _) | (Type::PATH_UNKNOWN, _) => {
                t1.clone_closed()
            }
            (_, Type::PATH_FAILURE) | (_, Type::PATH_UNKNOWN) => {
                t0.clone_closed()
            }
            // locals var cases
            (Type::PATH_LOCAL, Type::PATH_LOCAL) => {
                let v0 = &t0.first_arg()?.k;
                let v1 = &t1.first_arg()?.k;
                match Ord::cmp(v0, v1) {
                    Ordering::Equal => Ok(self.inferred_local(v0)),
                    Ordering::Less => {
                        lfailoc!(self.infer_type(v0, t1, opens))
                    }
                    Ordering::Greater => {
                        lfailoc!(self.infer_type(v1, t0, opens))
                    }
                }
            }
            (Type::PATH_LOCAL, _) => {
                let v0 = &t0.first_arg()?.k;
                lfailoc!(self.infer_type(v0, t1, opens))
            }
            (_, Type::PATH_LOCAL) => {
                let v1 = &t1.first_arg()?.k;
                lfailoc!(self.infer_type(v1, t0, opens))
            }
            // open var cases
            (Type::PATH_OPENVAR, _) => {
                let k0 = &t0.first_arg()?.k;
                lfailoc!(self.close_generic(k0.as_str(), t1, opens))
            }
            (_, Type::PATH_OPENVAR) => {
                let k1 = &t1.first_arg()?.k;
                lfailoc!(self.close_generic(k1.as_str(), t0, opens))
            }
            // type names match
            (p0, p1) if p0 == p1 && t0.argc() == t1.argc() => {
                let im: Lresult<TypeArgs>;
                im = t0
                    .args
                    .iter()
                    .zip(t1.args.iter())
                    .map(|iz| {
                        let k = iz.0.k.clone();
                        let v = ltry!(self.match_type(&iz.0.v, &iz.1.v, opens));
                        Ok(StrupleItem::new(k, v))
                    })
                    .collect();
                Ok(Type::new(t0.path.clone(), im?))
            }
            // case for alias types where the types may need to be
            // dereferenced before they will match
            _ => {
                if let Some(r) = self.match_type_alias(&t0.path, t1, opens)? {
                    return Ok(r);
                }
                if let Some(r) = self.match_type_alias(&t1.path, t0, opens)? {
                    return Ok(r);
                }
                Err(rustfail!(
                    SEMFAIL,
                    "types do not match: ({} != {})",
                    t0,
                    t1,
                ))
            }
        }
    }

    // check if one of these type 0 is an alias for type 1
    fn match_type_alias(
        &mut self,
        u0: &Canonical,
        t1: &Type,
        opens: &mut TypeArgSlice,
    ) -> Lresult<Option<Type>>
    {
        if let Ok(proto) = self.lib.path_proto(u0) {
            if let Some(alias) = proto.alias_type() {
                return Ok(Some(ltry!(self.match_type(alias, t1, opens))));
            }
        }
        Ok(None)
    }

    pub fn infer_type(
        &mut self,
        var: &Lstr,
        t: &Type,
        opens: &mut TypeArgSlice,
    ) -> Lresult<Type>
    {
        if self.infers.contains_key(var) {
            let var_type = self.inferred_local(var);
            if var_type == *t {
                Ok(var_type)
            } else {
                Ok(lfctx!(
                    self.match_type(&var_type, t, opens),
                    "inferring type var": var.clone(),
                    "as type": lstrf!("{}", var_type)
                ))
            }
        } else {
            self.infers.insert(var.clone(), t.clone());
            Ok(t.clone())
        }
    }

    pub fn close_generic(
        &mut self,
        var: &str,
        t: &Type,
        opens: &mut TypeArgSlice,
    ) -> Lresult<Type>
    {
        let open_idx =
            struple::find_idx(opens, var).map(|i| i.0).ok_or_else(|| {
                rustfail!(
                    TYPEFAIL,
                    "open type var {:?} not found in {:?} for {:?}",
                    var,
                    opens,
                    t,
                )
            })?;

        let open = &opens[open_idx].v;
        if *open == Type::UNKNOWN {
            // newly defined type, set it in opens
            opens[open_idx].v = t.clone();
            Ok(t.clone())
        } else if open.is_openvar() {
            if open.first_arg()?.k.as_str() == var {
                // newly defined type, set it in opens
                opens[open_idx].v = t.clone();
                Ok(t.clone())
            } else {
                panic!("unexpected var: {}", open);
            }
        } else if open == t {
            // already the same type, good
            Ok(t.clone())
        } else {
            // different type, run a match and then assign the result
            // this upgrades inference local vars to concrete types
            let old_type = opens[open_idx].v.clone();
            let new_type = ltry!(self.match_type(&old_type, t, opens));
            opens[open_idx].v = new_type.clone();
            Ok(new_type)
        }
    }

    /// What does apply_typecall do?
    pub fn apply_typecall(
        &mut self,
        calltype: &mut Type,
        args: &mut ast2::Xlist,
    ) -> Lresult<Type>
    {
        let TypeRefMut(_p, targs) = calltype.try_generic_ref_mut()?;

        if args.len() != targs.len() {
            return Err(rustfail!(
                TYPEFAIL,
                "wrong number of args, expected {}, found {}",
                targs.len(),
                args.len(),
            ));
        }
        for a in targs.iter_mut().zip(args.iter_mut()) {
            match &*a.1.v.node {
                Ast::ConstVal(Val::Type(t)) => {
                    a.0.v = t.clone();
                }
                Ast::Id(id) => {
                    a.0.v = self
                        .local_mod
                        .find_type(id)
                        .ok_or_else(|| {
                            rustfail!(
                                TYPEFAIL,
                                "undefined type: {} in module {}",
                                id,
                                self.local_mod.key.name,
                            )
                        })?
                        .clone();
                }
                what => {
                    let t = self.local_mod.ast_to_type(&a.1.v, &[])?;
                    a.0.v = t;
                    panic!("unexpected type setting: {:#?}", what);
                }
            }
        }
        let t = self.inferred_type(&calltype, calltype.type_args())?;
        *calltype = t;
        Ok(calltype.clone())
    }

    fn first_open_var(
        typeargs: &StrupleKV<&'static str, Type>,
    ) -> Lresult<(usize, &'static str)>
    {
        for (idx, i) in typeargs.iter().enumerate() {
            if i.v.is_openvar() || i.v == Type::UNKNOWN {
                return Ok((idx, i.k));
            }
        }
        Err(Failure::static_leema(
            failure::Mode::CompileFailure,
            lstrf!("open type has no open subtypes: {:?}", typeargs),
            Lstr::Sref(""),
            0,
        ))
    }

    pub fn applied_call_type(
        &mut self,
        calltype: &mut Type,
        args: &mut ast2::Xlist,
    ) -> Lresult<Type>
    {
        let mut funcref = calltype.try_func_ref_mut()?;
        self.match_argtypes(&mut funcref, args)
    }

    fn match_argtypes<'a>(
        &mut self,
        ftyp: &mut FuncTypeRefMut<'a>,
        args: &mut ast2::Xlist,
    ) -> Lresult<Type>
    {
        if args.len() < ftyp.args.len() {
            vout!("expected function type: {:#?}\n", ftyp);
            vout!("given args: {:#?}\n", args);
            return Err(rustfail!(
                SEMFAIL,
                "too few arguments, expected {}, found {}",
                ftyp.args.len(),
                args.len(),
            ));
        }
        if args.len() > ftyp.args.len() {
            return Err(rustfail!(
                SEMFAIL,
                "too many arguments, expected {}, found {}",
                ftyp.args.len(),
                args.len(),
            ));
        }

        for arg in ftyp.args.iter_mut().zip(args.iter_mut()) {
            let typ = ltry!(self
                .match_type(&arg.0.v, &arg.1.v.typ, &mut ftyp.type_args)
                .map_err(|f| {
                    f.add_context(lstrf!(
                        "function param: {}, expected {}, found {} column:{}",
                        arg.0.k.as_ref(),
                        arg.0.v,
                        arg.1.v.typ,
                        arg.1.v.loc.column,
                    ))
                    .lstr_loc(
                        self.local_mod.key.best_path(),
                        arg.1.v.loc.lineno as u32,
                    )
                }));
            arg.0.v = typ.clone();
            arg.1.v.typ = typ;
        }

        *ftyp.result = self.inferred_type(&ftyp.result, &ftyp.type_args)?;
        Ok((*ftyp.result).clone())
    }

    fn match_case_types(&mut self, cases: &Vec<ast2::Case>) -> Lresult<Type>
    {
        let mut prev_typ: Option<Type> = None;
        let mut opens = vec![];
        for case in cases.iter() {
            let next_typ = if let Some(ref pt) = prev_typ {
                ltry!(self.match_type(pt, &case.body.typ, &mut opens))
            } else {
                case.body.typ.clone()
            };
            prev_typ = Some(next_typ);
        }
        Ok(prev_typ.unwrap())
    }

    fn post_call(
        &mut self,
        call_typ: &mut Type,
        fref: &mut Fref,
        args: &mut Xlist,
        loc: Loc,
    ) -> Lresult<AstStep>
    {
        // an optimization here might be to iterate over
        // ast args and initialize any constants
        // actually better might be to stop having
        // the args in the Val::Call const?

        *call_typ =
            ltry!(self.applied_call_type(&mut fref.t, args).map_err(|f| {
                f.add_context(lstrf!("for function: {}", fref,))
                    .lstr_loc(self.local_mod.key.best_path(), loc.lineno as u32)
            }));
        self.calls.push(fref.clone());
        /*
        error message for some other scenario, maybe unnecessary
        */
        Ok(AstStep::Ok)
    }

    fn post_field_access(
        &self,
        base_typ: &Type,
        fld: &mut AstNode,
    ) -> Lresult<AstStep>
    {
        match (base_typ.type_ref(), &*fld.node) {
            (TypeRef(tname, targs), Ast::Id(f)) if targs.is_empty() => {
                // find a field in a struct or interface or
                // whatever else
                let tproto = self.lib.path_proto(&base_typ.path)?;

                if let Some(found) = tproto.find_method(f) {
                    match &*found.node {
                        Ast::ConstVal(_) => {
                            fld.replace(
                                (*found.node).clone(),
                                found.typ.clone(),
                            );
                        }
                        Ast::DataMember(_, _) => {
                            fld.replace(
                                (*found.node).clone(),
                                found.typ.clone(),
                            );
                        }
                        other => {
                            return Err(rustfail!(
                                "leema_failure",
                                "invalid type field: {}.{} {:?}",
                                tname,
                                f,
                                other,
                            ));
                        }
                    }
                } else {
                    return Err(Failure::static_leema(
                        failure::Mode::CompileFailure,
                        lstrf!("type has no field: {}.{}", tname, f,),
                        self.local_mod.key.name.to_lstr(),
                        fld.loc.lineno,
                    ));
                }
                Ok(AstStep::Ok)
            }
            (_, Ast::Id(id)) => {
                return Err(Failure::static_leema(
                    failure::Mode::CompileFailure,
                    lstrf!("builtin type has no {} field: {}", id, base_typ,),
                    self.local_mod.key.name.to_lstr(),
                    fld.loc.lineno,
                ));
            }
            (_, f) => {
                panic!("unsupported field name: {:#?}", f);
            }
        }
    }
}

impl<'p> ast2::Op for TypeCheck<'p>
{
    fn pre(&mut self, node: &mut AstNode, _mode: AstMode) -> StepResult
    {
        match &mut *node.node {
            Ast::Id(id) => {
                // mode == value
                // if the type is known, assign it to this variable
                if let Some(typ) = self.vartypes.get(id) {
                    node.typ = typ.clone();
                // put the node back the way it was
                // *node.node = Ast::Id(id);
                } else {
                    let tvar = Type::local(Lstr::Sref(id));
                    self.vartypes.insert(id, tvar.clone());
                    node.typ = tvar;
                    // put the node back the way it was
                    // *node.node = Ast::Id(id);
                }
            }
            Ast::ConstVal(c) if node.typ.is_open() => {
                node.typ = c.get_type();
            }
            Ast::RustBlock => {
                node.typ = self.result.clone();
            }
            Ast::Wildcard => {
                node.typ = Type::UNKNOWN;
            }
            Ast::Op2(_, _, _) => {
                // handled in post
            }
            Ast::Block(_)
            | Ast::Call(_, _)
            | Ast::Tuple(_)
            | Ast::Let(_, _, _) => {
                // handled in post
            }
            _ => {
                // should handle matches later, but for now it's fine
            }
        }
        Ok(AstStep::Ok)
    }

    /// TypeCheck post
    fn post(&mut self, node: &mut AstNode, _mode: AstMode) -> StepResult
    {
        match &mut *node.node {
            Ast::Block(items) => {
                if let Some(last) = items.last() {
                    node.typ = last.typ.clone();
                } else {
                    node.typ = Type::VOID;
                }
            }
            Ast::Id(_id) => {
                // does this _id need to be used? should be ok b/c the type
                // incorporates the id from pre phase
                let nopens = vec![];
                let id_type = self.inferred_type(&node.typ, &nopens)?;
                node.typ = id_type;
            }
            // set struct fields w/ name(x: y) syntax
            Ast::Call(ref mut callx, ref mut args) if callx.typ.is_user() => {
                let copy_typ = callx.typ.clone();
                let base = mem::take(callx);
                let args_copy = mem::take(args);
                return Ok(AstStep::Replace(
                    // should this be CopyIfNecessary instead?
                    // or maybe just Set and rely on Rc::make_mut?
                    Ast::CopyAndSet(base, args_copy),
                    copy_typ,
                ));
            }
            Ast::Call(ref mut callx, ref mut args) => {
                match &mut *callx.node {
                    // handle a non-method call
                    Ast::ConstVal(Val::Call(fref, _)) => {
                        steptry!(self.post_call(
                            &mut node.typ,
                            fref,
                            args,
                            node.loc
                        ));
                        callx.typ = fref.t.clone();
                    }
                    // handle a method call
                    Ast::Op2(".", ref mut base_ref, ref mut method_ref) => {
                        let base = mem::take(base_ref);
                        let method = mem::take(method_ref);
                        args.insert(0, StrupleItem::new_v(base));
                        *callx.node = *method.node;
                        callx.typ = method.typ;
                        return Ok(AstStep::Rewrite);
                    }
                    Ast::Generic(ref mut base, ref mut type_args) => {
                        return Err(rustfail!(
                            "leema_failure",
                            "generic not previously handled: <{:?} {:?}>",
                            base,
                            type_args,
                        ));
                    }
                    // handle closure call
                    // do they get handled like regular non-method calls
                    // and rely on the callx.typ?
                    closure => {
                        panic!("closures not supported: {:?}", closure);
                    }
                }
            }
            // field access, maybe a method
            Ast::Op2(".", a, b) => {
                steptry!(self.post_field_access(&a.typ, b));
            }
            Ast::Generic(ref mut callx, ref mut args) => {
                if let Ast::ConstVal(Val::Call(ref mut fref, _)) =
                    &mut *callx.node
                {
                    let typecall_t = self.apply_typecall(&mut fref.t, args)?;
                    if typecall_t.is_closed() {
                        *node.node = (*callx.node).clone();
                        node.typ = typecall_t;
                    } else {
                        callx.typ = typecall_t.clone();
                        node.typ = typecall_t;
                    }
                }
            }
            Ast::StrExpr(ref _items) => {
                // check items, but not necessary yet b/c everything
                // converts to strings right now
                node.typ = Type::STR.clone();
            }
            Ast::Copy(ref src) => {
                // this is a weird hacky thing just to pass through
                // any types to the children
                node.typ = src.typ.clone();
            }
            Ast::Ifx(ref mut cases) => {
                // all if cases should be boolean
                let mut opens = vec![];
                for case in cases.iter_mut() {
                    ltry!(self.match_type(
                        &case.cond.typ,
                        &Type::BOOL,
                        &mut opens
                    ));
                    case.cond.typ = Type::BOOL.clone();
                }
                node.typ = self.match_case_types(cases)?;
            }
            Ast::Matchx(Some(ref mut input), ref mut cases) => {
                let mut opens = vec![];
                for case in cases.iter_mut() {
                    let it = self
                        .match_type(&input.typ, &case.cond.typ, &mut opens)
                        .map_err(|f| {
                            f.add_context(lstrf!(
                                "for pattern {:?} at {:?}",
                                case.cond.node,
                                case.cond.loc
                            ))
                            .lstr_loc(
                                self.local_mod.key.best_path(),
                                case.cond.loc.lineno as u32,
                            )
                        })?;
                    input.typ = it.clone();
                    case.cond.typ = it;
                }
                node.typ = self.match_case_types(cases)?;
            }
            Ast::Let(ref mut patt, _, ref mut x) => {
                let mut opens = vec![];
                let typ = ltry!(self.match_type(&patt.typ, &x.typ, &mut opens));
                patt.typ = typ.clone();
                x.typ = typ;
            }
            Ast::List(ref inner) => {
                let inner_typ = inner
                    .first()
                    .map(|item| item.v.typ.clone())
                    .unwrap_or(Type::UNKNOWN);
                node.typ = Type::list(inner_typ);
            }
            Ast::Tuple(ref items) => {
                let itypes: Lresult<TypeArgs> = items
                    .iter()
                    .enumerate()
                    .map(|(idx, i)| {
                        let klstr = i.k.map(|k| Lstr::Sref(k));
                        Ok(StrupleItem::new(
                            Type::unwrap_name(&klstr, idx),
                            i.v.typ.clone(),
                        ))
                    })
                    .collect();
                node.typ = Type::tuple(itypes?);
            }
            Ast::ConstVal(_) => {
                // leave as is
            }
            Ast::Wildcard => {} // wildcard is whatever type
            Ast::Return(_) => {
                node.typ = Type::named(Type::PATH_NORETURN);
            }
            _ => {
                // should handle matches later, but for now it's fine
            }
        }
        Ok(AstStep::Ok)
    }
}

impl<'l> fmt::Debug for TypeCheck<'l>
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "TypeCheck({})", self.local_mod.key.name)
    }
}

#[derive(Debug)]
struct RemoveExtraCode;

impl ast2::Op for RemoveExtraCode
{
    fn post(&mut self, node: &mut AstNode, _mode: AstMode) -> StepResult
    {
        match &mut *node.node {
            Ast::Block(items) => {
                match items.len() {
                    0 => {
                        *node = AstNode::void();
                    }
                    1 => {
                        *node = items.pop().unwrap();
                    }
                    _ => {} // leave as-is
                }
            }
            Ast::Let(ref mut lhs, _, ref mut rhs) => {
                if let Ast::Id(_name) = &*lhs.node {
                    // if single assignment, replace the let w/
                    // rhs assigned to lhs name
                    rhs.dst = lhs.dst;
                    *node = mem::take(rhs);
                }
            }
            _ => {} // do nothing
        };
        Ok(AstStep::Ok)
    }
}


// 1
// 2
// 3
// 4

// preop: 1, 2, 3, 4
// advance, anterior, befoe, prior, early, prelude

// postop: 4, 3, 2, 1
// after, subsequent, late, postlude

#[derive(Debug)]
pub struct Semantics
{
    pub src: AstNode,
    pub args: Vec<&'static str>,
    pub infers: HashMap<Lstr, Type>,
    pub calls: HashSet<Fref>,
}

impl Semantics
{
    pub fn new() -> Semantics
    {
        Semantics {
            src: AstNode::void(),
            args: Vec::new(),
            infers: HashMap::new(),
            calls: HashSet::new(),
        }
    }

    pub fn get_type(&self) -> &Type
    {
        &self.src.typ
    }

    pub fn compile_call(lib: &mut ProtoLib, f: &Fref) -> Lresult<Semantics>
    {
        let mut sem = Semantics::new();

        let (modname, body) = lib.take_func(&f)?;
        let proto = lib.path_proto(&modname)?;

        let func_ref = proto.find_method(&f.f).ok_or_else(|| {
            rustfail!(SEMFAIL, "cannot find func ref for {}", f,)
        })?;
        // somehow figure out generic types if necessary
        let closed = vec![];
        let ftyp = if func_ref.typ.is_generic() {
            func_ref.typ.try_func_ref()?
        } else if f.t.is_closed() {
            f.t.try_func_ref()?
        } else {
            return Err(rustfail!(
                SEMFAIL,
                "{}.{} original call type: {:?}, result call type {}",
                f.m,
                f.f,
                f.t,
                func_ref.typ
            ));
        };

        sem.args = ftyp
            .args
            .iter()
            .enumerate()
            .map(|(_i, kv)| {
                match &kv.k {
                    Lstr::Sref(name) => *name,
                    Lstr::Arc(ref name) => {
                        panic!("func arg name is not a static: {}", name);
                    }
                    _ => "missing_field",
                }
            })
            .collect();

        let mut macs = MacroApplication::new(lib, proto, &ftyp, &closed);
        let mut scope_check = ScopeCheck::new(lib, proto, &ftyp)?;
        let mut type_check = TypeCheck::new(lib, proto, &ftyp)?;
        let mut remove_extra = RemoveExtraCode;

        // This interleaves all the calls.
        // Should it be that way or do each op completely before
        // starting the next?
        let mut pipe = ast2::Pipeline::new(vec![
            &mut remove_extra,
            &mut scope_check,
            &mut macs,
            &mut type_check,
        ]);
        let mut result = lfctx!(
            ast2::walk(body, &mut pipe),
            "module": f.m.name.to_lstr(),
            "function": Lstr::Sref(f.f)
        );

        result.typ = type_check.inferred_type(&result.typ, &[])?;
        if *ftyp.result != result.typ && *ftyp.result != Type::VOID {
            return Err(rustfail!(
                SEMFAIL,
                "bad return type in {}, expected: {}, found {}",
                f,
                ftyp.result,
                result.typ,
            ));
        }

        sem.infers = type_check.infers;
        sem.calls = type_check.calls.into_iter().collect();
        sem.src = result;
        Ok(sem)
    }
}

#[cfg(test)]
mod tests
{
    use super::TypeCheck;
    use crate::leema::ast2::Ast;
    use crate::leema::loader::Interloader;
    use crate::leema::lstr::Lstr;
    use crate::leema::module::ModKey;
    use crate::leema::program;
    use crate::leema::proto::{ProtoLib, ProtoModule};
    use crate::leema::struple::StrupleItem;
    use crate::leema::val::{Fref, Type};

    use matches::assert_matches;


    fn core_program(mods: &[(&'static str, String)]) -> program::Lib
    {
        let mut loader = Interloader::default();
        for (name, src) in mods.iter() {
            loader.set_mod_txt(ModKey::from(*name), src.clone());
        }
        program::Lib::new(loader)
    }

    #[test]
    fn test_typecheck_open_local_vars()
    {
        let input = r#"
        func <sort T>:[T] :: unsorted:[T] -RUST-
        func take_names:[Str] :: with_names:[(Int Str)] -RUST-

        func main ->
            let plain := [(3, "hello"), (5, "world")]
            let sorted_tuples := sort(plain)
            let names := take_names(sorted_tuples)
        --
        "#
        .to_string();

        let mut prog = core_program(&[("/foo", input)]);
        let fref = Fref::with_modules(From::from("/foo"), "main");
        let body = prog.read_semantics(&fref).unwrap();
        assert_matches!(*body.src.node, Ast::Block(_));
    }

    #[test]
    fn test_semantics_default_imports()
    {
        // 4;[6]
        let input = r#"
        func main ->
            4 < 6
            8 > 8
            3 <= 2
            8 >= 6
            4 == 8
            9 != 7
            3 + 4
            3 - 4
            3 * 4
            5 / 6
            -8
            5 modulo 6
            True and False
            True or False
            not True
        --
        "#
        .to_string();

        let mut prog = core_program(&[("/foo", input)]);
        let fref = Fref::with_modules(From::from("/foo"), "main");
        prog.read_semantics(&fref).unwrap();
    }

    #[test]
    fn test_semantics_local_macro()
    {
        let input = r#"
        macro test_and :: a b ->
            if
            |a -> b
            |_ -> False
            --
        --

        func main ->
            test_and(True, False)
        --
        "#
        .to_string();

        let mut prog = core_program(&[("/foo", input)]);
        let fref = Fref::from(("/foo", "main"));
        let sem = prog.read_semantics(&fref).unwrap();

        assert_matches!(*sem.src.node, Ast::Ifx(_));
    }

    #[test]
    fn test_semantics_external_macro()
    {
        let input = r#"
        func main ->
            True and False
        --
        "#
        .to_string();

        let mut prog = core_program(&[("/foo", input)]);
        let fref = Fref::from(("/foo", "main"));
        let sem = prog.read_semantics(&fref).unwrap();

        assert_matches!(*sem.src.node, Ast::Ifx(_));
    }

    #[test]
    #[should_panic]
    fn test_semantics_local_scope_fail()
    {
        let input = r#"
        func main >>
            if
            |True >>
                let x := 5
            --
            x + 1
        --
        "#
        .to_string();

        let mut prog = core_program(&[("foo", input)]);
        let fref = Fref::from(("foo", "main"));
        prog.read_semantics(&fref).unwrap();
    }

    #[test]
    fn test_semantics_param_in_scope()
    {
        let input = r#"
        func inc:Int :: i:Int ->
            i + 1
        --
        "#
        .to_string();

        let mut prog = core_program(&[("/foo", input)]);
        let fref = Fref::from(("/foo", "inc"));
        let result = prog.read_semantics(&fref).unwrap();
        assert_eq!(Type::INT, result.src.typ);
    }

    #[test]
    fn test_semantics_module_scope_call()
    {
        let input = r#"
        func foo:Int -> 5 --

        func main ->
            foo() + 3
        --
        "#
        .to_string();

        let mut prog = core_program(&[("/foo", input)]);
        let fref = Fref::from(("/foo", "main"));
        prog.read_semantics(&fref).unwrap();
    }

    #[test]
    fn test_type_inferred_var()
    {
        let input = r#"
        func main >>
            let x := 8
            let y := x + 1
        --
        "#
        .to_string();

        let mut prog = core_program(&[("/foo", input)]);
        let fref = Fref::from(("/foo", "main"));
        prog.read_semantics(&fref).unwrap();
    }

    #[test]
    fn test_generic_typecall_wrongargs()
    {
        let input = r#"
        func <swap T>:(T T) :: a:T b:T ->
            (b, a)
        --

        func main ->
            <swap Str>("hello", 8)
        --
        "#
        .to_string();

        let mut prog = core_program(&[("/foo", input)]);
        let fref = Fref::from(("/foo", "main"));
        let f = prog.read_semantics(&fref).unwrap_err();
        assert_eq!("types do not match: (/core/Str != /core/Int)", f.msg.str());
        assert_eq!("semantic_failure", f.tag.str());
    }

    #[test]
    fn test_type_genericfunc_1()
    {
        let input = r#"
        func <swap A B>:(B A) :: a:A b:B ->
            (b, a)
        --

        func main ->
            swap(3, "txt")
            <swap Str Int>("hello", 8)
        --
        "#
        .to_string();

        let mut prog = core_program(&[("/foo", input)]);
        let fref = Fref::from(("/foo", "main"));
        prog.read_semantics(&fref).unwrap();
    }

    #[test]
    fn test_type_genericfuncs()
    {
        let input = r#"
        func <new_pair A B>:(A B) :: a:A b:B ->
            (a, b)
        --

        func <first A B>:A :: p:(A B) ->
            match p
            |(a, _) -> a
            --
        --

        func main ->
            let p := new_pair(4, "b")
            let f := first(p)
        --
        "#
        .to_string();

        let mut prog = core_program(&[("/foo", input)]);
        let fref = Fref::from(("/foo", "main"));
        prog.read_semantics(&fref).unwrap();
    }

    #[test]
    fn test_inferred_type_func()
    {
        let ftyp = Type::generic_f(
            vec![StrupleItem::new(Lstr::Sref("T"), Type::STR)],
            Type::open(Lstr::Sref("T")),
            vec![StrupleItem::new(
                Lstr::Sref("a"),
                Type::open(Lstr::Sref("T")),
            )],
        );
        let ftyp_args = ftyp.type_args();
        let expected = Type::generic_f(
            vec![StrupleItem::new(Lstr::Sref("T"), Type::STR)],
            Type::STR,
            vec![StrupleItem::new(Lstr::Sref("a"), Type::STR)],
        );
        let lib = ProtoLib::new();
        let proto = ProtoModule::new(ModKey::from("foo"), "").unwrap();
        let mainf = Type::f(Type::VOID, vec![]);
        let mainfref = mainf.func_ref().unwrap();
        let type_check = TypeCheck::new(&lib, &proto, &mainfref).unwrap();
        let inferred = type_check.inferred_type(&ftyp, ftyp_args).unwrap();
        assert_eq!(expected, inferred);
    }

    #[test]
    fn test_semantics_match_with_fail()
    {
        let input = r#"
        func safediv:Int :: x:Int y:Int ->
            match
            |(_, 0) -> fail(#divide_by_0, "cannot divide by zero")
            |(a, b) -> a + b
            --
        --
        "#
        .to_string();

        let mut prog = core_program(&[("/foo", input)]);
        let fref = Fref::from(("/foo", "safediv"));
        prog.read_semantics(&fref).unwrap();
    }

    #[test]
    #[should_panic]
    fn test_semantics_module_scope_fail()
    {
        let input = r#"func main -> bar() --"#.to_string();

        let mut prog = core_program(&[("/foo", input)]);
        let fref = Fref::from(("/foo", "main"));
        prog.read_semantics(&fref).unwrap();
    }

    #[test]
    fn test_semantics_external_scope_call()
    {
        let food_input = r#"
        func tacos:Int -> 3 --
        func enchiladas:Int -> 2 --
        "#
        .to_string();

        let app_input = r#"
        import /food
        import /food.enchiladas

        func main ->
            food.tacos() + enchiladas()
        --
        "#
        .to_string();

        let mut prog =
            core_program(&[("/food", food_input), ("/app", app_input)]);
        let fref = Fref::from(("/app", "main"));
        prog.read_semantics(&fref).unwrap();
    }

    #[test]
    fn test_semantics_three_level_call()
    {
        let foo_src = r#"func taco:Int -> 2 --"#.to_string();

        let bar_src = r#"
        export foo.taco

        func inc:Int :: i:Int -> i + 1 --
        "#
        .to_string();

        let baz_src = r#"
        import /bar
        import /bar/foo.taco

        func main ->
            bar.inc(taco()) + 6
        --
        "#
        .to_string();

        let mut prog = core_program(&[
            ("/bar/foo", foo_src),
            ("/bar", bar_src),
            ("/baz", baz_src),
        ]);

        let fref = Fref::from(("/baz", "main"));
        prog.read_semantics(&fref).unwrap();
    }

    #[test]
    fn test_semantics_exported_call()
    {
        let foo_input = r#"func bar:Int -> 3 --"#.to_string();

        let baz_input = r#"
        import /foo.bar

        func main ->
            bar() + 6
        --
        "#
        .to_string();

        let mut prog =
            core_program(&[("/foo", foo_input), ("/baz", baz_input)]);
        let fref = Fref::from(("/baz", "main"));
        prog.read_semantics(&fref).unwrap();
    }

    #[test]
    #[should_panic]
    fn test_semantics_external_scope_fail()
    {
        let foo_input = r#"func bak >> 3 --"#.to_string();

        let app_input = r#"
        func main >>
            foo::bar() + 6
        --
        "#
        .to_string();

        let mut prog = core_program(&[("foo", foo_input), ("app", app_input)]);
        let fref = Fref::from(("app", "main"));
        prog.read_semantics(&fref).unwrap();
    }

    #[test]
    fn struct_field_expansion()
    {
        let baz_input = r#"
        datatype Foo ::
            x:Int
            y:Str
        --

        func main ->
            let flds := *Foo
        --
        "#
        .to_string();

        let mut prog = core_program(&[("/baz", baz_input)]);
        let fref = Fref::from(("/baz", "main"));
        prog.read_semantics(&fref).unwrap();
    }

    #[test]
    fn check_type_alias_without_import()
    {
        let input_a = r#"
        import /b
        import /c
        import /d

        func main ->
            b.consume_t(b.produce_a())
            b.consume_a(b.produce_t())
        --
        "#
        .to_string();

        let input_b = r#"
        import /c
        import /d

        func produce_a:c.A ->
            A(1, "a")
        --

        func produce_t:d.T ->
            T(1, "a")
        --

        func consume_a :: a:c.A ->
        --

        func consume_t :: t:d.T ->
        --
        "#
        .to_string();

        let input_c = r#"
        import /d

        datatype A := d.T
        "#
        .to_string();

        let input_d = r#"
        datatype T :: Int Str --
        "#
        .to_string();

        let mut prog = core_program(&[
            ("/a", input_a),
            ("/b", input_b),
            ("/c", input_c),
            ("/d", input_d),
        ]);
        prog.read_semantics(&Fref::from(("/a", "main"))).unwrap();
    }

    #[test]
    fn test_semantics_undefined_variable()
    {
        let baz_input = r#"
        func main >>
            let a := blah
            "a is $a\n"
        --
        "#
        .to_string();

        let mut prog = core_program(&[("/baz", baz_input)]);
        let fref = Fref::from(("/baz", "main"));
        let f = prog.read_semantics(&fref).unwrap_err();
        assert_eq!("semantic_failure", f.tag.str());
    }

    #[test]
    #[should_panic]
    fn test_semantics_external_scope_no_module()
    {
        let baz_input = r#"
        func main >>
            foo::bar() + 6
        --
        "#
        .to_string();

        let mut prog = core_program(&[("baz", baz_input)]);
        let fref = Fref::from(("baz", "main"));
        prog.read_semantics(&fref).unwrap();
    }

    #[test]
    #[should_panic]
    fn test_type_fail_func_types()
    {
        let input = r#"
        func inc i:Int :Int >> i + 1 --
        func main >> inc("5") --
        "#
        .to_string();

        let mut prog = core_program(&[("foo", input)]);
        let fref = Fref::from(("foo", "main"));
        prog.read_semantics(&fref).unwrap();
    }

    #[test]
    #[should_panic]
    fn test_type_fail_too_few_args()
    {
        let input = r#"
        func mult i:Int j:Int :Int >> i * j --
        func main >> mult(7) --
        "#
        .to_string();

        let mut prog = core_program(&[("foo", input)]);
        let fref = Fref::from(("foo", "main"));
        prog.read_semantics(&fref).unwrap();
    }

    #[test]
    #[should_panic]
    fn test_type_fail_too_many_args()
    {
        let input = r#"
        func inc i:Int / Int >> i + 1 --
        func main >> inc(2, 7) --
        "#
        .to_string();

        let mut prog = core_program(&[("foo", input)]);
        let fref = Fref::from(("foo", "main"));
        prog.read_semantics(&fref).unwrap();
    }

    #[test]
    #[should_panic]
    fn test_type_fail_bad_return_type()
    {
        let input = r#"
        func inc i:Int :Int >> "hello" --
        "#
        .to_string();

        let mut prog = core_program(&[("foo", input)]);
        let fref = Fref::from(("foo", "inc"));
        prog.read_semantics(&fref).unwrap();
    }

    #[test]
    #[should_panic]
    fn test_compile_mismatched_if_branches()
    {
        let input = r#"
        func factf i:Int :Int >>
            if
            |i == 1 >> "text"
            |else >> i * factf(i-1)
            --
        --
        "#
        .to_string();

        let mut prog = core_program(&[("foo", input)]);
        let fref = Fref::from(("foo", "factf"));
        prog.read_semantics(&fref).unwrap();
    }

    #[test]
    fn test_typefailure_undefined()
    {
        let input = r#"
        func inc i:Int /Inth >> i + 1 --
        "#
        .to_string();

        let mut prog = core_program(&[("/foo", input)]);
        let fref = Fref::from(("/foo", "inc"));
        let err = prog.read_semantics(&fref);
        assert_matches!(err, Err(_));
        err.unwrap_err();
    }

    /*
    // semantics tests copied over from inter.rs
    // these are cases that don't pass yet, but should be made to pass later

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

            func main >>
                let d := Dog
                let c := Cat(3)
                let m := Mouse(9, \"red\")
            --
            "
    }

    #[test]
    fn test_compile_match_existing_var()
    {
        let input = "
            func foo /Int >>
                let a := 5
                let b := 8
                match b
                |a -> a + 1
                |_ -> a - 1
                --
            --
            "
    }
    // */
}
