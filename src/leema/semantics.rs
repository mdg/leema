use crate::leema::ast2::{
    self, Ast, AstMode, AstNode, AstResult, AstStep, Loc, StepResult, Xlist,
};
use crate::leema::failure::{self, Failure, Lresult};
use crate::leema::inter::Blockstack;
use crate::leema::lstr::Lstr;
use crate::leema::module::CanonicalMod;
use crate::leema::proto::{ProtoModule, StructFieldMap};
use crate::leema::struple::{self, Struple2, StrupleItem, StrupleKV};
use crate::leema::val::{
    Fref, FuncType, GenericTypeSlice, GenericTypes, Type, Val,
};

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
    local: &'l ProtoModule,
    ftype: &'l FuncType,
    closed: &'l GenericTypes,
}

impl<'l> MacroApplication<'l>
{
    pub fn new(
        local: &'l ProtoModule,
        ftype: &'l FuncType,
        closed: &'l GenericTypes,
    ) -> MacroApplication<'l>
    {
        MacroApplication {
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
                let optmac = match &mut *callid.node {
                    Ast::Id(macroname) => self.find_macro_1(macroname)?,
                    _ => None,
                };
                if let Some(mac) = optmac {
                    *node = Self::apply_macro(mac, callid.loc, args)?;
                    return Ok(AstStep::Rewrite);
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
                if let Some((_, ctype)) = struple::find(self.closed, &id) {
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
                        if let Some(Lstr::Sref(argname)) = arg.k {
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
    fn expand_args(args: &mut Xlist) -> StepResult
    {
        let (expands, extra_items) =
            args.iter().fold((false, 0), |(matches, extra_items), a| {
                if let Ast::Op1("*", expansion_node) = &*a.v.node {
                    match &*expansion_node.node {
                        Ast::Tuple(expansion) => {
                            (true, extra_items + expansion.len())
                        }
                        what => {
                            panic!("expected tuple, found {:?}", what);
                        }
                    }
                } else {
                    (matches, extra_items)
                }
            });
        if !expands {
            return Ok(AstStep::Ok);
        }

        let mut new_args = Vec::with_capacity(args.len() + extra_items);
        let tmp_args = mem::take(args);
        for a in tmp_args.into_iter() {
            if let Ast::Op1("*", expansion_node) = *a.v.node {
                match *expansion_node.node {
                    Ast::Tuple(mut expansion) => {
                        new_args.append(&mut expansion);
                    }
                    what => {
                        panic!("expected tuple, found {:?}", what);
                    }
                }
            } else {
                // not an expansion, just pass it through
                new_args.push(a);
            }
        }
        mem::swap(args, &mut new_args);
        Ok(AstStep::Ok)
    }
}

impl<'a> ast2::Op for MacroReplacement<'a>
{
    fn pre(&mut self, node: &mut AstNode, _mode: AstMode) -> StepResult
    {
        if let Ast::Id(idname) = &*node.node {
            if let Some(newval) = self.arg_map.get(idname) {
                *node = (*newval).clone();
                return Ok(AstStep::Rewrite);
            }
        }
        // if not replacing the id, replace the location of the call
        // so everything in the macro body traces back to the macro name
        node.loc = self.loc;
        Ok(AstStep::Ok)
    }

    fn post(&mut self, node: &mut AstNode, mode: AstMode) -> StepResult
    {
        match &mut *node.node {
            Ast::Call(_callx, ref mut args) if mode == AstMode::Value => {
                Self::expand_args(args)?;
            }
            _ => {} // nothing
        }
        // if not replacing the id, replace the location of the call
        // so everything in the macro body traces back to the macro name
        node.loc = self.loc;
        Ok(AstStep::Ok)
    }
}

struct ScopeCheck<'p>
{
    blocks: Blockstack,
    local_mod: &'p ProtoModule,
}

impl<'p> ScopeCheck<'p>
{
    pub fn new(
        ftyp: &'p FuncType,
        local_mod: &'p ProtoModule,
    ) -> Lresult<ScopeCheck<'p>>
    {
        let mut args: Vec<&'static str> = Vec::new();
        for a in ftyp.args.iter() {
            match a.k {
                Some(Lstr::Sref(arg)) => {
                    args.push(arg);
                }
                _ => {
                    panic!("function argument missing name: {:?}", ftyp);
                }
            }
        }
        Ok(ScopeCheck {
            blocks: Blockstack::with_args(args),
            local_mod,
        })
    }
}

impl<'p> ast2::Op for ScopeCheck<'p>
{
    fn pre(&mut self, node: &mut AstNode, mode: AstMode) -> StepResult
    {
        match &*node.node {
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
                        Type::Kind,
                    );
                }
            }
            Ast::Id(id) => {
                if let Some(r) = self.blocks.var_in_scope(id) {
                    node.dst = r;
                } else if let Some(me) = self.local_mod.find_modelem(id) {
                    node.replace((*me.node).clone(), me.typ.clone());
                    return Ok(AstStep::Rewrite);
                } else {
                    return Err(rustfail!(
                        SEMFAIL,
                        "var not in scope: {} @ {:?}",
                        id,
                        node.loc,
                    ));
                }
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
    local_mod: &'p ProtoModule,
    fields: &'p StructFieldMap,
    result: Type,
    vartypes: HashMap<&'static str, Type>,
    infers: HashMap<Lstr, Type>,
    calls: Vec<Fref>,
}

impl<'p> TypeCheck<'p>
{
    pub fn new(
        local_mod: &'p ProtoModule,
        ftyp: &'p FuncType,
        fields: &'p StructFieldMap,
    ) -> Lresult<TypeCheck<'p>>
    {
        let mut check = TypeCheck {
            local_mod,
            fields,
            result: Type::VOID,
            vartypes: HashMap::new(),
            infers: HashMap::new(),
            calls: vec![],
        };

        for arg in ftyp.args.iter() {
            let argname = arg.k.as_ref().unwrap().sref()?;
            check.vartypes.insert(argname, arg.v.clone());
        }
        check.result = (*ftyp.result).clone();
        Ok(check)
    }

    pub fn inferred_local(&self, local_tvar: &Lstr) -> Type
    {
        self.infers
            .get(local_tvar.str())
            .map(|t| t.clone())
            .unwrap_or(Type::LocalVar(local_tvar.clone()))
    }

    pub fn inferred_type(
        &self,
        t: &Type,
        opens: &GenericTypeSlice,
    ) -> Lresult<Type>
    {
        let newt = match t {
            Type::OpenVar(v) => {
                struple::find(opens, &v)
                    .map(|(_, item)| item.clone())
                    .unwrap_or(Type::OpenVar(v))
            }
            Type::LocalVar(v) => self.inferred_local(&v),
            Type::Tuple(items) => {
                let mitems =
                    struple::map_v(items, |it| self.inferred_type(it, opens))?;
                Type::Tuple(mitems)
            }
            Type::StrictList(inner) => {
                let inferred = self.inferred_type(inner, opens)?;
                Type::StrictList(Box::new(inferred))
            }
            Type::Func(ftyp) => {
                let iargs = struple::map_v(&ftyp.args, |a| {
                    self.inferred_type(a, opens)
                })?;
                let iresult = self.inferred_type(&ftyp.result, opens)?;
                Type::Func(FuncType::new(iargs, iresult))
            }
            Type::Generic(_open, inner, type_args) => {
                let inner2 = self.inferred_type(&*inner, opens)?;
                let typargs2 = struple::map_v(type_args, |tv| {
                    self.inferred_type(tv, opens)
                })?;
                let open2 = typargs2.iter().any(|ta| ta.v.is_open());
                Type::Generic(open2, Box::new(inner2), typargs2)
            }
            Type::Variant(inner, var) => {
                let inner2 = self.inferred_type(&*inner, opens)?;
                Type::Variant(Box::new(inner2), var)
            }
            Type::User(_, _) => t.clone(),
            _ => t.clone(),
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
        opens: &mut StrupleKV<&'static str, Type>,
    ) -> Lresult<Type>
    {
        match (t0, t1) {
            (t0, Type::Unknown) => {
                if t0.is_open() {
                    eprintln!("yikes, type is open {}", t0);
                }
                Ok(t0.clone())
            }
            (Type::Unknown, t1) => {
                if t1.is_open() {
                    eprintln!("yikes, type is open {}", t1);
                }
                Ok(t1.clone())
            }
            (t0, fail) if fail.is_failure() => {
                if t0.is_open() {
                    eprintln!("yikes, type is open {}", t0);
                }
                Ok(t0.clone())
            }
            (fail, t1) if fail.is_failure() => {
                if t1.is_open() {
                    eprintln!("yikes, type is open {}", t1);
                }
                Ok(t1.clone())
            }
            (Type::Tuple(i0), Type::Tuple(i1)) => {
                let im: Lresult<Struple2<Type>>;
                im = i0
                    .iter()
                    .zip(i1.iter())
                    .map(|iz| {
                        let k = iz.0.k.clone();
                        let v = ltry!(self.match_type(&iz.0.v, &iz.1.v, opens));
                        Ok(StrupleItem::new(k, v))
                    })
                    .collect();
                Ok(Type::Tuple(im?))
            }
            (Type::StrictList(i0), Type::StrictList(i1)) => {
                let it = ltry!(self.match_type(i0, i1, opens));
                Ok(Type::StrictList(Box::new(it)))
            }
            (Type::StrictList(_), Type::LocalVar(v1)) => {
                let local_inner = Type::inner(v1, 0);
                let il1 = Type::StrictList(Box::new(local_inner));
                let mlist = ltry!(self.match_type(t0, &il1, opens));
                self.infer_type(v1, &mlist, opens)
            }
            (Type::LocalVar(_), Type::StrictList(_)) => {
                self.match_type(t1, t0, opens)
            }
            (Type::Variant(inner0, _), Type::Variant(inner1, _)) => {
                self.match_type(inner0, inner1, opens)
            }
            (Type::Variant(inner0, _), _) => self.match_type(inner0, t1, opens),
            (_, Type::Variant(inner1, _)) => self.match_type(t0, inner1, opens),
            (Type::OpenVar(v0), t1) => {
                lfailoc!(self.close_generic(v0, t1, opens))
            }
            (t0, Type::OpenVar(v1)) => {
                lfailoc!(self.close_generic(v1, t0, opens))
            }
            (Type::LocalVar(v0), Type::LocalVar(v1)) if v0 == v1 => {
                Ok(self.inferred_local(v0))
            }
            (Type::LocalVar(v0), Type::LocalVar(v1)) if v0 < v1 => {
                lfailoc!(self.infer_type(v0, t1, opens))
            }
            (Type::LocalVar(v0), Type::LocalVar(v1)) if v1 < v0 => {
                lfailoc!(self.infer_type(v1, t0, opens))
            }
            (Type::LocalVar(v0), t1) => {
                lfailoc!(self.infer_type(v0, t1, opens))
            }
            (t0, Type::LocalVar(v1)) => {
                lfailoc!(self.infer_type(v1, t0, opens))
            }
            (t0, t1) => {
                if t0 != t1 {
                    Err(rustfail!(
                        SEMFAIL,
                        "types do not match: ({:#?} != {})",
                        t0,
                        t1,
                    ))
                } else {
                    Ok(t0.clone())
                }
            }
        }
    }

    pub fn infer_type(
        &mut self,
        var: &Lstr,
        t: &Type,
        opens: &mut StrupleKV<&'static str, Type>,
    ) -> Lresult<Type>
    {
        if self.infers.contains_key(var) {
            let var_type = self.inferred_local(var);
            if var_type == *t {
                Ok(var_type)
            } else {
                lfailoc!(self.match_type(&var_type, t, opens)).map_err(|f| {
                    f.add_context(lstrf!(
                        "inferring type var {} as {}",
                        var,
                        var_type
                    ))
                })
            }
        } else {
            self.infers.insert(var.clone(), t.clone());
            Ok(t.clone())
        }
    }

    pub fn close_generic(
        &mut self,
        var: &'static str,
        t: &Type,
        opens: &mut StrupleKV<&'static str, Type>,
    ) -> Lresult<Type>
    {
        let open_idx = if let Some((found, _)) = struple::find(opens, &var) {
            found
        } else {
            return Err(rustfail!(
                TYPEFAIL,
                "open type var {:?} not found in {:?} for {:?}",
                var,
                opens,
                t,
            ));
        };

        match &opens[open_idx].v {
            Type::Unknown => {
                // newly defined type, set it in opens
                opens[open_idx].v = t.clone();
                Ok(t.clone())
            }
            Type::OpenVar(open_var) if **open_var == *var => {
                // newly defined type, set it in opens
                opens[open_idx].v = t.clone();
                Ok(t.clone())
            }
            Type::OpenVar(some_other_var) => {
                panic!("unexpected var: {:?}", some_other_var);
            }
            matched_t if *matched_t == *t => {
                // already the same type, good
                return Ok(t.clone());
            }
            _ => {
                // different type, run a match and then assign the result
                // this upgrades inference local vars to concrete types
                let old_type = opens[open_idx].v.clone();
                let new_type = ltry!(self.match_type(&old_type, t, opens));
                opens[open_idx].v = new_type.clone();
                Ok(new_type)
            }
        }
    }

    pub fn apply_typecall(
        &mut self,
        calltype: &mut Type,
        args: &mut ast2::Xlist,
    ) -> Lresult<Type>
    {
        match calltype {
            Type::Generic(gopen @ true, ref mut inner, ref mut targs) => {
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
                        _ => {
                            let t = self.local_mod.ast_to_type(
                                &self.local_mod.key.name,
                                &a.1.v,
                                &[],
                            )?;
                            a.0.v = t;
                            panic!("unexpected type setting");
                        }
                    }
                }
                let t = self.inferred_type(&inner, &targs)?;
                *inner = Box::new(t);
                *gopen = false;
            }
            _ => {
                return Err(rustfail!(
                    TYPEFAIL,
                    "not a function call {:#?}",
                    calltype,
                ));
            }
        }
        Ok(calltype.clone())
    }

    pub fn apply_typecall2(
        calltype: &mut AstNode,
        typearg: &AstNode,
    ) -> Lresult<()>
    {
        // let new_node = mem::take(a);
        // *node = new_node;
        match (&mut *calltype.node, &mut calltype.typ) {
            (_, Type::Generic(false, _, _)) => {
                Err(rustfail!(
                    "compile_error",
                    "generic type is closed: {:?}",
                    calltype,
                ))
            }
            (
                Ast::ConstVal(Val::Call(ref mut fref, _args)),
                Type::Generic(ref mut open, ref mut inner, ref mut targs),
            ) => {
                // open is true b/c of pattern above
                let (repl_idx, repl_id) = Self::first_open_var(targs)?;
                match &*typearg.node {
                    Ast::ConstVal(Val::Type(t)) => {
                        targs[repl_idx].v = t.clone();
                        **inner = inner.replace_openvar(repl_id, t)?;
                        *open = targs.iter().any(|a| a.v.is_open());
                        fref.t =
                            Type::Generic(*open, inner.clone(), targs.clone());
                        Ok(())
                    }
                    _ => Ok(()),
                }
            }
            _ => {
                Err(rustfail!("compile_error", "not generic: {:?}", calltype,))
            }
        }
    }

    fn first_open_var(
        typeargs: &StrupleKV<&'static str, Type>,
    ) -> Lresult<(usize, &'static str)>
    {
        for (idx, i) in typeargs.iter().enumerate() {
            match i.v {
                Type::OpenVar(_) | Type::Unknown => {
                    return Ok((idx, i.k));
                }
                _ => {} // not open, keep looking
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
        match calltype {
            Type::Func(inner_ftyp) => {
                let mut opens = vec![];
                self.match_argtypes(inner_ftyp, args, &mut opens)
            }
            Type::Generic(false, ref mut inner, _) => {
                self.applied_call_type(&mut *inner, args)
            }
            Type::Generic(gopen @ true, ref mut open_ftyp, ref mut opens) => {
                if let Type::Func(inner_ftyp) = &mut **open_ftyp {
                    // figure out arg types
                    let result =
                        self.match_argtypes(inner_ftyp, args, opens)?;
                    if result.is_open() {
                        return Err(rustfail!(
                            TYPEFAIL,
                            "function should not be open: {}",
                            result,
                        ));
                    }
                    *gopen = false;
                    Ok(result)
                } else {
                    return Err(rustfail!(
                        SEMFAIL,
                        "generic call type is not a function: {:?}",
                        calltype,
                    ));
                }
            }
            _ => {
                return Err(rustfail!(
                    SEMFAIL,
                    "call type is not a function: {:?}",
                    calltype,
                ));
            }
        }
    }

    fn match_argtypes(
        &mut self,
        ftyp: &mut FuncType,
        args: &mut ast2::Xlist,
        opens: &mut StrupleKV<&'static str, Type>,
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
                .match_type(&arg.0.v, &arg.1.v.typ, opens)
                .map_err(|f| {
                    f.add_context(lstrf!(
                        "function param: {:?}, expected {}, found {} column:{}",
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

        ftyp.result = Box::new(self.inferred_type(&ftyp.result, &opens)?);
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
                    let tvar = Type::LocalVar(Lstr::Sref(id));
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
                node.typ = Type::Unknown;
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
            Ast::Call(ref mut callx, ref mut args) => {
                if let Ast::ConstVal(ref mut ccv) = &mut *callx.node {
                    if let Val::Construct(cfref) = ccv {
                        // check for void constructors,
                        // do different stuff w/ tem
                        if args.len() == 1
                            && *args.first().unwrap().v.node == Ast::VOID
                        {
                            let new_val = match &cfref.t {
                                Type::Func(ft) => {
                                    let args =
                                        struple::map_v(&ft.args, |_| {
                                            Ok(Val::VOID)
                                        })?;
                                    let typ = (*ft.result).clone();
                                    Val::Struct(typ, args)
                                }
                                Type::Generic(_, _, _) => {
                                    panic!("constructors unimplemented for generics");
                                }
                                _ => {
                                    panic!("this doesn't make sense");
                                }
                            };
                            node.typ = new_val.get_type();
                            *node.node = Ast::ConstVal(new_val);
                            return Ok(AstStep::Rewrite);
                        }

                        // reassign the construct as a call
                        let result: Lresult<Val> = From::from(&*cfref);
                        *ccv = result?;
                    }

                    if let Val::Call(ref mut fref, _argvals) = ccv {
                        // an optimization here might be to iterate over
                        // ast args and initialize any constants
                        // actually better might be to stop having
                        // the args in the Val::Call const
                        fref.t = callx.typ.clone();

                        let call_result = ltry!(self
                            .applied_call_type(&mut callx.typ, args)
                            .map_err(|f| {
                                f.add_context(lstrf!("for function: {}", fref,))
                            }));
                        fref.t = callx.typ.clone();
                        self.calls.push(fref.clone());
                        node.typ = call_result;
                    }
                } else {
                    if callx.typ.is_user() {
                        let copy_typ = callx.typ.clone();
                        let base = mem::take(callx);
                        let args_copy = mem::take(args);
                        node.replace(
                            Ast::CopyAndSet(base, args_copy),
                            copy_typ,
                        );
                        return Ok(AstStep::Rewrite);
                    } else {
                        return Err(rustfail!(
                            SEMFAIL,
                            "unexpected call expression: {:?}",
                            callx,
                        ));
                    }
                }
            }
            Ast::Op2(".", a, b) => {
                match (&*a.node, &*b.node) {
                    (Ast::Module(tup), Ast::Id(name)) => {
                        match struple::find_str(&tup[..], name) {
                            Some((_i, elem)) => {
                                node.typ = elem.typ.clone();
                                *node.node = (*elem.node).clone();
                            }
                            None => {
                                return Err(rustfail!(
                                    "compile_error",
                                    "no {} found in module {:?}",
                                    name,
                                    tup,
                                ));
                            }
                        }
                    }
                    (Ast::Tuple(tup), Ast::Id(name)) => {
                        match struple::find_str(&tup[..], name) {
                            Some((_i, elem)) => {
                                node.typ = elem.typ.clone();
                                *node.node = (*elem.node).clone();
                            }
                            None => {
                                return Err(rustfail!(
                                    "compile_error",
                                    "no {} found in {:?}",
                                    name,
                                    tup,
                                ));
                            }
                        }
                    }
                    (_r, Ast::Id(f)) => {
                        match &a.typ {
                            Type::User(tmod, tname) => {
                                let (_styp, flds) = self
                                    .fields
                                    .get(&(
                                        CanonicalMod(tmod.canonical.clone()),
                                        *tname,
                                    ))
                                    .expect("no struct fields found");
                                match struple::find_lstr(&flds[..], f) {
                                    Some((fld_idx, _)) => {
                                        *b.node = Ast::ConstVal(Val::Int(
                                            fld_idx as i64,
                                        ));
                                    }
                                    None => {
                                        return Err(Failure::static_leema(
                                            failure::Mode::CompileFailure,
                                            lstrf!("type has no field: {}", f),
                                            self.local_mod.key.name.0.clone(),
                                            node.loc.lineno,
                                        ));
                                    }
                                }
                            }
                            _ => {
                                return Err(Failure::static_leema(
                                    failure::Mode::CompileFailure,
                                    lstrf!("type has no fields: {}", a.typ),
                                    self.local_mod.key.name.0.clone(),
                                    node.loc.lineno,
                                ));
                            }
                        }
                    }
                    (_r, f) => {
                        panic!("unsupported field name: {:#?}", f);
                    }
                }
            }
            Ast::Op2("'", ref mut a, b) => {
                Self::apply_typecall2(a, &b)?;
                let new_node = mem::take(a);
                *node = new_node;
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
                    .unwrap_or(Type::Unknown);
                node.typ = Type::StrictList(Box::new(inner_typ));
            }
            Ast::Tuple(ref items) => {
                let itypes: Lresult<Struple2<Type>> = items
                    .iter()
                    .map(|i| {
                        Ok(StrupleItem::new(
                            i.k.map(|k| Lstr::Sref(k)),
                            i.v.typ.clone(),
                        ))
                    })
                    .collect();
                node.typ = Type::Tuple(itypes?);
            }
            Ast::ConstVal(_) => {
                // leave as is
            }
            Ast::Wildcard => {} // wildcard is whatever type
            Ast::Module(_) => {}
            Ast::Return(_) => {
                node.typ = Type::NO_RETURN;
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

    pub fn compile_call(
        proto: &mut ProtoModule,
        f: &Fref,
        fields: &StructFieldMap,
    ) -> Lresult<Semantics>
    {
        let mut sem = Semantics::new();

        let func_ast = proto.pop_func(&f.f);
        let (_args, body) = func_ast.ok_or_else(|| {
            rustfail!(SEMFAIL, "ast is empty for function {}", f,)
        })?;

        let func_ref = proto.find_modelem(&f.f).ok_or_else(|| {
            rustfail!(SEMFAIL, "cannot find func ref for {}", f,)
        })?;
        let closed;
        let ftyp: &FuncType = match (&func_ref.typ, &f.t) {
            (Type::Func(ref ft1), _) => {
                closed = vec![];
                ft1
            }
            (
                Type::Generic(true, _ft1, _open),
                Type::Generic(false, ref ft2, ref iclosed),
            ) => {
                match &**ft2 {
                    // take the closed version that has real types
                    Type::Func(ref ift2) => {
                        closed = iclosed.clone();
                        ift2
                    }
                    _ => {
                        return Err(rustfail!(
                            SEMFAIL,
                            "open generic function is not a function: {}",
                            ft2,
                        ));
                    }
                }
            }
            _ => {
                return Err(rustfail!(
                    SEMFAIL,
                    "{}.{} original call type: {:?}, fields? {:?}, result call type {:?}",
                    f.m,
                    f.f,
                    f.t,
                    fields,
                    func_ref.typ
                ));
            }
        };

        sem.args = ftyp
            .args
            .iter()
            .enumerate()
            .map(|(_i, kv)| {
                match &kv.k {
                    Some(Lstr::Sref(name)) => *name,
                    Some(Lstr::Arc(ref name)) => {
                        panic!("func arg name is not a static: {}", name);
                    }
                    _ => "missing_field",
                }
            })
            .collect();

        let mut macs = MacroApplication::new(proto, ftyp, &closed);
        let mut scope_check = ScopeCheck::new(ftyp, proto)?;
        let mut type_check = TypeCheck::new(proto, ftyp, fields)?;
        let mut remove_extra = RemoveExtraCode;

        let mut pipe = ast2::Pipeline::new(vec![
            &mut remove_extra,
            &mut scope_check,
            &mut macs,
            &mut type_check,
        ]);
        let mut result = ltry!(ast2::walk(body, &mut pipe).map_err(|e| {
            e.add_context(lstrf!("function: {}.{}", f.m.name, f.f))
        }));

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
    use crate::leema::ast2::Ast;
    use crate::leema::loader::Interloader;
    use crate::leema::module::ModKey;
    use crate::leema::program;
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
        assert_eq!("types do not match: (/core.Str != /core.Int)", f.msg.str());
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
