use crate::leema::ast2::{self, Ast, AstNode, AstResult, Loc, Xlist};
use crate::leema::failure::Lresult;
use crate::leema::inter::{Blockstack, LocalType};
use crate::leema::lstr::Lstr;
use crate::leema::proto::{ProtoLib, ProtoModule};
use crate::leema::struple::{self, StrupleItem, StrupleKV};
use crate::leema::val::{Fref, FuncType, Type, Val};

use std::collections::HashMap;
use std::fmt;


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

pub enum SemanticAction
{
    Keep(AstNode),
    Rewrite(AstNode),
    Remove,
}

pub type SemanticResult = Lresult<SemanticAction>;

pub trait SemanticOp: fmt::Debug
{
    fn f(&mut self, node: AstNode) -> SemanticResult
    {
        Ok(SemanticAction::Keep(node))
    }
    fn pre(&mut self, node: AstNode) -> SemanticResult
    {
        Ok(SemanticAction::Keep(node))
    }
    fn post(&mut self, node: AstNode) -> SemanticResult
    {
        Ok(SemanticAction::Keep(node))
    }

    fn set_pattern(&mut self, _is_pattern: bool) {}
}

#[derive(Debug)]
struct SemanticPipeline<'p>
{
    ops: Vec<&'p mut SemanticOp>,
}

impl<'p> SemanticOp for SemanticPipeline<'p>
{
    fn pre(&mut self, mut node: AstNode) -> SemanticResult
    {
        loop {
            let mut do_loop = false;
            for op in self.ops.iter_mut() {
                match op.pre(node)? {
                    SemanticAction::Keep(knode) => {
                        node = knode;
                    }
                    SemanticAction::Rewrite(rnode) => {
                        node = rnode;
                        do_loop = true;
                        break;
                    }
                    SemanticAction::Remove => {
                        return Ok(SemanticAction::Remove);
                    }
                }
            }
            if !do_loop {
                break;
            }
        }

        Ok(SemanticAction::Keep(node))
    }

    fn post(&mut self, mut node: AstNode) -> SemanticResult
    {
        loop {
            let mut do_loop = false;
            for op in self.ops.iter_mut().rev() {
                match op.post(node)? {
                    SemanticAction::Keep(knode) => {
                        node = knode;
                    }
                    SemanticAction::Rewrite(rnode) => {
                        node = rnode;
                        do_loop = true;
                        break;
                    }
                    SemanticAction::Remove => {
                        return Ok(SemanticAction::Remove);
                    }
                }
            }
            if !do_loop {
                break;
            }
        }

        Ok(SemanticAction::Keep(node))
    }
}

struct MacroApplication<'l>
{
    local: &'l ProtoModule,
    proto: &'l ProtoLib,
    ftype: &'l FuncType,
}

impl<'l> MacroApplication<'l>
{
    fn apply_macro(
        mac: &Ast,
        loc: Loc,
        args: StrupleKV<Option<&'static str>, AstNode>,
    ) -> AstResult
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
                    "Too many arguments passed to macro {}, expected {}",
                    macro_name,
                    a,
                ));
            }
            (a, b) if a > b => {
                return Err(rustfail!(
                    SEMFAIL,
                    "Too few arguments passed to macro {}, expected {}",
                    macro_name,
                    a
                ));
            }
            _ => {
                // a == b. cool, proceed
            }
        }

        let mut arg_map: HashMap<&'static str, AstNode> = HashMap::new();
        for (n, arg_val) in arg_names.iter().zip(args.into_iter()) {
            arg_map.insert(n, arg_val.v);
        }
        vout!("replace_ids({:?})\n", arg_map);
        let mut macro_replace = MacroReplacement { arg_map, loc };
        Semantics::walk(&mut macro_replace, body.clone())
    }

    fn op_to_call(
        module: &'static str,
        func: &'static str,
        a: AstNode,
        b: AstNode,
        loc: Loc,
    ) -> AstNode
    {
        let callx = AstNode::new(Ast::Id2(Lstr::Sref(module), func), loc);
        let args: StrupleKV<Option<&'static str>, AstNode> =
            struple::new_tuple2(a, b);
            /*
            vec![
                StrupleItem::new(None, a),
                StrupleItem::new(None, b),
            ];
            */
        AstNode::new(Ast::Call(callx, args), loc)
    }
}

impl<'l> SemanticOp for MacroApplication<'l>
{
    fn pre(&mut self, node: AstNode) -> SemanticResult
    {
        match *node.node {
            Ast::Call(callid, args) => {
                let optmac = match *callid.node {
                    Ast::Id1(macroname) => self.local.find_macro(macroname),
                    Ast::Id2(ref modname, ref macroname) => {
                        self.proto.get(&modname)?.find_macro(macroname)
                    }
                    _ => None,
                };
                match optmac {
                    Some(mac) => {
                        let result = Self::apply_macro(mac, callid.loc, args)?;
                        Ok(SemanticAction::Rewrite(result))
                    }
                    None => {
                        let node2 =
                            AstNode::new(Ast::Call(callid, args), node.loc);
                        Ok(SemanticAction::Keep(node2))
                    }
                }
            }
            Ast::Op2("+", a, b) => {
                let call =
                    Self::op_to_call("prefab", "int_add", a, b, node.loc);
                Ok(SemanticAction::Rewrite(call))
            }
            Ast::Op2("-", a, b) => {
                let call =
                    Self::op_to_call("prefab", "int_sub", a, b, node.loc);
                Ok(SemanticAction::Rewrite(call))
            }
            Ast::Op2("*", a, b) => {
                let call =
                    Self::op_to_call("prefab", "int_mult", a, b, node.loc);
                Ok(SemanticAction::Rewrite(call))
            }
            Ast::Op2("/", a, b) => {
                let call =
                    Self::op_to_call("prefab", "int_div", a, b, node.loc);
                Ok(SemanticAction::Rewrite(call))
            }
            Ast::Op2("mod", a, b) => {
                let call =
                    Self::op_to_call("prefab", "int_mod", a, b, node.loc);
                Ok(SemanticAction::Rewrite(call))
            }
            Ast::Op2("and", a, b) => {
                let call =
                    Self::op_to_call("prefab", "boolean_and", a, b, node.loc);
                Ok(SemanticAction::Rewrite(call))
            }
            Ast::Op2("or", a, b) => {
                let call =
                    Self::op_to_call("prefab", "boolean_or", a, b, node.loc);
                Ok(SemanticAction::Rewrite(call))
            }
            Ast::Op2("==", a, b) => {
                let call =
                    Self::op_to_call("prefab", "int_equal", a, b, node.loc);
                Ok(SemanticAction::Rewrite(call))
            }
            Ast::Op2("<", a, b) => {
                let call =
                    Self::op_to_call("prefab", "int_less_than", a, b, node.loc);
                Ok(SemanticAction::Rewrite(call))
            }
            Ast::Op1("-", x) => {
                let callx = AstNode::new(Ast::Id2(
                        Lstr::Sref("prefab"),
                        "int_negate",
                    ), node.loc);
                let arg = vec![StrupleItem::new_v(x)];
                let call = AstNode::new(Ast::Call(callx, arg), node.loc);
                Ok(SemanticAction::Rewrite(call))
            }
            Ast::Op1("\\n", x) => {
                let newline = AstNode::new_constval(
                    Val::Str(Lstr::Sref("\n")),
                    node.loc,
                );
                let strx = Ast::StrExpr(vec![x, newline]);
                Ok(SemanticAction::Rewrite(AstNode::new(strx, node.loc)))
            }
            Ast::ConstVal(Val::Str(s)) => {
                let new_str_node = match s.str() {
                    "\\n" => {
                        Ast::ConstVal(Val::Str(Lstr::Sref("\n")))
                    }
                    "\\\"" => {
                        Ast::ConstVal(Val::Str(Lstr::Sref("\"")))
                    }
                    _ => {
                        Ast::ConstVal(Val::Str(s))
                    }
                };
                let node2 = AstNode::new(new_str_node, node.loc);
                Ok(SemanticAction::Keep(node2))
            }
            Ast::Matchx(None, cases) => {
                let node_loc = node.loc;
                let args: Lresult<Xlist> = self.ftype.args.iter().map(|arg| {
                    if let Some(Lstr::Sref(argname)) = arg.k {
                        let argnode = AstNode::new(Ast::Id1(argname), node_loc);
                        Ok(StrupleItem::new_v(argnode))
                    } else {
                        return Err(rustfail!(
                            SEMFAIL,
                            "arg name is not a static str: {:?}",
                            arg.k,
                        ));
                    }
                }).collect();
                let match_input = AstNode::new(Ast::Tuple(args?), node.loc);
                let matchx = Ast::Matchx(Some(match_input), cases);
                Ok(SemanticAction::Rewrite(AstNode::new(matchx, node.loc)))
            }
            // for single element tuples, just take the single item
            Ast::Tuple(mut items) => {
                if items.len() == 1 {
                    if items.first().unwrap().k.is_none() {
                        return Ok(SemanticAction::Rewrite(
                            items.pop().unwrap().v
                        ))
                    }
                }

                let node2 = AstNode::new(Ast::Tuple(items), node.loc);
                Ok(SemanticAction::Keep(node2))
            }
            _ => Ok(SemanticAction::Keep(node)),
        }
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
struct MacroReplacement
{
    arg_map: HashMap<&'static str, AstNode>,
    loc: Loc,
}

impl SemanticOp for MacroReplacement
{
    fn pre(&mut self, node: AstNode) -> SemanticResult
    {
        if let Ast::Id1(idname) = &*node.node {
            if let Some(newval) = self.arg_map.get(idname) {
                return Ok(SemanticAction::Rewrite(newval.clone()));
            }
        }
        // if not replacing the id, replace the location of the call
        // so everything in the macro body traces back to the macro name
        let mut new_node = node;
        new_node.loc = self.loc;
        Ok(SemanticAction::Keep(new_node))
    }
}

struct ClosureCollector
{
    closures: Vec<AstNode>,
}

struct ScopeCheck<'p>
{
    blocks: Blockstack,
    local_mod: &'p ProtoModule,
    lib: &'p ProtoLib,
}

impl<'p> ScopeCheck<'p>
{
    pub fn new(
        ftyp: &'p FuncType,
        local_mod: &'p ProtoModule,
        lib: &'p ProtoLib,
    ) -> Lresult<ScopeCheck<'p>>
    {
        let mut root = Blockstack::new();
        for arg in ftyp.args.iter() {
            let argn = arg.k.as_ref().ok_or_else(|| {
                rustfail!(SEMFAIL, "arguments must have a name: {:?}", arg)
            })?;
            root.assign_var(&argn, LocalType::Param);
        }
        Ok(ScopeCheck {
            blocks: root,
            local_mod,
            lib,
        })
    }
}

impl<'p> SemanticOp for ScopeCheck<'p>
{
    fn pre(&mut self, mut node: AstNode) -> SemanticResult
    {
        match &*node.node {
            Ast::Block(_) => {
                self.blocks.push_blockscope();
            }
            Ast::Let(patt, _, _) => {
                match &*patt.node {
                    Ast::Id1(id) => {
                        self.blocks.assign_var(&Lstr::Sref(id), LocalType::Let);
                    }
                    _ => {
                        unimplemented!();
                    }
                }
            }
            Ast::Id1(id) => {
                if self.blocks.var_in_scope(&Lstr::Sref(id)) {
                    // that's cool, nothing to do I guess?
                } else if self.local_mod.find_const(id).is_some() {
                    *node.node = Ast::Id2(self.local_mod.key.name.clone(), id);
                    return Ok(SemanticAction::Rewrite(node));
                } else {
                    println!("var not in scope: {}", id);
                    return Err(
                        rustfail!(SEMFAIL, "var not in scope: {}", id,),
                    );
                }
            }
            Ast::Id2(module, id) => {
                let proto = self.lib.get(module).map_err(|e| {
                    e.add_context(Lstr::from(format!(
                        "module {} not found at {:?}",
                        module, node.loc
                    )))
                })?;
                let val_ast = proto
                    .find_const(id)
                    .ok_or_else(|| {
                        rustfail!(
                            SEMFAIL,
                            "cannot find {} in module {} @ {:?}",
                            id,
                            module,
                            node.loc,
                        )
                    })?
                    .clone();
                node = node.replace(*val_ast.node, val_ast.typ);
                return Ok(SemanticAction::Rewrite(node));
            }
            _ => {
                // do nothing otherwise
            }
        }
        Ok(SemanticAction::Keep(node))
    }

    fn post(&mut self, node: AstNode) -> SemanticResult
    {
        match &*node.node {
            Ast::Block(_) => {
                self.blocks.pop_blockscope();
            }
            _ => {
                // do nothing, keep walking
            }
        }
        Ok(SemanticAction::Keep(node))
    }
}

impl<'l> fmt::Debug for ScopeCheck<'l>
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "ScopeCheck({})", self.local_mod.key.name)
    }
}

struct VarTypes<'p>
{
    vartypes: HashMap<&'static str, Type>,
    module: &'p Lstr,
}

impl<'p> VarTypes<'p>
{
    pub fn new(module: &'p Lstr, ftype: &FuncType) -> Lresult<VarTypes<'p>>
    {
        let mut vartypes = HashMap::new();
        for arg in ftype.args.iter() {
            let argname = arg.k.as_ref().unwrap().sref()?;
            vartypes.insert(argname, arg.v.clone());
        }
        Ok(VarTypes { vartypes, module })
    }

    pub fn decl_pattern_vartypes(&mut self, node: &AstNode) -> Lresult<Type>
    {
        match &*node.node {
            Ast::Id1(id) => {
                let tvar = Type::LocalVar(id);
                self.vartypes.insert(id, tvar.clone());
                Ok(tvar)
            }
            unsupported => {
                Err(rustfail!(
                    SEMFAIL,
                    "complex patterns not supported: {:?}",
                    unsupported,
                ))
            }
        }
    }
}

impl<'p> SemanticOp for VarTypes<'p>
{
    fn pre(&mut self, mut node: AstNode) -> SemanticResult
    {
        match *node.node {
            Ast::Let(patt, dtype, mut x) => {
                let ptype = self.decl_pattern_vartypes(&patt)?;
                x.typ = ptype;
                *node.node = Ast::Let(patt, dtype, x);
            }
            Ast::Id1(id) => {
                // if the type is known, assign it to this variable
                if let Some(typ) = self.vartypes.get(id) {
                    node.typ = typ.clone();
                    // put the node back the way it was
                    *node.node = Ast::Id1(id);
                } else {
                    // i guess do nothing?
                    // could be a local function or something
                }
            }
            _ => {
                // should handle matches later, but for now it's fine
            }
        }
        Ok(SemanticAction::Keep(node))
    }
}

impl<'p> fmt::Debug for VarTypes<'p>
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "VarTypes({})", self.module)
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
    lib: &'p ProtoLib,
    result: &'p Type,
    infers: HashMap<&'static str, Type>,
    calls: Vec<Fref>,
}

impl<'p> TypeCheck<'p>
{
    pub fn new(
        local_mod: &'p ProtoModule,
        lib: &'p ProtoLib,
        ftyp: &'p FuncType,
    ) -> TypeCheck<'p>
    {
        TypeCheck {
            local_mod,
            lib,
            result: &*ftyp.result,
            infers: HashMap::new(),
            calls: vec![],
        }
    }

    pub fn inferred_local(&self, local_tvar: &'static str) -> Type
    {
        self.infers
            .get(local_tvar)
            .map(|t| t.clone())
            .unwrap_or(Type::LocalVar(local_tvar))
    }

    pub fn match_type(&mut self, t0: &Type, t1: &Type, opens: &mut StrupleKV<&'static str, Type>) -> Lresult<Type>
    {
        match (t0, t1) {
            (Type::OpenVar(v0), t1) => {
                lfailoc!(self.close_generic(v0, t1, opens))
            }
            (_, Type::OpenVar(_)) => {
                Err(rustfail!(
                    TYPEFAIL,
                    "open type vars should be passed as the first arg",
                ))
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
            (t0, Type::Unknown) => {
                Ok(t0.clone())
            }
            (Type::Unknown, t1) => {
                Ok(t1.clone())
            }
            (t0, t1) => {
                if t0 != t1 {
                    Err(rustfail!(
                        SEMFAIL,
                        "match case types do not match: ({} != {})",
                        t0,
                        t1,
                    ))
                } else {
                    Ok(t0.clone())
                }
            }
        }
    }

    pub fn infer_type(&mut self, var: &'static str, t: &Type, opens: &mut StrupleKV<&'static str, Type>) -> Lresult<Type>
    {
        if self.infers.contains_key(var) {
            let var_type = self.inferred_local(var);
            if var_type == *t {
                Ok(var_type)
            } else {
                self.match_type(&var_type, t, opens)
            }
        } else {
            if t.is_closed() {
                self.infers.insert(var, t.clone());
            }
            Ok(t.clone())
        }
    }

    pub fn close_generic(&mut self, var: &'static str, t: &Type, opens: &mut StrupleKV<&'static str, Type>) -> Lresult<Type>
    {
        let open_idx = if let Some((found, _)) = struple::find(opens, &var) {
            found
        } else {
            return Err(rustfail!(
                TYPEFAIL,
                "open type var {:?} not found in {:?}",
                var,
                opens,
            ));
        };

        if opens[open_idx].v == Type::Unknown {
            // newly defined type, set it in opens
            opens[open_idx].v = t.clone();
            Ok(t.clone())
        } else if opens[open_idx].v == *t {
            // already the same type, good
            return Ok(t.clone());
        } else {
            // different type, run a match and then assign the result
            // this upgrades inference local vars to concrete types
            let old_type = opens[open_idx].v.clone();
            let new_type = self.match_type(&old_type, t, opens)?;
            opens[open_idx].v = new_type.clone();
            Ok(new_type)
        }
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
            Type::Generic(gopen @ true, ref mut open_ftyp, ref mut opens) => {
                if let Type::Func(inner_ftyp) = &mut **open_ftyp {
                    // figure out arg types
                    let result = self.match_argtypes(inner_ftyp, args, opens)?;
                    *gopen = false;
                    Ok(result)
                } else {
                    return Err(rustfail!(
                        SEMFAIL,
                        "call type is not a function: {:?}",
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

    fn match_argtypes(&mut self, ftyp: &mut FuncType, args: &mut ast2::Xlist, opens: &mut StrupleKV<&'static str, Type>) -> Lresult<Type>
    {
        if args.len() < ftyp.args.len() {
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
            let typ =  lfailoc!(self.match_type(&arg.0.v, &arg.1.v.typ, opens))
                .map_err(|f| {
                    f.add_context(lstrf!(
                        "function param: {}, expected {}, found {}",
                        arg.0.k.as_ref().unwrap(),
                        arg.0.v,
                        arg.1.v.typ,
                    ))
                })?;
            arg.0.v = typ.clone();
            arg.1.v.typ = typ;
        }

        let result = self.match_type(&ftyp.result, &Type::Unknown, opens)?;
        ftyp.result = Box::new(result.clone());
        Ok(result)
    }

    fn match_case_types(&mut self, cases: &Vec<ast2::Case>) -> Lresult<Type>
    {
        let mut prev_typ: Option<Type> = None;
        let mut opens = vec![];
        for case in cases.iter() {
            if let Some(ref pt) = prev_typ {
                self.match_type(pt, &case.body.typ, &mut opens)?;
            } else {
                prev_typ = Some(case.body.typ.clone());
            }
        }
        Ok(prev_typ.unwrap())
    }
}

impl<'p> SemanticOp for TypeCheck<'p>
{
    fn pre(&mut self, mut node: AstNode) -> SemanticResult
    {
        match &mut *node.node {
            // Ast::Let(patt, dtype, x) => {
            Ast::Id2(modname, id) => {
                let module = self.lib.get(modname)?;
                let typ = module.get_type(id)?;
                node.typ = typ.clone();
            }
            Ast::ConstVal(c) if node.typ.is_open() => {
                node.typ = c.get_type();
            }
            Ast::RustBlock => {
                node.typ = self.result.clone();
            }
            _ => {
                // should handle matches later, but for now it's fine
            }
        }
        Ok(SemanticAction::Keep(node))
    }

    fn post(&mut self, mut node: AstNode) -> SemanticResult
    {
        match &mut *node.node {
            Ast::Block(items) => {
                if let Some(last) = items.last() {
                    node.typ = last.typ.clone();
                } else {
                    node.typ = Type::Void;
                }
            }
            Ast::Call(ref mut callx, ref mut args) => {
                let call_result = self.applied_call_type(&mut callx.typ, args)?;
                node.typ = call_result;
            }
            Ast::StrExpr(ref _items) => {
                // check items, but not necessary yet b/c everything
                // converts to strings right now
                node.typ = Type::Str;
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
                    self.match_type(&case.cond.typ, &Type::Bool, &mut opens)?;
                    case.cond.typ = Type::Bool;
                }
                node.typ = self.match_case_types(cases)?;
            }
            Ast::Matchx(_, ref mut cases) => {
                // what about match input and pattern types?
                node.typ = self.match_case_types(cases)?;
            }
            Ast::Let(ref mut patt, _, ref mut x) => {
                let mut opens = vec![];
                let typ = self.match_type(&patt.typ, &x.typ, &mut opens)?;
                patt.typ = typ.clone();
                x.typ = typ;
            }
            _ => {
                // should handle matches later, but for now it's fine
            }
        }
        Ok(SemanticAction::Keep(node))
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

impl SemanticOp for RemoveExtraCode
{
    fn post(&mut self, mut node: AstNode) -> SemanticResult
    {
        let action = match *node.node {
            Ast::Block(mut items) => {
                match items.len() {
                    0 => SemanticAction::Keep(AstNode::void()),
                    1 => SemanticAction::Keep(items.pop().unwrap()),
                    _ => {
                        *node.node = Ast::Block(items);
                        SemanticAction::Keep(node)
                    }
                }
            }
            ast => {
                *node.node = ast;
                SemanticAction::Keep(node)
            }
        };
        Ok(action)
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
    // pub calls: HashSet<ModLocalId>,
    // pub typecalls: HashSet<SpecialModId>,
    // pub closed: Option<HashSet<Lstr>>,
    pub src: AstNode,
    pub args: Vec<Option<&'static str>>,
    pub infers: HashMap<&'static str, Type>,
}

impl Semantics
{
    pub fn new() -> Semantics
    {
        Semantics {
            src: AstNode::void(),
            args: Vec::new(),
            infers: HashMap::new(),
        }
    }

    pub fn get_type(&self) -> &Type
    {
        &self.src.typ
    }

    pub fn compile_call(
        &mut self,
        proto: &mut ProtoLib,
        mod_name: &str,
        func_name: &str,
    ) -> Lresult<()>
    {
        let func_ast = proto.pop_func(mod_name, func_name)?;
        let (_args, body) = func_ast.ok_or_else(|| {
            rustfail!(
                SEMFAIL,
                "ast is empty for function {}::{}",
                mod_name,
                func_name,
            )
        })?;

        let local_proto = proto.get(mod_name)?;
        let func_ref = local_proto.find_const(func_name)
            .ok_or_else(|| {
                rustfail!(
                    SEMFAIL,
                    "cannot find func ref for {}::{}",
                    mod_name,
                    func_name,
                )
            })?;
        let ftyp = match &func_ref.typ {
            Type::Func(ft) => ft,
            Type::Generic(true, _gen_id, _gen_types) => {
                return Err(rustfail!(
                    SEMFAIL,
                    "open generic function should be a type call: {}::{}",
                    mod_name,
                    func_name,
                ));
            }
            unexpected => {
                return Err(rustfail!(
                    SEMFAIL,
                    "unexpected call type: {}",
                    unexpected,
                ));
            }
        };

        self.args = ftyp
            .args
            .iter()
            .map(|kv| {
                match &kv.k {
                    Some(Lstr::Sref(name)) => Some(*name),
                    Some(Lstr::Arc(ref name)) => {
                        panic!("func arg name is not a static: {}", name);
                    }
                    _ => None,
                }
            })
            .collect();

        let mut macs: MacroApplication = MacroApplication {
            local: local_proto,
            proto: proto,
            ftype: ftyp,
        };
        let mut scope_check = ScopeCheck::new(ftyp, local_proto, proto)?;
        let mut var_types = VarTypes::new(&local_proto.key.name, ftyp)?;
        let mut type_check = TypeCheck::new(local_proto, proto, ftyp);
        let mut remove_extra = RemoveExtraCode;
        let mut pipe = SemanticPipeline {
            ops: vec![
                &mut remove_extra,
                &mut macs,
                &mut scope_check,
                &mut var_types,
                &mut type_check,
            ],
        };

        let result = Self::walk(&mut pipe, body)?;
        if *ftyp.result != result.typ && *ftyp.result != Type::Void {
            return Err(rustfail!(
                SEMFAIL,
                "bad return type in {}::{}, expected: {}, found {}",
                mod_name,
                func_name,
                ftyp.result,
                result.typ,
            ));
        }

        self.infers = type_check.infers;
        self.src = result;
        Ok(())
    }

    pub fn walk<Op: SemanticOp>(op: &mut Op, node: AstNode) -> AstResult
    {
        let mut prenode = match op.pre(node)? {
            SemanticAction::Keep(inode) => inode,
            SemanticAction::Rewrite(inode) => inode,
            SemanticAction::Remove => return Ok(AstNode::void()),
        };

        let new_ast = match *prenode.node {
            Ast::Block(children) => {
                let new_children: Lresult<Vec<AstNode>> =
                    children.into_iter().map(|ch| Self::walk(op, ch)).collect();
                Ast::Block(new_children?)
            }
            Ast::Call(id, args) => {
                let wid = Self::walk(op, id)?;
                let wargs = struple::map_v_into(args, |v| Self::walk(op, v))?;
                Ast::Call(wid, wargs)
            }
            Ast::Ifx(cases) => {
                let new_cases: Lresult<Vec<ast2::Case>> = cases
                    .into_iter()
                    .map(|ch| {
                        let wcond = Self::walk(op, ch.cond)?;
                        let wbody = Self::walk(op, ch.body)?;
                        Ok(ast2::Case::new(wcond, wbody))
                    })
                    .collect();
                Ast::Ifx(new_cases?)
            }
            Ast::Matchx(None, args) => {
                let new_args: Lresult<Vec<ast2::Case>> = args
                    .into_iter()
                    .map(|ch| {
                        op.set_pattern(true);
                        let wcond = Self::walk(op, ch.cond)?;
                        op.set_pattern(false);
                        let wbody = Self::walk(op, ch.body)?;
                        Ok(ast2::Case::new(wcond, wbody))
                    })
                    .collect();
                Ast::Matchx(None, new_args?)
            }
            Ast::Matchx(Some(cond), children) => {
                let wcond = Self::walk(op, cond)?;
                let wchildren: Lresult<Vec<ast2::Case>> = children
                    .into_iter()
                    .map(|ch| {
                        op.set_pattern(true);
                        let wcond = Self::walk(op, ch.cond)?;
                        op.set_pattern(false);
                        let wbody = Self::walk(op, ch.body)?;
                        Ok(ast2::Case::new(wcond, wbody))
                    })
                    .collect();
                Ast::Matchx(Some(wcond), wchildren?)
            }
            Ast::ConstVal(v) => {
                // can't walk past on const
                Ast::ConstVal(v)
            }
            Ast::DefConst(name, v) => {
                let wval = Self::walk(op, v)?;
                Ast::DefConst(name, wval)
            }
            Ast::DefFunc(name, args, result, body) => {
                let wname = Self::walk(op, name)?;
                let wargs = struple::map_v_into(args, |arg| Self::walk(op, arg))?;
                let wresult = Self::walk(op, result)?;
                let wbody = Self::walk(op, body)?;
                Ast::DefFunc(wname, wargs, wresult, wbody)
            }
            Ast::Generic(id, args) => {
                let wid = Self::walk(op, id)?;
                let wargs = struple::map_v_into(args, |arg| Self::walk(op, arg))?;
                Ast::Generic(wid, wargs)
            }
            Ast::Let(lhp, lht, rhs) => {
                op.set_pattern(true);
                let wlhp = Self::walk(op, lhp)?;
                op.set_pattern(false);
                let wrhs = Self::walk(op, rhs)?;
                Ast::Let(wlhp, lht, wrhs)
            }
            Ast::List(items) => {
                let witems = struple::map_v_into(items, |item| Self::walk(op, item))?;
                Ast::List(witems)
            }
            Ast::Op1(ast_op, node) => {
                let wnode = Self::walk(op, node)?;
                Ast::Op1(ast_op, wnode)
            }
            Ast::Op2(ast_op, a, b) => {
                let wa = Self::walk(op, a)?;
                let wb = Self::walk(op, b)?;
                Ast::Op2(ast_op, wa, wb)
            }
            Ast::Return(x) => Ast::Return(Self::walk(op, x)?),
            Ast::StrExpr(items) => {
                let witems: Lresult<Vec<AstNode>> =
                    items.into_iter().map(|i| Self::walk(op, i)).collect();
                Ast::StrExpr(witems?)
            }
            Ast::Tuple(items) => {
                let witems = struple::map_v_into(items, |item| Self::walk(op, item))?;
                Ast::Tuple(witems)
            }

            // nothing further to walk for these ASTs
            Ast::Id1(id) => Ast::Id1(id),
            Ast::Id2(id1, id2) => Ast::Id2(id1, id2),
            Ast::RustBlock => Ast::RustBlock,
            Ast::Void => Ast::Void,
            Ast::Wildcard => Ast::Wildcard,

            // these ASTs should already be processed in the proto phase
            Ast::DefMacro(name, _, _) => {
                return Err(rustfail!(
                    SEMFAIL,
                    "macro definition must already be processed: {} @ {:?}",
                    name,
                    prenode.loc,
                ));
            }
            Ast::DefType(_, name, _) => {
                return Err(rustfail!(
                    SEMFAIL,
                    "type definition must already be processed: {:?}",
                    name,
                ));
            }
            Ast::Import(module) => {
                return Err(rustfail!(
                    SEMFAIL,
                    "import must already be processed: {}",
                    module,
                ));
            }
            /*
            // unimplemented
            Ast::FuncType(_) => unimplemented!(),
            Ast::LessThan3(_, _, _, _, _) => unimplemented!(),
            Ast::Map(_) => unimplemented!(),
            Ast::NewStruct(_, _) => unimplemented!(),
            Ast::NewTuple(_) => unimplemented!(),
            Ast::NewUnion(_, _, _) => unimplemented!(),
            */
            ast => ast, // do nothing for everything else
        };
        *prenode.node = new_ast;

        let postnode = match op.post(prenode)? {
            SemanticAction::Keep(inode) => inode,
            SemanticAction::Rewrite(inode) => inode,
            SemanticAction::Remove => return Ok(AstNode::void()),
        };

        Ok(postnode)
    }
}

#[cfg(test)]
mod tests
{
    use super::Semantics;
    use crate::leema::ast2::Ast;
    use crate::leema::loader::Interloader;
    use crate::leema::lstr::Lstr;
    use crate::leema::proto::ProtoLib;
    use crate::leema::val::Type;

    use matches::assert_matches;


    fn load_proto_with_prefab() -> ProtoLib
    {
        let mut loader = Interloader::default();
        let mut proto = ProtoLib::new();
        lfailoc!(proto.load(&mut loader, &Lstr::Sref("prefab"))).unwrap();
        proto
    }

    #[test]
    fn test_semantics_macro()
    {
        let input = r#"
        macro test_and a b >>
            if
            |a >> b
            |else >> False
            --
        --

        func main >>
            test_and(True, False)
        --
        "#;

        let mut proto = load_proto_with_prefab();
        proto.add_module(&Lstr::Sref("foo"), input).unwrap();
        let mut semantics = Semantics::new();
        let body = semantics.compile_call(&mut proto, "foo", "main").unwrap();
        assert_matches!(*body.node, Ast::Ifx(_));
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
        "#;

        let mut proto = ProtoLib::new();
        proto.add_module(&Lstr::Sref("foo"), input).unwrap();
        let mut semantics = Semantics::new();
        semantics.compile_call(&mut proto, "foo", "main").unwrap();
    }

    #[test]
    fn test_semantics_param_in_scope()
    {
        let input = r#"func inc i:Int / Int >> i + 1 --"#;

        let mut proto = load_proto_with_prefab();
        proto.add_module(&Lstr::Sref("foo"), input).unwrap();
        let mut semantics = Semantics::new();
        let result = semantics.compile_call(&mut proto, "foo", "inc").unwrap();
        assert_eq!(Type::Int, result.typ);
    }

    #[test]
    fn test_semantics_module_scope_call()
    {
        let input = r#"
        func foo / Int >> 5 --

        func main >>
            foo() + 3
        --
        "#;

        let mut proto = load_proto_with_prefab();
        proto.add_module(&Lstr::Sref("foo"), input).unwrap();
        let mut semantics = Semantics::new();
        semantics.compile_call(&mut proto, "foo", "main").unwrap();
    }

    #[test]
    fn test_type_inferred_var()
    {
        let input = r#"
        func main >>
            let x := 8
            let y := x + 1
        --
        "#;

        let mut proto = load_proto_with_prefab();
        proto.add_module(&Lstr::Sref("foo"), input).unwrap();
        let mut semantics = Semantics::new();
        semantics.compile_call(&mut proto, "foo", "main").unwrap();
    }

    #[test]
    fn test_type_genericfunc()
    {
        let input = r#"
        func swap[:T] a:T b:T / (:T :T)
        >>
            (b, a)
        --

        func main >>
            swap(3, 5)
        --
        "#;

        let mut proto = load_proto_with_prefab();
        proto.add_module(&Lstr::Sref("foo"), input).unwrap();
        let mut semantics = Semantics::new();
        semantics.compile_call(&mut proto, "foo", "main").unwrap();
    }

    #[test]
    fn test_type_genericfuncs()
    {
        let input = r#"
        func new_pair[:A :B] a:A b:B /(:A :B) >> (a, b) --

        func first[:A :B] p:(:A :B) /A
        |(a, _) >> a
        --

        func main >>
            let p := new_pair(4, "b")
            let f := first(p)
        --
        "#;

        let mut proto = load_proto_with_prefab();
        proto.add_module(&Lstr::Sref("foo"), input).unwrap();
        let mut semantics = Semantics::new();
        semantics.compile_call(&mut proto, "foo", "main").unwrap();
    }

    #[test]
    #[should_panic]
    fn test_semantics_module_scope_fail()
    {
        let input = r#"func main >> foo() --"#;

        let mut proto = ProtoLib::new();
        proto.add_module(&Lstr::Sref("foo"), input).unwrap();
        let mut semantics = Semantics::new();
        semantics.compile_call(&mut proto, "foo", "main").unwrap();
    }

    #[test]
    fn test_semantics_external_scope_call()
    {
        let foo_input = r#"func bar / Int >> 3 --"#;

        let baz_input = r#"
        func main >>
            foo::bar() + 6
        --
        "#;

        let mut proto = load_proto_with_prefab();
        proto.add_module(&Lstr::Sref("foo"), foo_input).unwrap();
        proto.add_module(&Lstr::Sref("baz"), baz_input).unwrap();
        let mut semantics = Semantics::new();
        semantics.compile_call(&mut proto, "baz", "main").unwrap();
    }

    #[test]
    #[should_panic]
    fn test_semantics_external_scope_fail()
    {
        let foo_input = r#"func bak >> 3 --"#;

        let app_input = r#"
        func main >>
            foo::bar() + 6
        --
        "#;

        let mut proto = ProtoLib::new();
        proto.add_module(&Lstr::Sref("foo"), foo_input).unwrap();
        proto.add_module(&Lstr::Sref("app"), app_input).unwrap();
        let mut semantics = Semantics::new();
        semantics.compile_call(&mut proto, "app", "main").unwrap();
    }

    #[test]
    fn test_semantics_undefined_variable()
    {
        let baz_input = r#"
        func main >>
            let a := blah
            "a is $a\n"
        --
        "#;

        let mut proto = ProtoLib::new();
        proto.add_module(&Lstr::Sref("baz"), baz_input).unwrap();
        let mut semantics = Semantics::new();
        let result = semantics.compile_call(&mut proto, "baz", "main");
        assert_eq!("semantic_failure", result.unwrap_err().tag.str());
    }

    #[test]
    #[should_panic]
    fn test_semantics_external_scope_no_module()
    {
        let baz_input = r#"
        func main >>
            foo::bar() + 6
        --
        "#;

        let mut proto = ProtoLib::new();
        proto.add_module(&Lstr::Sref("baz"), baz_input).unwrap();
        let mut semantics = Semantics::new();
        semantics.compile_call(&mut proto, "baz", "main").unwrap();
    }

    #[test]
    #[should_panic]
    fn test_type_fail_func_types()
    {
        let input = r#"
        func inc i:Int :Int >> i + 1 --
        func main >> inc("5") --
        "#;

        let mut proto = ProtoLib::new();
        proto.add_module(&Lstr::Sref("foo"), input).unwrap();
        let mut semantics = Semantics::new();
        semantics.compile_call(&mut proto, "foo", "main").unwrap();
    }

    #[test]
    #[should_panic]
    fn test_type_fail_too_few_args()
    {
        let input = r#"
        func mult i:Int j:Int :Int >> i * j --
        func main >> mult(7) --
        "#;

        let mut proto = ProtoLib::new();
        proto.add_module(&Lstr::Sref("foo"), input).unwrap();
        let mut semantics = Semantics::new();
        semantics.compile_call(&mut proto, "foo", "main").unwrap();
    }

    #[test]
    #[should_panic]
    fn test_type_fail_too_many_args()
    {
        let input = r#"
        func inc i:Int / Int >> i + 1 --
        func main >> inc(2, 7) --
        "#;

        let mut proto = ProtoLib::new();
        proto.add_module(&Lstr::Sref("foo"), input).unwrap();
        let mut semantics = Semantics::new();
        semantics.compile_call(&mut proto, "foo", "main").unwrap();
    }

    #[test]
    #[should_panic]
    fn test_type_fail_bad_return_type()
    {
        let input = r#"
        func inc i:Int :Int >> "hello" --
        "#;

        let mut proto = ProtoLib::new();
        proto.add_module(&Lstr::Sref("foo"), input).unwrap();
        let mut semantics = Semantics::new();
        semantics.compile_call(&mut proto, "foo", "inc").unwrap();
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
        "#;

        let mut proto = load_proto_with_prefab();
        proto.add_module(&Lstr::Sref("foo"), input).unwrap();
        let mut semantics = Semantics::new();
        semantics.compile_call(&mut proto, "foo", "factf").unwrap();
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

            func main() ->
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
            func foo(): Int ->
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
