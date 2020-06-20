use crate::leema::ast2::{self, Ast, AstNode, AstResult, Loc, Xlist};
use crate::leema::failure::{self, Failure, Lresult};
use crate::leema::inter::{Blockstack, LocalType};
use crate::leema::lstr::Lstr;
use crate::leema::proto::ProtoModule;
use crate::leema::struple::{self, Struple2, StrupleItem, StrupleKV};
use crate::leema::val::{
    Fref, FuncType, GenericTypeSlice, GenericTypes, Type, Val,
};

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

#[derive(Copy)]
#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
pub enum AstMode
{
    Value,
    Type,
    LetPattern,
    MatchPattern,
}

impl AstMode
{
    pub fn is_pattern(&self) -> bool
    {
        match self {
            AstMode::LetPattern => true,
            AstMode::MatchPattern => true,
            _ => false,
        }
    }

    pub fn get_pattern(&self) -> Option<LocalType>
    {
        match self {
            AstMode::LetPattern => Some(LocalType::Let),
            AstMode::MatchPattern => Some(LocalType::Match),
            _ => None,
        }
    }
}

pub type SemanticResult = Lresult<SemanticAction>;

pub trait SemanticOp: fmt::Debug
{
    fn pre(&mut self, node: AstNode) -> SemanticResult
    {
        Ok(SemanticAction::Keep(node))
    }
    fn post(&mut self, node: AstNode) -> SemanticResult
    {
        Ok(SemanticAction::Keep(node))
    }

    fn set_mode(&mut self, _mode: AstMode) {}
}

#[derive(Debug)]
struct SemanticPipeline<'p>
{
    ops: Vec<&'p mut dyn SemanticOp>,
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

    fn set_mode(&mut self, mode: AstMode)
    {
        for op in self.ops.iter_mut() {
            op.set_mode(mode);
        }
    }
}

struct MacroApplication<'l>
{
    local: &'l ProtoModule,
    ftype: &'l FuncType,
    closed: &'l GenericTypes,
    mode: AstMode,
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
            mode: AstMode::Value,
        }
    }

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

    fn op_to_call1(
        func: &'static str,
        a: AstNode,
        b: AstNode,
        loc: Loc,
    ) -> AstNode
    {
        let callx = AstNode::new(Ast::Id1(func), loc);
        let args: StrupleKV<Option<&'static str>, AstNode> =
            struple::new_tuple2(a, b);
        AstNode::new(Ast::Call(callx, args), loc)
    }

    fn find_macro_1(&self, macroname: &str) -> Lresult<Option<&Ast>>
    {
        Ok(self.local.find_macro(macroname))
    }
}

impl<'l> SemanticOp for MacroApplication<'l>
{
    fn pre(&mut self, mut node: AstNode) -> SemanticResult
    {
        match *node.node {
            Ast::Call(callid, args) => {
                let optmac = match *callid.node {
                    Ast::Id1(macroname) => self.find_macro_1(macroname)?,
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
                let call = Self::op_to_call1("int_add", a, b, node.loc);
                Ok(SemanticAction::Rewrite(call))
            }
            Ast::Op2("-", a, b) => {
                let call = Self::op_to_call1("int_sub", a, b, node.loc);
                Ok(SemanticAction::Rewrite(call))
            }
            Ast::Op2("*", a, b) => {
                let call = Self::op_to_call1("int_mult", a, b, node.loc);
                Ok(SemanticAction::Rewrite(call))
            }
            Ast::Op2("/", a, b) => {
                let call = Self::op_to_call1("int_div", a, b, node.loc);
                Ok(SemanticAction::Rewrite(call))
            }
            Ast::Op2("mod", a, b) => {
                let call = Self::op_to_call1("int_mod", a, b, node.loc);
                Ok(SemanticAction::Rewrite(call))
            }
            Ast::Op2("and", a, b) => {
                let call = Self::op_to_call1("boolean_and", a, b, node.loc);
                Ok(SemanticAction::Rewrite(call))
            }
            Ast::Op2("or", a, b) => {
                let call = Self::op_to_call1("boolean_or", a, b, node.loc);
                Ok(SemanticAction::Rewrite(call))
            }
            Ast::Op2("==", a, b) => {
                let call = Self::op_to_call1("int_equal", a, b, node.loc);
                Ok(SemanticAction::Rewrite(call))
            }
            Ast::Op2("<", a, b) => {
                let call = Self::op_to_call1("int_less_than", a, b, node.loc);
                Ok(SemanticAction::Rewrite(call))
            }
            Ast::Op2(">", a, b) => {
                let call = Self::op_to_call1("int_gt", a, b, node.loc);
                Ok(SemanticAction::Rewrite(call))
            }
            Ast::Op2("<=", a, b) => {
                let call = Self::op_to_call1("int_lteq", a, b, node.loc);
                Ok(SemanticAction::Rewrite(call))
            }
            Ast::Op2(">=", a, b) => {
                let call = Self::op_to_call1("int_gteq", a, b, node.loc);
                Ok(SemanticAction::Rewrite(call))
            }
            Ast::Op2(";", a, b) => {
                if !self.mode.is_pattern() {
                    // if not a pattern, convert to a call
                    let callx = AstNode::new(Ast::Id1("cons"), node.loc);
                    let args = struple::new_tuple2(a, b);
                    let call = AstNode::new(Ast::Call(callx, args), node.loc);
                    Ok(SemanticAction::Rewrite(call))
                } else {
                    // if a pattern, leave as an Op
                    *node.node = Ast::Op2(";", a, b);
                    Ok(SemanticAction::Keep(node))
                }
            }
            Ast::Op1("-", x) => {
                let callx = AstNode::new(Ast::Id1("int_negate"), node.loc);
                let arg = vec![StrupleItem::new_v(x)];
                let call = AstNode::new(Ast::Call(callx, arg), node.loc);
                Ok(SemanticAction::Rewrite(call))
            }
            Ast::Op1("\\n", x) => {
                let newline =
                    AstNode::new_constval(Val::Str(Lstr::Sref("\n")), node.loc);
                let strx = Ast::StrExpr(vec![x, newline]);
                Ok(SemanticAction::Rewrite(AstNode::new(strx, node.loc)))
            }
            Ast::ConstVal(Val::Str(s)) => {
                let new_str_node = match s.str() {
                    "\\n" => Ast::ConstVal(Val::Str(Lstr::Sref("\n"))),
                    "\\\"" => Ast::ConstVal(Val::Str(Lstr::Sref("\""))),
                    _ => Ast::ConstVal(Val::Str(s)),
                };
                let node2 = AstNode::new(new_str_node, node.loc);
                Ok(SemanticAction::Keep(node2))
            }
            Ast::Id1(id) => {
                let nloc = node.loc;
                if let Some((_, ctype)) = struple::find(self.closed, &id) {
                    let type_ast = Ast::Type(ctype.clone());
                    Ok(SemanticAction::Rewrite(AstNode::new(type_ast, nloc)))
                } else {
                    Ok(SemanticAction::Keep(node))
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
                                AstNode::new(Ast::Id1(argname), node_loc);
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
                let matchx = Ast::Matchx(Some(match_input), cases);
                Ok(SemanticAction::Rewrite(AstNode::new(matchx, node.loc)))
            }
            // for single element tuples, just take the single item
            Ast::Tuple(mut items) => {
                if items.len() == 1 {
                    if items.first().unwrap().k.is_none() {
                        return Ok(SemanticAction::Rewrite(
                            items.pop().unwrap().v,
                        ));
                    }
                }

                let node2 = AstNode::new(Ast::Tuple(items), node.loc);
                Ok(SemanticAction::Keep(node2))
            }
            _ => Ok(SemanticAction::Keep(node)),
        }
    }

    fn set_mode(&mut self, mode: AstMode)
    {
        self.mode = mode;
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

struct ScopeCheck<'p>
{
    blocks: Blockstack,
    local_mod: &'p ProtoModule,
    mode: AstMode,
}

impl<'p> ScopeCheck<'p>
{
    pub fn new(
        ftyp: &'p FuncType,
        local_mod: &'p ProtoModule,
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
            mode: AstMode::Value,
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
            Ast::Id1(id) if self.mode.is_pattern() => {
                let local_type = self.mode.get_pattern().unwrap();
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
                self.blocks.assign_var(&Lstr::Sref(id), local_type);
            }
            Ast::Id1(id) if self.mode == AstMode::Type => {
                self.local_mod.get_type(id)?;
                return Ok(SemanticAction::Keep(node));
            }
            Ast::Id1(id) => {
                if self.blocks.var_in_scope(&Lstr::Sref(id)) {
                    // that's cool, nothing to do I guess?
                } else if let Some(me) = self.local_mod.find_modelem(id) {
                    node = node.replace((*me.node).clone(), me.typ.clone());
                    return Ok(SemanticAction::Rewrite(node));
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

    fn set_mode(&mut self, mode: AstMode)
    {
        self.mode = mode;
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
    result: Type,
    vartypes: HashMap<&'static str, Type>,
    infers: HashMap<Lstr, Type>,
    calls: Vec<Fref>,
    mode: AstMode,
}

impl<'p> TypeCheck<'p>
{
    pub fn new(
        local_mod: &'p ProtoModule,
        ftyp: &'p FuncType,
    ) -> Lresult<TypeCheck<'p>>
    {
        let mut check = TypeCheck {
            local_mod,
            result: Type::VOID,
            vartypes: HashMap::new(),
            infers: HashMap::new(),
            calls: vec![],
            mode: AstMode::Value,
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
            let new_type = ltry!(self.match_type(&old_type, t, opens));
            opens[open_idx].v = new_type.clone();
            Ok(new_type)
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
                    let t = self.local_mod.ast_to_type(
                        &self.local_mod.key.name,
                        &a.1.v,
                        &[],
                    )?;
                    a.0.v = t;
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

    fn match_argtypes(
        &mut self,
        ftyp: &mut FuncType,
        args: &mut ast2::Xlist,
        opens: &mut StrupleKV<&'static str, Type>,
    ) -> Lresult<Type>
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
            let typ = ltry!(self
                .match_type(&arg.0.v, &arg.1.v.typ, opens)
                .map_err(|f| {
                    f.add_context(lstrf!(
                        "function param: {}, expected {}, found {} column:{}",
                        arg.0.k.as_ref().unwrap(),
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

impl<'p> SemanticOp for TypeCheck<'p>
{
    fn pre(&mut self, mut node: AstNode) -> SemanticResult
    {
        match &mut *node.node {
            // Ast::Let(patt, dtype, x) => {
            Ast::Id1(id) => {
                // if the type is known, assign it to this variable
                if let Some(typ) = self.vartypes.get(id) {
                    node.typ = typ.clone();
                // put the node back the way it was
                // *node.node = Ast::Id1(id);
                } else {
                    let tvar = Type::LocalVar(Lstr::Sref(id));
                    self.vartypes.insert(id, tvar.clone());
                    node.typ = tvar;
                    // put the node back the way it was
                    // *node.node = Ast::Id1(id);
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
            Ast::Block(_) | Ast::Call(_, _) | Ast::Tuple(_) => {
                // handled in post
            }
            unsupported => {
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
                    node.typ = Type::VOID;
                }
            }
            Ast::Call(ref mut callx, ref mut args) => {
                let call_result = ltry!(self
                    .applied_call_type(&mut callx.typ, args)
                    .map_err(|f| {
                        f.add_context(lstrf!(
                            "for function: {:?} with type {}",
                            callx.node,
                            callx.typ,
                        ))
                    }));
                if let Ast::ConstVal(Val::Call(ref mut fref, _argvals)) =
                    &mut *callx.node
                {
                    fref.t = callx.typ.clone();
                    // an optimization here might be to iterate over ast args
                    // and initialize any constants
                    // actually better might be to stop having the args
                    // in the Val::Call const
                    self.calls.push(fref.clone());
                } else {
                    return Err(rustfail!(
                        SEMFAIL,
                        "call expression is not a const fref: {:?}",
                        callx,
                    ));
                }
                node.typ = call_result;
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
            Ast::Return(_) => {
                node.typ = user_type!("core", "NoReturn");
            }
            _ => {
                // should handle matches later, but for now it's fine
            }
        }
        Ok(SemanticAction::Keep(node))
    }

    fn set_mode(&mut self, mode: AstMode)
    {
        self.mode = mode;
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
    pub src: AstNode,
    pub args: Vec<Option<&'static str>>,
    pub infers: HashMap<Lstr, Type>,
    pub calls: Vec<Fref>,
}

impl Semantics
{
    pub fn new() -> Semantics
    {
        Semantics {
            src: AstNode::void(),
            args: Vec::new(),
            infers: HashMap::new(),
            calls: Vec::new(),
        }
    }

    pub fn get_type(&self) -> &Type
    {
        &self.src.typ
    }

    pub fn compile_call(proto: &mut ProtoModule, f: &Fref) -> Lresult<Semantics>
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
            unexpected => {
                return Err(rustfail!(
                    SEMFAIL,
                    "unexpected call types: {:?}",
                    unexpected,
                ));
            }
        };

        sem.args = ftyp
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

        let mut macs = MacroApplication::new(proto, ftyp, &closed);
        let mut scope_check = ScopeCheck::new(ftyp, proto)?;
        let mut type_check = TypeCheck::new(proto, ftyp)?;
        let mut remove_extra = RemoveExtraCode;

        let interleaved = true;
        let mut result = if interleaved {
            let mut pipe = SemanticPipeline {
                ops: vec![
                    &mut remove_extra,
                    &mut macs,
                    &mut scope_check,
                    &mut type_check,
                ],
            };
            ltry!(Self::walk(&mut pipe, body))
        } else {
            let mut result = ltry!(Self::walk(&mut macs, body));
            result = ltry!(Self::walk(&mut scope_check, result));
            result = ltry!(Self::walk(&mut type_check, result));
            ltry!(Self::walk(&mut remove_extra, result))
        };

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
        sem.calls = type_check.calls;
        sem.src = result;
        Ok(sem)
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
                let wid = ltry!(Self::walk(op, id));
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
                        op.set_mode(AstMode::MatchPattern);
                        let wcond = Self::walk(op, ch.cond)?;
                        op.set_mode(AstMode::Value);
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
                        op.set_mode(AstMode::MatchPattern);
                        let wcond = Self::walk(op, ch.cond)?;
                        op.set_mode(AstMode::Value);
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
                let wargs =
                    struple::map_v_into(args, |arg| Self::walk(op, arg))?;
                let wresult = Self::walk(op, result)?;
                let wbody = Self::walk(op, body)?;
                Ast::DefFunc(wname, wargs, wresult, wbody)
            }
            Ast::Generic(id, args) => {
                let wid = Self::walk(op, id)?;
                op.set_mode(AstMode::Type);
                let wargs =
                    struple::map_v_into(args, |arg| Self::walk(op, arg))?;
                op.set_mode(AstMode::Value);
                Ast::Generic(wid, wargs)
            }
            Ast::Let(lhp, lht, rhs) => {
                op.set_mode(AstMode::LetPattern);
                let wlhp = Self::walk(op, lhp)?;
                op.set_mode(AstMode::Value);
                let wrhs = Self::walk(op, rhs)?;
                Ast::Let(wlhp, lht, wrhs)
            }
            Ast::List(items) => {
                let witems =
                    struple::map_v_into(items, |item| Self::walk(op, item))?;
                Ast::List(witems)
            }
            Ast::Op1(ast_op, node) => {
                let wnode = Self::walk(op, node)?;
                Ast::Op1(ast_op, wnode)
            }
            Ast::Op2(".", a, b) => {
                let wa = Self::walk(op, a)?;
                let wb = Self::walk(op, b)?;
                Ast::Op2(".", wa, wb)
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
                let witems =
                    struple::map_v_into(items, |item| Self::walk(op, item))?;
                Ast::Tuple(witems)
            }

            // nothing further to walk for these ASTs
            Ast::Id1(id) => Ast::Id1(id),
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
            Ast::ModAction(action, tree) => {
                return Err(rustfail!(
                    SEMFAIL,
                    "module action must already be processed: {:?} {:?}",
                    action,
                    tree,
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
        func sort[T] unsorted:[T] /[T] >> __RUST__ --
        func take_names with_names:[(Int Str)] /[Str] >> __RUST__ --

        func main >>
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
        let input = r#"
        func main ->
            4;[6]
            4 < 6
            8 > 8
            "x" <= "y"
            "x" >= "y"
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
        let input = r#"func inc: Int :: i:Int -> i + 1 --"#.to_string();

        let mut prog = core_program(&[("/foo", input)]);
        let fref = Fref::from(("/foo", "inc"));
        let result = prog.read_semantics(&fref).unwrap();
        assert_eq!(Type::INT, result.src.typ);
    }

    #[test]
    fn test_semantics_module_scope_call()
    {
        let input = r#"
        func foo /Int >> 5 --

        func main >>
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
        func swap[T] a:T b:T /(T T)
        >>
            (b, a)
        --

        func main >>
            swap[Str #]("hello", #world)
        --
        "#
        .to_string();

        let mut prog = core_program(&[("/foo", input)]);
        let fref = Fref::from(("/foo", "main"));
        let f = prog.read_semantics(&fref).unwrap_err();
        assert_eq!("type_failure", f.tag.str());
        assert_eq!("wrong number of args, expected 1, found 2", f.msg.str());
    }

    #[test]
    fn test_type_genericfunc_1()
    {
        let input = r#"
        func swap[A B] a:A b:B /(B A)
        >>
            (b, a)
        --

        func main >>
            swap(3, 5)
            swap[Str #]("hello", #world)
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
        func new_pair[A B] a:A b:B /(A B) >> (a, b) --

        func first[A B] p:(A B) /A
        |(a, _) >> a
        --

        func main >>
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
        func safediv x:Int y:Int /Int
        |(_, 0) >> fail(#divide_by_0, "cannot divide by zero")
        |(a, b) >> a + b
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
        let input = r#"func main >> bar() --"#.to_string();

        let mut prog = core_program(&[("/foo", input)]);
        let fref = Fref::from(("/foo", "main"));
        prog.read_semantics(&fref).unwrap();
    }

    #[test]
    fn test_semantics_external_scope_call()
    {
        let foo_input = r#"func bar:Int -> 3 --"#.to_string();

        let baz_input = r#"
        import /foo

        func main ->
            foo.bar() + 6
        --
        "#
        .to_string();

        let mut prog =
            core_program(&[("/foo", foo_input), ("/baz", baz_input)]);
        let fref = Fref::from(("/baz", "main"));
        prog.read_semantics(&fref).unwrap();
    }

    #[test]
    fn test_semantics_three_level_call()
    {
        let foo_src = r#"datatype Taco --"#.to_string();

        let bar_src = r#"
        export foo.Taco

        func bar t: Taco /Int >> 3 --
        "#
        .to_string();

        let baz_src = r#"
        import /bar
        import /bar/foo.Taco

        func main >>
            bar::bar(Taco) + 6
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
