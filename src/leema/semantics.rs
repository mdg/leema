use crate::leema::ast2::{
    self, Ast, AstMode, AstNode, AstResult, AstStep, Loc, StepResult, Xlist,
};
use crate::leema::canonical::Canonical;
use crate::leema::failure::{self, Failure, Lresult};
use crate::leema::inter::Blockstack;
use crate::leema::loader::Interloader;
use crate::leema::lstr::Lstr;
use crate::leema::module::ModKey;
use crate::leema::proto::{self, ProtoLib, ProtoModule};
use crate::leema::struple::{self, StrupleItem, StrupleKV};
use crate::leema::val::{
    Fref, FuncTypeRef, FuncTypeRefMut, LocalTypeVars, Type, TypeArg,
    TypeArgSlice, TypeArgs, TypeRef, Val,
};

use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::mem;

use lazy_static::lazy_static;


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

const CLOSED_VAR: &'static str = "closed-var";

lazy_static! {
    static ref INFIX_CANONICALS: HashMap<&'static str, Canonical> = {
        let mut infix = HashMap::new();
        infix.insert("+", canonical!("/core.int_add"));
        infix.insert("-", canonical!("/core.int_sub"));
        infix.insert("*", canonical!("/core.int_mult"));
        infix.insert("/", canonical!("/core.int_div"));
        infix.insert("modulo", canonical!("/core.int_mod"));
        infix.insert(";", canonical!("/core.cons"));
        infix.insert("and", canonical!("/core.boolean_and"));
        infix.insert("or", canonical!("/core.boolean_or"));
        infix.insert("==", canonical!("/core.int_equal"));
        infix.insert("!=", canonical!("/core.not_equal"));
        infix.insert("<", canonical!("/core.int_less_than"));
        infix.insert("<=", canonical!("/core.int_lteq"));
        infix.insert(">", canonical!("/core.int_gt"));
        infix.insert(">=", canonical!("/core.int_gteq"));
        infix
    };
    static ref PREFIX_CANONICALS: HashMap<&'static str, Canonical> = {
        let mut prefix = HashMap::new();
        prefix.insert("-", canonical!("/core.int_negate"));
        prefix.insert("not", canonical!("/core.boolean_not"));
        prefix
    };
    static ref ESCAPED_STR: HashMap<&'static str, &'static str> = {
        let mut escaped = HashMap::new();
        escaped.insert("\\n", "\n");
        escaped.insert("\\\"", "\"");
        escaped
    };
}

const ANON_FUNC_TYPES: [&'static str; 5] = ["A.0", "A.1", "A.2", "A.3", "A.4"];

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

struct CloseVars
{
    closed: TypeArgs,
}

impl CloseVars
{
    pub fn new(closed: TypeArgs) -> CloseVars
    {
        CloseVars { closed }
    }
}

impl ast2::Op for CloseVars
{
    fn post(&mut self, node: &mut AstNode, mode: AstMode) -> StepResult
    {
        match &mut *node.node {
            Ast::Id(ref idname) if mode == AstMode::Value => {
                let found = struple::find_idx(&self.closed, idname);
                if let Some(typ) = found {
                    let mut closed =
                        AstNode::new(Ast::Id(CLOSED_VAR), node.loc);
                    closed.typ = Type::tuple(self.closed.clone());
                    let mut field = AstNode::new(Ast::Id(idname), node.loc);
                    field.typ = typ.1.clone();
                    *node.node = Ast::Op2(".", closed, field);
                    node.typ = typ.1.clone();
                }
            }
            Ast::Id(_idname) if mode.is_pattern() => {}
            _ => {} // do nothing
        }
        Ok(AstStep::Ok)
    }
}

struct ScopeCheck<'p>
{
    lib: &'p ProtoLib,
    local_mod: &'p ProtoModule,
    blocks: Blockstack,
    anons: StrupleKV<&'static str, AstNode>,
    ftyp: &'p FuncTypeRef<'p>,
    type_args: &'p TypeArgSlice,
    next_local_id: u32,
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
            anons: vec![],
            ftyp,
            type_args: &ftyp.type_args,
            next_local_id: 0,
        })
    }

    fn closure(&self, ftyp: &'p FuncTypeRef) -> ScopeCheck<'p>
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
        ScopeCheck {
            lib: self.lib,
            local_mod: self.local_mod,
            blocks: Blockstack::with_args(args),
            anons: vec![],
            ftyp,
            type_args: &ftyp.type_args,
            next_local_id: self.next_local_id + 1,
        }
    }

    fn localized_id(&mut self, loc: &Loc) -> String
    {
        let local_id = self.next_local_id;
        self.next_local_id += 1;
        format!("{}@{}", local_id, loc.lineno)
    }

    fn localize_node(&mut self, node: &mut AstNode)
    {
        if !node.typ.contains_open() {
            return;
        }
        let local_id = self.localized_id(&node.loc);
        self.localize_node_with_id(node, local_id)
    }

    fn localize_node_with_id(&mut self, node: &mut AstNode, local_id: String)
    {
        node.typ
            .localize_generics(&self.type_args, local_id.clone());
        match &mut *node.node {
            Ast::ConstVal(Val::Call(fref, _)) => {
                fref.t.localize_generics(&self.type_args, local_id);
            }
            Ast::ConstVal(Val::Struct(t, _)) => {
                t.localize_generics(&self.type_args, local_id);
            }
            Ast::ConstVal(Val::EnumStruct(t, _, _)) => {
                t.localize_generics(&self.type_args, local_id);
            }
            Ast::ConstVal(Val::EnumToken(t, _)) => {
                t.localize_generics(&self.type_args, local_id);
            }
            Ast::ConstVal(Val::Token(t)) => {
                t.localize_generics(&self.type_args, local_id);
            }
            Ast::Generic(base, type_args) => {
                for ta in type_args.iter_mut() {
                    self.localize_node_with_id(&mut ta.v, local_id.clone());
                }
                self.localize_node_with_id(base, local_id);
            }
            Ast::Call(callx, args) => {
                for ta in args.iter_mut() {
                    self.localize_node_with_id(&mut ta.v, local_id.clone());
                }
                self.localize_node_with_id(callx, local_id);
            }
            Ast::Tuple(items) => {
                for i in items.iter_mut() {
                    self.localize_node_with_id(&mut i.v, local_id.clone());
                }
            }
            Ast::Op2(_, ref mut a, ref mut b) => {
                self.localize_node_with_id(a, local_id.clone());
                self.localize_node_with_id(b, local_id.clone());
            }
            Ast::Type(t) => {
                t.localize_generics(&self.type_args, local_id);
            }
            Ast::Id(_id) => {
                // shouldn't have to localize ids
            }
            _ => {} // not a val that needs to be localized
        }
    }

    fn infix_to_call(
        op: &'static str,
        a: &mut AstNode,
        b: &mut AstNode,
        loc: Loc,
    ) -> Option<AstNode>
    {
        let infix = INFIX_CANONICALS.get(op)?;
        let callx = AstNode::new(Ast::Canonical(infix.clone()), loc);
        let new_a = mem::take(a);
        let new_b = mem::take(b);
        let args: Xlist = struple::new_tuple2(new_a, new_b);
        Some(AstNode::new(Ast::Call(callx, args), loc))
    }

    fn prefix_to_call(
        op: &'static str,
        x: &mut AstNode,
        loc: Loc,
    ) -> Option<AstNode>
    {
        let prefix = PREFIX_CANONICALS.get(op)?;
        let callx = AstNode::new(Ast::Canonical(prefix.clone()), loc);
        let new_x = mem::take(x);
        let args: Xlist = vec![StrupleItem::new_v(new_x)];
        Some(AstNode::new(Ast::Call(callx, args), loc))
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

    fn pre_closure(
        &mut self,
        loc: Loc,
        mut result: AstNode,
        mut args: Xlist,
        mut body: AstNode,
    ) -> Lresult<AstNode>
    {
        // totally hacky to make this a static str. need to figure this out.
        let name = format!("anon_fn_{}", self.localized_id(&loc));
        let name = Interloader::static_str(name);
        // initialize type_args w/ wrapping type args
        let mut type_args = Xlist::with_capacity(self.type_args.len());
        let mut type_type_args = TypeArgs::with_capacity(self.type_args.len());
        for ta in self.type_args.iter() {
            if let Lstr::Sref(a) = ta.k {
                type_args.push(StrupleItem::new(
                    Some(a),
                    AstNode::new(Ast::Type(ta.v.clone()), body.loc),
                ));
                type_type_args.push(StrupleItem::new(
                    ta.k.clone(),
                    Type::open(ta.k.clone()),
                ));
            } else {
                panic!("not a static Lstr: {}", ta.k);
            }
        }
        let mut next_type_it = ANON_FUNC_TYPES.iter();
        if *result.node == Ast::NOTOKEN {
            let next_type = next_type_it.next().unwrap();
            type_args.push(StrupleItem::new(Some(next_type), AstNode::void()));
            type_type_args.push(StrupleItem::new(
                Lstr::Sref(next_type),
                Type::open(Lstr::Sref(next_type)),
            ));
            *result.node = Ast::Id(next_type);
        }
        for a in args.iter_mut() {
            if let Some(_k) = a.k {
                // already typed, cool.
            } else if let Ast::Id(var) = &*a.v.node {
                // convert arg name to k and add open type var
                let next_type_name = next_type_it.next().unwrap();
                a.k = Some(var);
                *a.v.node = Ast::Id(next_type_name);
                type_args.push(StrupleItem::new(
                    Some(next_type_name),
                    AstNode::void(),
                ));
                type_type_args.push(StrupleItem::new(
                    Lstr::Sref(next_type_name),
                    Type::open(Lstr::Sref(next_type_name)),
                ));
                // let next_type = Type::open(Lstr::Sref(next_type_name));
                // arg_types.push(StrupleItem::new(Some(var), next_type));
            }
        }
        let mut impl_type_args = type_type_args.clone();
        let ftyp =
            ltry!(self.local_mod.ast_to_ftype(&result, &args, &type_type_args));

        // scope check the closure/anonymous function
        let ftyp_ref = ftyp.func_ref().unwrap();
        let mut closure_scope = self.closure(&ftyp_ref);
        ltry!(ast2::walk_ref_mut(&mut body, &mut closure_scope));
        self.anons.append(&mut closure_scope.anons);

        let fref = Fref::new(self.local_mod.key.clone(), name, ftyp.clone());
        let fname = AstNode::new(Ast::Id(name), loc);
        let def_name_node: AstNode;
        let mut result_node: AstNode;

        if !closure_scope.blocks.out_of_scope().is_empty() {
            // this is a closure
            let out_of_scope = closure_scope.blocks.out_of_scope();
            let mut closed_type: Xlist = Vec::with_capacity(out_of_scope.len());
            let mut closed_vars: Xlist = Vec::with_capacity(out_of_scope.len());
            let mut vars_to_close = Vec::with_capacity(out_of_scope.len());
            let mut cl_type_args: TypeArgs = Vec::with_capacity(args.len());
            for var in closure_scope.blocks.out_of_scope() {
                self.blocks.access_var(var.0, *var.1);
                let next_type_name = next_type_it.next().unwrap();
                closed_type.push(StrupleItem::new(
                    Some(var.0),
                    AstNode::new(Ast::Id(next_type_name), loc),
                ));
                let open_typ = Type::open(Lstr::Sref(next_type_name));
                impl_type_args.push(StrupleItem::new(
                    Lstr::Sref(next_type_name),
                    open_typ.clone(),
                ));
                cl_type_args.push(StrupleItem::new(
                    Lstr::Sref(next_type_name),
                    open_typ.clone(),
                ));
                vars_to_close
                    .push(StrupleItem::new(Lstr::Sref(*var.0), open_typ));

                closed_vars.push(StrupleItem::new(
                    Some(var.0),
                    AstNode::new(Ast::Id(var.0), loc),
                ));
            }

            // iterate body again and replace closed vars w/ "closed-arg.<var>"
            let mut closed_step = CloseVars::new(vars_to_close);
            ltry!(ast2::walk_ref_mut(&mut body, &mut closed_step));

            // last param of impl func is tuple of vars w/ name "closed var"
            args.push(StrupleItem::new(
                Some(CLOSED_VAR),
                AstNode::new(Ast::Tuple(closed_type), loc),
            ));
            // create the impl function by name
            // create closure struct w/ the impl fref and the closed tuple
            let closure_impl_t = Type::closure(ftyp.clone(), impl_type_args);
            let closed_type = Type::tuple(cl_type_args);

            def_name_node = AstNode::void();
            let cl_type_call = AstNode::new(
                Ast::Generic(
                    AstNode::new(
                        Ast::Canonical(canonical!(Type::PATH_CLOSURE)),
                        loc,
                    ),
                    vec![
                        StrupleItem::new_v(AstNode::new(
                            Ast::Type(closure_impl_t.clone()),
                            loc,
                        )),
                        StrupleItem::new_v(AstNode::new(
                            Ast::Type(closed_type),
                            loc,
                        )),
                    ],
                ),
                loc,
            )
            .with_type(closure_impl_t.clone());
            let cl_impl_ref = Fref::new(fref.m.clone(), fref.f, closure_impl_t);
            let impl_ref_val: Lresult<Val> = From::from(&cl_impl_ref);
            let impl_ref_ast = Ast::ConstVal(ltry!(impl_ref_val));
            let closure_args = vec![
                StrupleItem::new_v(AstNode::new(impl_ref_ast, loc)),
                StrupleItem::new_v(AstNode::new(Ast::Tuple(closed_vars), loc)),
            ];
            result_node =
                AstNode::new(Ast::Call(cl_type_call, closure_args), loc);
        } else {
            // this is an anoymous function w/ no closed vars

            // create the static function by name
            // return Ast::Const(Val::Call)

            def_name_node = if type_args.is_empty() {
                fname.clone()
            } else {
                AstNode::new(Ast::Generic(fname, type_args), loc)
            };

            // take the node as a new function, leave a call in its place
            let call: Lresult<Val> = From::from(&fref);
            result_node = AstNode::new_constval(ltry!(call), loc);
        }

        result_node.typ = fref.t.clone();
        let mut def_func_node =
            AstNode::new(Ast::DefFunc(def_name_node, args, result, body), loc);
        def_func_node.typ = fref.t;
        self.localize_node(&mut def_func_node);
        self.anons.push(StrupleItem::new(name, def_func_node));
        Ok(result_node)
    }

    fn pre_scope(&mut self, node: &mut AstNode, mode: AstMode) -> StepResult
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
                node.dst = ltry!(
                    self.blocks.assign_var(id, local_type),
                    "file": self.local_mod.key.best_path(),
                    "line": ldisplay!(node.loc.lineno),
                );
                if node.typ == Type::UNKNOWN {
                    let type_var = if *id == "_" {
                        lstrf!("_{}", self.localized_id(&node.loc))
                    } else {
                        Lstr::Sref(id)
                    };
                    node.typ = Type::local(type_var);
                }
                *node.node = Ast::ConstVal(Val::PatternVar(node.dst));
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
                if let Some(ctype) = struple::find(self.type_args, &id) {
                    let type_ast = Ast::Type(ctype.clone());
                    *node = AstNode::new(type_ast, loc);
                    return Ok(AstStep::Rewrite);
                } else if let Some(r) = self.blocks.var_in_scope(id) {
                    node.dst = r;
                } else if let Some(me) = self.local_mod.find_modelem(id) {
                    node.replace((*me.node).clone(), me.typ.clone());
                    return Ok(AstStep::Rewrite);
                } else if let Some(b) = proto::find_builtin(id) {
                    node.replace_node(b.clone());
                    return Ok(AstStep::Rewrite);
                } else {
                    // write the var as accessed, will check if it's
                    // out of scope or enclosed later
                    self.blocks.access_var(id, node.loc.lineno as i16);
                }
            }
            Ast::Call(callx, args) => {
                // go depth first on the call expression to find any macros
                // alternatively, maybe apply macros in post?
                ltry!(ast2::walk_ref_mut(callx, self));
                match &mut *callx.node {
                    mac @ Ast::DefMacro(_, _, _) => {
                        // should this happen in post?
                        *node = Self::apply_macro(mac, node.loc, args)?;
                        return Ok(AstStep::Rewrite);
                    }
                    _other => {} // do nothing
                }
            }
            Ast::Canonical(c) => {
                if self.lib.path_proto(c).is_ok() {
                    // in scope, this is fine
                } else if let Some((parent, id)) = c.split_function() {
                    // what case is this?
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
                match (&*base_node.node, &*sub_node.node) {
                    (Ast::Id(id), Ast::Id(sub)) => {
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
                                        return Ok(AstStep::Rewrite);
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
                    (Ast::Canonical(c), Ast::Id(sub)) => {
                        let me =
                            ltry!(self.lib.exported_elem(c, sub, node.loc));
                        node.replace((*me.node).clone(), me.typ.clone());
                    }
                    (other_base, other_sub) => {
                        // else it's probably (hopefully?) a method or something
                        eprintln!("{:?}", other_base);
                        eprintln!("{:?}", other_sub);
                        unimplemented!();
                    }
                }
            }
            Ast::Op2(op, a, b) if mode == AstMode::Value => {
                if let Some(n2) = Self::infix_to_call(op, a, b, node.loc) {
                    *node = n2;
                    return Ok(AstStep::Rewrite);
                }
            }
            Ast::Op1("\\n", x) => {
                let newline =
                    AstNode::new_constval(Val::Str(Lstr::Sref("\n")), node.loc);
                let strx = Ast::StrExpr(vec![mem::take(x), newline]);
                *node = AstNode::new(strx, node.loc);
                return Ok(AstStep::Rewrite);
            }
            Ast::Op1(op, x) => {
                if let Some(n2) = Self::prefix_to_call(op, x, node.loc) {
                    *node = n2;
                    return Ok(AstStep::Rewrite);
                }
            }
            Ast::ConstVal(Val::Call(fref, _args)) => {
                // for functions, if any types match function type args
                // replace them w/ the concrete types
                // if function defines any new type args, replace those
                // parameter types with local variabls
                if node.typ.contains_open() {
                    let local_id = format!(
                        "{}.{}-{}",
                        fref.m.name,
                        fref.f,
                        self.localized_id(&node.loc),
                    );
                    fref.t.localize_generics(&self.type_args, local_id);
                    node.typ = fref.t.clone();
                    // does this really need a rewrite?
                    return Ok(AstStep::Rewrite);
                }
            }
            Ast::ConstVal(Val::Type(t)) => {
                if t.contains_open() {
                    let local_id = self.localized_id(&node.loc);
                    t.localize_generics(&self.type_args, local_id);
                }
            }
            Ast::ConstVal(Val::Str(escaped)) => {
                // escaped strings
                if let Some(raw) = ESCAPED_STR.get(escaped.as_str()) {
                    *node = AstNode::new(
                        Ast::ConstVal(Val::Str(Lstr::Sref(raw))),
                        node.loc,
                    );
                }
                // else do nothing for unescaped strings
            }
            // for single element tuples, just take the single item
            Ast::Tuple(items) if items.len() == 1 => {
                if items.first().unwrap().k.is_none() {
                    *node = items.pop().unwrap().v;
                    return Ok(AstStep::Rewrite);
                }
            }
            Ast::Tuple(items) if mode == AstMode::Type => {
                let items2: Lresult<TypeArgs> = items
                    .drain(..)
                    .enumerate()
                    .map(|i| {
                        let k_lstr = i.1.k.map(|ik| Lstr::Sref(ik));
                        let k = Type::unwrap_name(&k_lstr, i.0);
                        let t = ltry!(self
                            .local_mod
                            .ast_to_type(&i.1.v, &self.type_args));
                        Ok(TypeArg::new(k, t))
                    })
                    .collect();
                *node.node = Ast::Type(Type::tuple(ltry!(items2)));
                return Ok(AstStep::Rewrite);
            }
            Ast::Matchx(None, cases) => {
                let node_loc = node.loc;
                let args: Lresult<Xlist> = self
                    .ftyp
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
            Ast::DefFunc(_name, ref_args, ref_result, ref_body) => {
                // collect all the undefined variables in body
                // validate their scope in this func
                // if there are closed vars, create a new func
                // make fname a globally unique id later, mix in args
                let result = mem::take(ref_result);
                let args = mem::take(ref_args);
                let body = mem::take(ref_body);
                *node = ltry!(self.pre_closure(node.loc, result, args, body));
                return Ok(AstStep::Rewrite);
            }
            _ => {
                // do nothing otherwise
            }
        }
        Ok(AstStep::Ok)
    }

    /// Handle a node that might be a type that needs to
    /// be replaced by a constructor
    fn post_constructor(&mut self, x: &mut AstNode) -> StepResult
    {
        if let Ast::Canonical(c) = &*x.node {
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
                let mut cons = optcons.unwrap().clone();
                self.localize_node(&mut cons);
                x.replace(*cons.node, cons.typ);
                return Ok(AstStep::Rewrite);
            }
        }
        Ok(AstStep::Ok)
    }
}

impl<'p> ast2::Op for ScopeCheck<'p>
{
    fn pre(&mut self, node: &mut AstNode, mode: AstMode) -> StepResult
    {
        steptry!(self.pre_scope(node, mode));
        // make sure that all open types have been converted
        // to concrete types or local type vars
        self.localize_node(node);
        if node.typ.contains_open() {
            Err(lfail!(
                failure::Mode::TypeFailure,
                "expression contains unexpected open generic type variable",
                "type": ldisplay!(node.typ),
                "node": ldebug!(node.node),
                "file": self.local_mod.key.best_path(),
                "line": ldisplay!(node.loc.lineno),
            ))
        } else {
            Ok(AstStep::Ok)
        }
    }

    fn post(&mut self, node: &mut AstNode, mode: AstMode) -> StepResult
    {
        match &mut *node.node {
            Ast::Block(_) => {
                self.blocks.pop_blockscope();
            }
            Ast::Call(callx, _) if mode == AstMode::Value => {
                steptry!(self.post_constructor(callx));
            }
            Ast::Call(callx, _) if mode.is_pattern() => {
                steptry!(self.post_constructor(callx));
            }
            Ast::Generic(inner, _) if mode == AstMode::Value => {
                steptry!(self.post_constructor(inner));
            }
            Ast::List(items) if mode == AstMode::Type => {
                if items.len() != 1 {
                    return Err(lfail!(
                        failure::Mode::TypeFailure,
                        "list types must have one element",
                        "num_items": ldisplay!(items.len()),
                        "items": ldebug!(items),
                    ));
                }
                let inner = match &*items[0].v.node {
                    Ast::ConstVal(Val::Type(t)) => t.clone(),
                    Ast::Type(t) => t.clone(),
                    other => {
                        return Err(lfail!(
                            failure::Mode::TypeFailure,
                            "unexpected inner list type",
                            "type": ldebug!(other),
                        ));
                    }
                };
                let list_t = Type::list(inner);
                *node.node = Ast::ConstVal(Val::Type(list_t));
                return Ok(AstStep::Rewrite);
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
    /// types of specific variables
    vartypes: HashMap<&'static str, Type>,
    infers: HashMap<Lstr, Type>,
    calls: Vec<Fref>,
    next_local_index: u32,
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
            result: Type::UNKNOWN,
            vartypes: HashMap::new(),
            infers: HashMap::new(),
            calls: vec![],
            next_local_index: 0,
        };

        for arg in ftyp.args.iter() {
            let argname = arg.k.sref()?;
            let argt = ltry!(check.inferred_type(&arg.v));
            check.vartypes.insert(argname, argt);
        }
        check.result = ltry!(check.inferred_type(&ftyp.result));
        Ok(check)
    }

    pub fn add_anon_func_args(&mut self, ftyp: &FuncTypeRef<'p>)
        -> Lresult<()>
    {
        for arg in ftyp.args.iter() {
            let argname = arg.k.sref()?;
            if self.vartypes.contains_key(argname) {
                let old_arg = self.vartypes.get(argname).unwrap().clone();
                dbg!(ltry!(self.match_type(&arg.v, &old_arg)));
            } else {
                let argt = ltry!(self.inferred_type(&arg.v));
                self.vartypes.insert(argname, argt);
            }
        }
        Ok(())
    }

    pub fn inferred_local(&self, local_tvar: &Lstr) -> Type
    {
        self.infers
            .get(local_tvar.as_ref())
            .map(|t| t.clone())
            .unwrap_or_else(|| Type::local(local_tvar.clone()))
    }

    pub fn inferred_type(&self, t: &Type) -> Lresult<Type>
    {
        let newt = match t.type_ref() {
            TypeRef(Type::PATH_LOCAL, [StrupleItem { k: local, .. }, ..]) => {
                self.inferred_local(local)
            }
            TypeRef(Type::PATH_OPENVAR, [StrupleItem { k: open, .. }, ..]) => {
                return Err(lfail!(
                    failure::Mode::TypeFailure,
                    "cannot infer open type var",
                    "type": open.clone(),
                ));
            }
            _ => {
                ltry!(t.map_v(&|a| self.inferred_type(a)), "type": ldisplay!(t),)
            }
        };
        Ok(newt)
    }

    /// match one type to another
    /// should be no open type vars
    pub fn match_type(&mut self, t0: &Type, t1: &Type) -> Lresult<Type>
    {
        if t0.contains_open() {
            panic!("unexpected open type variable: {:?}", t0);
        }
        if t1.contains_open() {
            panic!("unexpected open type variable: {:?}", t1);
        }
        match (t0.path_str(), t1.path_str()) {
            // open var cases should not happen
            (Type::PATH_OPENVAR, _) => {
                let k0 = &t0.first_arg()?.k;
                return Err(lfail!(
                    failure::Mode::TypeFailure,
                    "unexpected open generic",
                    "type": k0.clone(),
                ));
            }
            (_, Type::PATH_OPENVAR) => {
                let k1 = &t1.first_arg()?.k;
                return Err(lfail!(
                    failure::Mode::TypeFailure,
                    "unexpected open generic",
                    "type": k1.clone(),
                ));
            }
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
                // unpuncated type vars are types for variables,
                // keep those in favor of generated type names
                let v0p = Type::is_punctuated(v0);
                let v1p = Type::is_punctuated(v1);
                match (v0p, v1p) {
                    (false, true) => return Ok(ltry!(self.infer_type(v0, t1))),
                    (true, false) => return Ok(ltry!(self.infer_type(v1, t0))),
                    _ => {} // fall through to alpha comparison
                }
                match Ord::cmp(v0, v1) {
                    Ordering::Equal => Ok(self.inferred_local(v0)),
                    Ordering::Less => {
                        lfailoc!(self.infer_type(v0, t1))
                    }
                    Ordering::Greater => {
                        lfailoc!(self.infer_type(v1, t0))
                    }
                }
            }
            (Type::PATH_LOCAL, _) => {
                let v0 = &t0.first_arg()?.k;
                lfailoc!(self.infer_type(v0, t1))
            }
            (_, Type::PATH_LOCAL) => {
                let v1 = &t1.first_arg()?.k;
                lfailoc!(self.infer_type(v1, t0))
            }
            (Type::PATH_FN, Type::PATH_FN) => self.match_func_type(t0, t1),
            (Type::PATH_FN, Type::PATH_CLOSURE) => self.match_func_type(t0, t1),
            (Type::PATH_CLOSURE, Type::PATH_FN) => self.match_func_type(t0, t1),
            (Type::PATH_CLOSURE, Type::PATH_CLOSURE) => {
                self.match_func_type(t0, t1)
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
                        let v = ltry!(self.match_type(&iz.0.v, &iz.1.v));
                        Ok(StrupleItem::new(k, v))
                    })
                    .collect();
                Ok(Type::new(t0.path.clone(), im?))
            }
            // case for alias types where the types may need to be
            // dereferenced before they will match
            _ => {
                if let Some(r) = self.match_type_alias(&t0.path, t1)? {
                    return Ok(r);
                }
                if let Some(r) = self.match_type_alias(&t1.path, t0)? {
                    return Ok(r);
                }
                Err(lfail!(
                    failure::Mode::TypeFailure,
                    "types do not match",
                    "t0": ldisplay!(t0),
                    "t1": ldisplay!(t1),
                ))
            }
        }
    }

    fn match_func_type(&mut self, t0: &Type, t1: &Type) -> Lresult<Type>
    {
        let f0 = t0.func_ref().unwrap();
        let f1 = t1.func_ref().unwrap();
        let result = self.match_type(f0.result, f1.result)?;
        if f0.args.len() != f1.args.len() {
            return Err(rustfail!(
                "compile_error",
                "function arg count mismatch: {:?} != {:?}",
                f0,
                f1,
            ));
        }
        let mut args: TypeArgs = Vec::with_capacity(f0.args.len());
        for a in f0.args.iter().zip(f1.args.iter()) {
            let a2 = self.match_type(&a.0.v, &a.1.v)?;
            let k = a.0.k.clone();
            args.push(StrupleItem::new(k, a2));
        }
        Ok(Type::f(result, args))
    }

    // check if one of these type 0 is an alias for type 1
    fn match_type_alias(
        &mut self,
        u0: &Canonical,
        t1: &Type,
    ) -> Lresult<Option<Type>>
    {
        if let Ok(proto) = self.lib.path_proto(u0) {
            if let Some(alias) = proto.alias_type() {
                return Ok(Some(ltry!(self.match_type(alias, t1))));
            }
            if let Some(supert) = proto.trait_with_impl(t1) {
                return Ok(Some(supert.clone()));
            }
        }
        Ok(None)
    }

    pub fn infer_type(&mut self, var: &Lstr, t: &Type) -> Lresult<Type>
    {
        if self.infers.contains_key(var) {
            let var_type = self.inferred_local(var);
            if var_type == *t {
                Ok(var_type)
            } else {
                Ok(ltry!(
                    self.match_type(&var_type, t),
                    "inferring type var": var.clone(),
                    "as type": lstrf!("{}", var_type),
                ))
            }
        } else {
            self.infers.insert(var.clone(), t.clone());
            Ok(t.clone())
        }
    }

    /// What does apply_typecall do?
    pub fn apply_typecall(
        &mut self,
        calltype: &mut Type,
        args: &mut ast2::Xlist,
    ) -> Lresult<Type>
    {
        let fref = calltype.try_func_ref_mut()?;

        if args.len() != fref.type_args.len() {
            return Err(lfail!(
                failure::Mode::TypeFailure,
                "wrong number of type args",
                "expected_num": ldisplay!(fref.type_args.len()),
                "found_num": ldisplay!(args.len()),
                "expected": ldebug!(fref.type_args),
                "found": ldebug!(args),
            ));
        }
        for a in fref.type_args.iter_mut().zip(args.iter_mut()) {
            let next_type = match &*a.1.v.node {
                Ast::ConstVal(Val::Type(t)) => t,
                Ast::Type(t) => t,
                Ast::Id(id) => {
                    self.local_mod.find_type(id).ok_or_else(|| {
                        lfail!(
                            failure::Mode::TypeFailure,
                            "undefined type",
                            "type": Lstr::Sref(id),
                            "file": self.local_mod.key.best_path(),
                            "line": ldisplay!(a.1.v.loc.lineno),
                        )
                    })?
                }
                what => {
                    self.local_mod.ast_to_type(&a.1.v, &[])?;
                    panic!("unexpected type setting: {:#?}", what);
                }
            };
            a.0.v = ltry!(self.match_type(&a.0.v, next_type));
        }
        calltype.close_openvars();
        let t = ltry!(self.inferred_type(&calltype));
        *calltype = t;
        Ok(calltype.clone())
    }

    pub fn applied_call_type(
        &mut self,
        calltype: &mut Type,
        args: &mut ast2::Xlist,
    ) -> Lresult<Type>
    {
        let mut funcref = ltry!(calltype.try_func_ref_mut());
        Ok(ltry!(
            self.match_argtypes(&mut funcref, args),
            "calltype": ldisplay!(calltype),
            "args": ldebug!(args),
        ))
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
            return Err(lfail!(
                failure::Mode::TypeFailure,
                "too few arguments in call",
                "expected": ldisplay!(ftyp.args.len()),
                "found": ldisplay!(args.len()),
                "file": self.local_mod.key.best_path(),
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
            let typ = ltry!(
                self.match_type(&arg.0.v, &arg.1.v.typ),
                "function_param": arg.0.k.clone(),
                "expected": lstrf!("{}", arg.0.v),
                "found": lstrf!("{}", arg.1.v.typ),
                "loc": lstrf!("{:?}", arg.1.v.loc),
                "file": lstrf!(
                    "{}:{}",
                    self.local_mod.key.best_path(),
                    arg.1.v.loc.lineno,
                ),
            );
            arg.0.v = typ.clone();
            arg.1.v.typ = typ;
        }

        for arg in ftyp.args.iter_mut().zip(args.iter_mut()) {
            let inferred = ltry!(self.inferred_type(&arg.0.v));
            arg.0.v = inferred.clone();
            arg.1.v.typ = inferred;
        }

        *ftyp.result = ltry!(self.inferred_type(&ftyp.result));
        Ok((*ftyp.result).clone())
    }

    fn match_case_types(&mut self, cases: &Vec<ast2::Case>) -> Lresult<Type>
    {
        let mut prev_typ: Option<Type> = None;
        for case in cases.iter() {
            let next_typ = if let Some(ref pt) = prev_typ {
                ltry!(self.match_type(pt, &case.body.typ))
            } else {
                case.body.typ.clone()
            };
            prev_typ = Some(next_typ);
        }
        Ok(prev_typ.unwrap())
    }

    fn next_local_typevar(&mut self) -> Type
    {
        let local_index = self.next_local_index;
        self.next_local_index += 1;
        Type::local(lstrf!("local-{}", local_index))
    }

    fn post_call(
        &mut self,
        call_typ: &mut Type,
        fref: &mut Fref,
        args: &mut Xlist,
        loc: Loc,
    ) -> Lresult<AstStep>
    {
        if fref.t.contains_open() {
            return Err(lfail!(
                failure::Mode::TypeFailure,
                "unexpected open type variable",
                "type": ldisplay!(fref.t),
                "module": fref.m.name.as_lstr().clone(),
                "function": Lstr::Sref(fref.f),
                "file": self.local_mod.key.best_path(),
                "line": ldisplay!(loc.lineno),
            ));
        }

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

    /// like post_call but for function objects where fref isn't known
    /// at compile time
    fn post_var_call(
        &mut self,
        result_typ: &mut Type,
        call_typ: &mut Type,
        args: &mut Xlist,
        loc: Loc,
    ) -> Lresult<AstStep>
    {
        // an optimization here might be to iterate over
        // ast args and initialize any constants
        // actually better might be to stop having
        // the args in the Val::Call const?
        *result_typ =
            ltry!(self.applied_call_type(call_typ, args).map_err(|f| {
                f.lstr_loc(self.local_mod.key.best_path(), loc.lineno as u32)
            }));
        Ok(AstStep::Ok)
    }

    fn post_field_access(
        &self,
        expr_typ: &mut Type,
        base_typ: &Type,
        fld: &mut AstNode,
    ) -> Lresult<AstStep>
    {
        match (base_typ.type_ref(), &*fld.node) {
            (TypeRef(tname, targs), Ast::Id(f)) if targs.is_empty() => {
                // find a field in a struct or interface or
                // whatever else
                let tproto = ltry!(self.lib.path_proto(&base_typ.path));

                if let Some(found) = tproto.find_method(f) {
                    match &*found.node {
                        Ast::ConstVal(_) => {
                            fld.replace(
                                (*found.node).clone(),
                                found.typ.clone(),
                            );
                        }
                        Ast::DataMember(fld_typ, _) => {
                            fld.replace((*found.node).clone(), fld_typ.clone());
                            *expr_typ = fld_typ.clone();
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
                return Err(lfail!(
                    failure::Mode::TypeFailure,
                    "builtin type has no field",
                    "field": Lstr::Sref(id),
                    "type": ldisplay!(base_typ),
                    "file": self.local_mod.key.name.to_lstr(),
                    "line": ldisplay!(fld.loc.lineno),
                ));
            }
            (_, f) => {
                panic!("unsupported field name: {:#?}", f);
            }
        }
    }

    /// TypeCheck post check
    fn post_check(&mut self, node: &mut AstNode, mode: AstMode) -> StepResult
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
                let id_type = ltry!(self.inferred_type(&node.typ));
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
                        callx.typ = ltry!(self.inferred_type(&fref.t));
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
                        return Err(lfail!(
                            failure::Mode::TypeFailure,
                            "generic not previously handled",
                            "name": ldebug!(base),
                            "type_args": ldebug!(type_args),
                        ));
                    }
                    // handle closure call
                    // do they get handled like regular non-method calls
                    // and rely on the callx.typ?
                    _func_obj => {
                        steptry!(self.post_var_call(
                            &mut node.typ,
                            &mut callx.typ,
                            args,
                            node.loc,
                        ));
                    }
                }
            }
            // field access, maybe a method
            Ast::Op2(".", a, b) => {
                stepok!(ltry!(
                    self.post_field_access(&mut node.typ, &a.typ, b),
                    "a": ldebug!(&a),
                    "b": ldebug!(&b),
                    "node": ldebug!(&node),
                ));
            }
            Ast::Generic(ref mut callx, ref mut args) => {
                if let Ast::ConstVal(Val::Call(ref mut fref, _)) =
                    &mut *callx.node
                {
                    let typecall_t = ltry!(
                        self.apply_typecall(&mut fref.t, args),
                        "module": fref.m.name.to_lstr(),
                        "function": Lstr::Sref(fref.f),
                        "file": self.local_mod.key.best_path(),
                        "line": ldisplay!(node.loc.lineno),
                    );
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
                for case in cases.iter_mut() {
                    ltry!(self.match_type(&case.cond.typ, &Type::BOOL,));
                    case.cond.typ = Type::BOOL.clone();
                }
                node.typ = self.match_case_types(cases)?;
            }
            Ast::Matchx(None, _) => {
                return Err(lfail!(
                    failure::Mode::TypeFailure,
                    "no input to match",
                    "file": self.local_mod.key.best_path(),
                    "line": ldisplay!(node.loc.lineno),
                ));
            }
            Ast::Matchx(Some(ref mut input), ref mut cases) => {
                for case in cases.iter_mut() {
                    let it = self
                        .match_type(&input.typ, &case.cond.typ)
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
            Ast::Let(ref mut patt, ref mut let_typ, ref mut x) => {
                let mut typ = ltry!(self.match_type(&patt.typ, &x.typ));
                if *let_typ.node == Ast::NOTOKEN {
                    // no type was given, stick w/ patt <=> x
                } else if let Ast::ConstVal(Val::Type(t)) = &*let_typ.node {
                    typ = ltry!(self.match_type(&t, &typ));
                    let_typ.typ = Type::KIND;
                } else {
                    return Err(lfail!(
                        failure::Mode::TypeFailure,
                        "unexpected let type node",
                        "type": ldebug!(let_typ),
                    ));
                }
                patt.typ = typ.clone();
                x.typ = typ;
            }
            Ast::List(ref mut inner) => {
                let mut inner_typ = Type::UNKNOWN;
                for i in inner.iter_mut() {
                    inner_typ = ltry!(self.match_type(&i.v.typ, &inner_typ));
                    if i.v.typ != inner_typ {
                        i.v.typ = inner_typ.clone();
                    }
                }
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
            Ast::Op2(";", h, t) if mode.is_pattern() => {
                // show that [h] = t
                let hlist = Type::list(h.typ.clone());
                node.typ = self.match_type(&hlist, &t.typ)?;
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

impl<'p> ast2::Op for TypeCheck<'p>
{
    fn pre(&mut self, node: &mut AstNode, mode: AstMode) -> StepResult
    {
        if node.typ.contains_open() {
            return Err(lfail!(
                failure::Mode::TypeFailure,
                "unexpected open type variable",
                "node": ldebug!(&node.node),
                "open": ldisplay!(&node.typ),
                "module": self.local_mod.key.name.to_lstr(),
                "line": ldisplay!(node.loc.lineno),
            ));
        }
        match &mut *node.node {
            Ast::Id("_") if node.typ == Type::UNKNOWN => {
                node.typ = self.next_local_typevar();
            }
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
            Ast::ConstVal(c) if node.typ.contains_open() => {
                return Err(lfail!(
                    failure::Mode::TypeFailure,
                    "unexpected open type for constant",
                    "type": ldisplay!(node.typ),
                    "val": ldisplay!(c),
                    "file": self.local_mod.key.best_path(),
                    "line": ldisplay!(node.loc.lineno),
                ));
            }
            Ast::Call(callx, args) if mode.is_pattern() => {
                // what should this change to? struct val?
                if let Ast::ConstVal(Val::Call(fref, _cargs)) = &mut *callx.node
                {
                    if fref.f == "__construct" {
                        if let Some(ftyp) = fref.t.func_ref_mut() {
                            let mut pargs = Vec::with_capacity(args.len());
                            let mut targs = Vec::with_capacity(args.len());
                            for a in args.drain(..).zip(ftyp.args.into_iter()) {
                                let at =
                                    ltry!(self.match_type(&a.0.v.typ, &a.1.v));
                                targs.push(StrupleItem::new(a.1.k.clone(), at));
                                match *a.0.v.node {
                                    Ast::ConstVal(v) => {
                                        pargs.push(StrupleItem::new(
                                            a.0.k.map(|k| Lstr::Sref(k)),
                                            v,
                                        ))
                                    }
                                    other => {
                                        return Err(lfail!(
                                            failure::Mode::TypeFailure,
                                            "unexpected pattern value",
                                            "value": ldebug!(other),
                                        ));
                                    }
                                }
                            }
                            let result_t =
                                ltry!(self.inferred_type(&ftyp.result));
                            let pattern = if fref.m.name == ftyp.result.path {
                                Val::Struct(ftyp.result.clone(), pargs)
                            } else {
                                let split_f = fref.m.name.last_module();
                                if let Some(var) = split_f {
                                    Val::EnumStruct(
                                        ftyp.result.clone(),
                                        var.clone(),
                                        pargs,
                                    )
                                } else {
                                    dbg!(&ftyp.result);
                                    panic!("this is no good");
                                }
                            };
                            *node.node = Ast::ConstVal(pattern);
                            node.typ = result_t;
                            return Ok(AstStep::Rewrite);
                        } else {
                            return Err(lfail!(
                                failure::Mode::TypeFailure,
                                "expected function type",
                                "module": fref.m.name.to_lstr(),
                                "function": Lstr::Sref(fref.f),
                                "type": ldisplay!(fref.t),
                            ));
                        }
                    } else {
                        return Err(lfail!(
                            failure::Mode::TypeFailure,
                            "unexpected pattern call function",
                            "module": fref.m.name.to_lstr(),
                            "function": Lstr::Sref(fref.f),
                        ));
                    }
                } else {
                    return Err(lfail!(
                        failure::Mode::TypeFailure,
                        "cannot process non-const pattern call",
                        "node": ldebug!(callx.node),
                        "file": self.local_mod.key.best_path(),
                        "line": ldisplay!(node.loc.lineno),
                    ));
                }
            }
            Ast::Wildcard => {
                node.typ = Type::UNKNOWN;
            }
            Ast::Let(_, _, _) => {
                // a let statment has no return type
                node.typ = Type::VOID;
            }
            Ast::Op2(_, _, _) => {
                // handled in post
            }
            Ast::Block(_) | Ast::Call(_, _) | Ast::Tuple(_) => {
                // handled in post
            }
            Ast::ConstVal(Val::Type(utb)) if utb.is_untyped_block() => {
                node.typ = self.result.clone();
            }
            _ => {
                // should handle matches later, but for now it's fine
            }
        }
        Ok(AstStep::Ok)
    }

    /// TypeCheck post
    fn post(&mut self, node: &mut AstNode, mode: AstMode) -> StepResult
    {
        steptry!(self.post_check(node, mode));
        // make sure that the type has been fully resolved and
        // is no longer an open generic
        if node.typ.contains_open() {
            Err(lfail!(
                failure::Mode::TypeFailure,
                "expression contains unresolved open generic type variable",
                "type": ldisplay!(node.typ),
                "node": ldebug!(node.node),
                "file": self.local_mod.key.best_path(),
                "line": ldisplay!(node.loc.lineno),
            ))
        } else {
            Ok(AstStep::Ok)
        }
    }
}

impl<'l> fmt::Debug for TypeCheck<'l>
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "TypeCheck({})", self.local_mod.key.name)
    }
}

/// Resolve any unresolved type variables
struct ResolveTypes<'l>
{
    key: ModKey,
    infers: &'l LocalTypeVars,
    pub calls: HashSet<Fref>,
}

impl<'l> ResolveTypes<'l>
{
    pub fn new(key: ModKey, infers: &'l LocalTypeVars) -> ResolveTypes
    {
        ResolveTypes {
            key,
            infers,
            calls: HashSet::new(),
        }
    }

    pub fn infer_local_typevar(&self, t: &mut Type) -> Lresult<()>
    {
        if t.contains_local() {
            t.replace_localvars(&self.infers)?;
        }
        Ok(())
    }
}

impl<'l> ast2::Op for ResolveTypes<'l>
{
    fn pre(&mut self, node: &mut AstNode, _mode: AstMode) -> StepResult
    {
        if node.typ == Type::UNKNOWN {
            return Err(lfail!(
                failure::Mode::TypeFailure,
                "unknown node type",
                "type": ldisplay!(node.typ),
                "node": ldebug!(node.node),
            ));
        }
        match &mut *node.node {
            Ast::Type(t) if t.contains_local() => {
                ltry!(t.replace_localvars(&self.infers));
                Ok(AstStep::Rewrite)
            }
            Ast::ConstVal(cv) => {
                match cv {
                    Val::Call(f, _args) => {
                        if f.t.contains_local() {
                            ltry!(f.t.replace_localvars(&self.infers));
                            return Ok(AstStep::Rewrite);
                        }
                    }
                    Val::Struct(t, _fields) => {
                        if t.contains_local() {
                            ltry!(t.replace_localvars(&self.infers));
                            return Ok(AstStep::Rewrite);
                        }
                    }
                    Val::EnumStruct(t, _var, _fields) => {
                        if t.contains_local() {
                            ltry!(t.replace_localvars(&self.infers));
                            return Ok(AstStep::Rewrite);
                        }
                    }
                    Val::EnumToken(t, _var) => {
                        if t.contains_local() {
                            ltry!(t.replace_localvars(&self.infers));
                            return Ok(AstStep::Rewrite);
                        }
                    }
                    _ => {} // nothing to do?
                }
                Ok(AstStep::Ok)
            }
            other if node.typ.contains_local() => {
                ltry!(
                    node.typ.replace_localvars(&self.infers),
                    "node": ldebug!(other),
                    "type": ldisplay!(node.typ),
                    "file": self.key.best_path(),
                    "line": ldisplay!(node.loc.lineno),
                );
                Ok(AstStep::Rewrite)
            }
            _ => Ok(AstStep::Ok),
        }
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
    pub vartypes: HashMap<&'static str, Type>,
    pub infers: HashMap<Lstr, Type>,
    pub calls: HashSet<Fref>,
    pub anons: Vec<Ast>,
}

impl Semantics
{
    pub fn new() -> Semantics
    {
        Semantics {
            src: AstNode::void(),
            args: Vec::new(),
            vartypes: HashMap::new(),
            infers: HashMap::new(),
            calls: HashSet::new(),
            anons: vec![],
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
        if *body.node == Ast::BLOCK_ABSTRACT {
            return Err(rustfail!(
                "compile_failure",
                "cannot execute abstract function: {}.{}",
                modname,
                f.f,
            ));
        }
        let proto = lib.path_proto(&modname)?;

        let func_ref = proto.find_method(&f.f).ok_or_else(|| {
            rustfail!(SEMFAIL, "cannot find func ref for {}", f,)
        })?;

        let func_typ = if f.t == Type::UNKNOWN {
            &func_ref.typ
        } else if func_ref.typ.is_generic() {
            &f.t
        } else {
            &func_ref.typ
        };
        if func_typ.contains_open() {
            return Err(lfail!(
                failure::Mode::TypeFailure,
                "cannot compile open generic function",
                "module": f.m.name.as_lstr().clone(),
                "function": Lstr::Sref(f.f),
                "type": ldisplay!(func_typ),
                "called_type": ldisplay!(f.t),
                "found_type": ldisplay!(func_ref.typ),
            ));
        }
        if func_typ.contains_local() {
            return Err(lfail!(
                failure::Mode::TypeFailure,
                "cannot compile func with local variable",
                "module": f.m.name.as_lstr().clone(),
                "function": Lstr::Sref(f.f),
                "type": ldisplay!(func_typ),
                "called_type": ldisplay!(f.t),
                "found_type": ldisplay!(func_ref.typ),
            ));
        }
        let ftyp = ltry!(func_typ.try_func_ref());

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

        // check scope and apply macros
        let (scoped, mut anons) = {
            let mut scope_check = ScopeCheck::new(lib, proto, &ftyp)?;
            let result = ltry!(
                ast2::walk(body, &mut scope_check),
                "module": modname.as_lstr().clone(),
                "function": Lstr::Sref(f.f),
                "type": ldebug!(ftyp),
            );
            if !scope_check.blocks.out_of_scope().is_empty() {
                return Err(lfail!(
                    failure::Mode::ScopeFailure,
                    "undefined variables",
                    "module": modname.as_lstr().clone(),
                    "function": Lstr::Sref(f.f),
                    "variables": ldebug!(scope_check.blocks.out_of_scope()),
                ));
            }
            (result, scope_check.anons)
        };

        // type check and remove unnecessary code
        let mut type_check = TypeCheck::new(lib, proto, &ftyp)?;
        let mut result = ltry!(
            ast2::walk(scoped, &mut type_check),
            "module": modname.as_lstr().clone(),
            "function": Lstr::Sref(f.f),
            "type": ldebug!(ftyp),
        );
        if result.typ.is_untyped_block() {
            result.typ = ftyp.result.clone();
        } else if *ftyp.result == Type::VOID {
            result.typ = Type::VOID;
        } else {
            result.typ =
                ltry!(type_check.match_type(&ftyp.result, &result.typ));
        }

        for an in anons.iter_mut() {
            if let Ast::DefFunc(_name, _args, _result, body) = &mut *an.v.node {
                let anon_ftyp = an.v.typ.func_ref().unwrap();
                ltry!(type_check.add_anon_func_args(&anon_ftyp));
                ltry!(ast2::walk_ref_mut(body, &mut type_check));
                ltry!(type_check.match_type(&anon_ftyp.result, &body.typ));
            } else {
                panic!("not a func: {:?}", an);
            }
        }

        let mut resolver =
            ResolveTypes::new(proto.key.clone(), &type_check.infers);
        let mut remove_extra = RemoveExtraCode;
        let mut resolve_pipe =
            ast2::Pipeline::new(vec![&mut remove_extra, &mut resolver]);
        let resolved = ltry!(
            ast2::walk(result, &mut resolve_pipe),
            "module": modname.as_lstr().clone(),
            "function": Lstr::Sref(f.f),
            "type": ldebug!(ftyp),
        );

        if *ftyp.result != resolved.typ && *ftyp.result != Type::VOID {
            return Err(rustfail!(
                SEMFAIL,
                "bad return type in {}.{}, expected: {}, found {}",
                modname,
                f.f,
                ftyp.result,
                resolved.typ,
            ));
        }

        sem.calls = type_check.calls.into_iter().collect();
        sem.infers = type_check.infers;
        sem.vartypes = type_check.vartypes;
        sem.src = resolved;

        if !anons.is_empty() {
            let proto_mut = lib.path_proto_mut(&f.m.name).unwrap();
            for a in anons {
                ltry!(
                    proto_mut.add_definition(a.v),
                    "func": Lstr::Sref(a.k),
                );
            }
        }
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
    use crate::leema::struple::{self, StrupleItem};
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
    fn compile_let_with_type()
    {
        let input = r#"
        func bar ->
            let x: Int := 5
            x + 4
        --
        "#
        .to_string();

        let mut prog = core_program(&[("/foo", input)]);
        let fref = Fref::from(("/foo", "bar"));
        prog.read_semantics(&fref).unwrap();
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
    fn scopecheck_anon_func()
    {
        let input = r#"
        func main:Int ->
            let x2 := fn::i -> i * 2 --
            x2(3)
        --
        "#
        .to_string();

        let mut prog = core_program(&[("/foo", input)]);
        let fref = Fref::with_modules(From::from("/foo"), "main");
        let sem = prog.read_semantics(&fref).unwrap();
        println!("infers: {:#?}\n", sem.infers);
        assert_eq!(5, sem.infers.len());
    }

    #[test]
    #[ignore]
    fn test_compile_closure()
    {
        let input = r#"
        func bar:Int ->
            let mult := 4
            let x_mult := fn::i -> i * mult --
            x_mult(3)
        --
        "#
        .to_string();

        let mut prog = core_program(&[("/foo", input)]);
        let fref = Fref::with_modules(From::from("/foo"), "bar");
        let sem = prog.read_semantics(&fref).unwrap();
        println!("infers: {:#?}\n", sem.infers);
        assert_eq!(5, sem.infers.len());
    }

    #[test]
    fn test_compile_generic_constructor()
    {
        let input = r#"
        datatype <Taco T> :: Int T --

        func main ->
            let t := <Taco Str>(3, "hello")
        --
        "#
        .to_string();

        let mut prog = core_program(&[("/foo", input)]);
        let fref = Fref::with_modules(From::from("/foo"), "main");
        let sem = prog.read_semantics(&fref).unwrap();
        println!("infers: {:#?}\n", sem.infers);
        assert_eq!(2, sem.infers.len());
        // mainly making sure this doesn't fail
    }

    #[test]
    fn scopecheck_undefined_variable()
    {
        // 4;[6]
        let input = r#"
        func main ->
            let y := x
        --
        "#
        .to_string();

        let mut prog = core_program(&[("/foo", input)]);
        let fref = Fref::with_modules(From::from("/foo"), "main");
        let result = prog.read_semantics(&fref);
        let fail = result.unwrap_err();
        assert_eq!("scope_failure", fail.tag.str());
        assert_eq!("undefined variables", fail.msg.str());
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
        dbg!(prog.read_semantics(&fref)).unwrap();
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
        assert_eq!("type_failure", f.tag.str());
        assert_eq!("types do not match", f.msg.str());
        let ctx0 = f.context.first().unwrap();
        assert_eq!("/core/Str", struple::find(ctx0, "t0").unwrap());
        assert_eq!("/core/Int", struple::find(ctx0, "t1").unwrap());
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
        let local_t = Type::local(Lstr::Sref("t"));
        let ftyp = Type::f(
            local_t.clone(),
            vec![StrupleItem::new(Lstr::Sref("a"), local_t.clone())],
        );
        let expected = Type::f(
            Type::STR,
            vec![StrupleItem::new(Lstr::Sref("a"), Type::STR)],
        );
        let lib = ProtoLib::new();
        let proto = ProtoModule::new(ModKey::from("foo"), "").unwrap();
        let mainf = Type::f(Type::VOID, vec![]);
        let mainfref = mainf.func_ref().unwrap();
        let mut type_check = TypeCheck::new(&lib, &proto, &mainfref).unwrap();
        type_check.match_type(&local_t, &Type::STR).unwrap();
        let inferred = type_check.inferred_type(&ftyp).unwrap();
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
    fn struct_field_list()
    {
        let baz_input = r#"
        datatype Foo ::
            x:Int
            y:Str
        --

        func main ->
            let flds := Foo.__void
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
