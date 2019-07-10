use crate::leema::ast2::{self, Ast, AstNode, AstResult, Loc};
use crate::leema::failure::Lresult;
use crate::leema::inter::{Blockstack, LocalType};
use crate::leema::lstr::Lstr;
use crate::leema::proto::{ProtoLib, ProtoModule};
use crate::leema::reg::RegTable;
use crate::leema::struple::StrupleKV;
use crate::leema::val::{FuncType, Type};

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
                    macro_name, a,
                ));
            }
            (a, b) if a > b => {
                return Err(rustfail!(
                    SEMFAIL,
                    "Too few arguments passed to macro {}, expected {}",
                    macro_name, a
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
        let mut macro_replace = MacroReplacement{arg_map, loc};
        Semantics::walk(&mut macro_replace, body.clone())
    }
}

impl<'l> SemanticOp for MacroApplication<'l>
{
    fn pre(&mut self, node: AstNode) -> SemanticResult
    {
        if let Ast::Call(callid, args) = *node.node {
            let optmac = match *callid.node {
                Ast::Id1(macroname) => self.local.find_macro(macroname),
                Ast::Id2(modname, macroname) => {
                    self.proto.get(modname)?.find_macro(macroname)
                }
                _ => None,
            };
            match optmac {
                Some(mac) => {
                    let result = Self::apply_macro(mac, callid.loc, args)?;
                    Ok(SemanticAction::Rewrite(result))
                }
                None => {
                    let node2 = AstNode::new(Ast::Call(callid, args), node.loc);
                    Ok(SemanticAction::Keep(node2))
                }
            }
        } else {
            Ok(SemanticAction::Keep(node))
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

#[derive(Debug)]
struct CaseCheck
{
}

impl SemanticOp for CaseCheck
{
    fn pre(&mut self, node: AstNode) -> SemanticResult
    {
        match *node.node {
            Ast::Case(ast2::CaseType::If, Some(cond), _) => {
                return Err(rustfail!(
                    "semantic_failure",
                    "expected no input for if, found {:?}",
                    cond,
                ));
            }
            _ => Ok(SemanticAction::Keep(node)),
        }
    }
}

struct ClosureCollector
{
    closures: Vec<AstNode>,
}

struct CallCollection<'l>
{
    local: &'l ProtoModule,
    imports: &'l ProtoLib,
    // calls: Vec<Lri>,
    // typecalls: Vec<Lri>,
}

impl<'l> CallCollection<'l>
{
    pub fn new(local: &'l ProtoModule, imports: &'l ProtoLib) -> CallCollection<'l>
    {
        CallCollection {
            local,
            imports,
            // calls: vec![],
            // typecalls: vec![],
        }
    }
}

impl<'l> SemanticOp for CallCollection<'l>
{
    fn pre(&mut self, node: AstNode) -> SemanticResult
    {
        match &*node.node {
            Ast::Id1(_callid) => {
                // let local_type = self.local.types.get(callri)
                // if self.local.types
            }
            _ => {
                // if they're not IDs, we don't care
            }
        }
        Ok(SemanticAction::Keep(node))
    }
}

impl<'l> fmt::Debug for CallCollection<'l>
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "CallCollection")
    }
}

struct ScopeCheck<'p>
{
    blocks: Blockstack,
    local_mod: &'p ProtoModule,
    lib: &'p ProtoLib,
}

impl<'p> ScopeCheck<'p>
{
    pub fn new(local_mod: &'p ProtoModule, lib: &'p ProtoLib) -> ScopeCheck<'p>
    {
        ScopeCheck {
            blocks: Blockstack::new(),
            local_mod,
            lib,
        }
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
                    match &self.local_mod.key.name {
                        Lstr::Sref(smod) => {
                            println!("module str is static: {}", smod);
                            node.node = Box::new(Ast::Id2(smod, id));
                            return Ok(SemanticAction::Rewrite(node));
                        }
                        Lstr::Arc(inner) => {
                            return Err(rustfail!(
                                SEMFAIL,
                                "mod name is not static: {}",
                                inner,
                            ));
                        }
                        Lstr::Cat(_, _) => {
                            panic!("why is Lstr::Cat still here?");
                        }
                    }
                } else {
                    println!("var not in scope: {}", id);
                    return Err(rustfail!(
                        SEMFAIL,
                        "var not in scope: {}",
                        id,
                    ));
                }
            }
            Ast::Id2(module, id) => {
                println!("scope check id2: {}::{}", module, id);
                let proto = self.lib.get(module)
                    .map_err(|e| {
                        e.add_context(Lstr::from(format!(
                            "module {} not found at {:?}", module, node.loc
                        )))
                    })?;
                let val_ast = proto.find_const(id)
                    .ok_or_else(|| {
                        rustfail!(
                            SEMFAIL,
                            "cannot find {} in module {} @ {:?}",
                            id,
                            module,
                            node.loc,
                        )
                    })?;
                return Ok(SemanticAction::Rewrite(val_ast.clone().replace_loc(node.loc)))
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
        Ok(VarTypes {
            vartypes,
            module,
        })
    }

    pub fn decl_pattern_vartypes(&mut self, node: &AstNode) -> Lresult<Type>
    {
        match &*node.node {
            Ast::Id1(id) => {
                let tvar = Type::Var(lstrf!("inferred:{}", id));
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
                node.node = Box::new(Ast::Let(patt, dtype, x));
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

struct TypeCheck<'p>
{
    vartypes: HashMap<&'static str, Type>,
    // infer: Inferator,
    local_mod: &'p ProtoModule,
    lib: &'p ProtoLib,
}

impl<'p> TypeCheck<'p>
{
    pub fn new(local_mod: &'p ProtoModule, lib: &'p ProtoLib) -> TypeCheck<'p>
    {
        TypeCheck {
            vartypes: HashMap::new(),
            local_mod,
            lib,
        }
    }
}

impl<'p> SemanticOp for TypeCheck<'p>
{
    fn pre(&mut self, node: AstNode) -> SemanticResult
    {
        match &*node.node {
            // Ast::Let(patt, dtype, x) => {
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
                    0 => {
                        SemanticAction::Rewrite(AstNode::void())
                    }
                    1 => {
                        SemanticAction::Rewrite(items.pop().unwrap())
                    }
                    _ => {
                        node.node = Box::new(Ast::Block(items));
                        SemanticAction::Keep(node)
                    }
                }
            }
            ast => {
                node.node = Box::new(ast);
                SemanticAction::Keep(node)
            }
        };
        Ok(action)
    }
}

struct Registration
{
    reg: RegTable,
}

// 1
// 2
// 3
// 4

// preop: 1, 2, 3, 4
// advance, anterior, befoe, prior, early, prelude

// postop: 4, 3, 2, 1
// after, subsequent, late, postlude

pub struct Semantics
{
    pub types: HashMap<Lstr, HashMap<Lstr, Type>>,

    // pub calls: HashSet<ModLocalId>,
    // pub typecalls: HashSet<SpecialModId>,
    // pub closed: Option<HashSet<Lstr>>,
    pub src: HashMap<Lstr, HashMap<Lstr, AstNode>>,
}

impl Semantics
{
    pub fn new() -> Semantics
    {
        Semantics {
            types: HashMap::new(),
            src: HashMap::new(),
        }
    }

    pub fn compile(
        &mut self,
        _proto: &mut ProtoLib,
        _module: &str,
    ) -> Lresult<()>
    {
        /*
        while let Some(func_ast) = proto.pop_func(module)? {
            let comp_ast = self.compile_call(proto, func_ast)?;
            println!("compiled func to {:?}", comp_ast);
        }
        */
        Ok(())
    }

    pub fn compile_call(
        &mut self,
        proto: &mut ProtoLib,
        mod_name: &str,
        func_name: &str,
    ) -> AstResult
    {
        let func_ast = proto.pop_func(mod_name, func_name)?;
        let (_args, body) = func_ast.unwrap();

        let local_proto = proto.get(mod_name)?;
        let mut macs: MacroApplication = MacroApplication {
            local: local_proto,
            proto: proto,
        };
        let mut scope_check = ScopeCheck::new(local_proto, proto);
        let mut remove_extra = RemoveExtraCode;
        let mut pipe = SemanticPipeline {
            ops: vec![
                &mut remove_extra,
                &mut macs,
                &mut scope_check,
            ],
        };

        Self::walk(&mut pipe, body)
    }

    pub fn walk<Op: SemanticOp>(op: &mut Op, node: AstNode) -> AstResult
    {
        let mut prenode = match op.pre(node)? {
            SemanticAction::Keep(inode) => inode,
            SemanticAction::Rewrite(inode) => inode,
            SemanticAction::Remove => {
                return Ok(AstNode::void())
            }
        };

        let new_ast = match *prenode.node {
            Ast::Block(children) => {
                let new_children: Lresult<Vec<AstNode>> = children.into_iter().map(|ch| {
                    Self::walk(op, ch)
                }).collect();
                Ast::Block(new_children?)
            }
            Ast::Call(id, args) => {
                let wid = Self::walk(op, id)?;
                let wargs = args.map_v_into(|v| Self::walk(op, v))?;
                Ast::Call(wid, wargs)
            }
            Ast::Case(typ, None, args) => {
                let new_args: Lresult<Vec<ast2::Case>> = args.into_iter().map(|ch| {
                    op.set_pattern(typ == ast2::CaseType::Match);
                    let wcond = Self::walk(op, ch.cond)?;
                    op.set_pattern(false);
                    let wbody = Self::walk(op, ch.body)?;
                    Ok(ast2::Case::new(wcond, wbody))
                }).collect();
                Ast::Case(typ, None, new_args?)
            }
            Ast::Case(typ, Some(cond), children) => {
                let wcond = Self::walk(op, cond)?;
                let wchildren: Lresult<Vec<ast2::Case>> = children.into_iter().map(|ch| {
                    let wcond = Self::walk(op, ch.cond)?;
                    let wbody = Self::walk(op, ch.body)?;
                    Ok(ast2::Case::new(wcond, wbody))
                }).collect();
                Ast::Case(typ, Some(wcond), wchildren?)
            }
            Ast::ConstVal(v) => {
                // can't walk past on const
                Ast::ConstVal(v)
            }
            Ast::DefConst(name, v) => {
                let wval = Self::walk(op, v)?;
                Ast::DefConst(name, wval)
            }
            Ast::DefFunc(name, args, body) => {
                let wname = Self::walk(op, name)?;
                let wargs = args.map_v_into(|arg| {
                    Self::walk(op, arg)
                })?;
                let wbody = Self::walk(op, body)?;
                Ast::DefFunc(wname, wargs, wbody)
            }
            Ast::Generic(id, args) => {
                let wid = Self::walk(op, id)?;
                let wargs = args.map_v_into(|arg| {
                    Self::walk(op, arg)
                })?;
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
                let witems = items.map_v_into(|item| Self::walk(op, item))?;
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
                let witems: Lresult<Vec<AstNode>> = items.into_iter().map(|i| {
                    Self::walk(op, i)
                }).collect();
                Ast::StrExpr(witems?)
            }
            Ast::Tuple(items) => {
                let witems = items.map_v_into(|item| Self::walk(op, item))?;
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
            Ast::Type(_) => unimplemented!(),
            Ast::TypeCall(_, _) => unimplemented!(),
            */
            ast => ast, // do nothing for everything else
        };
        prenode.node = Box::new(new_ast);

        let postnode = match op.post(prenode)? {
            SemanticAction::Keep(inode) => inode,
            SemanticAction::Rewrite(inode) => inode,
            SemanticAction::Remove => {
                return Ok(AstNode::void())
            }
        };

        Ok(postnode)
    }
}

#[cfg(test)]
mod tests
{
    use super::Semantics;
    use crate::leema::ast2::Ast;
    use crate::leema::lstr::Lstr;
    use crate::leema::proto::ProtoLib;

    use matches::assert_matches;


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

        let mut proto = ProtoLib::new();
        proto.add_module(&Lstr::Sref("foo"), input).unwrap();
        let mut semantics = Semantics::new();
        let body = semantics.compile_call(&mut proto, "foo", "main").unwrap();
        assert_matches!(*body.node, Ast::Case(_, _, _));
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
    fn test_semantics_module_scope_call()
    {
        let input = r#"
        func foo >> 5 --

        func main >>
            foo() + 3
        --
        "#;

        let mut proto = ProtoLib::new();
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
        let foo_input = r#"func bar >> 3 --"#;

        let baz_input = r#"
        func main >>
            foo::bar() + 6
        --
        "#;

        let mut proto = ProtoLib::new();
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
    fn test_type_fail()
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
}
