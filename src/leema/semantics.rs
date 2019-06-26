use crate::leema::ast2::{self, Ast, AstNode, AstResult, Loc};
use crate::leema::failure::Lresult;
use crate::leema::inter::{Blockstack, LocalType};
use crate::leema::lstr::Lstr;
use crate::leema::proto::{ProtoLib, ProtoModule};
use crate::leema::reg::RegTable;
use crate::leema::struple::StrupleKV;
use crate::leema::val::Type;

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
                Ast::Id1(macroname) => self.local.get_macro(macroname)?,
                Ast::Id2(modname, macroname) => {
                    self.proto.get(modname)?.get_macro(macroname)?
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
            Ast::Id2(_module, _callri) => {
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

#[derive(Debug)]
struct ScopeCheck
{
    blocks: Blockstack,
}

impl ScopeCheck
{
    pub fn new() -> ScopeCheck
    {
        ScopeCheck {
            blocks: Blockstack::new(),
        }
    }
}

impl SemanticOp for ScopeCheck
{
    fn pre(&mut self, node: AstNode) -> SemanticResult
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
                if !self.blocks.var_in_scope(&Lstr::Sref(id)) {
                    println!("var not in scope: {}", id);
                    return Err(rustfail!(
                        SEMFAIL,
                        "var not in scope: {}",
                        id,
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
        match *node.node {
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

struct TypeCheck {
    // infer: Inferator,
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

        let mut macs: MacroApplication = MacroApplication {
            local: proto.get(mod_name)?,
            proto: proto,
        };
        let mut scope_check = ScopeCheck::new();
        let mut remove_extra = RemoveExtraCode;
        let mut pipe = SemanticPipeline {
            ops: vec![
                &mut macs,
                &mut scope_check,
                &mut remove_extra,
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
                    let wcond = Self::walk(op, ch.cond)?;
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
            Ast::DefConst(_, _) => {
                return Err(rustfail!(
                    "semantic_failure",
                    "cannot define const at {:?}",
                    prenode.loc,
                ));
            }
            /*
            Ast::DefFunc(name, args, body) => {
                write!(f, "DefFunc {:?} {:?} {:?}", name, args, body)
            }
            Ast::DefMacro(name, args, body) => {
                write!(f, "DefMacro {:?} {:?} {:?}", name, args, body)
            }
            Ast::DefType(dtype, name, fields) => {
                write!(f, "DefType {:?} {:?} {:?}", dtype, name, fields)
            }
            // Ast::Def(v) => write!(f, "Def {}", v),
            Ast::Id1(id) => write!(f, "Id {}", id),
            Ast::Id2(id1, id2) => write!(f, "Id {}::{}", id1, id2),
            Ast::IdGeneric(id, args) => {
                write!(f, "IdGeneric {:?} {:?}", id, args)
            }
            Ast::Import(module) => write!(f, "Import {:?}", module),
            Ast::Let(lhp, _lht, rhs) => write!(f, "Let {:?} := {:?}", lhp, rhs),
            Ast::List(items) => write!(f, "List {:?}", items),
            Ast::Op1(op, node) => write!(f, "Op1 {} {:?}", op, node),
            Ast::Op2(op, a, b) => write!(f, "Op2 {} {:?} {:?}", op, a, b),
            Ast::RustBlock => write!(f, "RustBlock"),
            Ast::StrExpr(items) => write!(f, "Str {:?}", items),
            Ast::Tuple(items) => write!(f, "Tuple {:?}", items),
            Ast::Void => write!(f, "Void"),
            Ast::Wildcard => write!(f, "_"),
            // unimplemented
            Ast::FuncType(_) => unimplemented!(),
            Ast::LessThan3(_, _, _, _, _) => unimplemented!(),
            Ast::Map(_) => unimplemented!(),
            Ast::NewStruct(_, _) => unimplemented!(),
            Ast::NewTuple(_) => unimplemented!(),
            Ast::NewUnion(_, _, _) => unimplemented!(),
            Ast::Return(_) => unimplemented!(),
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
}
