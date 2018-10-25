
use leema::ast::{IfCase, IfType};
use leema::failure::{Failure, Lresult};
use leema::infer::Inferator;
use leema::inter::Blockstack;
use leema::lri::{Lri, ModLocalId, SpecialModId};
use leema::lstr::Lstr;
use leema::reg::{Reg, RegTable};
use leema::struple::{Struple2, StrupleItem, StrupleKV};
use leema::val::{SrcLoc, Type, Val};

use std::collections::{HashMap, HashSet};
use std::iter::FromIterator;


#[derive(Debug)]
pub struct AstNode
{
    node: Box<Ast>,
    loc: SrcLoc,
    typ: Type,
    dst: Reg,
}

impl AstNode
{
    pub fn new(node: Ast, loc: SrcLoc) -> AstNode
    {
        AstNode {
            node: Box::new(node),
            loc,
            typ: Type::Void,
            dst: Reg::Void,
        }
    }
}

#[derive(Debug)]
pub enum Ast
{
    Block(Vec<AstNode>),
    Call(AstNode, Struple2<AstNode>),
    Cons(AstNode, AstNode),
    ConstVal(Val),
    DefFunc(AstNode, Struple2<AstNode>, AstNode, AstNode),
    DefMacro(AstNode, Struple2<AstNode>, AstNode),
    DefStruct(AstNode, Struple2<AstNode>),
    DefUnion(AstNode, Struple2<AstNode>),
    DotAccess(AstNode, Lstr),
    GenericId(AstNode, Struple2<Lstr>),
    Id(Lstr),
    Ifx(IfType, AstNode, IfCase),
    Import(Lstr),
    Let(AstNode, AstNode, AstNode),
    List(Struple2<AstNode>),
    Map(Struple2<AstNode>),
    ModId(Lstr, Lstr),
    NewStruct(AstNode, Struple2<AstNode>),
    NewTuple(Struple2<AstNode>),
    NewUnion(AstNode, Lstr, Struple2<AstNode>),
    Return(AstNode),
    RustBlock,
    StrExpr(Vec<AstNode>),
    SpecialId(AstNode, StrupleKV<Lstr, Type>),
    Type(Type),
    TypeCall(AstNode, Struple2<AstNode>),
    Wildcard,
}


pub struct Semantics<'a>
{
    pub name: Lstr,
    imports: HashSet<Lstr>,
    pub types: HashMap<Lstr, HashMap<Lstr, Type>>,

    pub calls: HashSet<ModLocalId>,
    pub typecalls: HashSet<SpecialModId>,
    pub closed: Option<HashSet<Lstr>>,

    is_closure: bool,
    blocks: Blockstack,
    infer: Inferator<'a>,
    reg: RegTable,
}

impl<'a> Semantics<'a>
{
    pub fn new(name: &'a Lri) -> Semantics<'a>
    {
        Semantics {
            name: name.localid.clone(),
            imports: HashSet::new(),
            types: HashMap::new(),

            calls: HashSet::new(),
            typecalls: HashSet::new(),
            closed: None,

            is_closure: false,
            blocks: Blockstack::new(),
            infer: Inferator::new(&name),
            reg: RegTable::new(),
        }
    }

    pub fn pass(&mut self, mut node: AstNode) -> Lresult<AstNode>
    {
        let inner_node = node.node;
        let result: Ast = match *inner_node {
            Ast::Block(items) => {
                if items.is_empty() {
                    node.node = Box::new(Ast::Block(items));
                    return Ok(node);
                }

                let item_results: Vec<Lresult<AstNode>> = items
                    .into_iter()
                    .map(|i| self.pass(i))
                    .collect();
                let new_items: Vec<AstNode> = Lresult::from_iter(item_results)?;
                {
                    let last_item = new_items.last().unwrap();
                    node.typ = self.merge_types(&node.typ, &last_item.typ)?;
                    node.dst = last_item.dst.clone();
                }
                Ast::Block(new_items)
            }
            Ast::TypeCall(id, args) => {
                self.pass_typecall(id, args)?
            }
            unknown => {
                return Err(Failure::new("compiler_error", Lstr::from(format!(
                    "cannot analyze: {:?}", unknown
                ))));
            }
        };
        node.node = Box::new(result);
        Ok(node)
    }

    pub fn pass_typecall(&mut self, id: AstNode, args: Struple2<AstNode>) -> Lresult<Ast>
    {
        let new_id = self.pass(id)?;
        let new_args = args.map_v_into(|a| {
            self.pass(a)
        })?;
        match &new_id.typ {
            Type::GenericFunc(ref targs, ref _ft) => {
                // ok, that's good
                let targs_len = targs.len();
                let new_args_len = new_args.len();
                if targs_len != new_args_len {
                    return Err(Failure::new("code_error", Lstr::from(format!(
                        "expected {} arguments to {:?}, found {}",
                        targs_len, new_id, new_args_len
                    ))));
                }
                let special_types: StrupleKV<Lstr, Type> = targs
                    .iter()
                    .zip(new_args.into_iter().unwrap())
                    .map(|arg| {
                        StrupleItem::new(arg.0.clone(), arg.1.v.typ)
                    })
                    .collect();
                // ft.replace_types(special_types);
                // *t = Type::SpecialFunc(special_types.clone(), ft);
                Ok(Ast::SpecialId(new_id, special_types))
            }
            Type::Func(_) => {
                return Err(Failure::new("code_error", Lstr::from(format!(
                    "cannot pass type args to regular function: {:?}", new_id
                ))));
            }
            not_func => {
                return Err(Failure::new("code_failure", Lstr::from(format!(
                    "typecall id must be type func: {:?}", not_func
                ))));
            }
        }
    }

    pub fn merge_types(&mut self, a: &Type, b: &Type) -> Lresult<Type>
    {
        self.infer
            .merge_types(a, b)
            .map_err(|_e| Failure::new("type_err", Lstr::Sref("e context")))
    }
}


#[cfg(test)]
mod tests
{
    use super::*;
    use leema::lri::Lri;
    use leema::lstr::Lstr;
    use leema::struple::StrupleKV;
    use leema::val::{SrcLoc, Type};


    fn new_node(node: Ast) -> AstNode
    {
        AstNode::new(node, SrcLoc::default())
    }

    #[test]
    fn test_typecall()
    {
        let lri = Lri::with_modules(Lstr::Sref("a"), Lstr::Sref("b"));
        let mut sem = Semantics::new(&lri);

        let typecall = new_node(Ast::TypeCall(
            new_node(Ast::ModId(Lstr::Sref("c"), Lstr::Sref("d"))),
            StrupleKV::from(vec![
                new_node(Ast::Type(Type::Int)),
                new_node(Ast::Type(Type::Str)),
            ]),
        ));
        let result = sem.pass(typecall).unwrap();

        assert_matches!(*result.node, Ast::SpecialId(_, _));
    }
}
