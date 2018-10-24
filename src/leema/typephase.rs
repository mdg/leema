
use leema::ast::{IfCase, IfType};
use leema::failure::{Failure, Lresult};
use leema::infer::Inferator;
use leema::inter::Blockstack;
use leema::lri::ModLocalId;
use leema::lstr::Lstr;
use leema::reg::{Reg, RegTable};
use leema::struple::Struple2;
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
    Type(Type),
    TypeCall(AstNode, Struple2<AstNode>),
    Wildcard,
}


pub struct Semantics<'a>
{
    pub name: Lstr,
    pub calls: HashSet<ModLocalId>,
    pub typecalls: HashMap<ModLocalId, Vec<Type>>,
    pub closed: Option<HashSet<Lstr>>,

    is_closure: bool,
    blocks: Blockstack,
    infer: Inferator<'a>,
    reg: RegTable,
}

impl<'a> Semantics<'a>
{
    pub fn pass(&mut self, mut node: AstNode) -> Lresult<AstNode>
    {
        let result: Ast = match *node.node {
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
                    node.typ = last_item.typ.clone();
                    node.dst = last_item.dst.clone();
                }
                Ast::Block(new_items)
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
}


#[cfg(test)]
mod tests
{
    #[test]
    fn test_semantics()
    {
        // let node = Ast::
        panic!("looks like this ran");
    }
}
