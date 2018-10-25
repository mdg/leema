
use leema::ast::{IfCase, IfType};
use leema::failure::{Failure, Lresult};
use leema::infer::Inferator;
use leema::inter::Blockstack;
use leema::lri::{Lri, ModLocalId, SpecialModId};
use leema::lstr::Lstr;
use leema::reg::{Reg, RegTable};
use leema::struple::{Struple, Struple2, StrupleItem, StrupleKV};
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

    pub fn replace(&self, node: Ast, t: Type) -> AstNode
    {
        AstNode {
            node: Box::new(node),
            loc: self.loc.clone(),
            typ: t,
            dst: self.dst.clone(),
        }
    }

    pub fn set_dst(&mut self, dst: Reg)
    {
        self.dst = dst;
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

    pub fn pass(&mut self, node: &AstNode) -> Lresult<AstNode>
    {
        let result: AstNode = match &*node.node {
            Ast::Block(ref items) => {
                if items.is_empty() {
                    let new_node = node.replace(
                        Ast::Block(Vec::with_capacity(0)),
                        Type::Void,
                    );
                    return Ok(new_node);
                }

                let item_results: Vec<Lresult<AstNode>> = items
                    .iter()
                    .map(|i| self.pass(i))
                    .collect();
                let new_items: Vec<AstNode> = Lresult::from_iter(item_results)?;
                let last_type = {
                    let last_item = new_items.last().unwrap();
                    self.merge_types(&node.typ, &last_item.typ)?
                };
                node.replace(Ast::Block(new_items), last_type)
            }
            Ast::TypeCall(ref id, ref args) => {
                let (new_node, new_type) = self.pass_typecall(id, args)?;
                let new_type2 = self.merge_types(&node.typ, &new_type)?;
                node.replace(new_node, new_type2)
            }
            unknown => {
                return Err(Failure::new("compiler_error", Lstr::from(format!(
                    "cannot analyze: {:?}", unknown
                ))));
            }
        };
        Ok(result)
    }

    pub fn pass_typecall(&mut self, id: &AstNode, args: &Struple2<AstNode>) -> Lresult<(Ast, Type)>
    {
        let new_id = self.pass(id)?;
        let new_args = args.map_v(|a| {
            self.pass(a)
        })?;

        let (result, rtype) = match &new_id.typ {
            Type::GenericFunc(ref targs, ref ft) => {
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
                let arg_vals: Struple<Val> = special_types
                    .iter()
                    .map(|i| (Some(i.k.clone()), Val::Void))
                    .collect();
                let fri = match *new_id.node {
                    Ast::Id(ref localid) => Lri::new(localid.clone()),
                    Ast::ModId(ref modid, ref localid) => {
                        Lri::with_modules(modid.clone(), localid.clone())
                    }
                    not_id => {
                        return Err(Failure::new("code_failure", Lstr::from(
                            format!("not an id: {:?}", not_id)
                        )));
                    }
                };
                // ft.replace_types(special_types);
                // *t = Type::SpecialFunc(special_types.clone(), ft);
                let spec_type = Type::SpecialFunc(special_types, ft.clone());
                let t2 = spec_type.clone();
                (Ast::ConstVal(Val::FuncRef(fri, arg_vals, spec_type)), t2)
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
        };
        Ok((result, rtype))
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
