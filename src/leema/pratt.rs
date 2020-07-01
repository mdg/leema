use crate::leema::ast2::{AstNode, AstResult};
use crate::leema::parser::Rule;

use std::iter::Peekable;

use pest::iterators::Pair;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Placement
{
    Infix,
    Prefix,
    Postfix,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Assoc
{
    Left,
    Right,
}

impl Assoc
{
    pub fn next_prec(self, prec: u32) -> u32
    {
        match self {
            Assoc::Left => prec + 1,
            Assoc::Right => prec,
        }
    }
}

struct LeemaPrec;

impl LeemaPrec
{
    fn prec(_placement: Placement, _rule: Rule) -> Option<&'static (u32, Assoc)>
    {
        // self.ops.get(&(placement, rule))
        None
    }

    fn primary<'i>(_op: Pair<'i, Rule>) -> AstResult
    {
        Ok(AstNode::void())
    }

    fn unary<'i>(_op: Pair<'i, Rule>, _a: AstNode) -> AstResult
    {
        Ok(AstNode::void())
    }

    fn binary<'i>(_a: AstNode, _op: Pair<'i, Rule>, _b: AstNode) -> AstResult
    {
        Ok(AstNode::void())
    }
}

pub fn parse<'i, P>(pairs: P) -> AstResult
where
    P: Iterator<Item = Pair<'i, Rule>>,
{
    parse_0(0, &mut pairs.peekable())
}

fn parse_0<'i, P>(
    min_prec: u32,
    pairs: &mut Peekable<P>,
) -> AstResult
where
    P: Iterator<Item = Pair<'i, Rule>>,
{
    let first = pairs
            .next()
            .expect("precedence climbing requires a non-empty Pairs");
    match LeemaPrec::prec(Placement::Prefix, first.as_rule()) {
        Some((pre_prec, _)) => {
            if *pre_prec < min_prec {
                LeemaPrec::primary(first)
            } else {
                let mut peeks = pairs.peekable();
                let rhs = parse_0(*pre_prec, &mut peeks)?;
                LeemaPrec::unary(first, rhs)
            }
        }
        None => {
            let lhs = LeemaPrec::primary(first)?;
            let mut peeks = pairs.peekable();
            parse_1(lhs, min_prec, &mut peeks)
        }
    }
}

fn parse_1<'i, P>(
    mut lhs: AstNode,
    min_prec: u32,
    pairs: &mut Peekable<P>,
) -> AstResult
where
    P: Iterator<Item = Pair<'i, Rule>>,
{
    while pairs.peek().is_some() {
        let rule = pairs.peek().unwrap().as_rule();
        if let Some((post_prec, _)) = LeemaPrec::prec(Placement::Postfix, rule) {
            if *post_prec < min_prec {
                // postfix operator of lower precedence. stop here.
                break;
            }
            let next = pairs.next().unwrap();
            lhs = LeemaPrec::unary(next, lhs)?;
        } else if let Some((in_prec, assoc)) = LeemaPrec::prec(Placement::Infix, rule) {
            if *in_prec < min_prec {
                break;
            }

            let next_prec = assoc.next_prec(*in_prec);
            let op = pairs.next().unwrap();
            let rhs = parse_0(next_prec, pairs)?;
            lhs = LeemaPrec::binary(lhs, op, rhs)?;
        } else {
            break;
        }
    }

    Ok(lhs)
}
