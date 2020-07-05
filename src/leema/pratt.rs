use crate::leema::ast2::{AstNode, AstResult};
use crate::leema::parser::{LeemaPrec, Placement, Rule};

use std::iter::Peekable;

use pest::iterators::Pair;

pub fn parse<P>(parser: &LeemaPrec, pairs: P) -> AstResult
where
    P: Iterator<Item = Pair<'static, Rule>>,
{
    parse_0(parser, 0, &mut pairs.peekable())
}

fn parse_0<P>(
    parser: &LeemaPrec,
    min_prec: i32,
    pairs: &mut Peekable<P>,
) -> AstResult
where
    P: Iterator<Item = Pair<'static, Rule>>,
{
    let first = pairs
            .next()
            .expect("precedence climbing requires a non-empty Pairs");
    match parser.prec(Phase::Zero, first.as_rule()) {
        Some((pre_prec, _)) => {
            if *pre_prec < min_prec {
                parser.primary(first)
            } else {
                let mut peeks = pairs.peekable();
                let rhs = parse_0(*pre_prec, &mut peeks)?;
                parser.unary(first, rhs)
            }
        }
        None => {
            let lhs = parser.primary(first)?;
            let mut peeks = pairs.peekable();
            parse_1(parser, lhs, min_prec, &mut peeks)
        }
    }
}

fn parse_1<P>(
    parser: &LeemaPrec,
    mut lhs: AstNode,
    min_prec: i32,
    pairs: &mut Peekable<P>,
) -> AstResult
where
    P: Iterator<Item = Pair<'static, Rule>>,
{
    while pairs.peek().is_some() {
        let rule = pairs.peek().unwrap().as_rule();
        if let Some((post_prec, _)) = parser.prec(Phase::Zero, rule) {
            if *post_prec < min_prec {
                // postfix operator of lower precedence. stop here.
                break;
            }
            let next = pairs.next().unwrap();
            lhs = parser.unary(next, lhs)?;
        } else if let Some((in_prec, assoc)) = parser.prec(Phase::One, rule) {
            if *in_prec < min_prec {
                break;
            }

            let next_prec = assoc.next_prec(*in_prec);
            let op = pairs.next().unwrap();
            let rhs = parse_0(next_prec, pairs)?;
            lhs = parser.binary(lhs, op, rhs)?;
        } else {
            break;
        }
    }

    Ok(lhs)
}
