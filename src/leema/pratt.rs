use crate::leema::ast2::{AstNode, AstResult};
use crate::leema::parser::{LeemaPrec, Prec, Rule};

use std::iter::Peekable;

use pest::iterators::Pair;

pub fn parse<P>(parser: &LeemaPrec, pairs: P) -> AstResult
where
    P: Iterator<Item = Pair<'static, Rule>>,
{
    parse_0(parser, Prec::default(), &mut pairs.peekable())
}

fn parse_0<P>(
    parser: &LeemaPrec,
    min_prec: Prec,
    pairs: &mut Peekable<P>,
) -> AstResult
where
    P: Iterator<Item = Pair<'static, Rule>>,
{
    let first = pairs
            .next()
            .expect("precedence climbing requires a non-empty Pairs");
    match parser.prefix_prec(first.as_rule()) {
        Some(pre_prec) => {
            if pre_prec < min_prec {
                parser.primary(first)
            } else {
                let rhs = parse_0(parser, pre_prec, pairs)?;
                parser.unary(first, rhs)
            }
        }
        None => {
            let lhs = parser.primary(first)?;
            parse_1(parser, lhs, min_prec, pairs)
        }
    }
}

fn parse_1<P>(
    parser: &LeemaPrec,
    mut lhs: AstNode,
    min_prec: Prec,
    pairs: &mut Peekable<P>,
) -> AstResult
where
    P: Iterator<Item = Pair<'static, Rule>>,
{
    while pairs.peek().is_some() {
        let rule = pairs.peek().unwrap().as_rule();
        if let Some(post_prec) = parser.postfix_prec(rule) {
            if post_prec < min_prec {
                // postfix operator of lower precedence. stop here.
                break;
            }
            let next = pairs.next().unwrap();
            lhs = parser.unary(next, lhs)?;
        } else if let Some((in_prec, assoc)) = parser.infix_prec(rule) {
            if in_prec < min_prec {
                break;
            }

            let next_prec = assoc.next_prec(in_prec);
            let op = pairs.next().unwrap();
            let rhs = parse_0(parser, next_prec, pairs)?;
            lhs = parser.binary(lhs, op, rhs)?;
        } else {
            break;
        }
    }

    Ok(lhs)
}
