use crate::leema::ast2::{AstNode, AstResult};
use crate::leema::parser::{self, LeemaPrec, Prec, Rule};

use std::iter::Peekable;

use pest::iterators::Pair;

pub fn parse<P>(parser: &LeemaPrec, m: parser::Mode, pairs: P) -> AstResult
where
    P: Iterator<Item = Pair<'static, Rule>>,
{
    parse_0(parser, m, Prec::default(), &mut pairs.peekable())
}

fn parse_0<P>(
    parser: &LeemaPrec,
    mode: parser::Mode,
    min_prec: Prec,
    pairs: &mut Peekable<P>,
) -> AstResult
where
    P: Iterator<Item = Pair<'static, Rule>>,
{
    if pairs.peek().is_none() {
        return Err(rustfail!(
            "compile_failure",
            "parsing precedence requires at least one pair",
        ));
    }

    let first = pairs.next().unwrap();
    match parser.prefix_prec(mode, first.as_rule()) {
        Some(pre_prec) => {
            if pre_prec < min_prec {
                parser.primary(mode, first)
            } else {
                let rhs = parse_0(parser, mode, pre_prec, pairs)?;
                let lhs = parser.unary(mode, first, rhs)?;
                parse_1(parser, mode, lhs, min_prec, pairs)
            }
        }
        None => {
            let lhs = parser.primary(mode, first)?;
            parse_1(parser, mode, lhs, min_prec, pairs)
        }
    }
}

fn parse_1<P>(
    parser: &LeemaPrec,
    mode: parser::Mode,
    mut lhs: AstNode,
    min_prec: Prec,
    pairs: &mut Peekable<P>,
) -> AstResult
where
    P: Iterator<Item = Pair<'static, Rule>>,
{
    while pairs.peek().is_some() {
        let rule = pairs.peek().unwrap().as_rule();
        if let Some(post_prec) = parser.postfix_prec(mode, rule) {
            if post_prec < min_prec {
                // postfix operator of lower precedence. stop here.
                break;
            }
            let next = pairs.next().unwrap();
            lhs = parser.unary(mode, next, lhs)?;
        } else if let Some((in_prec, assoc)) = parser.infix_prec(mode, rule) {
            if in_prec < min_prec {
                break;
            }

            let next_prec = assoc.next_prec(in_prec);
            let op = pairs.next().unwrap();
            let rhs = parse_0(parser, mode, next_prec, pairs)?;
            lhs = parser.binary(mode, lhs, op, rhs)?;
        } else {
            break;
        }
    }

    Ok(lhs)
}
