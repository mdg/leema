
use crate::leema::failure::Lresult;
use crate::leema::ast2::{self, Ast, AstNode, AstResult};

use pest::Parser;
use pest::iterators::Pair;
use pest::prec_climber::{Assoc, Operator, PrecClimber};

use lazy_static::lazy_static;

#[derive(Parser)]
#[grammar = "leema/leema.pest"]
pub struct LeemaParser;


pub fn loc(pair: &Pair<Rule>) -> ast2::Loc
{
    let (line, col) = pair.as_span().start_pos().line_col();
    ast2::Loc::new(line as u16, col as u8)
}

pub fn infix(lhsr: AstResult, op: Pair<'static, Rule>, rhsr: AstResult) -> AstResult
{
    let lhs = ltry!(lhsr);
    let rhs = ltry!(rhsr);
    match op.as_rule() {
        Rule::and => {
            let ast = Ast::Op2(op.as_str(), lhs, rhs);
            Ok(AstNode::new(ast, loc(&op)))
        }
        Rule::or => {
            let ast = Ast::Op2(op.as_str(), lhs, rhs);
            Ok(AstNode::new(ast, loc(&op)))
        }
        _ => unreachable!("unknown operator: {:?}", op),
    }
}

pub fn consume(pair: Pair<'static, Rule>, climber: &PrecClimber<Rule>) -> AstResult
{
    let primary = |p| {
        consume(p, climber)
    };

    match pair.as_rule() {
        Rule::expr => {
            let inner = pair.into_inner();
            climber.climb(inner, primary, infix)
        }
        Rule::id => {
            Ok(AstNode::new(
                Ast::Id1(pair.as_str()),
                loc(&pair),
            ))
        }
        Rule::and => Ok(AstNode::new(Ast::Id1("and"), loc(&pair))),
        Rule::or => Ok(AstNode::new(Ast::Id1("or"), loc(&pair))),
        _ => {
            let inner = pair.into_inner();
            if inner.as_str().is_empty() {
                // needs to be a better terminal case than this
                Ok(AstNode::void())
            } else {
                climber.climb(inner, primary, infix)
            }
        }
    }
}

pub fn parse(text: &'static str) -> Lresult<Vec<AstNode>>
{
    lazy_static! {
        static ref CLIMBER: PrecClimber<Rule> = PrecClimber::new(vec![
            Operator::new(Rule::or, Assoc::Left),
            Operator::new(Rule::and, Assoc::Left),
        ]);
    }
    let it = LeemaParser::parse(Rule::infix_expr, text)
        .map_err(|e| {
            println!("parse error: {:?}", e);
            rustfail!(
                "parse failure",
                "{:?}",
                e,
            )
        })?;
    it.map(|p| {
        consume(p, &CLIMBER)
    }).collect()
}

#[cfg(test)]
mod tests
{
    use super::{LeemaParser, parse, Rule};
    use crate::leema::ast2::Ast;

    use pest::{consumes_to, parses_to};

    #[test]
    fn bool_true()
    {
        parses_to!(
            parser: LeemaParser,
            input: "True",
            rule: Rule::expr,
            tokens: [
                id(0, 4)
            ]
        )
    }

    /*
    #[test]
    fn test_id()
    {
        parses_to!(
            parser: LeemaParser,
            input: "foo",
            rule: Rule::typex,
            tokens: [id(0, 3)]
        )
    }

    #[test]
    fn id_generic()
    {
        parses_to!(
            parser: LeemaParser,
            input: "foo'A",
            rule: Rule::expr,
            tokens: [
                id_generic(0, 4, [
                    id(0, 3),
                    id(4, 5),
                ])
            ]
        )
    }
    */

    #[test]
    fn infix_equality()
    {
        let input = "3 == x";
        let actual = parse(input).unwrap();
        println!("{:#?}", actual);
        parses_to!(
            parser: LeemaParser,
            input: input,
            rule: Rule::expr,
            tokens: [
                expr(0, 6, [
                    infix_expr(0, 6, [
                        int(0, 1),
                        infix_op(2, 4),
                        expr(5, 6, [
                            id(5, 6)
                        ]),
                    ])
                ])
            ]
        )
    }

    #[test]
    fn infix_or_only()
    {
        let input = "True or False";
        let actual = parse(input).unwrap();
        println!("{:#?}", actual);
        parses_to!(
            parser: LeemaParser,
            input: input,
            rule: Rule::expr,
            tokens: [
                expr(0, 13, [
                    infix_expr(0, 13, [
                        id(0, 4),
                        infix_op(5, 7),
                        expr(8, 13, [id(8, 13)])
                    ])
                ])
            ]
        )
    }

    #[test]
    fn infix_and_or()
    {
        let input = "a and b or c";
        let actual = parse(input).unwrap();
        println!("{:#?}", actual);

        assert_eq!(1, actual.len());
        let t = &actual[0];
        if let Ast::Op2("or", a_and_b, c) = &*t.node {
            assert_eq!(Ast::Id1("c"), *c.node);
            if let Ast::Op2("and", a, b) = &*a_and_b.node {
                assert_eq!(Ast::Id1("a"), *a.node);
                assert_eq!(Ast::Id1("b"), *b.node);
            } else {
                panic!("expected and operation, found {:?}", a_and_b);
            }
        } else {
            panic!("expected or operation, found {:?}", t);
        }
    }

    #[test]
    fn infix_or_and()
    {
        let input = "a or b and c";
        let actual = parse(input).unwrap();
        println!("{:#?}", actual);
        parses_to!(
            parser: LeemaParser,
            input: input,
            rule: Rule::expr,
            tokens: [
                expr(0, 12, [
                    infix_expr(0, 12, [
                        id(0, 1),
                        infix_op(2, 4),
                        expr(5, 12, [
                            infix_expr(5, 12, [
                                id(5, 6),
                                infix_op(7, 10),
                                expr(11, 12, [id(11, 12)])
                            ])
                        ]),
                    ])
                ])
            ]
        )
    }

    /*
    #[test]
    fn less_than_3()
    {
        let input = "3 < x <= 10";
        let actual = parse(input).unwrap();
        println!("{:?}", actual);
        parses_to!(
            parser: LeemaParser,
            input: input,
            rule: Rule::expr,
            tokens: [
                id(0, 4)
            ]
        )
    }
    */

    #[test]
    fn number_float()
    {
        parses_to!(
            parser: LeemaParser,
            input: "3.14159",
            rule: Rule::expr,
            tokens: [
                float(0, 7)
            ]
        )
    }

    #[test]
    fn number_int()
    {
        parses_to!(
            parser: LeemaParser,
            input: "1234",
            rule: Rule::expr,
            tokens: [int(0, 4)]
        )
    }

    /*
    #[test]
    fn negative_int()
    {
        parses_to!(
            parser: LeemaParser,
            input: "-34",
            rule: Rule::expr,
            tokens: [prefix_expr(0, 3, [dash(0, 1), int(1, 3)])]
        )
    }

    #[test]
    fn negative_int_plus()
    {
        // does the negative 34 get handled correctly?
        parses_to!(
            parser: LeemaParser,
            input: "-3 * 5",
            rule: Rule::expr,
            tokens: [
                infix_expr(0, 6, [
                    dash(0, 1),
                    int(1, 3)
                ])
            ]
        )
    }

    #[test]
    fn int_minus_negative()
    {
        // does the negative int get subtracted from
        parses_to!(
            parser: LeemaParser,
            input: "3 - -56",
            rule: Rule::expr,
            tokens: [
                infix_expr(0, 7, [
                    int(0, 1),
                    dash(2, 3),
                    prefix_expr(4, 7, [
                        dash(4, 5),
                        int(5, 7),
                    ])
                ])
            ]
        )
    }
    */
}
