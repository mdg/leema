
use crate::leema::failure::Lresult;
use crate::leema::ast2::{self, Ast, AstNode, AstResult};
use crate::leema::val::Val;

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
        Rule::and|Rule::or|Rule::less_than|Rule::equality|Rule::greater_than =>
        {
            let ast = Ast::Op2(op.as_str(), lhs, rhs);
            Ok(AstNode::new(ast, loc(&op)))
        }
        _ => {
            panic!("unknown operator: {:?}", op);
        }
    }
}

pub fn consume(pair: Pair<'static, Rule>, climber: &PrecClimber<Rule>) -> AstResult
{
    let primary = |p| {
        consume(p, climber)
    };

    match pair.as_rule() {
        Rule::x1 => {
            let inner = pair.into_inner();
            climber.climb(inner, primary, infix)
        }
        Rule::expr => {
            let inner = pair.into_inner();
            climber.climb(inner, primary, infix)
        }
        Rule::prefix_expr => {
            let mut inner = pair.into_inner();
            let op = inner.next().unwrap();
            let arg = consume(inner.next().unwrap(), climber)?;
            Ok(AstNode::new(Ast::Op1(op.as_str(), arg), loc(&op)))
        }
        Rule::id => {
            Ok(AstNode::new(
                Ast::Id1(pair.as_str()),
                loc(&pair),
            ))
        }
        Rule::int => {
            let i = pair.as_str().parse().unwrap();
            Ok(AstNode::new_constval(Val::Int(i), loc(&pair)))
        }
        Rule::and|Rule::or|Rule::not
            |Rule::less_than|Rule::equality|Rule::greater_than =>
        {
            Ok(AstNode::new(Ast::Id1(pair.as_str()), loc(&pair)))
        }
        _ => {
            println!("unsupported rule: {:?}", pair);
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
            Operator::new(Rule::less_than, Assoc::Left)
            | Operator::new(Rule::equality, Assoc::Left)
            | Operator::new(Rule::greater_than, Assoc::Left),
        ]);
    }
    let it = LeemaParser::parse(Rule::expr, text)
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
            rule: Rule::x1,
            tokens: [
                id(0, 4)
            ]
        )
    }

    #[test]
    fn test_id()
    {
        parses_to!(
            parser: LeemaParser,
            input: "foo",
            rule: Rule::expr,
            tokens: [expr(0, 3, [id(0, 3)])]
        )
    }

    #[test]
    fn generic_id()
    {
        parses_to!(
            parser: LeemaParser,
            input: "foo'A",
            rule: Rule::expr,
            tokens: [
                expr(0, 5, [
                    typecall_expr(0, 5, [
                        id(0, 3),
                        id(4, 5),
                    ])
                ])
            ]
        )
    }

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
                    int(0, 1),
                    equality(2, 4),
                    id(5, 6)
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

        parses_to!(
            parser: LeemaParser,
            input: input,
            rule: Rule::expr,
            tokens: [
                expr(0, 12, [
                    id(0, 1),
                    and(2, 5),
                    id(6, 7),
                    or(8, 10),
                    id(11, 12)
                ])
            ]
        );

        /*
        // assertion syntax like this would be nice
        ast_match!(actual, Ast::Op2("or", a_and_b, c), [
            ast_match!(a_and_b, Ast::Op2("and", a, b), [
                ast_eq!(b, Ast::Id1("a")),
                ast_eq!(c, Ast::Id1("b")),
            ])
            ast_eq!(c, Ast::Id1("c")),
        ]);
        */

        assert_eq!(1, actual.len());
        let t = &actual[0];
        if let Ast::Op2("or", a_and_b, c) = &*t.node {
            if let Ast::Op2("and", a, b) = &*a_and_b.node {
                assert_eq!(Ast::Id1("a"), *a.node);
                assert_eq!(Ast::Id1("b"), *b.node);
            } else {
                panic!("expected and operation, found {:?}", a_and_b);
            }
            assert_eq!(Ast::Id1("c"), *c.node);
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
                    id(0, 1),
                    or(2, 4),
                    id(5, 6),
                    and(7, 10),
                    id(11, 12)
                ])
            ]
        );

        assert_eq!(1, actual.len());
        let t = &actual[0];
        if let Ast::Op2("or", a, b_and_c) = &*t.node {
            assert_eq!(Ast::Id1("a"), *a.node);
            if let Ast::Op2("and", b, c) = &*b_and_c.node {
                assert_eq!(Ast::Id1("b"), *b.node);
                assert_eq!(Ast::Id1("c"), *c.node);
            } else {
                panic!("expected and operation, found {:?}", b_and_c);
            }
        } else {
            panic!("expected or operation, found {:?}", t);
        }
    }

    #[test]
    fn infix_not_and()
    {
        let input = "not a and b";
        let actual = parse(input).unwrap();
        println!("{:#?}", actual);

        parses_to!(
            parser: LeemaParser,
            input: input,
            rule: Rule::expr,
            tokens: [
                expr(0, 11, [
                    prefix_expr(0, 5, [
                        not(0, 3),
                        id(4, 5),
                    ]),
                    and(6, 9),
                    id(10, 11)
                ])
            ]
        );

        let t = &actual[0];
        if let Ast::Op2("and", not_a, b) = &*t.node {
            if let Ast::Op1("not", a) = &*not_a.node {
                assert_eq!(Ast::Id1("a"), *a.node);
            } else {
                panic!("expected not op1, found {:?}", not_a);
            }
            assert_eq!(Ast::Id1("b"), *b.node);
        } else {
            panic!("expected and op2, found {:?}", t);
        }
        assert_eq!(1, actual.len());
    }

    #[test]
    fn infix_and_not()
    {
        let input = "a and not b";
        let actual = parse(input).unwrap();
        println!("{:#?}", actual);

        parses_to!(
            parser: LeemaParser,
            input: input,
            rule: Rule::expr,
            tokens: [
                expr(0, 11, [
                    id(0, 1),
                    and(2, 5),
                    prefix_expr(6, 11, [
                        not(6, 9),
                        id(10, 11)
                    ]),
                ])
            ]
        );

        let t = &actual[0];
        if let Ast::Op2("and", a, not_b) = &*t.node {
            assert_eq!(Ast::Id1("a"), *a.node);
            if let Ast::Op1("not", b) = &*not_b.node {
                assert_eq!(Ast::Id1("b"), *b.node);
            } else {
                panic!("expected not op1, found {:?}", not_b);
            }
        } else {
            panic!("expected and op2, found {:?}", t);
        }
        assert_eq!(1, actual.len());
    }

    #[test]
    fn infix_less_than()
    {
        let input = "a < b";
        let actual = parse(input).unwrap();
        println!("{:#?}", actual);

        parses_to!(
            parser: LeemaParser,
            input: input,
            rule: Rule::expr,
            tokens: [
                expr(0, 5, [
                    id(0, 1),
                    less_than(2, 3),
                    id(4, 5),
                ])
            ]
        );

        let t = &actual[0];
        if let Ast::Op2("<", a, b) = &*t.node {
            assert_eq!(Ast::Id1("a"), *a.node);
            assert_eq!(Ast::Id1("b"), *b.node);
        } else {
            panic!("expected < op2, found {:?}", t);
        }
        assert_eq!(1, actual.len());
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
            rule: Rule::x1,
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
            tokens: [expr(0, 7, [float(0, 7)])]
        )
    }

    #[test]
    fn number_int()
    {
        parses_to!(
            parser: LeemaParser,
            input: "1234",
            rule: Rule::expr,
            tokens: [expr(0, 4, [int(0, 4)])]
        )
    }

    #[test]
    fn negative_int()
    {
        parses_to!(
            parser: LeemaParser,
            input: "-34",
            rule: Rule::expr,
            tokens: [expr(0, 3, [
                prefix_expr(0, 3, [dash(0, 1), int(1, 3)])
            ])]
        )
    }

    #[test]
    fn negative_int_plus()
    {
        // does the negative 34 get handled correctly?
        parses_to!(
            parser: LeemaParser,
            input: "-3 + 5",
            rule: Rule::expr,
            tokens: [
                expr(0, 6, [
                    prefix_expr(0, 2, [
                        dash(0, 1),
                        int(1, 2)
                    ]),
                    plus(3, 4),
                    int(5, 6)
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
                expr(0, 7, [
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
}
