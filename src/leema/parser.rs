
use crate::leema::failure::Lresult;

use pest::Parser;
use pest::iterators::Pair;
// use pest::prec_climber::{Assoc, PrecClimber};
// use pest::prec_climber;

#[derive(Parser)]
#[grammar = "leema/leema.pest"]
pub struct LeemaParser;

/*
static CLIMBER: PrecClimber<Rule> = prec_climber![
    L plus | dash,
    L slash | star,
];
*/

pub fn parse(text: &'static str) -> Lresult<Vec<Pair<Rule>>>
{
    let it = LeemaParser::parse(Rule::expr, text)
        .map_err(|e| {
            println!("parse error: {:?}", e);
            rustfail!(
                "parse failure",
                "{:?}",
                e,
            )
        })?;
    println!("tokens: {:?}", it.tokens());
    /*
    for p in it.into_inner() {
        println!("pair: {:?}", p);
    }
    */
    // Ok(it.collect())
    Ok(vec![])
}

#[cfg(test)]
mod tests
{
    use super::{LeemaParser, parse, Rule};

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
        println!("{:?}", actual);
        assert_eq!("whatever", format!("{:?}", actual));
        /*
        parses_to!(
            parser: LeemaParser,
            input: input,
            rule: Rule::expr,
            tokens: [
                id(0, 4)
            ]
        )
        */
    }

    #[test]
    fn infix_or()
    {
        let input = "True or False";
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
