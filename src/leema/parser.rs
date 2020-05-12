
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
    let it = LeemaParser::parse(Rule::stmt_list, text)
        .map_err(|e| {
            println!("parse error: {:?}", e);
            rustfail!(
                "parse failure",
                "{:?}",
                e,
            )
        })?;
    Ok(it.collect())
}

#[cfg(test)]
mod tests
{
    use super::{LeemaParser, Rule};

    use pest::{consumes_to, parses_to};

    #[test]
    fn test_parse_bool()
    {
        parses_to!(
            parser: LeemaParser,
            input: "True",
            rule: Rule::id,
            tokens: [
                id(0, 4)
            ]
        )
    }

    #[test]
    fn test_parse_number_float()
    {
        parses_to!(
            parser: LeemaParser,
            input: "3.14159",
            rule: Rule::number,
            tokens: [
                float(0, 7)
            ]
        )
    }

    #[test]
    fn test_parse_number_int()
    {
        parses_to!(
            parser: LeemaParser,
            input: "1234",
            rule: Rule::number,
            tokens: [int(0, 4)]
        )
    }

    #[test]
    fn test_parse_number_negative_int()
    {
        parses_to!(
            parser: LeemaParser,
            input: "-34",
            rule: Rule::expr,
            tokens: [prefix_expr(0, 3, [dash(0, 1), int(1, 3)])]
        )
    }
}
