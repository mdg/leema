
use crate::leema::failure::Lresult;

use pest::Parser;
use pest::iterators::Pair;

#[derive(Parser)]
#[grammar = "leema/leema.pest"]
pub struct LeemaParser;

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
    fn test_parse_func_def_notype()
    {
        parses_to!(
            parser: LeemaParser,
            input: "1234",
            rule: Rule::int,
            tokens: [
                int(0, 4) 
            ]
        )
    }
}
