
use pest::Parser;

#[derive(Parser)]
#[grammar = "leema/leema.pest"]
pub struct LeemaParser;

fn parse(text: &'static str)
{
    let result = LeemaParser::parse(Rule::def_func, text);
    println!("{:?}", result);
}
