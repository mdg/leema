use leema::failure::Lresult;
use leema::token::{Token, Tokenz};


struct Stmt
{
    line: u16,
}

struct Parser<'input>
{
    tok: Tokenz<'input>,
}

impl<'input> Parser<'input>
{
    pub fn new(mut tok: Tokenz<'input>) -> Parser<'input>
    {
        tok.set_filter(Parser::token_filter);
        Parser { tok }
    }

    pub fn parse_module(&mut self) -> Lresult<Vec<Stmt>>
    {
        let mut result = vec![];
        while !self.lookahead(Token::EOF) {
            result.push(self.parse_stmt()?);
        }
        Ok(result)
    }

    pub fn parse_stmt(&mut self) -> Lresult<Stmt>
    {
        Ok(Stmt { line: 0 })
    }

    pub fn lookahead(&mut self, _tok: Token) -> bool
    {
        true
    }

    fn token_filter(tok: Token) -> bool
    {
        match tok {
            Token::LineEnd => false,
            Token::Spaces => false,
            _ => true,
        }
    }
}


#[cfg(test)]
mod tests
{
    use super::Parser;
    use leema::token::Tokenz;

    #[test]
    fn test_parser_it()
    {
        let input = "const X = 5";
        let mut p = Parser::new(Tokenz::lex(input));
        let r = p.parse_module();
        assert!(r.is_ok());
    }
}
