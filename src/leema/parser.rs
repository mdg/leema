use leema::failure::Lresult;
use leema::token::{Token, Tokenz};


struct Ast
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

    pub fn parse_module(&mut self) -> Lresult<Vec<Ast>>
    {
        let mut result = vec![];
        while !self.lookahead(Token::EOF) {
            result.push(self.parse_stmt()?);
        }
        Ok(result)
    }

    pub fn parse_stmt(&mut self) -> Lresult<Ast>
    {
        self.tok.expect(Token::LineBegin)?;
        let toksrc = self.tok.next().unwrap()?;
        match toksrc.tok {
            Token::Let => {
                // let patt = self.parse_pattern()?;
                self.tok.expect(Token::Assignment)?;
                let _expr = self.parse_expr()?;
            }
            _ => {
                self.parse_expr()?;
            }
        }
        Ok(Ast { line: 0 })
    }

    pub fn parse_expr(&mut self) -> Lresult<Ast>
    {
        let tok = self.tok.next().unwrap()?;
        match tok.tok {
            Token::Id => {}
            Token::Int => {}
            _ => {}
        }
        Ok(Ast { line: 0 })
    }

    pub fn lookahead(&mut self, _tok: Token) -> bool
    {
        true
    }

    fn token_filter(tok: Token) -> bool
    {
        match tok {
            Token::EmptyLine => false,
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
