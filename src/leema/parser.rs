use leema::failure::Lresult;
use leema::token::{Token, TokenSrc};


struct TokenStream<'input>
{
    it: ::std::vec::IntoIter<TokenSrc<'input>>,
    peeked: Option<TokenSrc<'input>>,
}

impl<'input> TokenStream<'input>
{
    pub fn new(items: Vec<TokenSrc<'input>>) -> TokenStream<'input>
    {
        TokenStream {
            it: items.into_iter(),
            peeked: None,
        }
    }

    pub fn peek(&mut self) -> Lresult<Token>
    {
        self.peek_token().map(|t| t.tok)
    }

    pub fn next(&mut self) -> Lresult<TokenSrc<'input>>
    {
        self.peek_token()?;
        self.peeked.take().ok_or_else(|| {
            rustfail!("parse_failure", "failed peek")
        })
    }

    pub fn next_if(&mut self, t: Token) -> Lresult<Option<TokenSrc<'input>>>
    {
        let tok = self.peek_token()?;
        if tok.tok == t {
            self.peeked = None;
            Ok(Some(tok))
        } else {
            Ok(None)
        }
    }

    pub fn expect(&mut self, expected: Token) -> Lresult<TokenSrc<'input>>
    {
        let tok = self.peek_token()?;
        if tok.tok == expected {
            self.peeked = None;
            Ok(tok)
        } else {
            Err(rustfail!(
                "parse_failure",
                "expected {:?}, found {:?}",
                expected,
                tok,
            ))
        }
    }

    fn peek_token(&mut self) -> Lresult<TokenSrc<'input>>
    {
        if self.peeked.is_none() {
            self.peeked = self.it.next();
        }
        self.peeked
            .ok_or_else(|| {
                rustfail!("parse_failure", "token underflow")
            })
    }

    fn token_filter(t: &TokenSrc) -> bool
    {
        match t.tok {
            Token::EmptyLine => false,
            Token::LineEnd => false,
            Token::Spaces => false,
            _ => true,
        }
    }
}

struct Ast
{
    line: u16,
}

struct Parser<'input>
{
    tok: TokenStream<'input>,
}

impl<'input> Parser<'input>
{
    pub fn new(items: Vec<TokenSrc<'input>>) -> Parser<'input>
    {
        let tok = TokenStream::new(items);
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
        let toksrc = self.tok.next()?;
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
        let tok = self.tok.next()?;
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
    use super::{Parser, TokenStream};
    use leema::token::Tokenz;

    #[test]
    fn test_parser_it()
    {
        let input = "const X = 5";
        let mut p = Parser::new(Tokenz::lex(input));
        let r = p.parse_module();
        assert!(r.is_ok());
    }

    #[test]
    fn test_token_stream()
    {
        let input = "const X = 5";
        let items = Tokenz::lex(input).filter(|t| TokenStream::token_filter(t)).collect();
        let it = items.iter().peekable();
        let toks = TokenStream::new(it);
    }
}
