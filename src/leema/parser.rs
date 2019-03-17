use leema::ast2::{Ast, AstNode};
use leema::failure::Lresult;
use leema::token::{Token, TokenSrc};

use std::collections::HashMap;


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

    pub fn match_next(&mut self, t: Token) -> Lresult<bool>
    {
        self.peek().map(|tok| tok == t)
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

    pub fn expect_next(&mut self, expected: Token) -> Lresult<TokenSrc<'input>>
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

trait PrefixParser
{
    fn parse<'input>(&self, &mut Parser<'input>, TokenSrc<'input>) -> Lresult<AstNode<'input>>;
}

struct DefConstParser;
struct DefFuncParser;
struct DefTypeParser;
struct LetParser;

impl PrefixParser for LetParser
{
    fn parse<'input>(&self, _p: &mut Parser<'input>, _left: TokenSrc<'input>) -> Lresult<AstNode<'input>>
    {
        Ok(AstNode::void())
    }
}

struct BlockParser;

impl PrefixParser for BlockParser
{
    fn parse<'input>(&self, _p: &mut Parser<'input>, _left: TokenSrc<'input>) -> Lresult<AstNode<'input>>
    {
        Ok(AstNode::void())
    }
}

struct IdParser;

struct IfParser;

impl PrefixParser for IfParser
{
    fn parse<'input>(&self, _p: &mut Parser<'input>, _left: TokenSrc<'input>) -> Lresult<AstNode<'input>>
    {
        Ok(AstNode::void())
    }
}

struct ListParser;
struct MatchParser;
struct PrefixOpParser
{
    op: &'static str,
    precedence: u8,
}
struct TupleParser;

trait InfixParser
{
    fn parse<'input>(&self, &mut Parser<'input>, AstNode<'input>, TokenSrc<'input>) -> Lresult<AstNode<'input>>;
}

enum Assoc
{
    Left,
    Right,
}

enum PrecedenceMod
{
    UserHigher,
    AssocLower,
    UserLower,
}
struct Precedence(u8, Option<PrecedenceMod>);

struct BinaryOpParser
{
    op: &'static str,
    pre: Precedence,
    assoc: Assoc,
}

impl BinaryOpParser
{
    pub fn new(op: &'static str, pre: Precedence, assoc: Assoc) -> BinaryOpParser
    {
        BinaryOpParser
        {
            op,
            pre,
            assoc,
        }
    }
}

impl InfixParser for BinaryOpParser
{
    fn parse<'input>(&self, p: &mut Parser<'input>, left: AstNode<'input>, op: TokenSrc<'input>) -> Lresult<AstNode<'input>>
    {
        let right = p.parse_expr()?;
        let ast = Ast::Op2(op.src, left, right);
        Ok(AstNode::new(ast, Ast::loc(&op)))
    }
}

const OP_MULTIPLY: &'static BinaryOpParser = &BinaryOpParser {
    op: "*",
    pre: Precedence(13, None),
    assoc: Assoc::Left,
};

const OP_DIVIDE: &'static BinaryOpParser = &BinaryOpParser {
    op: "/",
    pre: Precedence(13, None),
    assoc: Assoc::Left,
};

const OP_ADD: &'static BinaryOpParser = &BinaryOpParser {
    op: "+",
    pre: Precedence(10, None),
    assoc: Assoc::Left,
};

const OP_SUBTRACT: &'static BinaryOpParser = &BinaryOpParser {
    op: "*",
    pre: Precedence(10, None),
    assoc: Assoc::Left,
};

// struct ConsParser;
// struct DollarParser;
// struct DotParser;
// struct PipeParser;
struct CallParser;
struct LessThanParser;
struct TypeParamParser;

struct Parser<'input>
{
    tok: TokenStream<'input>,
    stmts: HashMap<Token, &'static PrefixParser>,
    prefix: HashMap<Token, &'static PrefixParser>,
    infix: HashMap<Token, &'static InfixParser>,
}

impl<'input> Parser<'input>
{
    pub fn new(items: Vec<TokenSrc<'input>>) -> Parser<'input>
    {
        let tok = TokenStream::new(items);

        let mut stmts: HashMap<Token, &'static PrefixParser> = HashMap::new();
        stmts.insert(Token::Let, &LetParser);
        // stmt.insert(Token::Const, &DefConstParser);

        let mut prefix: HashMap<Token, &'static PrefixParser> = HashMap::new();
        prefix.insert(Token::DoubleArrow, &BlockParser);
        prefix.insert(Token::If, &IfParser);

        let mut infix: HashMap<Token, &'static InfixParser> = HashMap::new();
        infix.insert(Token::Star, OP_MULTIPLY);
        infix.insert(Token::Slash, OP_DIVIDE);
        infix.insert(Token::Plus, OP_ADD);
        infix.insert(Token::Dash, OP_SUBTRACT);

        Parser { tok, stmts, prefix, infix }
    }

    pub fn parse_module(&mut self) -> Lresult<Vec<AstNode<'input>>>
    {
        let mut result = vec![];
        while !self.tok.match_next(Token::EOF)? {
            result.push(self.parse_stmt()?);
        }
        Ok(result)
    }

    pub fn parse_stmt(&mut self) -> Lresult<AstNode<'input>>
    {
        self.tok.expect_next(Token::LineBegin)?;
        let toksrc = self.tok.next()?;
        let ast = match toksrc.tok {
            Token::Let => {
                let lhs = self.parse_pattern()?;
                self.tok.expect_next(Token::Assignment)?;
                let rhs = self.parse_expr()?;
                Ast::Let(lhs, AstNode::void(), rhs)
            }
            _ => {
                return self.parse_expr();
            }
        };
        Ok(AstNode::new(ast, Ast::loc(&toksrc)))
    }

    pub fn parse_expr(&mut self) -> Lresult<AstNode<'input>>
    {
        let first = self.tok.next()?;
        let _prefix = self.find_prefix(first.tok).ok_or_else(|| {
            rustfail!("parse_failure", "cannot find parser for {:?}", first.tok)
        })?;
        // let left = prefix.parse(first);

        /*
        let expr = match first.tok {
            Token::Id => {
                Ast::Id(first.src, Ast::loc(&tok))
            }
            Token::Bool(b) => {
                Ast::Bool(b, Ast::loc(&tok))
            }
            _ => {
                return Err(rustfail!(
                    "parse_failure",
                    "expected expression, found {:?}",
                    tok,
                ))
            }
        };
        */
        // Ok(expr)
        Err(rustfail!(
            "parse_failure",
            "parse not implemented",
        ))
    }

    pub fn parse_pattern(&mut self) -> Lresult<AstNode<'input>>
    {
        let tok = self.tok.next()?;
        let patt = match tok.tok {
            Token::Id => {
                Ast::Id1(tok.src)
            }
            _ => {
                return Err(rustfail!(
                    "parse_failure",
                    "expected pattern token, found {:?}",
                    tok,
                ))
            }
        };
        Ok(AstNode::new(patt, Ast::loc(&tok)))
    }

    fn find_prefix(&self, _tok: Token) -> Option<&PrefixParser>
    {
        None
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
