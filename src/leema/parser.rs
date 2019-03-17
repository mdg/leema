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

#[derive(Debug)]
struct DefConstParser;

impl PrefixParser for DefConstParser
{
    fn parse<'input>(&self, p: &mut Parser<'input>, left: TokenSrc<'input>) -> Lresult<AstNode<'input>>
    {
        let id = p.expect_next(Token::Id)?;
        let _assign = p.expect_next(Token::Assignment)?;
        let rhs = p.parse_expr()?;
        Ok(AstNode::new(Ast::DefConst(id.src, rhs), Ast::loc(&left)))
    }
}

#[derive(Debug)]
struct DefFuncParser;

#[derive(Debug)]
struct DefTypeParser;

#[derive(Debug)]
struct LetParser;

impl PrefixParser for LetParser
{
    fn parse<'input>(&self, _p: &mut Parser<'input>, _left: TokenSrc<'input>) -> Lresult<AstNode<'input>>
    {
        Ok(AstNode::void())
    }
}

#[derive(Debug)]
struct BlockParser;

impl PrefixParser for BlockParser
{
    fn parse<'input>(&self, _p: &mut Parser<'input>, _left: TokenSrc<'input>) -> Lresult<AstNode<'input>>
    {
        Ok(AstNode::void())
    }
}

#[derive(Debug)]
struct IdParser;

impl PrefixParser for IdParser
{
    fn parse<'input>(&self, _p: &mut Parser<'input>, left: TokenSrc<'input>) -> Lresult<AstNode<'input>>
    {
        Ok(AstNode::new(Ast::Id1(left.src), Ast::loc(&left)))
    }
}

#[derive(Debug)]
struct IfParser;

impl PrefixParser for IfParser
{
    fn parse<'input>(&self, _p: &mut Parser<'input>, _left: TokenSrc<'input>) -> Lresult<AstNode<'input>>
    {
        Ok(AstNode::void())
    }
}

#[derive(Debug)]
struct ListParser;

#[derive(Debug)]
struct MatchParser;

#[derive(Debug)]
struct PrefixOpParser
{
    op: &'static str,
    precedence: u8,
}

#[derive(Debug)]
struct TupleParser;

trait InfixParser
{
    fn parse<'input>(&self, &mut Parser<'input>, AstNode<'input>, TokenSrc<'input>) -> Lresult<AstNode<'input>>;
}

#[derive(Copy)]
#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
enum Assoc
{
    Left,
    Right,
}

#[derive(Copy)]
#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
enum PrecedenceMod
{
    UserHigher,
    AssocLower,
    UserLower,
}

#[derive(Copy)]
#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
struct Precedence(u8, Option<PrecedenceMod>);

#[derive(Debug)]
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

#[derive(Debug)]
struct CallParser;

#[derive(Debug)]
struct LessThanParser;

#[derive(Debug)]
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
        stmts.insert(Token::Const, &DefConstParser);

        let mut prefix: HashMap<Token, &'static PrefixParser> = HashMap::new();
        prefix.insert(Token::DoubleArrow, &BlockParser);
        prefix.insert(Token::Id, &IdParser);
        prefix.insert(Token::If, &IfParser);

        let mut infix: HashMap<Token, &'static InfixParser> = HashMap::new();
        infix.insert(Token::Star, OP_MULTIPLY);
        infix.insert(Token::Slash, OP_DIVIDE);
        infix.insert(Token::Plus, OP_ADD);
        infix.insert(Token::Dash, OP_SUBTRACT);

        Parser { tok, stmts, prefix, infix }
    }

    pub fn expect_next(&mut self, expected: Token) -> Lresult<TokenSrc<'input>>
    {
        self.tok.expect_next(expected)
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
        let prefix = self.find_prefix(first.tok).ok_or_else(|| {
            rustfail!("parse_failure", "cannot find parser for {:?}", first.tok)
        })?;
        let left = prefix.parse(self, first)?;

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
        Ok(left)
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

    fn find_prefix(&self, tok: Token) -> Option<&PrefixParser>
    {
        self.prefix.get(&tok).map(|pp| *pp)
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
    fn test_parse_const()
    {
        let input = "const X := 5";
        let mut p = Parser::new(Tokenz::lexp(input).unwrap());
        p.parse_module().unwrap();
    }
}
