use leema::ast2::{Ast, AstNode};
use leema::failure::Lresult;
use leema::token::{Token, TokenSrc};
use leema::val::Val;

use std::collections::HashMap;
use std::fmt::Debug;


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
        self.peek_token()
            .map(|t| t.tok)
            .map_err(|f| f.loc(file!(), line!()))
    }

    pub fn match_next(&mut self, t: Token) -> Lresult<bool>
    {
        self.peek()
            .map(|tok| tok == t)
            .map_err(|f| f.loc(file!(), line!()))
    }

    pub fn next(&mut self) -> Lresult<TokenSrc<'input>>
    {
        self.peek_token().map_err(|f| f.loc(file!(), line!()))?;
        self.peeked
            .take()
            .ok_or_else(|| rustfail!("parse_failure", "failed peek"))
    }

    pub fn next_if(&mut self, t: Token) -> Lresult<Option<TokenSrc<'input>>>
    {
        let tok = self.peek_token().map_err(|f| f.loc(file!(), line!()))?;
        if tok.tok == t {
            self.peeked = None;
            Ok(Some(tok))
        } else {
            Ok(None)
        }
    }

    pub fn expect_next(&mut self, expected: Token)
        -> Lresult<TokenSrc<'input>>
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
            .ok_or_else(|| rustfail!("parse_failure", "token underflow"))
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

trait PrefixParser: Debug
{
    fn parse<'input>(
        &self,
        &mut Parser<'input>,
        TokenSrc<'input>,
    ) -> Lresult<AstNode<'input>>;
}

#[derive(Debug)]
struct ParseBool;

impl PrefixParser for ParseBool
{
    fn parse<'input>(
        &self,
        _p: &mut Parser<'input>,
        left: TokenSrc<'input>,
    ) -> Lresult<AstNode<'input>>
    {
        let b = match left.src {
            "False" => false,
            "True" => true,
            _ => {
                return Err(rustfail!(
                    "parse_failure",
                    "bool token is not True or False: '{}'",
                    left.src,
                ));
            }
        };
        Ok(AstNode::new_constval(Val::Bool(b), Ast::loc(&left)))
    }
}

#[derive(Debug)]
struct DefConstParser;

impl PrefixParser for DefConstParser
{
    fn parse<'input>(
        &self,
        p: &mut Parser<'input>,
        left: TokenSrc<'input>,
    ) -> Lresult<AstNode<'input>>
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
    fn parse<'input>(
        &self,
        p: &mut Parser<'input>,
        left: TokenSrc<'input>,
    ) -> Lresult<AstNode<'input>>
    {
        let lhs = p.parse_expr()?;
        let _assign = p.expect_next(Token::Assignment)?;
        let rhs = p.parse_expr()?;
        Ok(AstNode::new(
            Ast::Let(lhs, AstNode::void(), rhs),
            Ast::loc(&left),
        ))
    }
}

#[derive(Debug)]
struct BlockParser;

impl PrefixParser for BlockParser
{
    fn parse<'input>(
        &self,
        _p: &mut Parser<'input>,
        _left: TokenSrc<'input>,
    ) -> Lresult<AstNode<'input>>
    {
        Ok(AstNode::void())
    }
}

#[derive(Debug)]
struct IdParser;

impl PrefixParser for IdParser
{
    fn parse<'input>(
        &self,
        _p: &mut Parser<'input>,
        left: TokenSrc<'input>,
    ) -> Lresult<AstNode<'input>>
    {
        Ok(AstNode::new(Ast::Id1(left.src), Ast::loc(&left)))
    }
}

#[derive(Debug)]
struct IfParser;

impl PrefixParser for IfParser
{
    fn parse<'input>(
        &self,
        _p: &mut Parser<'input>,
        _left: TokenSrc<'input>,
    ) -> Lresult<AstNode<'input>>
    {
        Ok(AstNode::void())
    }
}

#[derive(Debug)]
struct IntParser;

impl PrefixParser for IntParser
{
    fn parse<'input>(
        &self,
        _p: &mut Parser<'input>,
        left: TokenSrc<'input>,
    ) -> Lresult<AstNode<'input>>
    {
        let i: i64 = left.src.parse().map_err(|parsef| {
            rustfail!(
                "parse_failure",
                "int token is not an integer: {:?}",
                parsef,
            )
        })?;
        Ok(AstNode::new(Ast::ConstVal(Val::Int(i)), Ast::loc(&left)))
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

trait InfixParser: Debug
{
    fn parse<'input>(
        &self,
        &mut Parser<'input>,
        AstNode<'input>,
        TokenSrc<'input>,
    ) -> Lresult<AstNode<'input>>;

    fn precedence(&self) -> Precedence;
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
struct Precedence(u8, i8, Assoc);

const MIN_PRECEDENCE: Precedence = Precedence(0, 0, Assoc::Left);

#[derive(Debug)]
struct BinaryOpParser
{
    op: &'static str,
    pre: Precedence,
}

impl BinaryOpParser
{
    pub fn new(op: &'static str, pre: Precedence) -> BinaryOpParser
    {
        BinaryOpParser { op, pre }
    }
}

impl InfixParser for BinaryOpParser
{
    fn parse<'input>(
        &self,
        p: &mut Parser<'input>,
        left: AstNode<'input>,
        op: TokenSrc<'input>,
    ) -> Lresult<AstNode<'input>>
    {
        let right = p.parse_expr()?;
        let ast = Ast::Op2(op.src, left, right);
        Ok(AstNode::new(ast, Ast::loc(&op)))
    }

    fn precedence(&self) -> Precedence
    {
        self.pre
    }
}

const OP_MULTIPLY: &'static BinaryOpParser = &BinaryOpParser {
    op: "*",
    pre: Precedence(13, 0, Assoc::Left),
};

const OP_DIVIDE: &'static BinaryOpParser = &BinaryOpParser {
    op: "/",
    pre: Precedence(13, 0, Assoc::Left),
};

const OP_ADD: &'static BinaryOpParser = &BinaryOpParser {
    op: "+",
    pre: Precedence(13, 0, Assoc::Left),
};

const OP_SUBTRACT: &'static BinaryOpParser = &BinaryOpParser {
    op: "*",
    pre: Precedence(13, 0, Assoc::Left),
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
        stmts.insert(Token::Const, &DefConstParser);
        stmts.insert(Token::Let, &LetParser);

        let mut prefix: HashMap<Token, &'static PrefixParser> = HashMap::new();
        prefix.insert(Token::DoubleArrow, &BlockParser);
        prefix.insert(Token::Id, &IdParser);
        prefix.insert(Token::If, &IfParser);
        prefix.insert(Token::Int, &IntParser);

        let mut infix: HashMap<Token, &'static InfixParser> = HashMap::new();
        infix.insert(Token::Star, OP_MULTIPLY);
        infix.insert(Token::Slash, OP_DIVIDE);
        infix.insert(Token::Plus, OP_ADD);
        infix.insert(Token::Dash, OP_SUBTRACT);

        Parser {
            tok,
            stmts,
            prefix,
            infix,
        }
    }

    pub fn expect_next(&mut self, expected: Token)
        -> Lresult<TokenSrc<'input>>
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
        let first = self.tok.next()?;

        match self.find_stmtp(first.tok) {
            Some(stmtp) => stmtp.parse(self, first),
            None => self.parse_expr_first(first, MIN_PRECEDENCE),
        }
    }

    pub fn parse_expr(&mut self) -> Lresult<AstNode<'input>>
    {
        let first = self.tok.next()?;
        self.parse_expr_first(first, MIN_PRECEDENCE)
    }

    fn parse_expr_first(
        &mut self,
        first: TokenSrc<'input>,
        min_pre: Precedence,
    ) -> Lresult<AstNode<'input>>
    {
        let prefix = self.find_prefix(first.tok).ok_or_else(|| {
            rustfail!("parse_failure", "cannot find parser for {:?}", first.tok)
        })?;
        let mut left = prefix.parse(self, first)?;

        while let Some((tok, infix)) = self.next_infix(min_pre) {
            left = infix.parse(self, left, tok)?;
        }
        Ok(left)
    }

    fn find_stmtp(&self, tok: Token) -> Option<&'static PrefixParser>
    {
        self.stmts.get(&tok).map(|pp| *pp)
    }

    fn find_prefix(&self, tok: Token) -> Option<&'static PrefixParser>
    {
        self.prefix.get(&tok).map(|pp| *pp)
    }

    fn next_infix(
        &mut self,
        min_pre: Precedence,
    ) -> Option<(TokenSrc<'input>, &'static InfixParser)>
    {
        let tok = self.tok.peek().ok().unwrap_or(Token::EOF);
        if tok == Token::EOF {
            return None;
        }
        match self.infix.get(&tok).map(|ip| *ip) {
            None => None,
            Some(infix) => {
                if infix.precedence() >= min_pre {
                    let toksrc = self.tok.next().unwrap();
                    Some((toksrc, infix))
                } else {
                    None
                }
            }
        }
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
        let toks = Tokenz::lexp(input).unwrap();
        let mut p = Parser::new(toks);
        p.parse_module().unwrap();
    }

    #[test]
    fn test_parse_let()
    {
        let input = "let x := 5 + y";
        let toks = Tokenz::lexp(input).unwrap();
        let mut p = Parser::new(toks);
        p.parse_module().unwrap();
    }
}
