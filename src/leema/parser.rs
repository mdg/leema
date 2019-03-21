use crate::leema::ast2::{Ast, AstNode};
use crate::leema::failure::Lresult;
use crate::leema::struple::StrupleKV;
use crate::leema::token::{Token, TokenSrc};
use crate::leema::val::Val;

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

    fn peek(&mut self) -> Lresult<TokenSrc<'input>>
    {
        if self.peeked.is_none() {
            self.peeked = self.it.next();
        }
        self.peeked
            .ok_or_else(|| rustfail!("parse_failure", "token underflow"))
    }

    pub fn peek_token(&mut self) -> Lresult<Token>
    {
        self.peek()
            .map(|t| t.tok)
            .map_err(|f| f.loc(file!(), line!()))
    }

    pub fn match_next(&mut self, t: Token) -> Lresult<bool>
    {
        self.peek_token()
            .map(|tok| tok == t)
            .map_err(|f| f.loc(file!(), line!()))
    }

    pub fn next(&mut self) -> Lresult<TokenSrc<'input>>
    {
        self.peek().map_err(|f| f.loc(file!(), line!()))?;
        self.peeked
            .take()
            .ok_or_else(|| rustfail!("parse_failure", "failed peek"))
    }

    pub fn next_if(&mut self, t: Token) -> Lresult<Option<TokenSrc<'input>>>
    {
        let tok = self.peek().map_err(|f| f.loc(file!(), line!()))?;
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
        let tok = self.peek()?;
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

enum OpPrecedence
{
    Minimum,
    Equal,
    LessThan,
    Add,
    Multiply,
    Xor,
    Semicolon,
    Dollar,
    Pipe,
    Func,
    Dot,
}

trait PrefixParser: Debug
{
    fn parse<'input>(
        &self,
        p: &mut Parser<'input>,
        tok: TokenSrc<'input>,
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
struct ParseDefConst;

impl PrefixParser for ParseDefConst
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
struct ParseLet;

impl PrefixParser for ParseLet
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
struct ParseId;

impl PrefixParser for ParseId
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
struct ParseInt;

impl PrefixParser for ParseInt
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
        p: &mut Parser<'input>,
        left: AstNode<'input>,
        tok: TokenSrc<'input>,
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
struct ParseCall;

impl InfixParser for ParseCall
{
    fn parse<'input>(
        &self,
        p: &mut Parser<'input>,
        left: AstNode<'input>,
        _tok: TokenSrc<'input>,
    ) -> Lresult<AstNode<'input>>
    {
        let args = p.parse_xlist()?;
        let loc = left.loc;
        Ok(AstNode::new(Ast::Call(left, args), loc))
    }

    fn precedence(&self) -> Precedence
    {
        Precedence(15, 0, Assoc::Left)
    }
}

#[derive(Debug)]
struct LessThanParser;

#[derive(Debug)]
struct TypeParamParser;

type PrefixOption = Option<&'static PrefixParser>;
type InfixOption = Option<&'static InfixParser>;
type ParseRow = (Token, PrefixOption, PrefixOption, InfixOption);
const PARSE_TABLE: [ParseRow; 61] = [
    (Token::Id, None, Some(&ParseId), None),
    (Token::Int, None, Some(&ParseInt), None),
    (Token::Bool, None, Some(&ParseBool), None),
    (Token::Hashtag, None, None, None),
    (Token::StrLit, None, None, None),
    (Token::DollarId, None, None, None),
    // brackets
    (Token::ParenL, None, None, Some(&ParseCall)),
    (Token::ParenR, None, None, None),
    (Token::SquareL, None, None, None),
    (Token::SquareR, None, None, None),
    (Token::CurlyL, None, None, None),
    (Token::CurlyR, None, None, None),
    (Token::AngleL, None, None, None),
    (Token::AngleR, None, None, None),
    (Token::DoubleQuoteL, None, None, None),
    (Token::DoubleQuoteR, None, None, None),
    // keywords
    (Token::Const, Some(&ParseDefConst), None, None),
    (Token::Failed, None, None, None),
    (Token::Fork, None, None, None),
    (Token::Func, None, None, None),
    (Token::If, None, Some(&IfParser), None),
    (Token::Import, None, None, None),
    (Token::Let, Some(&ParseLet), None, None),
    (Token::Macro, None, None, None),
    (Token::Match, None, None, None),
    (Token::Return, None, None, None),
    (Token::Type, None, None, None),
    (Token::Underscore, None, None, None),
    // operators (arithmetic)
    (Token::Plus, None, None, Some(OP_ADD)),
    (Token::Dash, None, None, Some(OP_SUBTRACT)),
    (Token::Star, None, None, Some(OP_MULTIPLY)),
    (Token::Slash, None, None, Some(OP_DIVIDE)),
    (Token::Modulo, None, None, None),
    // operators (boolean)
    (Token::And, None, None, None),
    (Token::Not, None, None, None),
    (Token::Or, None, None, None),
    (Token::Xor, None, None, None),
    // operators (comparison)
    (Token::Equal, None, None, None),
    (Token::EqualNot, None, None, None),
    (Token::GreaterThanEqual, None, None, None),
    (Token::LessThanEqual, None, None, None),
    // separators
    (Token::Assignment, None, None, None),
    (Token::Colon, None, None, None),
    (Token::Comma, None, None, None),
    (Token::ConcatNewline, None, None, None),
    (Token::Dot, None, None, None),
    (Token::DoubleArrow, None, Some(&BlockParser), None),
    (Token::DoubleColon, None, None, None),
    (Token::DoubleDash, None, None, None),
    (Token::Pipe, None, None, None),
    (Token::Semicolon, None, None, None),
    (Token::StatementSep, None, None, None),
    // comments
    (Token::CommentBlockStart, None, None, None),
    (Token::CommentBlockStop, None, None, None),
    (Token::CommentLine, None, None, None),
    // whitespace
    (Token::EmptyLine, None, None, None),
    (Token::LineBegin, None, None, None),
    (Token::LineEnd, None, None, None),
    (Token::Spaces, None, None, None),
    (Token::Tabs, None, None, None),
    // EOF
    (Token::EOF, None, None, None),
];

pub struct Parser<'input>
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

    pub fn next(&mut self) -> Lresult<TokenSrc<'input>>
    {
        self.tok.next()
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

    pub fn parse_xlist(
        &mut self,
    ) -> Lresult<StrupleKV<Option<&'input str>, AstNode<'input>>>
    {
        let mut args = vec![];
        loop {
            match self.tok.peek_token()? {
                Token::ParenR => {
                    self.next()?;
                    break;
                }
                Token::EOF => {
                    return Err(rustfail!(
                        "parse_failure",
                        "expected ')', found EOF",
                    ));
                }
                _ => {
                    args.push(self.parse_expr()?);
                }
            };

            let comma = self.tok.next()?;
            match comma.tok {
                Token::Comma => {
                    // continue
                }
                Token::ParenR => {
                    break;
                }
                Token::EOF => {
                    return Err(rustfail!(
                        "parse_failure",
                        "expected ')', found EOF",
                    ));
                }
                _ => {
                    return Err(rustfail!(
                        "parse_failure",
                        "expected ')' or ',' found {:?}",
                        comma,
                    ));
                }
            }
        }
        Ok(StrupleKV::from(args))
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
        PARSE_TABLE.get(tok as usize).and_then(|row| row.1)
    }

    fn find_prefix(&self, tok: Token) -> Option<&'static PrefixParser>
    {
        PARSE_TABLE.get(tok as usize).and_then(|row| row.2)
    }

    fn next_infix(
        &mut self,
        min_pre: Precedence,
    ) -> Option<(TokenSrc<'input>, &'static InfixParser)>
    {
        let tok = self.tok.peek_token().ok().unwrap_or(Token::EOF);
        if tok == Token::EOF {
            return None;
        }
        match PARSE_TABLE.get(tok as usize).and_then(|row| row.3) {
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
    use crate::leema::token::Tokenz;

    #[test]
    fn test_parse_call_no_params()
    {
        let input = "let x := 5 + f()";
        Parser::new(Tokenz::lexp(input).unwrap())
            .parse_module()
            .unwrap();
    }

    #[test]
    fn test_parse_call_two_params()
    {
        let input = "let x := 5 + f(9, 3)";
        Parser::new(Tokenz::lexp(input).unwrap())
            .parse_module()
            .unwrap();
    }

    #[test]
    fn test_parse_call_two_params_trailing_comma()
    {
        let input = "let x := 5 + f(9, 3,)";
        Parser::new(Tokenz::lexp(input).unwrap())
            .parse_module()
            .unwrap();
    }

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
