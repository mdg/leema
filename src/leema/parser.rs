use crate::leema::ast2::{Ast, AstNode, AstResult};
use crate::leema::failure::Lresult;
use crate::leema::token::{Token, TokenResult, TokenSrc};

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

    fn peek(&mut self) -> TokenResult<'input>
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

    pub fn next(&mut self) -> TokenResult<'input>
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

#[derive(Copy)]
#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
pub enum Assoc
{
    Left,
    Right,
}

#[derive(Copy)]
#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
pub struct Precedence(pub u8, pub i8, pub Assoc);

pub const MIN_PRECEDENCE: Precedence = Precedence(0, 0, Assoc::Left);

pub trait PrefixParser: Debug
{
    fn parse<'input>(
        &self,
        p: &mut Parser<'input>,
        tok: TokenSrc<'input>,
    ) -> AstResult<'input>;
}

#[derive(Debug)]
pub struct PrefixOpParser
{
    pub op: &'static str,
    pub pre: Precedence,
}

impl PrefixParser for PrefixOpParser
{
    fn parse<'input>(
        &self,
        p: &mut Parser<'input>,
        op: TokenSrc<'input>,
    ) -> AstResult<'input>
    {
        let right = p.parse_expr(self.pre)?;
        let ast = Ast::Op1(op.src, right);
        Ok(AstNode::new(ast, Ast::loc(&op)))
    }
}

pub trait InfixParser: Debug
{
    fn parse<'input>(
        &self,
        p: &mut Parser<'input>,
        left: AstNode<'input>,
        tok: TokenSrc<'input>,
    ) -> AstResult<'input>;

    fn precedence(&self) -> Precedence;
}

#[derive(Debug)]
pub struct BinaryOpParser
{
    pub op: &'static str,
    pub pre: Precedence,
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
    ) -> AstResult<'input>
    {
        let right = p.parse_expr(self.pre)?;
        let ast = Ast::Op2(op.src, left, right);
        Ok(AstNode::new(ast, Ast::loc(&op)))
    }

    fn precedence(&self) -> Precedence
    {
        self.pre
    }
}

#[derive(Copy)]
#[derive(Clone)]
#[derive(Debug)]
pub enum TokenParser
{
    Stmt(&'static PrefixParser),
    Prefix(&'static PrefixParser),
    Infix(&'static InfixParser),
    Bothfix(&'static PrefixParser, &'static InfixParser),
    EndBlock,
    ExprBreak,
    Unimplemented,
}
type ParseRow = (Token, TokenParser);
pub type ParseTable = [ParseRow; Token::NumTokens as usize];

pub struct Parser<'input>
{
    tbl: &'static ParseTable,
    tok: TokenStream<'input>,
}

impl<'input> Parser<'input>
{
    pub fn new(
        tbl: &'static ParseTable,
        items: Vec<TokenSrc<'input>>,
    ) -> Parser<'input>
    {
        let tok = TokenStream::new(items);
        Parser { tbl, tok }
    }

    pub fn peek(&mut self) -> TokenResult<'input>
    {
        self.tok.peek()
    }

    pub fn peek_token(&mut self) -> Lresult<Token>
    {
        self.tok.peek_token()
    }

    pub fn next(&mut self) -> Lresult<TokenSrc<'input>>
    {
        self.tok.next()
    }

    pub fn next_if(&mut self, t: Token) -> Lresult<Option<TokenSrc<'input>>>
    {
        self.tok.next_if(t)
    }

    pub fn skip_if(&mut self, t: Token) -> Lresult<()>
    {
        while self.tok.next_if(t)?.is_some() {
            // nothing to do, just skipping
        }
        Ok(())
    }

    pub fn parse_stmts(&mut self) -> Lresult<Vec<AstNode<'input>>>
    {
        let mut stmts = vec![];
        loop {
            self.skip_if(Token::LineBegin)?;
            let tok = self.tok.peek()?;
            match tok.tok {
                Token::CasePipe => {
                    break;
                }
                Token::DoubleDash => {
                    break;
                }
                Token::EOF => {
                    break;
                }
                _ => {
                    // reset indentation outside of the match
                }
            }

            if let Some(stmtp) = self.find_stmtp(&tok)? {
                let first = self.tok.next()?;
                let stmt = stmtp.parse(self, first)?;
                stmts.push(stmt);
                continue;
            }
            if self.find_prefix(&tok)?.is_some() {
                let expr = self.parse_new_expr()?;
                stmts.push(expr);
                continue;
            }
            // else there are no parsers for this token
            // hopefully that's ok later
            break;
        }
        Ok(stmts)
    }

    pub fn parse_new_expr(&mut self) -> AstResult<'input>
    {
        self.parse_expr(MIN_PRECEDENCE)
    }

    pub fn parse_expr(&mut self, min_pre: Precedence) -> AstResult<'input>
    {
        let first = self.tok.next()?;
        let prefix = self.find_prefix(&first)?.ok_or_else(|| {
            rustfail!("parse_failure", "cannot find parser for {:?}", first.tok)
        })?;
        let mut left = prefix.parse(self, first)?;

        while let Some((tok, infix)) = self.next_infix(min_pre)? {
            left = infix.parse(self, left, tok)?;
        }
        Ok(left)
    }

    fn find_stmtp(
        &self,
        tok: &TokenSrc<'input>,
    ) -> Lresult<Option<&'static PrefixParser>>
    {
        match ltry!(self.token_parser(tok)) {
            TokenParser::Stmt(p) => Ok(Some(p)),
            _ => Ok(None),
        }
    }

    fn find_prefix(
        &self,
        tok: &TokenSrc<'input>,
    ) -> Lresult<Option<&'static PrefixParser>>
    {
        match ltry!(self.token_parser(tok)) {
            TokenParser::Prefix(p) => Ok(Some(p)),
            TokenParser::Bothfix(p, _) => Ok(Some(p)),
            _ => Ok(None),
        }
    }

    fn next_infix(
        &mut self,
        min_pre: Precedence,
    ) -> Lresult<Option<(TokenSrc<'input>, &'static InfixParser)>>
    {
        let tok = self.tok.peek()?;
        let infix = match ltry!(self.token_parser(&tok)) {
            TokenParser::Infix(p) => p,
            TokenParser::Bothfix(_, p) => p,
            _ => {
                return Ok(None);
            }
        };

        if infix.precedence() >= min_pre {
            let toksrc = self.tok.next().unwrap();
            Ok(Some((toksrc, infix)))
        } else {
            Ok(None)
        }
    }

    fn token_parser(&self, tok: &TokenSrc<'input>) -> Lresult<TokenParser>
    {
        let tprow = self.tbl.get(tok.tok as usize).ok_or_else(|| {
            rustfail!("parse_failure", "no parser for token: {:?}", tok)
        })?;
        match tprow.1 {
            TokenParser::Unimplemented => {
                Err(rustfail!(
                    "parse_failure",
                    "unimplemented parser for {:?}",
                    tok,
                ))
            }
            tp => Ok(tp),
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
