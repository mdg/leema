use crate::leema::failure::Lresult;
use crate::leema::token::{Token, TokenResult, TokenSrc};

use std::fmt;


#[macro_export]
macro_rules! expect_next {
    ($p:expr, $expected:expr) => {{
        let tok = $p.next()?;
        if tok.tok == $expected {
            Ok(tok)
        } else {
            Err(rustfail!(
                "parse_failure",
                "expected {:?}, found {}",
                $expected,
                tok,
            ))
        }
    }};
}

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

    /// Peek at the next token without consuming it
    fn peek(&mut self) -> TokenResult<'input>
    {
        if self.peeked.is_none() {
            self.peeked = self.it.next();
        }
        self.peeked
            .ok_or_else(|| rustfail!("parse_failure", "token underflow"))
    }

    /// Peek at the type of the next token without consuming it
    pub fn peek_token(&mut self) -> Lresult<Token>
    {
        self.peek()
            .map(|t| t.tok)
            .map_err(|f| f.loc(file!(), line!()))
    }

    /// Consume the next token
    pub fn next(&mut self) -> TokenResult<'input>
    {
        self.peek().map_err(|f| f.loc(file!(), line!()))?;
        self.peeked
            .take()
            .ok_or_else(|| rustfail!("parse_failure", "failed peek"))
    }

    /// Consume the next token if it is of the expected type
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

/*
pub enum ModeOp<'i>
{
    Push(&'static ParslMode<'i>),
    Replace(&'static ParslMode<'i>),
    Pop,
}

pub struct ParseOutput<'input>
{
    node: Option<AstNode<'input>>,
    consume: bool,
    /*
    Open(AstNode<'input>, bool, ModeOp),
    Close(AstNode<'input>, bool, ModeOp),
    Skip(bool, ModeOp),
    Stop,
    */
}

impl<'input> ParseOutput<'input>
{
    fn ok(node: AstNode<'input>, consume: bool) -> ParseResult<'input>
    {
        ParseOutput {
            node,
            consume,
        }
    }
}

type ParseResult<'i> = Lresult<ParseOutput<'i>>;
type ParseNResult<'i> = Lresult<Vec<AstNode<'i>>>;
*/

pub trait PrefixParser<'i>: fmt::Debug
{
    type Item;

    fn parse(
        &self,
        p: &mut Parsl<'i>,
        tok: TokenSrc<'i>,
    ) -> Lresult<Self::Item>;
}

pub trait InfixParser: fmt::Debug
{
    type Item;

    fn parse<'i>(
        &self,
        p: &mut Parsl<'i>,
        left: Self::Item,
        tok: TokenSrc<'i>,
    ) -> Lresult<Self::Item>;

    fn precedence(&self) -> Precedence;
}

pub trait ParslMode<'i>: fmt::Debug
{
    type Item;

    /// Some modes start with an implicit prefix and are infix only
    /// like a list of parameters
    fn implicit_prefix(&self) -> Option<Self::Item>
    {
        None
    }

    /// Get a prefix parser for this token
    fn prefix(&self, _tok: Token) -> Option<&PrefixParser<'i, Item=Self::Item>>
    {
        None
    }

    fn infix(&self, _tok: Token) -> Option<&InfixParser<Item=Self::Item>>
    {
        None
    }
}

pub struct Parsl<'i>
{
    src: TokenStream<'i>,
}

impl<'i> Parsl<'i>
{
    pub fn new(src: Vec<TokenSrc<'i>>) -> Parsl<'i>
    {
        let strm = TokenStream::new(src);
        Parsl { src: strm }
    }

    pub fn peek_token(&mut self) -> Lresult<Token>
    {
        self.src.peek_token()
    }

    pub fn peek(&mut self) -> TokenResult<'i>
    {
        self.src.peek()
    }

    pub fn next(&mut self) -> TokenResult<'i>
    {
        self.src.next()
    }

    pub fn next_if(&mut self, tok: Token) -> Lresult<Option<TokenSrc<'i>>>
    {
        self.src.next_if(tok)
    }

    pub fn skip_if(&mut self, tok: Token) -> Lresult<()>
    {
        while self.next_if(tok)?.is_some() {
            // keep going
        }
        Ok(())
    }

    pub fn parse_new<P>(&mut self, mode: &'static P) -> Lresult<P::Item>
        where P: ParslMode<'i>
    {
        self.parse(mode, MIN_PRECEDENCE)
    }

    pub fn parse<P>(&mut self, mode: &'static P, prec: Precedence) -> Lresult<P::Item>
        where P: ParslMode<'i>
    {
        let tok0 = self.next()?;
        let mut left = match mode.prefix(tok0.tok) {
            Some(parser) => parser.parse(self, tok0)?,
            None => {
                return Err(rustfail!(
                    "parse_failure",
                    "cannot parse token in mode {} {:?}",
                    tok0,
                    mode,
                ));
            }
        };

        loop {
            let infix = mode.infix(self.peek_token()?);
            if infix.is_none() || infix.unwrap().precedence() < prec {
                break;
            }
            let itok = self.next()?;
            left = infix.unwrap().parse(self, left, itok)?;
        }
        Ok(left)
    }

    /*
    pub fn parse_n<T>(&mut self, pi: &'static ParslMode<'i, T>) -> Lresult<T>
    {
        let mut result = vec![];
        loop {
            let tok = self.peek_token()?;
            if tok.tok == Token::EOF {
                break;
            }
            if !pi.more(self, tok.tok) {
                break;
            }
            let node = pi.parse(self)?;
            result.push_back(node);
        }
        Ok(result)
    }

    fn parse_0(&mut self, mode: &'static ParslMode<'i>) -> AstResult<'i>
    {
        let tok = self.src.next()?;
        let output = mode.prefix(self, tok)?;
        output.1.ok_or_else(|| {
            rustfail!(
                "parse_failure",
                "cannot parse token {:?} with parser {:?}",
                tok,
                mode,
            )
        })
    }

    pub fn parse_1(&mut self, mode: &'static ParslMode<'i>, left: AstNode<'i>) -> AstResult<'i>
    {
        let tok = self.src.peek()?;
        let output = mode.parse_0(self, left, tok)?;
        if output.2 {
            self.src.next()?;
        }
        output.1.ok_or_else(|| {
            rustfail!(
                "parse_failure",
                "cannot parse token {:?} with parser {:?}",
                tok,
                mode,
            )
        })
    }
    */
}
