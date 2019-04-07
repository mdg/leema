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

pub trait PrefixParser<'i>: fmt::Debug
{
    type Item;

    fn parse(
        &self,
        p: &mut Parsl<'i>,
        tok: TokenSrc<'i>,
    ) -> Lresult<Self::Item>;
}

pub trait InfixParser<'i>: fmt::Debug
{
    type Item;

    fn parse(
        &self,
        p: &mut Parsl<'i>,
        left: Self::Item,
        tok: TokenSrc<'i>,
    ) -> Lresult<Self::Item>;

    fn precedence(&self) -> Precedence;
}

#[derive(Debug)]
pub struct ParseFirst<P: 'static>(pub &'static P);

impl<'i, P> PrefixParser<'i> for ParseFirst<P>
    where P: PrefixParser<'i>
        , P::Item: fmt::Debug
{
    type Item = Vec<P::Item>;

    fn parse(&self, p: &mut Parsl<'i>, tok: TokenSrc<'i>) -> Lresult<Vec<P::Item>>
    {
        let first = self.0.parse(p, tok)?;
        Ok(vec![first])
    }
}

#[derive(Debug)]
pub struct ParseMore<P: 'static>(pub &'static P, pub Precedence);

impl<'i, P> InfixParser<'i> for ParseMore<P>
    where P: PrefixParser<'i>
        , P::Item: fmt::Debug
{
    type Item = Vec<P::Item>;

    fn parse(&self, p: &mut Parsl<'i>, mut left: Vec<P::Item>, tok: TokenSrc<'i>) -> Lresult<Vec<P::Item>>
    {
        let next = self.0.parse(p, tok)?;
        left.push(next);
        Ok(left)
    }

    fn precedence(&self) -> Precedence
    {
        self.1
    }
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

    fn infix(&self, _tok: Token) -> Option<&InfixParser<'i, Item=Self::Item>>
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
            , P::Item: fmt::Debug
    {
        let tok = self.next()?;
        self.parse(mode, MIN_PRECEDENCE, tok)
    }

    pub fn parse_more<P>(&mut self, mode: &'static P, prec: Precedence) -> Lresult<P::Item>
        where P: ParslMode<'i>
            , P::Item: fmt::Debug
    {
        let tok = self.next()?;
        self.parse(mode, prec, tok)
    }

    pub fn reparse<P>(&mut self, mode: &'static P, prec: Precedence, tok: TokenSrc<'i>) -> Lresult<P::Item>
        where P: ParslMode<'i>
            , P::Item: fmt::Debug
    {
        self.parse(mode, prec, tok)
    }

    fn parse<P>(&mut self, mode: &'static P, prec: Precedence, tok0: TokenSrc<'i>) -> Lresult<P::Item>
        where P: ParslMode<'i>
            , P::Item: fmt::Debug
    {
        let mut left = match mode.prefix(tok0.tok) {
            Some(parser) => parser.parse(self, tok0)?,
            None => {
                return Err(rustfail!(
                    "parse_failure",
                    "cannot parse token in {:?}: {}",
                    mode,
                    tok0,
                ));
            }
        };

        loop {
            let peek_infix = self.peek_token()?;
            let infix = mode.infix(peek_infix);
            if infix.is_none() || infix.unwrap().precedence() < prec {
                break;
            }
            let itok = self.next()?;
            left = infix.unwrap().parse(self, left, itok)?;
        }
        Ok(left)
    }
}
