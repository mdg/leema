use crate::leema::failure::{self, Failure, Lresult};
use crate::leema::lstr::Lstr;
use crate::leema::token::{Token, TokenResult, TokenSrc};

use std::fmt;


#[macro_export]
macro_rules! expect_next {
    ($p:expr, $expected:expr) => {{
        let tok = $p.next()?;
        if tok.tok == $expected {
            Ok(tok)
        } else {
            let f = crate::leema::failure::Failure::static_leema(
                crate::leema::failure::Mode::ParseFailure,
                lstrf!("expected {:?}, found {:?}", $expected, tok.tok),
                $p.path.clone(),
                tok.begin.lineno,
            )
            .loc(file!(), line!());
            Err(f)
        }
    }};
}

struct TokenStream
{
    it: ::std::vec::IntoIter<TokenSrc>,
    peeked: Option<TokenSrc>,
}

impl TokenStream
{
    pub fn new(items: Vec<TokenSrc>) -> TokenStream
    {
        TokenStream {
            it: items.into_iter(),
            peeked: None,
        }
    }

    /// Peek at the next token without consuming it
    fn peek(&mut self) -> TokenResult
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
    pub fn next(&mut self) -> TokenResult
    {
        self.peek().map_err(|f| f.loc(file!(), line!()))?;
        self.peeked
            .take()
            .ok_or_else(|| rustfail!("parse_failure", "failed peek"))
    }

    /// Consume the next token if it is of the expected type
    pub fn next_if(&mut self, t: Token) -> Lresult<Option<TokenSrc>>
    {
        let tok = self.peek().map_err(|f| f.loc(file!(), line!()))?;
        if tok.tok == t {
            self.peeked = None;
            Ok(Some(tok))
        } else {
            Ok(None)
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

pub trait PrefixParser: fmt::Debug
{
    type Item;

    fn parse(&self, p: &mut Parsl, tok: TokenSrc) -> Lresult<Self::Item>;
}

pub trait InfixParser: fmt::Debug
{
    type Item;

    fn parse(
        &self,
        p: &mut Parsl,
        left: Self::Item,
        tok: TokenSrc,
    ) -> Lresult<Self::Item>;

    fn precedence(&self) -> Precedence;
}

#[derive(Debug)]
pub struct ParseFirst<P: 'static>(pub &'static P);

impl<P> PrefixParser for ParseFirst<P>
where
    P: PrefixParser,
    P::Item: fmt::Debug,
{
    type Item = Vec<P::Item>;

    fn parse(&self, p: &mut Parsl, tok: TokenSrc) -> Lresult<Vec<P::Item>>
    {
        let first = self.0.parse(p, tok)?;
        Ok(vec![first])
    }
}

#[derive(Debug)]
pub struct ParseMore<P: 'static>(pub &'static P, pub Precedence);

impl<P> InfixParser for ParseMore<P>
where
    P: PrefixParser,
    P::Item: fmt::Debug,
{
    type Item = Vec<P::Item>;

    fn parse(
        &self,
        p: &mut Parsl,
        mut left: Vec<P::Item>,
        tok: TokenSrc,
    ) -> Lresult<Vec<P::Item>>
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

pub trait ParslMode: fmt::Debug
{
    type Item;

    /// Some modes start with an implicit prefix and are infix only
    /// like a list of parameters
    fn implicit_prefix(&self) -> Option<Self::Item>
    {
        None
    }

    /// Get a prefix parser for this token
    fn prefix(&self, _tok: Token) -> Option<&dyn PrefixParser<Item = Self::Item>>
    {
        None
    }

    fn infix(&self, _tok: Token) -> Option<&dyn InfixParser<Item = Self::Item>>
    {
        None
    }
}

pub struct Parsl
{
    src: TokenStream,
    pub path: Lstr,
}

impl Parsl
{
    pub fn new(src: Vec<TokenSrc>) -> Parsl
    {
        let strm = TokenStream::new(src);
        Parsl {
            src: strm,
            path: Lstr::Sref(""),
        }
    }

    pub fn peek_token(&mut self) -> Lresult<Token>
    {
        self.src.peek_token()
    }

    pub fn peek(&mut self) -> TokenResult
    {
        self.src.peek()
    }

    pub fn next(&mut self) -> TokenResult
    {
        self.src.next()
    }

    pub fn next_if(&mut self, tok: Token) -> Lresult<Option<TokenSrc>>
    {
        self.src.next_if(tok)
    }

    pub fn skip_if(&mut self, tok: Token) -> Lresult<bool>
    {
        let mut found = false;
        while self.next_if(tok)?.is_some() {
            found = true;
            // keep going
        }
        Ok(found)
    }

    pub fn parse_new<P>(&mut self, mode: &'static P) -> Lresult<P::Item>
    where
        P: ParslMode,
        P::Item: fmt::Debug,
    {
        let tok = self.next()?;
        self.parse(mode, MIN_PRECEDENCE, tok)
    }

    pub fn parse_more<P>(
        &mut self,
        mode: &'static P,
        prec: Precedence,
    ) -> Lresult<P::Item>
    where
        P: ParslMode,
        P::Item: fmt::Debug,
    {
        let tok = ltry!(self.next());
        lfailoc!(self.parse(mode, prec, tok))
    }

    pub fn reparse<P>(
        &mut self,
        mode: &'static P,
        prec: Precedence,
        tok: TokenSrc,
    ) -> Lresult<P::Item>
    where
        P: ParslMode,
        P::Item: fmt::Debug,
    {
        lfailoc!(self.parse(mode, prec, tok))
    }

    fn parse<P>(
        &mut self,
        mode: &'static P,
        prec: Precedence,
        tok0: TokenSrc,
    ) -> Lresult<P::Item>
    where
        P: ParslMode,
        P::Item: fmt::Debug,
    {
        let mut left = match mode.prefix(tok0.tok) {
            Some(parser) => parser.parse(self, tok0)?,
            None => {
                return Err(Failure::static_leema(
                    failure::Mode::ParseFailure,
                    lstrf!("cannot parse token in {:?}: {:?}", mode, tok0.tok),
                    self.path.clone(),
                    tok0.begin.lineno,
                )
                .loc(file!(), line!()));
            }
        };

        loop {
            let peek_infix = self.peek_token()?;
            let infix = mode.infix(peek_infix);
            if infix.is_none() || infix.unwrap().precedence() < prec {
                break;
            }
            let itok = self.next()?;
            left = ltry!(infix.unwrap().parse(self, left, itok));
        }
        Ok(left)
    }
}
