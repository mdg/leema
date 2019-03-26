use crate::leema::failure::Lresult;

use std::collections::HashMap;
use std::fmt;
use std::str::CharIndices;

use lazy_static::lazy_static;


#[derive(Copy)]
#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub struct Char
{
    pub index: usize,
    pub c: char,
    pub lineno: u16,
    pub column: u8,
}

impl Default for Char
{
    fn default() -> Char
    {
        Char {
            index: 0,
            c: '\0',
            lineno: 0,
            column: 0,
        }
    }
}

#[derive(Clone)]
struct CharIter<'input>
{
    chars: CharIndices<'input>,
    lineno: u16,
    column: u8,
}

impl<'input> CharIter<'input>
{
    fn new(src: &str) -> CharIter
    {
        CharIter {
            chars: src.char_indices(),
            lineno: 1,
            column: 1,
        }
    }

    fn next(&mut self) -> Option<Char>
    {
        match self.chars.next() {
            None => None,
            Some((index, '\n')) => {
                let result = Char {
                    index,
                    c: '\n',
                    lineno: self.lineno,
                    column: self.column,
                };
                self.lineno += 1;
                self.column = 1;
                Some(result)
            }
            Some((index, c)) => {
                let result = Char {
                    index,
                    c,
                    lineno: self.lineno,
                    column: self.column,
                };
                self.column += 1;
                Some(result)
            }
        }
    }
}

#[derive(Clone)]
#[derive(Copy)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
#[derive(Eq)]
#[derive(Ord)]
#[derive(Hash)]
pub enum Token
{
    Id,
    Int,
    Bool,
    Hashtag,
    StrLit,
    DollarId,

    // brackets
    ParenL,
    ParenR,
    SquareL,
    SquareR,
    CurlyL,
    CurlyR,
    AngleL,
    AngleR,
    DoubleQuoteL,
    DoubleQuoteR,

    // keywords
    Const,
    Else,
    Failed,
    Fork,
    Func,
    If,
    Import,
    Let,
    Macro,
    Match,
    Return,
    Type,
    Underscore,

    // operators (arithmetic)
    Plus,
    Dash,
    Star,
    Slash,
    Modulo,
    Dollar,

    // operators (boolean)
    And,
    Not,
    Or,
    Xor,

    // operators (comparison)
    Equal,
    EqualNot,
    GreaterThanEqual,
    LessThanEqual,

    // separators
    Assignment,
    CasePipe,
    Colon,
    Comma,
    ConcatNewline,
    Dot,
    DoubleArrow,
    DoubleColon,
    DoubleDash,
    Pipe,
    Semicolon,
    StatementSep,

    // comments
    CommentBlockStart,
    CommentBlockStop,
    CommentLine,

    // whitespace
    EmptyLine,
    LineBegin,
    LineEnd,
    Spaces,
    Tabs,

    // EOF
    EOF,

    // final counter
    NumTokens,
}

type TokenFilterFn = fn(Token) -> bool;

impl Token
{
    pub fn nofilter(_: Token) -> bool
    {
        true
    }

    pub fn parsefilter(tok: Token) -> bool
    {
        match tok {
            Token::EmptyLine => false,
            Token::LineEnd => false,
            Token::Spaces => false,
            _ => true,
        }
    }
}

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, Token> = {
        let mut keywords = HashMap::new();
        // keyword operators
        keywords.insert("and", Token::And);
        keywords.insert("mod", Token::Modulo);
        keywords.insert("not", Token::Not);
        keywords.insert("or", Token::Or);
        keywords.insert("xor", Token::Xor);
        // keywords
        keywords.insert("const", Token::Const);
        keywords.insert("else", Token::Else);
        keywords.insert("failed", Token::Failed);
        keywords.insert("fork", Token::Fork);
        keywords.insert("func", Token::Func);
        keywords.insert("if", Token::If);
        keywords.insert("import", Token::Import);
        keywords.insert("let", Token::Let);
        keywords.insert("macro", Token::Macro);
        keywords.insert("match", Token::Match);
        keywords.insert("return", Token::Return);
        keywords.insert("type", Token::Type);
        keywords.insert("_", Token::Underscore);
        // booleans
        keywords.insert("False", Token::Bool);
        keywords.insert("True", Token::Bool);
        keywords
    };
}

/// A location wrapper for the token
#[derive(Copy)]
#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub struct TokenSrc<'input>
{
    pub src: &'input str,
    pub tok: Token,
    pub begin: Char,
    pub len: usize,
}

pub type TokenResult<'input> = Lresult<TokenSrc<'input>>;

/// scan((start, line, col, (i, char))
/// return (consume_char, Option<new_token>, Option<push_scanner(scanner) | pop_state>) | error

trait ScanModeTrait: fmt::Debug
{
    fn scan(&self, c: Char) -> ScanResult;

    fn eof(&self) -> ScanResult
    {
        Ok(ScanOutput::EOF)
    }
}

// struct ScanMode(Box<ScanModeTrait>);
type ScanMode = &'static ScanModeTrait;

#[derive(Clone)]
#[derive(Debug)]
enum ScanModeOp
{
    Push(ScanMode),
    Replace(ScanMode),
    Pop,
    Noop,
}

enum ScanOutput
{
    Start(ScanModeOp),
    Next(ScanModeOp),
    Token(Token, bool, ScanModeOp),
    EOF,
}

type ScanResult = Lresult<ScanOutput>;

#[derive(Debug)]
struct ScanModeEOF;

#[derive(Debug)]
struct ScanModeFailure;

#[derive(Debug)]
struct ScanModeAngleL;

#[derive(Debug)]
struct ScanModeAngleR;

#[derive(Debug)]
struct ScanModeBackslash;

#[derive(Debug)]
struct ScanModeBang;

#[derive(Debug)]
struct ScanModeColon;

#[derive(Debug)]
struct ScanModeDash;

#[derive(Debug)]
struct ScanModeDollar;

#[derive(Debug)]
struct ScanModeEqual;

#[derive(Debug)]
struct ScanModeFileBegin;

#[derive(Debug)]
struct ScanModeId(Token);

struct ScanModeIndent
{
    space_or_tab: char,
}

impl fmt::Debug for ScanModeIndent
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "ScanModeIndent({:?})", self.space_or_tab)
    }
}

#[derive(Debug)]
struct ScanModeInt;

#[derive(Debug)]
struct ScanModeLine;

#[derive(Debug)]
struct ScanModeQuote;

#[derive(Debug)]
struct ScanModeSpace;

#[derive(Debug)]
struct ScanModeStr;

#[derive(Debug)]
struct ScanModeStrDollar;

impl ScanModeTrait for ScanModeLine
{
    fn scan(&self, next: Char) -> ScanResult
    {
        let output = match next.c {
            // brackets
            '(' => ScanOutput::Token(Token::ParenL, true, ScanModeOp::Noop),
            ')' => ScanOutput::Token(Token::ParenR, true, ScanModeOp::Noop),
            '{' => ScanOutput::Token(Token::CurlyL, true, ScanModeOp::Noop),
            '}' => ScanOutput::Token(Token::CurlyR, true, ScanModeOp::Noop),
            '[' => ScanOutput::Token(Token::SquareL, true, ScanModeOp::Noop),
            ']' => ScanOutput::Token(Token::SquareR, true, ScanModeOp::Noop),
            '<' => ScanOutput::Start(ScanModeOp::Push(&ScanModeAngleL)),
            '>' => ScanOutput::Start(ScanModeOp::Push(&ScanModeAngleR)),
            // arithmetic operators
            '+' => ScanOutput::Token(Token::Plus, true, ScanModeOp::Noop),
            '-' => ScanOutput::Start(ScanModeOp::Push(&ScanModeDash)),
            '*' => ScanOutput::Token(Token::Star, true, ScanModeOp::Noop),
            '/' => ScanOutput::Token(Token::Slash, true, ScanModeOp::Noop),
            '=' => ScanOutput::Start(ScanModeOp::Push(&ScanModeEqual)),
            '!' => ScanOutput::Start(ScanModeOp::Push(&ScanModeBang)),
            // separators
            ':' => ScanOutput::Start(ScanModeOp::Push(&ScanModeColon)),
            ',' => ScanOutput::Token(Token::Comma, true, ScanModeOp::Noop),
            '.' => ScanOutput::Token(Token::Dot, true, ScanModeOp::Noop),
            '|' => ScanOutput::Token(Token::Pipe, true, ScanModeOp::Noop),
            // whitespace
            '\n' => ScanOutput::Token(Token::LineEnd, true, ScanModeOp::Pop),
            ' ' => ScanOutput::Start(ScanModeOp::Push(&ScanModeSpace)),
            '\\' => ScanOutput::Start(ScanModeOp::Push(&ScanModeBackslash)),
            // strings
            '"' => {
                ScanOutput::Token(
                    Token::DoubleQuoteL,
                    true,
                    ScanModeOp::Push(&ScanModeQuote),
                )
            }
            '$' => ScanOutput::Start(ScanModeOp::Push(&ScanModeDollar)),
            // keywords
            '_' => ScanOutput::Start(ScanModeOp::Push(&ScanModeId(Token::Id))),
            c if c.is_alphabetic() => {
                ScanOutput::Start(ScanModeOp::Push(&ScanModeId(Token::Id)))
            }
            // numbers
            c if c.is_numeric() => {
                ScanOutput::Start(ScanModeOp::Push(&ScanModeInt))
            }
            _ => {
                eprintln!("root next: '{}'", next.c);
                ScanOutput::Next(ScanModeOp::Noop)
            }
        };
        Ok(output)
    }

    /*
    fn eof(&self, start: Char, next: Char, _src: &str) -> Option<ScanResult>
    {
        Ok(ScanOutput::Complete(Token(TokenData::EOF, next), true))
    }
    */
}

impl ScanModeTrait for ScanModeEOF
{
    fn scan(&self, _next: Char) -> ScanResult
    {
        Ok(ScanOutput::EOF)
    }
}

impl ScanModeTrait for ScanModeFailure
{
    fn scan(&self, next: Char) -> ScanResult
    {
        Err(rustfail!("token_failure", "token failure at {:?}", next))
    }
}

impl ScanModeTrait for ScanModeAngleL
{
    fn scan(&self, next: Char) -> ScanResult
    {
        match next.c {
            '=' => {
                Ok(ScanOutput::Token(
                    Token::LessThanEqual,
                    true,
                    ScanModeOp::Pop,
                ))
            }
            _ => Ok(ScanOutput::Token(Token::AngleL, false, ScanModeOp::Pop)),
        }
    }

    fn eof(&self) -> ScanResult
    {
        Ok(ScanOutput::Token(Token::AngleL, false, ScanModeOp::Pop))
    }
}

impl ScanModeTrait for ScanModeAngleR
{
    fn scan(&self, next: Char) -> ScanResult
    {
        match next.c {
            '=' => {
                Ok(ScanOutput::Token(
                    Token::GreaterThanEqual,
                    true,
                    ScanModeOp::Pop,
                ))
            }
            '>' => {
                Ok(ScanOutput::Token(Token::DoubleArrow, true, ScanModeOp::Pop))
            }
            _ => Ok(ScanOutput::Token(Token::AngleR, false, ScanModeOp::Pop)),
        }
    }

    fn eof(&self) -> ScanResult
    {
        Ok(ScanOutput::Token(Token::AngleR, false, ScanModeOp::Pop))
    }
}

impl ScanModeTrait for ScanModeBackslash
{
    fn scan(&self, next: Char) -> ScanResult
    {
        match next.c {
            'n' => {
                Ok(ScanOutput::Token(
                    Token::ConcatNewline,
                    true,
                    ScanModeOp::Pop,
                ))
            }
            '\n' => {
                Err(rustfail!(
                    "invalid_token",
                    "backslash-newline not yet supported",
                ))
            }
            _ => Err(rustfail!("invalid_token", "\\ is not a valid token")),
        }
    }

    fn eof(&self) -> ScanResult
    {
        Err(rustfail!("invalid_token", "\\ is not a valid token"))
    }
}

impl ScanModeTrait for ScanModeBang
{
    fn scan(&self, next: Char) -> ScanResult
    {
        let output = match next.c {
            '=' => ScanOutput::Token(Token::EqualNot, true, ScanModeOp::Pop),
            _ => {
                return Err(rustfail!(
                    "token_failure",
                    "'!' is not a valid token",
                ));
            }
        };
        Ok(output)
    }

    fn eof(&self) -> ScanResult
    {
        return Err(rustfail!("token_failure", "'!' is not a valid token",));
    }
}

impl ScanModeTrait for ScanModeColon
{
    fn scan(&self, next: Char) -> ScanResult
    {
        let output = match next.c {
            ':' => ScanOutput::Token(Token::DoubleColon, true, ScanModeOp::Pop),
            '=' => ScanOutput::Token(Token::Assignment, true, ScanModeOp::Pop),
            _ => ScanOutput::Token(Token::Colon, false, ScanModeOp::Pop),
        };
        Ok(output)
    }

    fn eof(&self) -> ScanResult
    {
        Ok(ScanOutput::Token(Token::Colon, false, ScanModeOp::Pop))
    }
}

impl ScanModeTrait for ScanModeDash
{
    fn scan(&self, next: Char) -> ScanResult
    {
        let output = match next.c {
            '-' => ScanOutput::Token(Token::DoubleDash, true, ScanModeOp::Pop),
            _ => ScanOutput::Token(Token::Dash, false, ScanModeOp::Pop),
        };
        Ok(output)
    }

    fn eof(&self) -> ScanResult
    {
        Ok(ScanOutput::Token(Token::Dash, false, ScanModeOp::Pop))
    }
}

impl ScanModeTrait for ScanModeDollar
{
    fn scan(&self, next: Char) -> ScanResult
    {
        let output = match next.c {
            c if c.is_alphabetic() => {
                let op = ScanModeOp::Replace(&ScanModeId(Token::DollarId));
                ScanOutput::Next(op)
            }
            _ => ScanOutput::Token(Token::Dollar, false, ScanModeOp::Pop),
        };
        Ok(output)
    }

    fn eof(&self) -> ScanResult
    {
        Ok(ScanOutput::Token(Token::Dash, false, ScanModeOp::Pop))
    }
}

impl ScanModeTrait for ScanModeEqual
{
    fn scan(&self, next: Char) -> ScanResult
    {
        match next.c {
            '=' => Ok(ScanOutput::Token(Token::Equal, true, ScanModeOp::Pop)),
            _ => {
                Err(rustfail!(
                    "invalid_token",
                    "single = is not a valid token. Use == or := at character: {:?}",
                    next,
                ))
            }
        }
    }

    fn eof(&self) -> ScanResult
    {
        Err(rustfail!(
            "invalid_token",
            "single = is not a valid token. Use == or := at EOF",
        ))
    }
}

impl ScanModeTrait for ScanModeFileBegin
{
    fn scan(&self, next: Char) -> ScanResult
    {
        match next.c {
            ' ' => {
                Ok(ScanOutput::Start(ScanModeOp::Replace(&ScanModeIndent {
                    space_or_tab: ' ',
                })))
            }
            '\t' => {
                Ok(ScanOutput::Start(ScanModeOp::Replace(&ScanModeIndent {
                    space_or_tab: '\t',
                })))
            }
            '\n' => {
                Ok(ScanOutput::Token(Token::EmptyLine, true, ScanModeOp::Noop))
            }
            _ => Ok(ScanOutput::Token(Token::LineBegin, false, PUSH_MODE_LINE)),
        }
    }
}

impl ScanModeTrait for ScanModeId
{
    fn scan(&self, next: Char) -> ScanResult
    {
        match next.c {
            '_' => Ok(ScanOutput::Next(ScanModeOp::Noop)),
            c if c.is_alphanumeric() => Ok(ScanOutput::Next(ScanModeOp::Noop)),
            _ => Ok(ScanOutput::Token(self.0, false, ScanModeOp::Pop)),
        }
    }

    fn eof(&self) -> ScanResult
    {
        Ok(ScanOutput::Token(Token::Id, false, ScanModeOp::Pop))
    }
}

impl ScanModeTrait for ScanModeIndent
{
    fn scan(&self, next: Char) -> ScanResult
    {
        match (self.space_or_tab, next.c) {
            (' ', ' ') => Ok(ScanOutput::Next(ScanModeOp::Noop)),
            ('\t', '\t') => Ok(ScanOutput::Next(ScanModeOp::Noop)),
            (_, '\n') => {
                Ok(ScanOutput::Token(Token::EmptyLine, true, ScanModeOp::Noop))
            }
            (' ', '\t') => {
                Err(rustfail!(
                    "token_failure",
                    "do not use tabs when already using spaces {},{}",
                    next.lineno,
                    next.column,
                ))
            }
            ('\t', ' ') => {
                Err(rustfail!(
                    "token_failure",
                    "do not use spaces when already using tabs {},{}",
                    next.lineno,
                    next.column,
                ))
            }
            _ => Ok(ScanOutput::Token(Token::LineBegin, false, PUSH_MODE_LINE)),
        }
    }
}

impl ScanModeTrait for ScanModeInt
{
    fn scan(&self, next: Char) -> ScanResult
    {
        if next.c.is_numeric() {
            Ok(ScanOutput::Next(ScanModeOp::Noop))
        } else {
            Ok(ScanOutput::Token(Token::Int, false, ScanModeOp::Pop))
        }
    }

    fn eof(&self) -> ScanResult
    {
        Ok(ScanOutput::Token(Token::Int, false, ScanModeOp::Pop))
    }
}

impl ScanModeTrait for ScanModeQuote
{
    fn scan(&self, next: Char) -> ScanResult
    {
        match next.c {
            '"' => {
                Ok(ScanOutput::Token(
                    Token::DoubleQuoteR,
                    true,
                    ScanModeOp::Pop,
                ))
            }
            '$' => Ok(ScanOutput::Start(ScanModeOp::Push(&ScanModeDollar))),
            _ => Ok(ScanOutput::Start(ScanModeOp::Push(&ScanModeStr))),
        }
    }

    fn eof(&self) -> ScanResult
    {
        Err(rustfail!("token_error", "unexpected end of file",))
    }
}

impl ScanModeTrait for ScanModeSpace
{
    fn scan(&self, next: Char) -> ScanResult
    {
        match next.c {
            ' ' => Ok(ScanOutput::Next(ScanModeOp::Noop)),
            '\t' => {
                Err(rustfail!(
                    "mixed_spaces_tabs",
                    "Do not use tabes when already using spaces {},{}",
                    next.lineno,
                    next.column,
                ))
            }
            _ => Ok(ScanOutput::Token(Token::Spaces, false, ScanModeOp::Pop)),
        }
    }

    fn eof(&self) -> ScanResult
    {
        Ok(ScanOutput::Token(Token::Spaces, false, ScanModeOp::Pop))
    }
}

impl ScanModeTrait for ScanModeStr
{
    fn scan(&self, next: Char) -> ScanResult
    {
        match next.c {
            '"' => Ok(ScanOutput::Token(Token::StrLit, false, ScanModeOp::Pop)),
            '$' => Ok(ScanOutput::Token(Token::StrLit, false, ScanModeOp::Pop)),
            _ => Ok(ScanOutput::Next(ScanModeOp::Noop)),
        }
    }

    fn eof(&self) -> ScanResult
    {
        Ok(ScanOutput::Token(Token::Int, false, ScanModeOp::Pop))
    }
}

impl ScanModeTrait for ScanModeStrDollar
{
    fn scan(&self, next: Char) -> ScanResult
    {
        let out = match next.c {
            '$' => ScanOutput::Start(ScanModeOp::Replace(&ScanModeDollar)),
            _ => {
                return Err(rustfail!(
                    "token_failure",
                    "expected $ found {:?}",
                    next,
                ));
            }
        };
        Ok(out)
    }

    fn eof(&self) -> ScanResult
    {
        Err(rustfail!("token_failure", "expected $ found eof"))
    }
}

const FILE_BEGIN: ScanMode = &ScanModeFileBegin;
const PUSH_MODE_INDENT_SPACE: ScanModeOp =
    ScanModeOp::Push(&ScanModeIndent { space_or_tab: ' ' });
const PUSH_MODE_INDENT_TAB: ScanModeOp =
    ScanModeOp::Push(&ScanModeIndent { space_or_tab: '\t' });
const PUSH_MODE_INT: ScanModeOp = ScanModeOp::Push(&ScanModeInt);
const PUSH_MODE_LINE: ScanModeOp = ScanModeOp::Push(&ScanModeLine);

pub struct Tokenz<'input>
{
    src: &'input str,
    chars: CharIter<'input>,
    filter: TokenFilterFn,

    mode: ScanMode,
    mode_stack: Vec<ScanMode>,
    // paren_stack: Vec<u16>,
    begin: Option<Char>,
    len: usize,
    unused_next: Option<Char>,
    eof: bool,
}

impl<'input> Tokenz<'input>
{
    pub fn lexp(src: &'input str) -> Lresult<Vec<TokenSrc<'input>>>
    {
        let mut toks = Tokenz::lex(src);
        toks.set_filter(Token::parsefilter);
        toks.collect()
    }

    pub fn lex(src: &'input str) -> Tokenz<'input>
    {
        let chars = CharIter::new(src);
        Tokenz {
            src,
            chars,
            filter: Token::nofilter,

            mode: FILE_BEGIN,
            mode_stack: vec![],

            begin: None,
            len: 0,
            unused_next: None,
            eof: false,
        }
    }

    pub fn set_filter(&mut self, filter: TokenFilterFn)
    {
        self.filter = filter;
    }

    pub fn expect(&mut self, tok: Token) -> TokenResult<'input>
    {
        match self.next() {
            Some(Ok(toksrc)) if toksrc.tok == tok => Ok(toksrc),
            Some(Ok(toksrc)) => {
                Err(rustfail!(
                    "parse_failure",
                    "expected {:?}, found {:?}",
                    tok,
                    toksrc,
                ))
            }
            Some(Err(e)) => Err(e),
            None => {
                Err(rustfail!("parse_failure", "expected {:?}, found eof", tok))
            }
        }
    }

    fn unfiltered_next(&mut self) -> Option<TokenResult<'input>>
    {
        if self.eof {
            return None;
        }
        let mut c = self.next_char();
        loop {
            match self.mode_scan(c) {
                Ok(ScanOutput::Start(mode_op)) => {
                    self.begin = c;
                    self.len = 1;
                    self.next_mode(mode_op);
                }
                Ok(ScanOutput::Next(mode_op)) => {
                    if self.begin.is_none() {
                        self.begin = c;
                        self.len = 0;
                    }
                    self.len += 1;
                    self.next_mode(mode_op);
                }
                Ok(ScanOutput::Token(tok, consume, mode_op)) => {
                    let tokr = self.next_token(tok, consume, c);
                    if tokr.is_ok() {
                        self.next_mode(mode_op);
                    }
                    return Some(tokr);
                }
                Ok(ScanOutput::EOF) => {
                    self.eof = true;
                    return Some(Ok(TokenSrc {
                        src: "",
                        tok: Token::EOF,
                        begin: Char::default(),
                        len: 0,
                    }));
                }
                Err(fail) => {
                    self.mode = &ScanModeEOF;
                    return Some(Err(fail));
                }
            }
            c = self.chars.next();
        }
    }

    fn next_char(&mut self) -> Option<Char>
    {
        self.unused_next.take().or_else(|| self.chars.next())
    }

    fn mode_scan(&mut self, opt_c: Option<Char>) -> ScanResult
    {
        match opt_c {
            Some(c) => self.mode.scan(c),
            None => self.mode.eof(),
        }
    }

    fn next_mode(&mut self, mode_op: ScanModeOp)
    {
        match mode_op {
            ScanModeOp::Push(new_mode) => {
                self.mode_stack.push(self.mode);
                self.mode = new_mode;
            }
            ScanModeOp::Replace(new_mode) => {
                self.mode = new_mode;
            }
            ScanModeOp::Pop => {
                self.mode = self.mode_stack.pop().unwrap_or(&ScanModeFailure);
            }
            ScanModeOp::Noop => {} // Noop means do nothing
        }
    }

    fn next_token(
        &mut self,
        mut tok: Token,
        consume: bool,
        c: Option<Char>,
    ) -> TokenResult<'input>
    {
        let begin = match self.begin.or(c) {
            Some(b) => b,
            None => {
                return Err(rustfail!(
                    "token_failure",
                    "begin is none, why isn't this already",
                ));
            }
        };
        if consume {
            self.len += 1;
            self.unused_next = None;
        } else {
            self.unused_next = c;
        }
        let len = self.len;

        self.begin = None;
        self.len = 0;

        let src = &self.src[begin.index..(begin.index + len)];
        let keyword = KEYWORDS.get(src);
        if keyword.is_some() {
            tok = *keyword.unwrap();
        }

        Ok(TokenSrc {
            src,
            tok,
            begin,
            len,
        })
    }
}

impl<'input> Iterator for Tokenz<'input>
{
    type Item = TokenResult<'input>;

    fn next(&mut self) -> Option<TokenResult<'input>>
    {
        let filter_fn = self.filter;
        loop {
            match self.unfiltered_next() {
                Some(Ok(tok)) => {
                    if filter_fn(tok.tok) {
                        return Some(Ok(tok));
                    }
                    // continue
                }
                Some(Err(e)) => {
                    return Some(Err(e));
                }
                None => {
                    return None;
                }
            }
        }
    }
}

#[cfg(test)]
mod tests
{
    use super::{Token, TokenResult, Tokenz};

    use std::iter::Iterator;


    fn nextok<'a, 'b, I>(it: &'a mut I) -> (Token, &'static str)
    where
        I: Iterator<Item = &'b TokenResult<'static>>,
        'b: 'a,
    {
        let tokr: &TokenResult = it.next().as_ref().unwrap();
        let t = tokr.as_ref().unwrap();
        (t.tok, t.src)
    }

    #[test]
    fn test_tokenz_brackets()
    {
        let input = "(){}[]<>";

        let t: Vec<TokenResult<'static>> = Tokenz::lex(input).collect();
        let mut i = t.iter();
        assert_eq!(Token::LineBegin, nextok(&mut i).0);
        assert_eq!(Token::ParenL, nextok(&mut i).0);
        assert_eq!(Token::ParenR, nextok(&mut i).0);
        assert_eq!(Token::CurlyL, nextok(&mut i).0);
        assert_eq!(Token::CurlyR, nextok(&mut i).0);
        assert_eq!(Token::SquareL, nextok(&mut i).0);
        assert_eq!(Token::SquareR, nextok(&mut i).0);
        assert_eq!(Token::AngleL, nextok(&mut i).0);
        assert_eq!(Token::AngleR, nextok(&mut i).0);
        assert_eq!(Token::EOF, nextok(&mut i).0);
        assert_eq!(None, i.next());
    }

    #[test]
    fn test_tokenz_doubledash()
    {
        let input = "- -- ---";

        let t: Vec<TokenResult<'static>> = Tokenz::lex(input).collect();
        let mut i = t.iter();
        assert_eq!(Token::LineBegin, nextok(&mut i).0);
        assert_eq!(Token::Dash, nextok(&mut i).0);
        i.next();
        assert_eq!(Token::DoubleDash, nextok(&mut i).0);
        i.next();
        assert_eq!(Token::DoubleDash, nextok(&mut i).0);
        assert_eq!(Token::Dash, nextok(&mut i).0);
        assert_eq!(Token::EOF, nextok(&mut i).0);
        assert_eq!(None, i.next());
    }

    #[test]
    fn test_tokenz_int()
    {
        let input = "1234 999999999999999";

        let t: Vec<TokenResult<'static>> = Tokenz::lex(input).collect();
        let mut i = t.iter();
        assert_eq!(Token::LineBegin, nextok(&mut i).0);
        assert_eq!((Token::Int, "1234"), nextok(&mut i));
        i.next();
        assert_eq!((Token::Int, "999999999999999"), nextok(&mut i));
        assert_eq!(Token::EOF, nextok(&mut i).0);
        assert_eq!(None, i.next());
    }

    #[test]
    fn test_tokenz_ids()
    {
        let input = "tacos burrit_s";

        let t: Vec<TokenResult<'static>> = Tokenz::lex(input).collect();
        let mut i = t.iter();
        assert_eq!((Token::LineBegin, ""), nextok(&mut i));
        assert_eq!((Token::Id, "tacos"), nextok(&mut i));
        assert_eq!((Token::Spaces, " "), nextok(&mut i));
        assert_eq!((Token::Id, "burrit_s"), nextok(&mut i));
        assert_eq!(Token::EOF, nextok(&mut i).0);
        assert_eq!(None, i.next());
    }

    #[test]
    fn test_tokenz_keyword_operators()
    {
        let input = "and mod not or xor";

        let t: Vec<TokenResult<'static>> = Tokenz::lex(input).collect();
        let mut i = t.iter();
        assert_eq!(Token::LineBegin, nextok(&mut i).0);
        assert_eq!(Token::And, nextok(&mut i).0);
        i.next();
        assert_eq!(Token::Modulo, nextok(&mut i).0);
        i.next();
        assert_eq!(Token::Not, nextok(&mut i).0);
        i.next();
        assert_eq!(Token::Or, nextok(&mut i).0);
        i.next();
        assert_eq!(Token::Xor, nextok(&mut i).0);
        assert_eq!(Token::EOF, nextok(&mut i).0);
        assert_eq!(None, i.next());
    }

    #[test]
    fn test_tokenz_keywords()
    {
        let input = "failed fork func if import let macro match return";

        let t: Vec<TokenResult<'static>> = Tokenz::lex(input).collect();
        let mut i = t.iter();
        assert_eq!(Token::LineBegin, nextok(&mut i).0);
        assert_eq!(Token::Failed, nextok(&mut i).0);
        i.next();
        assert_eq!(Token::Fork, nextok(&mut i).0);
        i.next();
        assert_eq!(Token::Func, nextok(&mut i).0);
        i.next();
        assert_eq!(Token::If, nextok(&mut i).0);
        i.next();
        assert_eq!(Token::Import, nextok(&mut i).0);
        i.next();
        assert_eq!(Token::Let, nextok(&mut i).0);
        i.next();
        assert_eq!(Token::Macro, nextok(&mut i).0);
        i.next();
        assert_eq!(Token::Match, nextok(&mut i).0);
        i.next();
        assert_eq!(Token::Return, nextok(&mut i).0);
        assert_eq!(Token::EOF, nextok(&mut i).0);
        assert_eq!(None, i.next());
    }

    #[test]
    fn test_tokenz_arithmetic_operators()
    {
        let input = "+ - * /";

        let t: Vec<TokenResult<'static>> = Tokenz::lex(input).collect();
        let mut i = t.iter();
        assert_eq!(Token::LineBegin, nextok(&mut i).0);
        assert_eq!(Token::Plus, nextok(&mut i).0);
        assert_eq!((Token::Spaces, " "), nextok(&mut i));
        assert_eq!(Token::Dash, nextok(&mut i).0);
        i.next();
        assert_eq!(Token::Star, nextok(&mut i).0);
        i.next();
        assert_eq!(Token::Slash, nextok(&mut i).0);
        assert_eq!(Token::EOF, nextok(&mut i).0);
        assert_eq!(None, i.next());
    }

    #[test]
    fn test_tokenz_comparison_operators()
    {
        let input = "== != < > <= >= \\n >> >";

        let t: Vec<TokenResult<'static>> = Tokenz::lex(input).collect();
        let mut i = t.iter();
        assert_eq!(Token::LineBegin, nextok(&mut i).0);
        assert_eq!(Token::Equal, nextok(&mut i).0);
        i.next();
        assert_eq!(Token::EqualNot, nextok(&mut i).0);
        i.next();
        assert_eq!(Token::AngleL, nextok(&mut i).0);
        i.next();
        assert_eq!(Token::AngleR, nextok(&mut i).0);
        i.next();
        assert_eq!(Token::LessThanEqual, nextok(&mut i).0);
        i.next();
        assert_eq!(Token::GreaterThanEqual, nextok(&mut i).0);
        i.next();
        assert_eq!(Token::ConcatNewline, nextok(&mut i).0);
        i.next();
        assert_eq!(Token::DoubleArrow, nextok(&mut i).0);
        i.next();
        assert_eq!(Token::AngleR, nextok(&mut i).0);
        assert_eq!(Token::EOF, nextok(&mut i).0);
        assert_eq!(None, i.next());
    }

    #[test]
    fn test_tokenz_separators()
    {
        let input = ": , . :: := | :";

        let t: Vec<TokenResult<'static>> = Tokenz::lex(input).collect();
        let mut i = t.iter();
        assert_eq!(Token::LineBegin, nextok(&mut i).0);
        assert_eq!(Token::Colon, nextok(&mut i).0);
        i.next();
        assert_eq!(Token::Comma, nextok(&mut i).0);
        i.next();
        assert_eq!(Token::Dot, nextok(&mut i).0);
        i.next();
        assert_eq!(Token::DoubleColon, nextok(&mut i).0);
        i.next();
        assert_eq!(Token::Assignment, nextok(&mut i).0);
        i.next();
        assert_eq!(Token::Pipe, nextok(&mut i).0);
        i.next();
        assert_eq!(Token::Colon, nextok(&mut i).0);
        assert_eq!(Token::EOF, nextok(&mut i).0);
        assert_eq!(None, i.next());
    }

    #[test]
    fn test_tokenz_strings()
    {
        let input = r#""tacos" "" "flautas $tortas burritos""#;

        let t: Vec<TokenResult<'static>> = Tokenz::lex(input).collect();
        let mut i = t.iter();

        assert_eq!(Token::LineBegin, nextok(&mut i).0);
        assert_eq!(Token::DoubleQuoteL, nextok(&mut i).0);
        assert_eq!((Token::StrLit, "tacos"), nextok(&mut i));
        assert_eq!(Token::DoubleQuoteR, nextok(&mut i).0);
        i.next();
        assert_eq!(Token::DoubleQuoteL, nextok(&mut i).0);
        assert_eq!(Token::DoubleQuoteR, nextok(&mut i).0);
        i.next();
        assert_eq!(Token::DoubleQuoteL, nextok(&mut i).0);
        assert_eq!((Token::StrLit, "flautas "), nextok(&mut i));
        assert_eq!((Token::DollarId, "$tortas"), nextok(&mut i));
        assert_eq!((Token::StrLit, " burritos"), nextok(&mut i));
        assert_eq!(Token::DoubleQuoteR, nextok(&mut i).0);

        assert_eq!(Token::EOF, nextok(&mut i).0);
        assert_eq!(None, i.next());
    }

    #[test]
    fn test_tokenz_iter()
    {
        let input = r"1 2
        5 6
        8 9
        ";

        let tokv: Vec<TokenResult> = Tokenz::lex(input).collect();
        assert_eq!(16, tokv.len());

        let toki = Tokenz::lex(input);
        let ftoks: Vec<TokenResult> = toki
            .filter(|opt_t| {
                if opt_t.is_err() {
                    return true;
                }
                opt_t.as_ref().unwrap().tok != Token::LineEnd
            })
            .collect();
        assert_eq!(13, ftoks.len());
    }

    #[test]
    fn test_tokenz_func()
    {
        let input = "
        func tacos: Int
        .filling: Str
        .size: Int
        >>
            make_tacos(meat, size)
        --
        ";

        let t: Vec<TokenResult<'static>> = Tokenz::lex(input).collect();
        let mut i = t.iter();

        assert_eq!(Token::EmptyLine, nextok(&mut i).0);

        assert_eq!((Token::LineBegin, "        "), nextok(&mut i));
        assert_eq!(Token::Func, nextok(&mut i).0);
        i.next();
        assert_eq!((Token::Id, "tacos"), nextok(&mut i));
        assert_eq!(Token::Colon, nextok(&mut i).0);
        i.next();
        assert_eq!((Token::Id, "Int"), nextok(&mut i));
        assert_eq!(Token::LineEnd, nextok(&mut i).0);

        assert_eq!(Token::LineBegin, nextok(&mut i).0);
        assert_eq!(Token::Dot, nextok(&mut i).0);
        assert_eq!((Token::Id, "filling"), nextok(&mut i));
        assert_eq!(Token::Colon, nextok(&mut i).0);
        i.next();
        assert_eq!((Token::Id, "Str"), nextok(&mut i));
        assert_eq!(Token::LineEnd, nextok(&mut i).0);

        assert_eq!(Token::LineBegin, nextok(&mut i).0);
        assert_eq!(Token::Dot, nextok(&mut i).0);
        assert_eq!((Token::Id, "size"), nextok(&mut i));
        assert_eq!(Token::Colon, nextok(&mut i).0);
        i.next();
        assert_eq!((Token::Id, "Int"), nextok(&mut i));
        assert_eq!(Token::LineEnd, nextok(&mut i).0);

        assert_eq!((Token::LineBegin, "        "), nextok(&mut i));
        assert_eq!(Token::DoubleArrow, nextok(&mut i).0);
        assert_eq!(Token::LineEnd, nextok(&mut i).0);

        assert_eq!(Token::LineBegin, nextok(&mut i).0);
        assert_eq!((Token::Id, "make_tacos"), nextok(&mut i));
        assert_eq!(Token::ParenL, nextok(&mut i).0);
        assert_eq!((Token::Id, "meat"), nextok(&mut i));
        assert_eq!(Token::Comma, nextok(&mut i).0);
        i.next();
        assert_eq!((Token::Id, "size"), nextok(&mut i));
        assert_eq!(Token::ParenR, nextok(&mut i).0);
        assert_eq!(Token::LineEnd, nextok(&mut i).0);

        assert_eq!(Token::LineBegin, nextok(&mut i).0);
        assert_eq!(Token::DoubleDash, nextok(&mut i).0);
        assert_eq!(Token::LineEnd, nextok(&mut i).0);

        assert_eq!(Token::EOF, nextok(&mut i).0);
        assert_eq!(None, i.next());
    }

    #[test]
    fn test_tokenz_struct()
    {
        let input = "
        type Foo[T]
        .dog: T
        .cat: Str
        .mouse: Int
        --
        ";

        let t: Vec<TokenResult<'static>> = Tokenz::lex(input).collect();
        let mut i = t.iter();

        i.next();

        assert_eq!(Token::LineBegin, nextok(&mut i).0);
        assert_eq!(Token::Type, nextok(&mut i).0);
        i.next();
        assert_eq!((Token::Id, "Foo"), nextok(&mut i));
        assert_eq!(Token::SquareL, nextok(&mut i).0);
        assert_eq!((Token::Id, "T"), nextok(&mut i));
        assert_eq!(Token::SquareR, nextok(&mut i).0);
        assert_eq!(Token::LineEnd, nextok(&mut i).0);

        assert_eq!(Token::LineBegin, nextok(&mut i).0);
        assert_eq!(Token::Dot, nextok(&mut i).0);
        assert_eq!((Token::Id, "dog"), nextok(&mut i));
        assert_eq!(Token::Colon, nextok(&mut i).0);
        i.next();
        assert_eq!((Token::Id, "T"), nextok(&mut i));
    }

    #[test]
    fn test_tokenz_enum()
    {
        let input = "
        type Roulette
        |Red
        |Black
        --
        ";

        let t: Vec<TokenResult<'static>> = Tokenz::lex(input).collect();
        let mut i = t.iter();

        i.next();

        assert_eq!(Token::LineBegin, nextok(&mut i).0);
        assert_eq!(Token::Type, nextok(&mut i).0);
        i.next();
        assert_eq!((Token::Id, "Roulette"), nextok(&mut i));
        assert_eq!(Token::LineEnd, nextok(&mut i).0);

        assert_eq!(Token::LineBegin, nextok(&mut i).0);
        assert_eq!(Token::Pipe, nextok(&mut i).0);
        assert_eq!((Token::Id, "Red"), nextok(&mut i));
        assert_eq!(Token::LineEnd, nextok(&mut i).0);

        assert_eq!(Token::LineBegin, nextok(&mut i).0);
        assert_eq!(Token::Pipe, nextok(&mut i).0);
        assert_eq!((Token::Id, "Black"), nextok(&mut i));
        assert_eq!(Token::LineEnd, nextok(&mut i).0);

        assert_eq!(Token::LineBegin, nextok(&mut i).0);
        assert_eq!(Token::DoubleDash, nextok(&mut i).0);
        assert_eq!(Token::LineEnd, nextok(&mut i).0);

        assert_eq!(Token::EOF, nextok(&mut i).0);
        assert_eq!(None, i.next());
    }

    #[test]
    fn test_lexp_constint()
    {
        let input = "const X := 5";
        let t = Tokenz::lexp(input).unwrap();
        let mut i = t.iter();

        assert_eq!(Token::LineBegin, i.next().unwrap().tok);
        assert_eq!(Token::Const, i.next().unwrap().tok);
        assert_eq!(Token::Id, i.next().unwrap().tok);
        assert_eq!(Token::Assignment, i.next().unwrap().tok);
        assert_eq!(Token::Int, i.next().unwrap().tok);
        assert_eq!(Token::EOF, i.next().unwrap().tok);
        assert_eq!(None, i.next());
    }
}
