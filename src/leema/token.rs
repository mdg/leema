use leema::failure::Lresult;

use std::collections::HashMap;
use std::fmt::Debug;
use std::str::CharIndices;


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
pub enum Token
{
    Id,
    Int,
    Bool(bool),
    Hashtag,
    StrLit,
    DollarId,
    Underscore,

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

    // operators (arithmetic)
    Plus,
    Dash,
    Star,
    Slash,
    Modulo,

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
    Indent,
    LineBegin,
    LineEnd,
    Spaces,
    Tabs,

    // EOF
    EOF,
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
        keywords
    };
}

/// A location wrapper for the token
#[derive(Copy)]
#[derive(Clone)]
#[derive(Debug)]
pub struct TokenChars<'input>
{
    tok: Token,
    first: Char,
    last: Char,
    src: &'input str,
}

type TokenResult<'input> = Lresult<TokenChars<'input>>;

/// scan((start, line, col, (i, char))
/// return (consume_char, Option<new_token>, Option<push_scanner(scanner) | pop_state>) | error

trait ScanModeTrait: Debug
{
    fn scan(&self, Char) -> ScanResult;

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

/*
StrLit,
Comment,
BlockComment,
*/

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
struct ScanModeColon;

#[derive(Debug)]
struct ScanModeDash;

#[derive(Debug)]
struct ScanModeEqual;

#[derive(Debug)]
struct ScanModeId;

#[derive(Debug)]
struct ScanModeIndent
{
    space_or_tab: char,
}

#[derive(Debug)]
struct ScanModeInt;

#[derive(Debug)]
struct ScanModeLine;

#[derive(Debug)]
struct ScanModeLineBegin;

#[derive(Debug)]
struct ScanModeQuote;

#[derive(Debug)]
struct ScanModeSpace;

#[derive(Debug)]
struct ScanModeStr;

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
            // keywords
            c if c.is_alphabetic() => {
                ScanOutput::Start(ScanModeOp::Push(&ScanModeId))
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

impl ScanModeTrait for ScanModeId
{
    fn scan(&self, next: Char) -> ScanResult
    {
        match next.c {
            '_' => Ok(ScanOutput::Next(ScanModeOp::Noop)),
            c if c.is_alphanumeric() => Ok(ScanOutput::Next(ScanModeOp::Noop)),
            _ => Ok(ScanOutput::Token(Token::Id, false, ScanModeOp::Pop)),
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
            _ => {
                Ok(ScanOutput::Token(Token::Indent, false, PUSH_MODE_LINE))
            }
        }
    }

    fn eof(&self) -> ScanResult
    {
        Ok(ScanOutput::Token(Token::Spaces, false, ScanModeOp::Pop))
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

impl ScanModeTrait for ScanModeLineBegin
{
    fn scan(&self, next: Char) -> ScanResult
    {
        match next.c {
            ' ' => {
                Ok(ScanOutput::Token(Token::LineBegin, false, PUSH_MODE_INDENT_SPACE))
            }
            '\t' => {
                Ok(ScanOutput::Token(Token::LineBegin, false, PUSH_MODE_INDENT_TAB))
            }
            _ => {
                Ok(ScanOutput::Token(Token::LineBegin, false, PUSH_MODE_LINE))
            }
        }
    }

    fn eof(&self) -> ScanResult
    {
        Err(rustfail!("invalid_token", "\\ is not a valid token"))
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
            ' ' => {
                Ok(ScanOutput::Next(ScanModeOp::Noop))
            }
            '\t' => {
                Err(rustfail!(
                    "mixed_spaces_tabs",
                    "Do not use tabes when already using spaces {},{}",
                    next.lineno,
                    next.column,
                ))
            }
            _ => {
                eprintln!("end of space");
                Ok(ScanOutput::Token(Token::Spaces, false, ScanModeOp::Pop))
            }
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
            _ => Ok(ScanOutput::Next(ScanModeOp::Noop)),
        }
    }

    fn eof(&self) -> ScanResult
    {
        Ok(ScanOutput::Token(Token::Int, false, ScanModeOp::Pop))
    }
}

const LINE_BEGIN: ScanMode = &ScanModeLineBegin;
const PUSH_MODE_INDENT_SPACE: ScanModeOp =
    ScanModeOp::Push(&ScanModeIndent{space_or_tab: ' '});
const PUSH_MODE_INDENT_TAB: ScanModeOp =
    ScanModeOp::Push(&ScanModeIndent{space_or_tab: '\t'});
const PUSH_MODE_INT: ScanModeOp = ScanModeOp::Push(&ScanModeInt);
const PUSH_MODE_LINE: ScanModeOp = ScanModeOp::Push(&ScanModeLine);

struct Tokenz<'input>
{
    src: &'input str,
    chars: CharIter<'input>,

    mode: ScanMode,
    mode_stack: Vec<ScanMode>,
    // paren_stack: Vec<u16>,
    first: Option<Char>,
    last: Option<Char>,
    unused_next: Option<Char>,
}

impl<'input> Tokenz<'input>
{
    pub fn lex(src: &'input str) -> Tokenz<'input>
    {
        let chars = CharIter::new(src);
        Tokenz {
            src,
            chars,

            mode: LINE_BEGIN,
            mode_stack: vec![],

            first: None,
            last: None,
            unused_next: None,
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
}

impl<'input> Iterator for Tokenz<'input>
{
    type Item = TokenResult<'input>;

    fn next(&mut self) -> Option<TokenResult<'input>>
    {
        let mut c = self.next_char();
        loop {
            match self.mode_scan(c) {
                Ok(ScanOutput::Start(mode_op)) => {
                    self.first = c;
                    self.last = c;
                    self.next_mode(mode_op);
                }
                Ok(ScanOutput::Next(mode_op)) => {
                    self.last = c;
                    self.next_mode(mode_op)
                }
                Ok(ScanOutput::Token(mut tok, consume, mode_op)) => {
                    let first = self.first.or(c).unwrap();
                    let last = if !consume {
                        self.unused_next = c;
                        self.last.or(c).unwrap()
                    } else {
                        self.unused_next = None;
                        c.unwrap()
                    };

                    self.first = None;
                    self.last = None;
                    self.next_mode(mode_op);
                    let src = &self.src[first.index..=last.index];

                    let keyword = KEYWORDS.get(src);
                    if keyword.is_some() {
                        tok = *keyword.unwrap();
                    }

                    return Some(Ok(TokenChars {
                        tok,
                        first,
                        last,
                        src,
                    }));
                }
                Ok(ScanOutput::EOF) => {
                    return None;
                }
                Err(fail) => {
                    self.mode = &ScanModeEOF;
                    return Some(Err(fail));
                }
            }
            c = self.chars.next();
        }
    }
}

#[cfg(test)]
mod tests
{
    use super::{Token, TokenResult, Tokenz};

    use std::iter::Iterator;


    fn tok<'a>(toks: &'a Vec<TokenResult>, i: usize) -> (Token, &'a str)
    {
        let t = toks[i].as_ref().unwrap();
        (t.tok, t.src)
    }

    fn nextok<'a, 'b, I>(it: &'a mut I) -> (Token, &'static str)
        where I: Iterator<Item=&'b TokenResult<'static>>
            , 'b: 'a
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
        assert_eq!(9, t.len());
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
        assert_eq!(7, t.len());
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
        assert_eq!(4, t.len());
    }

    #[test]
    fn test_tokenz_ids()
    {
        let input = "tacos burrit_s";

        let t: Vec<TokenResult<'static>> = Tokenz::lex(input).collect();
        let mut i = t.iter();
        assert_eq!(Token::LineBegin, nextok(&mut i).0);
        assert_eq!((Token::Id, "tacos"), nextok(&mut i));
        i.next();
        assert_eq!((Token::Id, "burrit_s"), nextok(&mut i));
        assert_eq!(3, t.len());
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
        assert_eq!(10, t.len());
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
        assert_eq!(18, t.len());
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
        assert_eq!(8, t.len());
    }

    #[test]
    fn test_tokenz_comparison_operators()
    {
        let input = "== < > <= >= \\n >>";

        let t: Vec<TokenResult<'static>> = Tokenz::lex(input).collect();
        assert_eq!(Token::Equal, tok(&t, 0).0);
        assert_eq!(Token::AngleL, tok(&t, 2).0);
        assert_eq!(Token::AngleR, tok(&t, 4).0);
        assert_eq!(Token::LessThanEqual, tok(&t, 6).0);
        assert_eq!(Token::GreaterThanEqual, tok(&t, 8).0);
        assert_eq!(Token::ConcatNewline, tok(&t, 10).0);
        assert_eq!(Token::DoubleArrow, tok(&t, 12).0);
        assert_eq!(13, t.len());
    }

    #[test]
    fn test_tokenz_separators()
    {
        let input = ": , . :: := | :";

        let t: Vec<TokenResult<'static>> = Tokenz::lex(input).collect();
        assert_eq!(Token::Colon, tok(&t, 0).0);
        assert_eq!(Token::Comma, tok(&t, 2).0);
        assert_eq!(Token::Dot, tok(&t, 4).0);
        assert_eq!(Token::DoubleColon, tok(&t, 6).0);
        assert_eq!(Token::Assignment, tok(&t, 8).0);
        assert_eq!(Token::Pipe, tok(&t, 10).0);
        assert_eq!(Token::Colon, tok(&t, 12).0);
        assert_eq!(13, t.len());
    }

    #[test]
    fn test_tokenz_strings()
    {
        let input = r#""tacos" "" "burritos""#;

        let t: Vec<TokenResult<'static>> = Tokenz::lex(input).collect();

        assert_eq!(Token::DoubleQuoteL, tok(&t, 0).0);
        assert_eq!((Token::StrLit, "tacos"), tok(&t, 1));
        assert_eq!(Token::DoubleQuoteR, tok(&t, 2).0);

        assert_eq!(Token::DoubleQuoteL, tok(&t, 4).0);
        assert_eq!(Token::DoubleQuoteR, tok(&t, 5).0);

        assert_eq!((Token::StrLit, "burritos"), tok(&t, 8));

        assert_eq!(10, t.len());
    }

    #[test]
    fn test_tokenize_func()
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

        assert_eq!(Token::Func, tok(&t, 2).0);
        assert_eq!((Token::Id, "tacos"), tok(&t, 4));
        assert_eq!(Token::Colon, tok(&t, 5).0);
        assert_eq!((Token::Id, "Int"), tok(&t, 7));

        assert_eq!(Token::Dot, tok(&t, 10).0);
        assert_eq!((Token::Id, "filling"), tok(&t, 11));
        assert_eq!(Token::Colon, tok(&t, 12).0);
        assert_eq!((Token::Id, "Str"), tok(&t, 14));

        assert_eq!(Token::Dot, tok(&t, 17).0);
        assert_eq!((Token::Id, "size"), tok(&t, 18));
        assert_eq!(Token::Colon, tok(&t, 19).0);
        assert_eq!((Token::Id, "Int"), tok(&t, 21));

        assert_eq!(Token::DoubleArrow, tok(&t, 24).0);
        assert_eq!((Token::Id, "make_tacos"), tok(&t, 27));
        assert_eq!(Token::ParenL, tok(&t, 28).0);
        assert_eq!((Token::Id, "meat"), tok(&t, 29));
        assert_eq!(Token::Comma, tok(&t, 30).0);
        assert_eq!((Token::Id, "size"), tok(&t, 32));
        assert_eq!(Token::ParenR, tok(&t, 33).0);
        assert_eq!(Token::DoubleDash, tok(&t, 36).0);

        assert_eq!(39, t.len());
    }

    #[test]
    fn test_tokenize_struct()
    {
        let input = "
        type Foo[T]
        .dog: T
        .cat: Str
        .mouse: Int
        --
        ";

        let t: Vec<TokenResult<'static>> = Tokenz::lex(input).collect();

        assert_eq!(Token::Type, tok(&t, 2).0);
        assert_eq!((Token::Id, "Foo"), tok(&t, 4));
        assert_eq!(Token::SquareL, tok(&t, 5).0);
        assert_eq!((Token::Id, "T"), tok(&t, 6));
        assert_eq!(Token::SquareR, tok(&t, 7).0);

        assert_eq!(Token::Dot, tok(&t, 10).0);
        assert_eq!((Token::Id, "dog"), tok(&t, 11));
        assert_eq!(Token::Colon, tok(&t, 12).0);
        assert_eq!((Token::Id, "T"), tok(&t, 14));

        assert_eq!(34, t.len());
    }
}
