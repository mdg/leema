use leema::failure::Lresult;

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
        CharIter{
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
                let result = Char{
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
                let result = Char{
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
    Str,
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
    Void,

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
    Newline,
    Spaces,
    Tabs,

    // EOF
    EOF,
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
struct ScanModeRoot;

#[derive(Debug)]
struct ScanModeEOF;

#[derive(Debug)]
struct ScanModeFailure;

#[derive(Debug)]
struct ScanModeDash;

#[derive(Debug)]
struct ScanModeInt;

#[derive(Debug)]
struct ScanModeSpace;

impl ScanModeTrait for ScanModeRoot
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
            '<' => ScanOutput::Token(Token::AngleL, true, ScanModeOp::Noop),
            '>' => ScanOutput::Token(Token::AngleR, true, ScanModeOp::Noop),
            // whitespace
            '\n' => ScanOutput::Token(Token::Newline, true, ScanModeOp::Noop),
            ' ' => ScanOutput::Start(ScanModeOp::Push(&ScanModeSpace)),
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
        Err(rustfail!(
            "token_failure",
            "token failure at {:?}",
            next,
        ))
    }
}

impl ScanModeTrait for ScanModeSpace
{
    fn scan(&self, next: Char) -> ScanResult
    {
        match next.c {
            ' ' => {
                eprintln!("more space");
                Ok(ScanOutput::Next(ScanModeOp::Noop))
            }
            '\t' => {
                Err(rustfail!(
                    "mixed_spaces_tabs",
                    "Do not use spaces when already using tabs {},{}",
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
}

impl ScanModeTrait for ScanModeInt
{
    fn scan(&self, _next: Char) -> ScanResult
    {
        Ok(ScanOutput::Next(ScanModeOp::Noop))
    }
}

/*
fn scan_str<'input>(
    _start: Option<Char>,
    _next: Option<Char>,
    _src: &'input str,
) -> ScanResult
{
    Ok(ScanOutput::Next)
}
*/

const SCAN_ROOT: ScanMode = &ScanModeRoot;
const SCAN_INT: ScanMode = &ScanModeInt;

struct Tokenz<'input>
{
    src: &'input str,
    chars: CharIter<'input>,

    mode: ScanMode,
    mode_stack: Vec<ScanMode>,
    // paren_stack: Vec<u16>,

    first: Option<Char>,
    last: Option<Char>,
    next: Option<Char>,
}

impl<'input> Tokenz<'input>
{
    pub fn lex(src: &'input str) -> Tokenz<'input>
    {
        let chars = CharIter::new(src);
        Tokenz{
            src,
            chars,

            mode: SCAN_ROOT,
            mode_stack: vec![],

            first: None,
            last: None,
            next: None,
        }
    }

    fn next_char(&mut self) -> Option<Char>
    {
        self.next.or_else(|| {
            self.chars.next()
        })
    }

    fn scan(&mut self, opt_c: Option<Char>) -> ScanResult
    {
        match opt_c {
            Some(c) => {
                self.mode.scan(c)
            }
            None => {
                // probably should get the last token here or something
                Ok(ScanOutput::EOF)
            }
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
        let mut c = self.next.or_else(|| {
            self.chars.next()
        });
        loop {
            match self.scan(c) {
                Ok(ScanOutput::Start(mode_op)) => {
                    self.first = c;
                    self.last = c;
                    self.next_mode(mode_op);
                }
                Ok(ScanOutput::Next(mode_op)) => {
                    self.last = c;
                    c = self.chars.next();
                    self.next_mode(mode_op)
                }
                Ok(ScanOutput::Token(tok, consume, mode_op)) => {
                    let first = self.first.or(c).unwrap();
                    let last = if !consume {
                        self.next = c;
                        self.last.unwrap()
                    } else {
                        c.unwrap()
                    };

                    self.first = None;
                    self.last = None;
                    self.next_mode(mode_op);

                    return Some(Ok(TokenChars{
                        tok,
                        first,
                        last,
                        src: &self.src[first.index ..= last.index],
                    }));
                }
                Ok(ScanOutput::EOF) => {
                    return None;
                }
                Err(fail) => {
                    self.mode = &ScanModeEOF;
                    return Some(Err(fail))
                }
            }
        }
    }
}

impl<'i> Tokenz<'i>
{
    pub fn lex_one_token(&mut self, _first: char)
    {
        /*
        match first {
            // separators
            ':' => {
                self.lex_colon();
            }
            '.' => {
                self.tokens.push(Token::Dot);
            }
            ',' => {
                self.tokens.push(Token::Comma);
            }
            '\n' => {
                self.increment_lineno();
            }
            '\\' => {
                self.lex_backslash();
            }
            // arithmetic operators
            '+' => {
                self.tokens.push(Token::Plus);
            }
            '-' => {
                self.lex_dash();
            }
            '*' => {
                self.tokens.push(Token::Star);
            }
            '/' => {
                self.tokens.push(Token::Slash);
            }
            // comparison operators
            '<' => {
                self.lex_peek('=', Token::LessThanEqual, Token::LessThan);
            }
            '=' => {
                self.lex_eq();
            }
            '>' => {
                self.lex_gt();
            }
            '_' => self.lex_id(),
            // data tokens
            _ => {
                println!("data token: ({})", first);
                if first.is_alphabetic() {
                    self.lex_id();
                } else if first.is_numeric() {
                    self.lex_int();
                } else {
                    panic!("dunno what kind of token: '{}'", first);
                }
            }
        }
        */
    }

    pub fn lex_backslash(&mut self)
    {
        /*
        match self.next() {
            '\n' => {
                self.increment_lineno();
                self.continued_line = true;
            }
            c => {
                panic!("unknown token: \\{}", c);
            }
        }
        */
    }

    pub fn lex_colon(&mut self)
    {
        /*
        match self.peek() {
            ':' => {
                self.next();
                self.tokens.push(Token::DoubleColon);
            }
            _ => {
                self.tokens.push(Token::Colon);
            }
        }
        */
    }

    pub fn lex_dash(&mut self)
    {
        /*
        match self.peek() {
            '-' => {
                self.next();
                if self.peek() == '-' {
                    panic!("--- is not a valid token");
                }
                self.tokens.push(Token::DoubleDash);
            }
            _ => {
                self.tokens.push(Token::Dash);
            }
        }
        */
    }

    pub fn lex_eq(&mut self)
    {
        /*
        match self.next() {
            '=' => {
                self.tokens.push(Token::Equal);
            }
            '>' => {
                println!("what is => used for now?");
            }
            _ => {
                panic!("= is an invalid token. use := or ==");
            }
        }
        */
    }

    pub fn lex_gt(&mut self)
    {
        /*
        match self.peek() {
            '>' => {
                self.next();
                self.tokens.push(Token::DoubleArrow);
            }
            '=' => {
                self.next();
                self.tokens.push(Token::GreaterThanEqual);
            }
            _ => {
                self.tokens.push(Token::GreaterThan);
            }
        }
        */
    }

    /*
    pub fn lex_int(&mut self)
    {
        let end = self
            .code
            .clone()
            .take_while(|ch| ch.1.is_numeric())
            .last()
            .map(|l| l.0)
            .unwrap_or(self.chindex + 1);
        let int_str = &self.input[self.chindex..end];
        let int_val: i64 = int_str.parse().expect("could not parse lexed int");
        self.tokens.push(Token::Int(int_val));
    }

    pub fn lex_id(&mut self)
    {
        let start = &self.input[self.chindex..];
        println!("look for id at: {}", start);
        let len = start
            .chars()
            .take_while(|ch| ch.is_alphanumeric() || *ch == '_')
            .count();
        let end = self.chindex + len;
        {
            let mut dec = len;
            while dec > 1 {
                self.code.next();
                dec -= 1;
            }
        }
        let id_str = &self.input[self.chindex..end];
        let keyword = Tokenz::lex_keyword(id_str);
        if keyword.is_some() {
            self.tokens.push(keyword.unwrap());
        } else {
            let id_token = Token::Id(id_str);
            println!("found token: {:?}", id_token);
            self.tokens.push(id_token);
        }
    }

    pub fn lex_keyword(id: &str) -> Option<Token>
    {
        let tok = match id {
            "enum" => Token::Enum,
            "failed" => Token::Failed,
            "false" => Token::Bool(false),
            "fork" => Token::Fork,
            "func" => Token::Func,
            "if" => Token::If,
            "import" => Token::Import,
            "let" => Token::Let,
            "macro" => Token::Macro,
            "match" => Token::Match,
            "return" => Token::Return,
            "struct" => Token::Struct,
            "true" => Token::Bool(true),
            "void" => Token::Void,
            _ => {
                return None;
            }
        };
        Some(tok)
    }
    */
}


#[cfg(test)]
mod tests
{
    use super::{Token, Tokenz, TokenResult};


    fn tok<'a>(toks: &'a Vec<TokenResult>, i: usize) -> (Token, &'a str)
    {
        let t = toks[i].as_ref().unwrap();
        (t.tok, t.src)
    }

    #[test]
    fn test_tokenize_brackets()
    {
        let input = "(){}[]<>";

        let t: Vec<TokenResult<'static>> = Tokenz::lex(input).collect();
        assert_eq!(Token::ParenL, tok(&t, 0).0);
        assert_eq!(Token::ParenR, tok(&t, 1).0);
        assert_eq!(Token::CurlyL, tok(&t, 2).0);
        assert_eq!(Token::CurlyR, tok(&t, 3).0);
        assert_eq!(Token::SquareL, tok(&t, 4).0);
        assert_eq!(Token::SquareR, tok(&t, 5).0);
        assert_eq!(Token::AngleL, tok(&t, 6).0);
        assert_eq!(Token::AngleR, tok(&t, 7).0);
        assert_eq!(8, t.len());
    }

    /*
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
        assert_eq!(Token::Func, tok(&t, 0).0);
        assert_eq!(Token::Colon, tok(&t, 2).0);
        assert_eq!(Token::Dot, tok(&t, 4).0);
        assert_eq!((Token::Id, "filling"), tok(&t, 5));
        assert_eq!(Token::Colon, tok(&t, 6).0);
        assert_eq!((Token::Id, "make_tacos"), tok(&t, 13));
        assert_eq!(Token::DoubleDash, tok(&t, 19).0);
        assert_eq!(20, t.len());
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

        // let (tok, toklen) = test_lex(input);
        let toks: Vec<TokenResult> = Tokenz::lex(input).collect();
        let tok = tok_src(&toks);
        assert_eq!(Token::Struct, tok(0));
        assert_eq!((Token::Id, "Foo"), tok(1));
        assert_eq!(Token::SquareL, tok(2));
        assert_eq!((Token::Id, "T"), tok(3));
        assert_eq!(Token::SquareR, tok(4));
        assert_eq!(18, toks.len());
    }

    #[test]
    #[should_panic]
    fn test_triple_dash()
    {
        let input = "func foo() >> 5 ---";
        Tokenz::lex(input);
    }
    */
}
