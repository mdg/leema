use leema::failure::{Failure, Lresult};

use std::iter::Peekable;
use std::str::CharIndices;


#[derive(Copy)]
#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub struct Loc
{
    pub index: usize,
    pub lineno: u16,
    pub column: u8,
}

impl Loc
{
    fn nextcol(&mut self)
    {
        self.column += 1;
    }

    fn newline(&mut self)
    {
        self.lineno += 1;
        self.column = 1;
    }
}

impl Default for Loc
{
    fn default() -> Loc
    {
        Loc {
            index: 0,
            lineno: 1,
            column: 1,
        }
    }
}

#[derive(Clone)]
#[derive(Copy)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
pub enum TokenData<'input>
{
    Id(&'input str),
    Int(&'input str),
    Bool(bool),
    Hashtag(&'input str),
    Str(&'input str),
    DollarId(&'input str),

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
    GreaterThan,
    GreaterThanEqual,
    LessThan,
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
}

#[derive(Copy)]
#[derive(Clone)]
#[derive(Debug)]
pub struct Token<'input>(TokenData<'input>, Loc);

/// scan((start, line, col, (i, char))
/// return (consume_char, Option<new_token>, Option<push_scanner(scanner) | pop_state>) | error

trait ScanModeTrait
{
    fn scan(&self, Option<Loc>, Option<Loc>, &str) -> ScanResult;
}

type ScanMode = &'static ScanModeTrait;

enum ScanModeOp
{
    Push(&'static Box<ScanModeTrait>),
    Pop,
}

enum ScanOutput<'input>
{
    StartToken(Loc, ScanMode),
    Next,
    Complete(Token<'input>, bool),
}

type ScanResult<'input> = Lresult<ScanOutput<'input>>;

struct ScanState<'input>
{
    mode: Vec<ScanMode>,
    src: &'input str,
    first: Option<Loc>,
    next: Option<Loc>,
    failure: Option<Failure>,
}

impl<'input> ScanState<'input>
{
    pub fn new(src: &'input str) -> ScanState<'input>
    {
        ScanState {
            mode: vec![SCAN_ROOT],
            src,
            first: None,
            next: None,
            failure: None,
        }
    }
}

    /*
    Root,
    StrLit,
    Comment,
    BlockComment,
    */

struct ScanModeRoot;
struct ScanModeInt;

impl ScanModeTrait for ScanModeRoot
{
    fn scan(
        &self,
        _start: Option<Loc>,
        _next: Option<Loc>,
        _src: &str,
    ) -> ScanResult
    {
        Ok(ScanOutput::Next)
    }
}

impl ScanModeTrait for ScanModeInt
{
    fn scan(
        &self,
        _start: Option<Loc>,
        _next: Option<Loc>,
        _src: &str,
    ) -> ScanResult
    {
        Ok(ScanOutput::Next)
    }
}

fn scan_str<'input>(
    _start: Option<Loc>,
    _next: Option<Loc>,
    _src: &'input str,
) -> ScanResult<'input>
{
    Ok(ScanOutput::Next)
}

const SCAN_ROOT: ScanMode = &ScanModeRoot;
const SCAN_INT: ScanMode = &ScanModeInt;

pub struct Tokenz<'i>
{
    input: &'i str,
    code: Peekable<CharIndices<'i>>,
    tokens: Vec<Token<'i>>,
    paren_stack: Vec<u16>,
    chindex: usize,
    lineno: u16,
    column: u8,
    continued_line: bool,
}

impl<'i> Tokenz<'i>
{
    pub fn new(input: &'i str) -> Tokenz<'i>
    {
        Tokenz {
            input,
            code: input.char_indices().peekable(),
            tokens: Vec::new(),
            paren_stack: Vec::with_capacity(16),
            chindex: 0,
            lineno: 1,
            column: 0,
            continued_line: false,
        }
    }

    pub fn lex(input: &'i str) -> Vec<Token<'i>>
    {
        let mut tokz = Tokenz::new(input);
        tokz.lex_all();
        // check state of things like paren stack
        tokz.tokens
    }

    pub fn lex_all(&mut self)
    {
        loop {
            let ch = self.next();
            if ch == '\0' {
                break;
            }
            self.lex_one_token(ch);
        }
    }

    pub fn lex_one_token(&mut self, _first: char)
    {
        /*
        match first {
            // brackets
            '(' => {
                self.paren_stack.push(self.lineno);
                self.tokens.push(Token::ParenL);
            }
            ')' => {
                if self.paren_stack.is_empty() {
                    panic!("parentheses underflow");
                }
                self.paren_stack.pop();
                self.tokens.push(Token::ParenR);
            }
            '{' => {
                self.tokens.push(Token::CurlyL);
            }
            '}' => {
                self.tokens.push(Token::CurlyR);
            }
            '[' => {
                self.tokens.push(Token::SquareL);
            }
            ']' => {
                self.tokens.push(Token::SquareR);
            }
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

    pub fn next(&mut self) -> char
    {
        loop {
            let first = self.code.next();
            if first.is_none() {
                return '\0';
            }
            let (loc, character) = first.unwrap();
            self.column += 1;
            self.chindex = loc;
            // skip if horizontal space
            if character == '\n' || !character.is_whitespace() {
                return character;
            }
            // else is horizontal whitespace so continue
        }
    }
}


#[cfg(test)]
mod tests
{
    use leema::token::{Token, Tokenz};


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
        "
        .to_string();

        let toks = Tokenz::lex(&input);
        assert_eq!(Token::Func, toks[0]);
        assert_eq!(Token::Colon, toks[2]);
        assert_eq!(Token::Dot, toks[4]);
        assert_eq!(Token::Id("filling"), toks[5]);
        assert_eq!(Token::Colon, toks[6]);
        assert_eq!(Token::Id("make_tacos"), toks[13]);
        assert_eq!(Token::DoubleDash, toks[19]);
        assert_eq!(20, toks.len());
    }

    #[test]
    fn test_tokenize_struct()
    {
        let input = "
        struct Foo[T]
        .dog: T
        .cat: Str
        .mouse: Int
        --
        "
        .to_string();

        let toks = Tokenz::lex(&input);
        assert_eq!(Token::Struct, toks[0]);
        assert_eq!(Token::Id("Foo"), toks[1]);
        assert_eq!(Token::SquareL, toks[2]);
        assert_eq!(Token::Id("T"), toks[3]);
        assert_eq!(Token::SquareR, toks[4]);
        assert_eq!(18, toks.len());
    }

    #[test]
    #[should_panic]
    fn test_triple_dash()
    {
        let input = "func foo() >> 5 ---".to_string();
        Tokenz::lex(&input);
    }
}
