use leema::ast::{TokenLoc, TokenData};
use parse::{self, Token};
use std::ptr;

#[repr(C)]
#[derive(Debug)]
struct LibTokenBuffer
{
    tok: i32,
    value: *const u8,
    len: usize,

    lineno: i32,
    token_column: i32,
    next_column: i32, // ignored here
    block_comment_depth: i32,
}

impl LibTokenBuffer
{
    fn val(&self) -> String
    {
        let mut valvec = Vec::with_capacity(self.len+1);
        unsafe {
            valvec.set_len(self.len);
            ptr::copy(self.value, valvec.as_mut_ptr(), self.len+1);
        }
        String::from_utf8(valvec).unwrap()
    }

    fn ival(&self) -> i64
    {
        let strval = self.val();
        let iresult = i64::from_str_radix(&strval, 10);
        iresult.unwrap()
    }
}

enum LexState {}

#[link(name = "leemalex")]
#[link(name = "stdc++")]
extern "C"
{
    fn lib_lexscan(input: *const u8) -> *mut LexState;
    fn lib_lexone(scanner: *mut LexState) -> *const LibTokenBuffer;
    fn lib_lexclose(scanner: *mut LexState);
}


impl Token
{
    fn from_lib(tok: *const LibTokenBuffer) -> Token
    {
        unsafe {
            let tl = TokenLoc::new((*tok).lineno, (*tok).token_column as i16);
            match (*tok).tok {
                parse::TOKEN_BLOCKARROW => Token::BLOCKARROW,
                parse::TOKEN_DOUBLEDASH => Token::DOUBLEDASH(tl),
                parse::TOKEN_Func => {
                    Token::Func
                }
                parse::TOKEN_MACRO => {
                    Token::MACRO
                }
                parse::TOKEN_STRUCT => {
                    Token::STRUCT
                }
                parse::TOKEN_INT => {
                    Token::INT((*tok).ival())
                }
                parse::TOKEN_ID => {
                    Token::ID(TokenData::new((*tok).val(), tl))
                }
                parse::TOKEN_CALL_ID => {
                    let mut txt = (*tok).val();
                    let txtlen = txt.len();
                    txt.truncate(txtlen - 1);
                    Token::CALL_ID(TokenData::new(txt, tl))
                }
                parse::TOKEN_CALL_TYPE_ID => {
                    let mut txt = (*tok).val();
                    let txtlen = txt.len();
                    txt.truncate(txtlen - 1);
                    Token::CALL_TYPE_ID(TokenData::new(txt, tl))
                }
                parse::TOKEN_HASHTAG => {
                    let mut txt = (*tok).val();
                    let hash = txt.remove(0);
                    if hash != '#' {
                        panic!("hashtag didn't start with #? {:?}", hash);
                    }
                    Token::HASHTAG(TokenData::new(txt, tl))
                }
                parse::TOKEN_ConcatNewline => {
                    Token::ConcatNewline
                }
                parse::TOKEN_LPAREN => {
                    Token::LPAREN
                }
                parse::TOKEN_COLON => {
                    Token::COLON
                }
                parse::TOKEN_COMMA => {
                    Token::COMMA(tl)
                }
                parse::TOKEN_DOT => {
                    Token::DOT
                }
                parse::TOKEN_RPAREN => {
                    Token::RPAREN
                }
                parse::TOKEN_SquareL => {
                    Token::SquareL
                }
                parse::TOKEN_SquareR => {
                    Token::SquareR
                }
                parse::TOKEN_TIMES => Token::TIMES,
                parse::TOKEN_SLASH => Token::SLASH(tl),
                parse::TOKEN_PIPE => Token::PIPE,
                parse::TOKEN_ASSIGN => Token::ASSIGN,
                parse::TOKEN_Fork => Token::Fork,
                parse::TOKEN_Let => Token::Let,
                /*
                parse::TOKEN_CurlyL => {
                    Token::CurlyL
                }
                parse::TOKEN_CurlyR => {
                    Token::CurlyR
                }
                */
                /* string tokens */
                parse::TOKEN_StrOpen => {
                    Token::StrOpen
                }
                parse::TOKEN_StrLit => {
                    Token::StrLit((*tok).val())
                }
                parse::TOKEN_StrClose => {
                    Token::StrClose
                }
                parse::TOKEN_CASE => {
                    Token::CASE
                }
                parse::TOKEN_IF => {
                    Token::IF
                }
                parse::TOKEN_MATCH => Token::MATCH,
                parse::TOKEN_UNDERSCORE => Token::UNDERSCORE,
                parse::TOKEN_ELSE => {
                    Token::ELSE(tl)
                }
                parse::TOKEN_FAIL => {
                    Token::FAIL
                }
                parse::TOKEN_FAILED => {
                    Token::FAILED
                }
                parse::TOKEN_PLUS => {
                    Token::PLUS(tl)
                }
                parse::TOKEN_MINUS => {
                    Token::MINUS
                }
                parse::TOKEN_NEGATE => {
                    Token::NEGATE
                }
                parse::TOKEN_MOD => {
                    Token::MOD
                }
                parse::TOKEN_False => {
                    Token::False
                }
                parse::TOKEN_True => {
                    Token::True
                }
                parse::TOKEN_AND => {
                    Token::AND
                }
                parse::TOKEN_OR => {
                    Token::OR
                }
                parse::TOKEN_XOR => {
                    Token::XOR
                }
                parse::TOKEN_NOT => {
                    Token::NOT
                }
                parse::TOKEN_LT => {
                    Token::LT
                }
                parse::TOKEN_LTEQ => {
                    Token::LTEQ
                }
                parse::TOKEN_EQ => {
                    Token::EQ
                }
                parse::TOKEN_NEQ => {
                    Token::NEQ
                }
                parse::TOKEN_GT => {
                    Token::GT
                }
                parse::TOKEN_GTEQ => {
                    Token::GTEQ
                }
                parse::TOKEN_RETURN => {
                    Token::RETURN
                }
                parse::TOKEN_TYPE_ID => {
                    Token::TYPE_ID(TokenData::new((*tok).val(), tl))
                }
                parse::TOKEN_TYPE_BOOL => {
                    Token::TYPE_BOOL
                }
                parse::TOKEN_TYPE_INT => {
                    Token::TYPE_INT
                }
                parse::TOKEN_TYPE_STR => {
                    Token::TYPE_STR
                }
                parse::TOKEN_TYPE_VOID => {
                    Token::TYPE_VOID
                }
                parse::TOKEN_EOI => {
                    Token::EOI
                }
                parse::TOKEN_ANY => {
                    Token::ANY
                }
                parse::TOKEN_DollarQuestion => {
                    Token::DollarQuestion
                }
                _ => {
                    panic!("Unrecognized token: {:?}", (*tok));
                }
            }
        }
    }
}


pub fn lex(mut input: String) -> Vec<Token>
{
    // need to append \0 so C library knows where the string stops
    input.push_str("\0");
    let lexer;
    unsafe {
        lexer = lib_lexscan(input.as_ptr());
    }

    let mut toks = vec![];
    let mut i = 0;
    loop {
        let tok;
        unsafe {
            let libtok = lib_lexone(lexer);
            tok = Token::from_lib(libtok);
        }
        if tok == Token::EOI {
            break;
        }
        toks.push(tok);
        i = i + 1;
    }
    unsafe {
        lib_lexclose(lexer);
    }
    toks
}


#[cfg(test)]
mod tests
{
    use parse::Token;
    use leema::ast::{TokenData, TokenLoc};

#[test]
fn test_lex_int()
{
    let actual = super::lex("5".to_string());
    assert_eq!(1, actual.len());
    assert_eq!(Token::INT(5), actual[0]);
}

#[test]
fn test_lex_minus_int()
{
    let actual = super::lex("-7".to_string());
    assert_eq!(2, actual.len());
    assert_eq!(Token::MINUS, actual[0]);
    assert_eq!(Token::INT(7), actual[1]);
}

#[test]
fn test_lex_string_id()
{
    let actual = super::lex("\"hello $who\n\"".to_string());
    assert_eq!(5, actual.len());

    assert_eq!(Token::StrOpen, actual[0]);
    assert_eq!(Token::StrLit("hello ".to_string()), actual[1]);
    assert_eq!(
        Token::ID(TokenData::new("who".to_string(), TokenLoc::new(1, 9))),
        actual[2]
    );
    assert_eq!(Token::StrLit("\n".to_string()), actual[3]);
    assert_eq!(Token::StrClose, actual[4]);
}

}
