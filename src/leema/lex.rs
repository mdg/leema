use leema::ast::TokenData;
use leema::parse::{self, Token};
use leema::val::SrcLoc;
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
        let mut valvec = Vec::with_capacity(self.len + 1);
        unsafe {
            valvec.set_len(self.len);
            ptr::copy(self.value, valvec.as_mut_ptr(), self.len + 1);
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
extern "C" {
    fn lib_lexscan(input: *const u8) -> *mut LexState;
    fn lib_lexone(scanner: *mut LexState) -> *const LibTokenBuffer;
    fn lib_lexclose(scanner: *mut LexState);
}


impl Token
{
    fn from_lib(tok: *const LibTokenBuffer) -> Token
    {
        unsafe {
            let tl =
                SrcLoc::new((*tok).lineno as i16, (*tok).token_column as i8);
            match (*tok).tok {
                parse::TOKEN_BLOCKARROW => Token::BLOCKARROW(tl),
                parse::TOKEN_DOUBLEDASH => Token::DOUBLEDASH(tl),
                parse::TOKEN_TYPEARROW => Token::TYPEARROW(tl),
                parse::TOKEN_Func => Token::Func(tl),
                parse::TOKEN_MACRO => Token::MACRO(tl),
                parse::TOKEN_STRUCT => Token::STRUCT(tl),
                parse::TOKEN_INT => Token::INT((*tok).ival()),
                parse::TOKEN_ID => Token::ID(TokenData::new((*tok).val(), tl)),
                parse::TOKEN_HASHTAG => {
                    let mut txt = (*tok).val();
                    let hash = txt.remove(0);
                    if hash != '#' {
                        panic!("hashtag didn't start with #? {:?}", hash);
                    }
                    Token::HASHTAG(TokenData::new(txt, tl))
                }
                parse::TOKEN_ConcatNewline => Token::ConcatNewline(tl),
                parse::TOKEN_IMPORT => Token::IMPORT(tl),
                parse::TOKEN_LPAREN => Token::LPAREN(tl),
                parse::TOKEN_PARENCALL => Token::PARENCALL(tl),
                parse::TOKEN_COLON => Token::COLON(tl),
                parse::TOKEN_DBLCOLON => Token::DBLCOLON(tl),
                parse::TOKEN_COMMA => Token::COMMA(tl),
                parse::TOKEN_DOT => Token::DOT,
                parse::TOKEN_ENUM => Token::ENUM(tl),
                parse::TOKEN_RPAREN => Token::RPAREN(tl),
                parse::TOKEN_SEMICOLON => Token::SEMICOLON(tl),
                parse::TOKEN_SquareL => Token::SquareL(tl),
                parse::TOKEN_SquareCall => Token::SquareCall(tl),
                parse::TOKEN_SquareR => Token::SquareR(tl),
                parse::TOKEN_STRUPLE => Token::STRUPLE(tl),
                parse::TOKEN_TIMES => Token::TIMES(tl),
                parse::TOKEN_SLASH => Token::SLASH(tl),
                parse::TOKEN_PIPE => Token::PIPE(tl),
                parse::TOKEN_ASSIGN => Token::ASSIGN,
                parse::TOKEN_Fork => Token::Fork(tl),
                parse::TOKEN_Let => Token::Let(tl),
                /*
                parse::TOKEN_CurlyL => {
                    Token::CurlyL
                }
                parse::TOKEN_CurlyR => {
                    Token::CurlyR
                }
                */
                /* string tokens */
                parse::TOKEN_StrOpen => Token::StrOpen(tl),
                parse::TOKEN_StrLit => Token::StrLit((*tok).val()),
                parse::TOKEN_StrClose => Token::StrClose,
                parse::TOKEN_IF => Token::IF(tl),
                parse::TOKEN_MATCH => Token::MATCH(tl),
                parse::TOKEN_UNDERSCORE => Token::UNDERSCORE,
                parse::TOKEN_ELSE => Token::ELSE(tl),
                parse::TOKEN_FAILED => Token::FAILED(tl),
                parse::TOKEN_PLUS => Token::PLUS(tl),
                parse::TOKEN_MINUS => Token::MINUS(tl),
                parse::TOKEN_NEGATE => Token::NEGATE(tl),
                parse::TOKEN_MOD => Token::MOD(tl),
                parse::TOKEN_False => Token::False,
                parse::TOKEN_True => Token::True,
                parse::TOKEN_AND => Token::AND(tl),
                parse::TOKEN_OR => Token::OR(tl),
                parse::TOKEN_XOR => Token::XOR(tl),
                parse::TOKEN_NOT => Token::NOT(tl),
                parse::TOKEN_LT => Token::LT(tl),
                parse::TOKEN_LTEQ => Token::LTEQ(tl),
                parse::TOKEN_EQ => Token::EQ(tl),
                parse::TOKEN_NEQ => Token::NEQ(tl),
                parse::TOKEN_GT => Token::GT(tl),
                parse::TOKEN_GTEQ => Token::GTEQ(tl),
                parse::TOKEN_EQ1 => Token::EQ1(tl),
                parse::TOKEN_RETURN => Token::RETURN(tl),
                parse::TOKEN_RUSTBLOCK => Token::RUSTBLOCK,
                parse::TOKEN_TYPE_BOOL => Token::TYPE_BOOL,
                parse::TOKEN_TYPE_FAILURE => Token::TYPE_FAILURE,
                parse::TOKEN_TYPE_INT => Token::TYPE_INT,
                parse::TOKEN_TYPE_STR => Token::TYPE_STR,
                parse::TOKEN_TYPE_HASHTAG => Token::TYPE_HASHTAG,
                parse::TOKEN_TYPE_VOID => Token::TYPE_VOID,
                parse::TOKEN_TYPE_VAR => {
                    Token::TYPE_VAR(TokenData::new((*tok).val(), tl))
                }
                parse::TOKEN_EOI => Token::EOI,
                parse::TOKEN_ANY => Token::ANY,
                parse::TOKEN_DollarQuestion => Token::DollarQuestion,
                _ => {
                    panic!("Unrecognized token: {:?}", (*tok));
                }
            }
        }
    }
}


pub fn lex(str_input: &str) -> Vec<Token>
{
    // need to append \0 so C library knows where the string stops
    let input = format!("{}\0", str_input);
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
    use leema::ast::TokenData;
    use leema::parse::Token;
    use leema::val::SrcLoc;

    #[test]
    fn test_lex_int()
    {
        let actual = super::lex("5");
        assert_eq!(1, actual.len());
        assert_eq!(Token::INT(5), actual[0]);
    }

    #[test]
    fn test_lex_minus_int()
    {
        let actual = super::lex("-7");
        assert_eq!(2, actual.len());
        assert_eq!(Token::MINUS(SrcLoc::new(1, 1)), actual[0]);
        assert_eq!(Token::INT(7), actual[1]);
    }

    #[test]
    fn test_lex_string_id()
    {
        let actual = super::lex("\"hello $who\n\"");
        assert_eq!(5, actual.len());

        assert_eq!(Token::StrOpen(SrcLoc::new(1, 1)), actual[0]);
        assert_eq!(Token::StrLit("hello ".to_string()), actual[1]);
        assert_eq!(
            Token::ID(TokenData::new("who".to_string(), SrcLoc::new(1, 9))),
            actual[2]
        );
        assert_eq!(Token::StrLit("\n".to_string()), actual[3]);
        assert_eq!(Token::StrClose, actual[4]);
    }

    #[test]
    fn test_lex_enum_variants()
    {
        let actual = super::lex(
            "
        enum Animal
        |Dog
        |Cat Int
        |Mouse $A
        |Giraffe
            .height: Int
            .weight: $A
        --
    ",
        );
        assert_eq!(21, actual.len());
    }

}
