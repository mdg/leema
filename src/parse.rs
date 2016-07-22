#![allow(dead_code)]
#![allow(unused_variables)]
/* TMPL: %include */

use leema::ast::{Ast, TokenLoc, TokenData};
use leema::val::{Val, SexprType, Type};
use leema::list;
use leema::log;
use leema::sexpr;
use std::sync::Arc;
use std::io::{stderr, Write};
/* TMPL: makeheader cruft */


/* TMPL: types */

type YYCODETYPE = i8;
const YYNOCODE: i32 = 92;
type YYACTIONTYPE = u8;
const YYWILDCARD: YYCODETYPE = 1;
enum YYMinorType {
    YY0,
    YY29(Type),
    YY32(Val),
    YY63(Ast),
    YY66(String),
    YY108(i64),
    YY158(TokenLoc),
    YY173(TokenData<String>),
}
const YYNSTATE: i32 = 154;
const YYNRULE: i32 = 85;
const YYERRORSYMBOL: i32 = 58;

//const YY_NO_ACTION: i32 = YYNSTATE+YYNRULE+2;
//const YY_ACCEPT_ACTION: i32 = YYNSTATE+YYNRULE+1;
//const YY_ERROR_ACTION: i32 = YYNSTATE+YYNRULE+1;

/* TMPL: action tables */

#[derive(Debug,PartialEq
)]
pub enum Token {
    EOI, //0
    ANY( TokenLoc ), //1
    COMMA( TokenLoc ), //2
    HASHTAG( TokenData<String> ), //3
    ID( String ), //4
    INT( i64 ), //5
    NEWLINE( TokenLoc ), //6
    PLUS( TokenLoc ), //7
    StrLit( String ), //8
    TYPE_ID( String ), //9
    ASSIGN, //10
    BLOCKARROW, //11
    WHERE, //12
    GIVEN, //13
    OR, //14
    XOR, //15
    AND, //16
    ConcatNewline, //17
    NOT, //18
    LT, //19
    LTEQ, //20
    EQ, //21
    NEQ, //22
    GT, //23
    GTEQ, //24
    MINUS, //25
    TIMES, //26
    DIVIDE, //27
    LPAREN, //28
    RPAREN, //29
    FAILED, //30
    WITH, //31
    CONTEXTID, //32
    DT, //33
    FAIL, //34
    Let, //35
    Fork, //36
    DollarGT, //37
    CurlyL, //38
    CurlyR, //39
    Func, //40
    COLON, //41
    TYPE_INT, //42
    TYPE_STR, //43
    TYPE_BOOL, //44
    MACRO, //45
    DOLLAR, //46
    IF, //47
    ELSE, //48
    MOD, //49
    VOID, //50
    DollarQuestion, //51
    True, //52
    False, //53
    SquareL, //54
    SquareR, //55
    StrOpen, //56
    StrClose, //57
}
pub const TOKEN_EOI: i32 = 0;
pub const TOKEN_ANY: i32 = 1;
pub const TOKEN_COMMA: i32 = 2;
pub const TOKEN_HASHTAG: i32 = 3;
pub const TOKEN_ID: i32 = 4;
pub const TOKEN_INT: i32 = 5;
pub const TOKEN_NEWLINE: i32 = 6;
pub const TOKEN_PLUS: i32 = 7;
pub const TOKEN_StrLit: i32 = 8;
pub const TOKEN_TYPE_ID: i32 = 9;
pub const TOKEN_ASSIGN: i32 = 10;
pub const TOKEN_BLOCKARROW: i32 = 11;
pub const TOKEN_WHERE: i32 = 12;
pub const TOKEN_GIVEN: i32 = 13;
pub const TOKEN_OR: i32 = 14;
pub const TOKEN_XOR: i32 = 15;
pub const TOKEN_AND: i32 = 16;
pub const TOKEN_ConcatNewline: i32 = 17;
pub const TOKEN_NOT: i32 = 18;
pub const TOKEN_LT: i32 = 19;
pub const TOKEN_LTEQ: i32 = 20;
pub const TOKEN_EQ: i32 = 21;
pub const TOKEN_NEQ: i32 = 22;
pub const TOKEN_GT: i32 = 23;
pub const TOKEN_GTEQ: i32 = 24;
pub const TOKEN_MINUS: i32 = 25;
pub const TOKEN_TIMES: i32 = 26;
pub const TOKEN_DIVIDE: i32 = 27;
pub const TOKEN_LPAREN: i32 = 28;
pub const TOKEN_RPAREN: i32 = 29;
pub const TOKEN_FAILED: i32 = 30;
pub const TOKEN_WITH: i32 = 31;
pub const TOKEN_CONTEXTID: i32 = 32;
pub const TOKEN_DT: i32 = 33;
pub const TOKEN_FAIL: i32 = 34;
pub const TOKEN_Let: i32 = 35;
pub const TOKEN_Fork: i32 = 36;
pub const TOKEN_DollarGT: i32 = 37;
pub const TOKEN_CurlyL: i32 = 38;
pub const TOKEN_CurlyR: i32 = 39;
pub const TOKEN_Func: i32 = 40;
pub const TOKEN_COLON: i32 = 41;
pub const TOKEN_TYPE_INT: i32 = 42;
pub const TOKEN_TYPE_STR: i32 = 43;
pub const TOKEN_TYPE_BOOL: i32 = 44;
pub const TOKEN_MACRO: i32 = 45;
pub const TOKEN_DOLLAR: i32 = 46;
pub const TOKEN_IF: i32 = 47;
pub const TOKEN_ELSE: i32 = 48;
pub const TOKEN_MOD: i32 = 49;
pub const TOKEN_VOID: i32 = 50;
pub const TOKEN_DollarQuestion: i32 = 51;
pub const TOKEN_True: i32 = 52;
pub const TOKEN_False: i32 = 53;
pub const TOKEN_SquareL: i32 = 54;
pub const TOKEN_SquareR: i32 = 55;
pub const TOKEN_StrOpen: i32 = 56;
pub const TOKEN_StrClose: i32 = 57;
#[inline]
fn token_major(t: &Token) -> i32 {
    match t {
        &Token::EOI => 0,
        &Token::ANY(_) => TOKEN_ANY,
        &Token::COMMA(_) => TOKEN_COMMA,
        &Token::HASHTAG(_) => TOKEN_HASHTAG,
        &Token::ID(_) => TOKEN_ID,
        &Token::INT(_) => TOKEN_INT,
        &Token::NEWLINE(_) => TOKEN_NEWLINE,
        &Token::PLUS(_) => TOKEN_PLUS,
        &Token::StrLit(_) => TOKEN_StrLit,
        &Token::TYPE_ID(_) => TOKEN_TYPE_ID,
        &Token::ASSIGN => TOKEN_ASSIGN,
        &Token::BLOCKARROW => TOKEN_BLOCKARROW,
        &Token::WHERE => TOKEN_WHERE,
        &Token::GIVEN => TOKEN_GIVEN,
        &Token::OR => TOKEN_OR,
        &Token::XOR => TOKEN_XOR,
        &Token::AND => TOKEN_AND,
        &Token::ConcatNewline => TOKEN_ConcatNewline,
        &Token::NOT => TOKEN_NOT,
        &Token::LT => TOKEN_LT,
        &Token::LTEQ => TOKEN_LTEQ,
        &Token::EQ => TOKEN_EQ,
        &Token::NEQ => TOKEN_NEQ,
        &Token::GT => TOKEN_GT,
        &Token::GTEQ => TOKEN_GTEQ,
        &Token::MINUS => TOKEN_MINUS,
        &Token::TIMES => TOKEN_TIMES,
        &Token::DIVIDE => TOKEN_DIVIDE,
        &Token::LPAREN => TOKEN_LPAREN,
        &Token::RPAREN => TOKEN_RPAREN,
        &Token::FAILED => TOKEN_FAILED,
        &Token::WITH => TOKEN_WITH,
        &Token::CONTEXTID => TOKEN_CONTEXTID,
        &Token::DT => TOKEN_DT,
        &Token::FAIL => TOKEN_FAIL,
        &Token::Let => TOKEN_Let,
        &Token::Fork => TOKEN_Fork,
        &Token::DollarGT => TOKEN_DollarGT,
        &Token::CurlyL => TOKEN_CurlyL,
        &Token::CurlyR => TOKEN_CurlyR,
        &Token::Func => TOKEN_Func,
        &Token::COLON => TOKEN_COLON,
        &Token::TYPE_INT => TOKEN_TYPE_INT,
        &Token::TYPE_STR => TOKEN_TYPE_STR,
        &Token::TYPE_BOOL => TOKEN_TYPE_BOOL,
        &Token::MACRO => TOKEN_MACRO,
        &Token::DOLLAR => TOKEN_DOLLAR,
        &Token::IF => TOKEN_IF,
        &Token::ELSE => TOKEN_ELSE,
        &Token::MOD => TOKEN_MOD,
        &Token::VOID => TOKEN_VOID,
        &Token::DollarQuestion => TOKEN_DollarQuestion,
        &Token::True => TOKEN_True,
        &Token::False => TOKEN_False,
        &Token::SquareL => TOKEN_SquareL,
        &Token::SquareR => TOKEN_SquareR,
        &Token::StrOpen => TOKEN_StrOpen,
        &Token::StrClose => TOKEN_StrClose,
    }
}
#[inline]
fn token_minor(t: Token) -> YYMinorType {
  match t {
        Token::ANY(x) => YYMinorType::YY158(x),
        Token::COMMA(x) => YYMinorType::YY158(x),
        Token::HASHTAG(x) => YYMinorType::YY173(x),
        Token::ID(x) => YYMinorType::YY66(x),
        Token::INT(x) => YYMinorType::YY108(x),
        Token::NEWLINE(x) => YYMinorType::YY158(x),
        Token::PLUS(x) => YYMinorType::YY158(x),
        Token::StrLit(x) => YYMinorType::YY66(x),
        Token::TYPE_ID(x) => YYMinorType::YY66(x),
        _ => YYMinorType::YY0
  }
}
const YY_ACTTAB_COUNT: i32 = 733;
const YY_ACTION: [YYACTIONTYPE; 733] = [
 /*     0 */   135,   98,  138,  102,   97,   36,  117,  119,  118,  130,
 /*    10 */   101,   95,  129,   66,  128,   30,  134,   67,   28,   27,
 /*    20 */     6,    2,   16,  146,  154,   10,    4,  103,   97,   41,
 /*    30 */   149,   99,   94,   90,   14,  134,  129,   76,  128,  122,
 /*    40 */   134,   26,   84,   32,   12,  142,  155,  140,  139,  137,
 /*    50 */   136,    8,   80,   68,  135,   98,  138,  134,   97,   62,
 /*    60 */   110,  119,  118,   97,   39,   38,  129,    5,  128,   30,
 /*    70 */   134,  129,   13,  128,   91,  134,   16,   37,  133,   10,
 /*    80 */   112,    3,   93,  125,  149,   99,   94,   90,   14,   31,
 /*    90 */    89,   76,  135,  141,  138,  123,   84,   15,   12,   95,
 /*   100 */    75,  140,  139,  137,  136,    8,    9,   68,  121,   74,
 /*   110 */    72,   11,   71,  115,  114,  113,   83,   33,   24,   23,
 /*   120 */    25,  143,   34,   22,   21,   18,   17,   20,   19,   29,
 /*   130 */    28,   27,   70,  144,  106,   65,   97,   43,  120,  140,
 /*   140 */   139,  137,  136,   32,  129,   68,  128,  124,  134,   26,
 /*   150 */    87,   86,   73,   26,   97,   42,    9,   61,  111,   35,
 /*   160 */    81,   11,  129,   92,  128,  241,  134,   82,   24,   23,
 /*   170 */    25,  143,   85,   22,   21,   18,   17,   20,   19,   29,
 /*   180 */    28,   27,  109,  127,   97,   42,    1,   96,  132,  131,
 /*   190 */   108,  241,  129,  126,  128,  241,  134,   97,   36,  241,
 /*   200 */   116,  241,  107,   26,  241,  129,    9,  128,  241,  134,
 /*   210 */   241,   11,  241,  241,  241,  241,  241,  241,   24,   23,
 /*   220 */    25,  143,  241,   22,   21,   18,   17,   20,   19,   29,
 /*   230 */    28,   27,   97,   40,  241,  241,  241,  241,  241,  241,
 /*   240 */   129,  241,  128,   79,  134,  241,  241,   97,   44,  241,
 /*   250 */   241,  241,  241,   26,  241,  129,    7,  128,  241,  134,
 /*   260 */   241,   11,  241,  241,  241,  241,  241,  241,   24,   23,
 /*   270 */    25,  143,  241,   22,   21,   18,   17,   20,   19,   29,
 /*   280 */    28,   27,   11,  241,  241,  241,  241,  241,  241,   24,
 /*   290 */    23,   25,  143,  241,   22,   21,   18,   17,   20,   19,
 /*   300 */    29,   28,   27,   26,  127,   97,   50,  241,   97,   54,
 /*   310 */   241,   11,  241,  129,  241,  128,  129,  134,  128,  241,
 /*   320 */   134,  241,  241,  241,   26,   18,   17,   20,   19,   29,
 /*   330 */    28,   27,   11,  241,  241,  241,  241,  241,  241,   24,
 /*   340 */    23,   25,  143,  241,   22,   21,   18,   17,   20,   19,
 /*   350 */    29,   28,   27,   26,   97,   64,  241,   97,   78,  241,
 /*   360 */    97,   77,  129,   95,  128,  129,  134,  128,  129,  134,
 /*   370 */   128,   11,  134,  241,   26,  241,  241,  241,   24,   23,
 /*   380 */    25,  143,  241,   22,   21,   18,   17,   20,   19,   29,
 /*   390 */    28,   27,  135,   98,  138,  241,   97,   49,  241,  241,
 /*   400 */   241,  241,  241,  241,  129,  241,  128,   30,  134,  241,
 /*   410 */   241,  241,  241,   26,   16,   11,  241,   10,  145,  241,
 /*   420 */   241,  241,  241,  241,   25,  143,  241,   22,   21,   18,
 /*   430 */    17,   20,   19,   29,   28,   27,   12,  241,  241,  140,
 /*   440 */   139,  137,  136,    8,  241,   68,  135,   98,  138,  241,
 /*   450 */    97,   53,  241,  241,  241,  241,  241,   26,  129,  241,
 /*   460 */   128,   30,  134,  241,   97,   52,  241,  241,   16,   97,
 /*   470 */    51,   10,  129,  241,  128,  241,  134,  129,  241,  128,
 /*   480 */   241,  134,  241,  241,  241,  241,  241,  241,  240,  100,
 /*   490 */    12,  241,   69,  140,  139,  137,  136,    8,  148,   68,
 /*   500 */   241,  147,  241,  152,  151,  150,   97,   47,  241,   11,
 /*   510 */   241,  241,  241,  241,  129,  241,  128,  241,  134,  143,
 /*   520 */   241,   22,   21,   18,   17,   20,   19,   29,   28,   27,
 /*   530 */   241,  241,  153,  241,  241,   69,  241,  241,  241,  241,
 /*   540 */   241,  148,  241,  241,  147,  241,  152,  151,  150,   97,
 /*   550 */    47,   26,  241,   88,  241,  241,   69,  129,  241,  128,
 /*   560 */   241,  134,  148,  241,  241,  147,  241,  152,  151,  150,
 /*   570 */    97,   47,  241,  241,  241,  241,  241,  241,  129,  241,
 /*   580 */   128,  241,  134,  105,  241,  241,   69,  241,  241,  241,
 /*   590 */   241,  241,  148,  241,  241,  147,  241,  152,  151,  150,
 /*   600 */    97,   47,  104,  241,  241,   69,  241,  241,  129,  241,
 /*   610 */   128,  148,  134,  241,  147,  241,  152,  151,  150,   97,
 /*   620 */    47,  241,   11,  241,  241,  241,  241,  129,  241,  128,
 /*   630 */   241,  134,  241,  241,  241,  241,  239,  239,  239,  239,
 /*   640 */    29,   28,   27,   97,   60,  241,   97,   59,  241,   97,
 /*   650 */    58,  129,  241,  128,  129,  134,  128,  129,  134,  128,
 /*   660 */   241,  134,  241,  241,   26,  241,  241,  241,  241,  241,
 /*   670 */    97,   57,  241,   97,   56,  241,   97,   55,  129,  241,
 /*   680 */   128,  129,  134,  128,  129,  134,  128,  241,  134,   97,
 /*   690 */    63,  241,  241,  241,   97,   48,  241,  129,  241,  128,
 /*   700 */   241,  134,  129,  241,  128,  241,  134,   97,   46,  241,
 /*   710 */   241,  241,  241,  241,  241,  129,  241,  128,  241,  134,
 /*   720 */    97,   45,  241,  241,  241,  241,  241,  241,  129,  241,
 /*   730 */   128,  241,  134,
];
const YY_LOOKAHEAD: [YYCODETYPE; 733] = [
 /*     0 */     3,    4,    5,   31,   77,   78,   65,   66,   67,   82,
 /*    10 */    32,   38,   85,    4,   87,   18,   89,    8,   26,   27,
 /*    20 */    47,    1,   25,   77,    0,   28,    6,   30,   77,   78,
 /*    30 */    33,   34,   35,   36,   37,   89,   85,   40,   87,   58,
 /*    40 */    89,   49,   45,   58,   47,   77,    0,   50,   51,   52,
 /*    50 */    53,   54,   67,   56,    3,    4,    5,   89,   77,   78,
 /*    60 */    65,   66,   67,   77,   78,    3,   85,   28,   87,   18,
 /*    70 */    89,   85,   11,   87,   88,   89,   25,   46,   57,   28,
 /*    80 */     9,    6,    4,   55,   33,   34,   35,   36,   37,   21,
 /*    90 */     4,   40,    3,    4,    5,   29,   45,   21,   47,   38,
 /*   100 */    28,   50,   51,   52,   53,   54,    2,   56,   39,   29,
 /*   110 */     2,    7,   28,   42,   43,   44,    4,   28,   14,   15,
 /*   120 */    16,   17,   29,   19,   20,   21,   22,   23,   24,   25,
 /*   130 */    26,   27,    2,   29,   29,   48,   77,   78,   71,   50,
 /*   140 */    51,   52,   53,   58,   85,   56,   87,   88,   89,   49,
 /*   150 */     4,   70,    4,   49,   77,   78,    2,   41,   70,   80,
 /*   160 */     4,    7,   85,   86,   87,   91,   89,   73,   14,   15,
 /*   170 */    16,   17,   80,   19,   20,   21,   22,   23,   24,   25,
 /*   180 */    26,   27,   73,   29,   77,   78,   58,   90,   90,   90,
 /*   190 */    67,   91,   85,   86,   87,   91,   89,   77,   78,   91,
 /*   200 */    79,   91,   82,   49,   91,   85,    2,   87,   91,   89,
 /*   210 */    91,    7,   91,   91,   91,   91,   91,   91,   14,   15,
 /*   220 */    16,   17,   91,   19,   20,   21,   22,   23,   24,   25,
 /*   230 */    26,   27,   77,   78,   91,   91,   91,   91,   91,   91,
 /*   240 */    85,   91,   87,   88,   89,   91,   91,   77,   78,   91,
 /*   250 */    91,   91,   91,   49,   91,   85,    2,   87,   91,   89,
 /*   260 */    91,    7,   91,   91,   91,   91,   91,   91,   14,   15,
 /*   270 */    16,   17,   91,   19,   20,   21,   22,   23,   24,   25,
 /*   280 */    26,   27,    7,   91,   91,   91,   91,   91,   91,   14,
 /*   290 */    15,   16,   17,   91,   19,   20,   21,   22,   23,   24,
 /*   300 */    25,   26,   27,   49,   29,   77,   78,   91,   77,   78,
 /*   310 */    91,    7,   91,   85,   91,   87,   85,   89,   87,   91,
 /*   320 */    89,   91,   91,   91,   49,   21,   22,   23,   24,   25,
 /*   330 */    26,   27,    7,   91,   91,   91,   91,   91,   91,   14,
 /*   340 */    15,   16,   17,   91,   19,   20,   21,   22,   23,   24,
 /*   350 */    25,   26,   27,   49,   77,   78,   91,   77,   78,   91,
 /*   360 */    77,   78,   85,   38,   87,   85,   89,   87,   85,   89,
 /*   370 */    87,    7,   89,   91,   49,   91,   91,   91,   14,   15,
 /*   380 */    16,   17,   91,   19,   20,   21,   22,   23,   24,   25,
 /*   390 */    26,   27,    3,    4,    5,   91,   77,   78,   91,   91,
 /*   400 */    91,   91,   91,   91,   85,   91,   87,   18,   89,   91,
 /*   410 */    91,   91,   91,   49,   25,    7,   91,   28,   29,   91,
 /*   420 */    91,   91,   91,   91,   16,   17,   91,   19,   20,   21,
 /*   430 */    22,   23,   24,   25,   26,   27,   47,   91,   91,   50,
 /*   440 */    51,   52,   53,   54,   91,   56,    3,    4,    5,   91,
 /*   450 */    77,   78,   91,   91,   91,   91,   91,   49,   85,   91,
 /*   460 */    87,   18,   89,   91,   77,   78,   91,   91,   25,   77,
 /*   470 */    78,   28,   85,   91,   87,   91,   89,   85,   91,   87,
 /*   480 */    91,   89,   91,   91,   91,   91,   91,   91,   59,   60,
 /*   490 */    47,   91,   63,   50,   51,   52,   53,   54,   69,   56,
 /*   500 */    91,   72,   91,   74,   75,   76,   77,   78,   91,    7,
 /*   510 */    91,   91,   91,   91,   85,   91,   87,   91,   89,   17,
 /*   520 */    91,   19,   20,   21,   22,   23,   24,   25,   26,   27,
 /*   530 */    91,   91,   60,   91,   91,   63,   91,   91,   91,   91,
 /*   540 */    91,   69,   91,   91,   72,   91,   74,   75,   76,   77,
 /*   550 */    78,   49,   91,   60,   91,   91,   63,   85,   91,   87,
 /*   560 */    91,   89,   69,   91,   91,   72,   91,   74,   75,   76,
 /*   570 */    77,   78,   91,   91,   91,   91,   91,   91,   85,   91,
 /*   580 */    87,   91,   89,   60,   91,   91,   63,   91,   91,   91,
 /*   590 */    91,   91,   69,   91,   91,   72,   91,   74,   75,   76,
 /*   600 */    77,   78,   60,   91,   91,   63,   91,   91,   85,   91,
 /*   610 */    87,   69,   89,   91,   72,   91,   74,   75,   76,   77,
 /*   620 */    78,   91,    7,   91,   91,   91,   91,   85,   91,   87,
 /*   630 */    91,   89,   91,   91,   91,   91,   21,   22,   23,   24,
 /*   640 */    25,   26,   27,   77,   78,   91,   77,   78,   91,   77,
 /*   650 */    78,   85,   91,   87,   85,   89,   87,   85,   89,   87,
 /*   660 */    91,   89,   91,   91,   49,   91,   91,   91,   91,   91,
 /*   670 */    77,   78,   91,   77,   78,   91,   77,   78,   85,   91,
 /*   680 */    87,   85,   89,   87,   85,   89,   87,   91,   89,   77,
 /*   690 */    78,   91,   91,   91,   77,   78,   91,   85,   91,   87,
 /*   700 */    91,   89,   85,   91,   87,   91,   89,   77,   78,   91,
 /*   710 */    91,   91,   91,   91,   91,   85,   91,   87,   91,   89,
 /*   720 */    77,   78,   91,   91,   91,   91,   91,   91,   85,   91,
 /*   730 */    87,   91,   89,
];
const YY_SHIFT_USE_DFLT: i32 = -29;
const YY_SHIFT_COUNT: i32 = 103;
const YY_SHIFT_MIN: i32 = -28;
const YY_SHIFT_MAX: i32 = 615;
const YY_SHIFT_OFST: [i16; 104] = [
 /*     0 */    -3,   51,   51,   51,   51,  389,  443,  443,  443,  443,
 /*    10 */   443,  443,  443,  443,  443,  443,  443,  443,  443,  443,
 /*    20 */   443,  443,  443,  443,  443,  443,  443,  443,  443,  443,
 /*    30 */   443,  443,  443,  443,   61,   61,  325,   89,   89,  154,
 /*    40 */   104,  275,  254,  204,  364,  364,  364,  364,  364,  364,
 /*    50 */   364,  408,  408,  502,  502,  615,  615,  615,  615,  304,
 /*    60 */   304,   71,   -8,   -8,   -8,  -27,    9,    9,    9,   20,
 /*    70 */   156,  156,  148,  116,  116,  148,  146,  100,  100,  105,
 /*    80 */    87,  130,   93,   84,  112,  108,   80,   72,   69,   76,
 /*    90 */    86,   66,   28,   68,   78,   75,   21,   31,   39,   62,
 /*   100 */    46,   24,  -22,  -28,
];
const YY_REDUCE_USE_DFLT: i32 = -74;
const YY_REDUCE_COUNT: i32 = 78;
const YY_REDUCE_MIN: i32 = -73;
const YY_REDUCE_MAX: i32 = 643;
const YY_REDUCE_OFST: [i16; 79] = [
 /*     0 */   429,  542,  523,  493,  472,  155,  120,  107,   77,   59,
 /*    10 */   -14,  -19,  -73,  643,  630,  617,  612,  599,  596,  593,
 /*    20 */   572,  569,  566,  392,  387,  373,  319,  283,  280,  277,
 /*    30 */   231,  228,  170,  -49,   -5,  -59,  -15,  -32,  -54,   85,
 /*    40 */    85,   85,   85,   85,   85,   85,   85,   85,   85,   85,
 /*    50 */    85,   85,   85,   85,   85,   85,   85,   85,   85,   85,
 /*    60 */    85,  121,   85,   85,   85,  123,   99,   98,   97,  128,
 /*    70 */   109,   94,   88,   92,   79,   81,   67,   85,   85,
];
const YY_DEFAULT: [YYACTIONTYPE; 154] = [
 /*     0 */   156,  156,  156,  156,  156,  239,  239,  229,  229,  239,
 /*    10 */   239,  239,  239,  239,  239,  239,  239,  239,  239,  239,
 /*    20 */   239,  239,  239,  239,  239,  239,  239,  239,  239,  239,
 /*    30 */   239,  239,  239,  239,  239,  239,  239,  239,  239,  239,
 /*    40 */   239,  239,  230,  233,  202,  173,  170,  169,  168,  208,
 /*    50 */   167,  211,  210,  209,  199,  217,  216,  215,  214,  213,
 /*    60 */   212,  239,  203,  201,  205,  239,  236,  236,  236,  239,
 /*    70 */   187,  187,  177,  180,  180,  177,  239,  207,  206,  239,
 /*    80 */   239,  188,  239,  239,  239,  178,  239,  239,  239,  239,
 /*    90 */   239,  239,  239,  239,  239,  239,  239,  218,  220,  239,
 /*   100 */   239,  239,  239,  239,  159,  158,  192,  196,  195,  189,
 /*   110 */   186,  179,  185,  184,  183,  182,  181,  176,  172,  171,
 /*   120 */   175,  174,  204,  232,  234,  228,  231,  219,  198,  197,
 /*   130 */   194,  238,  237,  235,  227,  226,  225,  224,  223,  222,
 /*   140 */   221,  220,  193,  200,  191,  190,  166,  165,  164,  163,
 /*   150 */   162,  161,  160,  157,
];

/* TMPL: fallback tokens */

const YY_FALLBACK: [i32; 0] = [
];

/* TMPL: symbol names */


/* TMPL: rules */


/* TMPL: destructors */


/* TMPL: stack-overflow */


/* TMPL: stack-overflow */

const YY_RULE_INFO: [YYCODETYPE; 85] = [
  59,
  59,
  60,
  60,
  60,
  60,
  63,
  63,
  63,
  63,
  63,
  63,
  76,
  74,
  74,
  75,
  75,
  65,
  65,
  66,
  67,
  69,
  71,
  70,
  70,
  70,
  80,
  80,
  79,
  79,
  79,
  79,
  72,
  73,
  73,
  73,
  78,
  78,
  78,
  78,
  78,
  82,
  82,
  78,
  78,
  78,
  78,
  78,
  78,
  78,
  78,
  78,
  78,
  78,
  78,
  78,
  78,
  78,
  78,
  78,
  78,
  78,
  78,
  78,
  78,
  77,
  77,
  77,
  77,
  77,
  77,
  77,
  77,
  77,
  85,
  86,
  86,
  86,
  87,
  88,
  88,
  89,
  90,
  90,
  90,
];

struct YYStackEntry {
    stateno: i32, /* The state-number */
    major: i32,     /* The major token value.  This is the code
                            ** number for the token at this stack level */
    minor: YYMinorType,    /* The user-supplied minor token value.  This
                            ** is the value of the token  */
}

pub struct Parser {
    yyerrcnt: i32, /* Shifts left before out of the error */
    yystack: Vec<YYStackEntry>,
    extra:  Result<Ast, i32> ,
}

impl Parser {

    pub fn new(
            extra:  Result<Ast, i32> ,
        ) -> Parser {
        let mut p = Parser { yyerrcnt: -1, yystack: Vec::new(), extra: extra};
        p.yystack.push(YYStackEntry{stateno: 0, major: 0, minor: YYMinorType::YY0});
        p
    }

    pub fn into_extra(self) ->  Result<Ast, i32>  {
        self.extra
    }
    pub fn extra(&self) -> & Result<Ast, i32>  {
        &self.extra
    }

    pub fn parse(&mut self, token: Token) {

        let yymajor = token_major(&token);
        let yyendofinput = yymajor==0;
        let mut yyerrorhit = false;
        while !self.yystack.is_empty() {
            let yyact = self.find_shift_action(yymajor);
            if yyact < YYNSTATE {
                assert!(!yyendofinput);  /* Impossible to shift the $ token */
                let yyminor = token_minor(token);
                self.yy_shift(yyact, yymajor, yyminor);
                self.yyerrcnt -= 1;
                break;
            } else if yyact < YYNSTATE + YYNRULE {
                self.yy_reduce(yyact - YYNSTATE);
            } else {
                /* A syntax error has occurred.
                 ** The response to an error depends upon whether or not the
                 ** grammar defines an error token "ERROR".
                 */
                assert!(yyact == YYNSTATE+YYNRULE);
                if YYERRORSYMBOL != 0 {
                    /* This is what we do if the grammar does define ERROR:
                     **
                     **  * Call the %syntax_error function.
                     **
                     **  * Begin popping the stack until we enter a state where
                     **    it is legal to shift the error symbol, then shift
                     **    the error symbol.
                     **
                     **  * Set the error count to three.
                     **
                     **  * Begin accepting and shifting new tokens.  No new error
                     **    processing will occur until three tokens have been
                     **    shifted successfully.
                     **
                     */
                    if self.yyerrcnt < 0 {
                        self.yy_syntax_error(&token);
                    }
                    let yymx = self.yystack[self.yystack.len() - 1].major;
                    if yymx==YYERRORSYMBOL || yyerrorhit {
                        break;
                    } else {
                        let mut yyact;
                        while !self.yystack.is_empty() {
                            yyact = self.find_reduce_action(YYERRORSYMBOL);
                            if yyact < YYNSTATE {
                                if !yyendofinput {
                                    self.yy_shift(yyact, YYERRORSYMBOL, YYMinorType::YY0);
                                }
                                break;
                            }
                            self.yystack.pop().unwrap();
                        }
                        if self.yystack.is_empty() || yyendofinput {
                            self.yy_parse_failed();
                            break;
                        }
                    }
                    self.yyerrcnt = 3;
                    yyerrorhit = true;
                } else {
                    /* This is what we do if the grammar does not define ERROR:
                     **
                     **  * Report an error message, and throw away the input token.
                     **
                     **  * If the input token is $, then fail the parse.
                     **
                     ** As before, subsequent error messages are suppressed until
                     ** three input tokens have been successfully shifted.
                     */
                    if self.yyerrcnt <= 0 {
                        self.yy_syntax_error(&token);
                    }
                    self.yyerrcnt = 3;
                    if yyendofinput {
                        self.yy_parse_failed();
                    }
                    break;
                }
            }
        }
    }

    /*
    ** Find the appropriate action for a parser given the terminal
    ** look-ahead token look_ahead.
    */
    fn find_shift_action(&self, look_ahead: i32) -> i32 {

        let stateno = self.yystack[self.yystack.len() - 1].stateno;

        if stateno > YY_SHIFT_COUNT {
            return YY_DEFAULT[stateno as usize] as i32;
        }
        let i = YY_SHIFT_OFST[stateno as usize] as i32;
        if i == YY_SHIFT_USE_DFLT {
            return YY_DEFAULT[stateno as usize] as i32;
        }
        assert!(look_ahead != YYNOCODE);
        let i = i + look_ahead;

        if i < 0 || i >= YY_ACTTAB_COUNT || YY_LOOKAHEAD[i as usize] as i32 != look_ahead {
            if look_ahead > 0 {
                if (look_ahead as usize) < YY_FALLBACK.len() {
                    let fallback = YY_FALLBACK[look_ahead as usize];
                    if fallback != 0 {
                        println!("FALLBACK");
                        return self.find_shift_action(fallback);
                    }
                }
                if YYWILDCARD > 0 {
                    let j = i - look_ahead + (YYWILDCARD as i32);
                    if j >= 0 && j < YY_ACTTAB_COUNT && YY_LOOKAHEAD[j as usize]==YYWILDCARD {
                        println!("WILDCARD");
                        return YY_ACTION[j as usize] as i32;
                    }
                }
            }
            return YY_DEFAULT[stateno as usize] as i32;
        } else {
            return YY_ACTION[i as usize] as i32;
        }
    }

    /*
    ** Find the appropriate action for a parser given the non-terminal
    ** look-ahead token iLookAhead.
    */
    fn find_reduce_action(&self, look_ahead: i32) -> i32 {
        let stateno = self.yystack[self.yystack.len() - 1].stateno;
        if YYERRORSYMBOL != 0 && stateno > YY_REDUCE_COUNT {
            return YY_DEFAULT[stateno as usize] as i32;
        }
        assert!(stateno <= YY_REDUCE_COUNT);
        let i = YY_REDUCE_OFST[stateno as usize] as i32;
        assert!(i != YY_REDUCE_USE_DFLT);
        assert!(look_ahead != YYNOCODE );
        let i = i + look_ahead;
        if YYERRORSYMBOL != 0 && (i < 0 || i >= YY_ACTTAB_COUNT || YY_LOOKAHEAD[i as usize] as i32 != look_ahead) {
            return YY_DEFAULT[stateno as usize] as i32;
        }
        assert!(i >= 0 && i < YY_ACTTAB_COUNT);
        assert!(YY_LOOKAHEAD[i as usize] as i32 == look_ahead);
        return YY_ACTION[i as usize] as i32;
    }

    fn yy_shift(&mut self, new_state: i32, major: i32, minor: YYMinorType) {
        self.yystack.push(YYStackEntry{stateno: new_state, major: major, minor: minor});
    }

    fn yy_reduce(&mut self, yyruleno: i32) {

        let yygotominor: YYMinorType = match yyruleno {
            /* Beginning here are the reduction cases.  */
            0 /* program ::= FAILED WITH CONTEXTID */
            => 
{
let yyres :  Ast ;
self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Ast::Nothing;

} };
 YYMinorType::YY63(yyres)
}
            ,
            1 /* program ::= stmts */
            => 
{
let yyres :  Ast ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY32(yy0),) => {

	if list::is_empty(&yy0) {
		panic!("null program");
	}
	// ignore yyres, it doesn't really go anywhere for program
	yyres = Ast::Nothing;
	// we're done, so put yy0 in extra
	self.extra = Ok(Ast::ReplRoot(yy0));

},    _ => unreachable!() };
 YYMinorType::YY63(yyres)
}
            ,
            2 /* stmts ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

    println!("parse empty statements list");
	yyres = sexpr::new(SexprType::BlockExpr, list::empty());

} };
 YYMinorType::YY32(yyres)
}
            ,
            3 /* stmts ::= stmt NEWLINE stmts */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy2),) => {

    println!("parse new stmt {:?}", yy0);
	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            4 /* stmts ::= stmt ANY stmts */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY158(yy1),) => {

    panic!("newline expected, found {:?}", yy1);
	yyres = Val::Void;

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            5 /* stmts ::= stmt error stmts */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match () {
 () => {

    println!("newline error between statements");
	yyres = Val::Void;

} };
 YYMinorType::YY32(yyres)
}
            ,
            6 /* stmt ::= let_stmt */
          | 8 /* stmt ::= fail_stmt */
          | 10 /* stmt ::= func_stmt */
          | 11 /* stmt ::= macro_stmt */
          | 43 /* expr ::= list */
          | 44 /* expr ::= tuple */
          | 64 /* expr ::= term */
          | 73 /* term ::= strexpr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY32(yy0),) => {
 yyres = yy0; 
},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            7 /* stmt ::= expr_stmt */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY32(yy0),) => {

    yyres = yy0;

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            9 /* stmt ::= DT */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Void; 
} };
 YYMinorType::YY32(yyres)
}
            ,
            12 /* fail_stmt ::= FAIL HASHTAG term */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY173(yy1),YYMinorType::YY32(yy2),) => {

verbose_out!("found fail_stmt {:?}\n", yy1);
	/*
	yyres = Val::list(
		list::push(yy0,
		list::push(yy1,
		list::push(yy2,
		NULL))));
		*/
	yyres = Val::Failure;

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            13 /* let_stmt ::= Let ID EQ expr */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY66(yy1),YYMinorType::YY32(yy3),) => {

	let bind = list::cons(Val::new_str(yy1), list::singleton(yy3));
	yyres = sexpr::new(SexprType::Bind, bind);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            14 /* let_stmt ::= Fork ID EQ expr */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY66(yy1),YYMinorType::YY32(yy3),) => {

	let bind = list::cons(Val::new_str(yy1), list::singleton(yy3));
	yyres = sexpr::new(SexprType::Fork, bind);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            15 /* expr_stmt ::= expr */
          | 17 /* block ::= arrow_block */
          | 18 /* block ::= curly_block */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY32(yy0),) => {

	yyres = yy0;

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            16 /* expr_stmt ::= DollarGT expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY32(yy1),) => {
 yyres = yy1; 
},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            19 /* arrow_block ::= BLOCKARROW expr */
          | 40 /* expr ::= IF if_expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY32(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            20 /* curly_block ::= CurlyL NEWLINE stmts CurlyR */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp2.minor,) {
 (YYMinorType::YY32(yy2),) => {

	yyres = yy2;

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            21 /* func_stmt ::= Func dfunc_1 */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY32(yy1),) => {

	/*
	const Val *plist = Val::list(p);
	const Val *mcase = Val::tuple2(plist, C);
	const list *mblk = list::singleton(mcase);

	Val *func = Val::fexpr(mblk, T);
	yyres = Val::tuple2(yy1, func, LET_ASSIGN);
	*/
	yyres = Val::Sexpr(SexprType::DefFunc, Box::new(yy1));

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            22 /* dfunc_1 ::= ID LPAREN dfunc_args RPAREN opt_typex block */
            => 
{
let yyres :  Val ;
let yyp5 = self.yystack.pop().unwrap();
let yyp4 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,yyp4.minor,yyp5.minor,) {
 (YYMinorType::YY66(yy0),YYMinorType::YY32(yy2),YYMinorType::YY29(yy4),YYMinorType::YY32(yy5),) => {

	/*
	const Val *plist = Val::list(p);
	const Val *mcase = Val::tuple2(plist, yy5);
	const list *mblk = list::singleton(mcase);

	Val *func = Val::fexpr(mblk, T);
	yyres = Val::tuple2(yy0, func, LET_ASSIGN);
	*/
	let id = Val::id(yy0);
	let typ = Val::Type(yy4);
	yyres = sexpr::single_func_list(id, yy2, typ, yy5)

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            23 /* dfunc_args ::= */
          | 75 /* list_items ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

	yyres = list::empty();

} };
 YYMinorType::YY32(yyres)
}
            ,
            24 /* dfunc_args ::= ID opt_typex */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY66(yy0),YYMinorType::YY29(yy1),) => {

	yyres = list::singleton(sexpr::id_with_type(yy0, yy1));

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            25 /* dfunc_args ::= ID opt_typex COMMA dfunc_args */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY66(yy0),YYMinorType::YY29(yy1),YYMinorType::YY32(yy3),) => {

	yyres = list::cons(sexpr::id_with_type(yy0, yy1), yy3);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            26 /* opt_typex ::= */
            => 
{
let yyres :  Type ;
match () {
 () => {

	yyres = Type::AnonVar;

} };
 YYMinorType::YY29(yyres)
}
            ,
            27 /* opt_typex ::= COLON typex */
            => 
{
let yyres :  Type ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY29(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY29(yyres)
}
            ,
            28 /* typex ::= TYPE_INT */
            => 
{
let yyres :  Type ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Type::Int;

} };
 YYMinorType::YY29(yyres)
}
            ,
            29 /* typex ::= TYPE_STR */
            => 
{
let yyres :  Type ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Type::Str;

} };
 YYMinorType::YY29(yyres)
}
            ,
            30 /* typex ::= TYPE_BOOL */
            => 
{
let yyres :  Type ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Type::Bool;

} };
 YYMinorType::YY29(yyres)
}
            ,
            31 /* typex ::= TYPE_ID */
            => 
{
let yyres :  Type ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY66(yy0),) => {

	yyres = Type::User(yy0);

},    _ => unreachable!() };
 YYMinorType::YY29(yyres)
}
            ,
            32 /* macro_stmt ::= MACRO ID LPAREN macro_args RPAREN block */
            => 
{
let yyres :  Val ;
let yyp5 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,yyp5.minor,) {
 (YYMinorType::YY66(yy1),YYMinorType::YY32(yy3),YYMinorType::YY32(yy5),) => {

    yyres = sexpr::new(SexprType::DefMacro,
        list::cons(Val::id(yy1),
        list::cons(yy3,
        list::cons(yy5,
        Val::Nil
    ))));

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            33 /* macro_args ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

    yyres = Val::Nil;

} };
 YYMinorType::YY32(yyres)
}
            ,
            34 /* macro_args ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY66(yy0),) => {

    yyres = list::singleton(Val::id(yy0));

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            35 /* macro_args ::= ID COMMA macro_args */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY66(yy0),YYMinorType::YY32(yy2),) => {

    yyres = list::cons(Val::id(yy0), yy2);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            36 /* expr ::= ID LPAREN RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY66(yy0),) => {

	verbose_out!("zero param function call!");
	yyres = sexpr::call(yy0, Val::Nil);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            37 /* expr ::= ID LPAREN expr RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY66(yy0),YYMinorType::YY32(yy2),) => {

	verbose_out!("one param function call!");
	let args = list::singleton(yy2);
	yyres = sexpr::call(yy0, args);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            38 /* expr ::= ID LPAREN tuple_args RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY66(yy0),YYMinorType::YY32(yy2),) => {

	verbose_out!("multi param function call!");
	yyres = sexpr::call(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            39 /* expr ::= term DOLLAR term */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy2),) => {

	/* yyres = Val::binaryop(yy0, yy2, D); */
	yyres = Val::Void;

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            41 /* if_expr ::= expr curly_block ELSE curly_block */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy1),YYMinorType::YY32(yy3),) => {

	yyres = sexpr::ifexpr(yy0, yy1, yy3);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            42 /* if_expr ::= expr curly_block ELSE IF if_expr */
            => 
{
let yyres :  Val ;
let yyp4 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,yyp4.minor,) {
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy1),YYMinorType::YY32(yy4),) => {

	yyres = sexpr::ifexpr(yy0, yy1, yy4);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            45 /* expr ::= NOT expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY32(yy1),) => {

	yyres = sexpr::call("bool_not".to_string(), list::singleton(yy1));

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            46 /* expr ::= expr ConcatNewline */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY32(yy0),) => {

	let newline = Val::Str(Arc::new("\n".to_string()));
	let args = list::cons(yy0, list::singleton(newline));
	yyres = sexpr::new(SexprType::StrExpr, args)

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            47 /* expr ::= MINUS expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY32(yy1),) => {

	yyres = sexpr::call("negate".to_string(), list::singleton(yy1));

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            48 /* expr ::= expr error expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy2),) => {

    write!(stderr(), "binaryop error: {:?} err {:?}\n", yy0, yy2).ok();
    yyres = Val::Void;

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            49 /* expr ::= expr PLUS expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy2),) => {

    println!("parse {:?}+{:?}\n", yy0, yy2);
	yyres = sexpr::binaryop("int_add".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            50 /* expr ::= expr PLUS error */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY32(yy0),) => {

    write!(stderr(), "wtf PLUS error: {:?} + error\n", yy0).ok();
    yyres = Val::Void;

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            51 /* expr ::= expr MINUS expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy2),) => {

	yyres = sexpr::binaryop("int_sub".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            52 /* expr ::= expr TIMES expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy2),) => {

	yyres = sexpr::binaryop("int_mult".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            53 /* expr ::= expr DIVIDE expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy2),) => {

	yyres = sexpr::binaryop("int_div".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            54 /* expr ::= expr MOD expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy2),) => {

	yyres = sexpr::binaryop("int_mod".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            55 /* expr ::= expr AND expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy2),) => {

	yyres = sexpr::binaryop("and".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            56 /* expr ::= expr OR expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy2),) => {

	yyres = sexpr::binaryop("or".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            57 /* expr ::= expr XOR expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy2),) => {

	yyres = sexpr::binaryop("xor".to_string(),yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            58 /* expr ::= expr LT expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy2),) => {

	yyres = sexpr::binaryop("less_than".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            59 /* expr ::= expr LTEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy2),) => {

	yyres = sexpr::binaryop("less_than_equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            60 /* expr ::= expr GT expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy2),) => {

	yyres = sexpr::binaryop("greater_than".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            61 /* expr ::= expr GTEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy2),) => {

	yyres = sexpr::binaryop("greater_than_equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            62 /* expr ::= expr EQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy2),) => {

	yyres = sexpr::binaryop("equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            63 /* expr ::= expr NEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy2),) => {

	let eq = sexpr::binaryop("equal".to_string(), yy0, yy2);
	yyres = sexpr::call("bool_not".to_string(), list::singleton(eq));

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            65 /* term ::= LPAREN expr RPAREN */
          | 74 /* list ::= SquareL list_items SquareR */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY32(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            66 /* term ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY66(yy0),) => {
 yyres = Val::id(yy0); 
},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            67 /* term ::= VOID */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Val::Void;

} };
 YYMinorType::YY32(yyres)
}
            ,
            68 /* term ::= DollarQuestion */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Val::id("$".to_string());

} };
 YYMinorType::YY32(yyres)
}
            ,
            69 /* term ::= INT */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY108(yy0),) => {

	yyres = Val::Int(yy0);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            70 /* term ::= True */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Bool(true); 
} };
 YYMinorType::YY32(yyres)
}
            ,
            71 /* term ::= False */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Bool(false); 
} };
 YYMinorType::YY32(yyres)
}
            ,
            72 /* term ::= HASHTAG */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY173(yy0),) => {

	yyres = Val::Hashtag(Arc::new(yy0.data));

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            76 /* list_items ::= expr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY32(yy0),) => {

	yyres = list::singleton(yy0);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            77 /* list_items ::= expr COMMA list_items */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy2),) => {

	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            78 /* tuple ::= LPAREN tuple_args RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY32(yy1),) => {

	yyres = Val::tuple_from_list(yy1);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            79 /* tuple_args ::= expr COMMA expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy2),) => {

	verbose_out!("base tuple args!");
	yyres = list::cons(yy0, list::singleton(yy2));

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            80 /* tuple_args ::= expr COMMA tuple_args */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy2),) => {

	verbose_out!("additional tuple arg!");
	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            81 /* strexpr ::= StrOpen strlist StrClose */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY32(yy1),) => {

	yyres = sexpr::strexpr(yy1);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            82 /* strlist ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

	yyres = Val::Nil;

} };
 YYMinorType::YY32(yyres)
}
            ,
            83 /* strlist ::= StrLit strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY66(yy0),YYMinorType::YY32(yy1),) => {

	yyres = list::cons(Val::new_str(yy0), yy1);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            84 /* strlist ::= ID strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY66(yy0),YYMinorType::YY32(yy1),) => {

	yyres = list::cons(Val::id(yy0), yy1);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            _ => unreachable!(),
        };
        let yygoto = YY_RULE_INFO[yyruleno as usize] as i32;
        let yyact = self.find_reduce_action(yygoto);
        if yyact < YYNSTATE {
            self.yy_shift(yyact, yygoto, yygotominor);
        } else {
            assert!(yyact == YYNSTATE + YYNRULE + 1);
            self.yy_accept();
        }
    }

    fn yy_parse_failed(&mut self) {
        self.yystack.clear();

	println!("parse failure");
	panic!("Parse failed.");
    }

    fn yy_syntax_error(&mut self, token: &Token) {

    match token {
        &Token::EOI => {
	        panic!("Unexpected end of file. Maybe add newline?");
        }
        _ => {
	        println!("syntax error at token {:?}\n", token);
	        panic!("Syntax error. wtf? @ <line,column>?");
        }
    }
    }

    fn yy_accept(&mut self) {
        self.yystack.clear();

	//println!("parse accepted");
    }
}

