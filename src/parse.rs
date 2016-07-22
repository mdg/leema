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
const YYNSTATE: i32 = 156;
const YYNRULE: i32 = 86;
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
 /*     0 */   137,   99,  140,   38,   80,   36,  118,  120,  119,  131,
 /*    10 */   103,   97,  130,   67,  129,   30,  136,   68,   28,   27,
 /*    20 */     6,    2,   16,  148,  156,   10,    4,  104,   80,   42,
 /*    30 */   151,  100,   96,   92,   14,  136,  130,   77,  129,  123,
 /*    40 */   136,   26,   86,   32,   12,   37,  102,  142,  141,  139,
 /*    50 */   138,    8,   82,   69,  137,   99,  140,  157,   80,   63,
 /*    60 */   111,  120,  119,   80,   40,  144,  130,   39,  129,   30,
 /*    70 */   136,  130,   13,  129,   93,  136,   16,  136,  132,   10,
 /*    80 */   113,    5,  135,   95,  151,  100,   96,   92,   14,    3,
 /*    90 */   136,   77,  137,  143,  140,   31,   86,  126,   12,   97,
 /*   100 */    91,  142,  141,  139,  138,    8,    9,   69,   15,  124,
 /*   110 */    76,   11,  122,  116,  115,  114,   73,   33,   24,   23,
 /*   120 */    25,  145,   75,   22,   21,   18,   17,   20,   19,   29,
 /*   130 */    28,   27,   85,  146,   72,   71,   80,   44,   34,  142,
 /*   140 */   141,  139,  138,   66,  130,   69,  129,  125,  136,   32,
 /*   150 */   107,   26,  121,   26,   80,   43,    9,   89,   88,   62,
 /*   160 */    74,   11,  130,   94,  129,   35,  136,  112,   24,   23,
 /*   170 */    25,  145,   87,   22,   21,   18,   17,   20,   19,   29,
 /*   180 */    28,   27,   83,  128,   80,   43,   84,    1,  110,   98,
 /*   190 */   134,  133,  130,  127,  129,  117,  136,   80,   36,  109,
 /*   200 */   244,  244,  108,   26,  244,  130,    9,  129,  244,  136,
 /*   210 */   244,   11,  244,  244,  244,  244,  244,  244,   24,   23,
 /*   220 */    25,  145,  244,   22,   21,   18,   17,   20,   19,   29,
 /*   230 */    28,   27,   80,   41,  244,  244,  244,  244,  244,  244,
 /*   240 */   130,  244,  129,   81,  136,  244,  244,   80,   45,  244,
 /*   250 */   244,  244,  244,   26,  244,  130,    7,  129,  244,  136,
 /*   260 */   244,   11,  244,  244,  244,  244,  244,  244,   24,   23,
 /*   270 */    25,  145,  244,   22,   21,   18,   17,   20,   19,   29,
 /*   280 */    28,   27,   11,  244,  244,  244,  244,  244,  244,   24,
 /*   290 */    23,   25,  145,  244,   22,   21,   18,   17,   20,   19,
 /*   300 */    29,   28,   27,   26,  128,   80,   51,  244,   80,   55,
 /*   310 */   244,   11,  244,  130,  244,  129,  130,  136,  129,  244,
 /*   320 */   136,  244,  244,  244,   26,   18,   17,   20,   19,   29,
 /*   330 */    28,   27,   11,  244,  244,  244,  244,  244,  244,   24,
 /*   340 */    23,   25,  145,  244,   22,   21,   18,   17,   20,   19,
 /*   350 */    29,   28,   27,   26,   80,   65,  244,   80,   79,  244,
 /*   360 */    80,   78,  130,   97,  129,  130,  136,  129,  130,  136,
 /*   370 */   129,   11,  136,  244,   26,  244,  244,  244,   24,   23,
 /*   380 */    25,  145,  244,   22,   21,   18,   17,   20,   19,   29,
 /*   390 */    28,   27,  137,   99,  140,  244,   80,   50,  244,  244,
 /*   400 */   244,  244,  244,  244,  130,  244,  129,   30,  136,  244,
 /*   410 */   244,  244,  244,   26,   16,   11,  244,   10,  147,  244,
 /*   420 */   244,  244,  244,  244,   25,  145,  244,   22,   21,   18,
 /*   430 */    17,   20,   19,   29,   28,   27,   12,  244,  244,  142,
 /*   440 */   141,  139,  138,    8,  244,   69,  137,   99,  140,  244,
 /*   450 */    80,   54,  244,  244,  244,  244,  244,   26,  130,  244,
 /*   460 */   129,   30,  136,  244,   80,   53,  244,  244,   16,   80,
 /*   470 */    52,   10,  130,  244,  129,  244,  136,  130,  244,  129,
 /*   480 */   244,  136,  244,  244,  244,  244,  244,  244,  243,  101,
 /*   490 */    12,  244,   70,  142,  141,  139,  138,    8,  150,   69,
 /*   500 */   244,  149,  244,  154,  153,  152,   80,   48,  244,   11,
 /*   510 */   244,  244,  244,  244,  130,  244,  129,  244,  136,  145,
 /*   520 */   244,   22,   21,   18,   17,   20,   19,   29,   28,   27,
 /*   530 */   244,  244,  155,  244,  244,   70,  244,  244,  244,  244,
 /*   540 */   244,  150,  244,  244,  149,  244,  154,  153,  152,   80,
 /*   550 */    48,   26,  244,   90,  244,  244,   70,  130,  244,  129,
 /*   560 */   244,  136,  150,  244,  244,  149,  244,  154,  153,  152,
 /*   570 */    80,   48,  244,  244,  244,  244,  244,  244,  130,  244,
 /*   580 */   129,  244,  136,  106,  244,  244,   70,  244,  244,  244,
 /*   590 */   244,  244,  150,  244,  244,  149,  244,  154,  153,  152,
 /*   600 */    80,   48,  105,  244,  244,   70,  244,  244,  130,  244,
 /*   610 */   129,  150,  136,  244,  149,  244,  154,  153,  152,   80,
 /*   620 */    48,  244,   11,  244,  244,  244,  244,  130,  244,  129,
 /*   630 */   244,  136,  244,  244,  244,  244,  242,  242,  242,  242,
 /*   640 */    29,   28,   27,   80,   61,  244,   80,   60,  244,   80,
 /*   650 */    59,  130,  244,  129,  130,  136,  129,  130,  136,  129,
 /*   660 */   244,  136,  244,  244,   26,  244,  244,  244,  244,  244,
 /*   670 */    80,   58,  244,   80,   57,  244,   80,   56,  130,  244,
 /*   680 */   129,  130,  136,  129,  130,  136,  129,  244,  136,   80,
 /*   690 */    64,  244,  244,  244,   80,   49,  244,  130,  244,  129,
 /*   700 */   244,  136,  130,  244,  129,  244,  136,   80,   47,  244,
 /*   710 */   244,  244,  244,  244,  244,  130,  244,  129,  244,  136,
 /*   720 */    80,   46,  244,  244,  244,  244,  244,  244,  130,  244,
 /*   730 */   129,  244,  136,
];
const YY_LOOKAHEAD: [YYCODETYPE; 733] = [
 /*     0 */     3,    4,    5,    4,   77,   78,   65,   66,   67,   82,
 /*    10 */    31,   38,   85,    4,   87,   18,   89,    8,   26,   27,
 /*    20 */    47,    1,   25,   77,    0,   28,    6,   30,   77,   78,
 /*    30 */    33,   34,   35,   36,   37,   89,   85,   40,   87,   58,
 /*    40 */    89,   49,   45,   58,   47,   46,   32,   50,   51,   52,
 /*    50 */    53,   54,   67,   56,    3,    4,    5,    0,   77,   78,
 /*    60 */    65,   66,   67,   77,   78,   77,   85,    3,   87,   18,
 /*    70 */    89,   85,   11,   87,   88,   89,   25,   89,   77,   28,
 /*    80 */     9,   28,   57,    4,   33,   34,   35,   36,   37,    6,
 /*    90 */    89,   40,    3,    4,    5,   21,   45,   55,   47,   38,
 /*   100 */     4,   50,   51,   52,   53,   54,    2,   56,   21,   29,
 /*   110 */    28,    7,   39,   42,   43,   44,    2,   28,   14,   15,
 /*   120 */    16,   17,   29,   19,   20,   21,   22,   23,   24,   25,
 /*   130 */    26,   27,    4,   29,   28,    2,   77,   78,   29,   50,
 /*   140 */    51,   52,   53,   48,   85,   56,   87,   88,   89,   58,
 /*   150 */    29,   49,   71,   49,   77,   78,    2,    4,   70,   41,
 /*   160 */     4,    7,   85,   86,   87,   80,   89,   70,   14,   15,
 /*   170 */    16,   17,   80,   19,   20,   21,   22,   23,   24,   25,
 /*   180 */    26,   27,    4,   29,   77,   78,   73,   58,   73,   90,
 /*   190 */    90,   90,   85,   86,   87,   79,   89,   77,   78,   67,
 /*   200 */    91,   91,   82,   49,   91,   85,    2,   87,   91,   89,
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
const YY_SHIFT_USE_DFLT: i32 = -28;
const YY_SHIFT_COUNT: i32 = 104;
const YY_SHIFT_MIN: i32 = -27;
const YY_SHIFT_MAX: i32 = 615;
const YY_SHIFT_OFST: [i16; 105] = [
 /*     0 */    -3,   51,   51,   51,   51,  389,  443,  443,  443,  443,
 /*    10 */   443,  443,  443,  443,  443,  443,  443,  443,  443,  443,
 /*    20 */   443,  443,  443,  443,  443,  443,  443,  443,  443,  443,
 /*    30 */   443,  443,  443,  443,   61,   61,  325,   89,   89,   89,
 /*    40 */   154,  104,  275,  254,  204,  364,  364,  364,  364,  364,
 /*    50 */   364,  364,  408,  408,  502,  502,  615,  615,  615,  615,
 /*    60 */   304,  304,   71,   -8,   -8,   -8,  -27,    9,    9,    9,
 /*    70 */    20,  178,  178,  156,  118,  118,  156,  153,  102,  102,
 /*    80 */    -1,  121,   95,  133,  109,  106,  128,  114,   93,   82,
 /*    90 */    73,   87,   96,   80,   42,   74,   79,   83,   25,   53,
 /*   100 */    64,   57,   24,   14,  -21,
];
const YY_REDUCE_USE_DFLT: i32 = -74;
const YY_REDUCE_COUNT: i32 = 79;
const YY_REDUCE_MIN: i32 = -73;
const YY_REDUCE_MAX: i32 = 643;
const YY_REDUCE_OFST: [i16; 80] = [
 /*     0 */   429,  542,  523,  493,  472,  155,  120,  107,   77,   59,
 /*    10 */   -14,  -19,  -73,  643,  630,  617,  612,  599,  596,  593,
 /*    20 */   572,  569,  566,  392,  387,  373,  319,  283,  280,  277,
 /*    30 */   231,  228,  170,  -49,   -5,  -59,  -15,    1,  -12,  -54,
 /*    40 */    91,   91,   91,   91,   91,   91,   91,   91,   91,   91,
 /*    50 */    91,   91,   91,   91,   91,   91,   91,   91,   91,   91,
 /*    60 */    91,   91,  116,   91,   91,   91,  132,  101,  100,   99,
 /*    70 */   129,  115,  113,   97,   92,   85,   88,   81,   91,   91,
];
const YY_DEFAULT: [YYACTIONTYPE; 156] = [
 /*     0 */   158,  158,  158,  158,  158,  242,  242,  232,  232,  242,
 /*    10 */   242,  242,  242,  242,  242,  242,  242,  242,  242,  242,
 /*    20 */   242,  242,  242,  242,  242,  242,  242,  242,  242,  242,
 /*    30 */   242,  242,  242,  242,  242,  242,  242,  242,  242,  242,
 /*    40 */   242,  242,  242,  233,  236,  205,  175,  172,  171,  170,
 /*    50 */   211,  169,  214,  213,  212,  202,  220,  219,  218,  217,
 /*    60 */   216,  215,  242,  206,  204,  208,  242,  239,  239,  239,
 /*    70 */   242,  189,  189,  179,  182,  182,  179,  242,  210,  209,
 /*    80 */   221,  242,  242,  190,  242,  242,  242,  180,  242,  242,
 /*    90 */   242,  242,  242,  242,  242,  242,  242,  242,  242,  223,
 /*   100 */   242,  242,  242,  242,  242,  161,  160,  194,  199,  198,
 /*   110 */   191,  188,  181,  187,  186,  185,  184,  183,  178,  174,
 /*   120 */   173,  177,  176,  207,  235,  237,  231,  234,  222,  201,
 /*   130 */   200,  197,  196,  241,  240,  238,  230,  229,  228,  227,
 /*   140 */   226,  225,  224,  223,  195,  203,  193,  192,  168,  167,
 /*   150 */   166,  165,  164,  163,  162,  159,
];

/* TMPL: fallback tokens */

const YY_FALLBACK: [i32; 0] = [
];

/* TMPL: symbol names */


/* TMPL: rules */


/* TMPL: destructors */


/* TMPL: stack-overflow */


/* TMPL: stack-overflow */

const YY_RULE_INFO: [YYCODETYPE; 86] = [
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
          | 44 /* expr ::= list */
          | 45 /* expr ::= tuple */
          | 65 /* expr ::= term */
          | 74 /* term ::= strexpr */
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
          | 41 /* expr ::= IF if_expr */
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
          | 76 /* list_items ::= */
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
            39 /* expr ::= term ID term */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY32(yy0),YYMinorType::YY66(yy1),YYMinorType::YY32(yy2),) => {

	yyres = sexpr::binaryop(yy1, yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            40 /* expr ::= term DOLLAR term */
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
            42 /* if_expr ::= expr curly_block ELSE curly_block */
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
            43 /* if_expr ::= expr curly_block ELSE IF if_expr */
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
            46 /* expr ::= NOT expr */
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
            47 /* expr ::= expr ConcatNewline */
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
            48 /* expr ::= MINUS expr */
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
            49 /* expr ::= expr error expr */
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
            50 /* expr ::= expr PLUS expr */
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
            51 /* expr ::= expr PLUS error */
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
            52 /* expr ::= expr MINUS expr */
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
            53 /* expr ::= expr TIMES expr */
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
            54 /* expr ::= expr DIVIDE expr */
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
            55 /* expr ::= expr MOD expr */
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
            56 /* expr ::= expr AND expr */
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
            57 /* expr ::= expr OR expr */
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
            58 /* expr ::= expr XOR expr */
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
            59 /* expr ::= expr LT expr */
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
            60 /* expr ::= expr LTEQ expr */
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
            61 /* expr ::= expr GT expr */
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
            62 /* expr ::= expr GTEQ expr */
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
            63 /* expr ::= expr EQ expr */
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
            64 /* expr ::= expr NEQ expr */
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
            66 /* term ::= LPAREN expr RPAREN */
          | 75 /* list ::= SquareL list_items SquareR */
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
            67 /* term ::= ID */
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
            68 /* term ::= VOID */
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
            69 /* term ::= DollarQuestion */
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
            70 /* term ::= INT */
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
            71 /* term ::= True */
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
            72 /* term ::= False */
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
            73 /* term ::= HASHTAG */
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
            77 /* list_items ::= expr */
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
            78 /* list_items ::= expr COMMA list_items */
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
            79 /* tuple ::= LPAREN tuple_args RPAREN */
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
            80 /* tuple_args ::= expr COMMA expr */
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
            81 /* tuple_args ::= expr COMMA tuple_args */
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
            82 /* strexpr ::= StrOpen strlist StrClose */
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
            83 /* strlist ::= */
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
            84 /* strlist ::= StrLit strlist */
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
            85 /* strlist ::= ID strlist */
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

