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
const YYNSTATE: i32 = 152;
const YYNRULE: i32 = 84;
const YYERRORSYMBOL: i32 = 58;

//const YY_NO_ACTION: i32 = YYNSTATE+YYNRULE+2;
//const YY_ACCEPT_ACTION: i32 = YYNSTATE+YYNRULE+1;
//const YY_ERROR_ACTION: i32 = YYNSTATE+YYNRULE+1;

/* TMPL: action tables */

#[derive(Debug,PartialEq
)]
pub enum Token {
    EOI, //0
    ANY, //1
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
        &Token::ANY => TOKEN_ANY,
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
const YY_ACTTAB_COUNT: i32 = 708;
const YY_ACTION: [YYACTIONTYPE; 708] = [
 /*     0 */   133,   97,  136,  101,   96,   35,  115,  117,  116,  128,
 /*    10 */   100,   94,  127,   65,  126,   29,  132,   66,   27,   26,
 /*    20 */     5,  152,   15,  144,  153,    9,   37,  102,   96,   40,
 /*    30 */   147,   98,   93,   89,   13,  132,  127,   74,  126,  120,
 /*    40 */   132,   25,   83,   31,   11,  140,    4,  138,  137,  135,
 /*    50 */   134,    7,   79,   67,  133,   97,  136,  132,   96,   61,
 /*    60 */   108,  117,  116,   96,   38,   36,  127,  131,  126,   29,
 /*    70 */   132,  127,   12,  126,   90,  132,   15,   92,    2,    9,
 /*    80 */   110,   30,  123,  121,  147,   98,   93,   89,   13,   88,
 /*    90 */    14,   74,  133,  139,  136,  119,   83,   72,   11,   94,
 /*   100 */    70,  138,  137,  135,  134,    7,    8,   67,   73,   82,
 /*   110 */    69,   10,   33,  113,  112,  111,   68,   32,   23,   22,
 /*   120 */    24,  141,   64,   21,   20,   17,   16,   19,   18,   28,
 /*   130 */    27,   26,  104,  142,    1,    3,   96,   42,  118,  138,
 /*   140 */   137,  135,  134,   25,  127,   67,  126,  122,  132,   31,
 /*   150 */    86,   85,   71,   25,   96,   41,    8,   60,   34,   84,
 /*   160 */   109,   10,  127,   91,  126,   81,  132,   80,   23,   22,
 /*   170 */    24,  141,  107,   21,   20,   17,   16,   19,   18,   28,
 /*   180 */    27,   26,  106,  125,   96,   41,   95,  130,  114,  238,
 /*   190 */   129,  238,  127,  124,  126,  238,  132,   96,   35,  238,
 /*   200 */   238,  238,  105,   25,  238,  127,    8,  126,  238,  132,
 /*   210 */   238,   10,  238,  238,  238,  238,  238,  238,   23,   22,
 /*   220 */    24,  141,  238,   21,   20,   17,   16,   19,   18,   28,
 /*   230 */    27,   26,   96,   39,  238,  238,  238,  238,  238,  238,
 /*   240 */   127,  238,  126,   78,  132,  238,  238,   96,   43,  238,
 /*   250 */   238,  238,  238,   25,  238,  127,    6,  126,  238,  132,
 /*   260 */   238,   10,  238,  238,  238,  238,  238,  238,   23,   22,
 /*   270 */    24,  141,  238,   21,   20,   17,   16,   19,   18,   28,
 /*   280 */    27,   26,   10,  238,  238,  238,  238,  238,  238,   23,
 /*   290 */    22,   24,  141,  238,   21,   20,   17,   16,   19,   18,
 /*   300 */    28,   27,   26,   25,  125,   96,   49,  238,   96,   53,
 /*   310 */   238,   10,  238,  127,  238,  126,  127,  132,  126,  238,
 /*   320 */   132,  238,  238,  238,   25,   17,   16,   19,   18,   28,
 /*   330 */    27,   26,   10,  238,  238,  238,  238,  238,  238,   23,
 /*   340 */    22,   24,  141,  238,   21,   20,   17,   16,   19,   18,
 /*   350 */    28,   27,   26,   25,   96,   63,  238,   96,   76,  238,
 /*   360 */    96,   75,  127,   94,  126,  127,  132,  126,  127,  132,
 /*   370 */   126,   10,  132,  238,   25,  238,  238,  238,   23,   22,
 /*   380 */    24,  141,  238,   21,   20,   17,   16,   19,   18,   28,
 /*   390 */    27,   26,  133,   97,  136,  238,   96,   48,  238,  238,
 /*   400 */   238,  238,  238,  238,  127,  238,  126,   29,  132,  238,
 /*   410 */   238,  238,  238,   25,   15,   10,  238,    9,  143,  238,
 /*   420 */   238,  238,  238,  238,   24,  141,  238,   21,   20,   17,
 /*   430 */    16,   19,   18,   28,   27,   26,   11,  238,  238,  138,
 /*   440 */   137,  135,  134,    7,  238,   67,  133,   97,  136,  238,
 /*   450 */    96,   52,  238,  238,  238,  238,  238,   25,  127,  238,
 /*   460 */   126,   29,  132,  238,   96,   51,  238,  238,   15,   96,
 /*   470 */    50,    9,  127,  238,  126,  238,  132,  127,  238,  126,
 /*   480 */   238,  132,  238,  238,  238,  238,  238,  238,  237,   99,
 /*   490 */    11,  238,   77,  138,  137,  135,  134,    7,  146,   67,
 /*   500 */   238,  145,  238,  150,  149,  148,   96,   46,  238,   10,
 /*   510 */   238,  238,  238,  238,  127,  238,  126,  238,  132,  141,
 /*   520 */   238,   21,   20,   17,   16,   19,   18,   28,   27,   26,
 /*   530 */   238,  238,  151,  238,  238,   77,  238,  238,  238,  238,
 /*   540 */   238,  146,  238,  238,  145,  238,  150,  149,  148,   96,
 /*   550 */    46,   25,  238,   87,  238,  238,   77,  127,  238,  126,
 /*   560 */   238,  132,  146,  238,  238,  145,  238,  150,  149,  148,
 /*   570 */    96,   46,  238,  238,  238,  238,  238,  238,  127,  238,
 /*   580 */   126,  238,  132,  103,  238,  238,   77,  238,  238,  238,
 /*   590 */   238,  238,  146,  238,  238,  145,  238,  150,  149,  148,
 /*   600 */    96,   46,  238,   10,  238,  238,  238,  238,  127,  238,
 /*   610 */   126,  238,  132,  238,  238,  238,  238,  236,  236,  236,
 /*   620 */   236,   28,   27,   26,   96,   59,  238,   96,   58,  238,
 /*   630 */   238,  238,  127,  238,  126,  127,  132,  126,  238,  132,
 /*   640 */    96,   57,  238,   96,   56,   25,   96,   55,  127,  238,
 /*   650 */   126,  127,  132,  126,  127,  132,  126,  238,  132,  238,
 /*   660 */    96,   54,  238,  238,  238,  238,  238,  238,  127,  238,
 /*   670 */   126,  238,  132,  238,  238,   96,   62,  238,   96,   47,
 /*   680 */   238,   96,   45,  127,  238,  126,  127,  132,  126,  127,
 /*   690 */   132,  126,  238,  132,  238,   96,   44,  238,  238,  238,
 /*   700 */   238,  238,  238,  127,  238,  126,  238,  132,
];
const YY_LOOKAHEAD: [YYCODETYPE; 708] = [
 /*     0 */     3,    4,    5,   31,   77,   78,   65,   66,   67,   82,
 /*    10 */    32,   38,   85,    4,   87,   18,   89,    8,   26,   27,
 /*    20 */    47,    0,   25,   77,    0,   28,    3,   30,   77,   78,
 /*    30 */    33,   34,   35,   36,   37,   89,   85,   40,   87,   58,
 /*    40 */    89,   49,   45,   58,   47,   77,   28,   50,   51,   52,
 /*    50 */    53,   54,   67,   56,    3,    4,    5,   89,   77,   78,
 /*    60 */    65,   66,   67,   77,   78,   46,   85,   57,   87,   18,
 /*    70 */    89,   85,   11,   87,   88,   89,   25,    4,    6,   28,
 /*    80 */     9,   21,   55,   29,   33,   34,   35,   36,   37,    4,
 /*    90 */    21,   40,    3,    4,    5,   39,   45,   29,   47,   38,
 /*   100 */     2,   50,   51,   52,   53,   54,    2,   56,   28,    4,
 /*   110 */    28,    7,   29,   42,   43,   44,    2,   28,   14,   15,
 /*   120 */    16,   17,   48,   19,   20,   21,   22,   23,   24,   25,
 /*   130 */    26,   27,   29,   29,   58,    6,   77,   78,   71,   50,
 /*   140 */    51,   52,   53,   49,   85,   56,   87,   88,   89,   58,
 /*   150 */     4,   70,    4,   49,   77,   78,    2,   41,   80,   80,
 /*   160 */    70,    7,   85,   86,   87,   73,   89,    4,   14,   15,
 /*   170 */    16,   17,   73,   19,   20,   21,   22,   23,   24,   25,
 /*   180 */    26,   27,   67,   29,   77,   78,   90,   90,   79,   91,
 /*   190 */    90,   91,   85,   86,   87,   91,   89,   77,   78,   91,
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
 /*   600 */    77,   78,   91,    7,   91,   91,   91,   91,   85,   91,
 /*   610 */    87,   91,   89,   91,   91,   91,   91,   21,   22,   23,
 /*   620 */    24,   25,   26,   27,   77,   78,   91,   77,   78,   91,
 /*   630 */    91,   91,   85,   91,   87,   85,   89,   87,   91,   89,
 /*   640 */    77,   78,   91,   77,   78,   49,   77,   78,   85,   91,
 /*   650 */    87,   85,   89,   87,   85,   89,   87,   91,   89,   91,
 /*   660 */    77,   78,   91,   91,   91,   91,   91,   91,   85,   91,
 /*   670 */    87,   91,   89,   91,   91,   77,   78,   91,   77,   78,
 /*   680 */    91,   77,   78,   85,   91,   87,   85,   89,   87,   85,
 /*   690 */    89,   87,   91,   89,   91,   77,   78,   91,   91,   91,
 /*   700 */    91,   91,   91,   85,   91,   87,   91,   89,
];
const YY_SHIFT_USE_DFLT: i32 = -29;
const YY_SHIFT_COUNT: i32 = 102;
const YY_SHIFT_MIN: i32 = -28;
const YY_SHIFT_MAX: i32 = 596;
const YY_SHIFT_OFST: [i16; 103] = [
 /*     0 */    -3,   51,   51,   51,  389,  443,  443,  443,  443,  443,
 /*    10 */   443,  443,  443,  443,  443,  443,  443,  443,  443,  443,
 /*    20 */   443,  443,  443,  443,  443,  443,  443,  443,  443,  443,
 /*    30 */   443,  443,  443,   61,   61,  325,   89,   89,  154,  104,
 /*    40 */   275,  254,  204,  364,  364,  364,  364,  364,  364,  364,
 /*    50 */   408,  408,  502,  502,  596,  596,  596,  596,  304,  304,
 /*    60 */    71,   -8,   -8,   -8,  -27,    9,    9,    9,  163,  163,
 /*    70 */   148,  116,  116,  148,  146,   94,   94,  129,  103,   74,
 /*    80 */   114,   83,   82,  105,   98,   68,   80,   56,   69,   85,
 /*    90 */    54,   27,   60,   73,   72,   10,   19,   18,   23,   24,
 /*   100 */    21,  -22,  -28,
];
const YY_REDUCE_USE_DFLT: i32 = -74;
const YY_REDUCE_COUNT: i32 = 77;
const YY_REDUCE_MIN: i32 = -73;
const YY_REDUCE_MAX: i32 = 618;
const YY_REDUCE_OFST: [i16; 78] = [
 /*     0 */   429,  523,  493,  472,  155,  120,  107,   77,   59,  -14,
 /*    10 */   -19,  -73,  618,  604,  601,  598,  583,  569,  566,  563,
 /*    20 */   550,  547,  392,  387,  373,  319,  283,  280,  277,  231,
 /*    30 */   228,  170,  -49,   -5,  -59,  -15,  -32,  -54,   91,   91,
 /*    40 */    91,   91,   91,   91,   91,   91,   91,   91,   91,   91,
 /*    50 */    91,   91,   91,   91,   91,   91,   91,   91,   91,   91,
 /*    60 */   109,   91,   91,   91,  115,  100,   97,   96,   99,   92,
 /*    70 */    90,   79,   78,   81,   67,   91,   91,   76,
];
const YY_DEFAULT: [YYACTIONTYPE; 152] = [
 /*     0 */   154,  154,  154,  154,  236,  236,  226,  226,  236,  236,
 /*    10 */   236,  236,  236,  236,  236,  236,  236,  236,  236,  236,
 /*    20 */   236,  236,  236,  236,  236,  236,  236,  236,  236,  236,
 /*    30 */   236,  236,  236,  236,  236,  236,  236,  236,  236,  236,
 /*    40 */   236,  227,  230,  199,  170,  167,  166,  165,  205,  164,
 /*    50 */   208,  207,  206,  196,  214,  213,  212,  211,  210,  209,
 /*    60 */   236,  200,  198,  202,  236,  233,  233,  233,  184,  184,
 /*    70 */   174,  177,  177,  174,  236,  204,  203,  236,  236,  236,
 /*    80 */   185,  236,  236,  236,  175,  236,  236,  236,  236,  236,
 /*    90 */   236,  236,  236,  236,  236,  236,  215,  217,  236,  236,
 /*   100 */   236,  236,  236,  156,  189,  193,  192,  186,  183,  176,
 /*   110 */   182,  181,  180,  179,  178,  173,  169,  168,  172,  171,
 /*   120 */   201,  229,  231,  225,  228,  216,  195,  194,  191,  235,
 /*   130 */   234,  232,  224,  223,  222,  221,  220,  219,  218,  217,
 /*   140 */   190,  197,  188,  187,  163,  162,  161,  160,  159,  158,
 /*   150 */   157,  155,
];

/* TMPL: fallback tokens */

const YY_FALLBACK: [i32; 0] = [
];

/* TMPL: symbol names */


/* TMPL: rules */


/* TMPL: destructors */


/* TMPL: stack-overflow */


/* TMPL: stack-overflow */

const YY_RULE_INFO: [YYCODETYPE; 84] = [
  59,
  59,
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
            4 /* stmts ::= stmt error stmts */
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
            5 /* stmt ::= let_stmt */
          | 7 /* stmt ::= fail_stmt */
          | 9 /* stmt ::= func_stmt */
          | 10 /* stmt ::= macro_stmt */
          | 42 /* expr ::= list */
          | 43 /* expr ::= tuple */
          | 63 /* expr ::= term */
          | 72 /* term ::= strexpr */
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
            6 /* stmt ::= expr_stmt */
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
            8 /* stmt ::= DT */
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
            11 /* fail_stmt ::= FAIL HASHTAG term */
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
            12 /* let_stmt ::= Let ID EQ expr */
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
            13 /* let_stmt ::= Fork ID EQ expr */
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
            14 /* expr_stmt ::= expr */
          | 16 /* block ::= arrow_block */
          | 17 /* block ::= curly_block */
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
            15 /* expr_stmt ::= DollarGT expr */
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
            18 /* arrow_block ::= BLOCKARROW expr */
          | 39 /* expr ::= IF if_expr */
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
            19 /* curly_block ::= CurlyL NEWLINE stmts CurlyR */
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
            20 /* func_stmt ::= Func dfunc_1 */
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
            21 /* dfunc_1 ::= ID LPAREN dfunc_args RPAREN opt_typex block */
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
            22 /* dfunc_args ::= */
          | 74 /* list_items ::= */
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
            23 /* dfunc_args ::= ID opt_typex */
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
            24 /* dfunc_args ::= ID opt_typex COMMA dfunc_args */
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
            25 /* opt_typex ::= */
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
            26 /* opt_typex ::= COLON typex */
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
            27 /* typex ::= TYPE_INT */
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
            28 /* typex ::= TYPE_STR */
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
            29 /* typex ::= TYPE_BOOL */
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
            30 /* typex ::= TYPE_ID */
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
            31 /* macro_stmt ::= MACRO ID LPAREN macro_args RPAREN block */
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
            32 /* macro_args ::= */
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
            33 /* macro_args ::= ID */
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
            34 /* macro_args ::= ID COMMA macro_args */
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
            35 /* expr ::= ID LPAREN RPAREN */
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
            36 /* expr ::= ID LPAREN expr RPAREN */
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
            37 /* expr ::= ID LPAREN tuple_args RPAREN */
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
            38 /* expr ::= term DOLLAR term */
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
            40 /* if_expr ::= expr curly_block ELSE curly_block */
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
            41 /* if_expr ::= expr curly_block ELSE IF if_expr */
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
            44 /* expr ::= NOT expr */
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
            45 /* expr ::= expr ConcatNewline */
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
            46 /* expr ::= MINUS expr */
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
            47 /* expr ::= expr error expr */
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
            48 /* expr ::= expr PLUS expr */
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
            49 /* expr ::= expr PLUS error */
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
            50 /* expr ::= expr MINUS expr */
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
            51 /* expr ::= expr TIMES expr */
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
            52 /* expr ::= expr DIVIDE expr */
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
            53 /* expr ::= expr MOD expr */
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
            54 /* expr ::= expr AND expr */
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
            55 /* expr ::= expr OR expr */
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
            56 /* expr ::= expr XOR expr */
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
            57 /* expr ::= expr LT expr */
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
            58 /* expr ::= expr LTEQ expr */
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
            59 /* expr ::= expr GT expr */
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
            60 /* expr ::= expr GTEQ expr */
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
            61 /* expr ::= expr EQ expr */
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
            62 /* expr ::= expr NEQ expr */
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
            64 /* term ::= LPAREN expr RPAREN */
          | 73 /* list ::= SquareL list_items SquareR */
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
            65 /* term ::= ID */
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
            66 /* term ::= VOID */
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
            67 /* term ::= DollarQuestion */
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
            68 /* term ::= INT */
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
            69 /* term ::= True */
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
            70 /* term ::= False */
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
            71 /* term ::= HASHTAG */
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
            75 /* list_items ::= expr */
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
            76 /* list_items ::= expr COMMA list_items */
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
            77 /* tuple ::= LPAREN tuple_args RPAREN */
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
            78 /* tuple_args ::= expr COMMA expr */
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
            79 /* tuple_args ::= expr COMMA tuple_args */
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
            80 /* strexpr ::= StrOpen strlist StrClose */
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
            81 /* strlist ::= */
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
            82 /* strlist ::= StrLit strlist */
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
            83 /* strlist ::= ID strlist */
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

