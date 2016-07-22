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
const YYNSTATE: i32 = 159;
const YYNRULE: i32 = 87;
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
    IF, //34
    FAIL, //35
    Let, //36
    Fork, //37
    DollarGT, //38
    CurlyL, //39
    CurlyR, //40
    Func, //41
    COLON, //42
    TYPE_INT, //43
    TYPE_STR, //44
    TYPE_BOOL, //45
    MACRO, //46
    DOLLAR, //47
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
pub const TOKEN_IF: i32 = 34;
pub const TOKEN_FAIL: i32 = 35;
pub const TOKEN_Let: i32 = 36;
pub const TOKEN_Fork: i32 = 37;
pub const TOKEN_DollarGT: i32 = 38;
pub const TOKEN_CurlyL: i32 = 39;
pub const TOKEN_CurlyR: i32 = 40;
pub const TOKEN_Func: i32 = 41;
pub const TOKEN_COLON: i32 = 42;
pub const TOKEN_TYPE_INT: i32 = 43;
pub const TOKEN_TYPE_STR: i32 = 44;
pub const TOKEN_TYPE_BOOL: i32 = 45;
pub const TOKEN_MACRO: i32 = 46;
pub const TOKEN_DOLLAR: i32 = 47;
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
        &Token::IF => TOKEN_IF,
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
const YY_ACTTAB_COUNT: i32 = 726;
const YY_ACTION: [YYACTIONTYPE; 726] = [
 /*     0 */   140,  100,  143,    2,   82,   38,   30,   29,    4,  134,
 /*    10 */   115,  105,  133,  106,  132,   32,  139,  120,  122,  121,
 /*    20 */    68,    6,   18,   40,   69,   10,  102,  107,   33,   28,
 /*    30 */   154,   13,  101,   94,   92,   15,   71,  103,   79,  140,
 /*    40 */   100,  143,   41,   86,  118,  117,  116,  145,  144,  142,
 /*    50 */   141,    8,  159,   70,   32,   82,   37,  113,  122,  121,
 /*    60 */   134,   18,    3,  133,   10,  132,   39,  139,  151,  154,
 /*    70 */    13,  101,   94,   92,   15,   33,  160,   79,    5,   14,
 /*    80 */   139,  147,   86,  135,   98,  138,  145,  144,  142,  141,
 /*    90 */     8,    9,   70,  139,  127,  139,   11,   93,  129,  125,
 /*   100 */   126,   17,   91,   26,   25,   27,  148,  102,   24,   23,
 /*   110 */    20,   19,   22,   21,   31,   30,   29,   16,  149,   82,
 /*   120 */    65,  124,   78,   75,   77,   82,   42,  133,   85,  132,
 /*   130 */    74,  139,   35,  133,   73,  132,   96,  139,   28,   82,
 /*   140 */    46,    9,   33,  123,   89,   88,   11,  133,   28,  132,
 /*   150 */   128,  139,   76,   26,   25,   27,  148,   36,   24,   23,
 /*   160 */    20,   19,   22,   21,   31,   30,   29,   64,  131,   83,
 /*   170 */   140,  100,  143,    1,   82,   45,  114,   84,  112,  111,
 /*   180 */    87,   99,  133,   97,  132,   32,  139,  248,   28,  119,
 /*   190 */   248,    9,   18,  248,  248,   10,   11,  137,  136,  248,
 /*   200 */   248,   12,  248,   26,   25,   27,  148,  248,   24,   23,
 /*   210 */    20,   19,   22,   21,   31,   30,   29,  145,  144,  142,
 /*   220 */   141,    8,  248,   70,   82,   45,  248,  248,  248,  248,
 /*   230 */   248,  248,  133,  130,  132,  248,  139,  248,   28,   82,
 /*   240 */    37,    7,  248,  248,  110,  248,   11,  133,  248,  132,
 /*   250 */   248,  139,  248,   26,   25,   27,  148,  248,   24,   23,
 /*   260 */    20,   19,   22,   21,   31,   30,   29,   11,  248,  248,
 /*   270 */   248,  248,  248,  248,   26,   25,   27,  148,  248,   24,
 /*   280 */    23,   20,   19,   22,   21,   31,   30,   29,   28,  131,
 /*   290 */    82,   43,  248,  248,  248,   82,   44,  248,  133,  248,
 /*   300 */   132,   95,  139,  133,  248,  132,  248,  139,  248,   28,
 /*   310 */    82,   52,  248,  248,  248,  248,  248,   11,  133,  248,
 /*   320 */   132,  248,  139,  248,   26,   25,   27,  148,  248,   24,
 /*   330 */    23,   20,   19,   22,   21,   31,   30,   29,   11,  248,
 /*   340 */   248,  248,  248,  248,  248,   26,   25,   27,  148,  102,
 /*   350 */    24,   23,   20,   19,   22,   21,   31,   30,   29,   28,
 /*   360 */   140,  100,  143,  248,   82,   57,  248,  248,  248,   82,
 /*   370 */    67,  248,  133,  248,  132,   32,  139,  133,  248,  132,
 /*   380 */    28,  139,   18,   11,  248,   10,  150,  248,  248,  248,
 /*   390 */   248,   12,   27,  148,  248,   24,   23,   20,   19,   22,
 /*   400 */    21,   31,   30,   29,  248,  248,  248,  145,  144,  142,
 /*   410 */   141,    8,  248,   70,  247,  104,  248,  248,   72,  248,
 /*   420 */   248,  248,  248,  248,  153,   28,  248,  152,  248,  157,
 /*   430 */   156,  155,   82,   49,  248,   11,  248,  248,  248,  248,
 /*   440 */   133,  248,  132,  248,  139,  148,  248,   24,   23,   20,
 /*   450 */    19,   22,   21,   31,   30,   29,  158,  248,  248,   72,
 /*   460 */   248,  248,  248,  248,  248,  153,  248,  248,  152,  248,
 /*   470 */   157,  156,  155,   82,   49,   90,  248,   28,   72,  248,
 /*   480 */   248,  133,  248,  132,  153,  139,  248,  152,  248,  157,
 /*   490 */   156,  155,   82,   49,  109,  248,  248,   72,  248,  248,
 /*   500 */   133,  248,  132,  153,  139,  248,  152,  248,  157,  156,
 /*   510 */   155,   82,   49,  108,  248,  248,   72,  248,  248,  133,
 /*   520 */   248,  132,  153,  139,  248,  152,  248,  157,  156,  155,
 /*   530 */    82,   49,  248,   11,  248,  248,  248,  248,  133,  248,
 /*   540 */   132,   11,  139,  248,  140,  146,  143,   20,   19,   22,
 /*   550 */    21,   31,   30,   29,  248,  246,  246,  246,  246,   31,
 /*   560 */    30,   29,   82,   81,  248,  248,  248,  248,  248,   34,
 /*   570 */   133,  248,  132,  248,  139,   28,  248,  248,   82,   80,
 /*   580 */   248,  248,  248,   28,  248,  248,  133,  248,  132,  248,
 /*   590 */   139,  145,  144,  142,  141,  248,  248,   70,  248,   82,
 /*   600 */    53,  248,  248,  248,  248,   82,   56,  133,  248,  132,
 /*   610 */   248,  139,  248,  133,  248,  132,  248,  139,  248,   82,
 /*   620 */    55,  248,   82,   54,  248,   82,   63,  133,  248,  132,
 /*   630 */   133,  139,  132,  133,  139,  132,  248,  139,   82,   62,
 /*   640 */   248,   82,   61,  248,  248,  248,  133,  248,  132,  133,
 /*   650 */   139,  132,  248,  139,  248,  248,   82,   60,  248,  248,
 /*   660 */   248,   82,   59,  248,  133,  248,  132,  248,  139,  133,
 /*   670 */   248,  132,  248,  139,  248,   82,   58,  248,   82,   66,
 /*   680 */   248,  248,  248,  133,  248,  132,  133,  139,  132,  248,
 /*   690 */   139,   82,   51,  248,  248,  248,  248,   82,   50,  133,
 /*   700 */   248,  132,  248,  139,  248,  133,  248,  132,  248,  139,
 /*   710 */    82,   48,  248,   82,   47,  248,  248,  248,  133,  248,
 /*   720 */   132,  133,  139,  132,  248,  139,
];
const YY_LOOKAHEAD: [YYCODETYPE; 726] = [
 /*     0 */     3,    4,    5,    1,   77,   78,   26,   27,    6,   82,
 /*    10 */     9,   32,   85,   31,   87,   18,   89,   65,   66,   67,
 /*    20 */     4,   34,   25,    4,    8,   28,   39,   30,   58,   49,
 /*    30 */    33,   34,   35,   36,   37,   38,   48,   67,   41,    3,
 /*    40 */     4,    5,    3,   46,   43,   44,   45,   50,   51,   52,
 /*    50 */    53,   54,    0,   56,   18,   77,   78,   65,   66,   67,
 /*    60 */    82,   25,    6,   85,   28,   87,   47,   89,   77,   33,
 /*    70 */    34,   35,   36,   37,   38,   58,    0,   41,   28,   11,
 /*    80 */    89,   77,   46,   77,   67,   57,   50,   51,   52,   53,
 /*    90 */    54,    2,   56,   89,   29,   89,    7,    4,   55,   29,
 /*   100 */    58,   21,    4,   14,   15,   16,   17,   39,   19,   20,
 /*   110 */    21,   22,   23,   24,   25,   26,   27,   21,   29,   77,
 /*   120 */    78,   40,   28,    2,   29,   77,   78,   85,    4,   87,
 /*   130 */    28,   89,   29,   85,    2,   87,   88,   89,   49,   77,
 /*   140 */    78,    2,   58,   71,    4,   70,    7,   85,   49,   87,
 /*   150 */    88,   89,    4,   14,   15,   16,   17,   80,   19,   20,
 /*   160 */    21,   22,   23,   24,   25,   26,   27,   42,   29,    4,
 /*   170 */     3,    4,    5,   58,   77,   78,   70,   73,   73,   67,
 /*   180 */    80,   90,   85,   86,   87,   18,   89,   91,   49,   79,
 /*   190 */    91,    2,   25,   91,   91,   28,    7,   90,   90,   91,
 /*   200 */    91,   34,   91,   14,   15,   16,   17,   91,   19,   20,
 /*   210 */    21,   22,   23,   24,   25,   26,   27,   50,   51,   52,
 /*   220 */    53,   54,   91,   56,   77,   78,   91,   91,   91,   91,
 /*   230 */    91,   91,   85,   86,   87,   91,   89,   91,   49,   77,
 /*   240 */    78,    2,   91,   91,   82,   91,    7,   85,   91,   87,
 /*   250 */    91,   89,   91,   14,   15,   16,   17,   91,   19,   20,
 /*   260 */    21,   22,   23,   24,   25,   26,   27,    7,   91,   91,
 /*   270 */    91,   91,   91,   91,   14,   15,   16,   17,   91,   19,
 /*   280 */    20,   21,   22,   23,   24,   25,   26,   27,   49,   29,
 /*   290 */    77,   78,   91,   91,   91,   77,   78,   91,   85,   91,
 /*   300 */    87,   88,   89,   85,   91,   87,   91,   89,   91,   49,
 /*   310 */    77,   78,   91,   91,   91,   91,   91,    7,   85,   91,
 /*   320 */    87,   91,   89,   91,   14,   15,   16,   17,   91,   19,
 /*   330 */    20,   21,   22,   23,   24,   25,   26,   27,    7,   91,
 /*   340 */    91,   91,   91,   91,   91,   14,   15,   16,   17,   39,
 /*   350 */    19,   20,   21,   22,   23,   24,   25,   26,   27,   49,
 /*   360 */     3,    4,    5,   91,   77,   78,   91,   91,   91,   77,
 /*   370 */    78,   91,   85,   91,   87,   18,   89,   85,   91,   87,
 /*   380 */    49,   89,   25,    7,   91,   28,   29,   91,   91,   91,
 /*   390 */    91,   34,   16,   17,   91,   19,   20,   21,   22,   23,
 /*   400 */    24,   25,   26,   27,   91,   91,   91,   50,   51,   52,
 /*   410 */    53,   54,   91,   56,   59,   60,   91,   91,   63,   91,
 /*   420 */    91,   91,   91,   91,   69,   49,   91,   72,   91,   74,
 /*   430 */    75,   76,   77,   78,   91,    7,   91,   91,   91,   91,
 /*   440 */    85,   91,   87,   91,   89,   17,   91,   19,   20,   21,
 /*   450 */    22,   23,   24,   25,   26,   27,   60,   91,   91,   63,
 /*   460 */    91,   91,   91,   91,   91,   69,   91,   91,   72,   91,
 /*   470 */    74,   75,   76,   77,   78,   60,   91,   49,   63,   91,
 /*   480 */    91,   85,   91,   87,   69,   89,   91,   72,   91,   74,
 /*   490 */    75,   76,   77,   78,   60,   91,   91,   63,   91,   91,
 /*   500 */    85,   91,   87,   69,   89,   91,   72,   91,   74,   75,
 /*   510 */    76,   77,   78,   60,   91,   91,   63,   91,   91,   85,
 /*   520 */    91,   87,   69,   89,   91,   72,   91,   74,   75,   76,
 /*   530 */    77,   78,   91,    7,   91,   91,   91,   91,   85,   91,
 /*   540 */    87,    7,   89,   91,    3,    4,    5,   21,   22,   23,
 /*   550 */    24,   25,   26,   27,   91,   21,   22,   23,   24,   25,
 /*   560 */    26,   27,   77,   78,   91,   91,   91,   91,   91,   28,
 /*   570 */    85,   91,   87,   91,   89,   49,   91,   91,   77,   78,
 /*   580 */    91,   91,   91,   49,   91,   91,   85,   91,   87,   91,
 /*   590 */    89,   50,   51,   52,   53,   91,   91,   56,   91,   77,
 /*   600 */    78,   91,   91,   91,   91,   77,   78,   85,   91,   87,
 /*   610 */    91,   89,   91,   85,   91,   87,   91,   89,   91,   77,
 /*   620 */    78,   91,   77,   78,   91,   77,   78,   85,   91,   87,
 /*   630 */    85,   89,   87,   85,   89,   87,   91,   89,   77,   78,
 /*   640 */    91,   77,   78,   91,   91,   91,   85,   91,   87,   85,
 /*   650 */    89,   87,   91,   89,   91,   91,   77,   78,   91,   91,
 /*   660 */    91,   77,   78,   91,   85,   91,   87,   91,   89,   85,
 /*   670 */    91,   87,   91,   89,   91,   77,   78,   91,   77,   78,
 /*   680 */    91,   91,   91,   85,   91,   87,   85,   89,   87,   91,
 /*   690 */    89,   77,   78,   91,   91,   91,   91,   77,   78,   85,
 /*   700 */    91,   87,   91,   89,   91,   85,   91,   87,   91,   89,
 /*   710 */    77,   78,   91,   77,   78,   91,   91,   91,   85,   91,
 /*   720 */    87,   85,   89,   87,   91,   89,
];
const YY_SHIFT_USE_DFLT: i32 = -22;
const YY_SHIFT_COUNT: i32 = 107;
const YY_SHIFT_MIN: i32 = -21;
const YY_SHIFT_MAX: i32 = 541;
const YY_SHIFT_OFST: [i16; 108] = [
 /*     0 */    -3,   36,   36,   36,   36,  357,  167,  167,  167,  167,
 /*    10 */   167,  167,  167,  167,  167,  167,  167,  167,  167,  167,
 /*    20 */   167,  167,  167,  167,  167,  167,  167,  167,  167,  167,
 /*    30 */   167,  167,  167,  167,  167,   68,   68,  310,  310,  541,
 /*    40 */   541,  541,  139,   89,  260,  239,  189,  331,  331,  331,
 /*    50 */   331,  331,  331,  331,  376,  376,  428,  428,  534,  534,
 /*    60 */   534,  534,  526,  526,    1,  -20,  -20,  -20,   16,   16,
 /*    70 */    16,  -13,    2,  165,  165,  148,  125,  125,  148,  140,
 /*    80 */    99,   99,   19,  132,  103,  102,  124,  121,   95,   94,
 /*    90 */    81,   96,   98,   80,   93,   70,   65,   43,  -12,   28,
 /*   100 */    50,   39,   56,  -12,   76,   52,  -21,  -18,
];
const YY_REDUCE_USE_DFLT: i32 = -74;
const YY_REDUCE_COUNT: i32 = 81;
const YY_REDUCE_MIN: i32 = -73;
const YY_REDUCE_MAX: i32 = 636;
const YY_REDUCE_OFST: [i16; 82] = [
 /*     0 */   355,  453,  434,  415,  396,  213,  162,  147,   97,   62,
 /*    10 */    48,   42,  -22,  -73,  636,  633,  620,  614,  601,  598,
 /*    20 */   584,  579,  564,  561,  548,  545,  542,  528,  522,  501,
 /*    30 */   485,  292,  287,  233,  218,   -8,  -48,   17,  -30,    6,
 /*    40 */     4,   -9,   84,   84,   84,   84,   84,   84,   84,   84,
 /*    50 */    84,   84,   84,   84,   84,   84,   84,   84,   84,   84,
 /*    60 */    84,   84,   84,   84,  110,   84,   84,   84,  108,  107,
 /*    70 */    91,  112,  115,  105,  104,  106,  100,   77,   75,   72,
 /*    80 */    84,   84,
];
const YY_DEFAULT: [YYACTIONTYPE; 159] = [
 /*     0 */   161,  161,  161,  161,  161,  246,  246,  236,  236,  246,
 /*    10 */   246,  246,  246,  246,  246,  246,  246,  246,  246,  246,
 /*    20 */   246,  246,  246,  246,  246,  246,  246,  246,  246,  246,
 /*    30 */   246,  246,  246,  246,  246,  246,  246,  246,  246,  246,
 /*    40 */   246,  246,  246,  246,  246,  237,  240,  179,  176,  175,
 /*    50 */   174,  173,  209,  215,  218,  217,  216,  206,  224,  223,
 /*    60 */   222,  221,  220,  219,  246,  210,  208,  212,  243,  243,
 /*    70 */   243,  246,  246,  193,  193,  183,  186,  186,  183,  246,
 /*    80 */   214,  213,  225,  194,  246,  246,  246,  184,  246,  246,
 /*    90 */   246,  246,  246,  246,  246,  246,  246,  246,  246,  246,
 /*   100 */   227,  246,  246,  171,  246,  246,  246,  246,  164,  163,
 /*   110 */   203,  202,  195,  192,  185,  191,  190,  189,  188,  187,
 /*   120 */   182,  178,  177,  181,  180,  198,  211,  239,  241,  235,
 /*   130 */   238,  226,  205,  204,  201,  200,  245,  244,  242,  234,
 /*   140 */   233,  232,  231,  230,  229,  228,  227,  199,  207,  197,
 /*   150 */   196,  172,  170,  169,  168,  167,  166,  165,  162,
];

/* TMPL: fallback tokens */

const YY_FALLBACK: [i32; 0] = [
];

/* TMPL: symbol names */


/* TMPL: rules */


/* TMPL: destructors */


/* TMPL: stack-overflow */


/* TMPL: stack-overflow */

const YY_RULE_INFO: [YYCODETYPE; 87] = [
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

    println!("newline expected, found {:?}", yy1);
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
          | 45 /* expr ::= list */
          | 46 /* expr ::= tuple */
          | 66 /* expr ::= term */
          | 75 /* term ::= strexpr */
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
            12 /* stmt ::= IF expr curly_block */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY32(yy1),YYMinorType::YY32(yy2),) => {

    yyres = sexpr::ifexpr(yy1, yy2, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            13 /* fail_stmt ::= FAIL HASHTAG term */
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
            14 /* let_stmt ::= Let ID EQ expr */
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
            15 /* let_stmt ::= Fork ID EQ expr */
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
            16 /* expr_stmt ::= expr */
          | 18 /* block ::= arrow_block */
          | 19 /* block ::= curly_block */
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
            17 /* expr_stmt ::= DollarGT expr */
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
            20 /* arrow_block ::= BLOCKARROW expr */
          | 42 /* expr ::= IF if_expr */
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
            21 /* curly_block ::= CurlyL NEWLINE stmts CurlyR */
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
            22 /* func_stmt ::= Func dfunc_1 */
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
            23 /* dfunc_1 ::= ID LPAREN dfunc_args RPAREN opt_typex block */
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
            24 /* dfunc_args ::= */
          | 77 /* list_items ::= */
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
            25 /* dfunc_args ::= ID opt_typex */
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
            26 /* dfunc_args ::= ID opt_typex COMMA dfunc_args */
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
            27 /* opt_typex ::= */
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
            28 /* opt_typex ::= COLON typex */
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
            29 /* typex ::= TYPE_INT */
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
            30 /* typex ::= TYPE_STR */
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
            31 /* typex ::= TYPE_BOOL */
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
            32 /* typex ::= TYPE_ID */
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
            33 /* macro_stmt ::= MACRO ID LPAREN macro_args RPAREN block */
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
            34 /* macro_args ::= */
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
            35 /* macro_args ::= ID */
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
            36 /* macro_args ::= ID COMMA macro_args */
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
            37 /* expr ::= ID LPAREN RPAREN */
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
            38 /* expr ::= ID LPAREN expr RPAREN */
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
            39 /* expr ::= ID LPAREN tuple_args RPAREN */
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
            40 /* expr ::= term ID term */
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
            41 /* expr ::= term DOLLAR term */
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
            43 /* if_expr ::= expr curly_block ELSE curly_block */
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
            44 /* if_expr ::= expr curly_block ELSE IF if_expr */
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
            47 /* expr ::= NOT expr */
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
            48 /* expr ::= expr ConcatNewline */
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
            49 /* expr ::= MINUS expr */
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
            50 /* expr ::= expr error expr */
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
            51 /* expr ::= expr PLUS expr */
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
            52 /* expr ::= expr PLUS error */
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
            53 /* expr ::= expr MINUS expr */
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
            54 /* expr ::= expr TIMES expr */
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
            55 /* expr ::= expr DIVIDE expr */
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
            56 /* expr ::= expr MOD expr */
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
            57 /* expr ::= expr AND expr */
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
            58 /* expr ::= expr OR expr */
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
            59 /* expr ::= expr XOR expr */
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
            60 /* expr ::= expr LT expr */
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
            61 /* expr ::= expr LTEQ expr */
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
            62 /* expr ::= expr GT expr */
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
            63 /* expr ::= expr GTEQ expr */
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
            64 /* expr ::= expr EQ expr */
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
            65 /* expr ::= expr NEQ expr */
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
            67 /* term ::= LPAREN expr RPAREN */
          | 76 /* list ::= SquareL list_items SquareR */
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
            68 /* term ::= ID */
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
            69 /* term ::= VOID */
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
            70 /* term ::= DollarQuestion */
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
            71 /* term ::= INT */
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
            72 /* term ::= True */
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
            73 /* term ::= False */
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
            74 /* term ::= HASHTAG */
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
            78 /* list_items ::= expr */
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
            79 /* list_items ::= expr COMMA list_items */
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
            80 /* tuple ::= LPAREN tuple_args RPAREN */
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
            81 /* tuple_args ::= expr COMMA expr */
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
            82 /* tuple_args ::= expr COMMA tuple_args */
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
            83 /* strexpr ::= StrOpen strlist StrClose */
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
            84 /* strlist ::= */
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
            85 /* strlist ::= StrLit strlist */
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
            86 /* strlist ::= ID strlist */
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

