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
const YYNOCODE: i32 = 91;
type YYACTIONTYPE = u8;
const YYWILDCARD: YYCODETYPE = 1;
enum YYMinorType {
    YY0,
    YY4(String),
    YY28(TokenLoc),
    YY44(Val),
    YY57(TokenData<String>),
    YY135(Type),
    YY140(i64),
    YY157(Ast),
}
const YYNSTATE: i32 = 147;
const YYNRULE: i32 = 81;
const YYERRORSYMBOL: i32 = 0;

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
    VOID, //49
    DollarQuestion, //50
    True, //51
    False, //52
    SquareL, //53
    SquareR, //54
    StrOpen, //55
    StrClose, //56
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
pub const TOKEN_VOID: i32 = 49;
pub const TOKEN_DollarQuestion: i32 = 50;
pub const TOKEN_True: i32 = 51;
pub const TOKEN_False: i32 = 52;
pub const TOKEN_SquareL: i32 = 53;
pub const TOKEN_SquareR: i32 = 54;
pub const TOKEN_StrOpen: i32 = 55;
pub const TOKEN_StrClose: i32 = 56;
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
        Token::COMMA(x) => YYMinorType::YY28(x),
        Token::HASHTAG(x) => YYMinorType::YY57(x),
        Token::ID(x) => YYMinorType::YY4(x),
        Token::INT(x) => YYMinorType::YY140(x),
        Token::NEWLINE(x) => YYMinorType::YY28(x),
        Token::PLUS(x) => YYMinorType::YY28(x),
        Token::StrLit(x) => YYMinorType::YY4(x),
        Token::TYPE_ID(x) => YYMinorType::YY4(x),
        _ => YYMinorType::YY0
  }
}
const YY_ACTTAB_COUNT: i32 = 603;
const YY_ACTION: [YYACTIONTYPE; 603] = [
 /*     0 */   128,   90,  131,   33,   70,   35,  108,  110,  109,  122,
 /*    10 */    23,   22,  121,   94,  120,   24,  127,  101,  110,  109,
 /*    20 */    88,   95,   12,  139,  103,    7,  147,   96,  148,    4,
 /*    30 */   142,   91,   87,   82,   10,  127,    2,   47,  128,   90,
 /*    40 */   131,   34,   76,  126,    8,   32,  133,  132,  130,  129,
 /*    50 */     6,    3,   40,   24,   66,   49,    1,  106,  105,  104,
 /*    60 */    12,   86,  121,    7,  120,   83,  127,   85,  142,   91,
 /*    70 */    87,   82,   10,   38,   29,   47,   33,   39,  117,  127,
 /*    80 */    76,  135,    8,  123,  133,  132,  130,  129,    6,    9,
 /*    90 */    40,   26,   27,  127,   29,  127,  114,  113,   81,   20,
 /*   100 */    19,   21,  136,   11,   18,   17,   14,   13,   16,   15,
 /*   110 */    25,   23,   22,   27,  137,  112,   88,   46,   32,   45,
 /*   120 */    20,   19,   21,  136,   43,   18,   17,   14,   13,   16,
 /*   130 */    15,   25,   23,   22,    5,  116,   42,   75,   30,   27,
 /*   140 */    41,   37,   97,  111,   79,   44,   20,   19,   21,  136,
 /*   150 */    78,   18,   17,   14,   13,   16,   15,   25,   23,   22,
 /*   160 */    27,   36,   31,  128,  134,  131,   77,   20,   19,   21,
 /*   170 */   136,  102,   18,   17,   14,   13,   16,   15,   25,   23,
 /*   180 */    22,   73,   74,  100,   89,   27,  125,  124,   28,   99,
 /*   190 */   107,   88,   20,   19,   21,  136,   72,   18,   17,   14,
 /*   200 */    13,   16,   15,   25,   23,   22,  128,   90,  131,  133,
 /*   210 */   132,  130,  129,  230,  230,   40,  230,  230,  230,  230,
 /*   220 */   230,   24,  230,  230,  230,   70,   48,  230,   12,   27,
 /*   230 */   230,    7,  138,  121,   84,  120,  230,  127,   21,  136,
 /*   240 */   230,   18,   17,   14,   13,   16,   15,   25,   23,   22,
 /*   250 */     8,   27,  133,  132,  130,  129,    6,  230,   40,  128,
 /*   260 */    90,  131,  230,  230,  230,   14,   13,   16,   15,   25,
 /*   270 */    23,   22,  230,   27,   24,  230,  230,  230,  230,  230,
 /*   280 */   230,   12,  230,  230,    7,  230,  230,  228,  228,  228,
 /*   290 */   228,   25,   23,   22,  230,  230,  230,  230,  230,  230,
 /*   300 */   230,  230,  230,    8,  230,  133,  132,  130,  129,    6,
 /*   310 */   230,   40,  229,   93,  230,  230,   92,  230,  230,  230,
 /*   320 */   230,  230,  141,  230,  230,  140,  230,  145,  144,  143,
 /*   330 */    70,   53,  230,  230,  146,  230,  230,   92,  121,  230,
 /*   340 */   120,  230,  127,  141,  230,  230,  140,  230,  145,  144,
 /*   350 */   143,   70,   53,  230,  230,  230,  230,  230,  230,  121,
 /*   360 */   230,  120,  230,  127,  230,   80,  230,  230,   92,  230,
 /*   370 */   230,  230,  230,  230,  141,  230,  230,  140,  230,  145,
 /*   380 */   144,  143,   70,   53,  230,   27,  230,  230,  230,  230,
 /*   390 */   121,  230,  120,  230,  127,  136,  230,   18,   17,   14,
 /*   400 */    13,   16,   15,   25,   23,   22,   70,   48,  230,  230,
 /*   410 */   230,  230,  230,  230,  121,  115,  120,  230,  127,   70,
 /*   420 */    35,  230,  230,  230,   98,   66,   50,  121,  230,  120,
 /*   430 */   230,  127,  230,  121,  230,  120,   71,  127,   70,   49,
 /*   440 */   230,  230,  230,  230,  230,  230,  121,  230,  120,  230,
 /*   450 */   127,   70,   67,  230,  230,  230,   70,   55,  230,  121,
 /*   460 */   230,  120,  230,  127,  121,  230,  120,  230,  127,   70,
 /*   470 */    68,  230,   70,   59,  230,   70,  119,  121,  230,  120,
 /*   480 */   121,  127,  120,  121,  127,  120,  230,  127,  230,  230,
 /*   490 */   230,   70,  118,  230,  230,  230,   70,   58,  230,  121,
 /*   500 */   230,  120,  230,  127,  121,  230,  120,  230,  127,  230,
 /*   510 */    70,   57,  230,   70,   56,  230,   70,   65,  121,  230,
 /*   520 */   120,  121,  127,  120,  121,  127,  120,  230,  127,   70,
 /*   530 */    64,  230,  230,  230,   70,   63,  230,  121,  230,  120,
 /*   540 */   230,  127,  121,  230,  120,  230,  127,   70,   62,  230,
 /*   550 */    70,   61,  230,   70,   60,  121,  230,  120,  121,  127,
 /*   560 */   120,  121,  127,  120,  230,  127,  230,  230,   70,   69,
 /*   570 */   230,  230,  230,   70,   54,  230,  121,  230,  120,  230,
 /*   580 */   127,  121,  230,  120,  230,  127,  230,   70,   52,  230,
 /*   590 */    70,   51,  230,  230,  230,  121,  230,  120,  121,  127,
 /*   600 */   120,  230,  127,
];
const YY_LOOKAHEAD: [YYCODETYPE; 603] = [
 /*     0 */     3,    4,    5,    4,   76,   77,   64,   65,   66,   81,
 /*    10 */    26,   27,   84,   32,   86,   18,   88,   64,   65,   66,
 /*    20 */    38,   31,   25,   76,    9,   28,    0,   30,    0,   47,
 /*    30 */    33,   34,   35,   36,   37,   88,    6,   40,    3,    4,
 /*    40 */     5,    3,   45,   56,   47,   46,   49,   50,   51,   52,
 /*    50 */    53,   28,   55,   18,   76,   77,    6,   42,   43,   44,
 /*    60 */    25,    4,   84,   28,   86,   87,   88,   76,   33,   34,
 /*    70 */    35,   36,   37,    4,    2,   40,    4,    8,   87,   88,
 /*    80 */    45,   76,   47,   76,   49,   50,   51,   52,   53,   11,
 /*    90 */    55,   21,    7,   88,    2,   88,   54,   29,    4,   14,
 /*   100 */    15,   16,   17,   21,   19,   20,   21,   22,   23,   24,
 /*   110 */    25,   26,   27,    7,   29,   39,   38,   28,   46,   29,
 /*   120 */    14,   15,   16,   17,    2,   19,   20,   21,   22,   23,
 /*   130 */    24,   25,   26,   27,    2,   29,   28,    4,   29,    7,
 /*   140 */     2,   48,   29,   70,    4,    4,   14,   15,   16,   17,
 /*   150 */    69,   19,   20,   21,   22,   23,   24,   25,   26,   27,
 /*   160 */     7,   41,   79,    3,    4,    5,   79,   14,   15,   16,
 /*   170 */    17,   69,   19,   20,   21,   22,   23,   24,   25,   26,
 /*   180 */    27,    4,   72,   72,   89,    7,   89,   89,   28,   66,
 /*   190 */    78,   38,   14,   15,   16,   17,   66,   19,   20,   21,
 /*   200 */    22,   23,   24,   25,   26,   27,    3,    4,    5,   49,
 /*   210 */    50,   51,   52,   90,   90,   55,   90,   90,   90,   90,
 /*   220 */    90,   18,   90,   90,   90,   76,   77,   90,   25,    7,
 /*   230 */    90,   28,   29,   84,   85,   86,   90,   88,   16,   17,
 /*   240 */    90,   19,   20,   21,   22,   23,   24,   25,   26,   27,
 /*   250 */    47,    7,   49,   50,   51,   52,   53,   90,   55,    3,
 /*   260 */     4,    5,   90,   90,   90,   21,   22,   23,   24,   25,
 /*   270 */    26,   27,   90,    7,   18,   90,   90,   90,   90,   90,
 /*   280 */    90,   25,   90,   90,   28,   90,   90,   21,   22,   23,
 /*   290 */    24,   25,   26,   27,   90,   90,   90,   90,   90,   90,
 /*   300 */    90,   90,   90,   47,   90,   49,   50,   51,   52,   53,
 /*   310 */    90,   55,   58,   59,   90,   90,   62,   90,   90,   90,
 /*   320 */    90,   90,   68,   90,   90,   71,   90,   73,   74,   75,
 /*   330 */    76,   77,   90,   90,   59,   90,   90,   62,   84,   90,
 /*   340 */    86,   90,   88,   68,   90,   90,   71,   90,   73,   74,
 /*   350 */    75,   76,   77,   90,   90,   90,   90,   90,   90,   84,
 /*   360 */    90,   86,   90,   88,   90,   59,   90,   90,   62,   90,
 /*   370 */    90,   90,   90,   90,   68,   90,   90,   71,   90,   73,
 /*   380 */    74,   75,   76,   77,   90,    7,   90,   90,   90,   90,
 /*   390 */    84,   90,   86,   90,   88,   17,   90,   19,   20,   21,
 /*   400 */    22,   23,   24,   25,   26,   27,   76,   77,   90,   90,
 /*   410 */    90,   90,   90,   90,   84,   85,   86,   90,   88,   76,
 /*   420 */    77,   90,   90,   90,   81,   76,   77,   84,   90,   86,
 /*   430 */    90,   88,   90,   84,   90,   86,   87,   88,   76,   77,
 /*   440 */    90,   90,   90,   90,   90,   90,   84,   90,   86,   90,
 /*   450 */    88,   76,   77,   90,   90,   90,   76,   77,   90,   84,
 /*   460 */    90,   86,   90,   88,   84,   90,   86,   90,   88,   76,
 /*   470 */    77,   90,   76,   77,   90,   76,   77,   84,   90,   86,
 /*   480 */    84,   88,   86,   84,   88,   86,   90,   88,   90,   90,
 /*   490 */    90,   76,   77,   90,   90,   90,   76,   77,   90,   84,
 /*   500 */    90,   86,   90,   88,   84,   90,   86,   90,   88,   90,
 /*   510 */    76,   77,   90,   76,   77,   90,   76,   77,   84,   90,
 /*   520 */    86,   84,   88,   86,   84,   88,   86,   90,   88,   76,
 /*   530 */    77,   90,   90,   90,   76,   77,   90,   84,   90,   86,
 /*   540 */    90,   88,   84,   90,   86,   90,   88,   76,   77,   90,
 /*   550 */    76,   77,   90,   76,   77,   84,   90,   86,   84,   88,
 /*   560 */    86,   84,   88,   86,   90,   88,   90,   90,   76,   77,
 /*   570 */    90,   90,   90,   76,   77,   90,   84,   90,   86,   90,
 /*   580 */    88,   84,   90,   86,   90,   88,   90,   76,   77,   90,
 /*   590 */    76,   77,   90,   90,   90,   84,   90,   86,   84,   88,
 /*   600 */    86,   90,   88,
];
const YY_SHIFT_USE_DFLT: i32 = -20;
const YY_SHIFT_COUNT: i32 = 96;
const YY_SHIFT_MIN: i32 = -19;
const YY_SHIFT_MAX: i32 = 378;
const YY_SHIFT_OFST: [i16; 97] = [
 /*     0 */    -3,   35,   35,  203,  256,  256,  256,  256,  256,  256,
 /*    10 */   256,  256,  256,  256,  256,  256,  256,  256,  256,  256,
 /*    20 */   256,  256,  256,  256,  256,  256,  256,  256,  256,  160,
 /*    30 */    78,   78,  160,  160,  160,  153,   15,  -18,   69,   69,
 /*    40 */    69,  177,  177,  141,  120,  120,  141,  140,  132,  106,
 /*    50 */    85,  178,  178,  178,  178,  178,  222,  222,  378,  378,
 /*    60 */   266,  266,  266,  266,  244,  244,   72,  -16,  -16,  -16,
 /*    70 */    -1,  113,   93,  138,  109,  108,  133,  122,   90,   89,
 /*    80 */    76,   82,   94,   68,   42,   92,   70,   57,   50,  -13,
 /*    90 */    23,   38,   30,   28,   26,  -19,  -10,
];
const YY_REDUCE_USE_DFLT: i32 = -73;
const YY_REDUCE_COUNT: i32 = 47;
const YY_REDUCE_MIN: i32 = -72;
const YY_REDUCE_MAX: i32 = 514;
const YY_REDUCE_OFST: [i16; 48] = [
 /*     0 */   254,  306,  275,  349,  343,  330,  149,  -22,  -72,  514,
 /*    10 */   511,  497,  492,  477,  474,  471,  458,  453,  440,  437,
 /*    20 */   434,  420,  415,  399,  396,  393,  380,  375,  362,   -9,
 /*    30 */   -47,  -58,    7,    5,  -53,  130,  112,  123,   98,   97,
 /*    40 */    95,  111,  110,  102,   87,   83,   81,   73,
];
const YY_DEFAULT: [YYACTIONTYPE; 147] = [
 /*     0 */   149,  149,  149,  228,  228,  218,  218,  228,  228,  228,
 /*    10 */   228,  228,  228,  228,  228,  228,  228,  228,  228,  228,
 /*    20 */   228,  228,  228,  228,  228,  228,  228,  228,  228,  228,
 /*    30 */   228,  228,  228,  228,  228,  228,  228,  228,  225,  225,
 /*    40 */   225,  178,  178,  168,  171,  171,  168,  228,  219,  228,
 /*    50 */   228,  164,  161,  160,  159,  158,  200,  199,  198,  191,
 /*    60 */   206,  205,  204,  203,  202,  201,  207,  194,  195,  193,
 /*    70 */   207,  228,  228,  179,  228,  228,  228,  169,  228,  228,
 /*    80 */   228,  228,  228,  228,  228,  222,  228,  228,  228,  228,
 /*    90 */   209,  228,  228,  228,  228,  228,  228,  183,  188,  187,
 /*   100 */   180,  177,  170,  176,  175,  174,  173,  172,  167,  163,
 /*   110 */   162,  166,  165,  221,  217,  220,  208,  223,  197,  196,
 /*   120 */   190,  189,  186,  185,  227,  226,  224,  216,  215,  214,
 /*   130 */   213,  212,  211,  210,  209,  184,  192,  182,  181,  157,
 /*   140 */   156,  155,  154,  153,  152,  151,  150,
];

/* TMPL: fallback tokens */

const YY_FALLBACK: [i32; 0] = [
];

/* TMPL: symbol names */


/* TMPL: rules */


/* TMPL: destructors */


/* TMPL: stack-overflow */


/* TMPL: stack-overflow */

const YY_RULE_INFO: [YYCODETYPE; 81] = [
  58,
  58,
  59,
  59,
  62,
  62,
  62,
  62,
  62,
  62,
  75,
  73,
  73,
  74,
  74,
  64,
  64,
  65,
  66,
  68,
  70,
  69,
  69,
  69,
  79,
  79,
  78,
  78,
  78,
  78,
  71,
  72,
  72,
  72,
  77,
  77,
  77,
  77,
  77,
  77,
  81,
  81,
  77,
  77,
  77,
  77,
  77,
  77,
  77,
  77,
  77,
  77,
  77,
  77,
  77,
  77,
  77,
  77,
  77,
  77,
  77,
  76,
  76,
  76,
  76,
  76,
  76,
  76,
  76,
  76,
  84,
  85,
  85,
  85,
  86,
  87,
  87,
  88,
  89,
  89,
  89,
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
 YYMinorType::YY157(yyres)
}
            ,
            1 /* program ::= stmts */
            => 
{
let yyres :  Ast ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY44(yy0),) => {

	if list::is_empty(&yy0) {
		panic!("null program");
	}
	// ignore yyres, it doesn't really go anywhere for program
	yyres = Ast::Nothing;
	// we're done, so put yy0 in extra
	self.extra = Ok(Ast::ReplRoot(yy0));

},    _ => unreachable!() };
 YYMinorType::YY157(yyres)
}
            ,
            2 /* stmts ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

	yyres = sexpr::new(SexprType::BlockExpr, list::empty());

} };
 YYMinorType::YY44(yyres)
}
            ,
            3 /* stmts ::= stmt NEWLINE stmts */
          | 73 /* list_items ::= expr COMMA list_items */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy2),) => {

	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            4 /* stmt ::= let_stmt */
          | 6 /* stmt ::= fail_stmt */
          | 8 /* stmt ::= func_stmt */
          | 9 /* stmt ::= macro_stmt */
          | 42 /* expr ::= list */
          | 43 /* expr ::= tuple */
          | 60 /* expr ::= term */
          | 69 /* term ::= strexpr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY44(yy0),) => {
 yyres = yy0; 
},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            5 /* stmt ::= expr_stmt */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY44(yy0),) => {

    yyres = yy0;

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            7 /* stmt ::= DT */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Void; 
} };
 YYMinorType::YY44(yyres)
}
            ,
            10 /* fail_stmt ::= FAIL HASHTAG term */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY57(yy1),YYMinorType::YY44(yy2),) => {

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
 YYMinorType::YY44(yyres)
}
            ,
            11 /* let_stmt ::= Let ID EQ expr */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY4(yy1),YYMinorType::YY44(yy3),) => {

	let bind = list::cons(Val::new_str(yy1), list::singleton(yy3));
	yyres = sexpr::new(SexprType::Bind, bind);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            12 /* let_stmt ::= Fork ID EQ expr */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY4(yy1),YYMinorType::YY44(yy3),) => {

	let bind = list::cons(Val::new_str(yy1), list::singleton(yy3));
	yyres = sexpr::new(SexprType::Fork, bind);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            13 /* expr_stmt ::= expr */
          | 15 /* block ::= arrow_block */
          | 16 /* block ::= curly_block */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY44(yy0),) => {

	yyres = yy0;

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            14 /* expr_stmt ::= DollarGT expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY44(yy1),) => {
 yyres = yy1; 
},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            17 /* arrow_block ::= BLOCKARROW expr */
          | 39 /* expr ::= IF if_expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY44(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            18 /* curly_block ::= CurlyL NEWLINE stmts CurlyR */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp2.minor,) {
 (YYMinorType::YY44(yy2),) => {

	yyres = yy2;

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            19 /* func_stmt ::= Func dfunc_1 */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY44(yy1),) => {

	/*
	const Val *plist = Val::list(p);
	const Val *mcase = Val::tuple2(plist, C);
	const list *mblk = list::singleton(mcase);

	Val *func = Val::fexpr(mblk, T);
	yyres = Val::tuple2(yy1, func, LET_ASSIGN);
	*/
	yyres = Val::Sexpr(SexprType::DefFunc, Box::new(yy1));

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            20 /* dfunc_1 ::= ID LPAREN dfunc_args RPAREN opt_typex block */
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
 (YYMinorType::YY4(yy0),YYMinorType::YY44(yy2),YYMinorType::YY135(yy4),YYMinorType::YY44(yy5),) => {

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
 YYMinorType::YY44(yyres)
}
            ,
            21 /* dfunc_args ::= */
          | 71 /* list_items ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

	yyres = list::empty();

} };
 YYMinorType::YY44(yyres)
}
            ,
            22 /* dfunc_args ::= ID opt_typex */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY4(yy0),YYMinorType::YY135(yy1),) => {

	yyres = list::singleton(sexpr::id_with_type(yy0, yy1));

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            23 /* dfunc_args ::= ID opt_typex COMMA dfunc_args */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY4(yy0),YYMinorType::YY135(yy1),YYMinorType::YY44(yy3),) => {

	yyres = list::cons(sexpr::id_with_type(yy0, yy1), yy3);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            24 /* opt_typex ::= */
            => 
{
let yyres :  Type ;
match () {
 () => {

	yyres = Type::AnonVar;

} };
 YYMinorType::YY135(yyres)
}
            ,
            25 /* opt_typex ::= COLON typex */
            => 
{
let yyres :  Type ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY135(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY135(yyres)
}
            ,
            26 /* typex ::= TYPE_INT */
            => 
{
let yyres :  Type ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Type::Int;

} };
 YYMinorType::YY135(yyres)
}
            ,
            27 /* typex ::= TYPE_STR */
            => 
{
let yyres :  Type ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Type::Str;

} };
 YYMinorType::YY135(yyres)
}
            ,
            28 /* typex ::= TYPE_BOOL */
            => 
{
let yyres :  Type ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Type::Bool;

} };
 YYMinorType::YY135(yyres)
}
            ,
            29 /* typex ::= TYPE_ID */
            => 
{
let yyres :  Type ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY4(yy0),) => {

	yyres = Type::User(yy0);

},    _ => unreachable!() };
 YYMinorType::YY135(yyres)
}
            ,
            30 /* macro_stmt ::= MACRO ID LPAREN macro_args RPAREN block */
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
 (YYMinorType::YY4(yy1),YYMinorType::YY44(yy3),YYMinorType::YY44(yy5),) => {

    yyres = sexpr::new(SexprType::DefMacro,
        list::cons(Val::id(yy1),
        list::cons(yy3,
        list::cons(yy5,
        Val::Nil
    ))));

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            31 /* macro_args ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

    yyres = Val::Nil;

} };
 YYMinorType::YY44(yyres)
}
            ,
            32 /* macro_args ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY4(yy0),) => {

    yyres = list::singleton(Val::id(yy0));

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            33 /* macro_args ::= ID COMMA macro_args */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY4(yy0),YYMinorType::YY44(yy2),) => {

    yyres = list::cons(Val::id(yy0), yy2);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            34 /* expr ::= ID LPAREN RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY4(yy0),) => {

	verbose_out!("zero param function call!");
	yyres = sexpr::call(yy0, Val::Nil);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            35 /* expr ::= ID LPAREN expr RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY4(yy0),YYMinorType::YY44(yy2),) => {

	verbose_out!("one param function call!");
	let args = list::singleton(yy2);
	yyres = sexpr::call(yy0, args);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            36 /* expr ::= ID LPAREN tuple_args RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY4(yy0),YYMinorType::YY44(yy2),) => {

	verbose_out!("multi param function call!");
	yyres = sexpr::call(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            37 /* expr ::= term ID term */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY44(yy0),YYMinorType::YY4(yy1),YYMinorType::YY44(yy2),) => {

	/* yyres = Val::binaryop(yy0, yy1, yy2); */
	yyres = Val::Void;

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
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
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy2),) => {

	/* yyres = Val::binaryop(yy0, yy2, D); */
	yyres = Val::Void;

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
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
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy1),YYMinorType::YY44(yy3),) => {

	yyres = sexpr::ifexpr(yy0, yy1, yy3);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
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
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy1),YYMinorType::YY44(yy4),) => {

	yyres = sexpr::ifexpr(yy0, yy1, yy4);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            44 /* expr ::= NOT expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY44(yy1),) => {

	yyres = sexpr::call("bool_not".to_string(), list::singleton(yy1));

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            45 /* expr ::= expr ConcatNewline */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY44(yy0),) => {

	let newline = Val::Str(Arc::new("\n".to_string()));
	let args = list::cons(yy0, list::singleton(newline));
	yyres = sexpr::new(SexprType::StrExpr, args)

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            46 /* expr ::= MINUS expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY44(yy1),) => {

	yyres = sexpr::call("negate".to_string(), list::singleton(yy1));

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            47 /* expr ::= expr PLUS expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy2),) => {

    verbose_out!("parse PLUS expr");
	yyres = sexpr::binaryop("int_add".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            48 /* expr ::= expr MINUS expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy2),) => {

	yyres = sexpr::binaryop("int_sub".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            49 /* expr ::= expr TIMES expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy2),) => {

	yyres = sexpr::binaryop("int_mult".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            50 /* expr ::= expr DIVIDE expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy2),) => {

	yyres = sexpr::binaryop("int_div".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            51 /* expr ::= expr AND expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy2),) => {

	yyres = sexpr::binaryop("and".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            52 /* expr ::= expr OR expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy2),) => {

	yyres = sexpr::binaryop("or".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            53 /* expr ::= expr XOR expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy2),) => {

	yyres = sexpr::binaryop("xor".to_string(),yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            54 /* expr ::= expr LT expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy2),) => {

	yyres = sexpr::binaryop("less_than".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            55 /* expr ::= expr LTEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy2),) => {

	yyres = sexpr::binaryop("less_than_equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            56 /* expr ::= expr GT expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy2),) => {

	yyres = sexpr::binaryop("greater_than".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            57 /* expr ::= expr GTEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy2),) => {

	yyres = sexpr::binaryop("greater_than_equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            58 /* expr ::= expr EQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy2),) => {

	yyres = sexpr::binaryop("equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            59 /* expr ::= expr NEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy2),) => {

	let eq = sexpr::binaryop("equal".to_string(), yy0, yy2);
	yyres = sexpr::call("bool_not".to_string(), list::singleton(eq));

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            61 /* term ::= LPAREN expr RPAREN */
          | 70 /* list ::= SquareL list_items SquareR */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY44(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            62 /* term ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY4(yy0),) => {
 yyres = Val::id(yy0); 
},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            63 /* term ::= VOID */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Val::Void;

} };
 YYMinorType::YY44(yyres)
}
            ,
            64 /* term ::= DollarQuestion */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Val::id("$".to_string());

} };
 YYMinorType::YY44(yyres)
}
            ,
            65 /* term ::= INT */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY140(yy0),) => {

	yyres = Val::Int(yy0);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            66 /* term ::= True */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Bool(true); 
} };
 YYMinorType::YY44(yyres)
}
            ,
            67 /* term ::= False */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Bool(false); 
} };
 YYMinorType::YY44(yyres)
}
            ,
            68 /* term ::= HASHTAG */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY57(yy0),) => {

	yyres = Val::Hashtag(Arc::new(yy0.data));

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            72 /* list_items ::= expr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY44(yy0),) => {

	yyres = list::singleton(yy0);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            74 /* tuple ::= LPAREN tuple_args RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY44(yy1),) => {

	yyres = Val::tuple_from_list(yy1);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            75 /* tuple_args ::= term COMMA term */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy2),) => {

	verbose_out!("base tuple args!");
	yyres = list::cons(yy0, list::singleton(yy2));

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            76 /* tuple_args ::= term COMMA tuple_args */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy2),) => {

	verbose_out!("additional tuple arg!");
	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            77 /* strexpr ::= StrOpen strlist StrClose */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY44(yy1),) => {

	yyres = sexpr::strexpr(yy1);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            78 /* strlist ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

	yyres = Val::Nil;

} };
 YYMinorType::YY44(yyres)
}
            ,
            79 /* strlist ::= StrLit strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY4(yy0),YYMinorType::YY44(yy1),) => {

	yyres = list::cons(Val::new_str(yy0), yy1);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            80 /* strlist ::= ID strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY4(yy0),YYMinorType::YY44(yy1),) => {

	yyres = list::cons(Val::id(yy0), yy1);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
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

