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
const YYNOCODE: i32 = 96;
type YYACTIONTYPE = u16;
const YYWILDCARD: YYCODETYPE = 1;
enum YYMinorType {
    YY0,
    YY14(Val),
    YY74(String),
    YY127(Ast),
    YY130(i64),
    YY137(TokenData<String>),
    YY168(TokenLoc),
    YY185(Type),
}
const YYNSTATE: i32 = 192;
const YYNRULE: i32 = 103;
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
    ELSE( TokenLoc ), //3
    HASHTAG( TokenData<String> ), //4
    ID( String ), //5
    INT( i64 ), //6
    PLUS( TokenLoc ), //7
    SLASH( TokenLoc ), //8
    StrLit( String ), //9
    TYPE_ID( String ), //10
    ASSIGN, //11
    BLOCKARROW, //12
    WHERE, //13
    GIVEN, //14
    OR, //15
    XOR, //16
    AND, //17
    ConcatNewline, //18
    NOT, //19
    EQ, //20
    NEQ, //21
    GT, //22
    GTEQ, //23
    LT, //24
    LTEQ, //25
    MINUS, //26
    TIMES, //27
    MOD, //28
    LPAREN, //29
    RPAREN, //30
    FAILED, //31
    WITH, //32
    CONTEXTID, //33
    DT, //34
    FAIL, //35
    Let, //36
    Fork, //37
    DollarGT, //38
    Func, //39
    DOUBLEDASH, //40
    COLON, //41
    TYPE_INT, //42
    TYPE_STR, //43
    TYPE_BOOL, //44
    TYPE_VOID, //45
    MACRO, //46
    IF, //47
    PIPE, //48
    DOLLAR, //49
    CASE, //50
    MATCH, //51
    True, //52
    False, //53
    UNDERSCORE, //54
    VOID, //55
    DollarQuestion, //56
    SquareL, //57
    SquareR, //58
    StrOpen, //59
    StrClose, //60
}
pub const TOKEN_EOI: i32 = 0;
pub const TOKEN_ANY: i32 = 1;
pub const TOKEN_COMMA: i32 = 2;
pub const TOKEN_ELSE: i32 = 3;
pub const TOKEN_HASHTAG: i32 = 4;
pub const TOKEN_ID: i32 = 5;
pub const TOKEN_INT: i32 = 6;
pub const TOKEN_PLUS: i32 = 7;
pub const TOKEN_SLASH: i32 = 8;
pub const TOKEN_StrLit: i32 = 9;
pub const TOKEN_TYPE_ID: i32 = 10;
pub const TOKEN_ASSIGN: i32 = 11;
pub const TOKEN_BLOCKARROW: i32 = 12;
pub const TOKEN_WHERE: i32 = 13;
pub const TOKEN_GIVEN: i32 = 14;
pub const TOKEN_OR: i32 = 15;
pub const TOKEN_XOR: i32 = 16;
pub const TOKEN_AND: i32 = 17;
pub const TOKEN_ConcatNewline: i32 = 18;
pub const TOKEN_NOT: i32 = 19;
pub const TOKEN_EQ: i32 = 20;
pub const TOKEN_NEQ: i32 = 21;
pub const TOKEN_GT: i32 = 22;
pub const TOKEN_GTEQ: i32 = 23;
pub const TOKEN_LT: i32 = 24;
pub const TOKEN_LTEQ: i32 = 25;
pub const TOKEN_MINUS: i32 = 26;
pub const TOKEN_TIMES: i32 = 27;
pub const TOKEN_MOD: i32 = 28;
pub const TOKEN_LPAREN: i32 = 29;
pub const TOKEN_RPAREN: i32 = 30;
pub const TOKEN_FAILED: i32 = 31;
pub const TOKEN_WITH: i32 = 32;
pub const TOKEN_CONTEXTID: i32 = 33;
pub const TOKEN_DT: i32 = 34;
pub const TOKEN_FAIL: i32 = 35;
pub const TOKEN_Let: i32 = 36;
pub const TOKEN_Fork: i32 = 37;
pub const TOKEN_DollarGT: i32 = 38;
pub const TOKEN_Func: i32 = 39;
pub const TOKEN_DOUBLEDASH: i32 = 40;
pub const TOKEN_COLON: i32 = 41;
pub const TOKEN_TYPE_INT: i32 = 42;
pub const TOKEN_TYPE_STR: i32 = 43;
pub const TOKEN_TYPE_BOOL: i32 = 44;
pub const TOKEN_TYPE_VOID: i32 = 45;
pub const TOKEN_MACRO: i32 = 46;
pub const TOKEN_IF: i32 = 47;
pub const TOKEN_PIPE: i32 = 48;
pub const TOKEN_DOLLAR: i32 = 49;
pub const TOKEN_CASE: i32 = 50;
pub const TOKEN_MATCH: i32 = 51;
pub const TOKEN_True: i32 = 52;
pub const TOKEN_False: i32 = 53;
pub const TOKEN_UNDERSCORE: i32 = 54;
pub const TOKEN_VOID: i32 = 55;
pub const TOKEN_DollarQuestion: i32 = 56;
pub const TOKEN_SquareL: i32 = 57;
pub const TOKEN_SquareR: i32 = 58;
pub const TOKEN_StrOpen: i32 = 59;
pub const TOKEN_StrClose: i32 = 60;
#[inline]
fn token_major(t: &Token) -> i32 {
    match t {
        &Token::EOI => 0,
        &Token::ANY => TOKEN_ANY,
        &Token::COMMA(_) => TOKEN_COMMA,
        &Token::ELSE(_) => TOKEN_ELSE,
        &Token::HASHTAG(_) => TOKEN_HASHTAG,
        &Token::ID(_) => TOKEN_ID,
        &Token::INT(_) => TOKEN_INT,
        &Token::PLUS(_) => TOKEN_PLUS,
        &Token::SLASH(_) => TOKEN_SLASH,
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
        &Token::EQ => TOKEN_EQ,
        &Token::NEQ => TOKEN_NEQ,
        &Token::GT => TOKEN_GT,
        &Token::GTEQ => TOKEN_GTEQ,
        &Token::LT => TOKEN_LT,
        &Token::LTEQ => TOKEN_LTEQ,
        &Token::MINUS => TOKEN_MINUS,
        &Token::TIMES => TOKEN_TIMES,
        &Token::MOD => TOKEN_MOD,
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
        &Token::Func => TOKEN_Func,
        &Token::DOUBLEDASH => TOKEN_DOUBLEDASH,
        &Token::COLON => TOKEN_COLON,
        &Token::TYPE_INT => TOKEN_TYPE_INT,
        &Token::TYPE_STR => TOKEN_TYPE_STR,
        &Token::TYPE_BOOL => TOKEN_TYPE_BOOL,
        &Token::TYPE_VOID => TOKEN_TYPE_VOID,
        &Token::MACRO => TOKEN_MACRO,
        &Token::IF => TOKEN_IF,
        &Token::PIPE => TOKEN_PIPE,
        &Token::DOLLAR => TOKEN_DOLLAR,
        &Token::CASE => TOKEN_CASE,
        &Token::MATCH => TOKEN_MATCH,
        &Token::True => TOKEN_True,
        &Token::False => TOKEN_False,
        &Token::UNDERSCORE => TOKEN_UNDERSCORE,
        &Token::VOID => TOKEN_VOID,
        &Token::DollarQuestion => TOKEN_DollarQuestion,
        &Token::SquareL => TOKEN_SquareL,
        &Token::SquareR => TOKEN_SquareR,
        &Token::StrOpen => TOKEN_StrOpen,
        &Token::StrClose => TOKEN_StrClose,
    }
}
#[inline]
fn token_minor(t: Token) -> YYMinorType {
  match t {
        Token::COMMA(x) => YYMinorType::YY168(x),
        Token::ELSE(x) => YYMinorType::YY168(x),
        Token::HASHTAG(x) => YYMinorType::YY137(x),
        Token::ID(x) => YYMinorType::YY74(x),
        Token::INT(x) => YYMinorType::YY130(x),
        Token::PLUS(x) => YYMinorType::YY168(x),
        Token::SLASH(x) => YYMinorType::YY168(x),
        Token::StrLit(x) => YYMinorType::YY74(x),
        Token::TYPE_ID(x) => YYMinorType::YY74(x),
        _ => YYMinorType::YY0
  }
}
const YY_ACTTAB_COUNT: i32 = 753;
const YY_ACTION: [YYACTIONTYPE; 753] = [
 /*     0 */   172,  116,  175,   90,   67,  110,  163,  155,  120,  153,
 /*    10 */    92,  152,  107,  171,    1,   26,   35,  119,   90,   43,
 /*    20 */    65,  163,   14,  192,  153,    8,  152,  121,  171,  193,
 /*    30 */   187,  117,  113,  106,   12,  104,   89,  163,  109,  172,
 /*    40 */   116,  175,   98,    3,  156,   37,   66,   28,  174,  173,
 /*    50 */    39,  177,  176,    6,   26,   51,   25,   90,   71,   32,
 /*    60 */    25,   14,   38,  153,    8,  152,  146,  171,    4,  187,
 /*    70 */   117,  113,  106,   12,  104,   27,   24,  170,   29,   27,
 /*    80 */    24,   98,    3,  166,  112,   66,   28,  174,  173,   36,
 /*    90 */   177,  176,    6,    7,   51,   90,   70,  135,   32,   25,
 /*   100 */    49,  153,  108,  152,   50,  171,   22,   21,   23,  180,
 /*   110 */    30,   16,   15,   18,   17,   20,   19,   29,   27,   24,
 /*   120 */     7,  181,    1,  165,   35,   32,   25,  101,  147,  139,
 /*   130 */   138,  137,  136,   22,   21,   23,  180,  100,   16,   15,
 /*   140 */    18,   17,   20,   19,   29,   27,   24,    7,  149,   47,
 /*   150 */   154,  145,   32,   25,  183,  105,   13,   11,  159,  162,
 /*   160 */    22,   21,   23,  180,  171,   16,   15,   18,   17,   20,
 /*   170 */    19,   29,   27,   24,    5,  179,  103,   63,  142,   32,
 /*   180 */    25,  167,   34,  157,   62,  171,  131,   22,   21,   23,
 /*   190 */   180,  171,   16,   15,   18,   17,   20,   19,   29,   27,
 /*   200 */    24,   32,   25,  159,  162,  161,  160,  158,  141,   22,
 /*   210 */    21,   23,  180,   60,   16,   15,   18,   17,   20,   19,
 /*   220 */    29,   27,   24,   58,  149,   32,   25,   34,   97,   59,
 /*   230 */     1,  133,   57,   22,   21,   23,  180,  130,   16,   15,
 /*   240 */    18,   17,   20,   19,   29,   27,   24,   32,   25,  127,
 /*   250 */   161,  160,  158,  114,  122,   22,   21,   23,  180,    1,
 /*   260 */    16,   15,   18,   17,   20,   19,   29,   27,   24,   31,
 /*   270 */    64,  164,   90,   70,   39,  102,   32,   25,  153,  148,
 /*   280 */   152,   61,  171,   46,   22,   21,   23,  180,   39,   16,
 /*   290 */    15,   18,   17,   20,   19,   29,   27,   24,   54,  172,
 /*   300 */   116,  175,   40,   99,  134,   90,   69,  296,  118,    2,
 /*   310 */    96,  153,  186,  152,   26,  171,  185,   94,  184,   95,
 /*   320 */   129,   14,  132,   47,    8,  190,  189,  188,   90,   73,
 /*   330 */   126,   90,   68,   10,  153,    9,  152,  153,  171,  152,
 /*   340 */    91,  171,  123,  115,  125,   66,   28,  174,  173,  124,
 /*   350 */   177,  176,    6,  169,   51,   52,  172,  116,  175,  168,
 /*   360 */    90,   86,   93,  128,  140,   53,  153,  111,  152,   48,
 /*   370 */   171,   26,   56,   55,  297,  297,   90,   45,   14,   32,
 /*   380 */    25,    8,  153,  297,  152,  297,  171,  297,  297,   23,
 /*   390 */   180,  297,   16,   15,   18,   17,   20,   19,   29,   27,
 /*   400 */    24,  297,   66,   28,  174,  173,  297,  177,  176,    6,
 /*   410 */   297,   51,  172,  116,  175,  297,  297,  297,  297,  297,
 /*   420 */   297,  297,  191,    2,  297,  297,  186,   26,  297,  297,
 /*   430 */   185,  297,  184,  297,   14,  297,  297,    8,  182,  190,
 /*   440 */   189,  188,   90,   73,  297,   90,   75,  297,  153,  297,
 /*   450 */   152,  153,  171,  152,  297,  171,  297,  297,   66,   28,
 /*   460 */   174,  173,  297,  177,  176,    6,  297,   51,  172,  116,
 /*   470 */   175,  297,  297,  297,   90,   87,  297,  143,    2,  297,
 /*   480 */   153,  186,  152,   26,  171,  185,  297,  184,  297,  297,
 /*   490 */    14,  297,  297,    8,  190,  189,  188,   90,   73,  297,
 /*   500 */   297,  297,  297,  153,  297,  152,  297,  171,  297,  297,
 /*   510 */   297,  297,   10,  297,   66,   28,  174,  173,  297,  177,
 /*   520 */   176,    6,  297,   51,  172,  116,  175,   90,   44,  297,
 /*   530 */    90,  144,  297,  153,  297,  152,  153,  171,  152,   26,
 /*   540 */   171,  297,  297,  297,   90,   79,   14,   32,   25,    8,
 /*   550 */   153,  297,  152,  297,  171,  297,  297,  297,  180,  297,
 /*   560 */    16,   15,   18,   17,   20,   19,   29,   27,   24,  297,
 /*   570 */    66,   28,  174,  173,  297,  177,  176,    6,  297,   51,
 /*   580 */    32,   25,  172,  178,  175,  297,  297,  297,  297,  297,
 /*   590 */   297,  297,  297,  295,  295,  295,  295,   20,   19,   29,
 /*   600 */    27,   24,  297,  297,   90,  151,  297,   33,  297,  297,
 /*   610 */   153,  297,  152,  297,  171,   90,  150,  297,  297,  297,
 /*   620 */   297,  153,  297,  152,  297,  171,  297,  297,  297,  297,
 /*   630 */   174,  173,  297,  177,  176,  297,  297,   51,   90,   78,
 /*   640 */   297,  297,  297,  297,  153,  297,  152,  297,  171,   90,
 /*   650 */    77,  297,   90,   76,  297,  153,  297,  152,  153,  171,
 /*   660 */   152,  297,  171,   90,   85,  297,   90,   84,  297,  153,
 /*   670 */   297,  152,  153,  171,  152,  297,  171,   90,   83,  297,
 /*   680 */    90,   82,  297,  153,  297,  152,  153,  171,  152,  297,
 /*   690 */   171,   90,   81,  297,   90,   80,  297,  153,  297,  152,
 /*   700 */   153,  171,  152,  297,  171,   90,   88,  297,  297,  297,
 /*   710 */   297,  153,  297,  152,  297,  171,  297,   90,   74,  297,
 /*   720 */   297,  297,  297,  153,  297,  152,  297,  171,   90,   72,
 /*   730 */   297,  297,  297,  297,  153,  297,  152,  297,  171,   90,
 /*   740 */    42,  297,   90,   41,  297,  153,  297,  152,  153,  171,
 /*   750 */   152,  297,  171,
];
const YY_LOOKAHEAD: [YYCODETYPE; 753] = [
 /*     0 */     4,    5,    6,   83,   84,   77,   78,   79,   32,   89,
 /*    10 */    75,   91,   92,   93,   12,   19,    2,   33,   83,   84,
 /*    20 */    77,   78,   26,    0,   89,   29,   91,   31,   93,    0,
 /*    30 */    34,   35,   36,   37,   38,   39,   77,   78,   79,    4,
 /*    40 */     5,    6,   46,   47,   30,    5,   50,   51,   52,   53,
 /*    50 */    48,   55,   56,   57,   19,   59,    8,   83,   84,    7,
 /*    60 */     8,   26,    4,   89,   29,   91,   92,   93,   29,   34,
 /*    70 */    35,   36,   37,   38,   39,   27,   28,   60,   26,   27,
 /*    80 */    28,   46,   47,   40,    5,   50,   51,   52,   53,   49,
 /*    90 */    55,   56,   57,    2,   59,   83,   84,   10,    7,    8,
 /*   100 */     5,   89,   90,   91,    9,   93,   15,   16,   17,   18,
 /*   110 */    20,   20,   21,   22,   23,   24,   25,   26,   27,   28,
 /*   120 */     2,   30,   12,   40,    2,    7,    8,   66,   58,   42,
 /*   130 */    43,   44,   45,   15,   16,   17,   18,   76,   20,   21,
 /*   140 */    22,   23,   24,   25,   26,   27,   28,    2,   30,    3,
 /*   150 */    30,   30,    7,    8,   83,    5,   20,   47,    5,    6,
 /*   160 */    15,   16,   17,   18,   93,   20,   21,   22,   23,   24,
 /*   170 */    25,   26,   27,   28,    2,   83,    5,   29,   40,    7,
 /*   180 */     8,   83,   29,   30,   30,   93,   40,   15,   16,   17,
 /*   190 */    18,   93,   20,   21,   22,   23,   24,   25,   26,   27,
 /*   200 */    28,    7,    8,    5,    6,   52,   53,   54,   40,   15,
 /*   210 */    16,   17,   18,    2,   20,   21,   22,   23,   24,   25,
 /*   220 */    26,   27,   28,   30,   30,    7,    8,   29,    5,   29,
 /*   230 */    12,   40,    2,   15,   16,   17,   18,   40,   20,   21,
 /*   240 */    22,   23,   24,   25,   26,   27,   28,    7,    8,   40,
 /*   250 */    52,   53,   54,   88,   30,   15,   16,   17,   18,   12,
 /*   260 */    20,   21,   22,   23,   24,   25,   26,   27,   28,   48,
 /*   270 */    66,   76,   83,   84,   48,   68,    7,    8,   89,   90,
 /*   280 */    91,    5,   93,   41,   15,   16,   17,   18,   48,   20,
 /*   290 */    21,   22,   23,   24,   25,   26,   27,   28,    3,    4,
 /*   300 */     5,    6,   86,   86,   68,   83,   84,   62,   63,   64,
 /*   310 */    72,   89,   67,   91,   19,   93,   71,    5,   73,   66,
 /*   320 */    74,   26,   72,    3,   29,   80,   81,   82,   83,   84,
 /*   330 */    75,   83,   84,   48,   89,   48,   91,   89,   93,   91,
 /*   340 */    92,   93,   88,   94,   66,   50,   51,   52,   53,   66,
 /*   350 */    55,   56,   57,   94,   59,    3,    4,    5,    6,   94,
 /*   360 */    83,   84,   74,   66,   85,   66,   89,   76,   91,   66,
 /*   370 */    93,   19,   66,   66,   95,   95,   83,   84,   26,    7,
 /*   380 */     8,   29,   89,   95,   91,   95,   93,   95,   95,   17,
 /*   390 */    18,   95,   20,   21,   22,   23,   24,   25,   26,   27,
 /*   400 */    28,   95,   50,   51,   52,   53,   95,   55,   56,   57,
 /*   410 */    95,   59,    4,    5,    6,   95,   95,   95,   95,   95,
 /*   420 */    95,   95,   63,   64,   95,   95,   67,   19,   95,   95,
 /*   430 */    71,   95,   73,   95,   26,   95,   95,   29,   30,   80,
 /*   440 */    81,   82,   83,   84,   95,   83,   84,   95,   89,   95,
 /*   450 */    91,   89,   93,   91,   95,   93,   95,   95,   50,   51,
 /*   460 */    52,   53,   95,   55,   56,   57,   95,   59,    4,    5,
 /*   470 */     6,   95,   95,   95,   83,   84,   95,   63,   64,   95,
 /*   480 */    89,   67,   91,   19,   93,   71,   95,   73,   95,   95,
 /*   490 */    26,   95,   95,   29,   80,   81,   82,   83,   84,   95,
 /*   500 */    95,   95,   95,   89,   95,   91,   95,   93,   95,   95,
 /*   510 */    95,   95,   48,   95,   50,   51,   52,   53,   95,   55,
 /*   520 */    56,   57,   95,   59,    4,    5,    6,   83,   84,   95,
 /*   530 */    83,   84,   95,   89,   95,   91,   89,   93,   91,   19,
 /*   540 */    93,   95,   95,   95,   83,   84,   26,    7,    8,   29,
 /*   550 */    89,   95,   91,   95,   93,   95,   95,   95,   18,   95,
 /*   560 */    20,   21,   22,   23,   24,   25,   26,   27,   28,   95,
 /*   570 */    50,   51,   52,   53,   95,   55,   56,   57,   95,   59,
 /*   580 */     7,    8,    4,    5,    6,   95,   95,   95,   95,   95,
 /*   590 */    95,   95,   95,   20,   21,   22,   23,   24,   25,   26,
 /*   600 */    27,   28,   95,   95,   83,   84,   95,   29,   95,   95,
 /*   610 */    89,   95,   91,   95,   93,   83,   84,   95,   95,   95,
 /*   620 */    95,   89,   95,   91,   95,   93,   95,   95,   95,   95,
 /*   630 */    52,   53,   95,   55,   56,   95,   95,   59,   83,   84,
 /*   640 */    95,   95,   95,   95,   89,   95,   91,   95,   93,   83,
 /*   650 */    84,   95,   83,   84,   95,   89,   95,   91,   89,   93,
 /*   660 */    91,   95,   93,   83,   84,   95,   83,   84,   95,   89,
 /*   670 */    95,   91,   89,   93,   91,   95,   93,   83,   84,   95,
 /*   680 */    83,   84,   95,   89,   95,   91,   89,   93,   91,   95,
 /*   690 */    93,   83,   84,   95,   83,   84,   95,   89,   95,   91,
 /*   700 */    89,   93,   91,   95,   93,   83,   84,   95,   95,   95,
 /*   710 */    95,   89,   95,   91,   95,   93,   95,   83,   84,   95,
 /*   720 */    95,   95,   95,   89,   95,   91,   95,   93,   83,   84,
 /*   730 */    95,   95,   95,   95,   89,   95,   91,   95,   93,   83,
 /*   740 */    84,   95,   83,   84,   95,   89,   95,   91,   89,   93,
 /*   750 */    91,   95,   93,
];
const YY_SHIFT_USE_DFLT: i32 = -25;
const YY_SHIFT_COUNT: i32 = 121;
const YY_SHIFT_MIN: i32 = -24;
const YY_SHIFT_MAX: i32 = 578;
const YY_SHIFT_OFST: [i16; 122] = [
 /*     0 */    -4,   35,   35,  464,  408,  520,  520,  520,  520,  352,
 /*    10 */   295,  520,  520,  520,  520,  520,  520,  520,  520,  520,
 /*    20 */   520,  520,  520,  520,  520,  520,  520,  520,  520,  520,
 /*    30 */   520,  520,  520,  520,  153,  198,  578,  578,  578,  198,
 /*    40 */     2,  218,  218,  218,  240,  218,   87,  110,  146,   95,
 /*    50 */    95,   95,  247,  287,  247,  285,  320,  312,  247,  312,
 /*    60 */   276,  242,  242,  276,  226,  247,  221,  118,   91,  194,
 /*    70 */   172,  145,  269,  269,  269,  269,  372,  372,  540,  540,
 /*    80 */   573,  573,  573,  573,   52,   52,   48,   48,   48,   14,
 /*    90 */    40,  224,  209,  197,  230,  191,  193,  200,  223,  211,
 /*   100 */   168,  138,  154,  148,  171,  136,  150,  121,   70,  120,
 /*   110 */   122,   83,   90,   79,   43,   17,   39,   58,   29,   23,
 /*   120 */   -16,  -24,
];
const YY_REDUCE_USE_DFLT: i32 = -81;
const YY_REDUCE_COUNT: i32 = 66;
const YY_REDUCE_MIN: i32 = -80;
const YY_REDUCE_MAX: i32 = 659;
const YY_REDUCE_OFST: [i16; 67] = [
 /*     0 */   245,  414,  359,  -65,  248,  189,   12,  -26,  -80,  293,
 /*    10 */   659,  656,  645,  634,  622,  611,  608,  597,  594,  583,
 /*    20 */   580,  569,  566,  555,  532,  521,  461,  447,  444,  391,
 /*    30 */   362,  293,  277,  222,  -41,  -72,   98,   92,   71,  -57,
 /*    40 */    61,  307,  306,  303,  291,  299,  279,  297,  288,  265,
 /*    50 */   259,  249,  283,  254,  278,  255,  246,  250,  253,  238,
 /*    60 */   236,  217,  216,  207,  195,  204,  165,
];
const YY_DEFAULT: [YYACTIONTYPE; 192] = [
 /*     0 */   194,  194,  194,  295,  295,  285,  285,  295,  295,  295,
 /*    10 */   295,  295,  295,  295,  295,  295,  295,  295,  295,  295,
 /*    20 */   295,  295,  295,  295,  295,  295,  295,  295,  295,  295,
 /*    30 */   295,  295,  295,  295,  295,  295,  295,  295,  295,  295,
 /*    40 */   295,  295,  295,  295,  295,  295,  295,  295,  295,  292,
 /*    50 */   292,  292,  295,  295,  295,  295,  229,  222,  295,  222,
 /*    60 */   211,  214,  214,  211,  243,  295,  295,  295,  295,  295,
 /*    70 */   286,  289,  207,  206,  205,  204,  267,  266,  265,  257,
 /*    80 */   273,  272,  271,  270,  269,  268,  260,  261,  259,  295,
 /*    90 */   274,  295,  295,  295,  223,  295,  295,  295,  295,  212,
 /*   100 */   295,  295,  295,  295,  295,  295,  295,  295,  295,  295,
 /*   110 */   253,  295,  295,  295,  295,  295,  276,  295,  295,  295,
 /*   120 */   295,  295,  235,  240,  239,  232,  231,  227,  230,  228,
 /*   130 */   226,  225,  224,  221,  213,  220,  219,  218,  217,  216,
 /*   140 */   215,  210,  209,  208,  262,  288,  290,  284,  287,  275,
 /*   150 */   264,  263,  256,  255,  252,  254,  251,  250,  249,  248,
 /*   160 */   247,  246,  245,  244,  242,  241,  238,  237,  294,  293,
 /*   170 */   291,  283,  282,  281,  280,  279,  278,  277,  276,  236,
 /*   180 */   258,  234,  233,  203,  202,  201,  200,  199,  198,  197,
 /*   190 */   196,  195,
];

/* TMPL: fallback tokens */

const YY_FALLBACK: [i32; 0] = [
];

/* TMPL: symbol names */


/* TMPL: rules */


/* TMPL: destructors */


/* TMPL: stack-overflow */


/* TMPL: stack-overflow */

const YY_RULE_INFO: [YYCODETYPE; 103] = [
  62,
  62,
  63,
  63,
  64,
  64,
  64,
  64,
  64,
  64,
  64,
  82,
  80,
  80,
  81,
  81,
  66,
  67,
  67,
  68,
  68,
  68,
  86,
  86,
  85,
  85,
  85,
  85,
  85,
  71,
  72,
  72,
  72,
  73,
  73,
  73,
  74,
  74,
  74,
  75,
  75,
  84,
  84,
  84,
  84,
  84,
  84,
  88,
  88,
  84,
  76,
  76,
  77,
  77,
  77,
  77,
  77,
  77,
  78,
  78,
  78,
  79,
  79,
  84,
  84,
  84,
  84,
  84,
  84,
  84,
  84,
  84,
  84,
  84,
  84,
  84,
  84,
  84,
  84,
  84,
  84,
  84,
  84,
  83,
  83,
  83,
  83,
  83,
  83,
  83,
  83,
  83,
  89,
  90,
  90,
  90,
  91,
  92,
  92,
  93,
  94,
  94,
  94,
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
 YYMinorType::YY127(yyres)
}
            ,
            1 /* program ::= stmts */
            => 
{
let yyres :  Ast ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY14(yy0),) => {

	if list::is_empty(&yy0) {
		panic!("null program");
	}
	// ignore yyres, it doesn't really go anywhere for program
	yyres = Ast::Nothing;
	// we're done, so put yy0 in extra
	self.extra = Ok(Ast::ReplRoot(yy0));

},    _ => unreachable!() };
 YYMinorType::YY127(yyres)
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
 YYMinorType::YY14(yyres)
}
            ,
            3 /* stmts ::= stmt stmts */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY14(yy0),YYMinorType::YY14(yy1),) => {

    verbose_out!("found new stmt: {:?}\n", yy0);
	yyres = list::cons(yy0, yy1);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            4 /* stmt ::= let_stmt */
          | 6 /* stmt ::= fail_stmt */
          | 8 /* stmt ::= func_stmt */
          | 9 /* stmt ::= macro_stmt */
          | 10 /* stmt ::= if_stmt */
          | 52 /* pexpr ::= ptuple */
          | 63 /* expr ::= list */
          | 64 /* expr ::= tuple */
          | 82 /* expr ::= term */
          | 91 /* term ::= strexpr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY14(yy0),) => {
 yyres = yy0; 
},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            5 /* stmt ::= expr_stmt */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY14(yy0),) => {

    yyres = yy0;

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
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
 YYMinorType::YY14(yyres)
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
 (YYMinorType::YY137(yy1),YYMinorType::YY14(yy2),) => {

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
 YYMinorType::YY14(yyres)
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
 (YYMinorType::YY74(yy1),YYMinorType::YY14(yy3),) => {

	let letx =
        list::cons(Val::id(yy1),
        list::cons(yy3,
        Val::Nil
        ));
	yyres = sexpr::new(SexprType::Let, letx);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
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
 (YYMinorType::YY74(yy1),YYMinorType::YY14(yy3),) => {

	let bind = list::cons(Val::new_str(yy1), list::singleton(yy3));
	yyres = sexpr::new(SexprType::Fork, bind);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            14 /* expr_stmt ::= expr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY14(yy0),) => {

	yyres = yy0;

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            15 /* expr_stmt ::= DollarGT expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY14(yy1),) => {
 yyres = yy1; 
},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            16 /* block ::= BLOCKARROW stmts */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY14(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            17 /* func_stmt ::= Func ID LPAREN dfunc_args RPAREN opt_typex block DOUBLEDASH */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp6 = self.yystack.pop().unwrap();
let yyp5 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,yyp5.minor,yyp6.minor,) {
 (YYMinorType::YY74(yy1),YYMinorType::YY14(yy3),YYMinorType::YY185(yy5),YYMinorType::YY14(yy6),) => {

	let id = Val::id(yy1);
	let typ = Val::Type(yy5);
	yyres = sexpr::defunc(id, yy3, typ, yy6)

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            18 /* func_stmt ::= Func ID LPAREN dfunc_args RPAREN opt_typex match_case DOUBLEDASH */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp6 = self.yystack.pop().unwrap();
let yyp5 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,yyp5.minor,yyp6.minor,) {
 (YYMinorType::YY74(yy1),YYMinorType::YY14(yy3),YYMinorType::YY185(yy5),YYMinorType::YY14(yy6),) => {

	let id = Val::id(yy1);
	let typ = Val::Type(yy5);
    let body = sexpr::match_expr(Val::CallParams, yy6);
	yyres = sexpr::defunc(id, yy3, typ, body)

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            19 /* dfunc_args ::= */
          | 93 /* list_items ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

	yyres = list::empty();

} };
 YYMinorType::YY14(yyres)
}
            ,
            20 /* dfunc_args ::= ID opt_typex */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY74(yy0),YYMinorType::YY185(yy1),) => {

	yyres = list::singleton(sexpr::id_with_type(yy0, yy1));

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            21 /* dfunc_args ::= ID opt_typex COMMA dfunc_args */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY74(yy0),YYMinorType::YY185(yy1),YYMinorType::YY14(yy3),) => {

	yyres = list::cons(sexpr::id_with_type(yy0, yy1), yy3);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            22 /* opt_typex ::= */
            => 
{
let yyres :  Type ;
match () {
 () => {

	yyres = Type::AnonVar;

} };
 YYMinorType::YY185(yyres)
}
            ,
            23 /* opt_typex ::= COLON typex */
            => 
{
let yyres :  Type ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY185(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY185(yyres)
}
            ,
            24 /* typex ::= TYPE_INT */
            => 
{
let yyres :  Type ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Type::Int;

} };
 YYMinorType::YY185(yyres)
}
            ,
            25 /* typex ::= TYPE_STR */
            => 
{
let yyres :  Type ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Type::Str;

} };
 YYMinorType::YY185(yyres)
}
            ,
            26 /* typex ::= TYPE_BOOL */
            => 
{
let yyres :  Type ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Type::Bool;

} };
 YYMinorType::YY185(yyres)
}
            ,
            27 /* typex ::= TYPE_VOID */
            => 
{
let yyres :  Type ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Type::Void;

} };
 YYMinorType::YY185(yyres)
}
            ,
            28 /* typex ::= TYPE_ID */
            => 
{
let yyres :  Type ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY74(yy0),) => {

	yyres = Type::Id(Arc::new(yy0));

},    _ => unreachable!() };
 YYMinorType::YY185(yyres)
}
            ,
            29 /* macro_stmt ::= MACRO ID LPAREN macro_args RPAREN block DOUBLEDASH */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp5 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,yyp5.minor,) {
 (YYMinorType::YY74(yy1),YYMinorType::YY14(yy3),YYMinorType::YY14(yy5),) => {

    verbose_out!("found macro {:?}\n", yy1);
    yyres = sexpr::new(SexprType::DefMacro,
        list::cons(Val::id(yy1),
        list::cons(yy3,
        list::cons(yy5,
        Val::Nil
    ))));

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            30 /* macro_args ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

    yyres = Val::Nil;

} };
 YYMinorType::YY14(yyres)
}
            ,
            31 /* macro_args ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY74(yy0),) => {

    yyres = list::singleton(Val::id(yy0));

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            32 /* macro_args ::= ID COMMA macro_args */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY74(yy0),YYMinorType::YY14(yy2),) => {

    yyres = list::cons(Val::id(yy0), yy2);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            33 /* if_stmt ::= IF expr block DOUBLEDASH */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY14(yy1),YYMinorType::YY14(yy2),) => {

    /* if-only style */
    yyres = sexpr::ifstmt(yy1, yy2, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            34 /* if_stmt ::= IF expr block else_if DOUBLEDASH */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,yyp3.minor,) {
 (YYMinorType::YY14(yy1),YYMinorType::YY14(yy2),YYMinorType::YY14(yy3),) => {

    /* if-else style */
    yyres = sexpr::ifstmt(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            35 /* if_stmt ::= IF if_case DOUBLEDASH */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY14(yy1),) => {

    /* case-expr style */
    yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            36 /* else_if ::= ELSE IF expr block else_if */
            => 
{
let yyres :  Val ;
let yyp4 = self.yystack.pop().unwrap();
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp2.minor,yyp3.minor,yyp4.minor,) {
 (YYMinorType::YY14(yy2),YYMinorType::YY14(yy3),YYMinorType::YY14(yy4),) => {

    yyres = sexpr::ifstmt(yy2, yy3, yy4);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            37 /* else_if ::= ELSE IF expr block */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp2.minor,yyp3.minor,) {
 (YYMinorType::YY14(yy2),YYMinorType::YY14(yy3),) => {

    yyres = sexpr::ifstmt(yy2, yy3, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            38 /* else_if ::= ELSE block */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY14(yy1),) => {

    yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            39 /* if_case ::= PIPE expr block if_case */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,yyp3.minor,) {
 (YYMinorType::YY14(yy1),YYMinorType::YY14(yy2),YYMinorType::YY14(yy3),) => {

    yyres = sexpr::ifstmt(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            40 /* if_case ::= PIPE ELSE block */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp2.minor,) {
 (YYMinorType::YY14(yy2),) => {

    yyres = yy2;

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            41 /* expr ::= ID LPAREN RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY74(yy0),) => {

	verbose_out!("zero param function call!");
	yyres = sexpr::call(yy0, Val::Nil);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            42 /* expr ::= ID LPAREN expr RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY74(yy0),YYMinorType::YY14(yy2),) => {

	verbose_out!("one param function call!");
	let args = list::singleton(yy2);
	yyres = sexpr::call(yy0, args);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            43 /* expr ::= ID LPAREN tuple_args RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY74(yy0),YYMinorType::YY14(yy2),) => {

	verbose_out!("multi param function call!");
	yyres = sexpr::call(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            44 /* expr ::= term ID term */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY14(yy0),YYMinorType::YY74(yy1),YYMinorType::YY14(yy2),) => {

	yyres = sexpr::binaryop(yy1, yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            45 /* expr ::= term DOLLAR term */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY14(yy0),YYMinorType::YY14(yy2),) => {

	/* yyres = Val::binaryop(yy0, yy2, D); */
	yyres = Val::Void;

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            46 /* expr ::= CASE cases DOUBLEDASH */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY14(yy1),) => {

    verbose_out!("parsed case expr\n");
	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            47 /* cases ::= PIPE expr block PIPE ELSE block */
            => 
{
let yyres :  Val ;
let yyp5 = self.yystack.pop().unwrap();
let yyp4 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,yyp5.minor,) {
 (YYMinorType::YY14(yy1),YYMinorType::YY14(yy2),YYMinorType::YY14(yy5),) => {

    verbose_out!("found cases base\n");
    yyres = sexpr::casex(yy1, yy2, yy5);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            48 /* cases ::= PIPE expr block cases */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,yyp3.minor,) {
 (YYMinorType::YY14(yy1),YYMinorType::YY14(yy2),YYMinorType::YY14(yy3),) => {

    verbose_out!("found extra case\n");
    yyres = sexpr::casex(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            49 /* expr ::= MATCH expr match_case DOUBLEDASH */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY14(yy1),YYMinorType::YY14(yy2),) => {

    verbose_out!("parsed match expr\n");
    yyres = sexpr::match_expr(yy1, yy2);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            50 /* match_case ::= PIPE pexpr block match_case */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,yyp3.minor,) {
 (YYMinorType::YY14(yy1),YYMinorType::YY14(yy2),YYMinorType::YY14(yy3),) => {

    verbose_out!("found cases base\n");
    yyres = sexpr::match_case(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            51 /* match_case ::= PIPE pexpr block */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY14(yy1),YYMinorType::YY14(yy2),) => {

    verbose_out!("parsed base match case\n");
    yyres = sexpr::match_case(yy1, yy2, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            53 /* pexpr ::= INT */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY130(yy0),) => {
 yyres = Val::Int(yy0); 
},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            54 /* pexpr ::= True */
          | 88 /* term ::= True */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Bool(true); 
} };
 YYMinorType::YY14(yyres)
}
            ,
            55 /* pexpr ::= False */
          | 89 /* term ::= False */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Bool(false); 
} };
 YYMinorType::YY14(yyres)
}
            ,
            56 /* pexpr ::= ID */
          | 84 /* term ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY74(yy0),) => {
 yyres = Val::id(yy0); 
},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            57 /* pexpr ::= UNDERSCORE */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Wildcard; 
} };
 YYMinorType::YY14(yyres)
}
            ,
            58 /* ptuple ::= LPAREN RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match () {
 () => {

	panic!("an empty tuple is not a valid pattern");

} };
 YYMinorType::YY14(yyres)
}
            ,
            59 /* ptuple ::= LPAREN pexpr RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY14(yy1),) => {

	yyres = Val::Tuple(vec![yy1]);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            60 /* ptuple ::= LPAREN pargs RPAREN */
          | 96 /* tuple ::= LPAREN tuple_args RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY14(yy1),) => {

	yyres = Val::tuple_from_list(yy1);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            61 /* pargs ::= pexpr COMMA pexpr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY14(yy0),YYMinorType::YY14(yy2),) => {

	yyres = list::cons(yy0,
        list::cons(yy2,
        Val::Nil
        ));

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            62 /* pargs ::= pexpr COMMA pargs */
          | 95 /* list_items ::= expr COMMA list_items */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY14(yy0),YYMinorType::YY14(yy2),) => {

	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            65 /* expr ::= NOT expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY14(yy1),) => {

	yyres = sexpr::call("bool_not".to_string(), list::singleton(yy1));

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            66 /* expr ::= expr ConcatNewline */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY14(yy0),) => {

	let newline = Val::Str(Arc::new("\n".to_string()));
	let args = list::cons(yy0, list::singleton(newline));
	yyres = sexpr::new(SexprType::StrExpr, args)

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            67 /* expr ::= MINUS expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY14(yy1),) => {

	yyres = sexpr::call("negate".to_string(), list::singleton(yy1));

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            68 /* expr ::= expr PLUS expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY14(yy0),YYMinorType::YY14(yy2),) => {

	yyres = sexpr::binaryop("int_add".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            69 /* expr ::= expr MINUS expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY14(yy0),YYMinorType::YY14(yy2),) => {

	yyres = sexpr::binaryop("int_sub".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            70 /* expr ::= expr TIMES expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY14(yy0),YYMinorType::YY14(yy2),) => {

	yyres = sexpr::binaryop("int_mult".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            71 /* expr ::= expr SLASH expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY14(yy0),YYMinorType::YY14(yy2),) => {

	yyres = sexpr::binaryop("int_div".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            72 /* expr ::= expr MOD expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY14(yy0),YYMinorType::YY14(yy2),) => {

	yyres = sexpr::binaryop("int_mod".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            73 /* expr ::= expr AND expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY14(yy0),YYMinorType::YY14(yy2),) => {

	yyres = sexpr::binaryop("and".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            74 /* expr ::= expr OR expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY14(yy0),YYMinorType::YY14(yy2),) => {

	yyres = sexpr::binaryop("or".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            75 /* expr ::= expr XOR expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY14(yy0),YYMinorType::YY14(yy2),) => {

	yyres = sexpr::binaryop("xor".to_string(),yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            76 /* expr ::= expr LT expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY14(yy0),YYMinorType::YY14(yy2),) => {

	yyres = sexpr::binaryop("less_than".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            77 /* expr ::= expr LTEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY14(yy0),YYMinorType::YY14(yy2),) => {

	yyres = sexpr::binaryop("less_than_equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            78 /* expr ::= expr GT expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY14(yy0),YYMinorType::YY14(yy2),) => {

	yyres = sexpr::binaryop("greater_than".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            79 /* expr ::= expr GTEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY14(yy0),YYMinorType::YY14(yy2),) => {

	yyres = sexpr::binaryop("greater_than_equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            80 /* expr ::= expr EQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY14(yy0),YYMinorType::YY14(yy2),) => {

	yyres = sexpr::binaryop("equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            81 /* expr ::= expr NEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY14(yy0),YYMinorType::YY14(yy2),) => {

	let eq = sexpr::binaryop("equal".to_string(), yy0, yy2);
	yyres = sexpr::call("bool_not".to_string(), list::singleton(eq));

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            83 /* term ::= LPAREN expr RPAREN */
          | 92 /* list ::= SquareL list_items SquareR */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY14(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            85 /* term ::= VOID */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Val::Void;

} };
 YYMinorType::YY14(yyres)
}
            ,
            86 /* term ::= DollarQuestion */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Val::id("$".to_string());

} };
 YYMinorType::YY14(yyres)
}
            ,
            87 /* term ::= INT */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY130(yy0),) => {

	yyres = Val::Int(yy0);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            90 /* term ::= HASHTAG */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY137(yy0),) => {

	yyres = Val::Hashtag(Arc::new(yy0.data));

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            94 /* list_items ::= expr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY14(yy0),) => {

	yyres = list::singleton(yy0);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            97 /* tuple_args ::= expr COMMA expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY14(yy0),YYMinorType::YY14(yy2),) => {

	verbose_out!("base tuple args!");
	yyres = list::cons(yy0, list::singleton(yy2));

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            98 /* tuple_args ::= expr COMMA tuple_args */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY14(yy0),YYMinorType::YY14(yy2),) => {

	verbose_out!("additional tuple arg!");
	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            99 /* strexpr ::= StrOpen strlist StrClose */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY14(yy1),) => {

	yyres = sexpr::strexpr(yy1);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            100 /* strlist ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

	yyres = Val::Nil;

} };
 YYMinorType::YY14(yyres)
}
            ,
            101 /* strlist ::= StrLit strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY74(yy0),YYMinorType::YY14(yy1),) => {

	yyres = list::cons(Val::new_str(yy0), yy1);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            102 /* strlist ::= ID strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY74(yy0),YYMinorType::YY14(yy1),) => {

	yyres = list::cons(Val::id(yy0), yy1);

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
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

