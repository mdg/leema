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
const YYNSTATE: i32 = 186;
const YYNRULE: i32 = 99;
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
    UNDERSCORE, //52
    VOID, //53
    DollarQuestion, //54
    True, //55
    False, //56
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
pub const TOKEN_UNDERSCORE: i32 = 52;
pub const TOKEN_VOID: i32 = 53;
pub const TOKEN_DollarQuestion: i32 = 54;
pub const TOKEN_True: i32 = 55;
pub const TOKEN_False: i32 = 56;
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
        &Token::UNDERSCORE => TOKEN_UNDERSCORE,
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
const YY_ACTTAB_COUNT: i32 = 731;
const YY_ACTION: [YYACTIONTYPE; 731] = [
 /*     0 */   166,  114,  169,  113,   66,  107,  158,  152,   45,  150,
 /*    10 */    90,  149,  104,  165,  118,   26,   35,  117,  113,   41,
 /*    20 */   186,    1,   14,  187,  150,    8,  149,  119,  165,   37,
 /*    30 */   181,  115,  110,  103,   12,  101,   88,  158,  106,  166,
 /*    40 */   114,  169,   96,    3,  153,  129,   65,   28,    4,  171,
 /*    50 */   170,  168,  167,    6,   26,   49,   11,  113,   70,   32,
 /*    60 */    25,   14,   36,  150,    8,  149,  143,  165,  164,  181,
 /*    70 */   115,  110,  103,   12,  101,  109,  156,  157,   29,   27,
 /*    80 */    24,   96,    3,   47,  161,   65,   28,   48,  171,  170,
 /*    90 */   168,  167,    6,    7,   49,  113,   69,  133,   32,   25,
 /*   100 */    34,  150,  105,  149,   30,  165,   22,   21,   23,  174,
 /*   110 */    35,   16,   15,   18,   17,   20,   19,   29,   27,   24,
 /*   120 */     7,  175,  160,  155,  151,   32,   25,   64,  158,  137,
 /*   130 */   136,  135,  134,   22,   21,   23,  174,  144,   16,   15,
 /*   140 */    18,   17,   20,   19,   29,   27,   24,    7,  146,  113,
 /*   150 */    69,  142,   32,   25,  102,  150,  145,  149,   25,  165,
 /*   160 */    22,   21,   23,  174,   13,   16,   15,   18,   17,   20,
 /*   170 */    19,   29,   27,   24,    5,  113,   67,   27,   24,   32,
 /*   180 */    25,  150,  100,  149,   89,  165,   62,   22,   21,   23,
 /*   190 */   174,   61,   16,   15,   18,   17,   20,   19,   29,   27,
 /*   200 */    24,   32,   25,  156,  157,  139,   58,   95,   57,   22,
 /*   210 */    21,   23,  174,  177,   16,   15,   18,   17,   20,   19,
 /*   220 */    29,   27,   24,  165,  146,   32,   25,   34,  154,   56,
 /*   230 */     1,  131,   55,   22,   21,   23,  174,  128,   16,   15,
 /*   240 */    18,   17,   20,   19,   29,   27,   24,   32,   25,  173,
 /*   250 */   155,  125,  111,  120,   31,   22,   21,   23,  174,  165,
 /*   260 */    16,   15,   18,   17,   20,   19,   29,   27,   24,   63,
 /*   270 */     1,  159,  113,   68,   38,   99,   32,   25,  150,   59,
 /*   280 */   149,   98,  165,   44,   22,   21,   23,  174,   38,   16,
 /*   290 */    15,   18,   17,   20,   19,   29,   27,   24,   52,  166,
 /*   300 */   114,  169,   60,   97,  132,  113,   85,  286,  116,    2,
 /*   310 */    94,  150,  180,  149,   26,  165,  179,   92,  178,   93,
 /*   320 */   127,   14,  130,   45,    8,  184,  183,  182,  113,   72,
 /*   330 */   124,  113,   43,   10,  150,  121,  149,  150,  165,  149,
 /*   340 */     9,  165,  123,  112,  122,   65,   28,   91,  171,  170,
 /*   350 */   168,  167,    6,  163,   49,   50,  166,  114,  169,  162,
 /*   360 */   113,   74,  126,   51,  138,  108,  150,   46,  149,   54,
 /*   370 */   165,   26,   53,  287,  287,  287,  113,   86,   14,   32,
 /*   380 */    25,    8,  150,  287,  149,  287,  165,  287,  287,   23,
 /*   390 */   174,  287,   16,   15,   18,   17,   20,   19,   29,   27,
 /*   400 */    24,  287,   65,   28,  287,  171,  170,  168,  167,    6,
 /*   410 */   287,   49,  166,  114,  169,  287,  287,  287,  287,  287,
 /*   420 */   287,  287,  185,    2,  287,  287,  180,   26,  287,  287,
 /*   430 */   179,  287,  178,  287,   14,  287,  287,    8,  176,  184,
 /*   440 */   183,  182,  113,   72,  287,  113,   42,  287,  150,  287,
 /*   450 */   149,  150,  165,  149,  287,  165,  287,  287,   65,   28,
 /*   460 */   287,  171,  170,  168,  167,    6,  287,   49,  166,  114,
 /*   470 */   169,  287,  287,  287,  113,  141,  287,  140,    2,  287,
 /*   480 */   150,  180,  149,   26,  165,  179,  287,  178,  287,  287,
 /*   490 */    14,  287,  287,    8,  184,  183,  182,  113,   72,  287,
 /*   500 */   287,  287,  287,  150,  287,  149,  287,  165,  287,  287,
 /*   510 */   287,  287,   10,  287,   65,   28,  287,  171,  170,  168,
 /*   520 */   167,    6,  287,   49,  166,  114,  169,  113,   78,  287,
 /*   530 */   113,  148,  287,  150,  287,  149,  150,  165,  149,   26,
 /*   540 */   165,  287,  287,  287,  287,  287,   14,   32,   25,    8,
 /*   550 */   287,  287,  287,  287,  287,  287,  287,  287,  174,  287,
 /*   560 */    16,   15,   18,   17,   20,   19,   29,   27,   24,  287,
 /*   570 */    65,   28,  287,  171,  170,  168,  167,    6,  287,   49,
 /*   580 */   287,   32,   25,  287,  166,  172,  169,  287,  287,  287,
 /*   590 */   287,  287,  287,  287,  285,  285,  285,  285,   20,   19,
 /*   600 */    29,   27,   24,  287,  113,  147,  287,  113,   77,   33,
 /*   610 */   150,  287,  149,  150,  165,  149,  287,  165,  287,  113,
 /*   620 */    76,  287,  113,   75,  287,  150,  287,  149,  150,  165,
 /*   630 */   149,  287,  165,  171,  170,  168,  167,  287,  287,   49,
 /*   640 */   287,  287,  113,   84,  287,  113,   83,  287,  150,  287,
 /*   650 */   149,  150,  165,  149,  287,  165,  287,  287,  113,   82,
 /*   660 */   287,  113,   81,  287,  150,  287,  149,  150,  165,  149,
 /*   670 */   287,  165,  287,  287,  287,  287,  287,  287,  113,   80,
 /*   680 */   287,  113,   79,  287,  150,  287,  149,  150,  165,  149,
 /*   690 */   287,  165,  113,   87,  287,  287,  287,  287,  150,  287,
 /*   700 */   149,  287,  165,  113,   73,  287,  113,   71,  287,  150,
 /*   710 */   287,  149,  150,  165,  149,  287,  165,  113,   40,  287,
 /*   720 */   113,   39,  287,  150,  287,  149,  150,  165,  149,  287,
 /*   730 */   165,
];
const YY_LOOKAHEAD: [YYCODETYPE; 731] = [
 /*     0 */     4,    5,    6,   83,   84,   77,   78,   79,    3,   89,
 /*    10 */    75,   91,   92,   93,   32,   19,    2,   33,   83,   84,
 /*    20 */     0,   12,   26,    0,   89,   29,   91,   31,   93,    4,
 /*    30 */    34,   35,   36,   37,   38,   39,   77,   78,   79,    4,
 /*    40 */     5,    6,   46,   47,   30,   40,   50,   51,   29,   53,
 /*    50 */    54,   55,   56,   57,   19,   59,   47,   83,   84,    7,
 /*    60 */     8,   26,   49,   89,   29,   91,   92,   93,   60,   34,
 /*    70 */    35,   36,   37,   38,   39,    5,    5,    6,   26,   27,
 /*    80 */    28,   46,   47,    5,   40,   50,   51,    9,   53,   54,
 /*    90 */    55,   56,   57,    2,   59,   83,   84,   10,    7,    8,
 /*   100 */    29,   89,   90,   91,   20,   93,   15,   16,   17,   18,
 /*   110 */     2,   20,   21,   22,   23,   24,   25,   26,   27,   28,
 /*   120 */     2,   30,   40,   52,   30,    7,    8,   77,   78,   42,
 /*   130 */    43,   44,   45,   15,   16,   17,   18,   58,   20,   21,
 /*   140 */    22,   23,   24,   25,   26,   27,   28,    2,   30,   83,
 /*   150 */    84,   30,    7,    8,    5,   89,   90,   91,    8,   93,
 /*   160 */    15,   16,   17,   18,   20,   20,   21,   22,   23,   24,
 /*   170 */    25,   26,   27,   28,    2,   83,   84,   27,   28,    7,
 /*   180 */     8,   89,    5,   91,   92,   93,   29,   15,   16,   17,
 /*   190 */    18,   30,   20,   21,   22,   23,   24,   25,   26,   27,
 /*   200 */    28,    7,    8,    5,    6,   40,    2,    5,   29,   15,
 /*   210 */    16,   17,   18,   83,   20,   21,   22,   23,   24,   25,
 /*   220 */    26,   27,   28,   93,   30,    7,    8,   29,   30,   30,
 /*   230 */    12,   40,    2,   15,   16,   17,   18,   40,   20,   21,
 /*   240 */    22,   23,   24,   25,   26,   27,   28,    7,    8,   83,
 /*   250 */    52,   40,   88,   30,   48,   15,   16,   17,   18,   93,
 /*   260 */    20,   21,   22,   23,   24,   25,   26,   27,   28,   66,
 /*   270 */    12,   76,   83,   84,   48,   68,    7,    8,   89,    5,
 /*   280 */    91,   66,   93,   41,   15,   16,   17,   18,   48,   20,
 /*   290 */    21,   22,   23,   24,   25,   26,   27,   28,    3,    4,
 /*   300 */     5,    6,   86,   86,   68,   83,   84,   62,   63,   64,
 /*   310 */    72,   89,   67,   91,   19,   93,   71,    5,   73,   66,
 /*   320 */    74,   26,   72,    3,   29,   80,   81,   82,   83,   84,
 /*   330 */    75,   83,   84,   48,   89,   88,   91,   89,   93,   91,
 /*   340 */    48,   93,   66,   94,   66,   50,   51,   74,   53,   54,
 /*   350 */    55,   56,   57,   94,   59,    3,    4,    5,    6,   94,
 /*   360 */    83,   84,   66,   66,   85,   76,   89,   66,   91,   66,
 /*   370 */    93,   19,   66,   95,   95,   95,   83,   84,   26,    7,
 /*   380 */     8,   29,   89,   95,   91,   95,   93,   95,   95,   17,
 /*   390 */    18,   95,   20,   21,   22,   23,   24,   25,   26,   27,
 /*   400 */    28,   95,   50,   51,   95,   53,   54,   55,   56,   57,
 /*   410 */    95,   59,    4,    5,    6,   95,   95,   95,   95,   95,
 /*   420 */    95,   95,   63,   64,   95,   95,   67,   19,   95,   95,
 /*   430 */    71,   95,   73,   95,   26,   95,   95,   29,   30,   80,
 /*   440 */    81,   82,   83,   84,   95,   83,   84,   95,   89,   95,
 /*   450 */    91,   89,   93,   91,   95,   93,   95,   95,   50,   51,
 /*   460 */    95,   53,   54,   55,   56,   57,   95,   59,    4,    5,
 /*   470 */     6,   95,   95,   95,   83,   84,   95,   63,   64,   95,
 /*   480 */    89,   67,   91,   19,   93,   71,   95,   73,   95,   95,
 /*   490 */    26,   95,   95,   29,   80,   81,   82,   83,   84,   95,
 /*   500 */    95,   95,   95,   89,   95,   91,   95,   93,   95,   95,
 /*   510 */    95,   95,   48,   95,   50,   51,   95,   53,   54,   55,
 /*   520 */    56,   57,   95,   59,    4,    5,    6,   83,   84,   95,
 /*   530 */    83,   84,   95,   89,   95,   91,   89,   93,   91,   19,
 /*   540 */    93,   95,   95,   95,   95,   95,   26,    7,    8,   29,
 /*   550 */    95,   95,   95,   95,   95,   95,   95,   95,   18,   95,
 /*   560 */    20,   21,   22,   23,   24,   25,   26,   27,   28,   95,
 /*   570 */    50,   51,   95,   53,   54,   55,   56,   57,   95,   59,
 /*   580 */    95,    7,    8,   95,    4,    5,    6,   95,   95,   95,
 /*   590 */    95,   95,   95,   95,   20,   21,   22,   23,   24,   25,
 /*   600 */    26,   27,   28,   95,   83,   84,   95,   83,   84,   29,
 /*   610 */    89,   95,   91,   89,   93,   91,   95,   93,   95,   83,
 /*   620 */    84,   95,   83,   84,   95,   89,   95,   91,   89,   93,
 /*   630 */    91,   95,   93,   53,   54,   55,   56,   95,   95,   59,
 /*   640 */    95,   95,   83,   84,   95,   83,   84,   95,   89,   95,
 /*   650 */    91,   89,   93,   91,   95,   93,   95,   95,   83,   84,
 /*   660 */    95,   83,   84,   95,   89,   95,   91,   89,   93,   91,
 /*   670 */    95,   93,   95,   95,   95,   95,   95,   95,   83,   84,
 /*   680 */    95,   83,   84,   95,   89,   95,   91,   89,   93,   91,
 /*   690 */    95,   93,   83,   84,   95,   95,   95,   95,   89,   95,
 /*   700 */    91,   95,   93,   83,   84,   95,   83,   84,   95,   89,
 /*   710 */    95,   91,   89,   93,   91,   95,   93,   83,   84,   95,
 /*   720 */    83,   84,   95,   89,   95,   91,   89,   93,   91,   95,
 /*   730 */    93,
];
const YY_SHIFT_USE_DFLT: i32 = -19;
const YY_SHIFT_COUNT: i32 = 119;
const YY_SHIFT_MIN: i32 = -18;
const YY_SHIFT_MAX: i32 = 580;
const YY_SHIFT_OFST: [i16; 120] = [
 /*     0 */    -4,   35,   35,  464,  408,  520,  520,  520,  520,  352,
 /*    10 */   295,  520,  520,  520,  520,  520,  520,  520,  520,  520,
 /*    20 */   520,  520,  520,  520,  520,  520,  520,  520,  520,  520,
 /*    30 */   520,  520,  520,  520,  198,   71,  580,  580,   71,  218,
 /*    40 */   218,  218,  240,  218,   87,    9,    5,   78,   78,   78,
 /*    50 */   258,  292,  258,  285,  320,  312,  258,  312,  274,  242,
 /*    60 */   258,  242,  274,  226,  258,  206,  118,   91,  194,  172,
 /*    70 */   145,  269,  269,  269,  269,  372,  372,  540,  540,  574,
 /*    80 */   574,  574,  574,   52,   52,  150,  150,  150,   14,  223,
 /*    90 */   211,  197,  230,  191,  199,  179,  202,  204,  165,  161,
 /*   100 */   157,  177,  144,  149,  121,   79,   94,  108,   82,   84,
 /*   110 */    70,   44,    8,   13,   19,   25,   23,   20,  -16,  -18,
];
const YY_REDUCE_USE_DFLT: i32 = -81;
const YY_REDUCE_COUNT: i32 = 65;
const YY_REDUCE_MIN: i32 = -80;
const YY_REDUCE_MAX: i32 = 637;
const YY_REDUCE_OFST: [i16; 66] = [
 /*     0 */   245,  414,  359,  -65,   92,   66,   12,  -26,  -80,  248,
 /*    10 */   637,  634,  623,  620,  609,  598,  595,  578,  575,  562,
 /*    20 */   559,  539,  536,  524,  521,  447,  444,  391,  362,  293,
 /*    30 */   277,  248,  222,  189,  -41,  -72,  166,  130,   50,  306,
 /*    40 */   303,  301,  289,  297,  279,  296,  273,  265,  259,  249,
 /*    50 */   278,  247,  276,  255,  246,  250,  253,  238,  236,  217,
 /*    60 */   215,  216,  207,  195,  203,  164,
];
const YY_DEFAULT: [YYACTIONTYPE; 186] = [
 /*     0 */   188,  188,  188,  285,  285,  275,  275,  285,  285,  285,
 /*    10 */   285,  285,  285,  285,  285,  285,  285,  285,  285,  285,
 /*    20 */   285,  285,  285,  285,  285,  285,  285,  285,  285,  285,
 /*    30 */   285,  285,  285,  285,  285,  285,  285,  285,  285,  285,
 /*    40 */   285,  285,  285,  285,  285,  285,  285,  282,  282,  282,
 /*    50 */   285,  285,  285,  285,  222,  215,  285,  215,  204,  207,
 /*    60 */   285,  207,  204,  235,  285,  285,  285,  285,  285,  276,
 /*    70 */   279,  201,  200,  199,  198,  257,  256,  255,  247,  263,
 /*    80 */   262,  261,  260,  259,  258,  250,  251,  249,  285,  285,
 /*    90 */   285,  285,  216,  285,  285,  285,  285,  205,  285,  285,
 /*   100 */   285,  285,  285,  285,  285,  285,  285,  243,  285,  285,
 /*   110 */   285,  285,  285,  264,  266,  285,  285,  285,  285,  285,
 /*   120 */   228,  232,  231,  225,  224,  220,  223,  221,  219,  218,
 /*   130 */   217,  214,  206,  213,  212,  211,  210,  209,  208,  203,
 /*   140 */   202,  252,  278,  280,  274,  277,  265,  254,  253,  246,
 /*   150 */   245,  242,  244,  241,  240,  239,  238,  237,  236,  234,
 /*   160 */   233,  230,  284,  283,  281,  273,  272,  271,  270,  269,
 /*   170 */   268,  267,  266,  229,  248,  227,  226,  197,  196,  195,
 /*   180 */   194,  193,  192,  191,  190,  189,
];

/* TMPL: fallback tokens */

const YY_FALLBACK: [i32; 0] = [
];

/* TMPL: symbol names */


/* TMPL: rules */


/* TMPL: destructors */


/* TMPL: stack-overflow */


/* TMPL: stack-overflow */

const YY_RULE_INFO: [YYCODETYPE; 99] = [
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
  88,
  88,
  84,
  76,
  76,
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
          | 50 /* pexpr ::= ptuple */
          | 59 /* expr ::= list */
          | 60 /* expr ::= tuple */
          | 78 /* expr ::= term */
          | 87 /* term ::= strexpr */
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
            18 /* dfunc_args ::= */
          | 89 /* list_items ::= */
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
            19 /* dfunc_args ::= ID opt_typex */
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
            20 /* dfunc_args ::= ID opt_typex COMMA dfunc_args */
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
            21 /* opt_typex ::= */
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
            22 /* opt_typex ::= COLON typex */
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
            23 /* typex ::= TYPE_INT */
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
            24 /* typex ::= TYPE_STR */
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
            25 /* typex ::= TYPE_BOOL */
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
            26 /* typex ::= TYPE_VOID */
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
            27 /* typex ::= TYPE_ID */
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
            28 /* macro_stmt ::= MACRO ID LPAREN macro_args RPAREN block DOUBLEDASH */
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
            29 /* macro_args ::= */
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
            30 /* macro_args ::= ID */
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
            31 /* macro_args ::= ID COMMA macro_args */
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
            32 /* if_stmt ::= IF expr block DOUBLEDASH */
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
            33 /* if_stmt ::= IF expr block else_if DOUBLEDASH */
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
            34 /* if_stmt ::= IF if_case DOUBLEDASH */
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
            35 /* else_if ::= ELSE IF expr block else_if */
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
            36 /* else_if ::= ELSE IF expr block */
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
            37 /* else_if ::= ELSE block */
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
            38 /* if_case ::= PIPE expr block if_case */
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
            39 /* if_case ::= PIPE ELSE block */
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
            40 /* expr ::= ID LPAREN RPAREN */
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
            41 /* expr ::= ID LPAREN expr RPAREN */
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
            42 /* expr ::= ID LPAREN tuple_args RPAREN */
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
            43 /* expr ::= term DOLLAR term */
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
            44 /* expr ::= CASE cases DOUBLEDASH */
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
            45 /* cases ::= PIPE expr block PIPE ELSE block */
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
            46 /* cases ::= PIPE expr block cases */
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
            47 /* expr ::= MATCH expr match_case DOUBLEDASH */
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
            48 /* match_case ::= PIPE pexpr block match_case */
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
            49 /* match_case ::= PIPE pexpr block */
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
            51 /* pexpr ::= INT */
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
            52 /* pexpr ::= ID */
          | 80 /* term ::= ID */
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
            53 /* pexpr ::= UNDERSCORE */
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
            54 /* ptuple ::= LPAREN RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Val::Tuple(vec![]);

} };
 YYMinorType::YY14(yyres)
}
            ,
            55 /* ptuple ::= LPAREN pexpr RPAREN */
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
            56 /* ptuple ::= LPAREN pargs RPAREN */
          | 92 /* tuple ::= LPAREN tuple_args RPAREN */
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
            57 /* pargs ::= pexpr COMMA pexpr */
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
            58 /* pargs ::= pexpr COMMA pargs */
          | 91 /* list_items ::= expr COMMA list_items */
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
            61 /* expr ::= NOT expr */
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
            62 /* expr ::= expr ConcatNewline */
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
            63 /* expr ::= MINUS expr */
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
            64 /* expr ::= expr PLUS expr */
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
            65 /* expr ::= expr MINUS expr */
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
            66 /* expr ::= expr TIMES expr */
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
            67 /* expr ::= expr SLASH expr */
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
            68 /* expr ::= expr MOD expr */
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
            69 /* expr ::= expr AND expr */
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
            70 /* expr ::= expr OR expr */
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
            71 /* expr ::= expr XOR expr */
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
            72 /* expr ::= expr LT expr */
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
            73 /* expr ::= expr LTEQ expr */
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
            74 /* expr ::= expr GT expr */
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
            75 /* expr ::= expr GTEQ expr */
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
            76 /* expr ::= expr EQ expr */
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
            77 /* expr ::= expr NEQ expr */
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
            79 /* term ::= LPAREN expr RPAREN */
          | 88 /* list ::= SquareL list_items SquareR */
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
            81 /* term ::= VOID */
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
            82 /* term ::= DollarQuestion */
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
            83 /* term ::= INT */
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
            84 /* term ::= True */
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
            85 /* term ::= False */
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
            86 /* term ::= HASHTAG */
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
            90 /* list_items ::= expr */
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
            93 /* tuple_args ::= expr COMMA expr */
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
            94 /* tuple_args ::= expr COMMA tuple_args */
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
            95 /* strexpr ::= StrOpen strlist StrClose */
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
            96 /* strlist ::= */
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
            97 /* strlist ::= StrLit strlist */
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
            98 /* strlist ::= ID strlist */
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

