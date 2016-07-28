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
const YYNSTATE: i32 = 176;
const YYNRULE: i32 = 95;
const YYERRORSYMBOL: i32 = 59;

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
    LT, //20
    LTEQ, //21
    EQ, //22
    NEQ, //23
    GT, //24
    GTEQ, //25
    MINUS, //26
    TIMES, //27
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
    DOUBLEDASH, //38
    Func, //39
    COLON, //40
    TYPE_INT, //41
    TYPE_STR, //42
    TYPE_BOOL, //43
    TYPE_VOID, //44
    MACRO, //45
    IF, //46
    PIPE, //47
    DOLLAR, //48
    CASE, //49
    MOD, //50
    VOID, //51
    DollarQuestion, //52
    True, //53
    False, //54
    SquareL, //55
    SquareR, //56
    StrOpen, //57
    StrClose, //58
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
pub const TOKEN_LT: i32 = 20;
pub const TOKEN_LTEQ: i32 = 21;
pub const TOKEN_EQ: i32 = 22;
pub const TOKEN_NEQ: i32 = 23;
pub const TOKEN_GT: i32 = 24;
pub const TOKEN_GTEQ: i32 = 25;
pub const TOKEN_MINUS: i32 = 26;
pub const TOKEN_TIMES: i32 = 27;
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
pub const TOKEN_DOUBLEDASH: i32 = 38;
pub const TOKEN_Func: i32 = 39;
pub const TOKEN_COLON: i32 = 40;
pub const TOKEN_TYPE_INT: i32 = 41;
pub const TOKEN_TYPE_STR: i32 = 42;
pub const TOKEN_TYPE_BOOL: i32 = 43;
pub const TOKEN_TYPE_VOID: i32 = 44;
pub const TOKEN_MACRO: i32 = 45;
pub const TOKEN_IF: i32 = 46;
pub const TOKEN_PIPE: i32 = 47;
pub const TOKEN_DOLLAR: i32 = 48;
pub const TOKEN_CASE: i32 = 49;
pub const TOKEN_MOD: i32 = 50;
pub const TOKEN_VOID: i32 = 51;
pub const TOKEN_DollarQuestion: i32 = 52;
pub const TOKEN_True: i32 = 53;
pub const TOKEN_False: i32 = 54;
pub const TOKEN_SquareL: i32 = 55;
pub const TOKEN_SquareR: i32 = 56;
pub const TOKEN_StrOpen: i32 = 57;
pub const TOKEN_StrClose: i32 = 58;
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
        &Token::LT => TOKEN_LT,
        &Token::LTEQ => TOKEN_LTEQ,
        &Token::EQ => TOKEN_EQ,
        &Token::NEQ => TOKEN_NEQ,
        &Token::GT => TOKEN_GT,
        &Token::GTEQ => TOKEN_GTEQ,
        &Token::MINUS => TOKEN_MINUS,
        &Token::TIMES => TOKEN_TIMES,
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
        &Token::DOUBLEDASH => TOKEN_DOUBLEDASH,
        &Token::Func => TOKEN_Func,
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
const YY_ACTTAB_COUNT: i32 = 786;
const YY_ACTION: [YYACTIONTYPE; 786] = [
 /*     0 */   156,  111,  159,   89,   47,  136,  167,  138,   99,  147,
 /*    10 */     1,  146,  107,  155,    1,   30,  155,   89,   49,  120,
 /*    20 */   149,  148,   16,  147,    8,  146,  116,  155,   45,  171,
 /*    30 */   112,  106,  104,   13,  115,  102,   31,  156,  111,  159,
 /*    40 */    31,   97,    3,  114,   12,   88,  163,  161,  160,  158,
 /*    50 */   157,    6,   30,   75,   89,   51,  155,   27,  176,   16,
 /*    60 */   147,    8,  146,  142,  155,  177,  171,  112,  106,  104,
 /*    70 */    13,   44,  102,  156,  162,  159,   28,   73,   97,    3,
 /*    80 */    46,   74,   88,   33,  161,  160,  158,  157,    6,    7,
 /*    90 */    75,   76,  149,  148,    9,   27,  130,   34,  128,   26,
 /*   100 */   138,   99,   24,   23,   25,  164,    4,   22,   21,   18,
 /*   110 */    17,   20,   19,   29,   28,  154,  165,  150,  143,  141,
 /*   120 */   161,  160,  158,  157,  105,  151,   75,  134,  133,  132,
 /*   130 */   131,  103,   15,   89,   48,  155,   14,   26,  101,  147,
 /*   140 */     7,  146,   90,  155,   85,    9,   27,  140,   82,   84,
 /*   150 */   119,  149,  148,   24,   23,   25,  164,   96,   22,   21,
 /*   160 */    18,   17,   20,   19,   29,   28,  137,  145,   81,  272,
 /*   170 */   113,   89,   70,    2,   42,   80,   32,  147,  100,  146,
 /*   180 */   170,  155,  126,  109,  169,  125,  168,  122,   26,  174,
 /*   190 */   173,  172,   89,   54,  117,   33,    9,   27,  147,   83,
 /*   200 */   146,    1,  155,   26,   24,   23,   25,  164,   43,   22,
 /*   210 */    21,   18,   17,   20,   19,   29,   28,   41,  156,  111,
 /*   220 */   159,  123,  149,  148,   69,   98,   95,   31,  129,   94,
 /*   230 */     7,  127,   39,   30,   11,    9,   27,   92,  124,   26,
 /*   240 */    16,  121,    8,   24,   23,   25,  164,   10,   22,   21,
 /*   250 */    18,   17,   20,   19,   29,   28,  118,  110,  153,  135,
 /*   260 */   152,  273,  273,   88,   33,  161,  160,  158,  157,    6,
 /*   270 */   273,   75,   79,  149,   93,  273,  273,  273,   26,  273,
 /*   280 */   273,    5,   89,   50,  273,  273,    9,   27,  147,  108,
 /*   290 */   146,  273,  155,  273,   24,   23,   25,  164,  273,   22,
 /*   300 */    21,   18,   17,   20,   19,   29,   28,  273,    9,   27,
 /*   310 */   273,  273,  273,  273,  273,  273,   24,   23,   25,  164,
 /*   320 */   273,   22,   21,   18,   17,   20,   19,   29,   28,   26,
 /*   330 */   145,  273,  273,  175,   89,   50,    2,  273,  273,  273,
 /*   340 */   147,  144,  146,  170,  155,  273,  273,  169,  273,  168,
 /*   350 */   273,   26,  174,  173,  172,   89,   54,  273,  273,    9,
 /*   360 */    27,  147,  273,  146,  273,  155,  273,   24,   23,   25,
 /*   370 */   164,  273,   22,   21,   18,   17,   20,   19,   29,   28,
 /*   380 */    40,  156,  111,  159,  273,  273,  273,  273,  273,   33,
 /*   390 */   139,  273,  273,    2,  273,  273,   30,   78,  149,  148,
 /*   400 */   170,  273,   26,   16,  169,    8,  168,   33,  273,  174,
 /*   410 */   173,  172,   89,   54,  273,   77,  149,  148,  147,  273,
 /*   420 */   146,  273,  155,  273,  273,  273,   88,  273,  161,  160,
 /*   430 */   158,  157,    6,  273,   75,  156,  111,  159,  273,   91,
 /*   440 */   273,  273,  273,   89,   37,  273,  273,  273,  273,  147,
 /*   450 */    30,  146,  273,  155,  273,  273,  273,   16,  273,    8,
 /*   460 */   166,   89,   52,  273,   89,   38,  273,  147,  273,  146,
 /*   470 */   147,  155,  146,  273,  155,  273,  273,  273,  273,  273,
 /*   480 */    88,  273,  161,  160,  158,  157,    6,  273,   75,  156,
 /*   490 */   111,  159,  273,   89,   58,  273,  273,  273,  273,  147,
 /*   500 */   273,  146,  273,  155,   30,  273,   89,   62,  273,  273,
 /*   510 */   273,   16,  147,    8,  146,  273,  155,  273,   89,   72,
 /*   520 */   273,   89,   87,  273,  147,  273,  146,  147,  155,  146,
 /*   530 */   273,  155,   11,  273,   88,  273,  161,  160,  158,  157,
 /*   540 */     6,  273,   75,  273,    9,   27,  273,  273,  273,  273,
 /*   550 */   156,  111,  159,  273,   25,  164,  273,   22,   21,   18,
 /*   560 */    17,   20,   19,   29,   28,   30,    9,   27,  273,  273,
 /*   570 */   273,  273,   16,  273,    8,  273,  273,  164,  273,   22,
 /*   580 */    21,   18,   17,   20,   19,   29,   28,   26,  273,  273,
 /*   590 */   273,  273,  273,  273,  273,   88,  273,  161,  160,  158,
 /*   600 */   157,    6,  273,   75,  273,    9,   27,  273,  273,   26,
 /*   610 */   273,    9,   27,  273,  273,  273,  273,  273,  273,  273,
 /*   620 */    18,   17,   20,   19,   29,   28,  271,  271,  271,  271,
 /*   630 */    29,   28,  273,  273,   89,   86,  273,  273,  273,  273,
 /*   640 */   147,  273,  146,  273,  155,   89,   57,  273,   26,  273,
 /*   650 */   273,  147,  273,  146,   26,  155,   89,   61,  273,  273,
 /*   660 */   273,  273,  147,  273,  146,  273,  155,   89,   60,  273,
 /*   670 */    89,   59,  273,  147,  273,  146,  147,  155,  146,  273,
 /*   680 */   155,  273,   89,   68,  273,   89,   67,  273,  147,  273,
 /*   690 */   146,  147,  155,  146,  273,  155,   89,   66,  273,  273,
 /*   700 */   273,  273,  147,  273,  146,  273,  155,   89,   65,  273,
 /*   710 */    89,   64,  273,  147,  273,  146,  147,  155,  146,  273,
 /*   720 */   155,  273,  273,  273,   89,   63,  273,  273,  273,  273,
 /*   730 */   147,  273,  146,  273,  155,   89,   71,  273,   89,   56,
 /*   740 */   273,  147,  273,  146,  147,  155,  146,  273,  155,  273,
 /*   750 */    89,   55,  273,  273,  273,  273,  147,  273,  146,  273,
 /*   760 */   155,   89,   53,  273,   89,   36,  273,  147,  273,  146,
 /*   770 */   147,  155,  146,  273,  155,   89,   35,  273,  273,  273,
 /*   780 */   273,  147,  273,  146,  273,  155,
];
const YY_LOOKAHEAD: [YYCODETYPE; 786] = [
 /*     0 */     4,    5,    6,   83,   84,   66,   83,   68,   69,   89,
 /*    10 */    12,   91,   92,   93,   12,   19,   93,   83,   84,   67,
 /*    20 */    68,   69,   26,   89,   28,   91,   30,   93,    5,   33,
 /*    30 */    34,   35,   36,   37,   31,   39,   38,    4,    5,    6,
 /*    40 */    38,   45,   46,   32,   46,   49,   83,   51,   52,   53,
 /*    50 */    54,   55,   19,   57,   83,   84,   93,    8,    0,   26,
 /*    60 */    89,   28,   91,   92,   93,    0,   33,   34,   35,   36,
 /*    70 */    37,   48,   39,    4,    5,    6,   27,    5,   45,   46,
 /*    80 */     4,    9,   49,   59,   51,   52,   53,   54,   55,    2,
 /*    90 */    57,   67,   68,   69,    7,    8,   10,   28,   66,   50,
 /*   100 */    68,   69,   15,   16,   17,   18,   28,   20,   21,   22,
 /*   110 */    23,   24,   25,   26,   27,   58,   29,   38,   56,   29,
 /*   120 */    51,   52,   53,   54,    5,   83,   57,   41,   42,   43,
 /*   130 */    44,    5,   22,   83,   84,   93,   22,   50,    5,   89,
 /*   140 */     2,   91,   92,   93,   28,    7,    8,   59,    2,   29,
 /*   150 */    67,   68,   69,   15,   16,   17,   18,    5,   20,   21,
 /*   160 */    22,   23,   24,   25,   26,   27,   38,   29,   28,   60,
 /*   170 */    61,   83,   84,   64,   29,    2,   47,   89,   72,   91,
 /*   180 */    71,   93,   38,   88,   75,   38,   77,   38,   50,   80,
 /*   190 */    81,   82,   83,   84,   29,   59,    7,    8,   89,    5,
 /*   200 */    91,   12,   93,   50,   15,   16,   17,   18,   86,   20,
 /*   210 */    21,   22,   23,   24,   25,   26,   27,    3,    4,    5,
 /*   220 */     6,   67,   68,   69,   40,   86,   76,   38,   72,    5,
 /*   230 */     2,   76,    3,   19,   47,    7,    8,   78,   78,   50,
 /*   240 */    26,   79,   28,   15,   16,   17,   18,   47,   20,   21,
 /*   250 */    22,   23,   24,   25,   26,   27,   88,   94,   94,   85,
 /*   260 */    94,   95,   95,   49,   59,   51,   52,   53,   54,   55,
 /*   270 */    95,   57,   67,   68,   69,   95,   95,   95,   50,   95,
 /*   280 */    95,    2,   83,   84,   95,   95,    7,    8,   89,   90,
 /*   290 */    91,   95,   93,   95,   15,   16,   17,   18,   95,   20,
 /*   300 */    21,   22,   23,   24,   25,   26,   27,   95,    7,    8,
 /*   310 */    95,   95,   95,   95,   95,   95,   15,   16,   17,   18,
 /*   320 */    95,   20,   21,   22,   23,   24,   25,   26,   27,   50,
 /*   330 */    29,   95,   95,   61,   83,   84,   64,   95,   95,   95,
 /*   340 */    89,   90,   91,   71,   93,   95,   95,   75,   95,   77,
 /*   350 */    95,   50,   80,   81,   82,   83,   84,   95,   95,    7,
 /*   360 */     8,   89,   95,   91,   95,   93,   95,   15,   16,   17,
 /*   370 */    18,   95,   20,   21,   22,   23,   24,   25,   26,   27,
 /*   380 */     3,    4,    5,    6,   95,   95,   95,   95,   95,   59,
 /*   390 */    61,   95,   95,   64,   95,   95,   19,   67,   68,   69,
 /*   400 */    71,   95,   50,   26,   75,   28,   77,   59,   95,   80,
 /*   410 */    81,   82,   83,   84,   95,   67,   68,   69,   89,   95,
 /*   420 */    91,   95,   93,   95,   95,   95,   49,   95,   51,   52,
 /*   430 */    53,   54,   55,   95,   57,    4,    5,    6,   95,   79,
 /*   440 */    95,   95,   95,   83,   84,   95,   95,   95,   95,   89,
 /*   450 */    19,   91,   95,   93,   95,   95,   95,   26,   95,   28,
 /*   460 */    29,   83,   84,   95,   83,   84,   95,   89,   95,   91,
 /*   470 */    89,   93,   91,   95,   93,   95,   95,   95,   95,   95,
 /*   480 */    49,   95,   51,   52,   53,   54,   55,   95,   57,    4,
 /*   490 */     5,    6,   95,   83,   84,   95,   95,   95,   95,   89,
 /*   500 */    95,   91,   95,   93,   19,   95,   83,   84,   95,   95,
 /*   510 */    95,   26,   89,   28,   91,   95,   93,   95,   83,   84,
 /*   520 */    95,   83,   84,   95,   89,   95,   91,   89,   93,   91,
 /*   530 */    95,   93,   47,   95,   49,   95,   51,   52,   53,   54,
 /*   540 */    55,   95,   57,   95,    7,    8,   95,   95,   95,   95,
 /*   550 */     4,    5,    6,   95,   17,   18,   95,   20,   21,   22,
 /*   560 */    23,   24,   25,   26,   27,   19,    7,    8,   95,   95,
 /*   570 */    95,   95,   26,   95,   28,   95,   95,   18,   95,   20,
 /*   580 */    21,   22,   23,   24,   25,   26,   27,   50,   95,   95,
 /*   590 */    95,   95,   95,   95,   95,   49,   95,   51,   52,   53,
 /*   600 */    54,   55,   95,   57,   95,    7,    8,   95,   95,   50,
 /*   610 */    95,    7,    8,   95,   95,   95,   95,   95,   95,   95,
 /*   620 */    22,   23,   24,   25,   26,   27,   22,   23,   24,   25,
 /*   630 */    26,   27,   95,   95,   83,   84,   95,   95,   95,   95,
 /*   640 */    89,   95,   91,   95,   93,   83,   84,   95,   50,   95,
 /*   650 */    95,   89,   95,   91,   50,   93,   83,   84,   95,   95,
 /*   660 */    95,   95,   89,   95,   91,   95,   93,   83,   84,   95,
 /*   670 */    83,   84,   95,   89,   95,   91,   89,   93,   91,   95,
 /*   680 */    93,   95,   83,   84,   95,   83,   84,   95,   89,   95,
 /*   690 */    91,   89,   93,   91,   95,   93,   83,   84,   95,   95,
 /*   700 */    95,   95,   89,   95,   91,   95,   93,   83,   84,   95,
 /*   710 */    83,   84,   95,   89,   95,   91,   89,   93,   91,   95,
 /*   720 */    93,   95,   95,   95,   83,   84,   95,   95,   95,   95,
 /*   730 */    89,   95,   91,   95,   93,   83,   84,   95,   83,   84,
 /*   740 */    95,   89,   95,   91,   89,   93,   91,   95,   93,   95,
 /*   750 */    83,   84,   95,   95,   95,   95,   89,   95,   91,   95,
 /*   760 */    93,   83,   84,   95,   83,   84,   95,   89,   95,   91,
 /*   770 */    89,   93,   91,   95,   93,   83,   84,   95,   95,   95,
 /*   780 */    95,   89,   95,   91,   95,   93,
];
const YY_SHIFT_USE_DFLT: i32 = -5;
const YY_SHIFT_COUNT: i32 = 116;
const YY_SHIFT_MIN: i32 = -4;
const YY_SHIFT_MAX: i32 = 604;
const YY_SHIFT_OFST: [i16; 117] = [
 /*     0 */    -4,   33,   33,  485,  431,  546,  546,  546,  546,  546,
 /*    10 */   377,  214,  546,  546,  546,  546,  546,  546,  546,  546,
 /*    20 */   546,  546,  546,  546,  546,  546,  546,  546,  546,  546,
 /*    30 */   546,  546,  546,  546,  546,  189,  189,  189,  189,   -2,
 /*    40 */     2,    2,    2,    2,   69,   69,   69,  138,   87,  301,
 /*    50 */   279,  228,  352,  352,  352,  352,  352,  352,  352,  537,
 /*    60 */   537,  559,  559,  604,  604,  604,  604,  598,  598,   86,
 /*    70 */    49,   49,   49,   72,   72,   72,  200,  187,  229,  229,
 /*    80 */   224,  224,  194,  184,  184,  194,  153,  153,  129,   23,
 /*    90 */   165,  149,  147,  144,  173,  145,  140,  152,  146,  128,
 /*   100 */   120,  116,  133,  114,  126,  110,  119,   90,   62,   79,
 /*   110 */    57,   78,   76,   65,   58,   11,    3,
];
const YY_REDUCE_USE_DFLT: i32 = -81;
const YY_REDUCE_COUNT: i32 = 88;
const YY_REDUCE_MIN: i32 = -80;
const YY_REDUCE_MAX: i32 = 692;
const YY_REDUCE_OFST: [i16; 89] = [
 /*     0 */   109,  329,  272,  360,   50,  251,  199,  -29,  -80,   88,
 /*    10 */   381,  692,  681,  678,  667,  655,  652,  641,  627,  624,
 /*    20 */   613,  602,  599,  587,  584,  573,  562,  551,  438,  435,
 /*    30 */   423,  410,  381,  378,  -66,  348,  330,  205,   24,  154,
 /*    40 */    83,  -48,   32,  -61,   42,  -37,  -77,  136,  136,  136,
 /*    50 */   136,  136,  136,  136,  136,  136,  136,  136,  136,  136,
 /*    60 */   136,  136,  136,  136,  136,  136,  136,  136,  136,  174,
 /*    70 */   136,  136,  136,  166,  164,  163,  168,  162,  160,  159,
 /*    80 */   155,  150,  156,  139,  122,  106,  136,  136,   95,
];
const YY_DEFAULT: [YYACTIONTYPE; 176] = [
 /*     0 */   178,  178,  178,  271,  271,  261,  261,  271,  271,  271,
 /*    10 */   271,  271,  271,  271,  271,  271,  271,  271,  271,  271,
 /*    20 */   271,  271,  271,  271,  271,  271,  271,  271,  271,  271,
 /*    30 */   271,  271,  271,  271,  271,  271,  271,  271,  271,  271,
 /*    40 */   271,  271,  271,  271,  271,  271,  271,  271,  271,  271,
 /*    50 */   262,  265,  234,  191,  190,  189,  188,  240,  196,  243,
 /*    60 */   242,  241,  231,  249,  248,  247,  246,  245,  244,  271,
 /*    70 */   235,  233,  237,  268,  268,  268,  271,  271,  217,  271,
 /*    80 */   210,  210,  199,  202,  202,  199,  239,  238,  271,  250,
 /*    90 */   271,  271,  271,  195,  211,  271,  271,  271,  200,  271,
 /*   100 */   271,  271,  271,  271,  271,  271,  271,  271,  271,  271,
 /*   110 */   271,  252,  271,  271,  271,  271,  271,  223,  228,  227,
 /*   120 */   220,  219,  215,  218,  216,  214,  213,  212,  209,  201,
 /*   130 */   208,  207,  206,  205,  204,  203,  198,  193,  192,  197,
 /*   140 */   236,  264,  266,  260,  263,  251,  230,  229,  195,  194,
 /*   150 */   226,  225,  270,  269,  267,  259,  258,  257,  256,  255,
 /*   160 */   254,  253,  252,  224,  232,  222,  221,  187,  186,  185,
 /*   170 */   184,  183,  182,  181,  180,  179,
];

/* TMPL: fallback tokens */

const YY_FALLBACK: [i32; 0] = [
];

/* TMPL: symbol names */


/* TMPL: rules */


/* TMPL: destructors */


/* TMPL: stack-overflow */


/* TMPL: stack-overflow */

const YY_RULE_INFO: [YYCODETYPE; 95] = [
  60,
  60,
  61,
  61,
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
  66,
  67,
  67,
  68,
  69,
  71,
  72,
  72,
  72,
  86,
  86,
  85,
  85,
  85,
  85,
  85,
  75,
  76,
  76,
  76,
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
  88,
  88,
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
          | 53 /* expr ::= list */
          | 54 /* expr ::= tuple */
          | 74 /* expr ::= term */
          | 83 /* term ::= strexpr */
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
          | 16 /* endblock ::= block_onex */
          | 18 /* midblock ::= block_onex */
          | 19 /* midblock ::= block_stmts */
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
            17 /* endblock ::= block_stmts DOUBLEDASH */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY14(yy0),) => {

	yyres = yy0;

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            20 /* block_onex ::= DOUBLEDASH expr */
          | 21 /* block_stmts ::= BLOCKARROW stmts */
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
            22 /* func_stmt ::= Func ID LPAREN dfunc_args RPAREN opt_typex endblock */
            => 
{
let yyres :  Val ;
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
            23 /* dfunc_args ::= */
          | 85 /* list_items ::= */
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
            24 /* dfunc_args ::= ID opt_typex */
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
            25 /* dfunc_args ::= ID opt_typex COMMA dfunc_args */
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
            26 /* opt_typex ::= */
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
            27 /* opt_typex ::= COLON typex */
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
            28 /* typex ::= TYPE_INT */
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
            29 /* typex ::= TYPE_STR */
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
            30 /* typex ::= TYPE_BOOL */
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
            31 /* typex ::= TYPE_VOID */
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
            32 /* typex ::= TYPE_ID */
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
            33 /* macro_stmt ::= MACRO ID LPAREN macro_args RPAREN endblock */
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
            34 /* macro_args ::= */
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
            35 /* macro_args ::= ID */
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
            36 /* macro_args ::= ID COMMA macro_args */
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
            37 /* if_stmt ::= IF expr block_stmts DOUBLEDASH */
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
            38 /* if_stmt ::= IF expr midblock else_if DOUBLEDASH */
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
            39 /* if_stmt ::= IF if_case DOUBLEDASH */
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
            40 /* else_if ::= ELSE IF expr midblock else_if */
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
            41 /* else_if ::= ELSE IF expr midblock */
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
            42 /* else_if ::= ELSE midblock */
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
            43 /* if_case ::= PIPE expr midblock if_case */
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
            44 /* if_case ::= PIPE ELSE midblock */
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
            45 /* expr ::= ID LPAREN RPAREN */
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
            46 /* expr ::= ID LPAREN expr RPAREN */
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
            47 /* expr ::= ID LPAREN tuple_args RPAREN */
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
            48 /* expr ::= term ID term */
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
            49 /* expr ::= term DOLLAR term */
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
            50 /* expr ::= CASE cases DOUBLEDASH */
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
            51 /* cases ::= PIPE expr midblock PIPE ELSE midblock */
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
            52 /* cases ::= PIPE expr midblock cases */
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
            55 /* expr ::= NOT expr */
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
            56 /* expr ::= expr ConcatNewline */
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
            57 /* expr ::= MINUS expr */
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
            58 /* expr ::= expr error expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY14(yy0),YYMinorType::YY14(yy2),) => {

    write!(stderr(), "binaryop error: {:?} err {:?}\n", yy0, yy2).ok();
    yyres = Val::Void;

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            59 /* expr ::= expr PLUS expr */
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
            60 /* expr ::= expr PLUS error */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY14(yy0),) => {

    write!(stderr(), "wtf PLUS error: {:?} + error\n", yy0).ok();
    yyres = Val::Void;

},    _ => unreachable!() };
 YYMinorType::YY14(yyres)
}
            ,
            61 /* expr ::= expr MINUS expr */
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
            62 /* expr ::= expr TIMES expr */
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
            63 /* expr ::= expr SLASH expr */
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
            64 /* expr ::= expr MOD expr */
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
            65 /* expr ::= expr AND expr */
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
            66 /* expr ::= expr OR expr */
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
            67 /* expr ::= expr XOR expr */
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
            68 /* expr ::= expr LT expr */
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
            69 /* expr ::= expr LTEQ expr */
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
            70 /* expr ::= expr GT expr */
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
            71 /* expr ::= expr GTEQ expr */
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
            72 /* expr ::= expr EQ expr */
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
            73 /* expr ::= expr NEQ expr */
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
            75 /* term ::= LPAREN expr RPAREN */
          | 84 /* list ::= SquareL list_items SquareR */
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
            76 /* term ::= ID */
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
            77 /* term ::= VOID */
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
            78 /* term ::= DollarQuestion */
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
            79 /* term ::= INT */
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
            80 /* term ::= True */
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
            81 /* term ::= False */
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
            82 /* term ::= HASHTAG */
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
            86 /* list_items ::= expr */
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
            87 /* list_items ::= expr COMMA list_items */
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
            88 /* tuple ::= LPAREN tuple_args RPAREN */
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
            89 /* tuple_args ::= expr COMMA expr */
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
            90 /* tuple_args ::= expr COMMA tuple_args */
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
            91 /* strexpr ::= StrOpen strlist StrClose */
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
            92 /* strlist ::= */
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
            93 /* strlist ::= StrLit strlist */
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
            94 /* strlist ::= ID strlist */
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

