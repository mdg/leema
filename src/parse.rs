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
const YYNOCODE: i32 = 99;
type YYACTIONTYPE = u16;
const YYWILDCARD: YYCODETYPE = 1;
enum YYMinorType {
    YY0,
    YY24(String),
    YY32(TokenLoc),
    YY49(Ast),
    YY60(Val),
    YY71(Type),
    YY136(i64),
    YY173(TokenData<String>),
}
const YYNSTATE: i32 = 192;
const YYNRULE: i32 = 104;
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
    DOUBLEDASH, //39
    Func, //40
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
pub const TOKEN_DOUBLEDASH: i32 = 39;
pub const TOKEN_Func: i32 = 40;
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
        Token::COMMA(x) => YYMinorType::YY32(x),
        Token::ELSE(x) => YYMinorType::YY32(x),
        Token::HASHTAG(x) => YYMinorType::YY173(x),
        Token::ID(x) => YYMinorType::YY24(x),
        Token::INT(x) => YYMinorType::YY136(x),
        Token::PLUS(x) => YYMinorType::YY32(x),
        Token::SLASH(x) => YYMinorType::YY32(x),
        Token::StrLit(x) => YYMinorType::YY24(x),
        Token::TYPE_ID(x) => YYMinorType::YY24(x),
        _ => YYMinorType::YY0
  }
}
const YY_ACTTAB_COUNT: i32 = 751;
const YY_ACTION: [YYACTIONTYPE; 751] = [
 /*     0 */   172,  116,  175,  115,   67,   56,  166,  165,  120,  163,
 /*    10 */    94,  162,  108,  171,  150,   26,  152,  102,  115,   37,
 /*    20 */    40,  192,   14,  119,  163,    8,  162,  121,  171,    1,
 /*    30 */   187,  117,  111,  107,   12,  142,  105,  152,  102,  172,
 /*    40 */   116,  175,  100,    3,   46,  132,   66,   29,  127,  177,
 /*    50 */   176,  174,  173,    6,   26,   54,   31,  115,   71,   33,
 /*    60 */    25,   14,  193,  163,    8,  162,  156,  171,  144,  187,
 /*    70 */   117,  111,  107,   12,  183,  105,   25,  179,   30,   27,
 /*    80 */    24,  100,    3,   48,  171,   66,   29,  171,  177,  176,
 /*    90 */   174,  173,    6,    7,   54,   27,   24,   47,   33,   25,
 /*   100 */   148,  147,  146,  145,  130,  131,   22,   21,   23,  180,
 /*   110 */     4,   16,   15,   18,   17,   20,   19,   30,   27,   24,
 /*   120 */     7,  181,  134,  166,  165,   33,   25,  170,   39,  128,
 /*   130 */    93,  132,  126,   22,   21,   23,  180,  167,   16,   15,
 /*   140 */    18,   17,   20,   19,   30,   27,   24,  164,  159,   33,
 /*   150 */    25,  129,   52,  110,    1,   28,   53,   22,   21,   23,
 /*   160 */   180,  157,   16,   15,   18,   17,   20,   19,   30,   27,
 /*   170 */    24,    7,  115,   70,  155,    1,   33,   25,  163,  109,
 /*   180 */   162,   31,  171,  106,   22,   21,   23,  180,   13,   16,
 /*   190 */    15,   18,   17,   20,   19,   30,   27,   24,    5,  115,
 /*   200 */    70,   65,   31,   33,   25,  163,  158,  162,  104,  171,
 /*   210 */    11,   22,   21,   23,  180,   64,   16,   15,   18,   17,
 /*   220 */    20,   19,   30,   27,   24,   33,   25,   62,  151,  130,
 /*   230 */   131,   99,   44,   22,   21,   23,  180,   61,   16,   15,
 /*   240 */    18,   17,   20,   19,   30,   27,   24,   60,  159,   33,
 /*   250 */    25,  140,  139,   39,  124,  166,  165,   22,   21,   23,
 /*   260 */   180,  136,   16,   15,   18,   17,   20,   19,   30,   27,
 /*   270 */    24,  137,  166,  165,  115,   68,  129,  113,   33,   25,
 /*   280 */   163,   40,  162,   91,  171,  125,   22,   21,   23,  180,
 /*   290 */    49,   16,   15,   18,   17,   20,   19,   30,   27,   24,
 /*   300 */    43,  172,  116,  175,  122,   32,  297,  118,    2,  115,
 /*   310 */    69,   90,  132,   92,  186,  163,   26,  162,  185,  171,
 /*   320 */   184,  103,   63,   14,   45,   51,    8,  190,  189,  188,
 /*   330 */   115,   73,  101,  115,   87,  143,  163,   98,  162,  163,
 /*   340 */   171,  162,   97,  171,   55,  166,  165,   66,   29,   95,
 /*   350 */   177,  176,  174,  173,    6,  141,   54,   42,  172,  116,
 /*   360 */   175,  115,   38,   59,  166,   96,   41,  163,  138,  162,
 /*   370 */    10,  171,  135,   26,   58,  166,  165,  133,  115,   76,
 /*   380 */    14,   33,   25,    8,  163,  114,  162,   49,  171,    9,
 /*   390 */   123,   23,  180,  149,   16,   15,   18,   17,   20,   19,
 /*   400 */    30,   27,   24,  169,   66,   29,  168,  177,  176,  174,
 /*   410 */   173,    6,  112,   54,  172,  116,  175,  115,   88,  298,
 /*   420 */   298,  191,    2,  163,  298,  162,  298,  171,  186,   26,
 /*   430 */   298,  298,  185,  298,  184,  298,   14,  298,  298,    8,
 /*   440 */   182,  190,  189,  188,  115,   73,  298,  115,   50,  298,
 /*   450 */   163,  298,  162,  163,  171,  162,  298,  171,  298,  298,
 /*   460 */    66,   29,  298,  177,  176,  174,  173,    6,  298,   54,
 /*   470 */   172,  116,  175,  115,   75,  298,  298,  153,    2,  163,
 /*   480 */   298,  162,  298,  171,  186,   26,  298,  298,  185,  298,
 /*   490 */   184,  298,   14,  298,  298,    8,  298,  190,  189,  188,
 /*   500 */   115,   73,  298,  115,  154,  298,  163,  298,  162,  163,
 /*   510 */   171,  162,  298,  171,   10,  298,   66,   29,  298,  177,
 /*   520 */   176,  174,  173,    6,  298,   54,  172,  116,  175,  115,
 /*   530 */    80,  298,  115,  161,  298,  163,  298,  162,  163,  171,
 /*   540 */   162,   26,  171,  298,  298,   57,  166,  165,   14,   33,
 /*   550 */    25,    8,  298,  298,  298,  298,  298,  298,  298,  298,
 /*   560 */   180,  298,   16,   15,   18,   17,   20,   19,   30,   27,
 /*   570 */    24,  298,   66,   29,  298,  177,  176,  174,  173,    6,
 /*   580 */   298,   54,  298,   33,   25,  298,  172,  178,  175,  298,
 /*   590 */   298,  298,  298,  298,  298,  298,  296,  296,  296,  296,
 /*   600 */    20,   19,   30,   27,   24,  298,  115,  160,  298,  115,
 /*   610 */    79,   34,  163,  298,  162,  163,  171,  162,  298,  171,
 /*   620 */   115,   78,  298,  298,  298,  298,  163,  298,  162,  298,
 /*   630 */   171,  298,  298,  298,  298,  177,  176,  174,  173,  298,
 /*   640 */   298,   54,  298,  298,  298,  298,  298,  115,   77,  298,
 /*   650 */   115,   86,  298,  163,  298,  162,  163,  171,  162,  298,
 /*   660 */   171,  298,  298,  115,   85,  298,  115,   84,  298,  163,
 /*   670 */   298,  162,  163,  171,  162,  298,  171,  298,  298,  298,
 /*   680 */   298,  298,  298,  115,   83,  298,  115,   82,  298,  163,
 /*   690 */   298,  162,  163,  171,  162,  298,  171,  115,   81,  298,
 /*   700 */   298,  298,  298,  163,  298,  162,  298,  171,  115,   89,
 /*   710 */   298,  298,  298,  298,  163,  298,  162,  298,  171,  298,
 /*   720 */   298,  298,  115,   74,  298,  115,   72,  298,  163,  298,
 /*   730 */   162,  163,  171,  162,  298,  171,  298,  115,   36,  298,
 /*   740 */   115,   35,  298,  163,  298,  162,  163,  171,  162,  298,
 /*   750 */   171,
];
const YY_LOOKAHEAD: [YYCODETYPE; 751] = [
 /*     0 */     4,    5,    6,   86,   87,   67,   68,   69,   32,   92,
 /*    10 */    78,   94,   95,   96,   66,   19,   68,   69,   86,   87,
 /*    20 */     2,    0,   26,   33,   92,   29,   94,   31,   96,   12,
 /*    30 */    34,   35,   36,   37,   38,   66,   40,   68,   69,    4,
 /*    40 */     5,    6,   46,   47,   80,   81,   50,   51,   30,   53,
 /*    50 */    54,   55,   56,   57,   19,   59,   39,   86,   87,    7,
 /*    60 */     8,   26,    0,   92,   29,   94,   95,   96,   10,   34,
 /*    70 */    35,   36,   37,   38,   86,   40,    8,   86,   26,   27,
 /*    80 */    28,   46,   47,    4,   96,   50,   51,   96,   53,   54,
 /*    90 */    55,   56,   57,    2,   59,   27,   28,   49,    7,    8,
 /*   100 */    42,   43,   44,   45,    5,    6,   15,   16,   17,   18,
 /*   110 */    29,   20,   21,   22,   23,   24,   25,   26,   27,   28,
 /*   120 */     2,   30,   67,   68,   69,    7,    8,   60,   29,   30,
 /*   130 */    80,   81,   82,   15,   16,   17,   18,   39,   20,   21,
 /*   140 */    22,   23,   24,   25,   26,   27,   28,   39,   30,    7,
 /*   150 */     8,   52,    5,    5,   12,   20,    9,   15,   16,   17,
 /*   160 */    18,   58,   20,   21,   22,   23,   24,   25,   26,   27,
 /*   170 */    28,    2,   86,   87,   30,   12,    7,    8,   92,   93,
 /*   180 */    94,   39,   96,    5,   15,   16,   17,   18,   20,   20,
 /*   190 */    21,   22,   23,   24,   25,   26,   27,   28,    2,   86,
 /*   200 */    87,   29,   39,    7,    8,   92,   93,   94,    5,   96,
 /*   210 */    47,   15,   16,   17,   18,   30,   20,   21,   22,   23,
 /*   220 */    24,   25,   26,   27,   28,    7,    8,    2,   39,    5,
 /*   230 */     6,    5,   30,   15,   16,   17,   18,   29,   20,   21,
 /*   240 */    22,   23,   24,   25,   26,   27,   28,    2,   30,    7,
 /*   250 */     8,   39,   39,   29,   67,   68,   69,   15,   16,   17,
 /*   260 */    18,   39,   20,   21,   22,   23,   24,   25,   26,   27,
 /*   270 */    28,   67,   68,   69,   86,   87,   52,   91,    7,    8,
 /*   280 */    92,    2,   94,   95,   96,   30,   15,   16,   17,   18,
 /*   290 */    48,   20,   21,   22,   23,   24,   25,   26,   27,   28,
 /*   300 */     3,    4,    5,    6,   30,   48,   62,   63,   64,   86,
 /*   310 */    87,   80,   81,   82,   70,   92,   19,   94,   74,   96,
 /*   320 */    76,   71,    5,   26,   89,   41,   29,   83,   84,   85,
 /*   330 */    86,   87,   89,   86,   87,   71,   92,   75,   94,   92,
 /*   340 */    96,   94,    5,   96,   67,   68,   69,   50,   51,   77,
 /*   350 */    53,   54,   55,   56,   57,   75,   59,    3,    4,    5,
 /*   360 */     6,   86,   87,   67,   68,   69,    3,   92,   77,   94,
 /*   370 */    48,   96,   78,   19,   67,   68,   69,   79,   86,   87,
 /*   380 */    26,    7,    8,   29,   92,   97,   94,   48,   96,   48,
 /*   390 */    91,   17,   18,   88,   20,   21,   22,   23,   24,   25,
 /*   400 */    26,   27,   28,   97,   50,   51,   97,   53,   54,   55,
 /*   410 */    56,   57,   79,   59,    4,    5,    6,   86,   87,   98,
 /*   420 */    98,   63,   64,   92,   98,   94,   98,   96,   70,   19,
 /*   430 */    98,   98,   74,   98,   76,   98,   26,   98,   98,   29,
 /*   440 */    30,   83,   84,   85,   86,   87,   98,   86,   87,   98,
 /*   450 */    92,   98,   94,   92,   96,   94,   98,   96,   98,   98,
 /*   460 */    50,   51,   98,   53,   54,   55,   56,   57,   98,   59,
 /*   470 */     4,    5,    6,   86,   87,   98,   98,   63,   64,   92,
 /*   480 */    98,   94,   98,   96,   70,   19,   98,   98,   74,   98,
 /*   490 */    76,   98,   26,   98,   98,   29,   98,   83,   84,   85,
 /*   500 */    86,   87,   98,   86,   87,   98,   92,   98,   94,   92,
 /*   510 */    96,   94,   98,   96,   48,   98,   50,   51,   98,   53,
 /*   520 */    54,   55,   56,   57,   98,   59,    4,    5,    6,   86,
 /*   530 */    87,   98,   86,   87,   98,   92,   98,   94,   92,   96,
 /*   540 */    94,   19,   96,   98,   98,   67,   68,   69,   26,    7,
 /*   550 */     8,   29,   98,   98,   98,   98,   98,   98,   98,   98,
 /*   560 */    18,   98,   20,   21,   22,   23,   24,   25,   26,   27,
 /*   570 */    28,   98,   50,   51,   98,   53,   54,   55,   56,   57,
 /*   580 */    98,   59,   98,    7,    8,   98,    4,    5,    6,   98,
 /*   590 */    98,   98,   98,   98,   98,   98,   20,   21,   22,   23,
 /*   600 */    24,   25,   26,   27,   28,   98,   86,   87,   98,   86,
 /*   610 */    87,   29,   92,   98,   94,   92,   96,   94,   98,   96,
 /*   620 */    86,   87,   98,   98,   98,   98,   92,   98,   94,   98,
 /*   630 */    96,   98,   98,   98,   98,   53,   54,   55,   56,   98,
 /*   640 */    98,   59,   98,   98,   98,   98,   98,   86,   87,   98,
 /*   650 */    86,   87,   98,   92,   98,   94,   92,   96,   94,   98,
 /*   660 */    96,   98,   98,   86,   87,   98,   86,   87,   98,   92,
 /*   670 */    98,   94,   92,   96,   94,   98,   96,   98,   98,   98,
 /*   680 */    98,   98,   98,   86,   87,   98,   86,   87,   98,   92,
 /*   690 */    98,   94,   92,   96,   94,   98,   96,   86,   87,   98,
 /*   700 */    98,   98,   98,   92,   98,   94,   98,   96,   86,   87,
 /*   710 */    98,   98,   98,   98,   92,   98,   94,   98,   96,   98,
 /*   720 */    98,   98,   86,   87,   98,   86,   87,   98,   92,   98,
 /*   730 */    94,   92,   96,   94,   98,   96,   98,   86,   87,   98,
 /*   740 */    86,   87,   98,   92,   98,   94,   92,   96,   94,   98,
 /*   750 */    96,
];
const YY_SHIFT_USE_DFLT: i32 = -25;
const YY_SHIFT_COUNT: i32 = 121;
const YY_SHIFT_MIN: i32 = -24;
const YY_SHIFT_MAX: i32 = 582;
const YY_SHIFT_OFST: [i16; 122] = [
 /*     0 */    -4,   35,   35,  466,  410,  522,  522,  522,  522,  354,
 /*    10 */   297,  522,  522,  522,  522,  522,  522,  522,  522,  522,
 /*    20 */   522,  522,  522,  522,  522,  522,  522,  522,  522,  522,
 /*    30 */   522,  522,  522,  522,  522,  142,  142,  142,  142,   99,
 /*    40 */   224,  163,   17,   17,   17,   17,   17,  582,  582,  224,
 /*    50 */   242,   58,  147,  147,  147,  341,  339,  322,  363,  363,
 /*    60 */   337,  337,  317,  284,  284,  317,  257,  118,   91,  218,
 /*    70 */   196,  169,  271,  271,  271,  271,  271,  374,  374,  542,
 /*    80 */   542,  576,  576,  576,  576,   52,   52,   68,   68,   68,
 /*    90 */    18,  274,  255,  279,  222,  213,  212,  245,  202,  208,
 /*   100 */   226,  225,  189,  185,  172,  203,  168,  178,  144,  103,
 /*   110 */   135,  148,  108,   98,   67,   48,   81,   79,   62,   21,
 /*   120 */   -10,  -24,
];
const YY_REDUCE_USE_DFLT: i32 = -84;
const YY_REDUCE_COUNT: i32 = 66;
const YY_REDUCE_MIN: i32 = -83;
const YY_REDUCE_MAX: i32 = 654;
const YY_REDUCE_OFST: [i16; 67] = [
 /*     0 */   244,  414,  358,  -68,  188,  113,   86,  -29,  -83,  275,
 /*    10 */   654,  651,  639,  636,  622,  611,  600,  597,  580,  577,
 /*    20 */   564,  561,  534,  523,  520,  446,  443,  417,  387,  361,
 /*    30 */   331,  292,  275,  247,  223,  478,  307,  296,  277,  231,
 /*    40 */    50,  204,  187,   55,  -31,  -52,  -62,   -9,  -12,  -36,
 /*    50 */   333,  305,  309,  306,  288,  299,  298,  294,  291,  272,
 /*    60 */   280,  262,  264,  243,  235,  250,  186,
];
const YY_DEFAULT: [YYACTIONTYPE; 192] = [
 /*     0 */   194,  194,  194,  296,  296,  286,  286,  296,  296,  296,
 /*    10 */   296,  296,  296,  296,  296,  296,  296,  296,  296,  296,
 /*    20 */   296,  296,  296,  296,  296,  296,  296,  296,  296,  296,
 /*    30 */   296,  296,  296,  296,  296,  296,  296,  296,  296,  296,
 /*    40 */   296,  296,  296,  296,  296,  296,  296,  296,  296,  296,
 /*    50 */   296,  296,  293,  293,  293,  296,  246,  296,  233,  296,
 /*    60 */   226,  226,  215,  218,  218,  215,  296,  296,  296,  296,
 /*    70 */   287,  290,  207,  206,  205,  204,  212,  268,  267,  266,
 /*    80 */   258,  274,  273,  272,  271,  270,  269,  261,  262,  260,
 /*    90 */   296,  296,  296,  254,  296,  296,  211,  227,  296,  296,
 /*   100 */   296,  216,  296,  296,  296,  296,  296,  296,  296,  296,
 /*   110 */   296,  296,  296,  296,  296,  275,  277,  296,  296,  296,
 /*   120 */   296,  296,  239,  243,  242,  253,  255,  252,  251,  250,
 /*   130 */   249,  248,  247,  245,  236,  235,  231,  234,  232,  230,
 /*   140 */   229,  228,  225,  217,  224,  223,  222,  221,  220,  219,
 /*   150 */   214,  209,  208,  213,  263,  289,  291,  285,  288,  276,
 /*   160 */   265,  264,  257,  256,  244,  211,  210,  241,  295,  294,
 /*   170 */   292,  284,  283,  282,  281,  280,  279,  278,  277,  240,
 /*   180 */   259,  238,  237,  203,  202,  201,  200,  199,  198,  197,
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

const YY_RULE_INFO: [YYCODETYPE; 104] = [
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
  85,
  83,
  83,
  84,
  84,
  66,
  66,
  67,
  67,
  68,
  69,
  70,
  71,
  71,
  71,
  89,
  89,
  88,
  88,
  88,
  88,
  88,
  74,
  75,
  75,
  75,
  76,
  76,
  76,
  77,
  77,
  77,
  78,
  78,
  87,
  87,
  87,
  87,
  87,
  91,
  91,
  87,
  79,
  79,
  80,
  80,
  80,
  80,
  81,
  81,
  81,
  82,
  82,
  87,
  87,
  87,
  87,
  87,
  87,
  87,
  87,
  87,
  87,
  87,
  87,
  87,
  87,
  87,
  87,
  87,
  87,
  87,
  87,
  86,
  86,
  86,
  86,
  86,
  86,
  86,
  86,
  86,
  92,
  93,
  93,
  93,
  94,
  95,
  95,
  96,
  97,
  97,
  97,
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
 YYMinorType::YY49(yyres)
}
            ,
            1 /* program ::= stmts */
            => 
{
let yyres :  Ast ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY60(yy0),) => {

	if list::is_empty(&yy0) {
		panic!("null program");
	}
	// ignore yyres, it doesn't really go anywhere for program
	yyres = Ast::Nothing;
	// we're done, so put yy0 in extra
	self.extra = Ok(Ast::ReplRoot(yy0));

},    _ => unreachable!() };
 YYMinorType::YY49(yyres)
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
 YYMinorType::YY60(yyres)
}
            ,
            3 /* stmts ::= stmt stmts */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY60(yy0),YYMinorType::YY60(yy1),) => {

    verbose_out!("found new stmt: {:?}\n", yy0);
	yyres = list::cons(yy0, yy1);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            4 /* stmt ::= let_stmt */
          | 6 /* stmt ::= fail_stmt */
          | 8 /* stmt ::= func_stmt */
          | 9 /* stmt ::= macro_stmt */
          | 10 /* stmt ::= if_stmt */
          | 55 /* pexpr ::= ptuple */
          | 64 /* expr ::= list */
          | 65 /* expr ::= tuple */
          | 83 /* expr ::= term */
          | 92 /* term ::= strexpr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY60(yy0),) => {
 yyres = yy0; 
},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            5 /* stmt ::= expr_stmt */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY60(yy0),) => {

    yyres = yy0;

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
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
 YYMinorType::YY60(yyres)
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
 (YYMinorType::YY173(yy1),YYMinorType::YY60(yy2),) => {

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
 YYMinorType::YY60(yyres)
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
 (YYMinorType::YY24(yy1),YYMinorType::YY60(yy3),) => {

	let letx =
        list::cons(Val::id(yy1),
        list::cons(yy3,
        Val::Nil
        ));
	yyres = sexpr::new(SexprType::Let, letx);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
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
 (YYMinorType::YY24(yy1),YYMinorType::YY60(yy3),) => {

	let bind = list::cons(Val::new_str(yy1), list::singleton(yy3));
	yyres = sexpr::new(SexprType::Fork, bind);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
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
 (YYMinorType::YY60(yy0),) => {

	yyres = yy0;

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            15 /* expr_stmt ::= DollarGT expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY60(yy1),) => {
 yyres = yy1; 
},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            17 /* endblock ::= block_stmts DOUBLEDASH */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY60(yy0),) => {

	yyres = yy0;

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
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
 (YYMinorType::YY60(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
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
 (YYMinorType::YY24(yy1),YYMinorType::YY60(yy3),YYMinorType::YY71(yy5),YYMinorType::YY60(yy6),) => {

	let id = Val::id(yy1);
	let typ = Val::Type(yy5);
	yyres = sexpr::defunc(id, yy3, typ, yy6)

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            23 /* dfunc_args ::= */
          | 94 /* list_items ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

	yyres = list::empty();

} };
 YYMinorType::YY60(yyres)
}
            ,
            24 /* dfunc_args ::= ID opt_typex */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY24(yy0),YYMinorType::YY71(yy1),) => {

	yyres = list::singleton(sexpr::id_with_type(yy0, yy1));

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
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
 (YYMinorType::YY24(yy0),YYMinorType::YY71(yy1),YYMinorType::YY60(yy3),) => {

	yyres = list::cons(sexpr::id_with_type(yy0, yy1), yy3);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
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
 YYMinorType::YY71(yyres)
}
            ,
            27 /* opt_typex ::= COLON typex */
            => 
{
let yyres :  Type ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY71(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY71(yyres)
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
 YYMinorType::YY71(yyres)
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
 YYMinorType::YY71(yyres)
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
 YYMinorType::YY71(yyres)
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
 YYMinorType::YY71(yyres)
}
            ,
            32 /* typex ::= TYPE_ID */
            => 
{
let yyres :  Type ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY24(yy0),) => {

	yyres = Type::Id(Arc::new(yy0));

},    _ => unreachable!() };
 YYMinorType::YY71(yyres)
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
 (YYMinorType::YY24(yy1),YYMinorType::YY60(yy3),YYMinorType::YY60(yy5),) => {

    verbose_out!("found macro {:?}\n", yy1);
    yyres = sexpr::new(SexprType::DefMacro,
        list::cons(Val::id(yy1),
        list::cons(yy3,
        list::cons(yy5,
        Val::Nil
    ))));

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
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
 YYMinorType::YY60(yyres)
}
            ,
            35 /* macro_args ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY24(yy0),) => {

    yyres = list::singleton(Val::id(yy0));

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
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
 (YYMinorType::YY24(yy0),YYMinorType::YY60(yy2),) => {

    yyres = list::cons(Val::id(yy0), yy2);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
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
 (YYMinorType::YY60(yy1),YYMinorType::YY60(yy2),) => {

    /* if-only style */
    yyres = sexpr::ifstmt(yy1, yy2, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
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
 (YYMinorType::YY60(yy1),YYMinorType::YY60(yy2),YYMinorType::YY60(yy3),) => {

    /* if-else style */
    yyres = sexpr::ifstmt(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
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
 (YYMinorType::YY60(yy1),) => {

    /* case-expr style */
    yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
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
 (YYMinorType::YY60(yy2),YYMinorType::YY60(yy3),YYMinorType::YY60(yy4),) => {

    yyres = sexpr::ifstmt(yy2, yy3, yy4);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
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
 (YYMinorType::YY60(yy2),YYMinorType::YY60(yy3),) => {

    yyres = sexpr::ifstmt(yy2, yy3, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            42 /* else_if ::= ELSE midblock */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY60(yy1),) => {

    yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
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
 (YYMinorType::YY60(yy1),YYMinorType::YY60(yy2),YYMinorType::YY60(yy3),) => {

    yyres = sexpr::ifstmt(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
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
 (YYMinorType::YY60(yy2),) => {

    yyres = yy2;

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
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
 (YYMinorType::YY24(yy0),) => {

	verbose_out!("zero param function call!");
	yyres = sexpr::call(yy0, Val::Nil);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
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
 (YYMinorType::YY24(yy0),YYMinorType::YY60(yy2),) => {

	verbose_out!("one param function call!");
	let args = list::singleton(yy2);
	yyres = sexpr::call(yy0, args);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
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
 (YYMinorType::YY24(yy0),YYMinorType::YY60(yy2),) => {

	verbose_out!("multi param function call!");
	yyres = sexpr::call(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            48 /* expr ::= term DOLLAR term */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY60(yy0),YYMinorType::YY60(yy2),) => {

	/* yyres = Val::binaryop(yy0, yy2, D); */
	yyres = Val::Void;

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            49 /* expr ::= CASE cases DOUBLEDASH */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY60(yy1),) => {

    verbose_out!("parsed case expr\n");
	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            50 /* cases ::= PIPE expr midblock PIPE ELSE midblock */
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
 (YYMinorType::YY60(yy1),YYMinorType::YY60(yy2),YYMinorType::YY60(yy5),) => {

    verbose_out!("found cases base\n");
    yyres = sexpr::casex(yy1, yy2, yy5);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            51 /* cases ::= PIPE expr midblock cases */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,yyp3.minor,) {
 (YYMinorType::YY60(yy1),YYMinorType::YY60(yy2),YYMinorType::YY60(yy3),) => {

    verbose_out!("found extra case\n");
    yyres = sexpr::casex(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            52 /* expr ::= MATCH expr match_case DOUBLEDASH */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY60(yy1),YYMinorType::YY60(yy2),) => {

    verbose_out!("parsed match expr\n");
    yyres = sexpr::match_expr(yy1, yy2);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            53 /* match_case ::= PIPE pexpr midblock match_case */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,yyp3.minor,) {
 (YYMinorType::YY60(yy1),YYMinorType::YY60(yy2),YYMinorType::YY60(yy3),) => {

    verbose_out!("found cases base\n");
    yyres = sexpr::match_case(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            54 /* match_case ::= PIPE pexpr midblock */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY60(yy1),YYMinorType::YY60(yy2),) => {

    verbose_out!("parsed base match case\n");
    yyres = sexpr::match_case(yy1, yy2, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            56 /* pexpr ::= INT */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY136(yy0),) => {
 yyres = Val::Int(yy0); 
},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            57 /* pexpr ::= ID */
          | 85 /* term ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY24(yy0),) => {
 yyres = Val::id(yy0); 
},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            58 /* pexpr ::= UNDERSCORE */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Wildcard; 
} };
 YYMinorType::YY60(yyres)
}
            ,
            59 /* ptuple ::= LPAREN RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Val::Tuple(vec![]);

} };
 YYMinorType::YY60(yyres)
}
            ,
            60 /* ptuple ::= LPAREN pexpr RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY60(yy1),) => {

	yyres = Val::Tuple(vec![yy1]);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            61 /* ptuple ::= LPAREN pargs RPAREN */
          | 97 /* tuple ::= LPAREN tuple_args RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY60(yy1),) => {

	yyres = Val::tuple_from_list(yy1);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            62 /* pargs ::= pexpr COMMA pexpr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY60(yy0),YYMinorType::YY60(yy2),) => {

	yyres = list::cons(yy0,
        list::cons(yy2,
        Val::Nil
        ));

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            63 /* pargs ::= pexpr COMMA pargs */
          | 96 /* list_items ::= expr COMMA list_items */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY60(yy0),YYMinorType::YY60(yy2),) => {

	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            66 /* expr ::= NOT expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY60(yy1),) => {

	yyres = sexpr::call("bool_not".to_string(), list::singleton(yy1));

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            67 /* expr ::= expr ConcatNewline */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY60(yy0),) => {

	let newline = Val::Str(Arc::new("\n".to_string()));
	let args = list::cons(yy0, list::singleton(newline));
	yyres = sexpr::new(SexprType::StrExpr, args)

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            68 /* expr ::= MINUS expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY60(yy1),) => {

	yyres = sexpr::call("negate".to_string(), list::singleton(yy1));

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            69 /* expr ::= expr PLUS expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY60(yy0),YYMinorType::YY60(yy2),) => {

	yyres = sexpr::binaryop("int_add".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            70 /* expr ::= expr MINUS expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY60(yy0),YYMinorType::YY60(yy2),) => {

	yyres = sexpr::binaryop("int_sub".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            71 /* expr ::= expr TIMES expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY60(yy0),YYMinorType::YY60(yy2),) => {

	yyres = sexpr::binaryop("int_mult".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            72 /* expr ::= expr SLASH expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY60(yy0),YYMinorType::YY60(yy2),) => {

	yyres = sexpr::binaryop("int_div".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            73 /* expr ::= expr MOD expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY60(yy0),YYMinorType::YY60(yy2),) => {

	yyres = sexpr::binaryop("int_mod".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            74 /* expr ::= expr AND expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY60(yy0),YYMinorType::YY60(yy2),) => {

	yyres = sexpr::binaryop("and".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            75 /* expr ::= expr OR expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY60(yy0),YYMinorType::YY60(yy2),) => {

	yyres = sexpr::binaryop("or".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            76 /* expr ::= expr XOR expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY60(yy0),YYMinorType::YY60(yy2),) => {

	yyres = sexpr::binaryop("xor".to_string(),yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            77 /* expr ::= expr LT expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY60(yy0),YYMinorType::YY60(yy2),) => {

	yyres = sexpr::binaryop("less_than".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            78 /* expr ::= expr LTEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY60(yy0),YYMinorType::YY60(yy2),) => {

	yyres = sexpr::binaryop("less_than_equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            79 /* expr ::= expr GT expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY60(yy0),YYMinorType::YY60(yy2),) => {

	yyres = sexpr::binaryop("greater_than".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            80 /* expr ::= expr GTEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY60(yy0),YYMinorType::YY60(yy2),) => {

	yyres = sexpr::binaryop("greater_than_equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            81 /* expr ::= expr EQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY60(yy0),YYMinorType::YY60(yy2),) => {

	yyres = sexpr::binaryop("equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            82 /* expr ::= expr NEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY60(yy0),YYMinorType::YY60(yy2),) => {

	let eq = sexpr::binaryop("equal".to_string(), yy0, yy2);
	yyres = sexpr::call("bool_not".to_string(), list::singleton(eq));

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            84 /* term ::= LPAREN expr RPAREN */
          | 93 /* list ::= SquareL list_items SquareR */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY60(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            86 /* term ::= VOID */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Val::Void;

} };
 YYMinorType::YY60(yyres)
}
            ,
            87 /* term ::= DollarQuestion */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Val::id("$".to_string());

} };
 YYMinorType::YY60(yyres)
}
            ,
            88 /* term ::= INT */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY136(yy0),) => {

	yyres = Val::Int(yy0);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            89 /* term ::= True */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Bool(true); 
} };
 YYMinorType::YY60(yyres)
}
            ,
            90 /* term ::= False */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Bool(false); 
} };
 YYMinorType::YY60(yyres)
}
            ,
            91 /* term ::= HASHTAG */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY173(yy0),) => {

	yyres = Val::Hashtag(Arc::new(yy0.data));

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            95 /* list_items ::= expr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY60(yy0),) => {

	yyres = list::singleton(yy0);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            98 /* tuple_args ::= expr COMMA expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY60(yy0),YYMinorType::YY60(yy2),) => {

	verbose_out!("base tuple args!");
	yyres = list::cons(yy0, list::singleton(yy2));

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            99 /* tuple_args ::= expr COMMA tuple_args */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY60(yy0),YYMinorType::YY60(yy2),) => {

	verbose_out!("additional tuple arg!");
	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            100 /* strexpr ::= StrOpen strlist StrClose */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY60(yy1),) => {

	yyres = sexpr::strexpr(yy1);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            101 /* strlist ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

	yyres = Val::Nil;

} };
 YYMinorType::YY60(yyres)
}
            ,
            102 /* strlist ::= StrLit strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY24(yy0),YYMinorType::YY60(yy1),) => {

	yyres = list::cons(Val::new_str(yy0), yy1);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            103 /* strlist ::= ID strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY24(yy0),YYMinorType::YY60(yy1),) => {

	yyres = list::cons(Val::id(yy0), yy1);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
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

