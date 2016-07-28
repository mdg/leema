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
const YYNSTATE: i32 = 197;
const YYNRULE: i32 = 107;
const YYERRORSYMBOL: i32 = 61;

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
    MATCH, //50
    UNDERSCORE, //51
    MOD, //52
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
pub const TOKEN_MATCH: i32 = 50;
pub const TOKEN_UNDERSCORE: i32 = 51;
pub const TOKEN_MOD: i32 = 52;
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
        &Token::MATCH => TOKEN_MATCH,
        &Token::UNDERSCORE => TOKEN_UNDERSCORE,
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
const YY_ACTTAB_COUNT: i32 = 776;
const YY_ACTION: [YYACTIONTYPE; 776] = [
 /*     0 */   177,  122,  180,   97,   53,  151,   83,  170,  169,  167,
 /*    10 */    34,  166,  115,  176,    1,   28,   41,   97,   55,  141,
 /*    20 */   170,  169,   15,  167,    8,  166,  127,  176,  119,  192,
 /*    30 */   123,  118,  114,   13,  131,  112,  155,  154,  153,  152,
 /*    40 */    32,  107,    3,  134,  126,   95,   31,   50,   12,  182,
 /*    50 */   181,  179,  178,    6,  101,   81,  177,  122,  180,   97,
 /*    60 */    76,  125,   97,   38,  197,  167,  198,  166,  167,  176,
 /*    70 */   166,   28,  176,   97,   57,    4,  137,  138,   15,  167,
 /*    80 */     8,  166,  162,  176,   51,  192,  123,  118,  114,   13,
 /*    90 */    49,  112,   79,  177,  183,  180,   80,  107,    3,   40,
 /*   100 */   135,   95,   31,   47,  139,  182,  181,  179,  178,    6,
 /*   110 */     7,   81,   34,  175,    1,    9,   26,   35,   82,  170,
 /*   120 */   169,  171,  136,   23,   22,   24,  185,  117,   21,   20,
 /*   130 */    17,   16,   19,   18,   29,   27,  157,  186,  159,  109,
 /*   140 */    32,  168,  182,  181,  179,  178,   97,   56,   81,   97,
 /*   150 */    58,   26,  167,  116,  166,  167,  176,  166,   30,  176,
 /*   160 */    25,  113,  163,    7,  188,   97,   56,  161,    9,   26,
 /*   170 */    27,  167,  164,  166,  176,  176,   23,   22,   24,  185,
 /*   180 */    14,   21,   20,   17,   16,   19,   18,   29,   27,   91,
 /*   190 */   165,    9,   26,  184,  111,   25,    1,  158,   92,   23,
 /*   200 */    22,   24,  185,  176,   21,   20,   17,   16,   19,   18,
 /*   210 */    29,   27,  149,   25,  159,  109,   89,  106,   97,   54,
 /*   220 */    88,   87,   32,   45,  167,    7,  166,   98,  176,  147,
 /*   230 */     9,   26,  100,  139,  133,  146,   25,  143,   23,   22,
 /*   240 */    24,  185,   41,   21,   20,   17,   16,   19,   18,   29,
 /*   250 */    27,  132,   97,   39,  128,   97,   64,   33,  167,  120,
 /*   260 */   166,  167,  176,  166,   34,  176,   97,   48,  130,  170,
 /*   270 */   169,  172,  167,   25,  166,   25,  176,  110,    5,   97,
 /*   280 */    63,  176,   90,    9,   26,  167,  150,  166,   75,  176,
 /*   290 */   104,   23,   22,   24,  185,  105,   21,   20,   17,   16,
 /*   300 */    19,   18,   29,   27,    9,   26,  144,  170,  169,   96,
 /*   310 */   139,   99,   23,   22,   24,  185,   46,   21,   20,   17,
 /*   320 */    16,   19,   18,   29,   27,  108,  165,  148,   25,   42,
 /*   330 */   102,  145,   97,   77,  142,   11,    9,   26,  167,  140,
 /*   340 */   166,   52,  176,   10,    9,   26,  137,  138,  129,   25,
 /*   350 */   156,   17,   16,   19,   18,   29,   27,    9,   26,  304,
 /*   360 */   304,  304,  304,   29,   27,   23,   22,   24,  185,   40,
 /*   370 */    21,   20,   17,   16,   19,   18,   29,   27,   97,   68,
 /*   380 */   121,   25,  306,  174,  167,  306,  166,  173,  176,   25,
 /*   390 */    97,   94,  136,  306,  306,  306,  167,   52,  166,  306,
 /*   400 */   176,  306,   25,  306,   34,    9,   26,  306,  306,  306,
 /*   410 */    86,  170,  103,   23,   22,   24,  185,  306,   21,   20,
 /*   420 */    17,   16,   19,   18,   29,   27,   44,  177,  122,  180,
 /*   430 */   306,  305,  124,    2,  306,  306,  306,  306,  306,  191,
 /*   440 */    34,  306,   28,  190,  306,  189,   85,  170,  169,   15,
 /*   450 */    25,    8,  195,  194,  193,   97,   60,  306,   97,   93,
 /*   460 */   306,  167,  306,  166,  167,  176,  166,  306,  176,  306,
 /*   470 */   306,  306,   95,   31,  306,  306,  182,  181,  179,  178,
 /*   480 */     6,  306,   81,   43,  177,  122,  180,   97,   62,  196,
 /*   490 */     2,  306,  306,  167,  306,  166,  191,  176,  306,   28,
 /*   500 */   190,  306,  189,  306,  306,  306,   15,  306,    8,  195,
 /*   510 */   194,  193,   97,   60,  306,   97,   67,  306,  167,  306,
 /*   520 */   166,  167,  176,  166,  306,  176,  306,  306,  306,   95,
 /*   530 */    31,  306,  306,  182,  181,  179,  178,    6,  306,   81,
 /*   540 */   177,  122,  180,  306,  306,  306,  160,    2,   97,   66,
 /*   550 */   306,  306,  306,  191,  167,   28,  166,  190,  176,  189,
 /*   560 */   306,  306,   15,  306,    8,  187,  195,  194,  193,   97,
 /*   570 */    60,  306,   97,   65,  306,  167,  306,  166,  167,  176,
 /*   580 */   166,  306,  176,  306,  306,   95,   31,  306,  306,  182,
 /*   590 */   181,  179,  178,    6,  306,   81,  177,  122,  180,   97,
 /*   600 */    74,  306,   97,   73,  306,  167,  306,  166,  167,  176,
 /*   610 */   166,   28,  176,  306,  306,  306,   97,   72,   15,  306,
 /*   620 */     8,  306,  167,  306,  166,  306,  176,   97,   71,  306,
 /*   630 */   306,  306,  306,  167,  306,  166,  306,  176,  306,   11,
 /*   640 */   306,   95,   31,  306,  306,  182,  181,  179,  178,    6,
 /*   650 */   306,   81,  177,  122,  180,   97,   70,  306,  306,  306,
 /*   660 */   306,  167,  306,  166,  306,  176,  306,   28,  306,   97,
 /*   670 */    69,  306,   97,   78,   15,  167,    8,  166,  167,  176,
 /*   680 */   166,  306,  176,   97,   61,  306,   97,   59,  306,  167,
 /*   690 */   306,  166,  167,  176,  166,  306,  176,   95,   31,  306,
 /*   700 */   306,  182,  181,  179,  178,    6,  306,   81,   34,    9,
 /*   710 */    26,  306,  306,  306,   84,  170,  169,  306,  306,   24,
 /*   720 */   185,  306,   21,   20,   17,   16,   19,   18,   29,   27,
 /*   730 */     9,   26,   97,   37,  306,  306,  306,  306,  167,  306,
 /*   740 */   166,  185,  176,   21,   20,   17,   16,   19,   18,   29,
 /*   750 */    27,  306,   97,   36,   25,  306,  306,  306,  167,  306,
 /*   760 */   166,  306,  176,  306,  306,  306,  306,  306,  306,  306,
 /*   770 */   306,  306,  306,  306,  306,   25,
];
const YY_LOOKAHEAD: [YYCODETYPE; 776] = [
 /*     0 */     4,    5,    6,   86,   87,   10,   67,   68,   69,   92,
 /*    10 */    61,   94,   95,   96,   12,   19,    2,   86,   87,   67,
 /*    20 */    68,   69,   26,   92,   28,   94,   30,   96,   79,   33,
 /*    30 */    34,   35,   36,   37,   61,   39,   41,   42,   43,   44,
 /*    40 */    38,   45,   46,   29,   31,   49,   50,    5,   46,   53,
 /*    50 */    54,   55,   56,   57,   78,   59,    4,    5,    6,   86,
 /*    60 */    87,   32,   86,   87,    0,   92,    0,   94,   92,   96,
 /*    70 */    94,   19,   96,   86,   87,   28,    5,    6,   26,   92,
 /*    80 */    28,   94,   95,   96,    4,   33,   34,   35,   36,   37,
 /*    90 */    48,   39,    5,    4,    5,    6,    9,   45,   46,   28,
 /*   100 */    29,   49,   50,   80,   81,   53,   54,   55,   56,   57,
 /*   110 */     2,   59,   61,   60,   12,    7,    8,   28,   67,   68,
 /*   120 */    69,   38,   51,   15,   16,   17,   18,    5,   20,   21,
 /*   130 */    22,   23,   24,   25,   26,   27,   66,   29,   68,   69,
 /*   140 */    38,   38,   53,   54,   55,   56,   86,   87,   59,   86,
 /*   150 */    87,    8,   92,   93,   94,   92,   96,   94,   22,   96,
 /*   160 */    52,    5,   58,    2,   86,   86,   87,   29,    7,    8,
 /*   170 */    27,   92,   93,   94,   96,   96,   15,   16,   17,   18,
 /*   180 */    22,   20,   21,   22,   23,   24,   25,   26,   27,   29,
 /*   190 */    29,    7,    8,   86,    5,   52,   12,   38,   28,   15,
 /*   200 */    16,   17,   18,   96,   20,   21,   22,   23,   24,   25,
 /*   210 */    26,   27,   66,   52,   68,   69,    2,    5,   86,   87,
 /*   220 */    28,    2,   38,   29,   92,    2,   94,   95,   96,   38,
 /*   230 */     7,    8,   80,   81,   82,   38,   52,   38,   15,   16,
 /*   240 */    17,   18,    2,   20,   21,   22,   23,   24,   25,   26,
 /*   250 */    27,   29,   86,   87,   29,   86,   87,   47,   92,   91,
 /*   260 */    94,   92,   96,   94,   61,   96,   86,   87,   67,   68,
 /*   270 */    69,   86,   92,   52,   94,   52,   96,   71,    2,   86,
 /*   280 */    87,   96,    5,    7,    8,   92,   71,   94,   40,   96,
 /*   290 */     5,   15,   16,   17,   18,   75,   20,   21,   22,   23,
 /*   300 */    24,   25,   26,   27,    7,    8,   67,   68,   69,   80,
 /*   310 */    81,   82,   15,   16,   17,   18,   89,   20,   21,   22,
 /*   320 */    23,   24,   25,   26,   27,   89,   29,   75,   52,    3,
 /*   330 */    77,   77,   86,   87,   78,   47,    7,    8,   92,   79,
 /*   340 */    94,   47,   96,   47,    7,    8,    5,    6,   91,   52,
 /*   350 */    88,   22,   23,   24,   25,   26,   27,    7,    8,   22,
 /*   360 */    23,   24,   25,   26,   27,   15,   16,   17,   18,   28,
 /*   370 */    20,   21,   22,   23,   24,   25,   26,   27,   86,   87,
 /*   380 */    97,   52,   98,   97,   92,   98,   94,   97,   96,   52,
 /*   390 */    86,   87,   51,   98,   98,   98,   92,   47,   94,   98,
 /*   400 */    96,   98,   52,   98,   61,    7,    8,   98,   98,   98,
 /*   410 */    67,   68,   69,   15,   16,   17,   18,   98,   20,   21,
 /*   420 */    22,   23,   24,   25,   26,   27,    3,    4,    5,    6,
 /*   430 */    98,   62,   63,   64,   98,   98,   98,   98,   98,   70,
 /*   440 */    61,   98,   19,   74,   98,   76,   67,   68,   69,   26,
 /*   450 */    52,   28,   83,   84,   85,   86,   87,   98,   86,   87,
 /*   460 */    98,   92,   98,   94,   92,   96,   94,   98,   96,   98,
 /*   470 */    98,   98,   49,   50,   98,   98,   53,   54,   55,   56,
 /*   480 */    57,   98,   59,    3,    4,    5,    6,   86,   87,   63,
 /*   490 */    64,   98,   98,   92,   98,   94,   70,   96,   98,   19,
 /*   500 */    74,   98,   76,   98,   98,   98,   26,   98,   28,   83,
 /*   510 */    84,   85,   86,   87,   98,   86,   87,   98,   92,   98,
 /*   520 */    94,   92,   96,   94,   98,   96,   98,   98,   98,   49,
 /*   530 */    50,   98,   98,   53,   54,   55,   56,   57,   98,   59,
 /*   540 */     4,    5,    6,   98,   98,   98,   63,   64,   86,   87,
 /*   550 */    98,   98,   98,   70,   92,   19,   94,   74,   96,   76,
 /*   560 */    98,   98,   26,   98,   28,   29,   83,   84,   85,   86,
 /*   570 */    87,   98,   86,   87,   98,   92,   98,   94,   92,   96,
 /*   580 */    94,   98,   96,   98,   98,   49,   50,   98,   98,   53,
 /*   590 */    54,   55,   56,   57,   98,   59,    4,    5,    6,   86,
 /*   600 */    87,   98,   86,   87,   98,   92,   98,   94,   92,   96,
 /*   610 */    94,   19,   96,   98,   98,   98,   86,   87,   26,   98,
 /*   620 */    28,   98,   92,   98,   94,   98,   96,   86,   87,   98,
 /*   630 */    98,   98,   98,   92,   98,   94,   98,   96,   98,   47,
 /*   640 */    98,   49,   50,   98,   98,   53,   54,   55,   56,   57,
 /*   650 */    98,   59,    4,    5,    6,   86,   87,   98,   98,   98,
 /*   660 */    98,   92,   98,   94,   98,   96,   98,   19,   98,   86,
 /*   670 */    87,   98,   86,   87,   26,   92,   28,   94,   92,   96,
 /*   680 */    94,   98,   96,   86,   87,   98,   86,   87,   98,   92,
 /*   690 */    98,   94,   92,   96,   94,   98,   96,   49,   50,   98,
 /*   700 */    98,   53,   54,   55,   56,   57,   98,   59,   61,    7,
 /*   710 */     8,   98,   98,   98,   67,   68,   69,   98,   98,   17,
 /*   720 */    18,   98,   20,   21,   22,   23,   24,   25,   26,   27,
 /*   730 */     7,    8,   86,   87,   98,   98,   98,   98,   92,   98,
 /*   740 */    94,   18,   96,   20,   21,   22,   23,   24,   25,   26,
 /*   750 */    27,   98,   86,   87,   52,   98,   98,   98,   92,   98,
 /*   760 */    94,   98,   96,   98,   98,   98,   98,   98,   98,   98,
 /*   770 */    98,   98,   98,   98,   98,   52,
];
const YY_SHIFT_USE_DFLT: i32 = -6;
const YY_SHIFT_COUNT: i32 = 127;
const YY_SHIFT_MIN: i32 = -5;
const YY_SHIFT_MAX: i32 = 723;
const YY_SHIFT_OFST: [i16; 128] = [
 /*     0 */    -4,   52,   52,  592,  536,  648,  648,  648,  648,  648,
 /*    10 */   480,  423,  648,  648,  648,  648,  648,  648,  648,  648,
 /*    20 */   648,  648,  648,  648,  648,  648,  648,  648,  648,  648,
 /*    30 */   648,  648,  648,  648,  648,  648,  184,  184,  184,  184,
 /*    40 */    71,  341,    2,  102,  102,  102,  102,  102,  350,   89,
 /*    50 */    89,   89,  341,  161,  108,  297,  276,  223,  398,  398,
 /*    60 */   398,  398,  398,  398,  398,  702,  702,  723,  723,  337,
 /*    70 */   337,  337,  337,  329,  329,   -5,  143,  143,  143,   87,
 /*    80 */    87,   87,  296,  294,  288,  326,  326,  285,  285,  277,
 /*    90 */   248,  248,  277,  221,  221,  210,   14,   42,  225,  222,
 /*   100 */   240,  199,  197,  191,  219,  194,  192,  212,  214,  159,
 /*   110 */   160,  170,  189,  158,  156,  138,  104,  136,  122,  103,
 /*   120 */    83,   53,   47,   80,   66,   64,   29,   13,
];
const YY_REDUCE_USE_DFLT: i32 = -84;
const YY_REDUCE_COUNT: i32 = 95;
const YY_REDUCE_MIN: i32 = -83;
const YY_REDUCE_MAX: i32 = 666;
const YY_REDUCE_OFST: [i16; 96] = [
 /*     0 */   369,  483,  426,  -24,  132,   79,   60,  -13,  -83,  -27,
 /*    10 */   166,  666,  646,  600,  597,  586,  583,  569,  541,  530,
 /*    20 */   516,  513,  486,  462,  429,  401,  372,  304,  292,  246,
 /*    30 */   193,  180,  169,  166,   63,  -69,  647,  379,  343,   51,
 /*    40 */   229,  152,  239,  201,  -48,  146,   70,  -61,  -51,  185,
 /*    50 */   107,   78,   23,  203,  203,  203,  203,  203,  203,  203,
 /*    60 */   203,  203,  203,  203,  203,  203,  203,  203,  203,  203,
 /*    70 */   203,  203,  203,  203,  203,  262,  203,  203,  203,  290,
 /*    80 */   286,  283,  257,  260,  256,  254,  253,  252,  220,  215,
 /*    90 */   236,  227,  206,  203,  203,  168,
];
const YY_DEFAULT: [YYACTIONTYPE; 197] = [
 /*     0 */   199,  199,  199,  304,  304,  294,  294,  304,  304,  304,
 /*    10 */   304,  304,  304,  304,  304,  304,  304,  304,  304,  304,
 /*    20 */   304,  304,  304,  304,  304,  304,  304,  304,  304,  304,
 /*    30 */   304,  304,  304,  304,  304,  304,  304,  304,  304,  304,
 /*    40 */   304,  304,  304,  304,  304,  304,  304,  304,  304,  304,
 /*    50 */   304,  304,  304,  304,  304,  304,  295,  298,  267,  212,
 /*    60 */   211,  210,  273,  209,  217,  276,  275,  274,  264,  282,
 /*    70 */   281,  280,  279,  278,  277,  304,  268,  270,  266,  301,
 /*    80 */   301,  301,  304,  252,  304,  238,  304,  231,  231,  220,
 /*    90 */   223,  223,  220,  272,  271,  304,  304,  283,  304,  304,
 /*   100 */   260,  304,  304,  216,  232,  304,  304,  304,  221,  304,
 /*   110 */   304,  304,  304,  304,  304,  304,  304,  304,  304,  304,
 /*   120 */   304,  304,  285,  304,  304,  304,  304,  304,  244,  249,
 /*   130 */   248,  269,  259,  261,  258,  257,  256,  255,  254,  253,
 /*   140 */   251,  241,  240,  236,  239,  237,  235,  234,  233,  230,
 /*   150 */   222,  229,  228,  227,  226,  225,  224,  219,  214,  213,
 /*   160 */   218,  297,  299,  293,  296,  284,  263,  262,  250,  216,
 /*   170 */   215,  247,  246,  303,  302,  300,  292,  291,  290,  289,
 /*   180 */   288,  287,  286,  285,  245,  265,  243,  242,  208,  207,
 /*   190 */   206,  205,  204,  203,  202,  201,  200,
];

/* TMPL: fallback tokens */

const YY_FALLBACK: [i32; 0] = [
];

/* TMPL: symbol names */


/* TMPL: rules */


/* TMPL: destructors */


/* TMPL: stack-overflow */


/* TMPL: stack-overflow */

const YY_RULE_INFO: [YYCODETYPE; 107] = [
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
          | 56 /* pexpr ::= ptuple */
          | 65 /* expr ::= list */
          | 66 /* expr ::= tuple */
          | 86 /* expr ::= term */
          | 95 /* term ::= strexpr */
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
          | 97 /* list_items ::= */
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
            48 /* expr ::= term ID term */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY60(yy0),YYMinorType::YY24(yy1),YYMinorType::YY60(yy2),) => {

	yyres = sexpr::binaryop(yy1, yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
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
 (YYMinorType::YY60(yy0),YYMinorType::YY60(yy2),) => {

	/* yyres = Val::binaryop(yy0, yy2, D); */
	yyres = Val::Void;

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
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
 (YYMinorType::YY60(yy1),) => {

    verbose_out!("parsed case expr\n");
	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
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
 (YYMinorType::YY60(yy1),YYMinorType::YY60(yy2),YYMinorType::YY60(yy5),) => {

    verbose_out!("found cases base\n");
    yyres = sexpr::casex(yy1, yy2, yy5);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
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
 (YYMinorType::YY60(yy1),YYMinorType::YY60(yy2),YYMinorType::YY60(yy3),) => {

    verbose_out!("found extra case\n");
    yyres = sexpr::casex(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            53 /* expr ::= MATCH expr match_case DOUBLEDASH */
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
            54 /* match_case ::= PIPE pexpr midblock match_case */
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
            55 /* match_case ::= PIPE pexpr midblock */
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
            57 /* pexpr ::= INT */
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
            58 /* pexpr ::= ID */
          | 88 /* term ::= ID */
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
            59 /* pexpr ::= UNDERSCORE */
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
            60 /* ptuple ::= LPAREN RPAREN */
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
            61 /* ptuple ::= LPAREN pexpr RPAREN */
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
            62 /* ptuple ::= LPAREN pargs RPAREN */
          | 100 /* tuple ::= LPAREN tuple_args RPAREN */
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
            63 /* pargs ::= pexpr COMMA pexpr */
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
            64 /* pargs ::= pexpr COMMA pargs */
          | 99 /* list_items ::= expr COMMA list_items */
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
            67 /* expr ::= NOT expr */
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
            68 /* expr ::= expr ConcatNewline */
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
            69 /* expr ::= MINUS expr */
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
            70 /* expr ::= expr error expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY60(yy0),YYMinorType::YY60(yy2),) => {

    write!(stderr(), "binaryop error: {:?} err {:?}\n", yy0, yy2).ok();
    yyres = Val::Void;

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            71 /* expr ::= expr PLUS expr */
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
            72 /* expr ::= expr PLUS error */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY60(yy0),) => {

    write!(stderr(), "wtf PLUS error: {:?} + error\n", yy0).ok();
    yyres = Val::Void;

},    _ => unreachable!() };
 YYMinorType::YY60(yyres)
}
            ,
            73 /* expr ::= expr MINUS expr */
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
            74 /* expr ::= expr TIMES expr */
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
            75 /* expr ::= expr SLASH expr */
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
            76 /* expr ::= expr MOD expr */
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
            77 /* expr ::= expr AND expr */
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
            78 /* expr ::= expr OR expr */
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
            79 /* expr ::= expr XOR expr */
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
            80 /* expr ::= expr LT expr */
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
            81 /* expr ::= expr LTEQ expr */
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
            82 /* expr ::= expr GT expr */
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
            83 /* expr ::= expr GTEQ expr */
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
            84 /* expr ::= expr EQ expr */
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
            85 /* expr ::= expr NEQ expr */
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
            87 /* term ::= LPAREN expr RPAREN */
          | 96 /* list ::= SquareL list_items SquareR */
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
            89 /* term ::= VOID */
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
            90 /* term ::= DollarQuestion */
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
            91 /* term ::= INT */
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
            92 /* term ::= True */
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
            93 /* term ::= False */
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
            94 /* term ::= HASHTAG */
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
            98 /* list_items ::= expr */
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
            101 /* tuple_args ::= expr COMMA expr */
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
            102 /* tuple_args ::= expr COMMA tuple_args */
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
            103 /* strexpr ::= StrOpen strlist StrClose */
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
            104 /* strlist ::= */
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
            105 /* strlist ::= StrLit strlist */
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
            106 /* strlist ::= ID strlist */
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

