#![allow(dead_code)]
#![allow(unused_variables)]
/* TMPL: %include */

use leema::ast::{TokenLoc, TokenData};
use leema::val::{Val, SexprType, Type};
use leema::list;
use leema::log;
use leema::sexpr;
use std::sync::Arc;
use std::io::{stderr, Write};
/* TMPL: makeheader cruft */


/* TMPL: types */

type YYCODETYPE = i8;
const YYNOCODE: i32 = 108;
type YYACTIONTYPE = u16;
const YYWILDCARD: YYCODETYPE = 1;
enum YYMinorType {
    YY0,
    YY39(Type),
    YY82(Val),
    YY132(TokenLoc),
    YY158(String),
    YY186(i64),
    YY209(TokenData<String>),
}
const YYNSTATE: i32 = 242;
const YYNRULE: i32 = 129;
const YYERRORSYMBOL: i32 = 0;

//const YY_NO_ACTION: i32 = YYNSTATE+YYNRULE+2;
//const YY_ACCEPT_ACTION: i32 = YYNSTATE+YYNRULE+1;
//const YY_ERROR_ACTION: i32 = YYNSTATE+YYNRULE+1;

/* TMPL: action tables */

#[derive(Debug,Clone,PartialEq
)]
pub enum Token {
    EOI, //0
    ANY, //1
    COLON( TokenLoc ), //2
    COMMA( TokenLoc ), //3
    DOUBLEDASH( TokenLoc ), //4
    ELSE( TokenLoc ), //5
    HASHTAG( TokenData<String> ), //6
    ID( TokenData<String> ), //7
    INT( i64 ), //8
    Let( TokenLoc ), //9
    PLUS( TokenLoc ), //10
    SEMICOLON( TokenLoc ), //11
    SLASH( TokenLoc ), //12
    SquareL( TokenLoc ), //13
    SquareR( TokenLoc ), //14
    StrLit( String ), //15
    TYPE_ID( TokenData<String> ), //16
    ASSIGN, //17
    BLOCKARROW, //18
    RETURN, //19
    OR, //20
    XOR, //21
    AND, //22
    ConcatNewline, //23
    NOT, //24
    EQ, //25
    NEQ, //26
    GT, //27
    GTEQ, //28
    LT, //29
    LTEQ, //30
    MINUS, //31
    TIMES, //32
    MOD, //33
    DOLLAR, //34
    DOT, //35
    LPAREN, //36
    RPAREN, //37
    IMPORT, //38
    STRUCT, //39
    FAIL, //40
    FAILED, //41
    Fork, //42
    Func, //43
    PARENCALL, //44
    TYPE_INT, //45
    TYPE_STR, //46
    TYPE_HASHTAG, //47
    TYPE_BOOL, //48
    TYPE_VOID, //49
    PS, //50
    MACRO, //51
    IF, //52
    PIPE, //53
    CASE, //54
    MATCH, //55
    True, //56
    False, //57
    UNDERSCORE, //58
    NEGATE, //59
    VOID, //60
    DollarQuestion, //61
    StrOpen, //62
    StrClose, //63
}
pub const TOKEN_EOI: i32 = 0;
pub const TOKEN_ANY: i32 = 1;
pub const TOKEN_COLON: i32 = 2;
pub const TOKEN_COMMA: i32 = 3;
pub const TOKEN_DOUBLEDASH: i32 = 4;
pub const TOKEN_ELSE: i32 = 5;
pub const TOKEN_HASHTAG: i32 = 6;
pub const TOKEN_ID: i32 = 7;
pub const TOKEN_INT: i32 = 8;
pub const TOKEN_Let: i32 = 9;
pub const TOKEN_PLUS: i32 = 10;
pub const TOKEN_SEMICOLON: i32 = 11;
pub const TOKEN_SLASH: i32 = 12;
pub const TOKEN_SquareL: i32 = 13;
pub const TOKEN_SquareR: i32 = 14;
pub const TOKEN_StrLit: i32 = 15;
pub const TOKEN_TYPE_ID: i32 = 16;
pub const TOKEN_ASSIGN: i32 = 17;
pub const TOKEN_BLOCKARROW: i32 = 18;
pub const TOKEN_RETURN: i32 = 19;
pub const TOKEN_OR: i32 = 20;
pub const TOKEN_XOR: i32 = 21;
pub const TOKEN_AND: i32 = 22;
pub const TOKEN_ConcatNewline: i32 = 23;
pub const TOKEN_NOT: i32 = 24;
pub const TOKEN_EQ: i32 = 25;
pub const TOKEN_NEQ: i32 = 26;
pub const TOKEN_GT: i32 = 27;
pub const TOKEN_GTEQ: i32 = 28;
pub const TOKEN_LT: i32 = 29;
pub const TOKEN_LTEQ: i32 = 30;
pub const TOKEN_MINUS: i32 = 31;
pub const TOKEN_TIMES: i32 = 32;
pub const TOKEN_MOD: i32 = 33;
pub const TOKEN_DOLLAR: i32 = 34;
pub const TOKEN_DOT: i32 = 35;
pub const TOKEN_LPAREN: i32 = 36;
pub const TOKEN_RPAREN: i32 = 37;
pub const TOKEN_IMPORT: i32 = 38;
pub const TOKEN_STRUCT: i32 = 39;
pub const TOKEN_FAIL: i32 = 40;
pub const TOKEN_FAILED: i32 = 41;
pub const TOKEN_Fork: i32 = 42;
pub const TOKEN_Func: i32 = 43;
pub const TOKEN_PARENCALL: i32 = 44;
pub const TOKEN_TYPE_INT: i32 = 45;
pub const TOKEN_TYPE_STR: i32 = 46;
pub const TOKEN_TYPE_HASHTAG: i32 = 47;
pub const TOKEN_TYPE_BOOL: i32 = 48;
pub const TOKEN_TYPE_VOID: i32 = 49;
pub const TOKEN_PS: i32 = 50;
pub const TOKEN_MACRO: i32 = 51;
pub const TOKEN_IF: i32 = 52;
pub const TOKEN_PIPE: i32 = 53;
pub const TOKEN_CASE: i32 = 54;
pub const TOKEN_MATCH: i32 = 55;
pub const TOKEN_True: i32 = 56;
pub const TOKEN_False: i32 = 57;
pub const TOKEN_UNDERSCORE: i32 = 58;
pub const TOKEN_NEGATE: i32 = 59;
pub const TOKEN_VOID: i32 = 60;
pub const TOKEN_DollarQuestion: i32 = 61;
pub const TOKEN_StrOpen: i32 = 62;
pub const TOKEN_StrClose: i32 = 63;
#[inline]
fn token_major(t: &Token) -> i32 {
    match t {
        &Token::EOI => 0,
        &Token::ANY => TOKEN_ANY,
        &Token::COLON(_) => TOKEN_COLON,
        &Token::COMMA(_) => TOKEN_COMMA,
        &Token::DOUBLEDASH(_) => TOKEN_DOUBLEDASH,
        &Token::ELSE(_) => TOKEN_ELSE,
        &Token::HASHTAG(_) => TOKEN_HASHTAG,
        &Token::ID(_) => TOKEN_ID,
        &Token::INT(_) => TOKEN_INT,
        &Token::Let(_) => TOKEN_Let,
        &Token::PLUS(_) => TOKEN_PLUS,
        &Token::SEMICOLON(_) => TOKEN_SEMICOLON,
        &Token::SLASH(_) => TOKEN_SLASH,
        &Token::SquareL(_) => TOKEN_SquareL,
        &Token::SquareR(_) => TOKEN_SquareR,
        &Token::StrLit(_) => TOKEN_StrLit,
        &Token::TYPE_ID(_) => TOKEN_TYPE_ID,
        &Token::ASSIGN => TOKEN_ASSIGN,
        &Token::BLOCKARROW => TOKEN_BLOCKARROW,
        &Token::RETURN => TOKEN_RETURN,
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
        &Token::DOLLAR => TOKEN_DOLLAR,
        &Token::DOT => TOKEN_DOT,
        &Token::LPAREN => TOKEN_LPAREN,
        &Token::RPAREN => TOKEN_RPAREN,
        &Token::IMPORT => TOKEN_IMPORT,
        &Token::STRUCT => TOKEN_STRUCT,
        &Token::FAIL => TOKEN_FAIL,
        &Token::FAILED => TOKEN_FAILED,
        &Token::Fork => TOKEN_Fork,
        &Token::Func => TOKEN_Func,
        &Token::PARENCALL => TOKEN_PARENCALL,
        &Token::TYPE_INT => TOKEN_TYPE_INT,
        &Token::TYPE_STR => TOKEN_TYPE_STR,
        &Token::TYPE_HASHTAG => TOKEN_TYPE_HASHTAG,
        &Token::TYPE_BOOL => TOKEN_TYPE_BOOL,
        &Token::TYPE_VOID => TOKEN_TYPE_VOID,
        &Token::PS => TOKEN_PS,
        &Token::MACRO => TOKEN_MACRO,
        &Token::IF => TOKEN_IF,
        &Token::PIPE => TOKEN_PIPE,
        &Token::CASE => TOKEN_CASE,
        &Token::MATCH => TOKEN_MATCH,
        &Token::True => TOKEN_True,
        &Token::False => TOKEN_False,
        &Token::UNDERSCORE => TOKEN_UNDERSCORE,
        &Token::NEGATE => TOKEN_NEGATE,
        &Token::VOID => TOKEN_VOID,
        &Token::DollarQuestion => TOKEN_DollarQuestion,
        &Token::StrOpen => TOKEN_StrOpen,
        &Token::StrClose => TOKEN_StrClose,
    }
}
#[inline]
fn token_minor(t: Token) -> YYMinorType {
  match t {
        Token::COLON(x) => YYMinorType::YY132(x),
        Token::COMMA(x) => YYMinorType::YY132(x),
        Token::DOUBLEDASH(x) => YYMinorType::YY132(x),
        Token::ELSE(x) => YYMinorType::YY132(x),
        Token::HASHTAG(x) => YYMinorType::YY209(x),
        Token::ID(x) => YYMinorType::YY209(x),
        Token::INT(x) => YYMinorType::YY186(x),
        Token::Let(x) => YYMinorType::YY132(x),
        Token::PLUS(x) => YYMinorType::YY132(x),
        Token::SEMICOLON(x) => YYMinorType::YY132(x),
        Token::SLASH(x) => YYMinorType::YY132(x),
        Token::SquareL(x) => YYMinorType::YY132(x),
        Token::SquareR(x) => YYMinorType::YY132(x),
        Token::StrLit(x) => YYMinorType::YY158(x),
        Token::TYPE_ID(x) => YYMinorType::YY209(x),
        _ => YYMinorType::YY0
  }
}
const YY_ACTTAB_COUNT: i32 = 1253;
const YY_ACTION: [YYACTIONTYPE; 1253] = [
 /*     0 */   192,  198,  195,  126,   38,  242,   34,    6,   27,  239,
 /*    10 */   231,  230,   37,   30,  142,  217,  207,  216,   26,  237,
 /*    20 */    39,   17,   16,   19,   18,   21,   20,   33,   29,   25,
 /*    30 */     9,    5,  149,   59,  131,  128,  124,  122,  208,  236,
 /*    40 */   235,  234,  233,  232,   11,  114,    4,  152,   79,   28,
 /*    50 */   194,  193,  119,   41,  197,  196,   46,   62,  192,  198,
 /*    60 */   195,  145,   42,  145,   34,    6,   27,  116,  231,  226,
 /*    70 */     1,   34,  299,   27,  105,  217,   26,  216,  204,  371,
 /*    80 */   371,  371,  371,   21,   20,   33,   29,   25,    9,  192,
 /*    90 */   198,  195,   33,   29,   25,  165,   60,  236,  235,  234,
 /*   100 */   233,  232,  106,  217,  140,  216,   79,   28,  194,  193,
 /*   110 */    27,   41,  197,  196,   46,   64,  192,  198,  195,   32,
 /*   120 */    38,  105,  217,    6,  216,  141,  231,    1,  135,   49,
 /*   130 */    29,   25,  224,  187,   26,   78,  217,  219,  216,  194,
 /*   140 */   193,   45,    1,  197,  196,   46,    9,  212,  211,  215,
 /*   150 */   203,  217,  205,  216,   35,  236,  235,  234,  233,  232,
 /*   160 */    57,  137,   40,  231,   79,   28,  194,  193,  202,   41,
 /*   170 */   197,  196,   46,  192,  198,  195,   12,   36,  209,  186,
 /*   180 */     6,  153,  187,  231,  178,   49,   47,  117,   47,  171,
 /*   190 */    45,   26,  236,  235,  234,  233,  232,  214,  213,  210,
 /*   200 */   138,   43,  190,    9,  189,   43,  188,   43,  183,  146,
 /*   210 */   139,  179,  236,  235,  234,  233,  232,  133,   58,  191,
 /*   220 */   191,   79,   28,  194,  193,  130,   41,  197,  196,   46,
 /*   230 */   192,  198,  195,  229,  147,  129,   15,    6,   76,  175,
 /*   240 */   231,  125,   14,  123,   13,  121,   75,   73,   26,  103,
 /*   250 */    80,  225,   74,  172,   72,  201,   70,  200,  136,  191,
 /*   260 */     9,  228,   48,  113,   69,   68,  167,   67,  144,  236,
 /*   270 */   235,  234,  233,  232,  164,   31,  161,  155,   79,   28,
 /*   280 */   194,  193,  218,   41,  197,  196,   46,  192,  198,  195,
 /*   290 */    40,   77,  229,  147,    6,    1,  127,  231,   71,   56,
 /*   300 */   120,   44,  118,  115,  156,   26,  110,  163,  103,   83,
 /*   310 */   225,  168,  173,   60,  201,  154,  200,    9,  191,  170,
 /*   320 */    10,    3,  112,  111,  166,  159,  236,  235,  234,  233,
 /*   330 */   232,  157,  373,  160,   10,   79,   28,  194,  193,  109,
 /*   340 */    41,  197,  196,   46,  192,  198,  195,  162,   63,  229,
 /*   350 */   147,    6,  143,   61,  231,   50,  177,  132,   66,  134,
 /*   360 */    65,  169,   26,  128,  373,  103,   83,  148,  373,  373,
 /*   370 */   373,  201,  104,  200,    9,  191,  373,  373,  373,  373,
 /*   380 */   373,  373,  373,  236,  235,  234,  233,  232,  373,  373,
 /*   390 */   373,  373,   79,   28,  194,  193,  373,   41,  197,  196,
 /*   400 */    46,  372,  150,    2,  373,  222,  373,  373,  373,  373,
 /*   410 */   180,  373,  221,  373,  229,  147,  220,  373,  373,  212,
 /*   420 */   211,  215,  373,  373,  373,  240,   35,  206,  238,  223,
 /*   430 */   103,   90,  225,  373,    8,  373,  201,  373,  200,  373,
 /*   440 */   191,   34,  373,   27,  373,  373,  373,  373,  373,   36,
 /*   450 */   373,   23,   22,   24,  237,  373,   17,   16,   19,   18,
 /*   460 */    21,   20,   33,   29,   25,    8,  373,  373,  227,  214,
 /*   470 */   213,  210,   34,  373,   27,  373,  373,  373,  373,  373,
 /*   480 */   373,  373,   23,   22,   24,  237,  373,   17,   16,   19,
 /*   490 */    18,   21,   20,   33,   29,   25,  212,  211,  215,  185,
 /*   500 */   373,  241,    2,   35,  222,  373,  373,  373,  373,  180,
 /*   510 */   373,  221,  373,  229,  147,  220,  373,  373,  373,  373,
 /*   520 */   373,  373,  373,  373,  240,  373,   36,  238,  223,  103,
 /*   530 */    90,  225,  373,  373,  373,  201,  373,  200,  373,  191,
 /*   540 */   373,  174,    2,  373,  222,  373,  214,  213,  210,  180,
 /*   550 */   373,  221,  373,  229,  147,  220,  373,  373,  373,  373,
 /*   560 */   373,  373,  373,  373,  240,  373,  373,  238,  223,  103,
 /*   570 */    90,  225,  373,    8,  373,  201,  373,  200,  373,  191,
 /*   580 */    34,  373,   27,  373,  373,  373,  373,  373,  373,  373,
 /*   590 */    23,   22,   24,  237,  373,   17,   16,   19,   18,   21,
 /*   600 */    20,   33,   29,   25,   34,  373,   27,  373,  373,  373,
 /*   610 */   373,  373,  373,  373,   23,   22,   24,  237,  373,   17,
 /*   620 */    16,   19,   18,   21,   20,   33,   29,   25,   34,  373,
 /*   630 */    27,  176,  373,  373,  373,  373,  373,  373,   23,   22,
 /*   640 */    24,  237,  373,   17,   16,   19,   18,   21,   20,   33,
 /*   650 */    29,   25,    7,  373,  373,  185,  373,  373,  373,   34,
 /*   660 */   373,   27,  373,  373,  373,  373,  373,  373,  373,   23,
 /*   670 */    22,   24,  237,  373,   17,   16,   19,   18,   21,   20,
 /*   680 */    33,   29,   25,   34,  373,   27,  373,  151,  373,  373,
 /*   690 */   373,  373,  373,   23,   22,   24,  237,  373,   17,   16,
 /*   700 */    19,   18,   21,   20,   33,   29,   25,   34,  373,   27,
 /*   710 */   373,  373,  373,  373,  373,    1,  373,   23,   22,   24,
 /*   720 */   237,  373,   17,   16,   19,   18,   21,   20,   33,   29,
 /*   730 */    25,   34,  373,   27,  373,  373,  373,  373,  373,  373,
 /*   740 */   373,   23,   22,   24,  237,  373,   17,   16,   19,   18,
 /*   750 */    21,   20,   33,   29,   25,  373,  373,  373,  373,  373,
 /*   760 */    34,  373,   27,  373,  373,  373,  373,  373,  373,  373,
 /*   770 */    23,   22,   24,  237,   40,   17,   16,   19,   18,   21,
 /*   780 */    20,   33,   29,   25,   34,  373,   27,  373,  373,  373,
 /*   790 */   373,  373,  373,  373,  229,  147,   24,  237,  373,   17,
 /*   800 */    16,   19,   18,   21,   20,   33,   29,   25,  229,  147,
 /*   810 */   103,   86,  225,  373,  373,  373,  201,  373,  200,  184,
 /*   820 */   191,  373,  373,  373,  103,   81,  225,  229,  147,  158,
 /*   830 */   201,  108,  200,  107,  191,  373,  373,  373,  373,  229,
 /*   840 */   147,  373,  373,  103,   53,  225,  373,  373,  373,  201,
 /*   850 */   373,  200,  373,  191,  373,  103,   55,  225,  229,  147,
 /*   860 */   373,  201,  373,  200,  373,  191,  373,  373,  373,  373,
 /*   870 */   373,  373,  373,  373,  103,  101,  225,  373,  229,  147,
 /*   880 */   201,  373,  200,  373,  191,  373,  373,  373,  373,  229,
 /*   890 */   147,  373,  373,  373,  103,  102,  225,  373,  373,  373,
 /*   900 */   201,  373,  200,  373,  191,  103,   84,  225,  229,  147,
 /*   910 */   373,  201,  373,  200,  373,  191,  373,  373,  373,  229,
 /*   920 */   147,  373,  229,  147,  103,   55,  225,  373,  373,  373,
 /*   930 */   201,  373,  200,  373,  191,  103,   89,  225,  103,  181,
 /*   940 */   225,  201,  373,  200,  201,  191,  200,  373,  191,  373,
 /*   950 */   373,  373,  373,  229,  147,  373,  373,  373,  373,  373,
 /*   960 */   373,  373,  373,  373,  373,  373,  373,  373,  373,  103,
 /*   970 */    54,  225,  373,  229,  147,  201,  373,  200,  373,  191,
 /*   980 */   373,  373,  373,  373,  229,  147,  373,  229,  147,  103,
 /*   990 */   182,  225,  373,  373,  373,  201,  373,  200,  373,  191,
 /*  1000 */   103,   94,  225,  103,  199,  225,  201,  373,  200,  201,
 /*  1010 */   191,  200,  373,  191,  229,  147,  373,  229,  147,  373,
 /*  1020 */   373,  373,  373,  373,  373,  373,  373,  373,  373,  373,
 /*  1030 */   103,   93,  225,  103,   92,  225,  201,  373,  200,  201,
 /*  1040 */   191,  200,  373,  191,  373,  373,  373,  373,  229,  147,
 /*  1050 */   373,  373,  373,  373,  373,  373,  373,  373,  373,  373,
 /*  1060 */   373,  373,  373,  373,  103,   91,  225,  229,  147,  373,
 /*  1070 */   201,  373,  200,  373,  191,  373,  373,  373,  229,  147,
 /*  1080 */   373,  229,  147,  103,  100,  225,  373,  373,  373,  201,
 /*  1090 */   373,  200,  373,  191,  103,   99,  225,  103,   98,  225,
 /*  1100 */   201,  373,  200,  201,  191,  200,  373,  191,  373,  229,
 /*  1110 */   147,  373,  229,  147,  373,  373,  373,  373,  373,  373,
 /*  1120 */   373,  373,  373,  373,  373,  103,   97,  225,  103,   96,
 /*  1130 */   225,  201,  373,  200,  201,  191,  200,  373,  191,  373,
 /*  1140 */   373,  373,  373,  229,  147,  373,  229,  147,  373,  373,
 /*  1150 */   373,  373,  373,  373,  373,  373,  373,  229,  147,  103,
 /*  1160 */    95,  225,  103,   85,  225,  201,  373,  200,  201,  191,
 /*  1170 */   200,  373,  191,  103,   88,  225,  229,  147,  373,  201,
 /*  1180 */   373,  200,  373,  191,  373,  373,  373,  373,  229,  147,
 /*  1190 */   373,  373,  103,   87,  225,  373,  373,  373,  201,  373,
 /*  1200 */   200,  373,  191,  373,  103,   52,  225,  229,  147,  373,
 /*  1210 */   201,  373,  200,  373,  191,  373,  373,  373,  373,  373,
 /*  1220 */   373,  373,  373,  103,   82,  225,  229,  147,  373,  201,
 /*  1230 */   373,  200,  373,  191,  373,  373,  373,  373,  373,  373,
 /*  1240 */   373,  373,  103,   51,  225,  373,  373,  373,  201,  373,
 /*  1250 */   200,  373,  191,
];
const YY_LOOKAHEAD: [YYCODETYPE; 1253] = [
 /*     0 */     6,    7,    8,    9,    3,    0,   10,   13,   12,    7,
 /*    10 */    16,   14,    3,   19,   84,   85,   86,   87,   24,   23,
 /*    20 */    11,   25,   26,   27,   28,   29,   30,   31,   32,   33,
 /*    30 */    36,   44,   38,   39,   40,   41,   42,   43,   37,   45,
 /*    40 */    46,   47,   48,   49,   11,   51,   52,   14,   54,   55,
 /*    50 */    56,   57,   68,   59,   60,   61,   62,    5,    6,    7,
 /*    60 */     8,   35,   34,   35,   10,   13,   12,   83,   16,    7,
 /*    70 */    18,   10,   44,   12,   84,   85,   24,   87,   88,   25,
 /*    80 */    26,   27,   28,   29,   30,   31,   32,   33,   36,    6,
 /*    90 */     7,    8,   31,   32,   33,    4,    5,   45,   46,   47,
 /*   100 */    48,   49,   84,   85,   86,   87,   54,   55,   56,   57,
 /*   110 */    12,   59,   60,   61,   62,    5,    6,    7,    8,   36,
 /*   120 */     3,   84,   85,   13,   87,   88,   16,   18,   90,   91,
 /*   130 */    32,   33,    4,    7,   24,   84,   85,    4,   87,   56,
 /*   140 */    57,   15,   18,   60,   61,   62,   36,    6,    7,    8,
 /*   150 */    84,   85,   14,   87,   13,   45,   46,   47,   48,   49,
 /*   160 */    13,   35,   53,   16,   54,   55,   56,   57,   37,   59,
 /*   170 */    60,   61,   62,    6,    7,    8,   52,   36,   37,    7,
 /*   180 */    13,   14,    7,   16,   90,   91,   74,   75,   74,   75,
 /*   190 */    15,   24,   45,   46,   47,   48,   49,   56,   57,   58,
 /*   200 */   105,  106,   63,   36,  105,  106,  105,  106,   37,   94,
 /*   210 */    94,    4,   45,   46,   47,   48,   49,    7,    2,  104,
 /*   220 */   104,   54,   55,   56,   57,   36,   59,   60,   61,   62,
 /*   230 */     6,    7,    8,   78,   79,    6,    3,   13,    7,    4,
 /*   240 */    16,    7,   17,    7,   17,    7,   44,    4,   24,   94,
 /*   250 */    95,   96,   37,    4,    4,  100,    3,  102,  103,  104,
 /*   260 */    36,   37,   18,    7,   44,   37,    4,    3,   99,   45,
 /*   270 */    46,   47,   48,   49,    4,   53,    4,   37,   54,   55,
 /*   280 */    56,   57,   83,   59,   60,   61,   62,    6,    7,    8,
 /*   290 */    53,   68,   78,   79,   13,   18,   83,   16,    7,    2,
 /*   300 */    70,   97,   50,   97,   99,   24,    7,   81,   94,   95,
 /*   310 */    96,   70,   73,    5,  100,  101,  102,   36,  104,   73,
 /*   320 */    53,   53,   77,   68,   77,   68,   45,   46,   47,   48,
 /*   330 */    49,   68,  107,   82,   53,   54,   55,   56,   57,   81,
 /*   340 */    59,   60,   61,   62,    6,    7,    8,   68,   68,   78,
 /*   350 */    79,   13,   83,   68,   16,   96,   96,   96,   68,   35,
 /*   360 */    68,   96,   24,   41,  107,   94,   95,   96,  107,  107,
 /*   370 */   107,  100,  101,  102,   36,  104,  107,  107,  107,  107,
 /*   380 */   107,  107,  107,   45,   46,   47,   48,   49,  107,  107,
 /*   390 */   107,  107,   54,   55,   56,   57,  107,   59,   60,   61,
 /*   400 */    62,   65,   66,   67,  107,   69,  107,  107,  107,  107,
 /*   410 */    74,  107,   76,  107,   78,   79,   80,  107,  107,    6,
 /*   420 */     7,    8,  107,  107,  107,   89,   13,   14,   92,   93,
 /*   430 */    94,   95,   96,  107,    3,  107,  100,  107,  102,  107,
 /*   440 */   104,   10,  107,   12,  107,  107,  107,  107,  107,   36,
 /*   450 */   107,   20,   21,   22,   23,  107,   25,   26,   27,   28,
 /*   460 */    29,   30,   31,   32,   33,    3,  107,  107,   37,   56,
 /*   470 */    57,   58,   10,  107,   12,  107,  107,  107,  107,  107,
 /*   480 */   107,  107,   20,   21,   22,   23,  107,   25,   26,   27,
 /*   490 */    28,   29,   30,   31,   32,   33,    6,    7,    8,   37,
 /*   500 */   107,   66,   67,   13,   69,  107,  107,  107,  107,   74,
 /*   510 */   107,   76,  107,   78,   79,   80,  107,  107,  107,  107,
 /*   520 */   107,  107,  107,  107,   89,  107,   36,   92,   93,   94,
 /*   530 */    95,   96,  107,  107,  107,  100,  107,  102,  107,  104,
 /*   540 */   107,   66,   67,  107,   69,  107,   56,   57,   58,   74,
 /*   550 */   107,   76,  107,   78,   79,   80,  107,  107,  107,  107,
 /*   560 */   107,  107,  107,  107,   89,  107,  107,   92,   93,   94,
 /*   570 */    95,   96,  107,    3,  107,  100,  107,  102,  107,  104,
 /*   580 */    10,  107,   12,  107,  107,  107,  107,  107,  107,  107,
 /*   590 */    20,   21,   22,   23,  107,   25,   26,   27,   28,   29,
 /*   600 */    30,   31,   32,   33,   10,  107,   12,  107,  107,  107,
 /*   610 */   107,  107,  107,  107,   20,   21,   22,   23,  107,   25,
 /*   620 */    26,   27,   28,   29,   30,   31,   32,   33,   10,  107,
 /*   630 */    12,   37,  107,  107,  107,  107,  107,  107,   20,   21,
 /*   640 */    22,   23,  107,   25,   26,   27,   28,   29,   30,   31,
 /*   650 */    32,   33,    3,  107,  107,   37,  107,  107,  107,   10,
 /*   660 */   107,   12,  107,  107,  107,  107,  107,  107,  107,   20,
 /*   670 */    21,   22,   23,  107,   25,   26,   27,   28,   29,   30,
 /*   680 */    31,   32,   33,   10,  107,   12,  107,   14,  107,  107,
 /*   690 */   107,  107,  107,   20,   21,   22,   23,  107,   25,   26,
 /*   700 */    27,   28,   29,   30,   31,   32,   33,   10,  107,   12,
 /*   710 */   107,  107,  107,  107,  107,   18,  107,   20,   21,   22,
 /*   720 */    23,  107,   25,   26,   27,   28,   29,   30,   31,   32,
 /*   730 */    33,   10,  107,   12,  107,  107,  107,  107,  107,  107,
 /*   740 */   107,   20,   21,   22,   23,  107,   25,   26,   27,   28,
 /*   750 */    29,   30,   31,   32,   33,  107,  107,  107,  107,  107,
 /*   760 */    10,  107,   12,  107,  107,  107,  107,  107,  107,  107,
 /*   770 */    20,   21,   22,   23,   53,   25,   26,   27,   28,   29,
 /*   780 */    30,   31,   32,   33,   10,  107,   12,  107,  107,  107,
 /*   790 */   107,  107,  107,  107,   78,   79,   22,   23,  107,   25,
 /*   800 */    26,   27,   28,   29,   30,   31,   32,   33,   78,   79,
 /*   810 */    94,   95,   96,  107,  107,  107,  100,  107,  102,  103,
 /*   820 */   104,  107,  107,  107,   94,   95,   96,   78,   79,   68,
 /*   830 */   100,   82,  102,  103,  104,  107,  107,  107,  107,   78,
 /*   840 */    79,  107,  107,   94,   95,   96,  107,  107,  107,  100,
 /*   850 */   107,  102,  107,  104,  107,   94,   95,   96,   78,   79,
 /*   860 */   107,  100,  107,  102,  107,  104,  107,  107,  107,  107,
 /*   870 */   107,  107,  107,  107,   94,   95,   96,  107,   78,   79,
 /*   880 */   100,  107,  102,  107,  104,  107,  107,  107,  107,   78,
 /*   890 */    79,  107,  107,  107,   94,   95,   96,  107,  107,  107,
 /*   900 */   100,  107,  102,  107,  104,   94,   95,   96,   78,   79,
 /*   910 */   107,  100,  107,  102,  107,  104,  107,  107,  107,   78,
 /*   920 */    79,  107,   78,   79,   94,   95,   96,  107,  107,  107,
 /*   930 */   100,  107,  102,  107,  104,   94,   95,   96,   94,   95,
 /*   940 */    96,  100,  107,  102,  100,  104,  102,  107,  104,  107,
 /*   950 */   107,  107,  107,   78,   79,  107,  107,  107,  107,  107,
 /*   960 */   107,  107,  107,  107,  107,  107,  107,  107,  107,   94,
 /*   970 */    95,   96,  107,   78,   79,  100,  107,  102,  107,  104,
 /*   980 */   107,  107,  107,  107,   78,   79,  107,   78,   79,   94,
 /*   990 */    95,   96,  107,  107,  107,  100,  107,  102,  107,  104,
 /*  1000 */    94,   95,   96,   94,   95,   96,  100,  107,  102,  100,
 /*  1010 */   104,  102,  107,  104,   78,   79,  107,   78,   79,  107,
 /*  1020 */   107,  107,  107,  107,  107,  107,  107,  107,  107,  107,
 /*  1030 */    94,   95,   96,   94,   95,   96,  100,  107,  102,  100,
 /*  1040 */   104,  102,  107,  104,  107,  107,  107,  107,   78,   79,
 /*  1050 */   107,  107,  107,  107,  107,  107,  107,  107,  107,  107,
 /*  1060 */   107,  107,  107,  107,   94,   95,   96,   78,   79,  107,
 /*  1070 */   100,  107,  102,  107,  104,  107,  107,  107,   78,   79,
 /*  1080 */   107,   78,   79,   94,   95,   96,  107,  107,  107,  100,
 /*  1090 */   107,  102,  107,  104,   94,   95,   96,   94,   95,   96,
 /*  1100 */   100,  107,  102,  100,  104,  102,  107,  104,  107,   78,
 /*  1110 */    79,  107,   78,   79,  107,  107,  107,  107,  107,  107,
 /*  1120 */   107,  107,  107,  107,  107,   94,   95,   96,   94,   95,
 /*  1130 */    96,  100,  107,  102,  100,  104,  102,  107,  104,  107,
 /*  1140 */   107,  107,  107,   78,   79,  107,   78,   79,  107,  107,
 /*  1150 */   107,  107,  107,  107,  107,  107,  107,   78,   79,   94,
 /*  1160 */    95,   96,   94,   95,   96,  100,  107,  102,  100,  104,
 /*  1170 */   102,  107,  104,   94,   95,   96,   78,   79,  107,  100,
 /*  1180 */   107,  102,  107,  104,  107,  107,  107,  107,   78,   79,
 /*  1190 */   107,  107,   94,   95,   96,  107,  107,  107,  100,  107,
 /*  1200 */   102,  107,  104,  107,   94,   95,   96,   78,   79,  107,
 /*  1210 */   100,  107,  102,  107,  104,  107,  107,  107,  107,  107,
 /*  1220 */   107,  107,  107,   94,   95,   96,   78,   79,  107,  100,
 /*  1230 */   107,  102,  107,  104,  107,  107,  107,  107,  107,  107,
 /*  1240 */   107,  107,   94,   95,   96,  107,  107,  107,  100,  107,
 /*  1250 */   102,  107,  104,
];
const YY_SHIFT_USE_DFLT: i32 = -14;
const YY_SHIFT_COUNT: i32 = 150;
const YY_SHIFT_MIN: i32 = -13;
const YY_SHIFT_MAX: i32 = 774;
const YY_SHIFT_OFST: [i16; 151] = [
 /*     0 */    -6,   -6,   -6,   52,  281,  224,  167,  338,  338,  338,
 /*    10 */   110,  338,  338,  338,  338,  338,  338,  338,  338,  338,
 /*    20 */   338,  338,  338,  338,  338,  338,  338,  338,  338,  338,
 /*    30 */   338,  338,  338,  338,  338,  413,  141,  490,  490,  490,
 /*    40 */   490,   83,   83,  126,  109,  175,  175,  322,  322,  324,
 /*    50 */   324,  697,  697,  697,  721,  697,  147,  147,  147,  147,
 /*    60 */   124,   91,  277,  268,  277,  267,  308,  299,  277,  299,
 /*    70 */   291,  297,  252,  252,  297,  291,  237,  237,  277,  222,
 /*    80 */   462,  431,  673,  649,  618,  594,  570,  750,  750,  750,
 /*    90 */   750,  774,  774,   -4,   -4,   54,   54,   54,   54,   61,
 /*   100 */    61,   98,   98,   28,   33,    9,    1,  240,  272,  270,
 /*   110 */   264,  262,  228,  220,  256,  253,  250,  249,  244,  243,
 /*   120 */   215,  202,  238,  227,  236,  225,  234,  235,  231,  233,
 /*   130 */   229,  189,   -3,  216,  210,  207,  171,  172,  139,   26,
 /*   140 */   131,  138,  117,  133,  128,   62,   26,  -13,   -3,    2,
 /*   150 */     5,
];
const YY_REDUCE_USE_DFLT: i32 = -71;
const YY_REDUCE_COUNT: i32 = 79;
const YY_REDUCE_MIN: i32 = -70;
const YY_REDUCE_MAX: i32 = 1148;
const YY_REDUCE_OFST: [i16; 80] = [
 /*     0 */   336,  475,  435,  761,  749,  730,  271,  214,  716,  155,
 /*    10 */  1148, 1129, 1110, 1098, 1079, 1068, 1065, 1034, 1031, 1003,
 /*    20 */  1000,  989,  970,  939,  936,  909,  906,  895,  875,  844,
 /*    30 */   841,  830,  811,  800,  780,   37,   18,  -10,  -70,   66,
 /*    40 */    51,  116,  115,  101,  -16,   99,   95,  114,  112,   94,
 /*    50 */    38,  292,  290,  285,  269,  280,  265,  261,  260,  259,
 /*    60 */   279,  258,  263,  205,  257,  251,  226,  247,  255,  245,
 /*    70 */   241,  206,  246,  239,  204,  230,  213,  199,  223,  169,
];
const YY_DEFAULT: [YYACTIONTYPE; 242] = [
 /*     0 */   243,  243,  243,  371,  371,  371,  371,  371,  371,  371,
 /*    10 */   371,  371,  371,  371,  371,  371,  371,  371,  371,  371,
 /*    20 */   371,  371,  371,  371,  371,  371,  371,  371,  371,  371,
 /*    30 */   371,  371,  371,  371,  371,  371,  371,  371,  371,  371,
 /*    40 */   371,  371,  371,  366,  371,  366,  366,  280,  371,  257,
 /*    50 */   257,  371,  371,  371,  371,  371,  371,  371,  371,  371,
 /*    60 */   371,  371,  371,  371,  371,  292,  290,  283,  371,  283,
 /*    70 */   266,  269,  278,  278,  269,  266,  371,  308,  371,  371,
 /*    80 */   371,  371,  371,  360,  371,  371,  363,  262,  261,  253,
 /*    90 */   248,  339,  338,  337,  329,  345,  344,  343,  342,  341,
 /*   100 */   340,  332,  333,  346,  371,  324,  371,  371,  371,  371,
 /*   110 */   284,  371,  371,  371,  371,  267,  371,  371,  371,  371,
 /*   120 */   371,  371,  371,  371,  371,  371,  371,  371,  371,  371,
 /*   130 */   371,  371,  371,  371,  371,  371,  371,  371,  371,  331,
 /*   140 */   371,  371,  320,  371,  371,  371,  301,  371,  300,  371,
 /*   150 */   371,  359,  358,  357,  361,  298,  305,  304,  303,  294,
 /*   160 */   293,  288,  291,  289,  287,  286,  285,  282,  268,  270,
 /*   170 */   265,  281,  279,  264,  263,  260,  259,  258,  256,  255,
 /*   180 */   254,  334,  335,  362,  364,  347,  370,  369,  368,  367,
 /*   190 */   365,  355,  354,  353,  352,  351,  350,  349,  348,  336,
 /*   200 */   328,  327,  319,  326,  325,  323,  322,  321,  318,  317,
 /*   210 */   316,  315,  314,  313,  312,  311,  310,  309,  307,  306,
 /*   220 */   252,  251,  250,  249,  302,  300,  356,  297,  296,  295,
 /*   230 */   277,  276,  275,  274,  273,  272,  271,  330,  247,  246,
 /*   240 */   245,  244,
];

/* TMPL: fallback tokens */

const YY_FALLBACK: [i32; 0] = [
];

/* TMPL: symbol names */


/* TMPL: rules */


/* TMPL: destructors */


/* TMPL: stack-overflow */


/* TMPL: stack-overflow */

const YY_RULE_INFO: [YYCODETYPE; 129] = [
  65,
  66,
  66,
  67,
  67,
  67,
  67,
  67,
  67,
  67,
  67,
  67,
  67,
  89,
  90,
  90,
  91,
  93,
  74,
  92,
  92,
  68,
  69,
  69,
  70,
  70,
  70,
  97,
  97,
  96,
  96,
  96,
  96,
  96,
  96,
  96,
  73,
  73,
  75,
  75,
  76,
  77,
  77,
  77,
  80,
  80,
  80,
  81,
  81,
  81,
  82,
  82,
  82,
  95,
  78,
  78,
  78,
  79,
  79,
  95,
  95,
  99,
  99,
  99,
  95,
  83,
  83,
  84,
  84,
  84,
  84,
  84,
  84,
  84,
  84,
  85,
  85,
  85,
  86,
  86,
  87,
  87,
  88,
  88,
  88,
  95,
  95,
  95,
  95,
  95,
  95,
  95,
  95,
  95,
  95,
  95,
  95,
  95,
  95,
  95,
  95,
  95,
  95,
  95,
  95,
  94,
  94,
  94,
  94,
  94,
  94,
  94,
  94,
  94,
  94,
  100,
  100,
  100,
  101,
  101,
  102,
  103,
  103,
  104,
  105,
  105,
  105,
  106,
  106,
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
    extra:  Result<Val, i32> ,
}

impl Parser {

    pub fn new(
            extra:  Result<Val, i32> ,
        ) -> Parser {
        let mut p = Parser { yyerrcnt: -1, yystack: Vec::new(), extra: extra};
        p.yystack.push(YYStackEntry{stateno: 0, major: 0, minor: YYMinorType::YY0});
        p
    }

    pub fn into_extra(self) ->  Result<Val, i32>  {
        self.extra
    }
    pub fn extra(&self) -> & Result<Val, i32>  {
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
            0 /* program ::= stmts */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY82(yy0),) => {

	// ignore yyres, it doesn't really go anywhere for program
	yyres = Val::Void;
	// we're done, so put yy0 in extra
	self.extra = Ok(yy0);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            1 /* stmts ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

	yyres = sexpr::new(SexprType::BlockExpr, list::empty());

} };
 YYMinorType::YY82(yyres)
}
            ,
            2 /* stmts ::= stmt stmts */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy1),) => {

    vout!("found new stmt: {:?}\n", yy0);
	yyres = list::cons(yy0, yy1);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            3 /* stmt ::= defstruct */
          | 5 /* stmt ::= let_stmt */
          | 7 /* stmt ::= fail_stmt */
          | 8 /* stmt ::= func_stmt */
          | 9 /* stmt ::= macro_stmt */
          | 10 /* stmt ::= if_stmt */
          | 12 /* stmt ::= failed_stmt */
          | 67 /* pexpr ::= ptuple */
          | 68 /* pexpr ::= plist */
          | 85 /* expr ::= list */
          | 86 /* expr ::= tuple */
          | 104 /* expr ::= term */
          | 113 /* term ::= strexpr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY82(yy0),) => {
 yyres = yy0; 
},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            4 /* stmt ::= IMPORT ID */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY209(yy1),) => {

    yyres = sexpr::new_import(Val::id(yy1.data));

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            6 /* stmt ::= expr */
          | 53 /* expr ::= call_expr */
          | 57 /* func_term ::= term */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY82(yy0),) => {

    yyres = yy0;

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            11 /* stmt ::= RETURN expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY82(yy1),) => {

    yyres = sexpr::new(SexprType::Return, yy1);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            13 /* defstruct ::= STRUCT typex defstruct_fields DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY39(yy1),YYMinorType::YY82(yy2),) => {

    yyres = sexpr::def_struct(Val::Type(yy1), yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            14 /* defstruct_fields ::= defstruct_field defstruct_fields */
          | 126 /* strlist ::= strlist_term strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy1),) => {

	yyres = list::cons(yy0, yy1);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            15 /* defstruct_fields ::= */
          | 24 /* dfunc_args ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

	yyres = list::empty();

} };
 YYMinorType::YY82(yyres)
}
            ,
            16 /* defstruct_field ::= DOT ID COLON typex */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY209(yy1),YYMinorType::YY39(yy3),) => {

	yyres = Val::typed_id(&yy1.data, yy3);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            17 /* fail_stmt ::= FAIL LPAREN HASHTAG COMMA expr RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp4 = self.yystack.pop().unwrap();
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp2.minor,yyp4.minor,) {
 (YYMinorType::YY209(yy2),YYMinorType::YY82(yy4),) => {

vout!("found fail_stmt {:?}\n", yy2);
	yyres = sexpr::new(SexprType::Fail,
        list::cons(Val::hashtag(yy2.data),
        list::cons(yy4,
        Val::Nil,
        ))
    );

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            18 /* failed_stmt ::= FAILED ID match_case DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY209(yy1),YYMinorType::YY82(yy2),) => {

	yyres = sexpr::new(SexprType::MatchFailed,
        list::cons(Val::id(yy1.data),
        list::cons(yy2,
        Val::Nil
        ))
    );

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            19 /* let_stmt ::= Let ID ASSIGN expr */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY209(yy1),YYMinorType::YY82(yy3),) => {

    let letx =
        list::cons(Val::id(yy1.data),
        list::cons(yy3,
        Val::Nil
        ));
    yyres = sexpr::new(SexprType::Let, letx);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            20 /* let_stmt ::= Fork ID ASSIGN expr */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY209(yy1),YYMinorType::YY82(yy3),) => {

	let bind = list::cons(Val::new_str(yy1.data), list::singleton(yy3));
	yyres = sexpr::new(SexprType::Fork, bind);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            21 /* block ::= BLOCKARROW stmts */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY82(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            22 /* func_stmt ::= Func ID PARENCALL dfunc_args RPAREN opt_typex block DOUBLEDASH opt_ps */
            => 
{
let yyres :  Val ;
let yyp8 = self.yystack.pop().unwrap();
let yyp7 = self.yystack.pop().unwrap();
let yyp6 = self.yystack.pop().unwrap();
let yyp5 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,yyp5.minor,yyp6.minor,yyp8.minor,) {
 (YYMinorType::YY209(yy1),YYMinorType::YY82(yy3),YYMinorType::YY39(yy5),YYMinorType::YY82(yy6),YYMinorType::YY82(yy8),) => {

	let id = Val::id(yy1.data);
	let typ = Val::Type(yy5);
	yyres = sexpr::defunc(id, yy3, typ, yy6, yy8)

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            23 /* func_stmt ::= Func ID PARENCALL dfunc_args RPAREN opt_typex match_case DOUBLEDASH opt_ps */
            => 
{
let yyres :  Val ;
let yyp8 = self.yystack.pop().unwrap();
let yyp7 = self.yystack.pop().unwrap();
let yyp6 = self.yystack.pop().unwrap();
let yyp5 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,yyp5.minor,yyp6.minor,yyp8.minor,) {
 (YYMinorType::YY209(yy1),YYMinorType::YY82(yy3),YYMinorType::YY39(yy5),YYMinorType::YY82(yy6),YYMinorType::YY82(yy8),) => {

	let id = Val::id(yy1.data);
	let typ = Val::Type(yy5);
    let body = sexpr::match_expr(Val::CallParams, yy6);
	yyres = sexpr::defunc(id, yy3, typ, body, yy8)

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            25 /* dfunc_args ::= ID opt_typex */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY209(yy0),YYMinorType::YY39(yy1),) => {

	yyres = list::singleton(Val::typed_id(&yy0.data, yy1));

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
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
 (YYMinorType::YY209(yy0),YYMinorType::YY39(yy1),YYMinorType::YY82(yy3),) => {

	yyres = list::cons(Val::typed_id(&yy0.data, yy1), yy3);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
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
 YYMinorType::YY39(yyres)
}
            ,
            28 /* opt_typex ::= COLON typex */
            => 
{
let yyres :  Type ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY39(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY39(yyres)
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
 YYMinorType::YY39(yyres)
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
 YYMinorType::YY39(yyres)
}
            ,
            31 /* typex ::= TYPE_HASHTAG */
            => 
{
let yyres :  Type ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Type::Hashtag;

} };
 YYMinorType::YY39(yyres)
}
            ,
            32 /* typex ::= TYPE_BOOL */
            => 
{
let yyres :  Type ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Type::Bool;

} };
 YYMinorType::YY39(yyres)
}
            ,
            33 /* typex ::= TYPE_VOID */
            => 
{
let yyres :  Type ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Type::Void;

} };
 YYMinorType::YY39(yyres)
}
            ,
            34 /* typex ::= TYPE_ID */
            => 
{
let yyres :  Type ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY209(yy0),) => {

	yyres = Type::Id(Arc::new(yy0.data));

},    _ => unreachable!() };
 YYMinorType::YY39(yyres)
}
            ,
            35 /* typex ::= SquareL typex SquareR */
            => 
{
let yyres :  Type ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY39(yy1),) => {

	yyres = Type::StrictList(Box::new(yy1));

},    _ => unreachable!() };
 YYMinorType::YY39(yyres)
}
            ,
            36 /* opt_ps ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

    yyres = Val::Void;

} };
 YYMinorType::YY82(yyres)
}
            ,
            37 /* opt_ps ::= PS BLOCKARROW failed_stmts DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp2.minor,) {
 (YYMinorType::YY82(yy2),) => {

    yyres = yy2;

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            38 /* failed_stmts ::= failed_stmt */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY82(yy0),) => {

    yyres = list::singleton(yy0);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            39 /* failed_stmts ::= failed_stmt failed_stmts */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy1),) => {

    yyres = list::cons(yy0, yy1);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            40 /* macro_stmt ::= MACRO ID PARENCALL macro_args RPAREN block DOUBLEDASH */
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
match (yyp1.minor,yyp3.minor,yyp5.minor,) {
 (YYMinorType::YY209(yy1),YYMinorType::YY82(yy3),YYMinorType::YY82(yy5),) => {

    vout!("found macro {:?}\n", yy1);
    yyres = sexpr::new(SexprType::DefMacro,
        list::cons(Val::id(yy1.data),
        list::cons(yy3,
        list::cons(yy5,
        Val::Nil
    ))));

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            41 /* macro_args ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

    yyres = Val::Nil;

} };
 YYMinorType::YY82(yyres)
}
            ,
            42 /* macro_args ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY209(yy0),) => {

    yyres = list::singleton(Val::id(yy0.data));

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            43 /* macro_args ::= ID COMMA macro_args */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY209(yy0),YYMinorType::YY82(yy2),) => {

    yyres = list::cons(Val::id(yy0.data), yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            44 /* if_stmt ::= IF expr block DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy1),YYMinorType::YY82(yy2),) => {

    /* if-only style */
    yyres = sexpr::ifstmt(yy1, yy2, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            45 /* if_stmt ::= IF expr block else_if DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp4 = self.yystack.pop().unwrap();
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,yyp3.minor,) {
 (YYMinorType::YY82(yy1),YYMinorType::YY82(yy2),YYMinorType::YY82(yy3),) => {

    /* if-else style */
    yyres = sexpr::ifstmt(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            46 /* if_stmt ::= IF if_case DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY82(yy1),) => {

    /* case-expr style */
    yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            47 /* else_if ::= ELSE IF expr block else_if */
            => 
{
let yyres :  Val ;
let yyp4 = self.yystack.pop().unwrap();
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp2.minor,yyp3.minor,yyp4.minor,) {
 (YYMinorType::YY82(yy2),YYMinorType::YY82(yy3),YYMinorType::YY82(yy4),) => {

    yyres = sexpr::ifstmt(yy2, yy3, yy4);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            48 /* else_if ::= ELSE IF expr block */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp2.minor,yyp3.minor,) {
 (YYMinorType::YY82(yy2),YYMinorType::YY82(yy3),) => {

    yyres = sexpr::ifstmt(yy2, yy3, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            49 /* else_if ::= ELSE block */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY82(yy1),) => {

    yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            50 /* if_case ::= PIPE expr block */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy1),YYMinorType::YY82(yy2),) => {

    yyres = sexpr::ifstmt(yy1, yy2, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            51 /* if_case ::= PIPE expr block if_case */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,yyp3.minor,) {
 (YYMinorType::YY82(yy1),YYMinorType::YY82(yy2),YYMinorType::YY82(yy3),) => {

    yyres = sexpr::ifstmt(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            52 /* if_case ::= PIPE ELSE block */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp2.minor,) {
 (YYMinorType::YY82(yy2),) => {

    yyres = yy2;

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            54 /* call_expr ::= func_term PARENCALL RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY82(yy0),) => {

	vout!("zero param function call!");
	yyres = sexpr::call(yy0, Val::Nil);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            55 /* call_expr ::= func_term PARENCALL expr RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	vout!("one param function call!");
	yyres = sexpr::call(yy0, list::singleton(yy2));

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            56 /* call_expr ::= func_term PARENCALL tuple_args RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	vout!("multi param function call!");
	yyres = sexpr::call(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            58 /* func_term ::= typex */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY39(yy0),) => {

    yyres = Val::Type(yy0);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            59 /* expr ::= term DOLLAR term */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	/* yyres = Val::binaryop(yy0, yy2, D); */
	yyres = Val::Void;

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            60 /* expr ::= CASE cases DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY82(yy1),) => {

    vout!("parsed case expr\n");
	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            61 /* cases ::= PIPE expr block PIPE block */
            => 
{
let yyres :  Val ;
let yyp4 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,yyp4.minor,) {
 (YYMinorType::YY82(yy1),YYMinorType::YY82(yy2),YYMinorType::YY82(yy4),) => {

    vout!("found cases base\n");
    yyres = sexpr::casex(yy1, yy2, yy4);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            62 /* cases ::= PIPE expr block PIPE ELSE block */
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
 (YYMinorType::YY82(yy1),YYMinorType::YY82(yy2),YYMinorType::YY82(yy5),) => {

    vout!("found cases base\n");
    yyres = sexpr::casex(yy1, yy2, yy5);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            63 /* cases ::= PIPE expr block cases */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,yyp3.minor,) {
 (YYMinorType::YY82(yy1),YYMinorType::YY82(yy2),YYMinorType::YY82(yy3),) => {

    vout!("found extra case\n");
    yyres = sexpr::casex(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            64 /* expr ::= MATCH expr match_case DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy1),YYMinorType::YY82(yy2),) => {

    vout!("parsed match expr\n");
    yyres = sexpr::match_expr(yy1, yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            65 /* match_case ::= PIPE pexpr block match_case */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,yyp3.minor,) {
 (YYMinorType::YY82(yy1),YYMinorType::YY82(yy2),YYMinorType::YY82(yy3),) => {

    vout!("found cases base\n");
    yyres = list::from3(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            66 /* match_case ::= PIPE pexpr block */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy1),YYMinorType::YY82(yy2),) => {

    vout!("parsed base match case\n");
    yyres = list::from2(yy1, yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            69 /* pexpr ::= INT */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY186(yy0),) => {
 yyres = Val::Int(yy0); 
},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            70 /* pexpr ::= True */
          | 110 /* term ::= True */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Bool(true); 
} };
 YYMinorType::YY82(yyres)
}
            ,
            71 /* pexpr ::= False */
          | 111 /* term ::= False */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Bool(false); 
} };
 YYMinorType::YY82(yyres)
}
            ,
            72 /* pexpr ::= HASHTAG */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY209(yy0),) => {
 yyres = Val::Hashtag(Arc::new(yy0.data)); 
},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            73 /* pexpr ::= ID */
          | 106 /* term ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY209(yy0),) => {
 yyres = Val::id(yy0.data); 
},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            74 /* pexpr ::= UNDERSCORE */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Wildcard; 
} };
 YYMinorType::YY82(yyres)
}
            ,
            75 /* ptuple ::= LPAREN RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match () {
 () => {

	panic!("an empty tuple is not a valid pattern");

} };
 YYMinorType::YY82(yyres)
}
            ,
            76 /* ptuple ::= LPAREN pexpr RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY82(yy1),) => {

	yyres = Val::Tuple(vec![yy1]);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            77 /* ptuple ::= LPAREN pargs RPAREN */
          | 120 /* tuple ::= LPAREN tuple_args RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY82(yy1),) => {

	yyres = Val::tuple_from_list(yy1);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            78 /* pargs ::= pexpr COMMA pexpr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	yyres = list::cons(yy0,
        list::cons(yy2,
        Val::Nil
        ));

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            79 /* pargs ::= pexpr COMMA pargs */
          | 84 /* plist_items ::= pexpr SEMICOLON pexpr */
          | 119 /* list_items ::= expr COMMA list_items */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            80 /* plist ::= SquareL SquareR */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match () {
 () => {

    yyres = list::empty();

} };
 YYMinorType::YY82(yyres)
}
            ,
            81 /* plist ::= SquareL plist_items SquareR */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY82(yy1),) => {

    yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            82 /* plist_items ::= pexpr */
          | 118 /* list_items ::= expr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY82(yy0),) => {

	yyres = list::singleton(yy0);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            83 /* plist_items ::= pexpr COMMA plist_items */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

    yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            87 /* expr ::= NOT expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY82(yy1),) => {

	yyres = sexpr::call(Val::id("bool_not".to_string()), list::singleton(yy1));

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            88 /* expr ::= expr ConcatNewline */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY82(yy0),) => {

	let newline = Val::Str(Arc::new("\n".to_string()));
	let args = list::cons(yy0, list::singleton(newline));
	yyres = sexpr::new(SexprType::StrExpr, args)

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            89 /* expr ::= NEGATE term */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY82(yy1),) => {

	yyres = sexpr::call(Val::id("negate".to_string()), list::singleton(yy1));

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            90 /* expr ::= expr PLUS expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	yyres = sexpr::binaryop("int_add".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            91 /* expr ::= expr MINUS expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	yyres = sexpr::binaryop("int_sub".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            92 /* expr ::= expr TIMES expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	yyres = sexpr::binaryop("int_mult".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            93 /* expr ::= expr SLASH expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	yyres = sexpr::binaryop("int_div".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            94 /* expr ::= expr MOD expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	yyres = sexpr::binaryop("int_mod".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            95 /* expr ::= expr AND expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	yyres = sexpr::binaryop("boolean_and".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            96 /* expr ::= expr OR expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	yyres = sexpr::binaryop("boolean_or".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            97 /* expr ::= expr XOR expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	yyres = sexpr::binaryop("boolean_xor".to_string(),yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            98 /* expr ::= expr LT expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	yyres = sexpr::binaryop("less_than".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            99 /* expr ::= expr LTEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	yyres = sexpr::binaryop("less_than_equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            100 /* expr ::= expr GT expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	yyres = sexpr::binaryop("greater_than".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            101 /* expr ::= expr GTEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	yyres = sexpr::binaryop("greater_than_equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            102 /* expr ::= expr EQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	yyres = sexpr::binaryop("equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            103 /* expr ::= expr NEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	let eq = sexpr::binaryop("equal".to_string(), yy0, yy2);
	yyres = sexpr::call(Val::id("bool_not".to_string()), list::singleton(eq));

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            105 /* term ::= LPAREN expr RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY82(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            107 /* term ::= VOID */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Val::Void;

} };
 YYMinorType::YY82(yyres)
}
            ,
            108 /* term ::= DollarQuestion */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Val::id("$".to_string());

} };
 YYMinorType::YY82(yyres)
}
            ,
            109 /* term ::= INT */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY186(yy0),) => {

	yyres = Val::Int(yy0);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            112 /* term ::= HASHTAG */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY209(yy0),) => {

	yyres = Val::Hashtag(Arc::new(yy0.data));

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            114 /* term ::= term DOT ID */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY209(yy2),) => {

    yyres = sexpr::new(SexprType::FieldAccess,
        list::cons(yy0,
        list::cons(Val::id(yy2.data),
        Val::Nil,
        ))
    );

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            115 /* list ::= SquareL SquareR */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match () {
 () => {

	yyres = list::empty();

} };
 YYMinorType::YY82(yyres)
}
            ,
            116 /* list ::= SquareL list_items SquareR */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY82(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            117 /* list ::= SquareL list_items SEMICOLON expr SquareR */
            => 
{
let yyres :  Val ;
let yyp4 = self.yystack.pop().unwrap();
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY82(yy1),YYMinorType::YY82(yy3),) => {

	yyres = sexpr::call(Val::id("list_cons".to_string()), list::from2(yy1, yy3));

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            121 /* tuple_args ::= expr COMMA expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	vout!("base tuple args!");
	yyres = list::cons(yy0, list::singleton(yy2));

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            122 /* tuple_args ::= expr COMMA tuple_args */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	vout!("additional tuple arg!");
	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            123 /* strexpr ::= StrOpen strlist StrClose */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY82(yy1),) => {

	yyres = sexpr::strexpr(yy1);
    vout!("strexpr({:?})\n", yyres);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            124 /* strlist ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

	yyres = Val::Nil;

} };
 YYMinorType::YY82(yyres)
}
            ,
            125 /* strlist ::= StrLit strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY158(yy0),YYMinorType::YY82(yy1),) => {

	yyres = list::cons(Val::new_str(yy0), yy1);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            127 /* strlist_term ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY209(yy0),) => {

    yyres = Val::id(yy0.data);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            128 /* strlist_term ::= strlist_term DOT ID */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY209(yy2),) => {

    yyres = sexpr::new(SexprType::FieldAccess,
        list::cons(yy0,
        list::cons(Val::id(yy2.data),
        Val::Nil,
    )))

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
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

