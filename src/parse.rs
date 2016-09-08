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
const YYNOCODE: i32 = 108;
type YYACTIONTYPE = u16;
const YYWILDCARD: YYCODETYPE = 1;
enum YYMinorType {
    YY0,
    YY39(Type),
    YY49(Ast),
    YY82(Val),
    YY132(TokenLoc),
    YY158(String),
    YY186(i64),
    YY209(TokenData<String>),
}
const YYNSTATE: i32 = 240;
const YYNRULE: i32 = 126;
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
    COLON( TokenLoc ), //2
    COMMA( TokenLoc ), //3
    DOUBLEDASH( TokenLoc ), //4
    ELSE( TokenLoc ), //5
    HASHTAG( TokenData<String> ), //6
    ID( TokenData<String> ), //7
    INT( i64 ), //8
    PLUS( TokenLoc ), //9
    SEMICOLON( TokenLoc ), //10
    SLASH( TokenLoc ), //11
    SquareL( TokenLoc ), //12
    SquareR( TokenLoc ), //13
    StrLit( String ), //14
    TYPE_ID( TokenData<String> ), //15
    ASSIGN, //16
    BLOCKARROW, //17
    RETURN, //18
    OR, //19
    XOR, //20
    AND, //21
    ConcatNewline, //22
    NOT, //23
    EQ, //24
    NEQ, //25
    GT, //26
    GTEQ, //27
    LT, //28
    LTEQ, //29
    MINUS, //30
    TIMES, //31
    MOD, //32
    DOLLAR, //33
    DOT, //34
    LPAREN, //35
    RPAREN, //36
    STRUCT, //37
    FAIL, //38
    FAILED, //39
    Let, //40
    Fork, //41
    Func, //42
    PARENCALL, //43
    TYPE_INT, //44
    TYPE_STR, //45
    TYPE_HASHTAG, //46
    TYPE_BOOL, //47
    TYPE_VOID, //48
    PS, //49
    MACRO, //50
    IF, //51
    PIPE, //52
    CASE, //53
    MATCH, //54
    True, //55
    False, //56
    UNDERSCORE, //57
    NEGATE, //58
    VOID, //59
    DollarQuestion, //60
    StrOpen, //61
    StrClose, //62
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
pub const TOKEN_PLUS: i32 = 9;
pub const TOKEN_SEMICOLON: i32 = 10;
pub const TOKEN_SLASH: i32 = 11;
pub const TOKEN_SquareL: i32 = 12;
pub const TOKEN_SquareR: i32 = 13;
pub const TOKEN_StrLit: i32 = 14;
pub const TOKEN_TYPE_ID: i32 = 15;
pub const TOKEN_ASSIGN: i32 = 16;
pub const TOKEN_BLOCKARROW: i32 = 17;
pub const TOKEN_RETURN: i32 = 18;
pub const TOKEN_OR: i32 = 19;
pub const TOKEN_XOR: i32 = 20;
pub const TOKEN_AND: i32 = 21;
pub const TOKEN_ConcatNewline: i32 = 22;
pub const TOKEN_NOT: i32 = 23;
pub const TOKEN_EQ: i32 = 24;
pub const TOKEN_NEQ: i32 = 25;
pub const TOKEN_GT: i32 = 26;
pub const TOKEN_GTEQ: i32 = 27;
pub const TOKEN_LT: i32 = 28;
pub const TOKEN_LTEQ: i32 = 29;
pub const TOKEN_MINUS: i32 = 30;
pub const TOKEN_TIMES: i32 = 31;
pub const TOKEN_MOD: i32 = 32;
pub const TOKEN_DOLLAR: i32 = 33;
pub const TOKEN_DOT: i32 = 34;
pub const TOKEN_LPAREN: i32 = 35;
pub const TOKEN_RPAREN: i32 = 36;
pub const TOKEN_STRUCT: i32 = 37;
pub const TOKEN_FAIL: i32 = 38;
pub const TOKEN_FAILED: i32 = 39;
pub const TOKEN_Let: i32 = 40;
pub const TOKEN_Fork: i32 = 41;
pub const TOKEN_Func: i32 = 42;
pub const TOKEN_PARENCALL: i32 = 43;
pub const TOKEN_TYPE_INT: i32 = 44;
pub const TOKEN_TYPE_STR: i32 = 45;
pub const TOKEN_TYPE_HASHTAG: i32 = 46;
pub const TOKEN_TYPE_BOOL: i32 = 47;
pub const TOKEN_TYPE_VOID: i32 = 48;
pub const TOKEN_PS: i32 = 49;
pub const TOKEN_MACRO: i32 = 50;
pub const TOKEN_IF: i32 = 51;
pub const TOKEN_PIPE: i32 = 52;
pub const TOKEN_CASE: i32 = 53;
pub const TOKEN_MATCH: i32 = 54;
pub const TOKEN_True: i32 = 55;
pub const TOKEN_False: i32 = 56;
pub const TOKEN_UNDERSCORE: i32 = 57;
pub const TOKEN_NEGATE: i32 = 58;
pub const TOKEN_VOID: i32 = 59;
pub const TOKEN_DollarQuestion: i32 = 60;
pub const TOKEN_StrOpen: i32 = 61;
pub const TOKEN_StrClose: i32 = 62;
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
        &Token::STRUCT => TOKEN_STRUCT,
        &Token::FAIL => TOKEN_FAIL,
        &Token::FAILED => TOKEN_FAILED,
        &Token::Let => TOKEN_Let,
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
const YY_ACTTAB_COUNT: i32 = 1206;
const YY_ACTION: [YYACTIONTYPE; 1206] = [
 /*     0 */   190,  196,  193,  240,   38,   34,    5,   27,   39,  229,
 /*    10 */   228,  203,   30,  145,  215,  205,  214,   16,  235,  236,
 /*    20 */    18,   17,   20,   19,   22,   21,   33,   29,   26,    8,
 /*    30 */   150,   59,  131,  128,  126,  124,  122,  206,  234,  233,
 /*    40 */   232,  231,  230,   11,  114,    3,  153,   79,   28,  192,
 /*    50 */   191,   42,   41,  195,  194,   46,   64,  190,  196,  193,
 /*    60 */     4,  295,   34,    5,   27,  222,  229,    1,   27,  143,
 /*    70 */   215,  217,  214,  201,   16,  165,   60,  366,  366,  366,
 /*    80 */   366,   22,   21,   33,   29,   26,    8,  150,   29,   26,
 /*    90 */    38,  150,  190,  196,  193,  234,  233,  232,  231,  230,
 /*   100 */    57,  185,   40,  229,   79,   28,  192,  191,   45,   41,
 /*   110 */   195,  194,   46,   62,  190,  196,  193,  135,   49,  202,
 /*   120 */     5,   32,   37,  229,  105,  215,  142,  214,  138,  178,
 /*   130 */    49,   16,  234,  233,  232,  231,  230,   78,  215,  200,
 /*   140 */   214,  192,  191,    8,    1,  195,  194,   46,  144,  215,
 /*   150 */   119,  214,  234,  233,  232,  231,  230,   47,  117,  150,
 /*   160 */   188,   79,   28,  192,  191,  116,   41,  195,  194,   46,
 /*   170 */   190,  196,  193,  227,  148,  184,    5,  154,   12,  229,
 /*   180 */    34,  185,   27,   47,  171,  139,   43,   16,   45,  181,
 /*   190 */   106,   83,  223,  179,  187,   43,  199,  155,  198,    8,
 /*   200 */   189,   33,   29,   26,  133,  150,  186,   43,  234,  233,
 /*   210 */   232,  231,  230,   58,  130,  129,   15,   79,   28,  192,
 /*   220 */   191,   76,   41,  195,  194,   46,  190,  196,  193,  224,
 /*   230 */   143,  215,    5,  214,  104,  229,  197,  175,  125,  189,
 /*   240 */    14,  123,   13,   16,  121,   75,  189,   73,  172,   48,
 /*   250 */    72,   74,   70,   69,  113,    8,  226,   68,  167,   67,
 /*   260 */   147,  164,  161,  156,  234,  233,  232,  231,  230,   31,
 /*   270 */    77,   40,    1,   79,   28,  192,  191,  216,   41,  195,
 /*   280 */   194,   46,  190,  196,  193,  120,  127,   71,    5,  227,
 /*   290 */   148,  229,   56,   44,  173,  170,  115,  118,  168,   16,
 /*   300 */   110,  112,  111,   60,  166,   10,  106,   80,  223,  159,
 /*   310 */   163,    8,  199,    9,  198,  137,  189,  160,  158,  109,
 /*   320 */   234,  233,  232,  231,  230,  134,  162,  128,   10,   79,
 /*   330 */    28,  192,  191,   63,   41,  195,  194,   46,  190,  196,
 /*   340 */   193,   50,   61,  368,    5,  227,  148,  229,  177,  157,
 /*   350 */    66,  146,  210,  209,  213,   16,  132,  368,   35,  169,
 /*   360 */   368,   65,  106,   86,  223,  368,  368,    8,  199,  368,
 /*   370 */   198,  182,  189,  368,  368,  368,  234,  233,  232,  231,
 /*   380 */   230,   36,  368,  368,  368,   79,   28,  192,  191,    7,
 /*   390 */    41,  195,  194,   46,  368,   34,  368,   27,  368,  368,
 /*   400 */   368,  212,  211,  208,  368,   24,   23,   25,  235,  368,
 /*   410 */    18,   17,   20,   19,   22,   21,   33,   29,   26,    7,
 /*   420 */   150,  368,  225,  368,  368,   34,  368,   27,  368,  368,
 /*   430 */   368,  368,  368,  368,  368,   24,   23,   25,  235,  368,
 /*   440 */    18,   17,   20,   19,   22,   21,   33,   29,   26,  368,
 /*   450 */   150,  368,  183,  367,  151,    2,  368,  220,  368,  368,
 /*   460 */   368,  368,  180,  368,  219,  368,  227,  148,  218,  368,
 /*   470 */   368,  368,  368,  368,  368,  368,  368,  238,  368,  368,
 /*   480 */   237,  221,  368,  106,   90,  223,  368,  368,    7,  199,
 /*   490 */   368,  198,  368,  189,   34,  368,   27,  368,  368,  368,
 /*   500 */   368,  368,  368,  368,   24,   23,   25,  235,  368,   18,
 /*   510 */    17,   20,   19,   22,   21,   33,   29,   26,  368,  150,
 /*   520 */    34,  368,   27,  368,  368,  368,  368,  368,  368,  368,
 /*   530 */    24,   23,   25,  235,  368,   18,   17,   20,   19,   22,
 /*   540 */    21,   33,   29,   26,  368,  150,   34,  176,   27,  368,
 /*   550 */   368,  368,  368,  368,  368,  368,   24,   23,   25,  235,
 /*   560 */   368,   18,   17,   20,   19,   22,   21,   33,   29,   26,
 /*   570 */     6,  150,  368,  183,  368,  368,   34,  368,   27,  368,
 /*   580 */   368,  368,  368,  368,  368,  368,   24,   23,   25,  235,
 /*   590 */   368,   18,   17,   20,   19,   22,   21,   33,   29,   26,
 /*   600 */   368,  150,   34,  368,   27,  368,  152,  368,  368,  368,
 /*   610 */   368,  368,   24,   23,   25,  235,  368,   18,   17,   20,
 /*   620 */    19,   22,   21,   33,   29,   26,  368,  150,   34,  368,
 /*   630 */    27,  368,  368,  368,  368,  368,    1,  368,   24,   23,
 /*   640 */    25,  235,  368,   18,   17,   20,   19,   22,   21,   33,
 /*   650 */    29,   26,  368,  150,   34,  368,   27,  368,  368,  368,
 /*   660 */   368,  368,  368,  368,   24,   23,   25,  235,  368,   18,
 /*   670 */    17,   20,   19,   22,   21,   33,   29,   26,  368,  150,
 /*   680 */   239,    2,  368,  220,  368,  368,  368,  368,  180,  368,
 /*   690 */   219,  368,  227,  148,  218,  368,  368,   40,  368,  368,
 /*   700 */   368,  368,  368,  238,  368,  368,  237,  221,  368,  106,
 /*   710 */    90,  223,  368,  174,    2,  199,  220,  198,  368,  189,
 /*   720 */   368,  180,  368,  219,  368,  227,  148,  218,  368,  368,
 /*   730 */   368,  368,  368,  368,  368,  368,  238,  368,  368,  237,
 /*   740 */   221,  368,  106,   90,  223,  368,  368,  368,  199,   34,
 /*   750 */   198,   27,  189,  368,  368,  368,  368,  368,  368,   24,
 /*   760 */    23,   25,  235,  368,   18,   17,   20,   19,   22,   21,
 /*   770 */    33,   29,   26,  368,  150,   34,  368,   27,  210,  209,
 /*   780 */   213,  368,  368,  368,   35,  368,  368,   25,  235,  368,
 /*   790 */    18,   17,   20,   19,   22,   21,   33,   29,   26,  368,
 /*   800 */   150,  368,  368,  210,  209,  213,  368,   36,  207,   35,
 /*   810 */   204,  368,  368,  368,  368,  368,  368,  368,  227,  148,
 /*   820 */   368,  227,  148,  368,  368,  368,  368,  212,  211,  208,
 /*   830 */   368,  368,   36,  368,  368,  106,   83,  149,  106,   81,
 /*   840 */   223,  199,  103,  198,  199,  189,  198,  107,  189,  368,
 /*   850 */   368,  368,  212,  211,  208,  227,  148,  368,  368,  108,
 /*   860 */   368,  368,  368,  368,  368,  368,  227,  148,  368,  227,
 /*   870 */   148,  368,  106,   53,  223,  368,  368,  368,  199,  368,
 /*   880 */   198,  368,  189,  106,  101,  223,  106,  102,  223,  199,
 /*   890 */   368,  198,  199,  189,  198,  368,  189,  227,  148,  368,
 /*   900 */   227,  148,  368,  368,  368,  368,  368,  368,  368,  368,
 /*   910 */   368,  368,  368,  368,  106,   84,  223,  106,   55,  223,
 /*   920 */   199,  368,  198,  199,  189,  198,  368,  189,  368,  368,
 /*   930 */   227,  148,  368,  227,  148,  368,  368,  368,  368,  368,
 /*   940 */   368,  368,  368,  368,  227,  148,  368,  106,   89,  223,
 /*   950 */   106,  136,  223,  199,  368,  198,  199,  189,  198,  368,
 /*   960 */   189,  106,   54,  223,  227,  148,  368,  199,  368,  198,
 /*   970 */   368,  189,  368,  368,  368,  227,  148,  368,  227,  148,
 /*   980 */   368,  106,  141,  223,  368,  368,  368,  199,  368,  198,
 /*   990 */   368,  189,  106,  140,  223,  106,   94,  223,  199,  368,
 /*  1000 */   198,  199,  189,  198,  368,  189,  368,  368,  227,  148,
 /*  1010 */   368,  227,  148,  368,  368,  368,  368,  368,  368,  368,
 /*  1020 */   368,  368,  227,  148,  368,  106,   92,  223,  106,   91,
 /*  1030 */   223,  199,  368,  198,  199,  189,  198,  368,  189,  106,
 /*  1040 */   100,  223,  227,  148,  368,  199,  368,  198,  368,  189,
 /*  1050 */   368,  368,  368,  227,  148,  368,  227,  148,  368,  106,
 /*  1060 */    99,  223,  368,  368,  368,  199,  368,  198,  368,  189,
 /*  1070 */   106,   98,  223,  106,   97,  223,  199,  368,  198,  199,
 /*  1080 */   189,  198,  368,  189,  368,  368,  227,  148,  368,  227,
 /*  1090 */   148,  368,  368,  368,  368,  368,  368,  368,  368,  368,
 /*  1100 */   227,  148,  368,  106,   96,  223,  106,   95,  223,  199,
 /*  1110 */   368,  198,  199,  189,  198,  368,  189,  106,   93,  223,
 /*  1120 */   227,  148,  368,  199,  368,  198,  368,  189,  368,  368,
 /*  1130 */   368,  227,  148,  368,  227,  148,  368,  106,   85,  223,
 /*  1140 */   368,  368,  368,  199,  368,  198,  368,  189,  106,   88,
 /*  1150 */   223,  106,   87,  223,  199,  368,  198,  199,  189,  198,
 /*  1160 */   368,  189,  368,  368,  227,  148,  368,  227,  148,  368,
 /*  1170 */   368,  368,  368,  368,  368,  368,  368,  368,  227,  148,
 /*  1180 */   368,  106,   52,  223,  106,   82,  223,  199,  368,  198,
 /*  1190 */   199,  189,  198,  368,  189,  106,   51,  223,  368,  368,
 /*  1200 */   368,  199,  368,  198,  368,  189,
];
const YY_LOOKAHEAD: [YYCODETYPE; 1206] = [
 /*     0 */     6,    7,    8,    0,    3,    9,   12,   11,   10,   15,
 /*    10 */    13,   13,   18,   83,   84,   85,   86,   23,   22,    7,
 /*    20 */    24,   25,   26,   27,   28,   29,   30,   31,   32,   35,
 /*    30 */    34,   37,   38,   39,   40,   41,   42,   36,   44,   45,
 /*    40 */    46,   47,   48,   10,   50,   51,   13,   53,   54,   55,
 /*    50 */    56,   33,   58,   59,   60,   61,    5,    6,    7,    8,
 /*    60 */    43,   43,    9,   12,   11,    4,   15,   17,   11,   83,
 /*    70 */    84,    4,   86,   87,   23,    4,    5,   24,   25,   26,
 /*    80 */    27,   28,   29,   30,   31,   32,   35,   34,   31,   32,
 /*    90 */     3,   34,    6,    7,    8,   44,   45,   46,   47,   48,
 /*   100 */    12,    7,   52,   15,   53,   54,   55,   56,   14,   58,
 /*   110 */    59,   60,   61,    5,    6,    7,    8,   89,   90,   13,
 /*   120 */    12,   35,    3,   15,   83,   84,   85,   86,   34,   89,
 /*   130 */    90,   23,   44,   45,   46,   47,   48,   83,   84,   36,
 /*   140 */    86,   55,   56,   35,   17,   59,   60,   61,   83,   84,
 /*   150 */    67,   86,   44,   45,   46,   47,   48,   73,   74,   34,
 /*   160 */    62,   53,   54,   55,   56,   82,   58,   59,   60,   61,
 /*   170 */     6,    7,    8,   77,   78,    7,   12,   13,   51,   15,
 /*   180 */     9,    7,   11,   73,   74,  105,  106,   23,   14,   36,
 /*   190 */    94,   95,   96,    4,  105,  106,  100,  101,  102,   35,
 /*   200 */   104,   30,   31,   32,    7,   34,  105,  106,   44,   45,
 /*   210 */    46,   47,   48,    2,   35,    6,    3,   53,   54,   55,
 /*   220 */    56,    7,   58,   59,   60,   61,    6,    7,    8,   94,
 /*   230 */    83,   84,   12,   86,   87,   15,   94,    4,    7,  104,
 /*   240 */    16,    7,   16,   23,    7,   43,  104,    4,    4,   17,
 /*   250 */     4,   36,    3,   43,    7,   35,   36,   36,    4,    3,
 /*   260 */    99,    4,    4,   36,   44,   45,   46,   47,   48,   52,
 /*   270 */    67,   52,   17,   53,   54,   55,   56,   82,   58,   59,
 /*   280 */    60,   61,    6,    7,    8,   69,   82,    7,   12,   77,
 /*   290 */    78,   15,    2,   97,   72,   72,   97,   49,   69,   23,
 /*   300 */     7,   76,   67,    5,   76,   52,   94,   95,   96,   67,
 /*   310 */    80,   35,  100,   52,  102,  103,  104,   81,   67,   80,
 /*   320 */    44,   45,   46,   47,   48,   34,   67,   39,   52,   53,
 /*   330 */    54,   55,   56,   67,   58,   59,   60,   61,    6,    7,
 /*   340 */     8,   96,   67,  107,   12,   77,   78,   15,   96,   99,
 /*   350 */    67,   82,    6,    7,    8,   23,   96,  107,   12,   96,
 /*   360 */   107,   67,   94,   95,   96,  107,  107,   35,  100,  107,
 /*   370 */   102,  103,  104,  107,  107,  107,   44,   45,   46,   47,
 /*   380 */    48,   35,  107,  107,  107,   53,   54,   55,   56,    3,
 /*   390 */    58,   59,   60,   61,  107,    9,  107,   11,  107,  107,
 /*   400 */   107,   55,   56,   57,  107,   19,   20,   21,   22,  107,
 /*   410 */    24,   25,   26,   27,   28,   29,   30,   31,   32,    3,
 /*   420 */    34,  107,   36,  107,  107,    9,  107,   11,  107,  107,
 /*   430 */   107,  107,  107,  107,  107,   19,   20,   21,   22,  107,
 /*   440 */    24,   25,   26,   27,   28,   29,   30,   31,   32,  107,
 /*   450 */    34,  107,   36,   64,   65,   66,  107,   68,  107,  107,
 /*   460 */   107,  107,   73,  107,   75,  107,   77,   78,   79,  107,
 /*   470 */   107,  107,  107,  107,  107,  107,  107,   88,  107,  107,
 /*   480 */    91,   92,  107,   94,   95,   96,  107,  107,    3,  100,
 /*   490 */   107,  102,  107,  104,    9,  107,   11,  107,  107,  107,
 /*   500 */   107,  107,  107,  107,   19,   20,   21,   22,  107,   24,
 /*   510 */    25,   26,   27,   28,   29,   30,   31,   32,  107,   34,
 /*   520 */     9,  107,   11,  107,  107,  107,  107,  107,  107,  107,
 /*   530 */    19,   20,   21,   22,  107,   24,   25,   26,   27,   28,
 /*   540 */    29,   30,   31,   32,  107,   34,    9,   36,   11,  107,
 /*   550 */   107,  107,  107,  107,  107,  107,   19,   20,   21,   22,
 /*   560 */   107,   24,   25,   26,   27,   28,   29,   30,   31,   32,
 /*   570 */     3,   34,  107,   36,  107,  107,    9,  107,   11,  107,
 /*   580 */   107,  107,  107,  107,  107,  107,   19,   20,   21,   22,
 /*   590 */   107,   24,   25,   26,   27,   28,   29,   30,   31,   32,
 /*   600 */   107,   34,    9,  107,   11,  107,   13,  107,  107,  107,
 /*   610 */   107,  107,   19,   20,   21,   22,  107,   24,   25,   26,
 /*   620 */    27,   28,   29,   30,   31,   32,  107,   34,    9,  107,
 /*   630 */    11,  107,  107,  107,  107,  107,   17,  107,   19,   20,
 /*   640 */    21,   22,  107,   24,   25,   26,   27,   28,   29,   30,
 /*   650 */    31,   32,  107,   34,    9,  107,   11,  107,  107,  107,
 /*   660 */   107,  107,  107,  107,   19,   20,   21,   22,  107,   24,
 /*   670 */    25,   26,   27,   28,   29,   30,   31,   32,  107,   34,
 /*   680 */    65,   66,  107,   68,  107,  107,  107,  107,   73,  107,
 /*   690 */    75,  107,   77,   78,   79,  107,  107,   52,  107,  107,
 /*   700 */   107,  107,  107,   88,  107,  107,   91,   92,  107,   94,
 /*   710 */    95,   96,  107,   65,   66,  100,   68,  102,  107,  104,
 /*   720 */   107,   73,  107,   75,  107,   77,   78,   79,  107,  107,
 /*   730 */   107,  107,  107,  107,  107,  107,   88,  107,  107,   91,
 /*   740 */    92,  107,   94,   95,   96,  107,  107,  107,  100,    9,
 /*   750 */   102,   11,  104,  107,  107,  107,  107,  107,  107,   19,
 /*   760 */    20,   21,   22,  107,   24,   25,   26,   27,   28,   29,
 /*   770 */    30,   31,   32,  107,   34,    9,  107,   11,    6,    7,
 /*   780 */     8,  107,  107,  107,   12,  107,  107,   21,   22,  107,
 /*   790 */    24,   25,   26,   27,   28,   29,   30,   31,   32,  107,
 /*   800 */    34,  107,  107,    6,    7,    8,  107,   35,   36,   12,
 /*   810 */    13,  107,  107,  107,  107,  107,  107,  107,   77,   78,
 /*   820 */   107,   77,   78,  107,  107,  107,  107,   55,   56,   57,
 /*   830 */   107,  107,   35,  107,  107,   94,   95,   96,   94,   95,
 /*   840 */    96,  100,  101,  102,  100,  104,  102,  103,  104,  107,
 /*   850 */   107,  107,   55,   56,   57,   77,   78,  107,  107,   81,
 /*   860 */   107,  107,  107,  107,  107,  107,   77,   78,  107,   77,
 /*   870 */    78,  107,   94,   95,   96,  107,  107,  107,  100,  107,
 /*   880 */   102,  107,  104,   94,   95,   96,   94,   95,   96,  100,
 /*   890 */   107,  102,  100,  104,  102,  107,  104,   77,   78,  107,
 /*   900 */    77,   78,  107,  107,  107,  107,  107,  107,  107,  107,
 /*   910 */   107,  107,  107,  107,   94,   95,   96,   94,   95,   96,
 /*   920 */   100,  107,  102,  100,  104,  102,  107,  104,  107,  107,
 /*   930 */    77,   78,  107,   77,   78,  107,  107,  107,  107,  107,
 /*   940 */   107,  107,  107,  107,   77,   78,  107,   94,   95,   96,
 /*   950 */    94,   95,   96,  100,  107,  102,  100,  104,  102,  107,
 /*   960 */   104,   94,   95,   96,   77,   78,  107,  100,  107,  102,
 /*   970 */   107,  104,  107,  107,  107,   77,   78,  107,   77,   78,
 /*   980 */   107,   94,   95,   96,  107,  107,  107,  100,  107,  102,
 /*   990 */   107,  104,   94,   95,   96,   94,   95,   96,  100,  107,
 /*  1000 */   102,  100,  104,  102,  107,  104,  107,  107,   77,   78,
 /*  1010 */   107,   77,   78,  107,  107,  107,  107,  107,  107,  107,
 /*  1020 */   107,  107,   77,   78,  107,   94,   95,   96,   94,   95,
 /*  1030 */    96,  100,  107,  102,  100,  104,  102,  107,  104,   94,
 /*  1040 */    95,   96,   77,   78,  107,  100,  107,  102,  107,  104,
 /*  1050 */   107,  107,  107,   77,   78,  107,   77,   78,  107,   94,
 /*  1060 */    95,   96,  107,  107,  107,  100,  107,  102,  107,  104,
 /*  1070 */    94,   95,   96,   94,   95,   96,  100,  107,  102,  100,
 /*  1080 */   104,  102,  107,  104,  107,  107,   77,   78,  107,   77,
 /*  1090 */    78,  107,  107,  107,  107,  107,  107,  107,  107,  107,
 /*  1100 */    77,   78,  107,   94,   95,   96,   94,   95,   96,  100,
 /*  1110 */   107,  102,  100,  104,  102,  107,  104,   94,   95,   96,
 /*  1120 */    77,   78,  107,  100,  107,  102,  107,  104,  107,  107,
 /*  1130 */   107,   77,   78,  107,   77,   78,  107,   94,   95,   96,
 /*  1140 */   107,  107,  107,  100,  107,  102,  107,  104,   94,   95,
 /*  1150 */    96,   94,   95,   96,  100,  107,  102,  100,  104,  102,
 /*  1160 */   107,  104,  107,  107,   77,   78,  107,   77,   78,  107,
 /*  1170 */   107,  107,  107,  107,  107,  107,  107,  107,   77,   78,
 /*  1180 */   107,   94,   95,   96,   94,   95,   96,  100,  107,  102,
 /*  1190 */   100,  104,  102,  107,  104,   94,   95,   96,  107,  107,
 /*  1200 */   107,  100,  107,  102,  107,  104,
];
const YY_SHIFT_USE_DFLT: i32 = -7;
const YY_SHIFT_COUNT: i32 = 151;
const YY_SHIFT_MIN: i32 = -6;
const YY_SHIFT_MAX: i32 = 797;
const YY_SHIFT_OFST: [i16; 152] = [
 /*     0 */    -6,   -6,   -6,  276,  220,  164,  332,  332,  332,  108,
 /*    10 */    51,  332,  332,  332,  332,  332,  332,  332,  332,  332,
 /*    20 */   332,  332,  332,  332,  332,  332,  332,  332,  332,  332,
 /*    30 */   332,  332,  332,  332,  332,  797,  772,  346,  346,  346,
 /*    40 */   346,   86,   86,   94,   50,  174,  174,  288,  288,  291,
 /*    50 */   291,  619,  619,  619,  645,  619,   88,   88,   88,   88,
 /*    60 */   127,   71,  255,  261,  255,  253,  298,  293,  255,  293,
 /*    70 */   280,  290,  248,  248,  290,  280,  219,  219,  255,  217,
 /*    80 */   416,  386,  593,  567,  537,  511,  485,  740,  740,  740,
 /*    90 */   740,  766,  766,   -4,   -4,   53,   53,   53,   53,  171,
 /*   100 */   171,   57,   57,   33,   -2,    1,   18,  227,  258,  257,
 /*   110 */   256,  254,  221,  210,  247,  249,  246,  244,  232,  243,
 /*   120 */   215,  202,  237,  226,  234,  224,  231,  233,  214,  213,
 /*   130 */   209,  179,   -3,  211,  197,  189,  125,  153,  168,   98,
 /*   140 */   125,  125,  103,  119,  106,   87,   67,   61,   17,   -3,
 /*   150 */    12,    3,
];
const YY_REDUCE_USE_DFLT: i32 = -71;
const YY_REDUCE_COUNT: i32 = 79;
const YY_REDUCE_MIN: i32 = -70;
const YY_REDUCE_MAX: i32 = 1101;
const YY_REDUCE_OFST: [i16; 80] = [
 /*     0 */   389,  648,  615,  778,  744,  741,   96,  268,  212,  823,
 /*    10 */  1101, 1090, 1087, 1057, 1054, 1043, 1023, 1012, 1009,  979,
 /*    20 */   976,  965,  945,  934,  931,  901,  898,  887,  867,  856,
 /*    30 */   853,  823,  820,  792,  789,  147,   41,  -14,  -70,   65,
 /*    40 */    54,  142,  135,  101,   83,   89,   80,  110,   84,   40,
 /*    50 */    28,  294,  283,  275,  269,  266,  263,  260,  252,  245,
 /*    60 */   259,  239,  251,  250,  242,  236,  230,  228,  235,  225,
 /*    70 */   229,  199,  223,  222,  196,  216,  204,  195,  203,  161,
];
const YY_DEFAULT: [YYACTIONTYPE; 240] = [
 /*     0 */   241,  241,  241,  366,  366,  366,  366,  366,  366,  366,
 /*    10 */   366,  366,  366,  366,  366,  366,  366,  366,  366,  366,
 /*    20 */   366,  366,  366,  366,  366,  366,  366,  366,  366,  366,
 /*    30 */   366,  366,  366,  366,  366,  366,  366,  366,  366,  366,
 /*    40 */   366,  366,  366,  361,  366,  361,  361,  277,  366,  254,
 /*    50 */   254,  366,  366,  366,  366,  366,  366,  366,  366,  366,
 /*    60 */   366,  366,  366,  366,  366,  366,  287,  280,  366,  280,
 /*    70 */   263,  266,  275,  275,  266,  263,  366,  303,  366,  366,
 /*    80 */   366,  366,  366,  355,  366,  366,  358,  259,  258,  250,
 /*    90 */   245,  335,  334,  325,  333,  341,  340,  339,  338,  337,
 /*   100 */   336,  328,  329,  366,  366,  366,  342,  366,  366,  366,
 /*   110 */   281,  366,  366,  366,  366,  264,  366,  366,  366,  366,
 /*   120 */   366,  366,  366,  366,  366,  366,  366,  366,  366,  366,
 /*   130 */   366,  366,  366,  366,  366,  366,  330,  366,  366,  366,
 /*   140 */   332,  331,  366,  320,  366,  315,  366,  366,  366,  296,
 /*   150 */   366,  366,  354,  353,  352,  356,  294,  300,  299,  290,
 /*   160 */   289,  285,  288,  286,  284,  283,  282,  279,  265,  267,
 /*   170 */   262,  278,  276,  261,  260,  257,  256,  255,  253,  252,
 /*   180 */   251,  357,  359,  343,  365,  364,  363,  362,  360,  351,
 /*   190 */   350,  349,  348,  347,  346,  345,  344,  327,  324,  323,
 /*   200 */   314,  321,  319,  318,  317,  316,  313,  312,  311,  310,
 /*   210 */   309,  308,  307,  306,  305,  304,  302,  301,  249,  248,
 /*   220 */   247,  246,  298,  296,  297,  293,  292,  291,  274,  273,
 /*   230 */   272,  271,  270,  269,  268,  326,  322,  244,  243,  242,
];

/* TMPL: fallback tokens */

const YY_FALLBACK: [i32; 0] = [
];

/* TMPL: symbol names */


/* TMPL: rules */


/* TMPL: destructors */


/* TMPL: stack-overflow */


/* TMPL: stack-overflow */

const YY_RULE_INFO: [YYCODETYPE; 126] = [
  64,
  65,
  65,
  66,
  66,
  66,
  66,
  66,
  66,
  66,
  66,
  66,
  88,
  89,
  89,
  90,
  92,
  73,
  91,
  91,
  67,
  68,
  68,
  69,
  69,
  69,
  97,
  97,
  96,
  96,
  96,
  96,
  96,
  96,
  96,
  72,
  72,
  74,
  74,
  75,
  76,
  76,
  76,
  79,
  79,
  79,
  80,
  80,
  80,
  81,
  81,
  95,
  77,
  77,
  77,
  78,
  78,
  95,
  95,
  99,
  99,
  95,
  82,
  82,
  83,
  83,
  83,
  83,
  83,
  83,
  83,
  83,
  84,
  84,
  84,
  85,
  85,
  86,
  86,
  86,
  87,
  87,
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
            0 /* program ::= stmts */
            => 
{
let yyres :  Ast ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY82(yy0),) => {

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
          | 4 /* stmt ::= let_stmt */
          | 6 /* stmt ::= fail_stmt */
          | 7 /* stmt ::= func_stmt */
          | 8 /* stmt ::= macro_stmt */
          | 9 /* stmt ::= if_stmt */
          | 11 /* stmt ::= failed_stmt */
          | 64 /* pexpr ::= ptuple */
          | 65 /* pexpr ::= plist */
          | 83 /* expr ::= list */
          | 84 /* expr ::= tuple */
          | 102 /* expr ::= term */
          | 111 /* term ::= strexpr */
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
            5 /* stmt ::= expr */
          | 51 /* expr ::= call_expr */
          | 55 /* func_term ::= term */
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
            10 /* stmt ::= RETURN expr */
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
            12 /* defstruct ::= STRUCT typex defstruct_fields DOUBLEDASH */
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
            13 /* defstruct_fields ::= defstruct_field defstruct_fields */
          | 123 /* strlist ::= strlist_term strlist */
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
            14 /* defstruct_fields ::= */
          | 23 /* dfunc_args ::= */
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
            15 /* defstruct_field ::= DOT ID COLON typex */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY209(yy1),YYMinorType::YY39(yy3),) => {

	yyres = sexpr::id_with_type(yy1.data, yy3);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            16 /* fail_stmt ::= FAIL LPAREN HASHTAG COMMA expr RPAREN */
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
            17 /* failed_stmt ::= FAILED ID match_case DOUBLEDASH */
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
            18 /* let_stmt ::= Let ID ASSIGN expr */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
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
            19 /* let_stmt ::= Fork ID ASSIGN expr */
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
            20 /* block ::= BLOCKARROW stmts */
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
            21 /* func_stmt ::= Func ID PARENCALL dfunc_args RPAREN opt_typex block DOUBLEDASH opt_ps */
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
            22 /* func_stmt ::= Func ID PARENCALL dfunc_args RPAREN opt_typex match_case DOUBLEDASH opt_ps */
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
            24 /* dfunc_args ::= ID opt_typex */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY209(yy0),YYMinorType::YY39(yy1),) => {

	yyres = list::singleton(sexpr::id_with_type(yy0.data, yy1));

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
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
 (YYMinorType::YY209(yy0),YYMinorType::YY39(yy1),YYMinorType::YY82(yy3),) => {

	yyres = list::cons(sexpr::id_with_type(yy0.data, yy1), yy3);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
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
 YYMinorType::YY39(yyres)
}
            ,
            27 /* opt_typex ::= COLON typex */
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
            28 /* typex ::= TYPE_INT */
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
            29 /* typex ::= TYPE_STR */
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
            30 /* typex ::= TYPE_HASHTAG */
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
            31 /* typex ::= TYPE_BOOL */
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
            32 /* typex ::= TYPE_VOID */
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
            33 /* typex ::= TYPE_ID */
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
            34 /* typex ::= SquareL typex SquareR */
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
            35 /* opt_ps ::= */
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
            36 /* opt_ps ::= PS BLOCKARROW failed_stmts DOUBLEDASH */
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
            37 /* failed_stmts ::= failed_stmt */
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
            38 /* failed_stmts ::= failed_stmt failed_stmts */
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
            39 /* macro_stmt ::= MACRO ID PARENCALL macro_args RPAREN block DOUBLEDASH */
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
            40 /* macro_args ::= */
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
            41 /* macro_args ::= ID */
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
            42 /* macro_args ::= ID COMMA macro_args */
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
            43 /* if_stmt ::= IF expr block DOUBLEDASH */
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
            44 /* if_stmt ::= IF expr block else_if DOUBLEDASH */
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
            45 /* if_stmt ::= IF if_case DOUBLEDASH */
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
            46 /* else_if ::= ELSE IF expr block else_if */
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
            47 /* else_if ::= ELSE IF expr block */
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
            48 /* else_if ::= ELSE block */
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
            49 /* if_case ::= PIPE expr block if_case */
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
            50 /* if_case ::= PIPE ELSE block */
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
            52 /* call_expr ::= func_term PARENCALL RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY82(yy0),) => {

	vout!("zero param function call!");
	yyres = sexpr::call(yy0, vec![]);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            53 /* call_expr ::= func_term PARENCALL expr RPAREN */
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
	yyres = sexpr::call(yy0, vec![yy2]);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            54 /* call_expr ::= func_term PARENCALL tuple_args RPAREN */
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
	yyres = sexpr::call(yy0, list::to_vec(yy2));

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            56 /* func_term ::= typex */
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
            57 /* expr ::= term DOLLAR term */
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
            58 /* expr ::= CASE cases DOUBLEDASH */
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
            59 /* cases ::= PIPE expr block PIPE ELSE block */
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
            60 /* cases ::= PIPE expr block cases */
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
            61 /* expr ::= MATCH expr match_case DOUBLEDASH */
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
            62 /* match_case ::= PIPE pexpr block match_case */
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
    yyres = sexpr::match_case(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            63 /* match_case ::= PIPE pexpr block */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy1),YYMinorType::YY82(yy2),) => {

    vout!("parsed base match case\n");
    yyres = sexpr::match_case(yy1, yy2, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            66 /* pexpr ::= INT */
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
            67 /* pexpr ::= True */
          | 108 /* term ::= True */
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
            68 /* pexpr ::= False */
          | 109 /* term ::= False */
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
            69 /* pexpr ::= HASHTAG */
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
            70 /* pexpr ::= ID */
          | 104 /* term ::= ID */
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
            71 /* pexpr ::= UNDERSCORE */
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
            72 /* ptuple ::= LPAREN RPAREN */
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
            73 /* ptuple ::= LPAREN pexpr RPAREN */
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
            74 /* ptuple ::= LPAREN pargs RPAREN */
          | 117 /* tuple ::= LPAREN tuple_args RPAREN */
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
            75 /* pargs ::= pexpr COMMA pexpr */
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
            76 /* pargs ::= pexpr COMMA pargs */
          | 116 /* list_items ::= expr COMMA list_items */
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
            77 /* plist ::= SquareL SquareR */
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
            78 /* plist ::= SquareL plist_items SquareR */
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
            79 /* plist ::= SquareL plist_items SEMICOLON pexpr SquareR */
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

	yyres = list::cons(yy1, yy3);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            80 /* plist_items ::= pexpr */
          | 115 /* list_items ::= expr */
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
            81 /* plist_items ::= pexpr COMMA plist_items */
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
            82 /* expr ::= expr DOT ID */
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
            85 /* expr ::= NOT expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY82(yy1),) => {

	yyres = sexpr::call(Val::id("bool_not".to_string()), vec![yy1]);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            86 /* expr ::= expr ConcatNewline */
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
            87 /* expr ::= NEGATE term */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY82(yy1),) => {

	yyres = sexpr::call(Val::id("negate".to_string()), vec![yy1]);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            88 /* expr ::= expr PLUS expr */
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
            89 /* expr ::= expr MINUS expr */
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
            90 /* expr ::= expr TIMES expr */
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
            91 /* expr ::= expr SLASH expr */
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
            92 /* expr ::= expr MOD expr */
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
            93 /* expr ::= expr AND expr */
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
            94 /* expr ::= expr OR expr */
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
            95 /* expr ::= expr XOR expr */
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
            96 /* expr ::= expr LT expr */
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
            97 /* expr ::= expr LTEQ expr */
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
            98 /* expr ::= expr GT expr */
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
            99 /* expr ::= expr GTEQ expr */
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
            100 /* expr ::= expr EQ expr */
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
            101 /* expr ::= expr NEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	let eq = sexpr::binaryop("equal".to_string(), yy0, yy2);
	yyres = sexpr::call(Val::id("bool_not".to_string()), vec![eq]);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            103 /* term ::= LPAREN expr RPAREN */
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
            105 /* term ::= VOID */
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
            106 /* term ::= DollarQuestion */
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
            107 /* term ::= INT */
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
            110 /* term ::= HASHTAG */
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
            112 /* list ::= SquareL SquareR */
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
            113 /* list ::= SquareL list_items SquareR */
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
            114 /* list ::= SquareL list_items SEMICOLON expr SquareR */
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

	yyres = sexpr::call(Val::id("list_cons".to_string()), vec![yy1, yy3]);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            118 /* tuple_args ::= expr COMMA expr */
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
            119 /* tuple_args ::= expr COMMA tuple_args */
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
            120 /* strexpr ::= StrOpen strlist StrClose */
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
            121 /* strlist ::= */
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
            122 /* strlist ::= StrLit strlist */
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
            124 /* strlist_term ::= ID */
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
            125 /* strlist_term ::= strlist_term DOT ID */
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

