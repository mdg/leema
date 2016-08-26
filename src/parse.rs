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
const YYNOCODE: i32 = 107;
type YYACTIONTYPE = u16;
const YYWILDCARD: YYCODETYPE = 1;
enum YYMinorType {
    YY0,
    YY56(Val),
    YY68(TokenLoc),
    YY117(Ast),
    YY155(Type),
    YY177(TokenData<String>),
    YY192(String),
    YY212(i64),
}
const YYNSTATE: i32 = 234;
const YYNRULE: i32 = 123;
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
        Token::COLON(x) => YYMinorType::YY68(x),
        Token::COMMA(x) => YYMinorType::YY68(x),
        Token::DOUBLEDASH(x) => YYMinorType::YY68(x),
        Token::ELSE(x) => YYMinorType::YY68(x),
        Token::HASHTAG(x) => YYMinorType::YY177(x),
        Token::ID(x) => YYMinorType::YY177(x),
        Token::INT(x) => YYMinorType::YY212(x),
        Token::PLUS(x) => YYMinorType::YY68(x),
        Token::SEMICOLON(x) => YYMinorType::YY68(x),
        Token::SLASH(x) => YYMinorType::YY68(x),
        Token::SquareL(x) => YYMinorType::YY68(x),
        Token::SquareR(x) => YYMinorType::YY68(x),
        Token::StrLit(x) => YYMinorType::YY192(x),
        Token::TYPE_ID(x) => YYMinorType::YY177(x),
        _ => YYMinorType::YY0
  }
}
const YY_ACTTAB_COUNT: i32 = 1370;
const YY_ACTION: [YYACTIONTYPE; 1370] = [
 /*     0 */   187,  193,  190,  234,   37,   35,    5,   28,   12,  223,
 /*    10 */   222,  149,   31,  141,  209,  199,  208,   17,  229,  230,
 /*    20 */    19,   18,   21,   20,   23,   22,   34,   30,   27,    8,
 /*    30 */   146,   57,  129,  126,  124,  122,  120,  200,  228,  227,
 /*    40 */   226,  225,  224,   55,  112,    3,  223,   77,   29,  189,
 /*    50 */   188,   28,   39,  192,  191,   44,  187,  193,  190,   40,
 /*    60 */   161,   58,    5,  198,    4,  223,  221,  144,  216,  289,
 /*    70 */   211,   30,   27,   17,  146,  228,  227,  226,  225,  224,
 /*    80 */   182,   37,  104,   78,  217,    8,  182,   43,  196,    1,
 /*    90 */   195,  136,  186,   43,  228,  227,  226,  225,  224,  103,
 /*   100 */   209,  135,  208,   77,   29,  189,  188,  137,   39,  192,
 /*   110 */   191,   44,   62,  187,  193,  190,  133,   47,   35,    5,
 /*   120 */    28,  146,  223,   13,   76,  209,  181,  208,   45,  115,
 /*   130 */    17,  174,   47,  357,  357,  357,  357,   23,   22,   34,
 /*   140 */    30,   27,    8,  146,  185,    1,  138,   41,  187,  193,
 /*   150 */   190,  228,  227,  226,  225,  224,   45,  167,  184,   41,
 /*   160 */    77,   29,  189,  188,  175,   39,  192,  191,   44,   60,
 /*   170 */   187,  193,  190,  117,  183,   41,    5,   33,  131,  223,
 /*   180 */    38,  218,  194,  178,  177,   56,  128,   17,  114,   16,
 /*   190 */   127,  186,  186,  171,   74,  123,   15,  189,  188,    8,
 /*   200 */   121,  192,  191,   44,  119,   73,   14,   71,  228,  227,
 /*   210 */   226,  225,  224,   72,  168,   46,   70,   77,   29,  189,
 /*   220 */   188,   68,   39,  192,  191,   44,  187,  193,  190,  111,
 /*   230 */   221,  144,    5,  150,   67,  223,   35,   66,   28,  163,
 /*   240 */    65,  152,  160,   17,  157,   75,  104,   81,  217,    1,
 /*   250 */   143,   32,  196,  151,  195,    8,  186,   34,   30,   27,
 /*   260 */    38,  146,   69,  210,  228,  227,  226,  225,  224,  118,
 /*   270 */    42,   54,  116,   77,   29,  189,  188,  125,   39,  192,
 /*   280 */   191,   44,  187,  193,  190,  169,  166,  110,    5,  108,
 /*   290 */   113,  223,  109,  164,   58,  162,  159,   10,  156,   17,
 /*   300 */   155,    9,  153,  154,   48,  132,  107,  126,  158,   61,
 /*   310 */   173,    8,  220,  142,   59,   64,  130,   63,  359,  359,
 /*   320 */   228,  227,  226,  225,  224,  165,  359,  359,  359,   77,
 /*   330 */    29,  189,  188,  359,   39,  192,  191,   44,  187,  193,
 /*   340 */   190,  359,  221,  144,    5,  359,  359,  223,  359,  359,
 /*   350 */   359,  359,  359,  359,  359,   17,  359,  359,  104,   81,
 /*   360 */   145,  359,  359,  359,  196,  102,  195,    8,  186,  359,
 /*   370 */   359,  359,  359,  359,  359,  359,  228,  227,  226,  225,
 /*   380 */   224,  359,  359,  359,   10,   77,   29,  189,  188,  359,
 /*   390 */    39,  192,  191,   44,  187,  193,  190,  359,  221,  144,
 /*   400 */     5,  359,  106,  223,  359,  359,  359,  359,  359,  359,
 /*   410 */   359,   17,  359,  359,  104,   51,  217,  359,  359,  359,
 /*   420 */   196,  359,  195,    8,  186,  359,  359,  359,  359,  359,
 /*   430 */   359,  359,  228,  227,  226,  225,  224,  359,  359,  359,
 /*   440 */   359,   77,   29,  189,  188,    7,   39,  192,  191,   44,
 /*   450 */   359,   35,  359,   28,  359,  359,  359,  359,  359,  359,
 /*   460 */   359,   25,   24,   26,  229,  359,   19,   18,   21,   20,
 /*   470 */    23,   22,   34,   30,   27,    7,  146,  359,  219,  359,
 /*   480 */   359,   35,  359,   28,  359,  359,  359,  359,  359,  359,
 /*   490 */   359,   25,   24,   26,  229,  359,   19,   18,   21,   20,
 /*   500 */    23,   22,   34,   30,   27,  359,  146,  359,  180,  358,
 /*   510 */   147,    2,  359,  214,  359,  359,  359,  359,  176,  359,
 /*   520 */   213,  359,  221,  144,  212,  359,  359,  359,  359,  359,
 /*   530 */   359,  359,  232,  359,  359,  231,  215,  359,  104,   89,
 /*   540 */   217,  359,  359,  359,  196,   35,  195,   28,  186,  197,
 /*   550 */   359,  359,  359,  359,  359,   25,   24,   26,  229,  359,
 /*   560 */    19,   18,   21,   20,   23,   22,   34,   30,   27,    7,
 /*   570 */   146,  359,  359,  359,  359,   35,  359,   28,  359,  359,
 /*   580 */   359,  359,  359,  359,  359,   25,   24,   26,  229,  359,
 /*   590 */    19,   18,   21,   20,   23,   22,   34,   30,   27,  359,
 /*   600 */   146,   35,  359,   28,  359,  359,  359,  359,  359,  359,
 /*   610 */   359,   25,   24,   26,  229,  359,   19,   18,   21,   20,
 /*   620 */    23,   22,   34,   30,   27,  359,  146,   35,  172,   28,
 /*   630 */   359,  359,  359,  359,  359,  359,  359,   25,   24,   26,
 /*   640 */   229,  359,   19,   18,   21,   20,   23,   22,   34,   30,
 /*   650 */    27,    6,  146,  359,  180,  359,  359,   35,  359,   28,
 /*   660 */   359,  359,  359,  359,  359,  359,  359,   25,   24,   26,
 /*   670 */   229,  359,   19,   18,   21,   20,   23,   22,   34,   30,
 /*   680 */    27,  359,  146,   35,  359,   28,  359,  148,  359,  359,
 /*   690 */   359,  359,  359,   25,   24,   26,  229,  359,   19,   18,
 /*   700 */    21,   20,   23,   22,   34,   30,   27,  359,  146,   35,
 /*   710 */   359,   28,  359,  359,  359,  359,  359,    1,  359,   25,
 /*   720 */    24,   26,  229,  359,   19,   18,   21,   20,   23,   22,
 /*   730 */    34,   30,   27,  359,  146,   35,  359,   28,  359,  359,
 /*   740 */   359,  359,  359,  359,  359,   25,   24,   26,  229,  359,
 /*   750 */    19,   18,   21,   20,   23,   22,   34,   30,   27,  359,
 /*   760 */   146,  233,    2,  359,  214,  359,  359,  359,  359,  176,
 /*   770 */   359,  213,  359,  221,  144,  212,  359,  359,   38,  359,
 /*   780 */   359,  359,  359,  232,  359,  359,  231,  215,  359,  104,
 /*   790 */    89,  217,  359,  170,    2,  196,  214,  195,  359,  186,
 /*   800 */   359,  176,  359,  213,  359,  221,  144,  212,  359,  359,
 /*   810 */   359,  359,  359,  359,  359,  232,  359,  359,  231,  215,
 /*   820 */   359,  104,   89,  217,  359,  359,  359,  196,   35,  195,
 /*   830 */    28,  186,  359,  359,  359,  359,  359,  359,   25,   24,
 /*   840 */    26,  229,  359,   19,   18,   21,   20,   23,   22,   34,
 /*   850 */    30,   27,  359,  146,   35,  359,   28,  204,  203,  207,
 /*   860 */   359,  359,  359,   11,  359,  359,   26,  229,  359,   19,
 /*   870 */    18,   21,   20,   23,   22,   34,   30,   27,  359,  146,
 /*   880 */   359,  359,  359,  204,  203,  207,   36,  201,  359,   11,
 /*   890 */   359,  359,  359,  221,  144,  359,  359,  359,  359,  359,
 /*   900 */   359,  359,  359,  359,  359,  359,  206,  205,  202,  104,
 /*   910 */    84,  217,   36,  359,  359,  196,  359,  195,  179,  186,
 /*   920 */   359,  221,  144,  359,  359,  359,  359,  359,  359,  359,
 /*   930 */   359,  359,  206,  205,  202,  221,  144,  104,   79,  217,
 /*   940 */   359,  359,  359,  196,  359,  195,  105,  186,  221,  144,
 /*   950 */   359,  104,  100,  217,  359,  359,  359,  196,  359,  195,
 /*   960 */   359,  186,  221,  144,  104,  101,  217,  359,  359,  359,
 /*   970 */   196,  359,  195,  359,  186,  359,  359,  359,  104,   82,
 /*   980 */   217,  359,  359,  359,  196,  359,  195,  359,  186,  221,
 /*   990 */   144,  359,  359,  359,  359,  359,  359,  359,  359,  359,
 /*  1000 */   221,  144,  359,  359,  359,  104,   53,  217,  359,  359,
 /*  1010 */   359,  196,  359,  195,  359,  186,  104,   88,  217,  359,
 /*  1020 */   359,  359,  196,  359,  195,  359,  186,  221,  144,  359,
 /*  1030 */   221,  144,  359,  359,  359,  359,  359,  359,  359,  359,
 /*  1040 */   359,  221,  144,  104,  134,  217,  104,   52,  217,  196,
 /*  1050 */   359,  195,  196,  186,  195,  359,  186,  104,  140,  217,
 /*  1060 */   359,  359,  359,  196,  359,  195,  359,  186,  221,  144,
 /*  1070 */   359,  359,  359,  359,  359,  359,  359,  359,  359,  221,
 /*  1080 */   144,  359,  359,  359,  104,  139,  217,  359,  359,  359,
 /*  1090 */   196,  359,  195,  359,  186,  104,   93,  217,  359,  359,
 /*  1100 */   359,  196,  359,  195,  359,  186,  221,  144,  359,  221,
 /*  1110 */   144,  359,  359,  359,  359,  359,  359,  359,  359,  359,
 /*  1120 */   221,  144,  104,   91,  217,  104,   90,  217,  196,  359,
 /*  1130 */   195,  196,  186,  195,  359,  186,  104,   99,  217,  359,
 /*  1140 */   359,  359,  196,  359,  195,  359,  186,  221,  144,  359,
 /*  1150 */   359,  359,  359,  359,  359,  359,  359,  359,  221,  144,
 /*  1160 */   359,  359,  359,  104,   98,  217,  359,  359,  359,  196,
 /*  1170 */   359,  195,  359,  186,  104,   97,  217,  359,  359,  359,
 /*  1180 */   196,  359,  195,  359,  186,  221,  144,  359,  221,  144,
 /*  1190 */   359,  359,  359,  359,  359,  359,  359,  359,  359,  221,
 /*  1200 */   144,  104,   96,  217,  104,   95,  217,  196,  359,  195,
 /*  1210 */   196,  186,  195,  359,  186,  104,   94,  217,  359,  359,
 /*  1220 */   359,  196,  359,  195,  359,  186,  221,  144,  359,  359,
 /*  1230 */   359,  359,  359,  359,  359,  359,  359,  221,  144,  359,
 /*  1240 */   359,  359,  104,   92,  217,  359,  359,  359,  196,  359,
 /*  1250 */   195,  359,  186,  104,   83,  217,  359,  359,  359,  196,
 /*  1260 */   359,  195,  359,  186,  221,  144,  359,  221,  144,  359,
 /*  1270 */   359,  359,  359,  359,  359,  359,  359,  359,  221,  144,
 /*  1280 */   104,   87,  217,  104,   86,  217,  196,  359,  195,  196,
 /*  1290 */   186,  195,  359,  186,  104,   50,  217,  359,  359,  359,
 /*  1300 */   196,  359,  195,  359,  186,  221,  144,  359,  359,  359,
 /*  1310 */   359,  359,  359,  359,  359,  359,  221,  144,  359,  359,
 /*  1320 */   359,  104,   80,  217,  359,  359,  359,  196,  359,  195,
 /*  1330 */   359,  186,  104,   85,  217,  359,  359,  359,  196,  359,
 /*  1340 */   195,  359,  186,  221,  144,  359,  359,  359,  359,  359,
 /*  1350 */   359,  359,  359,  359,  359,  359,  359,  359,  359,  104,
 /*  1360 */    49,  217,  359,  359,  359,  196,  359,  195,  359,  186,
];
const YY_LOOKAHEAD: [YYCODETYPE; 1370] = [
 /*     0 */     6,    7,    8,    0,    3,    9,   12,   11,   10,   15,
 /*    10 */    13,   13,   18,   83,   84,   85,   86,   23,   22,    7,
 /*    20 */    24,   25,   26,   27,   28,   29,   30,   31,   32,   35,
 /*    30 */    34,   37,   38,   39,   40,   41,   42,   36,   44,   45,
 /*    40 */    46,   47,   48,   12,   50,   51,   15,   53,   54,   55,
 /*    50 */    56,   11,   58,   59,   60,   61,    6,    7,    8,   33,
 /*    60 */     4,    5,   12,   13,   43,   15,   77,   78,    4,   43,
 /*    70 */     4,   31,   32,   23,   34,   44,   45,   46,   47,   48,
 /*    80 */     7,    3,   93,   94,   95,   35,    7,   14,   99,   17,
 /*    90 */   101,  102,  103,   14,   44,   45,   46,   47,   48,   83,
 /*   100 */    84,   85,   86,   53,   54,   55,   56,   34,   58,   59,
 /*   110 */    60,   61,    5,    6,    7,    8,   88,   89,    9,   12,
 /*   120 */    11,   34,   15,   51,   83,   84,    7,   86,   73,   74,
 /*   130 */    23,   88,   89,   24,   25,   26,   27,   28,   29,   30,
 /*   140 */    31,   32,   35,   34,   62,   17,  104,  105,    6,    7,
 /*   150 */     8,   44,   45,   46,   47,   48,   73,   74,  104,  105,
 /*   160 */    53,   54,   55,   56,    4,   58,   59,   60,   61,    5,
 /*   170 */     6,    7,    8,   67,  104,  105,   12,   35,    7,   15,
 /*   180 */    52,   93,   93,   36,   36,    2,   35,   23,   82,    3,
 /*   190 */     6,  103,  103,    4,    7,    7,   16,   55,   56,   35,
 /*   200 */     7,   59,   60,   61,    7,   43,   16,    4,   44,   45,
 /*   210 */    46,   47,   48,   36,    4,   17,    4,   53,   54,   55,
 /*   220 */    56,    3,   58,   59,   60,   61,    6,    7,    8,    7,
 /*   230 */    77,   78,   12,   13,   43,   15,    9,   36,   11,    4,
 /*   240 */     3,   36,    4,   23,    4,   67,   93,   94,   95,   17,
 /*   250 */    98,   52,   99,  100,  101,   35,  103,   30,   31,   32,
 /*   260 */    52,   34,    7,   82,   44,   45,   46,   47,   48,   69,
 /*   270 */    96,    2,   49,   53,   54,   55,   56,   82,   58,   59,
 /*   280 */    60,   61,    6,    7,    8,   72,   72,   76,   12,    7,
 /*   290 */    96,   15,   67,   69,    5,   76,   80,   52,   81,   23,
 /*   300 */    67,   52,   98,   67,   95,   34,   80,   39,   67,   67,
 /*   310 */    95,   35,   36,   82,   67,   67,   95,   67,  106,  106,
 /*   320 */    44,   45,   46,   47,   48,   95,  106,  106,  106,   53,
 /*   330 */    54,   55,   56,  106,   58,   59,   60,   61,    6,    7,
 /*   340 */     8,  106,   77,   78,   12,  106,  106,   15,  106,  106,
 /*   350 */   106,  106,  106,  106,  106,   23,  106,  106,   93,   94,
 /*   360 */    95,  106,  106,  106,   99,  100,  101,   35,  103,  106,
 /*   370 */   106,  106,  106,  106,  106,  106,   44,   45,   46,   47,
 /*   380 */    48,  106,  106,  106,   52,   53,   54,   55,   56,  106,
 /*   390 */    58,   59,   60,   61,    6,    7,    8,  106,   77,   78,
 /*   400 */    12,  106,   81,   15,  106,  106,  106,  106,  106,  106,
 /*   410 */   106,   23,  106,  106,   93,   94,   95,  106,  106,  106,
 /*   420 */    99,  106,  101,   35,  103,  106,  106,  106,  106,  106,
 /*   430 */   106,  106,   44,   45,   46,   47,   48,  106,  106,  106,
 /*   440 */   106,   53,   54,   55,   56,    3,   58,   59,   60,   61,
 /*   450 */   106,    9,  106,   11,  106,  106,  106,  106,  106,  106,
 /*   460 */   106,   19,   20,   21,   22,  106,   24,   25,   26,   27,
 /*   470 */    28,   29,   30,   31,   32,    3,   34,  106,   36,  106,
 /*   480 */   106,    9,  106,   11,  106,  106,  106,  106,  106,  106,
 /*   490 */   106,   19,   20,   21,   22,  106,   24,   25,   26,   27,
 /*   500 */    28,   29,   30,   31,   32,  106,   34,  106,   36,   64,
 /*   510 */    65,   66,  106,   68,  106,  106,  106,  106,   73,  106,
 /*   520 */    75,  106,   77,   78,   79,  106,  106,  106,  106,  106,
 /*   530 */   106,  106,   87,  106,  106,   90,   91,  106,   93,   94,
 /*   540 */    95,  106,  106,  106,   99,    9,  101,   11,  103,   13,
 /*   550 */   106,  106,  106,  106,  106,   19,   20,   21,   22,  106,
 /*   560 */    24,   25,   26,   27,   28,   29,   30,   31,   32,    3,
 /*   570 */    34,  106,  106,  106,  106,    9,  106,   11,  106,  106,
 /*   580 */   106,  106,  106,  106,  106,   19,   20,   21,   22,  106,
 /*   590 */    24,   25,   26,   27,   28,   29,   30,   31,   32,  106,
 /*   600 */    34,    9,  106,   11,  106,  106,  106,  106,  106,  106,
 /*   610 */   106,   19,   20,   21,   22,  106,   24,   25,   26,   27,
 /*   620 */    28,   29,   30,   31,   32,  106,   34,    9,   36,   11,
 /*   630 */   106,  106,  106,  106,  106,  106,  106,   19,   20,   21,
 /*   640 */    22,  106,   24,   25,   26,   27,   28,   29,   30,   31,
 /*   650 */    32,    3,   34,  106,   36,  106,  106,    9,  106,   11,
 /*   660 */   106,  106,  106,  106,  106,  106,  106,   19,   20,   21,
 /*   670 */    22,  106,   24,   25,   26,   27,   28,   29,   30,   31,
 /*   680 */    32,  106,   34,    9,  106,   11,  106,   13,  106,  106,
 /*   690 */   106,  106,  106,   19,   20,   21,   22,  106,   24,   25,
 /*   700 */    26,   27,   28,   29,   30,   31,   32,  106,   34,    9,
 /*   710 */   106,   11,  106,  106,  106,  106,  106,   17,  106,   19,
 /*   720 */    20,   21,   22,  106,   24,   25,   26,   27,   28,   29,
 /*   730 */    30,   31,   32,  106,   34,    9,  106,   11,  106,  106,
 /*   740 */   106,  106,  106,  106,  106,   19,   20,   21,   22,  106,
 /*   750 */    24,   25,   26,   27,   28,   29,   30,   31,   32,  106,
 /*   760 */    34,   65,   66,  106,   68,  106,  106,  106,  106,   73,
 /*   770 */   106,   75,  106,   77,   78,   79,  106,  106,   52,  106,
 /*   780 */   106,  106,  106,   87,  106,  106,   90,   91,  106,   93,
 /*   790 */    94,   95,  106,   65,   66,   99,   68,  101,  106,  103,
 /*   800 */   106,   73,  106,   75,  106,   77,   78,   79,  106,  106,
 /*   810 */   106,  106,  106,  106,  106,   87,  106,  106,   90,   91,
 /*   820 */   106,   93,   94,   95,  106,  106,  106,   99,    9,  101,
 /*   830 */    11,  103,  106,  106,  106,  106,  106,  106,   19,   20,
 /*   840 */    21,   22,  106,   24,   25,   26,   27,   28,   29,   30,
 /*   850 */    31,   32,  106,   34,    9,  106,   11,    6,    7,    8,
 /*   860 */   106,  106,  106,   12,  106,  106,   21,   22,  106,   24,
 /*   870 */    25,   26,   27,   28,   29,   30,   31,   32,  106,   34,
 /*   880 */   106,  106,  106,    6,    7,    8,   35,   36,  106,   12,
 /*   890 */   106,  106,  106,   77,   78,  106,  106,  106,  106,  106,
 /*   900 */   106,  106,  106,  106,  106,  106,   55,   56,   57,   93,
 /*   910 */    94,   95,   35,  106,  106,   99,  106,  101,  102,  103,
 /*   920 */   106,   77,   78,  106,  106,  106,  106,  106,  106,  106,
 /*   930 */   106,  106,   55,   56,   57,   77,   78,   93,   94,   95,
 /*   940 */   106,  106,  106,   99,  106,  101,  102,  103,   77,   78,
 /*   950 */   106,   93,   94,   95,  106,  106,  106,   99,  106,  101,
 /*   960 */   106,  103,   77,   78,   93,   94,   95,  106,  106,  106,
 /*   970 */    99,  106,  101,  106,  103,  106,  106,  106,   93,   94,
 /*   980 */    95,  106,  106,  106,   99,  106,  101,  106,  103,   77,
 /*   990 */    78,  106,  106,  106,  106,  106,  106,  106,  106,  106,
 /*  1000 */    77,   78,  106,  106,  106,   93,   94,   95,  106,  106,
 /*  1010 */   106,   99,  106,  101,  106,  103,   93,   94,   95,  106,
 /*  1020 */   106,  106,   99,  106,  101,  106,  103,   77,   78,  106,
 /*  1030 */    77,   78,  106,  106,  106,  106,  106,  106,  106,  106,
 /*  1040 */   106,   77,   78,   93,   94,   95,   93,   94,   95,   99,
 /*  1050 */   106,  101,   99,  103,  101,  106,  103,   93,   94,   95,
 /*  1060 */   106,  106,  106,   99,  106,  101,  106,  103,   77,   78,
 /*  1070 */   106,  106,  106,  106,  106,  106,  106,  106,  106,   77,
 /*  1080 */    78,  106,  106,  106,   93,   94,   95,  106,  106,  106,
 /*  1090 */    99,  106,  101,  106,  103,   93,   94,   95,  106,  106,
 /*  1100 */   106,   99,  106,  101,  106,  103,   77,   78,  106,   77,
 /*  1110 */    78,  106,  106,  106,  106,  106,  106,  106,  106,  106,
 /*  1120 */    77,   78,   93,   94,   95,   93,   94,   95,   99,  106,
 /*  1130 */   101,   99,  103,  101,  106,  103,   93,   94,   95,  106,
 /*  1140 */   106,  106,   99,  106,  101,  106,  103,   77,   78,  106,
 /*  1150 */   106,  106,  106,  106,  106,  106,  106,  106,   77,   78,
 /*  1160 */   106,  106,  106,   93,   94,   95,  106,  106,  106,   99,
 /*  1170 */   106,  101,  106,  103,   93,   94,   95,  106,  106,  106,
 /*  1180 */    99,  106,  101,  106,  103,   77,   78,  106,   77,   78,
 /*  1190 */   106,  106,  106,  106,  106,  106,  106,  106,  106,   77,
 /*  1200 */    78,   93,   94,   95,   93,   94,   95,   99,  106,  101,
 /*  1210 */    99,  103,  101,  106,  103,   93,   94,   95,  106,  106,
 /*  1220 */   106,   99,  106,  101,  106,  103,   77,   78,  106,  106,
 /*  1230 */   106,  106,  106,  106,  106,  106,  106,   77,   78,  106,
 /*  1240 */   106,  106,   93,   94,   95,  106,  106,  106,   99,  106,
 /*  1250 */   101,  106,  103,   93,   94,   95,  106,  106,  106,   99,
 /*  1260 */   106,  101,  106,  103,   77,   78,  106,   77,   78,  106,
 /*  1270 */   106,  106,  106,  106,  106,  106,  106,  106,   77,   78,
 /*  1280 */    93,   94,   95,   93,   94,   95,   99,  106,  101,   99,
 /*  1290 */   103,  101,  106,  103,   93,   94,   95,  106,  106,  106,
 /*  1300 */    99,  106,  101,  106,  103,   77,   78,  106,  106,  106,
 /*  1310 */   106,  106,  106,  106,  106,  106,   77,   78,  106,  106,
 /*  1320 */   106,   93,   94,   95,  106,  106,  106,   99,  106,  101,
 /*  1330 */   106,  103,   93,   94,   95,  106,  106,  106,   99,  106,
 /*  1340 */   101,  106,  103,   77,   78,  106,  106,  106,  106,  106,
 /*  1350 */   106,  106,  106,  106,  106,  106,  106,  106,  106,   93,
 /*  1360 */    94,   95,  106,  106,  106,   99,  106,  101,  106,  103,
];
const YY_SHIFT_USE_DFLT: i32 = -7;
const YY_SHIFT_COUNT: i32 = 147;
const YY_SHIFT_MIN: i32 = -6;
const YY_SHIFT_MAX: i32 = 877;
const YY_SHIFT_OFST: [i16; 148] = [
 /*     0 */    -6,   -6,   -6,  332,  276,  220,  388,  388,  388,  164,
 /*    10 */   107,   50,  388,  388,  388,  388,  388,  388,  388,  388,
 /*    20 */   388,  388,  388,  388,  388,  388,  388,  388,  388,  388,
 /*    30 */   388,  388,  388,  388,  388,  388,  851,  877,  877,  142,
 /*    40 */   142,   73,  128,   79,   79,  268,  268,  271,  271,  700,
 /*    50 */   700,  700,  726,  700,   31,   31,   31,   31,   72,   56,
 /*    60 */   232,  249,  232,  245,  289,  282,  232,  282,  255,  269,
 /*    70 */   223,  223,  269,  255,  208,  208,  232,  199,  472,  442,
 /*    80 */   674,  648,  618,  592,  566,  536,  819,  819,  819,  819,
 /*    90 */   845,  845,   -4,   -4,  109,  109,  109,  109,  227,  227,
 /*   100 */    40,   40,   -2,    1,   26,  205,  240,  238,  237,  235,
 /*   110 */   201,  191,  222,  218,  212,  210,  198,  203,  177,  162,
 /*   120 */   197,  190,  193,  180,  188,  189,  187,  186,  184,  151,
 /*   130 */    -3,  183,  171,  160,   87,  148,  147,  119,   82,   87,
 /*   140 */    87,   78,   66,   64,   21,   -3,   12,    3,
];
const YY_REDUCE_USE_DFLT: i32 = -71;
const YY_REDUCE_COUNT: i32 = 77;
const YY_REDUCE_MIN: i32 = -70;
const YY_REDUCE_MAX: i32 = 1266;
const YY_REDUCE_OFST: [i16; 78] = [
 /*     0 */   445,  728,  696,  321,  844,  265,  153,  816,  -11,  912,
 /*    10 */  1266, 1239, 1228, 1201, 1190, 1187, 1160, 1149, 1122, 1111,
 /*    20 */  1108, 1081, 1070, 1043, 1032, 1029, 1002,  991,  964,  953,
 /*    30 */   950,  923,  912,  885,  871,  858,   16,  -70,   41,   89,
 /*    40 */    88,   70,  106,   54,   42,   83,   55,   43,   28,  250,
 /*    50 */   248,  247,  231,  242,  230,  221,  215,  209,  241,  226,
 /*    60 */   236,  204,  233,  217,  216,  219,  225,  211,  224,  194,
 /*    70 */   214,  213,  174,  200,  195,  181,  178,  152,
];
const YY_DEFAULT: [YYACTIONTYPE; 234] = [
 /*     0 */   235,  235,  235,  357,  357,  357,  357,  357,  357,  357,
 /*    10 */   357,  357,  357,  357,  357,  357,  357,  357,  357,  357,
 /*    20 */   357,  357,  357,  357,  357,  357,  357,  357,  357,  357,
 /*    30 */   357,  357,  357,  357,  357,  357,  357,  357,  357,  357,
 /*    40 */   357,  352,  357,  352,  352,  271,  357,  248,  248,  357,
 /*    50 */   357,  357,  357,  357,  357,  357,  357,  357,  357,  357,
 /*    60 */   357,  357,  357,  357,  281,  274,  357,  274,  257,  260,
 /*    70 */   269,  269,  260,  257,  357,  297,  357,  357,  357,  357,
 /*    80 */   357,  346,  357,  357,  349,  357,  253,  252,  244,  239,
 /*    90 */   326,  325,  316,  324,  332,  331,  330,  329,  328,  327,
 /*   100 */   319,  320,  357,  357,  333,  357,  357,  357,  275,  357,
 /*   110 */   357,  357,  357,  258,  357,  357,  357,  357,  357,  357,
 /*   120 */   357,  357,  357,  357,  357,  357,  357,  357,  357,  357,
 /*   130 */   357,  357,  357,  357,  321,  357,  357,  357,  357,  323,
 /*   140 */   322,  309,  357,  357,  357,  290,  357,  357,  345,  344,
 /*   150 */   343,  347,  288,  294,  293,  284,  283,  279,  282,  280,
 /*   160 */   278,  277,  276,  273,  259,  261,  256,  272,  270,  255,
 /*   170 */   254,  251,  250,  249,  247,  246,  245,  308,  348,  350,
 /*   180 */   334,  356,  355,  354,  353,  351,  342,  341,  340,  339,
 /*   190 */   338,  337,  336,  335,  318,  315,  314,  312,  311,  310,
 /*   200 */   307,  306,  305,  304,  303,  302,  301,  300,  299,  298,
 /*   210 */   296,  295,  243,  242,  241,  240,  292,  290,  291,  287,
 /*   220 */   286,  285,  268,  267,  266,  265,  264,  263,  262,  317,
 /*   230 */   313,  238,  237,  236,
];

/* TMPL: fallback tokens */

const YY_FALLBACK: [i32; 0] = [
];

/* TMPL: symbol names */


/* TMPL: rules */


/* TMPL: destructors */


/* TMPL: stack-overflow */


/* TMPL: stack-overflow */

const YY_RULE_INFO: [YYCODETYPE; 123] = [
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
  87,
  88,
  88,
  89,
  91,
  73,
  90,
  90,
  67,
  68,
  68,
  69,
  69,
  69,
  96,
  96,
  95,
  95,
  95,
  95,
  95,
  95,
  95,
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
  94,
  77,
  77,
  77,
  78,
  78,
  94,
  94,
  98,
  98,
  94,
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
  94,
  93,
  93,
  93,
  93,
  93,
  93,
  93,
  93,
  93,
  99,
  99,
  99,
  100,
  100,
  101,
  102,
  102,
  103,
  104,
  104,
  104,
  105,
  105,
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
 (YYMinorType::YY56(yy0),) => {

	if list::is_empty(&yy0) {
		panic!("null program");
	}
	// ignore yyres, it doesn't really go anywhere for program
	yyres = Ast::Nothing;
	// we're done, so put yy0 in extra
	self.extra = Ok(Ast::ReplRoot(yy0));

},    _ => unreachable!() };
 YYMinorType::YY117(yyres)
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
 YYMinorType::YY56(yyres)
}
            ,
            2 /* stmts ::= stmt stmts */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY56(yy0),YYMinorType::YY56(yy1),) => {

    vout!("found new stmt: {:?}\n", yy0);
	yyres = list::cons(yy0, yy1);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
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
          | 80 /* expr ::= list */
          | 81 /* expr ::= tuple */
          | 99 /* expr ::= term */
          | 108 /* term ::= strexpr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY56(yy0),) => {
 yyres = yy0; 
},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
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
 (YYMinorType::YY56(yy0),) => {

    yyres = yy0;

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            10 /* stmt ::= RETURN expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY56(yy1),) => {

    yyres = sexpr::new(SexprType::Return, yy1);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
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
 (YYMinorType::YY155(yy1),YYMinorType::YY56(yy2),) => {

    yyres = sexpr::def_struct(Val::Type(yy1), yy2);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            13 /* defstruct_fields ::= defstruct_field defstruct_fields */
          | 120 /* strlist ::= strlist_term strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY56(yy0),YYMinorType::YY56(yy1),) => {

	yyres = list::cons(yy0, yy1);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
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
 YYMinorType::YY56(yyres)
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
 (YYMinorType::YY177(yy1),YYMinorType::YY155(yy3),) => {

	yyres = sexpr::id_with_type(yy1.data, yy3);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
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
 (YYMinorType::YY177(yy2),YYMinorType::YY56(yy4),) => {

vout!("found fail_stmt {:?}\n", yy2);
	yyres = sexpr::new(SexprType::Fail,
        list::cons(Val::hashtag(yy2.data),
        list::cons(yy4,
        Val::Nil,
        ))
    );

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
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
 (YYMinorType::YY177(yy1),YYMinorType::YY56(yy2),) => {

	yyres = sexpr::new(SexprType::MatchFailed,
        list::cons(Val::id(yy1.data),
        list::cons(yy2,
        Val::Nil
        ))
    );

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
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
 (YYMinorType::YY177(yy1),YYMinorType::YY56(yy3),) => {

	let letx =
        list::cons(Val::id(yy1.data),
        list::cons(yy3,
        Val::Nil
        ));
	yyres = sexpr::new(SexprType::Let, letx);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
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
 (YYMinorType::YY177(yy1),YYMinorType::YY56(yy3),) => {

	let bind = list::cons(Val::new_str(yy1.data), list::singleton(yy3));
	yyres = sexpr::new(SexprType::Fork, bind);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            20 /* block ::= BLOCKARROW stmts */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY56(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
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
 (YYMinorType::YY177(yy1),YYMinorType::YY56(yy3),YYMinorType::YY155(yy5),YYMinorType::YY56(yy6),YYMinorType::YY56(yy8),) => {

	let id = Val::id(yy1.data);
	let typ = Val::Type(yy5);
	yyres = sexpr::defunc(id, yy3, typ, yy6, yy8)

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
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
 (YYMinorType::YY177(yy1),YYMinorType::YY56(yy3),YYMinorType::YY155(yy5),YYMinorType::YY56(yy6),YYMinorType::YY56(yy8),) => {

	let id = Val::id(yy1.data);
	let typ = Val::Type(yy5);
    let body = sexpr::match_expr(Val::CallParams, yy6);
	yyres = sexpr::defunc(id, yy3, typ, body, yy8)

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            24 /* dfunc_args ::= ID opt_typex */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY177(yy0),YYMinorType::YY155(yy1),) => {

	yyres = list::singleton(sexpr::id_with_type(yy0.data, yy1));

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
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
 (YYMinorType::YY177(yy0),YYMinorType::YY155(yy1),YYMinorType::YY56(yy3),) => {

	yyres = list::cons(sexpr::id_with_type(yy0.data, yy1), yy3);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
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
 YYMinorType::YY155(yyres)
}
            ,
            27 /* opt_typex ::= COLON typex */
            => 
{
let yyres :  Type ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY155(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY155(yyres)
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
 YYMinorType::YY155(yyres)
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
 YYMinorType::YY155(yyres)
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
 YYMinorType::YY155(yyres)
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
 YYMinorType::YY155(yyres)
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
 YYMinorType::YY155(yyres)
}
            ,
            33 /* typex ::= TYPE_ID */
            => 
{
let yyres :  Type ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY177(yy0),) => {

	yyres = Type::Id(Arc::new(yy0.data));

},    _ => unreachable!() };
 YYMinorType::YY155(yyres)
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
 (YYMinorType::YY155(yy1),) => {

	yyres = Type::StrictList(Box::new(yy1));

},    _ => unreachable!() };
 YYMinorType::YY155(yyres)
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
 YYMinorType::YY56(yyres)
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
 (YYMinorType::YY56(yy2),) => {

    yyres = yy2;

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            37 /* failed_stmts ::= failed_stmt */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY56(yy0),) => {

    yyres = list::singleton(yy0);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            38 /* failed_stmts ::= failed_stmt failed_stmts */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY56(yy0),YYMinorType::YY56(yy1),) => {

    yyres = list::cons(yy0, yy1);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
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
 (YYMinorType::YY177(yy1),YYMinorType::YY56(yy3),YYMinorType::YY56(yy5),) => {

    vout!("found macro {:?}\n", yy1);
    yyres = sexpr::new(SexprType::DefMacro,
        list::cons(Val::id(yy1.data),
        list::cons(yy3,
        list::cons(yy5,
        Val::Nil
    ))));

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
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
 YYMinorType::YY56(yyres)
}
            ,
            41 /* macro_args ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY177(yy0),) => {

    yyres = list::singleton(Val::id(yy0.data));

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
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
 (YYMinorType::YY177(yy0),YYMinorType::YY56(yy2),) => {

    yyres = list::cons(Val::id(yy0.data), yy2);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
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
 (YYMinorType::YY56(yy1),YYMinorType::YY56(yy2),) => {

    /* if-only style */
    yyres = sexpr::ifstmt(yy1, yy2, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
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
 (YYMinorType::YY56(yy1),YYMinorType::YY56(yy2),YYMinorType::YY56(yy3),) => {

    /* if-else style */
    yyres = sexpr::ifstmt(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
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
 (YYMinorType::YY56(yy1),) => {

    /* case-expr style */
    yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
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
 (YYMinorType::YY56(yy2),YYMinorType::YY56(yy3),YYMinorType::YY56(yy4),) => {

    yyres = sexpr::ifstmt(yy2, yy3, yy4);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
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
 (YYMinorType::YY56(yy2),YYMinorType::YY56(yy3),) => {

    yyres = sexpr::ifstmt(yy2, yy3, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            48 /* else_if ::= ELSE block */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY56(yy1),) => {

    yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
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
 (YYMinorType::YY56(yy1),YYMinorType::YY56(yy2),YYMinorType::YY56(yy3),) => {

    yyres = sexpr::ifstmt(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
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
 (YYMinorType::YY56(yy2),) => {

    yyres = yy2;

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
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
 (YYMinorType::YY56(yy0),) => {

	vout!("zero param function call!");
	yyres = sexpr::call(yy0, vec![]);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
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
 (YYMinorType::YY56(yy0),YYMinorType::YY56(yy2),) => {

	vout!("one param function call!");
	yyres = sexpr::call(yy0, vec![yy2]);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
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
 (YYMinorType::YY56(yy0),YYMinorType::YY56(yy2),) => {

	vout!("multi param function call!");
	yyres = sexpr::call(yy0, list::to_vec(yy2));

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            56 /* func_term ::= typex */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY155(yy0),) => {

    yyres = Val::Type(yy0);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
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
 (YYMinorType::YY56(yy0),YYMinorType::YY56(yy2),) => {

	/* yyres = Val::binaryop(yy0, yy2, D); */
	yyres = Val::Void;

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
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
 (YYMinorType::YY56(yy1),) => {

    vout!("parsed case expr\n");
	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
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
 (YYMinorType::YY56(yy1),YYMinorType::YY56(yy2),YYMinorType::YY56(yy5),) => {

    vout!("found cases base\n");
    yyres = sexpr::casex(yy1, yy2, yy5);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
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
 (YYMinorType::YY56(yy1),YYMinorType::YY56(yy2),YYMinorType::YY56(yy3),) => {

    vout!("found extra case\n");
    yyres = sexpr::casex(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
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
 (YYMinorType::YY56(yy1),YYMinorType::YY56(yy2),) => {

    vout!("parsed match expr\n");
    yyres = sexpr::match_expr(yy1, yy2);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
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
 (YYMinorType::YY56(yy1),YYMinorType::YY56(yy2),YYMinorType::YY56(yy3),) => {

    vout!("found cases base\n");
    yyres = sexpr::match_case(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
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
 (YYMinorType::YY56(yy1),YYMinorType::YY56(yy2),) => {

    vout!("parsed base match case\n");
    yyres = sexpr::match_case(yy1, yy2, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            66 /* pexpr ::= INT */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY212(yy0),) => {
 yyres = Val::Int(yy0); 
},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            67 /* pexpr ::= True */
          | 105 /* term ::= True */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Bool(true); 
} };
 YYMinorType::YY56(yyres)
}
            ,
            68 /* pexpr ::= False */
          | 106 /* term ::= False */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Bool(false); 
} };
 YYMinorType::YY56(yyres)
}
            ,
            69 /* pexpr ::= HASHTAG */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY177(yy0),) => {
 yyres = Val::Hashtag(Arc::new(yy0.data)); 
},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            70 /* pexpr ::= ID */
          | 101 /* term ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY177(yy0),) => {
 yyres = Val::id(yy0.data); 
},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
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
 YYMinorType::YY56(yyres)
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
 YYMinorType::YY56(yyres)
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
 (YYMinorType::YY56(yy1),) => {

	yyres = Val::Tuple(vec![yy1]);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            74 /* ptuple ::= LPAREN pargs RPAREN */
          | 114 /* tuple ::= LPAREN tuple_args RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY56(yy1),) => {

	yyres = Val::tuple_from_list(yy1);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
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
 (YYMinorType::YY56(yy0),YYMinorType::YY56(yy2),) => {

	yyres = list::cons(yy0,
        list::cons(yy2,
        Val::Nil
        ));

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            76 /* pargs ::= pexpr COMMA pargs */
          | 113 /* list_items ::= expr COMMA list_items */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY56(yy0),YYMinorType::YY56(yy2),) => {

	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
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
 YYMinorType::YY56(yyres)
}
            ,
            78 /* plist ::= SquareL expr SquareR */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY56(yy1),) => {

    yyres = list::singleton(yy1);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            79 /* expr ::= expr DOT ID */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY56(yy0),YYMinorType::YY177(yy2),) => {

    yyres = sexpr::new(SexprType::FieldAccess,
        list::cons(yy0,
        list::cons(Val::id(yy2.data),
        Val::Nil,
        ))
    );

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            82 /* expr ::= NOT expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY56(yy1),) => {

	yyres = sexpr::call(Val::id("bool_not".to_string()), vec![yy1]);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            83 /* expr ::= expr ConcatNewline */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY56(yy0),) => {

	let newline = Val::Str(Arc::new("\n".to_string()));
	let args = list::cons(yy0, list::singleton(newline));
	yyres = sexpr::new(SexprType::StrExpr, args)

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            84 /* expr ::= NEGATE term */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY56(yy1),) => {

	yyres = sexpr::call(Val::id("negate".to_string()), vec![yy1]);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            85 /* expr ::= expr PLUS expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY56(yy0),YYMinorType::YY56(yy2),) => {

	yyres = sexpr::binaryop("int_add".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            86 /* expr ::= expr MINUS expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY56(yy0),YYMinorType::YY56(yy2),) => {

	yyres = sexpr::binaryop("int_sub".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            87 /* expr ::= expr TIMES expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY56(yy0),YYMinorType::YY56(yy2),) => {

	yyres = sexpr::binaryop("int_mult".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            88 /* expr ::= expr SLASH expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY56(yy0),YYMinorType::YY56(yy2),) => {

	yyres = sexpr::binaryop("int_div".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            89 /* expr ::= expr MOD expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY56(yy0),YYMinorType::YY56(yy2),) => {

	yyres = sexpr::binaryop("int_mod".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            90 /* expr ::= expr AND expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY56(yy0),YYMinorType::YY56(yy2),) => {

	yyres = sexpr::binaryop("boolean_and".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            91 /* expr ::= expr OR expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY56(yy0),YYMinorType::YY56(yy2),) => {

	yyres = sexpr::binaryop("boolean_or".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            92 /* expr ::= expr XOR expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY56(yy0),YYMinorType::YY56(yy2),) => {

	yyres = sexpr::binaryop("boolean_xor".to_string(),yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            93 /* expr ::= expr LT expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY56(yy0),YYMinorType::YY56(yy2),) => {

	yyres = sexpr::binaryop("less_than".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            94 /* expr ::= expr LTEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY56(yy0),YYMinorType::YY56(yy2),) => {

	yyres = sexpr::binaryop("less_than_equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            95 /* expr ::= expr GT expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY56(yy0),YYMinorType::YY56(yy2),) => {

	yyres = sexpr::binaryop("greater_than".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            96 /* expr ::= expr GTEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY56(yy0),YYMinorType::YY56(yy2),) => {

	yyres = sexpr::binaryop("greater_than_equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            97 /* expr ::= expr EQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY56(yy0),YYMinorType::YY56(yy2),) => {

	yyres = sexpr::binaryop("equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            98 /* expr ::= expr NEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY56(yy0),YYMinorType::YY56(yy2),) => {

	let eq = sexpr::binaryop("equal".to_string(), yy0, yy2);
	yyres = sexpr::call(Val::id("bool_not".to_string()), vec![eq]);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            100 /* term ::= LPAREN expr RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY56(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            102 /* term ::= VOID */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Val::Void;

} };
 YYMinorType::YY56(yyres)
}
            ,
            103 /* term ::= DollarQuestion */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Val::id("$".to_string());

} };
 YYMinorType::YY56(yyres)
}
            ,
            104 /* term ::= INT */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY212(yy0),) => {

	yyres = Val::Int(yy0);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            107 /* term ::= HASHTAG */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY177(yy0),) => {

	yyres = Val::Hashtag(Arc::new(yy0.data));

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            109 /* list ::= SquareL SquareR */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match () {
 () => {

	yyres = list::empty();

} };
 YYMinorType::YY56(yyres)
}
            ,
            110 /* list ::= SquareL list_items SquareR */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY56(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            111 /* list ::= SquareL list_items SEMICOLON expr SquareR */
            => 
{
let yyres :  Val ;
let yyp4 = self.yystack.pop().unwrap();
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY56(yy1),YYMinorType::YY56(yy3),) => {

	yyres = sexpr::call(Val::id("list_cons".to_string()), vec![yy1, yy3]);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            112 /* list_items ::= expr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY56(yy0),) => {

	yyres = list::singleton(yy0);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            115 /* tuple_args ::= expr COMMA expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY56(yy0),YYMinorType::YY56(yy2),) => {

	vout!("base tuple args!");
	yyres = list::cons(yy0, list::singleton(yy2));

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            116 /* tuple_args ::= expr COMMA tuple_args */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY56(yy0),YYMinorType::YY56(yy2),) => {

	vout!("additional tuple arg!");
	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            117 /* strexpr ::= StrOpen strlist StrClose */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY56(yy1),) => {

	yyres = sexpr::strexpr(yy1);
    vout!("strexpr({:?})\n", yyres);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            118 /* strlist ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

	yyres = Val::Nil;

} };
 YYMinorType::YY56(yyres)
}
            ,
            119 /* strlist ::= StrLit strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY192(yy0),YYMinorType::YY56(yy1),) => {

	yyres = list::cons(Val::new_str(yy0), yy1);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            121 /* strlist_term ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY177(yy0),) => {

    yyres = Val::id(yy0.data);

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
}
            ,
            122 /* strlist_term ::= strlist_term DOT ID */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY56(yy0),YYMinorType::YY177(yy2),) => {

    yyres = sexpr::new(SexprType::FieldAccess,
        list::cons(yy0,
        list::cons(Val::id(yy2.data),
        Val::Nil,
    )))

},    _ => unreachable!() };
 YYMinorType::YY56(yyres)
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

