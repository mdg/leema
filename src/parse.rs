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
const YYNOCODE: i32 = 103;
type YYACTIONTYPE = u16;
const YYWILDCARD: YYCODETYPE = 1;
enum YYMinorType {
    YY0,
    YY3(Type),
    YY20(i64),
    YY28(TokenLoc),
    YY40(String),
    YY93(TokenData<String>),
    YY97(Ast),
    YY188(Val),
}
const YYNSTATE: i32 = 219;
const YYNRULE: i32 = 116;
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
    DOUBLEDASH( TokenLoc ), //3
    ELSE( TokenLoc ), //4
    HASHTAG( TokenData<String> ), //5
    ID( TokenData<String> ), //6
    INT( i64 ), //7
    PLUS( TokenLoc ), //8
    SLASH( TokenLoc ), //9
    StrLit( String ), //10
    TYPE_ID( TokenData<String> ), //11
    ASSIGN, //12
    BLOCKARROW, //13
    RETURN, //14
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
    DOLLAR, //29
    DOT, //30
    LPAREN, //31
    RPAREN, //32
    STRUCT, //33
    COLON, //34
    FAIL, //35
    FAILED, //36
    Let, //37
    Fork, //38
    Func, //39
    PARENCALL, //40
    TYPE_INT, //41
    TYPE_STR, //42
    TYPE_BOOL, //43
    TYPE_VOID, //44
    PS, //45
    MACRO, //46
    IF, //47
    PIPE, //48
    CASE, //49
    MATCH, //50
    True, //51
    False, //52
    UNDERSCORE, //53
    NEGATE, //54
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
pub const TOKEN_DOUBLEDASH: i32 = 3;
pub const TOKEN_ELSE: i32 = 4;
pub const TOKEN_HASHTAG: i32 = 5;
pub const TOKEN_ID: i32 = 6;
pub const TOKEN_INT: i32 = 7;
pub const TOKEN_PLUS: i32 = 8;
pub const TOKEN_SLASH: i32 = 9;
pub const TOKEN_StrLit: i32 = 10;
pub const TOKEN_TYPE_ID: i32 = 11;
pub const TOKEN_ASSIGN: i32 = 12;
pub const TOKEN_BLOCKARROW: i32 = 13;
pub const TOKEN_RETURN: i32 = 14;
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
pub const TOKEN_DOLLAR: i32 = 29;
pub const TOKEN_DOT: i32 = 30;
pub const TOKEN_LPAREN: i32 = 31;
pub const TOKEN_RPAREN: i32 = 32;
pub const TOKEN_STRUCT: i32 = 33;
pub const TOKEN_COLON: i32 = 34;
pub const TOKEN_FAIL: i32 = 35;
pub const TOKEN_FAILED: i32 = 36;
pub const TOKEN_Let: i32 = 37;
pub const TOKEN_Fork: i32 = 38;
pub const TOKEN_Func: i32 = 39;
pub const TOKEN_PARENCALL: i32 = 40;
pub const TOKEN_TYPE_INT: i32 = 41;
pub const TOKEN_TYPE_STR: i32 = 42;
pub const TOKEN_TYPE_BOOL: i32 = 43;
pub const TOKEN_TYPE_VOID: i32 = 44;
pub const TOKEN_PS: i32 = 45;
pub const TOKEN_MACRO: i32 = 46;
pub const TOKEN_IF: i32 = 47;
pub const TOKEN_PIPE: i32 = 48;
pub const TOKEN_CASE: i32 = 49;
pub const TOKEN_MATCH: i32 = 50;
pub const TOKEN_True: i32 = 51;
pub const TOKEN_False: i32 = 52;
pub const TOKEN_UNDERSCORE: i32 = 53;
pub const TOKEN_NEGATE: i32 = 54;
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
        &Token::DOUBLEDASH(_) => TOKEN_DOUBLEDASH,
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
        &Token::COLON => TOKEN_COLON,
        &Token::FAIL => TOKEN_FAIL,
        &Token::FAILED => TOKEN_FAILED,
        &Token::Let => TOKEN_Let,
        &Token::Fork => TOKEN_Fork,
        &Token::Func => TOKEN_Func,
        &Token::PARENCALL => TOKEN_PARENCALL,
        &Token::TYPE_INT => TOKEN_TYPE_INT,
        &Token::TYPE_STR => TOKEN_TYPE_STR,
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
        Token::DOUBLEDASH(x) => YYMinorType::YY28(x),
        Token::ELSE(x) => YYMinorType::YY28(x),
        Token::HASHTAG(x) => YYMinorType::YY93(x),
        Token::ID(x) => YYMinorType::YY93(x),
        Token::INT(x) => YYMinorType::YY20(x),
        Token::PLUS(x) => YYMinorType::YY28(x),
        Token::SLASH(x) => YYMinorType::YY28(x),
        Token::StrLit(x) => YYMinorType::YY40(x),
        Token::TYPE_ID(x) => YYMinorType::YY93(x),
        _ => YYMinorType::YY0
  }
}
const YY_ACTTAB_COUNT: i32 = 1058;
const YY_ACTION: [YYACTIONTYPE; 1058] = [
 /*     0 */   176,  182,  179,  208,  170,  185,  209,  183,   41,   30,
 /*    10 */   135,  198,  189,   37,   16,  175,  170,   98,   78,  183,
 /*    20 */    41,  150,   55,  187,  129,  186,    8,  175,   54,    1,
 /*    30 */   123,  120,  118,  116,  114,  219,  213,  212,  211,  210,
 /*    40 */   130,  106,    3,  190,   74,   28,  178,  177,   37,   35,
 /*    50 */   181,  180,    6,  208,   42,   59,  176,  182,  179,   97,
 /*    60 */   198,  134,  209,   11,  208,  126,   45,   98,   75,  183,
 /*    70 */    16,  163,   45,  187,   34,  186,  128,  175,   98,   79,
 /*    80 */   183,  140,    8,  183,  187,    4,  186,  167,  175,   73,
 /*    90 */   198,  175,  213,  212,  211,  210,  215,   33,   27,  205,
 /*   100 */    74,   28,  178,  177,  209,   35,  181,  180,    6,  208,
 /*   110 */    42,   57,  176,  182,  179,   32,   29,   26,  209,  138,
 /*   120 */   200,   43,  109,   98,   78,  183,   16,   43,  156,  187,
 /*   130 */   174,  186,  188,  175,  213,  212,  211,  210,    8,  194,
 /*   140 */   193,  197,  131,   39,  194,  193,  197,  173,  213,  212,
 /*   150 */   211,  210,  172,   39,    1,  138,   74,   28,  178,  177,
 /*   160 */   208,   35,  181,  180,    6,   36,   42,  176,  182,  179,
 /*   170 */    36,  191,  169,  209,   98,   76,  183,   27,  171,   39,
 /*   180 */   187,   16,  186,   99,  175,  196,  195,  192,  111,   38,
 /*   190 */   196,  195,  192,    8,  207,   29,   26,  166,  138,  164,
 /*   200 */   168,   53,  108,  213,  212,  211,  210,  124,  122,  121,
 /*   210 */    14,   74,   28,  178,  177,   71,   35,  181,  180,    6,
 /*   220 */   117,   42,  176,  182,  179,  160,  115,  208,  209,  113,
 /*   230 */   100,   13,   70,   12,   69,   68,   16,   44,  157,   67,
 /*   240 */    65,   98,   49,  183,   63,  105,   64,  187,    8,  186,
 /*   250 */   152,  175,   62,  137,  149,  146,   72,  141,  213,  212,
 /*   260 */   211,  210,    1,   31,  199,   10,   74,   28,  178,  177,
 /*   270 */   208,   35,  181,  180,    6,   38,   42,  176,  182,  179,
 /*   280 */   112,   66,  208,  209,   98,   95,  183,   52,  158,  119,
 /*   290 */   187,   16,  186,  110,  175,   40,   98,   96,  183,  155,
 /*   300 */   153,  102,  187,    8,  186,  104,  175,  103,  107,   55,
 /*   310 */    10,  151,  145,  213,  212,  211,  210,  148,  208,  144,
 /*   320 */   142,   74,   28,  178,  177,    9,   35,  181,  180,    6,
 /*   330 */     7,   42,   98,   51,  183,  143,   33,   27,  187,  147,
 /*   340 */   186,  101,  175,   24,   23,   25,  214,   46,   18,   17,
 /*   350 */    20,   19,   22,   21,   32,   29,   26,    7,  138,  162,
 /*   360 */   206,   58,  154,   33,   27,  136,   56,   61,   60,  125,
 /*   370 */    24,   23,   25,  214,  120,   18,   17,   20,   19,   22,
 /*   380 */    21,   32,   29,   26,  337,  138,  337,  184,   33,   27,
 /*   390 */   337,  337,  337,  337,  337,   24,   23,   25,  214,  337,
 /*   400 */    18,   17,   20,   19,   22,   21,   32,   29,   26,    7,
 /*   410 */   138,  337,  184,  337,  337,   33,   27,  337,  337,  337,
 /*   420 */   337,  337,   24,   23,   25,  214,  337,   18,   17,   20,
 /*   430 */    19,   22,   21,   32,   29,   26,    5,  138,  337,  337,
 /*   440 */   337,  337,   33,   27,  337,  337,  337,  337,  337,   24,
 /*   450 */    23,   25,  214,  337,   18,   17,   20,   19,   22,   21,
 /*   460 */    32,   29,   26,  337,  138,   33,   27,  337,  337,  337,
 /*   470 */   337,  337,   24,   23,   25,  214,  337,   18,   17,   20,
 /*   480 */    19,   22,   21,   32,   29,   26,  337,  138,  337,  161,
 /*   490 */    33,   27,  337,  337,  337,    1,  337,   24,   23,   25,
 /*   500 */   214,  337,   18,   17,   20,   19,   22,   21,   32,   29,
 /*   510 */    26,  337,  138,   33,   27,  337,  337,  337,  337,  337,
 /*   520 */    24,   23,   25,  214,  337,   18,   17,   20,   19,   22,
 /*   530 */    21,   32,   29,   26,  337,  138,  336,  139,    2,  337,
 /*   540 */   203,  337,  337,  337,  337,  165,  337,  202,  337,  208,
 /*   550 */   201,  337,  337,   38,  337,  337,  337,  217,  337,  337,
 /*   560 */   216,  204,  337,   98,   84,  183,  337,  337,  337,  187,
 /*   570 */   337,  186,  337,  175,   33,   27,  337,  337,  337,  337,
 /*   580 */   337,   24,   23,   25,  214,  337,   18,   17,   20,   19,
 /*   590 */    22,   21,   32,   29,   26,  337,  138,  218,    2,  337,
 /*   600 */   203,  337,  337,  337,  337,  165,  337,  202,  337,  208,
 /*   610 */   201,  337,  337,  337,  337,  337,  337,  217,  337,  337,
 /*   620 */   216,  204,  337,   98,   84,  183,  337,  159,    2,  187,
 /*   630 */   203,  186,  337,  175,  337,  165,  337,  202,  337,  208,
 /*   640 */   201,  337,  337,  337,  337,  337,  337,  217,  337,  337,
 /*   650 */   216,  204,  337,   98,   84,  183,   33,   27,  337,  187,
 /*   660 */   337,  186,  337,  175,  337,   25,  214,  337,   18,   17,
 /*   670 */    20,   19,   22,   21,   32,   29,   26,  337,  138,  176,
 /*   680 */   182,  179,  337,  337,  337,  209,  208,  337,  337,  337,
 /*   690 */   337,  337,  337,  337,  337,  337,  337,  337,  337,  337,
 /*   700 */    98,   83,  183,  337,  337,   15,  187,  337,  186,  337,
 /*   710 */   175,  337,  337,  337,  337,  213,  212,  211,  210,  337,
 /*   720 */   337,  337,  337,  337,  337,  178,  177,   33,   27,  181,
 /*   730 */   180,  337,  337,   42,  337,  337,  337,  214,  337,   18,
 /*   740 */    17,   20,   19,   22,   21,   32,   29,   26,  337,  138,
 /*   750 */    33,   27,  337,  337,  337,  337,  337,  337,  337,  337,
 /*   760 */   337,  208,  335,  335,  335,  335,   22,   21,   32,   29,
 /*   770 */    26,  337,  138,  337,  337,   98,  127,  183,  208,  337,
 /*   780 */   337,  187,  337,  186,  337,  175,  337,  337,  337,  337,
 /*   790 */   337,  208,   98,   50,  183,  337,  337,  337,  187,  337,
 /*   800 */   186,  337,  175,  208,  337,   98,  133,  183,  337,  337,
 /*   810 */   337,  187,  337,  186,  337,  175,  208,   98,  132,  183,
 /*   820 */   337,  337,  337,  187,  337,  186,  337,  175,  337,  337,
 /*   830 */    98,   88,  183,  337,  337,  337,  187,  208,  186,  337,
 /*   840 */   175,  337,  337,  337,  337,  337,  337,  337,  208,  337,
 /*   850 */   337,   98,   86,  183,  337,  337,  337,  187,  337,  186,
 /*   860 */   337,  175,   98,   85,  183,  337,  337,  208,  187,  337,
 /*   870 */   186,  337,  175,  337,  337,  337,  337,  337,  208,  337,
 /*   880 */   337,   98,   94,  183,  337,  337,  337,  187,  337,  186,
 /*   890 */   337,  175,   98,   93,  183,  208,  337,  337,  187,  337,
 /*   900 */   186,  337,  175,  337,  337,  337,  208,  337,  337,   98,
 /*   910 */    92,  183,  337,  337,  337,  187,  337,  186,  337,  175,
 /*   920 */    98,   91,  183,  337,  208,  337,  187,  208,  186,  337,
 /*   930 */   175,  337,  337,  337,  337,  337,  337,  337,   98,   90,
 /*   940 */   183,   98,   89,  183,  187,  337,  186,  187,  175,  186,
 /*   950 */   337,  175,  337,  337,  208,  337,  337,  208,  337,  337,
 /*   960 */   337,  337,  337,  337,  337,  337,  337,  337,   98,   87,
 /*   970 */   183,   98,   80,  183,  187,  337,  186,  187,  175,  186,
 /*   980 */   337,  175,  208,  337,  337,  208,  337,  337,  337,  337,
 /*   990 */   337,  337,  337,  337,  337,  337,   98,   77,  183,   98,
 /*  1000 */    82,  183,  187,  208,  186,  187,  175,  186,  337,  175,
 /*  1010 */   337,  337,  337,  337,  208,  337,  337,   98,   81,  183,
 /*  1020 */   337,  337,  337,  187,  337,  186,  337,  175,   98,   48,
 /*  1030 */   183,  337,  337,  208,  187,  337,  186,  337,  175,  337,
 /*  1040 */   337,  337,  337,  337,  337,  337,  337,   98,   47,  183,
 /*  1050 */   337,  337,  337,  187,  337,  186,  337,  175,
];
const YY_LOOKAHEAD: [YYCODETYPE; 1058] = [
 /*     0 */     5,    6,    7,   75,    6,   89,   11,   91,   10,   14,
 /*    10 */    80,   81,   82,    2,   19,   99,    6,   89,   90,   91,
 /*    20 */    10,    3,    4,   95,   96,   97,   31,   99,   33,   13,
 /*    30 */    35,   36,   37,   38,   39,    0,   41,   42,   43,   44,
 /*    40 */    30,   46,   47,   32,   49,   50,   51,   52,    2,   54,
 /*    50 */    55,   56,   57,   75,   59,    4,    5,    6,    7,   80,
 /*    60 */    81,   82,   11,   47,   75,   84,   85,   89,   90,   91,
 /*    70 */    19,   84,   85,   95,   29,   97,   98,   99,   89,   90,
 /*    80 */    91,   89,   31,   91,   95,   40,   97,   98,   99,   80,
 /*    90 */    81,   99,   41,   42,   43,   44,    6,    8,    9,    3,
 /*   100 */    49,   50,   51,   52,   11,   54,   55,   56,   57,   75,
 /*   110 */    59,    4,    5,    6,    7,   26,   27,   28,   11,   30,
 /*   120 */     3,   71,   72,   89,   90,   91,   19,   71,   72,   95,
 /*   130 */    96,   97,   32,   99,   41,   42,   43,   44,   31,    5,
 /*   140 */     6,    7,  100,  101,    5,    6,    7,   60,   41,   42,
 /*   150 */    43,   44,  100,  101,   13,   30,   49,   50,   51,   52,
 /*   160 */    75,   54,   55,   56,   57,   31,   59,    5,    6,    7,
 /*   170 */    31,   32,    6,   11,   89,   90,   91,    9,  100,  101,
 /*   180 */    95,   19,   97,   98,   99,   51,   52,   53,   65,   48,
 /*   190 */    51,   52,   53,   31,   32,   27,   28,   32,   30,    3,
 /*   200 */    58,   34,   79,   41,   42,   43,   44,    6,   31,    5,
 /*   210 */     2,   49,   50,   51,   52,    6,   54,   55,   56,   57,
 /*   220 */     6,   59,    5,    6,    7,    3,    6,   75,   11,    6,
 /*   230 */    78,   12,   40,   12,   32,    3,   19,   13,    3,    3,
 /*   240 */     2,   89,   90,   91,   32,    6,   40,   95,   31,   97,
 /*   250 */     3,   99,    2,   94,    3,    3,   65,   32,   41,   42,
 /*   260 */    43,   44,   13,   48,   79,   48,   49,   50,   51,   52,
 /*   270 */    75,   54,   55,   56,   57,   48,   59,    5,    6,    7,
 /*   280 */    67,    6,   75,   11,   89,   90,   91,   34,   70,   79,
 /*   290 */    95,   19,   97,   45,   99,   92,   89,   90,   91,   70,
 /*   300 */    67,    6,   95,   31,   97,   74,   99,   65,   92,    4,
 /*   310 */    48,   74,   78,   41,   42,   43,   44,   77,   75,   65,
 /*   320 */    94,   49,   50,   51,   52,   48,   54,   55,   56,   57,
 /*   330 */     2,   59,   89,   90,   91,   65,    8,    9,   95,   65,
 /*   340 */    97,   77,   99,   15,   16,   17,   18,   91,   20,   21,
 /*   350 */    22,   23,   24,   25,   26,   27,   28,    2,   30,   91,
 /*   360 */    32,   65,   91,    8,    9,   79,   65,   65,   65,   30,
 /*   370 */    15,   16,   17,   18,   36,   20,   21,   22,   23,   24,
 /*   380 */    25,   26,   27,   28,  102,   30,  102,   32,    8,    9,
 /*   390 */   102,  102,  102,  102,  102,   15,   16,   17,   18,  102,
 /*   400 */    20,   21,   22,   23,   24,   25,   26,   27,   28,    2,
 /*   410 */    30,  102,   32,  102,  102,    8,    9,  102,  102,  102,
 /*   420 */   102,  102,   15,   16,   17,   18,  102,   20,   21,   22,
 /*   430 */    23,   24,   25,   26,   27,   28,    2,   30,  102,  102,
 /*   440 */   102,  102,    8,    9,  102,  102,  102,  102,  102,   15,
 /*   450 */    16,   17,   18,  102,   20,   21,   22,   23,   24,   25,
 /*   460 */    26,   27,   28,  102,   30,    8,    9,  102,  102,  102,
 /*   470 */   102,  102,   15,   16,   17,   18,  102,   20,   21,   22,
 /*   480 */    23,   24,   25,   26,   27,   28,  102,   30,  102,   32,
 /*   490 */     8,    9,  102,  102,  102,   13,  102,   15,   16,   17,
 /*   500 */    18,  102,   20,   21,   22,   23,   24,   25,   26,   27,
 /*   510 */    28,  102,   30,    8,    9,  102,  102,  102,  102,  102,
 /*   520 */    15,   16,   17,   18,  102,   20,   21,   22,   23,   24,
 /*   530 */    25,   26,   27,   28,  102,   30,   62,   63,   64,  102,
 /*   540 */    66,  102,  102,  102,  102,   71,  102,   73,  102,   75,
 /*   550 */    76,  102,  102,   48,  102,  102,  102,   83,  102,  102,
 /*   560 */    86,   87,  102,   89,   90,   91,  102,  102,  102,   95,
 /*   570 */   102,   97,  102,   99,    8,    9,  102,  102,  102,  102,
 /*   580 */   102,   15,   16,   17,   18,  102,   20,   21,   22,   23,
 /*   590 */    24,   25,   26,   27,   28,  102,   30,   63,   64,  102,
 /*   600 */    66,  102,  102,  102,  102,   71,  102,   73,  102,   75,
 /*   610 */    76,  102,  102,  102,  102,  102,  102,   83,  102,  102,
 /*   620 */    86,   87,  102,   89,   90,   91,  102,   63,   64,   95,
 /*   630 */    66,   97,  102,   99,  102,   71,  102,   73,  102,   75,
 /*   640 */    76,  102,  102,  102,  102,  102,  102,   83,  102,  102,
 /*   650 */    86,   87,  102,   89,   90,   91,    8,    9,  102,   95,
 /*   660 */   102,   97,  102,   99,  102,   17,   18,  102,   20,   21,
 /*   670 */    22,   23,   24,   25,   26,   27,   28,  102,   30,    5,
 /*   680 */     6,    7,  102,  102,  102,   11,   75,  102,  102,  102,
 /*   690 */   102,  102,  102,  102,  102,  102,  102,  102,  102,  102,
 /*   700 */    89,   90,   91,  102,  102,   31,   95,  102,   97,  102,
 /*   710 */    99,  102,  102,  102,  102,   41,   42,   43,   44,  102,
 /*   720 */   102,  102,  102,  102,  102,   51,   52,    8,    9,   55,
 /*   730 */    56,  102,  102,   59,  102,  102,  102,   18,  102,   20,
 /*   740 */    21,   22,   23,   24,   25,   26,   27,   28,  102,   30,
 /*   750 */     8,    9,  102,  102,  102,  102,  102,  102,  102,  102,
 /*   760 */   102,   75,   20,   21,   22,   23,   24,   25,   26,   27,
 /*   770 */    28,  102,   30,  102,  102,   89,   90,   91,   75,  102,
 /*   780 */   102,   95,  102,   97,  102,   99,  102,  102,  102,  102,
 /*   790 */   102,   75,   89,   90,   91,  102,  102,  102,   95,  102,
 /*   800 */    97,  102,   99,   75,  102,   89,   90,   91,  102,  102,
 /*   810 */   102,   95,  102,   97,  102,   99,   75,   89,   90,   91,
 /*   820 */   102,  102,  102,   95,  102,   97,  102,   99,  102,  102,
 /*   830 */    89,   90,   91,  102,  102,  102,   95,   75,   97,  102,
 /*   840 */    99,  102,  102,  102,  102,  102,  102,  102,   75,  102,
 /*   850 */   102,   89,   90,   91,  102,  102,  102,   95,  102,   97,
 /*   860 */   102,   99,   89,   90,   91,  102,  102,   75,   95,  102,
 /*   870 */    97,  102,   99,  102,  102,  102,  102,  102,   75,  102,
 /*   880 */   102,   89,   90,   91,  102,  102,  102,   95,  102,   97,
 /*   890 */   102,   99,   89,   90,   91,   75,  102,  102,   95,  102,
 /*   900 */    97,  102,   99,  102,  102,  102,   75,  102,  102,   89,
 /*   910 */    90,   91,  102,  102,  102,   95,  102,   97,  102,   99,
 /*   920 */    89,   90,   91,  102,   75,  102,   95,   75,   97,  102,
 /*   930 */    99,  102,  102,  102,  102,  102,  102,  102,   89,   90,
 /*   940 */    91,   89,   90,   91,   95,  102,   97,   95,   99,   97,
 /*   950 */   102,   99,  102,  102,   75,  102,  102,   75,  102,  102,
 /*   960 */   102,  102,  102,  102,  102,  102,  102,  102,   89,   90,
 /*   970 */    91,   89,   90,   91,   95,  102,   97,   95,   99,   97,
 /*   980 */   102,   99,   75,  102,  102,   75,  102,  102,  102,  102,
 /*   990 */   102,  102,  102,  102,  102,  102,   89,   90,   91,   89,
 /*  1000 */    90,   91,   95,   75,   97,   95,   99,   97,  102,   99,
 /*  1010 */   102,  102,  102,  102,   75,  102,  102,   89,   90,   91,
 /*  1020 */   102,  102,  102,   95,  102,   97,  102,   99,   89,   90,
 /*  1030 */    91,  102,  102,   75,   95,  102,   97,  102,   99,  102,
 /*  1040 */   102,  102,  102,  102,  102,  102,  102,   89,   90,   91,
 /*  1050 */   102,  102,  102,   95,  102,   97,  102,   99,
];
const YY_SHIFT_USE_DFLT: i32 = -6;
const YY_SHIFT_COUNT: i32 = 139;
const YY_SHIFT_MIN: i32 = -5;
const YY_SHIFT_MAX: i32 = 742;
const YY_SHIFT_OFST: [i16; 140] = [
 /*     0 */    -5,   -5,   -5,  217,  162,  272,  272,  272,  272,  107,
 /*    10 */    51,  272,  272,  272,  272,  272,  272,  272,  272,  272,
 /*    20 */   272,  272,  272,  272,  272,  272,  272,  272,  272,  272,
 /*    30 */   272,  272,  272,  272,  674,  674,  139,  134,  134,   10,
 /*    40 */   141,   -2,   -2,  338,  338,  339,  339,  482,  482,  482,
 /*    50 */   505,  482,   93,   93,   93,   16,   18,  249,  277,  249,
 /*    60 */   262,  305,  295,  249,  295,  275,  253,  248,  248,  253,
 /*    70 */   275,  227,  227,  249,  215,  355,  328,  457,  434,  407,
 /*    80 */   380,  566,  566,  566,  566,  648,  648,  719,  719,  742,
 /*    90 */   742,  742,  742,   89,   89,  168,  168,   11,   45,  225,
 /*   100 */   252,  251,  250,  247,  212,  206,  239,  238,  236,  235,
 /*   110 */   224,  232,  202,  192,  223,  221,  220,  219,  214,  222,
 /*   120 */   209,  208,  204,  177,  167,  201,  196,  125,  165,  142,
 /*   130 */   166,   87,  125,  125,  100,   46,  117,   96,   90,   35,
];
const YY_REDUCE_USE_DFLT: i32 = -85;
const YY_REDUCE_COUNT: i32 = 74;
const YY_REDUCE_MIN: i32 = -84;
const YY_REDUCE_MAX: i32 = 958;
const YY_REDUCE_OFST: [i16; 75] = [
 /*     0 */   474,  564,  534,  152,   85,   34,  -72,  -11,  -22,  243,
 /*    10 */   958,  939,  928,  910,  907,  882,  879,  852,  849,  831,
 /*    20 */   820,  803,  792,  773,  762,  741,  728,  716,  703,  686,
 /*    30 */   611,  243,  207,  195,   -8,  -84,  -21,  -70,    9,   78,
 /*    40 */   123,   52,   42,   56,   50,  -13,  -19,  303,  302,  301,
 /*    50 */   286,  296,  271,  268,  256,  274,  264,  270,  226,  254,
 /*    60 */   234,  240,  237,  242,  231,  233,  216,  229,  218,  203,
 /*    70 */   213,  210,  185,  191,  159,
];
const YY_DEFAULT: [YYACTIONTYPE; 219] = [
 /*     0 */   220,  220,  220,  335,  335,  323,  323,  335,  335,  335,
 /*    10 */   335,  335,  335,  335,  335,  335,  335,  335,  335,  335,
 /*    20 */   335,  335,  335,  335,  335,  335,  335,  335,  335,  335,
 /*    30 */   335,  335,  335,  335,  335,  335,  335,  335,  335,  330,
 /*    40 */   335,  330,  330,  254,  335,  233,  233,  335,  335,  335,
 /*    50 */   335,  335,  335,  335,  335,  335,  335,  335,  335,  335,
 /*    60 */   335,  264,  257,  335,  257,  242,  245,  252,  252,  245,
 /*    70 */   242,  335,  278,  335,  335,  335,  335,  335,  324,  327,
 /*    80 */   335,  238,  237,  229,  224,  304,  303,  294,  302,  310,
 /*    90 */   309,  308,  307,  306,  305,  297,  298,  335,  311,  335,
 /*   100 */   335,  335,  258,  335,  335,  335,  335,  243,  335,  335,
 /*   110 */   335,  335,  335,  335,  335,  335,  335,  335,  335,  335,
 /*   120 */   335,  335,  335,  335,  335,  335,  335,  299,  335,  335,
 /*   130 */   335,  335,  301,  300,  335,  289,  335,  335,  335,  335,
 /*   140 */   272,  271,  275,  274,  267,  266,  262,  265,  263,  261,
 /*   150 */   260,  259,  256,  244,  246,  241,  255,  253,  240,  239,
 /*   160 */   236,  235,  234,  232,  231,  230,  326,  328,  322,  334,
 /*   170 */   333,  332,  331,  329,  325,  321,  320,  319,  318,  317,
 /*   180 */   316,  315,  314,  313,  312,  296,  293,  292,  288,  290,
 /*   190 */   287,  286,  285,  284,  283,  282,  281,  280,  279,  277,
 /*   200 */   276,  228,  227,  226,  225,  273,  270,  269,  268,  251,
 /*   210 */   250,  249,  248,  247,  295,  291,  223,  222,  221,
];

/* TMPL: fallback tokens */

const YY_FALLBACK: [i32; 0] = [
];

/* TMPL: symbol names */


/* TMPL: rules */


/* TMPL: destructors */


/* TMPL: stack-overflow */


/* TMPL: stack-overflow */

const YY_RULE_INFO: [YYCODETYPE; 116] = [
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
  64,
  64,
  83,
  84,
  84,
  85,
  87,
  71,
  86,
  86,
  65,
  66,
  66,
  67,
  67,
  67,
  92,
  92,
  91,
  91,
  91,
  91,
  91,
  70,
  70,
  72,
  72,
  73,
  74,
  74,
  74,
  76,
  76,
  76,
  77,
  77,
  77,
  78,
  78,
  90,
  75,
  75,
  75,
  90,
  90,
  94,
  94,
  90,
  79,
  79,
  80,
  80,
  80,
  80,
  80,
  80,
  80,
  81,
  81,
  81,
  82,
  82,
  90,
  90,
  90,
  90,
  90,
  90,
  90,
  90,
  90,
  90,
  90,
  90,
  90,
  90,
  90,
  90,
  90,
  90,
  90,
  90,
  90,
  89,
  89,
  89,
  89,
  89,
  89,
  89,
  89,
  89,
  89,
  95,
  96,
  96,
  96,
  97,
  98,
  98,
  99,
  100,
  100,
  100,
  101,
  101,
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
 (YYMinorType::YY188(yy0),) => {

	if list::is_empty(&yy0) {
		panic!("null program");
	}
	// ignore yyres, it doesn't really go anywhere for program
	yyres = Ast::Nothing;
	// we're done, so put yy0 in extra
	self.extra = Ok(Ast::ReplRoot(yy0));

},    _ => unreachable!() };
 YYMinorType::YY97(yyres)
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
 YYMinorType::YY188(yyres)
}
            ,
            2 /* stmts ::= stmt stmts */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY188(yy0),YYMinorType::YY188(yy1),) => {

    vout!("found new stmt: {:?}\n", yy0);
	yyres = list::cons(yy0, yy1);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            3 /* stmt ::= defstruct */
          | 4 /* stmt ::= let_stmt */
          | 6 /* stmt ::= fail_stmt */
          | 7 /* stmt ::= func_stmt */
          | 8 /* stmt ::= macro_stmt */
          | 9 /* stmt ::= if_stmt */
          | 11 /* stmt ::= failed_stmt */
          | 60 /* pexpr ::= ptuple */
          | 73 /* expr ::= list */
          | 74 /* expr ::= tuple */
          | 92 /* expr ::= term */
          | 102 /* term ::= strexpr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY188(yy0),) => {
 yyres = yy0; 
},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            5 /* stmt ::= expr */
          | 49 /* expr ::= call_expr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY188(yy0),) => {

    yyres = yy0;

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            10 /* stmt ::= RETURN expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY188(yy1),) => {

    yyres = sexpr::new(SexprType::Return, yy1);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
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
 (YYMinorType::YY3(yy1),YYMinorType::YY188(yy2),) => {

    yyres = sexpr::def_struct(Val::Type(yy1), yy2);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            13 /* defstruct_fields ::= defstruct_field defstruct_fields */
          | 113 /* strlist ::= strlist_term strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY188(yy0),YYMinorType::YY188(yy1),) => {

	yyres = list::cons(yy0, yy1);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            14 /* defstruct_fields ::= */
          | 23 /* dfunc_args ::= */
          | 104 /* list_items ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

	yyres = list::empty();

} };
 YYMinorType::YY188(yyres)
}
            ,
            15 /* defstruct_field ::= DOT ID COLON typex */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY93(yy1),YYMinorType::YY3(yy3),) => {

	yyres = sexpr::id_with_type(yy1.data, yy3);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
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
 (YYMinorType::YY93(yy2),YYMinorType::YY188(yy4),) => {

vout!("found fail_stmt {:?}\n", yy2);
	yyres = sexpr::new(SexprType::Fail,
        list::cons(Val::hashtag(yy2.data),
        list::cons(yy4,
        Val::Nil,
        ))
    );

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
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
 (YYMinorType::YY93(yy1),YYMinorType::YY188(yy2),) => {

	yyres = sexpr::new(SexprType::MatchFailed,
        list::cons(Val::id(yy1.data),
        list::cons(yy2,
        Val::Nil
        ))
    );

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
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
 (YYMinorType::YY93(yy1),YYMinorType::YY188(yy3),) => {

	let letx =
        list::cons(Val::id(yy1.data),
        list::cons(yy3,
        Val::Nil
        ));
	yyres = sexpr::new(SexprType::Let, letx);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
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
 (YYMinorType::YY93(yy1),YYMinorType::YY188(yy3),) => {

	let bind = list::cons(Val::new_str(yy1.data), list::singleton(yy3));
	yyres = sexpr::new(SexprType::Fork, bind);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            20 /* block ::= BLOCKARROW stmts */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY188(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
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
 (YYMinorType::YY93(yy1),YYMinorType::YY188(yy3),YYMinorType::YY3(yy5),YYMinorType::YY188(yy6),YYMinorType::YY188(yy8),) => {

	let id = Val::id(yy1.data);
	let typ = Val::Type(yy5);
	yyres = sexpr::defunc(id, yy3, typ, yy6)

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
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
 (YYMinorType::YY93(yy1),YYMinorType::YY188(yy3),YYMinorType::YY3(yy5),YYMinorType::YY188(yy6),YYMinorType::YY188(yy8),) => {

	let id = Val::id(yy1.data);
	let typ = Val::Type(yy5);
    let body = sexpr::match_expr(Val::CallParams, yy6);
	yyres = sexpr::defunc(id, yy3, typ, body)

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            24 /* dfunc_args ::= ID opt_typex */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY93(yy0),YYMinorType::YY3(yy1),) => {

	yyres = list::singleton(sexpr::id_with_type(yy0.data, yy1));

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
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
 (YYMinorType::YY93(yy0),YYMinorType::YY3(yy1),YYMinorType::YY188(yy3),) => {

	yyres = list::cons(sexpr::id_with_type(yy0.data, yy1), yy3);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
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
 YYMinorType::YY3(yyres)
}
            ,
            27 /* opt_typex ::= COLON typex */
            => 
{
let yyres :  Type ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY3(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY3(yyres)
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
 YYMinorType::YY3(yyres)
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
 YYMinorType::YY3(yyres)
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
 YYMinorType::YY3(yyres)
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
 YYMinorType::YY3(yyres)
}
            ,
            32 /* typex ::= TYPE_ID */
            => 
{
let yyres :  Type ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY93(yy0),) => {

	yyres = Type::Id(Arc::new(yy0.data));

},    _ => unreachable!() };
 YYMinorType::YY3(yyres)
}
            ,
            33 /* opt_ps ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

    yyres = Val::Void;

} };
 YYMinorType::YY188(yyres)
}
            ,
            34 /* opt_ps ::= PS BLOCKARROW failed_stmts DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp2.minor,) {
 (YYMinorType::YY188(yy2),) => {

    yyres = yy2;

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            35 /* failed_stmts ::= failed_stmt */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY188(yy0),) => {

    yyres = list::singleton(yy0);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            36 /* failed_stmts ::= failed_stmt failed_stmts */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY188(yy0),YYMinorType::YY188(yy1),) => {

    yyres = list::cons(yy0, yy1);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            37 /* macro_stmt ::= MACRO ID PARENCALL macro_args RPAREN block DOUBLEDASH */
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
 (YYMinorType::YY93(yy1),YYMinorType::YY188(yy3),YYMinorType::YY188(yy5),) => {

    vout!("found macro {:?}\n", yy1);
    yyres = sexpr::new(SexprType::DefMacro,
        list::cons(Val::id(yy1.data),
        list::cons(yy3,
        list::cons(yy5,
        Val::Nil
    ))));

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            38 /* macro_args ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

    yyres = Val::Nil;

} };
 YYMinorType::YY188(yyres)
}
            ,
            39 /* macro_args ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY93(yy0),) => {

    yyres = list::singleton(Val::id(yy0.data));

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            40 /* macro_args ::= ID COMMA macro_args */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY93(yy0),YYMinorType::YY188(yy2),) => {

    yyres = list::cons(Val::id(yy0.data), yy2);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            41 /* if_stmt ::= IF expr block DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY188(yy1),YYMinorType::YY188(yy2),) => {

    /* if-only style */
    yyres = sexpr::ifstmt(yy1, yy2, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            42 /* if_stmt ::= IF expr block else_if DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp4 = self.yystack.pop().unwrap();
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,yyp3.minor,) {
 (YYMinorType::YY188(yy1),YYMinorType::YY188(yy2),YYMinorType::YY188(yy3),) => {

    /* if-else style */
    yyres = sexpr::ifstmt(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            43 /* if_stmt ::= IF if_case DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY188(yy1),) => {

    /* case-expr style */
    yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            44 /* else_if ::= ELSE IF expr block else_if */
            => 
{
let yyres :  Val ;
let yyp4 = self.yystack.pop().unwrap();
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp2.minor,yyp3.minor,yyp4.minor,) {
 (YYMinorType::YY188(yy2),YYMinorType::YY188(yy3),YYMinorType::YY188(yy4),) => {

    yyres = sexpr::ifstmt(yy2, yy3, yy4);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            45 /* else_if ::= ELSE IF expr block */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp2.minor,yyp3.minor,) {
 (YYMinorType::YY188(yy2),YYMinorType::YY188(yy3),) => {

    yyres = sexpr::ifstmt(yy2, yy3, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            46 /* else_if ::= ELSE block */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY188(yy1),) => {

    yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            47 /* if_case ::= PIPE expr block if_case */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,yyp3.minor,) {
 (YYMinorType::YY188(yy1),YYMinorType::YY188(yy2),YYMinorType::YY188(yy3),) => {

    yyres = sexpr::ifstmt(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            48 /* if_case ::= PIPE ELSE block */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp2.minor,) {
 (YYMinorType::YY188(yy2),) => {

    yyres = yy2;

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            50 /* call_expr ::= term PARENCALL RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY188(yy0),) => {

	vout!("zero param function call!");
	yyres = sexpr::call(yy0, vec![]);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            51 /* call_expr ::= term PARENCALL expr RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY188(yy0),YYMinorType::YY188(yy2),) => {

	vout!("one param function call!");
	yyres = sexpr::call(yy0, vec![yy2]);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            52 /* call_expr ::= term PARENCALL tuple_args RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY188(yy0),YYMinorType::YY188(yy2),) => {

	vout!("multi param function call!");
	yyres = sexpr::call(yy0, list::to_vec(yy2));

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            53 /* expr ::= term DOLLAR term */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY188(yy0),YYMinorType::YY188(yy2),) => {

	/* yyres = Val::binaryop(yy0, yy2, D); */
	yyres = Val::Void;

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            54 /* expr ::= CASE cases DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY188(yy1),) => {

    vout!("parsed case expr\n");
	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            55 /* cases ::= PIPE expr block PIPE ELSE block */
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
 (YYMinorType::YY188(yy1),YYMinorType::YY188(yy2),YYMinorType::YY188(yy5),) => {

    vout!("found cases base\n");
    yyres = sexpr::casex(yy1, yy2, yy5);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            56 /* cases ::= PIPE expr block cases */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,yyp3.minor,) {
 (YYMinorType::YY188(yy1),YYMinorType::YY188(yy2),YYMinorType::YY188(yy3),) => {

    vout!("found extra case\n");
    yyres = sexpr::casex(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            57 /* expr ::= MATCH expr match_case DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY188(yy1),YYMinorType::YY188(yy2),) => {

    vout!("parsed match expr\n");
    yyres = sexpr::match_expr(yy1, yy2);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            58 /* match_case ::= PIPE pexpr block match_case */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,yyp3.minor,) {
 (YYMinorType::YY188(yy1),YYMinorType::YY188(yy2),YYMinorType::YY188(yy3),) => {

    vout!("found cases base\n");
    yyres = sexpr::match_case(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            59 /* match_case ::= PIPE pexpr block */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY188(yy1),YYMinorType::YY188(yy2),) => {

    vout!("parsed base match case\n");
    yyres = sexpr::match_case(yy1, yy2, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            61 /* pexpr ::= INT */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY20(yy0),) => {
 yyres = Val::Int(yy0); 
},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            62 /* pexpr ::= True */
          | 99 /* term ::= True */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Bool(true); 
} };
 YYMinorType::YY188(yyres)
}
            ,
            63 /* pexpr ::= False */
          | 100 /* term ::= False */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Bool(false); 
} };
 YYMinorType::YY188(yyres)
}
            ,
            64 /* pexpr ::= HASHTAG */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY93(yy0),) => {
 yyres = Val::Hashtag(Arc::new(yy0.data)); 
},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            65 /* pexpr ::= ID */
          | 95 /* term ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY93(yy0),) => {
 yyres = Val::id(yy0.data); 
},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            66 /* pexpr ::= UNDERSCORE */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Wildcard; 
} };
 YYMinorType::YY188(yyres)
}
            ,
            67 /* ptuple ::= LPAREN RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match () {
 () => {

	panic!("an empty tuple is not a valid pattern");

} };
 YYMinorType::YY188(yyres)
}
            ,
            68 /* ptuple ::= LPAREN pexpr RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY188(yy1),) => {

	yyres = Val::Tuple(vec![yy1]);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            69 /* ptuple ::= LPAREN pargs RPAREN */
          | 107 /* tuple ::= LPAREN tuple_args RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY188(yy1),) => {

	yyres = Val::tuple_from_list(yy1);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            70 /* pargs ::= pexpr COMMA pexpr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY188(yy0),YYMinorType::YY188(yy2),) => {

	yyres = list::cons(yy0,
        list::cons(yy2,
        Val::Nil
        ));

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            71 /* pargs ::= pexpr COMMA pargs */
          | 106 /* list_items ::= expr COMMA list_items */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY188(yy0),YYMinorType::YY188(yy2),) => {

	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            72 /* expr ::= expr DOT ID */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY188(yy0),YYMinorType::YY93(yy2),) => {

    yyres = sexpr::new(SexprType::FieldAccess,
        list::cons(yy0,
        list::cons(Val::id(yy2.data),
        Val::Nil,
        ))
    );

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            75 /* expr ::= NOT expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY188(yy1),) => {

	yyres = sexpr::call(Val::id("bool_not".to_string()), vec![yy1]);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            76 /* expr ::= expr ConcatNewline */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY188(yy0),) => {

	let newline = Val::Str(Arc::new("\n".to_string()));
	let args = list::cons(yy0, list::singleton(newline));
	yyres = sexpr::new(SexprType::StrExpr, args)

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            77 /* expr ::= NEGATE term */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY188(yy1),) => {

	yyres = sexpr::call(Val::id("negate".to_string()), vec![yy1]);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            78 /* expr ::= expr PLUS expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY188(yy0),YYMinorType::YY188(yy2),) => {

	yyres = sexpr::binaryop("int_add".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            79 /* expr ::= expr MINUS expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY188(yy0),YYMinorType::YY188(yy2),) => {

	yyres = sexpr::binaryop("int_sub".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            80 /* expr ::= expr TIMES expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY188(yy0),YYMinorType::YY188(yy2),) => {

	yyres = sexpr::binaryop("int_mult".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            81 /* expr ::= expr SLASH expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY188(yy0),YYMinorType::YY188(yy2),) => {

	yyres = sexpr::binaryop("int_div".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            82 /* expr ::= expr MOD expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY188(yy0),YYMinorType::YY188(yy2),) => {

	yyres = sexpr::binaryop("int_mod".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            83 /* expr ::= expr AND expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY188(yy0),YYMinorType::YY188(yy2),) => {

	yyres = sexpr::binaryop("boolean_and".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            84 /* expr ::= expr OR expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY188(yy0),YYMinorType::YY188(yy2),) => {

	yyres = sexpr::binaryop("boolean_or".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            85 /* expr ::= expr XOR expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY188(yy0),YYMinorType::YY188(yy2),) => {

	yyres = sexpr::binaryop("boolean_xor".to_string(),yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            86 /* expr ::= expr LT expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY188(yy0),YYMinorType::YY188(yy2),) => {

	yyres = sexpr::binaryop("less_than".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            87 /* expr ::= expr LTEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY188(yy0),YYMinorType::YY188(yy2),) => {

	yyres = sexpr::binaryop("less_than_equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            88 /* expr ::= expr GT expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY188(yy0),YYMinorType::YY188(yy2),) => {

	yyres = sexpr::binaryop("greater_than".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            89 /* expr ::= expr GTEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY188(yy0),YYMinorType::YY188(yy2),) => {

	yyres = sexpr::binaryop("greater_than_equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            90 /* expr ::= expr EQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY188(yy0),YYMinorType::YY188(yy2),) => {

	yyres = sexpr::binaryop("equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            91 /* expr ::= expr NEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY188(yy0),YYMinorType::YY188(yy2),) => {

	let eq = sexpr::binaryop("equal".to_string(), yy0, yy2);
	yyres = sexpr::call(Val::id("bool_not".to_string()), vec![eq]);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            93 /* term ::= LPAREN expr RPAREN */
          | 103 /* list ::= SquareL list_items SquareR */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY188(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            94 /* term ::= typex */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY3(yy0),) => {

    yyres = Val::Type(yy0);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            96 /* term ::= VOID */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Val::Void;

} };
 YYMinorType::YY188(yyres)
}
            ,
            97 /* term ::= DollarQuestion */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Val::id("$".to_string());

} };
 YYMinorType::YY188(yyres)
}
            ,
            98 /* term ::= INT */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY20(yy0),) => {

	yyres = Val::Int(yy0);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            101 /* term ::= HASHTAG */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY93(yy0),) => {

	yyres = Val::Hashtag(Arc::new(yy0.data));

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            105 /* list_items ::= expr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY188(yy0),) => {

	yyres = list::singleton(yy0);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            108 /* tuple_args ::= expr COMMA expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY188(yy0),YYMinorType::YY188(yy2),) => {

	vout!("base tuple args!");
	yyres = list::cons(yy0, list::singleton(yy2));

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            109 /* tuple_args ::= expr COMMA tuple_args */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY188(yy0),YYMinorType::YY188(yy2),) => {

	vout!("additional tuple arg!");
	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            110 /* strexpr ::= StrOpen strlist StrClose */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY188(yy1),) => {

	yyres = sexpr::strexpr(yy1);
    vout!("strexpr({:?})\n", yyres);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            111 /* strlist ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

	yyres = Val::Nil;

} };
 YYMinorType::YY188(yyres)
}
            ,
            112 /* strlist ::= StrLit strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY40(yy0),YYMinorType::YY188(yy1),) => {

	yyres = list::cons(Val::new_str(yy0), yy1);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            114 /* strlist_term ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY93(yy0),) => {

    yyres = Val::id(yy0.data);

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
}
            ,
            115 /* strlist_term ::= strlist_term DOT ID */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY188(yy0),YYMinorType::YY93(yy2),) => {

    yyres = sexpr::new(SexprType::FieldAccess,
        list::cons(yy0,
        list::cons(Val::id(yy2.data),
        Val::Nil,
    )))

},    _ => unreachable!() };
 YYMinorType::YY188(yyres)
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

