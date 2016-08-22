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
const YYNOCODE: i32 = 100;
type YYACTIONTYPE = u16;
const YYWILDCARD: YYCODETYPE = 1;
enum YYMinorType {
    YY0,
    YY9(Type),
    YY68(i64),
    YY103(Ast),
    YY111(TokenData<String>),
    YY116(Val),
    YY154(TokenLoc),
    YY166(String),
}
const YYNSTATE: i32 = 210;
const YYNRULE: i32 = 111;
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
    FAILED, //33
    STRUCT, //34
    COLON, //35
    FAIL, //36
    Let, //37
    Fork, //38
    Func, //39
    PARENCALL, //40
    TYPE_INT, //41
    TYPE_STR, //42
    TYPE_BOOL, //43
    TYPE_VOID, //44
    MACRO, //45
    IF, //46
    PIPE, //47
    CASE, //48
    MATCH, //49
    True, //50
    False, //51
    UNDERSCORE, //52
    NEGATE, //53
    VOID, //54
    DollarQuestion, //55
    SquareL, //56
    SquareR, //57
    StrOpen, //58
    StrClose, //59
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
pub const TOKEN_FAILED: i32 = 33;
pub const TOKEN_STRUCT: i32 = 34;
pub const TOKEN_COLON: i32 = 35;
pub const TOKEN_FAIL: i32 = 36;
pub const TOKEN_Let: i32 = 37;
pub const TOKEN_Fork: i32 = 38;
pub const TOKEN_Func: i32 = 39;
pub const TOKEN_PARENCALL: i32 = 40;
pub const TOKEN_TYPE_INT: i32 = 41;
pub const TOKEN_TYPE_STR: i32 = 42;
pub const TOKEN_TYPE_BOOL: i32 = 43;
pub const TOKEN_TYPE_VOID: i32 = 44;
pub const TOKEN_MACRO: i32 = 45;
pub const TOKEN_IF: i32 = 46;
pub const TOKEN_PIPE: i32 = 47;
pub const TOKEN_CASE: i32 = 48;
pub const TOKEN_MATCH: i32 = 49;
pub const TOKEN_True: i32 = 50;
pub const TOKEN_False: i32 = 51;
pub const TOKEN_UNDERSCORE: i32 = 52;
pub const TOKEN_NEGATE: i32 = 53;
pub const TOKEN_VOID: i32 = 54;
pub const TOKEN_DollarQuestion: i32 = 55;
pub const TOKEN_SquareL: i32 = 56;
pub const TOKEN_SquareR: i32 = 57;
pub const TOKEN_StrOpen: i32 = 58;
pub const TOKEN_StrClose: i32 = 59;
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
        &Token::FAILED => TOKEN_FAILED,
        &Token::STRUCT => TOKEN_STRUCT,
        &Token::COLON => TOKEN_COLON,
        &Token::FAIL => TOKEN_FAIL,
        &Token::Let => TOKEN_Let,
        &Token::Fork => TOKEN_Fork,
        &Token::Func => TOKEN_Func,
        &Token::PARENCALL => TOKEN_PARENCALL,
        &Token::TYPE_INT => TOKEN_TYPE_INT,
        &Token::TYPE_STR => TOKEN_TYPE_STR,
        &Token::TYPE_BOOL => TOKEN_TYPE_BOOL,
        &Token::TYPE_VOID => TOKEN_TYPE_VOID,
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
        Token::COMMA(x) => YYMinorType::YY154(x),
        Token::DOUBLEDASH(x) => YYMinorType::YY154(x),
        Token::ELSE(x) => YYMinorType::YY154(x),
        Token::HASHTAG(x) => YYMinorType::YY111(x),
        Token::ID(x) => YYMinorType::YY111(x),
        Token::INT(x) => YYMinorType::YY68(x),
        Token::PLUS(x) => YYMinorType::YY154(x),
        Token::SLASH(x) => YYMinorType::YY154(x),
        Token::StrLit(x) => YYMinorType::YY166(x),
        Token::TYPE_ID(x) => YYMinorType::YY111(x),
        _ => YYMinorType::YY0
  }
}
const YY_ACTTAB_COUNT: i32 = 1019;
const YY_ACTION: [YYACTIONTYPE; 1019] = [
 /*     0 */   167,  173,  170,   27,  144,   53,  200,   34,  105,   30,
 /*    10 */   129,  189,  180,  210,   16,   93,  189,  128,    4,    1,
 /*    20 */   104,   29,   26,  206,  132,  176,    8,  174,  120,   52,
 /*    30 */    37,  115,  112,  110,  108,  166,  204,  203,  202,  201,
 /*    40 */   102,    3,  196,   70,   28,  169,  168,  199,   35,  172,
 /*    50 */   171,    6,   11,   42,   57,  167,  173,  170,  191,  199,
 /*    60 */   181,  200,   94,   71,  174,  118,   43,   37,  178,   16,
 /*    70 */   177,  122,  166,    1,   94,   75,  174,  154,   43,  179,
 /*    80 */   178,    8,  177,  158,  166,   69,  189,  185,  184,  188,
 /*    90 */   164,  204,  203,  202,  201,  185,  184,  188,   70,   28,
 /*   100 */   169,  168,  132,   35,  172,  171,    6,   38,   42,   55,
 /*   110 */   167,  173,  170,   36,  182,  161,  200,  160,  134,   41,
 /*   120 */   174,   36,  125,   39,   16,  161,  163,   39,  166,   41,
 /*   130 */   162,   39,  187,  186,  183,  159,    8,   67,  157,  124,
 /*   140 */   187,  186,  183,  156,  155,  116,  204,  203,  202,  201,
 /*   150 */    51,  114,  199,   70,   28,  169,  168,  113,   35,  172,
 /*   160 */   171,    6,   14,   42,  167,  173,  170,   94,   74,  174,
 /*   170 */   200,  199,  111,  178,  123,  177,   13,  166,   16,  109,
 /*   180 */    12,  107,   66,   65,  150,  149,   94,   74,  174,   63,
 /*   190 */     8,  198,  178,  165,  177,  101,  166,   62,  146,   61,
 /*   200 */   204,  203,  202,  201,  200,   60,  199,   70,   28,  169,
 /*   210 */   168,  135,   35,  172,  171,    6,  131,   42,  167,  173,
 /*   220 */   170,   94,   72,  174,  200,  199,  143,  178,  140,  177,
 /*   230 */    95,  166,   16,   31,  204,  203,  202,  201,   68,    1,
 /*   240 */    94,   91,  174,   38,    8,  190,  178,  119,  177,  106,
 /*   250 */   166,   64,   40,   50,  204,  203,  202,  201,  147,  103,
 /*   260 */    10,   70,   28,  169,  168,   98,   35,  172,  171,    6,
 /*   270 */   100,   42,  167,  173,  170,   99,  145,   53,  200,  199,
 /*   280 */   139,  142,   10,   96,  138,  136,   16,    9,  137,   97,
 /*   290 */    44,  141,  153,   56,   94,   47,  174,  148,    8,  117,
 /*   300 */   178,   54,  177,   59,  166,   58,  130,  323,  204,  203,
 /*   310 */   202,  201,  323,  323,  323,   70,   28,  169,  168,  323,
 /*   320 */    35,  172,  171,    6,    7,   42,  323,  323,  323,  323,
 /*   330 */    33,   27,  323,  323,  323,  323,  323,   24,   23,   25,
 /*   340 */   205,  323,   18,   17,   20,   19,   22,   21,   32,   29,
 /*   350 */    26,    7,  132,  323,  197,  323,  323,   33,   27,  323,
 /*   360 */   323,  323,  323,  323,   24,   23,   25,  205,  323,   18,
 /*   370 */    17,   20,   19,   22,   21,   32,   29,   26,  323,  132,
 /*   380 */   323,  175,   33,   27,  323,  323,  323,  323,  323,   24,
 /*   390 */    23,   25,  205,  323,   18,   17,   20,   19,   22,   21,
 /*   400 */    32,   29,   26,    7,  132,  323,  175,  323,  323,   33,
 /*   410 */    27,  323,  323,  323,  323,  323,   24,   23,   25,  205,
 /*   420 */   323,   18,   17,   20,   19,   22,   21,   32,   29,   26,
 /*   430 */     5,  132,  323,  323,  323,  323,   33,   27,  323,  323,
 /*   440 */   323,  323,  323,   24,   23,   25,  205,  323,   18,   17,
 /*   450 */    20,   19,   22,   21,   32,   29,   26,  323,  132,   33,
 /*   460 */    27,  323,  323,  323,  323,  323,   24,   23,   25,  205,
 /*   470 */   323,   18,   17,   20,   19,   22,   21,   32,   29,   26,
 /*   480 */   323,  132,  323,  152,   33,   27,  323,  323,  323,    1,
 /*   490 */   323,   24,   23,   25,  205,  323,   18,   17,   20,   19,
 /*   500 */    22,   21,   32,   29,   26,  323,  132,   33,   27,  323,
 /*   510 */   323,  323,   33,   27,   24,   23,   25,  205,  323,   18,
 /*   520 */    17,   20,   19,   22,   21,   32,   29,   26,  323,  132,
 /*   530 */    32,   29,   26,  323,  132,   33,   27,  323,  323,  323,
 /*   540 */   323,  323,   24,   23,   25,  205,   38,   18,   17,   20,
 /*   550 */    19,   22,   21,   32,   29,   26,  323,  132,  322,  133,
 /*   560 */     2,  323,  194,  323,  323,  323,  193,  323,  199,  323,
 /*   570 */   192,  323,  323,  323,  323,  323,  323,  208,  323,  323,
 /*   580 */   207,  195,  323,   94,   80,  174,  323,  209,    2,  178,
 /*   590 */   194,  177,  323,  166,  193,  323,  199,  323,  192,  323,
 /*   600 */   323,  323,  323,  323,  323,  208,  323,  323,  207,  195,
 /*   610 */   323,   94,   80,  174,  323,  151,    2,  178,  194,  177,
 /*   620 */   323,  166,  193,  323,  199,  323,  192,  323,  323,  323,
 /*   630 */   323,  323,  323,  208,  323,  323,  207,  195,  323,   94,
 /*   640 */    80,  174,   33,   27,  323,  178,  323,  177,  323,  166,
 /*   650 */   323,   25,  205,  199,   18,   17,   20,   19,   22,   21,
 /*   660 */    32,   29,   26,  323,  132,  167,  173,  170,   94,   92,
 /*   670 */   174,  200,  323,  323,  178,  323,  177,  323,  166,  323,
 /*   680 */   323,  323,  323,  323,  323,  323,  323,  323,  323,  323,
 /*   690 */   323,   15,  323,  323,  323,  323,  323,  323,  323,  323,
 /*   700 */   323,  204,  203,  202,  201,  323,  323,  323,  323,  323,
 /*   710 */   169,  168,   33,   27,  172,  171,  323,  323,   42,  323,
 /*   720 */   323,  323,  205,  323,   18,   17,   20,   19,   22,   21,
 /*   730 */    32,   29,   26,  323,  132,   33,   27,  323,  323,  323,
 /*   740 */   323,  323,  323,  199,  323,  323,  199,  321,  321,  321,
 /*   750 */   321,   22,   21,   32,   29,   26,  323,  132,   94,   49,
 /*   760 */   174,   94,   79,  174,  178,  323,  177,  178,  166,  177,
 /*   770 */   323,  166,  323,  199,  323,  323,  199,  323,  323,  323,
 /*   780 */   323,  323,  323,  323,  323,  323,  323,  199,   94,  121,
 /*   790 */   174,   94,   48,  174,  178,  323,  177,  178,  166,  177,
 /*   800 */   199,  166,   94,  127,  174,  323,  323,  323,  178,  323,
 /*   810 */   177,  323,  166,  323,  323,   94,  126,  174,  199,  323,
 /*   820 */   323,  178,  323,  177,  323,  166,  323,  323,  323,  199,
 /*   830 */   323,  323,  199,   94,   84,  174,  323,  323,  323,  178,
 /*   840 */   323,  177,  323,  166,   94,   82,  174,   94,   81,  174,
 /*   850 */   178,  323,  177,  178,  166,  177,  323,  166,  323,  199,
 /*   860 */   323,  323,  199,  323,  323,  323,  323,  323,  323,  323,
 /*   870 */   323,  323,  323,  199,   94,   90,  174,   94,   89,  174,
 /*   880 */   178,  323,  177,  178,  166,  177,  323,  166,   94,   88,
 /*   890 */   174,  323,  323,  199,  178,  323,  177,  323,  166,  323,
 /*   900 */   323,  323,  323,  323,  199,  323,  323,  199,   94,   87,
 /*   910 */   174,  323,  323,  323,  178,  323,  177,  323,  166,   94,
 /*   920 */    86,  174,   94,   85,  174,  178,  199,  177,  178,  166,
 /*   930 */   177,  323,  166,  323,  323,  323,  323,  323,  323,  323,
 /*   940 */   323,   94,   83,  174,  323,  199,  323,  178,  199,  177,
 /*   950 */   323,  166,  323,  323,  323,  323,  323,  323,  323,  199,
 /*   960 */    94,   76,  174,   94,   73,  174,  178,  323,  177,  178,
 /*   970 */   166,  177,  323,  166,   94,   78,  174,  323,  323,  199,
 /*   980 */   178,  323,  177,  323,  166,  323,  323,  323,  323,  323,
 /*   990 */   199,  323,  323,  199,   94,   77,  174,  323,  323,  323,
 /*  1000 */   178,  323,  177,  323,  166,   94,   46,  174,   94,   45,
 /*  1010 */   174,  178,  323,  177,  178,  166,  177,  323,  166,
];
const YY_LOOKAHEAD: [YYCODETYPE; 1019] = [
 /*     0 */     5,    6,    7,    9,    3,    4,   11,   29,   64,   14,
 /*    10 */    77,   78,   79,    0,   19,   77,   78,   79,   40,   13,
 /*    20 */    76,   27,   28,    6,   30,   86,   31,   88,   33,   34,
 /*    30 */     2,   36,   37,   38,   39,   96,   41,   42,   43,   44,
 /*    40 */    45,   46,    3,   48,   49,   50,   51,   71,   53,   54,
 /*    50 */    55,   56,   46,   58,    4,    5,    6,    7,    3,   71,
 /*    60 */    32,   11,   86,   87,   88,   81,   82,    2,   92,   19,
 /*    70 */    94,   95,   96,   13,   86,   87,   88,   81,   82,   32,
 /*    80 */    92,   31,   94,   95,   96,   77,   78,    5,    6,    7,
 /*    90 */    59,   41,   42,   43,   44,    5,    6,    7,   48,   49,
 /*   100 */    50,   51,   30,   53,   54,   55,   56,   47,   58,    4,
 /*   110 */     5,    6,    7,   31,   32,    6,   11,    6,   86,   10,
 /*   120 */    88,   31,   97,   98,   19,    6,   97,   98,   96,   10,
 /*   130 */    97,   98,   50,   51,   52,   57,   31,    6,   32,   30,
 /*   140 */    50,   51,   52,    3,    3,    6,   41,   42,   43,   44,
 /*   150 */    35,   31,   71,   48,   49,   50,   51,    5,   53,   54,
 /*   160 */    55,   56,    2,   58,    5,    6,    7,   86,   87,   88,
 /*   170 */    11,   71,    6,   92,   93,   94,   12,   96,   19,    6,
 /*   180 */    12,    6,   40,   32,    3,    3,   86,   87,   88,    2,
 /*   190 */    31,   32,   92,   93,   94,    6,   96,   40,    3,   32,
 /*   200 */    41,   42,   43,   44,   11,    2,   71,   48,   49,   50,
 /*   210 */    51,   32,   53,   54,   55,   56,   91,   58,    5,    6,
 /*   220 */     7,   86,   87,   88,   11,   71,    3,   92,    3,   94,
 /*   230 */    95,   96,   19,   47,   41,   42,   43,   44,   64,   13,
 /*   240 */    86,   87,   88,   47,   31,   76,   92,   76,   94,   66,
 /*   250 */    96,    6,   89,   35,   41,   42,   43,   44,   66,   89,
 /*   260 */    47,   48,   49,   50,   51,    6,   53,   54,   55,   56,
 /*   270 */    70,   58,    5,    6,    7,   64,   70,    4,   11,   71,
 /*   280 */    75,   74,   47,   75,   64,   91,   19,   47,   64,   74,
 /*   290 */    88,   64,   88,   64,   86,   87,   88,   88,   31,   30,
 /*   300 */    92,   64,   94,   64,   96,   64,   76,   99,   41,   42,
 /*   310 */    43,   44,   99,   99,   99,   48,   49,   50,   51,   99,
 /*   320 */    53,   54,   55,   56,    2,   58,   99,   99,   99,   99,
 /*   330 */     8,    9,   99,   99,   99,   99,   99,   15,   16,   17,
 /*   340 */    18,   99,   20,   21,   22,   23,   24,   25,   26,   27,
 /*   350 */    28,    2,   30,   99,   32,   99,   99,    8,    9,   99,
 /*   360 */    99,   99,   99,   99,   15,   16,   17,   18,   99,   20,
 /*   370 */    21,   22,   23,   24,   25,   26,   27,   28,   99,   30,
 /*   380 */    99,   32,    8,    9,   99,   99,   99,   99,   99,   15,
 /*   390 */    16,   17,   18,   99,   20,   21,   22,   23,   24,   25,
 /*   400 */    26,   27,   28,    2,   30,   99,   32,   99,   99,    8,
 /*   410 */     9,   99,   99,   99,   99,   99,   15,   16,   17,   18,
 /*   420 */    99,   20,   21,   22,   23,   24,   25,   26,   27,   28,
 /*   430 */     2,   30,   99,   99,   99,   99,    8,    9,   99,   99,
 /*   440 */    99,   99,   99,   15,   16,   17,   18,   99,   20,   21,
 /*   450 */    22,   23,   24,   25,   26,   27,   28,   99,   30,    8,
 /*   460 */     9,   99,   99,   99,   99,   99,   15,   16,   17,   18,
 /*   470 */    99,   20,   21,   22,   23,   24,   25,   26,   27,   28,
 /*   480 */    99,   30,   99,   32,    8,    9,   99,   99,   99,   13,
 /*   490 */    99,   15,   16,   17,   18,   99,   20,   21,   22,   23,
 /*   500 */    24,   25,   26,   27,   28,   99,   30,    8,    9,   99,
 /*   510 */    99,   99,    8,    9,   15,   16,   17,   18,   99,   20,
 /*   520 */    21,   22,   23,   24,   25,   26,   27,   28,   99,   30,
 /*   530 */    26,   27,   28,   99,   30,    8,    9,   99,   99,   99,
 /*   540 */    99,   99,   15,   16,   17,   18,   47,   20,   21,   22,
 /*   550 */    23,   24,   25,   26,   27,   28,   99,   30,   61,   62,
 /*   560 */    63,   99,   65,   99,   99,   99,   69,   99,   71,   99,
 /*   570 */    73,   99,   99,   99,   99,   99,   99,   80,   99,   99,
 /*   580 */    83,   84,   99,   86,   87,   88,   99,   62,   63,   92,
 /*   590 */    65,   94,   99,   96,   69,   99,   71,   99,   73,   99,
 /*   600 */    99,   99,   99,   99,   99,   80,   99,   99,   83,   84,
 /*   610 */    99,   86,   87,   88,   99,   62,   63,   92,   65,   94,
 /*   620 */    99,   96,   69,   99,   71,   99,   73,   99,   99,   99,
 /*   630 */    99,   99,   99,   80,   99,   99,   83,   84,   99,   86,
 /*   640 */    87,   88,    8,    9,   99,   92,   99,   94,   99,   96,
 /*   650 */    99,   17,   18,   71,   20,   21,   22,   23,   24,   25,
 /*   660 */    26,   27,   28,   99,   30,    5,    6,    7,   86,   87,
 /*   670 */    88,   11,   99,   99,   92,   99,   94,   99,   96,   99,
 /*   680 */    99,   99,   99,   99,   99,   99,   99,   99,   99,   99,
 /*   690 */    99,   31,   99,   99,   99,   99,   99,   99,   99,   99,
 /*   700 */    99,   41,   42,   43,   44,   99,   99,   99,   99,   99,
 /*   710 */    50,   51,    8,    9,   54,   55,   99,   99,   58,   99,
 /*   720 */    99,   99,   18,   99,   20,   21,   22,   23,   24,   25,
 /*   730 */    26,   27,   28,   99,   30,    8,    9,   99,   99,   99,
 /*   740 */    99,   99,   99,   71,   99,   99,   71,   20,   21,   22,
 /*   750 */    23,   24,   25,   26,   27,   28,   99,   30,   86,   87,
 /*   760 */    88,   86,   87,   88,   92,   99,   94,   92,   96,   94,
 /*   770 */    99,   96,   99,   71,   99,   99,   71,   99,   99,   99,
 /*   780 */    99,   99,   99,   99,   99,   99,   99,   71,   86,   87,
 /*   790 */    88,   86,   87,   88,   92,   99,   94,   92,   96,   94,
 /*   800 */    71,   96,   86,   87,   88,   99,   99,   99,   92,   99,
 /*   810 */    94,   99,   96,   99,   99,   86,   87,   88,   71,   99,
 /*   820 */    99,   92,   99,   94,   99,   96,   99,   99,   99,   71,
 /*   830 */    99,   99,   71,   86,   87,   88,   99,   99,   99,   92,
 /*   840 */    99,   94,   99,   96,   86,   87,   88,   86,   87,   88,
 /*   850 */    92,   99,   94,   92,   96,   94,   99,   96,   99,   71,
 /*   860 */    99,   99,   71,   99,   99,   99,   99,   99,   99,   99,
 /*   870 */    99,   99,   99,   71,   86,   87,   88,   86,   87,   88,
 /*   880 */    92,   99,   94,   92,   96,   94,   99,   96,   86,   87,
 /*   890 */    88,   99,   99,   71,   92,   99,   94,   99,   96,   99,
 /*   900 */    99,   99,   99,   99,   71,   99,   99,   71,   86,   87,
 /*   910 */    88,   99,   99,   99,   92,   99,   94,   99,   96,   86,
 /*   920 */    87,   88,   86,   87,   88,   92,   71,   94,   92,   96,
 /*   930 */    94,   99,   96,   99,   99,   99,   99,   99,   99,   99,
 /*   940 */    99,   86,   87,   88,   99,   71,   99,   92,   71,   94,
 /*   950 */    99,   96,   99,   99,   99,   99,   99,   99,   99,   71,
 /*   960 */    86,   87,   88,   86,   87,   88,   92,   99,   94,   92,
 /*   970 */    96,   94,   99,   96,   86,   87,   88,   99,   99,   71,
 /*   980 */    92,   99,   94,   99,   96,   99,   99,   99,   99,   99,
 /*   990 */    71,   99,   99,   71,   86,   87,   88,   99,   99,   99,
 /*  1000 */    92,   99,   94,   99,   96,   86,   87,   88,   86,   87,
 /*  1010 */    88,   92,   99,   94,   92,   96,   94,   99,   96,
];
const YY_SHIFT_USE_DFLT: i32 = -23;
const YY_SHIFT_COUNT: i32 = 133;
const YY_SHIFT_MIN: i32 = -22;
const YY_SHIFT_MAX: i32 = 727;
const YY_SHIFT_OFST: [i16; 134] = [
 /*     0 */    -5,   -5,   -5,  213,  159,  267,  267,  267,  267,  105,
 /*    10 */    50,  267,  267,  267,  267,  267,  267,  267,  267,  267,
 /*    20 */   267,  267,  267,  267,  267,  267,  267,  267,  267,  267,
 /*    30 */   267,  267,  267,  267,  660,  660,   82,   90,   90,  109,
 /*    40 */    60,  119,  119,  269,  269,  476,  476,  476,  499,  476,
 /*    50 */   193,  193,  193,    6,    1,  226,  240,  226,  235,  273,
 /*    60 */   259,  226,  259,  245,  218,  218,  245,  196,  196,  226,
 /*    70 */   186,  349,  322,  451,  428,  401,  374,  527,  527,  527,
 /*    80 */   527,  634,  634,  704,  704,  727,  727,  727,  727,  504,
 /*    90 */   504,   -6,   -6,   28,  -22,  179,  225,  223,  203,  195,
 /*   100 */   167,  157,  189,  187,  182,  181,  151,  142,  175,  168,
 /*   110 */   173,  164,  166,  160,  152,  120,  115,  139,  141,  140,
 /*   120 */   131,   72,  106,   78,  111,   31,   72,   72,   47,   65,
 /*   130 */    55,   39,   17,   13,
];
const YY_REDUCE_USE_DFLT: i32 = -68;
const YY_REDUCE_COUNT: i32 = 70;
const YY_REDUCE_MIN: i32 = -67;
const YY_REDUCE_MAX: i32 = 922;
const YY_REDUCE_OFST: [i16; 71] = [
 /*     0 */   497,  553,  525,  208,  135,  100,   81,  -12,  -24,  672,
 /*    10 */   922,  919,  908,  888,  877,  874,  855,  836,  833,  822,
 /*    20 */   802,  791,  788,  761,  758,  747,  729,  716,  705,  702,
 /*    30 */   675,  672,  582,  154,   32,  -61,  -62,  -67,    8,   33,
 /*    40 */   -56,   29,   25,   -4,  -16,  241,  239,  237,  230,  229,
 /*    50 */   209,  204,  202,  227,  215,  224,  194,  220,  205,  207,
 /*    60 */   206,  211,  200,  192,  170,  163,  183,  171,  169,  174,
 /*    70 */   125,
];
const YY_DEFAULT: [YYACTIONTYPE; 210] = [
 /*     0 */   211,  211,  211,  321,  321,  309,  309,  321,  321,  321,
 /*    10 */   321,  321,  321,  321,  321,  321,  321,  321,  321,  321,
 /*    20 */   321,  321,  321,  321,  321,  321,  321,  321,  321,  321,
 /*    30 */   321,  321,  321,  321,  321,  321,  321,  321,  321,  316,
 /*    40 */   321,  316,  316,  224,  224,  321,  321,  321,  321,  321,
 /*    50 */   321,  321,  321,  321,  321,  321,  321,  321,  321,  250,
 /*    60 */   243,  321,  243,  232,  235,  235,  232,  321,  264,  321,
 /*    70 */   321,  321,  321,  321,  310,  313,  321,  228,  227,  220,
 /*    80 */   215,  290,  289,  280,  288,  296,  295,  294,  293,  292,
 /*    90 */   291,  283,  284,  321,  297,  321,  321,  321,  244,  321,
 /*   100 */   321,  321,  321,  233,  321,  321,  321,  321,  321,  321,
 /*   110 */   321,  321,  321,  321,  321,  321,  321,  321,  321,  321,
 /*   120 */   321,  285,  321,  321,  321,  321,  287,  286,  321,  275,
 /*   130 */   321,  321,  321,  321,  258,  257,  261,  260,  253,  252,
 /*   140 */   248,  251,  249,  247,  246,  245,  242,  234,  236,  231,
 /*   150 */   230,  229,  226,  225,  223,  222,  221,  312,  314,  308,
 /*   160 */   320,  319,  318,  317,  315,  311,  307,  306,  305,  304,
 /*   170 */   303,  302,  301,  300,  299,  298,  282,  279,  278,  274,
 /*   180 */   276,  273,  272,  271,  270,  269,  268,  267,  266,  265,
 /*   190 */   263,  262,  219,  218,  217,  216,  259,  256,  255,  254,
 /*   200 */   241,  240,  239,  238,  237,  281,  277,  214,  213,  212,
];

/* TMPL: fallback tokens */

const YY_FALLBACK: [i32; 0] = [
];

/* TMPL: symbol names */


/* TMPL: rules */


/* TMPL: destructors */


/* TMPL: stack-overflow */


/* TMPL: stack-overflow */

const YY_RULE_INFO: [YYCODETYPE; 111] = [
  61,
  62,
  62,
  63,
  63,
  63,
  63,
  63,
  63,
  63,
  63,
  63,
  80,
  81,
  81,
  82,
  84,
  83,
  83,
  64,
  65,
  65,
  66,
  66,
  66,
  89,
  89,
  88,
  88,
  88,
  88,
  88,
  69,
  70,
  70,
  70,
  73,
  73,
  73,
  74,
  74,
  74,
  75,
  75,
  87,
  71,
  71,
  71,
  87,
  87,
  91,
  91,
  87,
  76,
  76,
  77,
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
  98,
  98,
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
 (YYMinorType::YY116(yy0),) => {

	if list::is_empty(&yy0) {
		panic!("null program");
	}
	// ignore yyres, it doesn't really go anywhere for program
	yyres = Ast::Nothing;
	// we're done, so put yy0 in extra
	self.extra = Ok(Ast::ReplRoot(yy0));

},    _ => unreachable!() };
 YYMinorType::YY103(yyres)
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
 YYMinorType::YY116(yyres)
}
            ,
            2 /* stmts ::= stmt stmts */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY116(yy0),YYMinorType::YY116(yy1),) => {

    verbose_out!("found new stmt: {:?}\n", yy0);
	yyres = list::cons(yy0, yy1);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            3 /* stmt ::= defstruct */
          | 4 /* stmt ::= let_stmt */
          | 6 /* stmt ::= fail_stmt */
          | 7 /* stmt ::= func_stmt */
          | 8 /* stmt ::= macro_stmt */
          | 9 /* stmt ::= if_stmt */
          | 55 /* pexpr ::= ptuple */
          | 68 /* expr ::= list */
          | 69 /* expr ::= tuple */
          | 87 /* expr ::= term */
          | 97 /* term ::= strexpr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY116(yy0),) => {
 yyres = yy0; 
},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            5 /* stmt ::= expr */
          | 44 /* expr ::= call_expr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY116(yy0),) => {

    yyres = yy0;

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            10 /* stmt ::= RETURN expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY116(yy1),) => {

    yyres = sexpr::new(SexprType::Return, yy1);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            11 /* stmt ::= FAILED ID match_case DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY111(yy1),YYMinorType::YY116(yy2),) => {

	yyres = sexpr::new(SexprType::MatchFailed,
        list::cons(Val::id(yy1.data),
        list::cons(yy2,
        Val::Nil
        ))
    );

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
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
 (YYMinorType::YY9(yy1),YYMinorType::YY116(yy2),) => {

    yyres = sexpr::def_struct(Val::Type(yy1), yy2);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            13 /* defstruct_fields ::= defstruct_field defstruct_fields */
          | 108 /* strlist ::= strlist_term strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY116(yy0),YYMinorType::YY116(yy1),) => {

	yyres = list::cons(yy0, yy1);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            14 /* defstruct_fields ::= */
          | 22 /* dfunc_args ::= */
          | 99 /* list_items ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

	yyres = list::empty();

} };
 YYMinorType::YY116(yyres)
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
 (YYMinorType::YY111(yy1),YYMinorType::YY9(yy3),) => {

	yyres = sexpr::id_with_type(yy1.data, yy3);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
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
 (YYMinorType::YY111(yy2),YYMinorType::YY116(yy4),) => {

vout!("found fail_stmt {:?}\n", yy2);
	yyres = sexpr::new(SexprType::Fail,
        list::cons(Val::hashtag(yy2.data),
        list::cons(yy4,
        Val::Nil,
        ))
    );

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            17 /* let_stmt ::= Let ID ASSIGN expr */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY111(yy1),YYMinorType::YY116(yy3),) => {

	let letx =
        list::cons(Val::id(yy1.data),
        list::cons(yy3,
        Val::Nil
        ));
	yyres = sexpr::new(SexprType::Let, letx);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            18 /* let_stmt ::= Fork ID ASSIGN expr */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY111(yy1),YYMinorType::YY116(yy3),) => {

	let bind = list::cons(Val::new_str(yy1.data), list::singleton(yy3));
	yyres = sexpr::new(SexprType::Fork, bind);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            19 /* block ::= BLOCKARROW stmts */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY116(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            20 /* func_stmt ::= Func ID PARENCALL dfunc_args RPAREN opt_typex block DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp7 = self.yystack.pop().unwrap();
let yyp6 = self.yystack.pop().unwrap();
let yyp5 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,yyp5.minor,yyp6.minor,) {
 (YYMinorType::YY111(yy1),YYMinorType::YY116(yy3),YYMinorType::YY9(yy5),YYMinorType::YY116(yy6),) => {

	let id = Val::id(yy1.data);
	let typ = Val::Type(yy5);
	yyres = sexpr::defunc(id, yy3, typ, yy6)

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            21 /* func_stmt ::= Func ID PARENCALL dfunc_args RPAREN opt_typex match_case DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp7 = self.yystack.pop().unwrap();
let yyp6 = self.yystack.pop().unwrap();
let yyp5 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,yyp5.minor,yyp6.minor,) {
 (YYMinorType::YY111(yy1),YYMinorType::YY116(yy3),YYMinorType::YY9(yy5),YYMinorType::YY116(yy6),) => {

	let id = Val::id(yy1.data);
	let typ = Val::Type(yy5);
    let body = sexpr::match_expr(Val::CallParams, yy6);
	yyres = sexpr::defunc(id, yy3, typ, body)

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            23 /* dfunc_args ::= ID opt_typex */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY111(yy0),YYMinorType::YY9(yy1),) => {

	yyres = list::singleton(sexpr::id_with_type(yy0.data, yy1));

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            24 /* dfunc_args ::= ID opt_typex COMMA dfunc_args */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY111(yy0),YYMinorType::YY9(yy1),YYMinorType::YY116(yy3),) => {

	yyres = list::cons(sexpr::id_with_type(yy0.data, yy1), yy3);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            25 /* opt_typex ::= */
            => 
{
let yyres :  Type ;
match () {
 () => {

	yyres = Type::AnonVar;

} };
 YYMinorType::YY9(yyres)
}
            ,
            26 /* opt_typex ::= COLON typex */
            => 
{
let yyres :  Type ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY9(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY9(yyres)
}
            ,
            27 /* typex ::= TYPE_INT */
            => 
{
let yyres :  Type ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Type::Int;

} };
 YYMinorType::YY9(yyres)
}
            ,
            28 /* typex ::= TYPE_STR */
            => 
{
let yyres :  Type ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Type::Str;

} };
 YYMinorType::YY9(yyres)
}
            ,
            29 /* typex ::= TYPE_BOOL */
            => 
{
let yyres :  Type ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Type::Bool;

} };
 YYMinorType::YY9(yyres)
}
            ,
            30 /* typex ::= TYPE_VOID */
            => 
{
let yyres :  Type ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Type::Void;

} };
 YYMinorType::YY9(yyres)
}
            ,
            31 /* typex ::= TYPE_ID */
            => 
{
let yyres :  Type ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY111(yy0),) => {

	yyres = Type::Id(Arc::new(yy0.data));

},    _ => unreachable!() };
 YYMinorType::YY9(yyres)
}
            ,
            32 /* macro_stmt ::= MACRO ID PARENCALL macro_args RPAREN block DOUBLEDASH */
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
 (YYMinorType::YY111(yy1),YYMinorType::YY116(yy3),YYMinorType::YY116(yy5),) => {

    verbose_out!("found macro {:?}\n", yy1);
    yyres = sexpr::new(SexprType::DefMacro,
        list::cons(Val::id(yy1.data),
        list::cons(yy3,
        list::cons(yy5,
        Val::Nil
    ))));

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            33 /* macro_args ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

    yyres = Val::Nil;

} };
 YYMinorType::YY116(yyres)
}
            ,
            34 /* macro_args ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY111(yy0),) => {

    yyres = list::singleton(Val::id(yy0.data));

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            35 /* macro_args ::= ID COMMA macro_args */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY111(yy0),YYMinorType::YY116(yy2),) => {

    yyres = list::cons(Val::id(yy0.data), yy2);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            36 /* if_stmt ::= IF expr block DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY116(yy1),YYMinorType::YY116(yy2),) => {

    /* if-only style */
    yyres = sexpr::ifstmt(yy1, yy2, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            37 /* if_stmt ::= IF expr block else_if DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp4 = self.yystack.pop().unwrap();
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,yyp3.minor,) {
 (YYMinorType::YY116(yy1),YYMinorType::YY116(yy2),YYMinorType::YY116(yy3),) => {

    /* if-else style */
    yyres = sexpr::ifstmt(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            38 /* if_stmt ::= IF if_case DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY116(yy1),) => {

    /* case-expr style */
    yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            39 /* else_if ::= ELSE IF expr block else_if */
            => 
{
let yyres :  Val ;
let yyp4 = self.yystack.pop().unwrap();
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp2.minor,yyp3.minor,yyp4.minor,) {
 (YYMinorType::YY116(yy2),YYMinorType::YY116(yy3),YYMinorType::YY116(yy4),) => {

    yyres = sexpr::ifstmt(yy2, yy3, yy4);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            40 /* else_if ::= ELSE IF expr block */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp2.minor,yyp3.minor,) {
 (YYMinorType::YY116(yy2),YYMinorType::YY116(yy3),) => {

    yyres = sexpr::ifstmt(yy2, yy3, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            41 /* else_if ::= ELSE block */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY116(yy1),) => {

    yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            42 /* if_case ::= PIPE expr block if_case */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,yyp3.minor,) {
 (YYMinorType::YY116(yy1),YYMinorType::YY116(yy2),YYMinorType::YY116(yy3),) => {

    yyres = sexpr::ifstmt(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            43 /* if_case ::= PIPE ELSE block */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp2.minor,) {
 (YYMinorType::YY116(yy2),) => {

    yyres = yy2;

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            45 /* call_expr ::= term PARENCALL RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY116(yy0),) => {

	verbose_out!("zero param function call!");
	yyres = sexpr::call(yy0, vec![]);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            46 /* call_expr ::= term PARENCALL expr RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY116(yy0),YYMinorType::YY116(yy2),) => {

	verbose_out!("one param function call!");
	yyres = sexpr::call(yy0, vec![yy2]);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            47 /* call_expr ::= term PARENCALL tuple_args RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY116(yy0),YYMinorType::YY116(yy2),) => {

	verbose_out!("multi param function call!");
	yyres = sexpr::call(yy0, list::to_vec(yy2));

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
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
 (YYMinorType::YY116(yy0),YYMinorType::YY116(yy2),) => {

	/* yyres = Val::binaryop(yy0, yy2, D); */
	yyres = Val::Void;

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            49 /* expr ::= CASE cases DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY116(yy1),) => {

    verbose_out!("parsed case expr\n");
	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            50 /* cases ::= PIPE expr block PIPE ELSE block */
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
 (YYMinorType::YY116(yy1),YYMinorType::YY116(yy2),YYMinorType::YY116(yy5),) => {

    verbose_out!("found cases base\n");
    yyres = sexpr::casex(yy1, yy2, yy5);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            51 /* cases ::= PIPE expr block cases */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,yyp3.minor,) {
 (YYMinorType::YY116(yy1),YYMinorType::YY116(yy2),YYMinorType::YY116(yy3),) => {

    verbose_out!("found extra case\n");
    yyres = sexpr::casex(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            52 /* expr ::= MATCH expr match_case DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY116(yy1),YYMinorType::YY116(yy2),) => {

    verbose_out!("parsed match expr\n");
    yyres = sexpr::match_expr(yy1, yy2);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            53 /* match_case ::= PIPE pexpr block match_case */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,yyp3.minor,) {
 (YYMinorType::YY116(yy1),YYMinorType::YY116(yy2),YYMinorType::YY116(yy3),) => {

    verbose_out!("found cases base\n");
    yyres = sexpr::match_case(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            54 /* match_case ::= PIPE pexpr block */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY116(yy1),YYMinorType::YY116(yy2),) => {

    verbose_out!("parsed base match case\n");
    yyres = sexpr::match_case(yy1, yy2, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            56 /* pexpr ::= INT */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY68(yy0),) => {
 yyres = Val::Int(yy0); 
},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            57 /* pexpr ::= True */
          | 94 /* term ::= True */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Bool(true); 
} };
 YYMinorType::YY116(yyres)
}
            ,
            58 /* pexpr ::= False */
          | 95 /* term ::= False */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Bool(false); 
} };
 YYMinorType::YY116(yyres)
}
            ,
            59 /* pexpr ::= HASHTAG */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY111(yy0),) => {
 yyres = Val::Hashtag(Arc::new(yy0.data)); 
},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            60 /* pexpr ::= ID */
          | 90 /* term ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY111(yy0),) => {
 yyres = Val::id(yy0.data); 
},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            61 /* pexpr ::= UNDERSCORE */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Wildcard; 
} };
 YYMinorType::YY116(yyres)
}
            ,
            62 /* ptuple ::= LPAREN RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match () {
 () => {

	panic!("an empty tuple is not a valid pattern");

} };
 YYMinorType::YY116(yyres)
}
            ,
            63 /* ptuple ::= LPAREN pexpr RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY116(yy1),) => {

	yyres = Val::Tuple(vec![yy1]);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            64 /* ptuple ::= LPAREN pargs RPAREN */
          | 102 /* tuple ::= LPAREN tuple_args RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY116(yy1),) => {

	yyres = Val::tuple_from_list(yy1);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            65 /* pargs ::= pexpr COMMA pexpr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY116(yy0),YYMinorType::YY116(yy2),) => {

	yyres = list::cons(yy0,
        list::cons(yy2,
        Val::Nil
        ));

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            66 /* pargs ::= pexpr COMMA pargs */
          | 101 /* list_items ::= expr COMMA list_items */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY116(yy0),YYMinorType::YY116(yy2),) => {

	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            67 /* expr ::= expr DOT ID */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY116(yy0),YYMinorType::YY111(yy2),) => {

    yyres = sexpr::new(SexprType::FieldAccess,
        list::cons(yy0,
        list::cons(Val::id(yy2.data),
        Val::Nil,
        ))
    );

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            70 /* expr ::= NOT expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY116(yy1),) => {

	yyres = sexpr::call(Val::id("bool_not".to_string()), vec![yy1]);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            71 /* expr ::= expr ConcatNewline */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY116(yy0),) => {

	let newline = Val::Str(Arc::new("\n".to_string()));
	let args = list::cons(yy0, list::singleton(newline));
	yyres = sexpr::new(SexprType::StrExpr, args)

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            72 /* expr ::= NEGATE term */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY116(yy1),) => {

	yyres = sexpr::call(Val::id("negate".to_string()), vec![yy1]);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            73 /* expr ::= expr PLUS expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY116(yy0),YYMinorType::YY116(yy2),) => {

	yyres = sexpr::binaryop("int_add".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            74 /* expr ::= expr MINUS expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY116(yy0),YYMinorType::YY116(yy2),) => {

	yyres = sexpr::binaryop("int_sub".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            75 /* expr ::= expr TIMES expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY116(yy0),YYMinorType::YY116(yy2),) => {

	yyres = sexpr::binaryop("int_mult".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            76 /* expr ::= expr SLASH expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY116(yy0),YYMinorType::YY116(yy2),) => {

	yyres = sexpr::binaryop("int_div".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            77 /* expr ::= expr MOD expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY116(yy0),YYMinorType::YY116(yy2),) => {

	yyres = sexpr::binaryop("int_mod".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            78 /* expr ::= expr AND expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY116(yy0),YYMinorType::YY116(yy2),) => {

	yyres = sexpr::binaryop("boolean_and".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            79 /* expr ::= expr OR expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY116(yy0),YYMinorType::YY116(yy2),) => {

	yyres = sexpr::binaryop("boolean_or".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            80 /* expr ::= expr XOR expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY116(yy0),YYMinorType::YY116(yy2),) => {

	yyres = sexpr::binaryop("boolean_xor".to_string(),yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            81 /* expr ::= expr LT expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY116(yy0),YYMinorType::YY116(yy2),) => {

	yyres = sexpr::binaryop("less_than".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            82 /* expr ::= expr LTEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY116(yy0),YYMinorType::YY116(yy2),) => {

	yyres = sexpr::binaryop("less_than_equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            83 /* expr ::= expr GT expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY116(yy0),YYMinorType::YY116(yy2),) => {

	yyres = sexpr::binaryop("greater_than".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            84 /* expr ::= expr GTEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY116(yy0),YYMinorType::YY116(yy2),) => {

	yyres = sexpr::binaryop("greater_than_equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            85 /* expr ::= expr EQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY116(yy0),YYMinorType::YY116(yy2),) => {

	yyres = sexpr::binaryop("equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            86 /* expr ::= expr NEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY116(yy0),YYMinorType::YY116(yy2),) => {

	let eq = sexpr::binaryop("equal".to_string(), yy0, yy2);
	yyres = sexpr::call(Val::id("bool_not".to_string()), vec![eq]);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            88 /* term ::= LPAREN expr RPAREN */
          | 98 /* list ::= SquareL list_items SquareR */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY116(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            89 /* term ::= typex */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY9(yy0),) => {

    yyres = Val::Type(yy0);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            91 /* term ::= VOID */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Val::Void;

} };
 YYMinorType::YY116(yyres)
}
            ,
            92 /* term ::= DollarQuestion */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Val::id("$".to_string());

} };
 YYMinorType::YY116(yyres)
}
            ,
            93 /* term ::= INT */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY68(yy0),) => {

	yyres = Val::Int(yy0);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            96 /* term ::= HASHTAG */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY111(yy0),) => {

	yyres = Val::Hashtag(Arc::new(yy0.data));

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            100 /* list_items ::= expr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY116(yy0),) => {

	yyres = list::singleton(yy0);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            103 /* tuple_args ::= expr COMMA expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY116(yy0),YYMinorType::YY116(yy2),) => {

	vout!("base tuple args!");
	yyres = list::cons(yy0, list::singleton(yy2));

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            104 /* tuple_args ::= expr COMMA tuple_args */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY116(yy0),YYMinorType::YY116(yy2),) => {

	vout!("additional tuple arg!");
	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            105 /* strexpr ::= StrOpen strlist StrClose */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY116(yy1),) => {

	yyres = sexpr::strexpr(yy1);
    vout!("strexpr({:?})\n", yyres);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            106 /* strlist ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

	yyres = Val::Nil;

} };
 YYMinorType::YY116(yyres)
}
            ,
            107 /* strlist ::= StrLit strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY166(yy0),YYMinorType::YY116(yy1),) => {

	yyres = list::cons(Val::new_str(yy0), yy1);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            109 /* strlist_term ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY111(yy0),) => {

    yyres = Val::id(yy0.data);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            110 /* strlist_term ::= strlist_term DOT ID */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY116(yy0),YYMinorType::YY111(yy2),) => {

    yyres = sexpr::new(SexprType::FieldAccess,
        list::cons(yy0,
        list::cons(Val::id(yy2.data),
        Val::Nil,
    )))

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
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

