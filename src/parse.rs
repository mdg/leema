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
const YYNOCODE: i32 = 101;
type YYACTIONTYPE = u16;
const YYWILDCARD: YYCODETYPE = 1;
enum YYMinorType {
    YY0,
    YY4(String),
    YY24(Val),
    YY60(i64),
    YY117(TokenData<String>),
    YY128(TokenLoc),
    YY195(Type),
    YY197(Ast),
}
const YYNSTATE: i32 = 212;
const YYNRULE: i32 = 113;
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
    CALL_ID( TokenData<String> ), //6
    CALL_TYPE_ID( TokenData<String> ), //7
    ID( TokenData<String> ), //8
    INT( i64 ), //9
    PLUS( TokenLoc ), //10
    SLASH( TokenLoc ), //11
    StrLit( String ), //12
    TYPE_ID( TokenData<String> ), //13
    ASSIGN, //14
    BLOCKARROW, //15
    RETURN, //16
    OR, //17
    XOR, //18
    AND, //19
    ConcatNewline, //20
    NOT, //21
    EQ, //22
    NEQ, //23
    GT, //24
    GTEQ, //25
    LT, //26
    LTEQ, //27
    MINUS, //28
    TIMES, //29
    MOD, //30
    DOLLAR, //31
    DOT, //32
    LPAREN, //33
    RPAREN, //34
    FAILED, //35
    STRUCT, //36
    COLON, //37
    FAIL, //38
    Let, //39
    Fork, //40
    Func, //41
    TYPE_INT, //42
    TYPE_STR, //43
    TYPE_BOOL, //44
    TYPE_VOID, //45
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
pub const TOKEN_CALL_ID: i32 = 6;
pub const TOKEN_CALL_TYPE_ID: i32 = 7;
pub const TOKEN_ID: i32 = 8;
pub const TOKEN_INT: i32 = 9;
pub const TOKEN_PLUS: i32 = 10;
pub const TOKEN_SLASH: i32 = 11;
pub const TOKEN_StrLit: i32 = 12;
pub const TOKEN_TYPE_ID: i32 = 13;
pub const TOKEN_ASSIGN: i32 = 14;
pub const TOKEN_BLOCKARROW: i32 = 15;
pub const TOKEN_RETURN: i32 = 16;
pub const TOKEN_OR: i32 = 17;
pub const TOKEN_XOR: i32 = 18;
pub const TOKEN_AND: i32 = 19;
pub const TOKEN_ConcatNewline: i32 = 20;
pub const TOKEN_NOT: i32 = 21;
pub const TOKEN_EQ: i32 = 22;
pub const TOKEN_NEQ: i32 = 23;
pub const TOKEN_GT: i32 = 24;
pub const TOKEN_GTEQ: i32 = 25;
pub const TOKEN_LT: i32 = 26;
pub const TOKEN_LTEQ: i32 = 27;
pub const TOKEN_MINUS: i32 = 28;
pub const TOKEN_TIMES: i32 = 29;
pub const TOKEN_MOD: i32 = 30;
pub const TOKEN_DOLLAR: i32 = 31;
pub const TOKEN_DOT: i32 = 32;
pub const TOKEN_LPAREN: i32 = 33;
pub const TOKEN_RPAREN: i32 = 34;
pub const TOKEN_FAILED: i32 = 35;
pub const TOKEN_STRUCT: i32 = 36;
pub const TOKEN_COLON: i32 = 37;
pub const TOKEN_FAIL: i32 = 38;
pub const TOKEN_Let: i32 = 39;
pub const TOKEN_Fork: i32 = 40;
pub const TOKEN_Func: i32 = 41;
pub const TOKEN_TYPE_INT: i32 = 42;
pub const TOKEN_TYPE_STR: i32 = 43;
pub const TOKEN_TYPE_BOOL: i32 = 44;
pub const TOKEN_TYPE_VOID: i32 = 45;
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
        &Token::CALL_ID(_) => TOKEN_CALL_ID,
        &Token::CALL_TYPE_ID(_) => TOKEN_CALL_TYPE_ID,
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
        Token::COMMA(x) => YYMinorType::YY128(x),
        Token::DOUBLEDASH(x) => YYMinorType::YY128(x),
        Token::ELSE(x) => YYMinorType::YY128(x),
        Token::HASHTAG(x) => YYMinorType::YY117(x),
        Token::CALL_ID(x) => YYMinorType::YY117(x),
        Token::CALL_TYPE_ID(x) => YYMinorType::YY117(x),
        Token::ID(x) => YYMinorType::YY117(x),
        Token::INT(x) => YYMinorType::YY60(x),
        Token::PLUS(x) => YYMinorType::YY128(x),
        Token::SLASH(x) => YYMinorType::YY128(x),
        Token::StrLit(x) => YYMinorType::YY4(x),
        Token::TYPE_ID(x) => YYMinorType::YY117(x),
        _ => YYMinorType::YY0
  }
}
const YY_ACTTAB_COUNT: i32 = 1093;
const YY_ACTION: [YYACTIONTYPE; 1093] = [
 /*     0 */   169,  162,  161,  175,  172,   26,  143,   53,  202,  117,
 /*    10 */    43,   29,  128,  190,  181,  104,   15,   93,  190,  127,
 /*    20 */   153,   43,  212,   28,   25,  164,  132,  103,    8,   41,
 /*    30 */   119,   52,  208,  114,  111,  109,  107,  206,  205,  204,
 /*    40 */   203,  101,    3,   35,   70,   27,  171,  170,  197,   34,
 /*    50 */   174,  173,    6,  201,   42,   57,  169,  162,  161,  175,
 /*    60 */   172,  198,  192,  176,  202,  201,   37,    4,  131,   71,
 /*    70 */   176,  168,   15,  177,  179,  176,  178,  121,  168,    4,
 /*    80 */   131,   76,  176,  168,    8,    1,  179,    1,  178,  157,
 /*    90 */   168,   69,  190,  206,  205,  204,  203,   37,  182,  180,
 /*   100 */    70,   27,  171,  170,  167,   34,  174,  173,    6,  201,
 /*   110 */    42,   55,  169,  162,  161,  175,  172,  164,   38,   11,
 /*   120 */   202,   41,  201,    4,  131,   75,  176,  163,   15,  132,
 /*   130 */   179,  122,  178,  158,  168,  156,    4,  131,   75,  176,
 /*   140 */     8,  123,   67,  179,  159,  178,  155,  168,  202,  206,
 /*   150 */   205,  204,  203,  124,   39,  154,   70,   27,  171,  170,
 /*   160 */    51,   34,  174,  173,    6,  115,   42,  169,  162,  161,
 /*   170 */   175,  172,  166,   39,  113,  202,  201,  206,  205,  204,
 /*   180 */   203,  186,  112,   15,  185,  189,  165,   39,   14,  110,
 /*   190 */     4,  131,   72,  176,   13,    8,  200,  179,  108,  178,
 /*   200 */    94,  168,   12,  106,  206,  205,  204,  203,   66,   36,
 /*   210 */   183,   70,   27,  171,  170,  149,   34,  174,  173,    6,
 /*   220 */    63,   42,  169,  162,  161,  175,  172,  188,  187,  184,
 /*   230 */   202,  201,   65,  148,  100,   95,   62,  145,   15,   61,
 /*   240 */    60,  134,  142,  139,   30,    4,  131,   47,  176,   68,
 /*   250 */     8,    1,  179,  130,  178,  191,  168,   38,  105,  206,
 /*   260 */   205,  204,  203,   64,  118,   10,   70,   27,  171,  170,
 /*   270 */    50,   34,  174,  173,    6,   40,   42,  169,  162,  161,
 /*   280 */   175,  172,  102,  201,   99,  202,  146,   97,   98,  141,
 /*   290 */    10,  186,  144,   15,  185,  189,   53,    4,  131,   91,
 /*   300 */   176,  138,  137,   96,  179,    8,  178,  135,  168,    9,
 /*   310 */   136,  140,   56,   44,  206,  205,  204,  203,  152,   36,
 /*   320 */   147,   70,   27,  171,  170,    7,   34,  174,  173,    6,
 /*   330 */   129,   42,   54,   33,   26,   59,   58,  188,  187,  184,
 /*   340 */    23,   22,   24,  207,  116,   17,   16,   19,   18,   21,
 /*   350 */    20,   32,   28,   25,    7,  132,  327,  199,  327,  327,
 /*   360 */   327,  327,   33,   26,  327,  327,  327,  327,  327,   23,
 /*   370 */    22,   24,  207,  327,   17,   16,   19,   18,   21,   20,
 /*   380 */    32,   28,   25,    7,  132,  327,  160,  327,  327,  327,
 /*   390 */   327,   33,   26,  327,  327,  327,  327,  327,   23,   22,
 /*   400 */    24,  207,  327,   17,   16,   19,   18,   21,   20,   32,
 /*   410 */    28,   25,    5,  132,  327,  327,  327,  327,  327,  327,
 /*   420 */    33,   26,  327,  327,  327,  327,  327,   23,   22,   24,
 /*   430 */   207,  327,   17,   16,   19,   18,   21,   20,   32,   28,
 /*   440 */    25,  327,  132,   33,   26,  327,  327,  327,  327,  327,
 /*   450 */    23,   22,   24,  207,  327,   17,   16,   19,   18,   21,
 /*   460 */    20,   32,   28,   25,  327,  132,  327,  151,   33,   26,
 /*   470 */   327,  327,  327,  327,  327,   23,   22,   24,  207,  327,
 /*   480 */    17,   16,   19,   18,   21,   20,   32,   28,   25,  327,
 /*   490 */   132,  327,  160,   33,   26,  327,  327,  327,    1,  327,
 /*   500 */    23,   22,   24,  207,  327,   17,   16,   19,   18,   21,
 /*   510 */    20,   32,   28,   25,  327,  132,   33,   26,  327,  327,
 /*   520 */   327,  327,  327,   23,   22,   24,  207,  327,   17,   16,
 /*   530 */    19,   18,   21,   20,   32,   28,   25,  327,  132,  326,
 /*   540 */   133,    2,  327,  195,  327,  327,  327,  194,  327,  201,
 /*   550 */   327,  193,  327,  327,   38,  327,  327,  327,  210,  327,
 /*   560 */   327,  209,  196,    4,  131,   80,  176,  327,  327,  327,
 /*   570 */   179,  327,  178,  327,  168,   33,   26,  327,  327,  327,
 /*   580 */   327,  327,   23,   22,   24,  207,  327,   17,   16,   19,
 /*   590 */    18,   21,   20,   32,   28,   25,  327,  132,  211,    2,
 /*   600 */   327,  195,  327,  327,  327,  194,  327,  201,  327,  193,
 /*   610 */   327,  327,  327,  327,  327,  327,  210,  327,  327,  209,
 /*   620 */   196,    4,  131,   80,  176,  327,  150,    2,  179,  195,
 /*   630 */   178,  327,  168,  194,  327,  201,  327,  193,  327,  327,
 /*   640 */   327,  327,  327,  327,  210,  327,  327,  209,  196,    4,
 /*   650 */   131,   80,  176,  327,  327,  327,  179,  327,  178,  327,
 /*   660 */   168,   33,   26,  327,  169,  327,  327,  175,  172,  327,
 /*   670 */    24,  207,  202,   17,   16,   19,   18,   21,   20,   32,
 /*   680 */    28,   25,  327,  132,  327,  327,  327,   33,   26,  327,
 /*   690 */   327,  327,   31,  327,  327,  327,  327,  327,  327,  327,
 /*   700 */   327,  206,  205,  204,  203,   32,   28,   25,  327,  132,
 /*   710 */   171,  170,   33,   26,  174,  173,  327,  327,   42,  327,
 /*   720 */   327,  327,  207,  327,   17,   16,   19,   18,   21,   20,
 /*   730 */    32,   28,   25,  327,  132,   33,   26,  327,  327,  327,
 /*   740 */   327,  327,  327,  327,  201,  327,  327,  325,  325,  325,
 /*   750 */   325,   21,   20,   32,   28,   25,  201,  132,    4,  131,
 /*   760 */    92,  176,  327,  327,  327,  179,  327,  178,  327,  168,
 /*   770 */     4,  131,   73,  176,  327,  201,  327,  179,  327,  178,
 /*   780 */   327,  168,  327,  327,  327,  327,  327,  201,  327,    4,
 /*   790 */   131,   49,  176,  327,  327,  327,  179,  327,  178,  201,
 /*   800 */   168,    4,  131,   79,  176,  327,  327,  327,  179,  327,
 /*   810 */   178,  327,  168,    4,  131,  120,  176,  201,  327,  327,
 /*   820 */   179,  327,  178,  327,  168,  327,  327,  327,  327,  201,
 /*   830 */   327,    4,  131,   48,  176,  327,  327,  327,  179,  327,
 /*   840 */   178,  327,  168,    4,  131,  126,  176,  327,  201,  327,
 /*   850 */   179,  327,  178,  327,  168,  327,  327,  327,  327,  327,
 /*   860 */   201,  327,    4,  131,  125,  176,  327,  327,  327,  179,
 /*   870 */   327,  178,  201,  168,    4,  131,   84,  176,  327,  327,
 /*   880 */   327,  179,  327,  178,  327,  168,    4,  131,   82,  176,
 /*   890 */   201,  327,  327,  179,  327,  178,  327,  168,  327,  327,
 /*   900 */   327,  327,  201,  327,    4,  131,   81,  176,  327,  327,
 /*   910 */   327,  179,  327,  178,  327,  168,    4,  131,   90,  176,
 /*   920 */   327,  201,  327,  179,  327,  178,  327,  168,  327,  327,
 /*   930 */   327,  327,  327,  201,  327,    4,  131,   89,  176,  327,
 /*   940 */   327,  327,  179,  327,  178,  201,  168,    4,  131,   88,
 /*   950 */   176,  327,  327,  327,  179,  327,  178,  327,  168,    4,
 /*   960 */   131,   87,  176,  201,  327,  327,  179,  327,  178,  327,
 /*   970 */   168,  327,  327,  327,  327,  201,  327,    4,  131,   86,
 /*   980 */   176,  327,  327,  327,  179,  327,  178,  327,  168,    4,
 /*   990 */   131,   85,  176,  327,  201,  327,  179,  327,  178,  327,
 /*  1000 */   168,  327,  327,  327,  327,  327,  201,  327,    4,  131,
 /*  1010 */    83,  176,  327,  327,  327,  179,  327,  178,  201,  168,
 /*  1020 */     4,  131,   74,  176,  327,  327,  327,  179,  327,  178,
 /*  1030 */   327,  168,    4,  131,   78,  176,  201,  327,  327,  179,
 /*  1040 */   327,  178,  327,  168,  327,  327,  327,  327,  201,  327,
 /*  1050 */     4,  131,   77,  176,  327,  327,  327,  179,  327,  178,
 /*  1060 */   327,  168,    4,  131,   46,  176,  327,  201,  327,  179,
 /*  1070 */   327,  178,  327,  168,  327,  327,  327,  327,  327,  327,
 /*  1080 */   327,    4,  131,   45,  176,  327,  327,  327,  179,  327,
 /*  1090 */   178,  327,  168,
];
const YY_LOOKAHEAD: [YYCODETYPE; 1093] = [
 /*     0 */     5,    6,    7,    8,    9,   11,    3,    4,   13,   82,
 /*    10 */    83,   16,   78,   79,   80,   65,   21,   78,   79,   80,
 /*    20 */    82,   83,    0,   29,   30,    8,   32,   77,   33,   12,
 /*    30 */    35,   36,    8,   38,   39,   40,   41,   42,   43,   44,
 /*    40 */    45,   46,   47,   31,   49,   50,   51,   52,    3,   54,
 /*    50 */    55,   56,   57,   72,   59,    4,    5,    6,    7,    8,
 /*    60 */     9,   87,    3,   89,   13,   72,    2,   86,   87,   88,
 /*    70 */    89,   97,   21,   87,   93,   89,   95,   96,   97,   86,
 /*    80 */    87,   88,   89,   97,   33,   15,   93,   15,   95,   96,
 /*    90 */    97,   78,   79,   42,   43,   44,   45,    2,   34,   34,
 /*   100 */    49,   50,   51,   52,   60,   54,   55,   56,   57,   72,
 /*   110 */    59,    4,    5,    6,    7,    8,    9,    8,   48,   47,
 /*   120 */    13,   12,   72,   86,   87,   88,   89,    8,   21,   32,
 /*   130 */    93,   94,   95,   58,   97,   34,   86,   87,   88,   89,
 /*   140 */    33,   32,    8,   93,   94,   95,    3,   97,   13,   42,
 /*   150 */    43,   44,   45,   98,   99,    3,   49,   50,   51,   52,
 /*   160 */    37,   54,   55,   56,   57,    8,   59,    5,    6,    7,
 /*   170 */     8,    9,   98,   99,   33,   13,   72,   42,   43,   44,
 /*   180 */    45,    5,    5,   21,    8,    9,   98,   99,    2,    8,
 /*   190 */    86,   87,   88,   89,   14,   33,   34,   93,    8,   95,
 /*   200 */    96,   97,   14,    8,   42,   43,   44,   45,   33,   33,
 /*   210 */    34,   49,   50,   51,   52,    3,   54,   55,   56,   57,
 /*   220 */     2,   59,    5,    6,    7,    8,    9,   51,   52,   53,
 /*   230 */    13,   72,   34,    3,    8,   76,   33,    3,   21,   34,
 /*   240 */     2,   34,    3,    3,   48,   86,   87,   88,   89,   65,
 /*   250 */    33,   15,   93,   92,   95,   77,   97,   48,   67,   42,
 /*   260 */    43,   44,   45,    8,   77,   48,   49,   50,   51,   52,
 /*   270 */    37,   54,   55,   56,   57,   90,   59,    5,    6,    7,
 /*   280 */     8,    9,   90,   72,   71,   13,   67,    8,   65,   75,
 /*   290 */    48,    5,   71,   21,    8,    9,    4,   86,   87,   88,
 /*   300 */    89,   76,   65,   75,   93,   33,   95,   92,   97,   48,
 /*   310 */    65,   65,   65,   89,   42,   43,   44,   45,   89,   33,
 /*   320 */    89,   49,   50,   51,   52,    2,   54,   55,   56,   57,
 /*   330 */    77,   59,   65,   10,   11,   65,   65,   51,   52,   53,
 /*   340 */    17,   18,   19,   20,   32,   22,   23,   24,   25,   26,
 /*   350 */    27,   28,   29,   30,    2,   32,  100,   34,  100,  100,
 /*   360 */   100,  100,   10,   11,  100,  100,  100,  100,  100,   17,
 /*   370 */    18,   19,   20,  100,   22,   23,   24,   25,   26,   27,
 /*   380 */    28,   29,   30,    2,   32,  100,   34,  100,  100,  100,
 /*   390 */   100,   10,   11,  100,  100,  100,  100,  100,   17,   18,
 /*   400 */    19,   20,  100,   22,   23,   24,   25,   26,   27,   28,
 /*   410 */    29,   30,    2,   32,  100,  100,  100,  100,  100,  100,
 /*   420 */    10,   11,  100,  100,  100,  100,  100,   17,   18,   19,
 /*   430 */    20,  100,   22,   23,   24,   25,   26,   27,   28,   29,
 /*   440 */    30,  100,   32,   10,   11,  100,  100,  100,  100,  100,
 /*   450 */    17,   18,   19,   20,  100,   22,   23,   24,   25,   26,
 /*   460 */    27,   28,   29,   30,  100,   32,  100,   34,   10,   11,
 /*   470 */   100,  100,  100,  100,  100,   17,   18,   19,   20,  100,
 /*   480 */    22,   23,   24,   25,   26,   27,   28,   29,   30,  100,
 /*   490 */    32,  100,   34,   10,   11,  100,  100,  100,   15,  100,
 /*   500 */    17,   18,   19,   20,  100,   22,   23,   24,   25,   26,
 /*   510 */    27,   28,   29,   30,  100,   32,   10,   11,  100,  100,
 /*   520 */   100,  100,  100,   17,   18,   19,   20,  100,   22,   23,
 /*   530 */    24,   25,   26,   27,   28,   29,   30,  100,   32,   62,
 /*   540 */    63,   64,  100,   66,  100,  100,  100,   70,  100,   72,
 /*   550 */   100,   74,  100,  100,   48,  100,  100,  100,   81,  100,
 /*   560 */   100,   84,   85,   86,   87,   88,   89,  100,  100,  100,
 /*   570 */    93,  100,   95,  100,   97,   10,   11,  100,  100,  100,
 /*   580 */   100,  100,   17,   18,   19,   20,  100,   22,   23,   24,
 /*   590 */    25,   26,   27,   28,   29,   30,  100,   32,   63,   64,
 /*   600 */   100,   66,  100,  100,  100,   70,  100,   72,  100,   74,
 /*   610 */   100,  100,  100,  100,  100,  100,   81,  100,  100,   84,
 /*   620 */    85,   86,   87,   88,   89,  100,   63,   64,   93,   66,
 /*   630 */    95,  100,   97,   70,  100,   72,  100,   74,  100,  100,
 /*   640 */   100,  100,  100,  100,   81,  100,  100,   84,   85,   86,
 /*   650 */    87,   88,   89,  100,  100,  100,   93,  100,   95,  100,
 /*   660 */    97,   10,   11,  100,    5,  100,  100,    8,    9,  100,
 /*   670 */    19,   20,   13,   22,   23,   24,   25,   26,   27,   28,
 /*   680 */    29,   30,  100,   32,  100,  100,  100,   10,   11,  100,
 /*   690 */   100,  100,   33,  100,  100,  100,  100,  100,  100,  100,
 /*   700 */   100,   42,   43,   44,   45,   28,   29,   30,  100,   32,
 /*   710 */    51,   52,   10,   11,   55,   56,  100,  100,   59,  100,
 /*   720 */   100,  100,   20,  100,   22,   23,   24,   25,   26,   27,
 /*   730 */    28,   29,   30,  100,   32,   10,   11,  100,  100,  100,
 /*   740 */   100,  100,  100,  100,   72,  100,  100,   22,   23,   24,
 /*   750 */    25,   26,   27,   28,   29,   30,   72,   32,   86,   87,
 /*   760 */    88,   89,  100,  100,  100,   93,  100,   95,  100,   97,
 /*   770 */    86,   87,   88,   89,  100,   72,  100,   93,  100,   95,
 /*   780 */   100,   97,  100,  100,  100,  100,  100,   72,  100,   86,
 /*   790 */    87,   88,   89,  100,  100,  100,   93,  100,   95,   72,
 /*   800 */    97,   86,   87,   88,   89,  100,  100,  100,   93,  100,
 /*   810 */    95,  100,   97,   86,   87,   88,   89,   72,  100,  100,
 /*   820 */    93,  100,   95,  100,   97,  100,  100,  100,  100,   72,
 /*   830 */   100,   86,   87,   88,   89,  100,  100,  100,   93,  100,
 /*   840 */    95,  100,   97,   86,   87,   88,   89,  100,   72,  100,
 /*   850 */    93,  100,   95,  100,   97,  100,  100,  100,  100,  100,
 /*   860 */    72,  100,   86,   87,   88,   89,  100,  100,  100,   93,
 /*   870 */   100,   95,   72,   97,   86,   87,   88,   89,  100,  100,
 /*   880 */   100,   93,  100,   95,  100,   97,   86,   87,   88,   89,
 /*   890 */    72,  100,  100,   93,  100,   95,  100,   97,  100,  100,
 /*   900 */   100,  100,   72,  100,   86,   87,   88,   89,  100,  100,
 /*   910 */   100,   93,  100,   95,  100,   97,   86,   87,   88,   89,
 /*   920 */   100,   72,  100,   93,  100,   95,  100,   97,  100,  100,
 /*   930 */   100,  100,  100,   72,  100,   86,   87,   88,   89,  100,
 /*   940 */   100,  100,   93,  100,   95,   72,   97,   86,   87,   88,
 /*   950 */    89,  100,  100,  100,   93,  100,   95,  100,   97,   86,
 /*   960 */    87,   88,   89,   72,  100,  100,   93,  100,   95,  100,
 /*   970 */    97,  100,  100,  100,  100,   72,  100,   86,   87,   88,
 /*   980 */    89,  100,  100,  100,   93,  100,   95,  100,   97,   86,
 /*   990 */    87,   88,   89,  100,   72,  100,   93,  100,   95,  100,
 /*  1000 */    97,  100,  100,  100,  100,  100,   72,  100,   86,   87,
 /*  1010 */    88,   89,  100,  100,  100,   93,  100,   95,   72,   97,
 /*  1020 */    86,   87,   88,   89,  100,  100,  100,   93,  100,   95,
 /*  1030 */   100,   97,   86,   87,   88,   89,   72,  100,  100,   93,
 /*  1040 */   100,   95,  100,   97,  100,  100,  100,  100,   72,  100,
 /*  1050 */    86,   87,   88,   89,  100,  100,  100,   93,  100,   95,
 /*  1060 */   100,   97,   86,   87,   88,   89,  100,   72,  100,   93,
 /*  1070 */   100,   95,  100,   97,  100,  100,  100,  100,  100,  100,
 /*  1080 */   100,   86,   87,   88,   89,  100,  100,  100,   93,  100,
 /*  1090 */    95,  100,   97,
];
const YY_SHIFT_USE_DFLT: i32 = -7;
const YY_SHIFT_COUNT: i32 = 133;
const YY_SHIFT_MIN: i32 = -6;
const YY_SHIFT_MAX: i32 = 725;
const YY_SHIFT_OFST: [i16; 134] = [
 /*     0 */    -5,   -5,   -5,  217,  162,  272,  272,  272,  272,  107,
 /*    10 */    51,  272,  272,  272,  272,  272,  272,  272,  272,  272,
 /*    20 */   272,  272,  272,  272,  272,  272,  272,  272,  272,  272,
 /*    30 */   272,  272,  272,  272,  659,  659,  176,  286,  286,  109,
 /*    40 */    70,   17,   17,  312,  312,  483,  483,  483,  506,  483,
 /*    50 */   135,  135,  135,   72,    3,  236,  261,  236,  242,  292,
 /*    60 */   279,  236,  279,  255,  233,  233,  255,  209,  209,  236,
 /*    70 */   196,  352,  323,  458,  433,  410,  381,  565,  565,  565,
 /*    80 */   565,  651,  651,  702,  702,  725,  725,  725,  725,  677,
 /*    90 */   677,   -6,   -6,   64,  207,  240,  239,  238,  234,  205,
 /*   100 */   203,  226,  218,  230,  212,  198,  175,  195,  188,  190,
 /*   110 */   180,  181,  186,  177,  141,  123,  157,  152,  143,  134,
 /*   120 */    97,  101,   75,  119,   44,   97,   97,   65,   95,   59,
 /*   130 */    45,   12,   24,   22,
];
const YY_REDUCE_USE_DFLT: i32 = -74;
const YY_REDUCE_COUNT: i32 = 70;
const YY_REDUCE_MIN: i32 = -73;
const YY_REDUCE_MAX: i32 = 995;
const YY_REDUCE_OFST: [i16; 71] = [
 /*     0 */   477,  563,  535,  159,  104,   50,   37,   -7,  -19,  703,
 /*    10 */   995,  976,  964,  946,  934,  922,  903,  891,  873,  861,
 /*    20 */   849,  830,  818,  800,  788,  776,  757,  745,  727,  715,
 /*    30 */   703,  684,  672,  211,  -14,  -26,  -61,  -66,   13,   88,
 /*    40 */   -50,   74,   55,  -62,  -73,  271,  270,  267,  253,  247,
 /*    50 */   231,  229,  224,  246,  228,  245,  215,  237,  225,  214,
 /*    60 */   221,  223,  213,  219,  192,  185,  191,  187,  178,  184,
 /*    70 */   161,
];
const YY_DEFAULT: [YYACTIONTYPE; 212] = [
 /*     0 */   213,  213,  213,  325,  325,  313,  313,  325,  325,  325,
 /*    10 */   325,  325,  325,  325,  325,  325,  325,  325,  325,  325,
 /*    20 */   325,  325,  325,  325,  325,  325,  325,  325,  325,  325,
 /*    30 */   325,  325,  325,  325,  325,  325,  325,  325,  325,  320,
 /*    40 */   325,  320,  320,  226,  226,  325,  325,  325,  325,  325,
 /*    50 */   325,  325,  325,  325,  325,  325,  325,  325,  325,  252,
 /*    60 */   245,  325,  245,  234,  237,  237,  234,  325,  266,  325,
 /*    70 */   325,  325,  325,  325,  325,  314,  317,  230,  229,  222,
 /*    80 */   217,  292,  291,  282,  290,  298,  297,  296,  295,  294,
 /*    90 */   293,  285,  286,  325,  325,  325,  325,  246,  325,  325,
 /*   100 */   325,  325,  235,  325,  325,  325,  325,  325,  325,  325,
 /*   110 */   325,  325,  325,  325,  325,  325,  325,  325,  325,  325,
 /*   120 */   287,  325,  325,  325,  325,  289,  288,  325,  277,  325,
 /*   130 */   325,  299,  325,  325,  259,  263,  262,  255,  254,  250,
 /*   140 */   253,  251,  249,  248,  247,  244,  236,  238,  233,  232,
 /*   150 */   231,  228,  227,  225,  224,  223,  316,  318,  312,  315,
 /*   160 */   302,  301,  300,  324,  323,  322,  321,  319,  311,  310,
 /*   170 */   309,  308,  307,  306,  305,  304,  303,  284,  281,  280,
 /*   180 */   276,  278,  275,  274,  273,  272,  271,  270,  269,  268,
 /*   190 */   267,  265,  264,  221,  220,  219,  218,  261,  260,  258,
 /*   200 */   257,  256,  243,  242,  241,  240,  239,  283,  279,  216,
 /*   210 */   215,  214,
];

/* TMPL: fallback tokens */

const YY_FALLBACK: [i32; 0] = [
];

/* TMPL: symbol names */


/* TMPL: rules */


/* TMPL: destructors */


/* TMPL: stack-overflow */


/* TMPL: stack-overflow */

const YY_RULE_INFO: [YYCODETYPE; 113] = [
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
  81,
  82,
  82,
  83,
  85,
  84,
  84,
  65,
  66,
  66,
  67,
  67,
  67,
  90,
  90,
  89,
  89,
  89,
  89,
  89,
  70,
  71,
  71,
  71,
  74,
  74,
  74,
  75,
  75,
  75,
  76,
  76,
  88,
  72,
  72,
  72,
  88,
  88,
  92,
  92,
  88,
  77,
  77,
  78,
  78,
  78,
  78,
  78,
  78,
  78,
  79,
  79,
  79,
  80,
  80,
  88,
  88,
  88,
  88,
  88,
  88,
  88,
  88,
  88,
  88,
  88,
  88,
  88,
  88,
  88,
  88,
  88,
  88,
  88,
  88,
  88,
  86,
  86,
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
  93,
  94,
  94,
  94,
  95,
  96,
  96,
  97,
  98,
  98,
  98,
  99,
  99,
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
 (YYMinorType::YY24(yy0),) => {

	if list::is_empty(&yy0) {
		panic!("null program");
	}
	// ignore yyres, it doesn't really go anywhere for program
	yyres = Ast::Nothing;
	// we're done, so put yy0 in extra
	self.extra = Ok(Ast::ReplRoot(yy0));

},    _ => unreachable!() };
 YYMinorType::YY197(yyres)
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
 YYMinorType::YY24(yyres)
}
            ,
            2 /* stmts ::= stmt stmts */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY24(yy0),YYMinorType::YY24(yy1),) => {

    verbose_out!("found new stmt: {:?}\n", yy0);
	yyres = list::cons(yy0, yy1);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
          | 99 /* term ::= strexpr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY24(yy0),) => {
 yyres = yy0; 
},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            5 /* stmt ::= expr */
          | 44 /* expr ::= call_expr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY24(yy0),) => {

    yyres = yy0;

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            10 /* stmt ::= RETURN expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY24(yy1),) => {

    yyres = sexpr::new(SexprType::Return, yy1);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY117(yy1),YYMinorType::YY24(yy2),) => {

	yyres = sexpr::new(SexprType::MatchFailed,
        list::cons(Val::id(yy1.data),
        list::cons(yy2,
        Val::Nil
        ))
    );

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY195(yy1),YYMinorType::YY24(yy2),) => {

    yyres = sexpr::def_struct(Val::Type(yy1), yy2);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            13 /* defstruct_fields ::= defstruct_field defstruct_fields */
          | 110 /* strlist ::= strlist_term strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY24(yy0),YYMinorType::YY24(yy1),) => {

	yyres = list::cons(yy0, yy1);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            14 /* defstruct_fields ::= */
          | 22 /* dfunc_args ::= */
          | 101 /* list_items ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

	yyres = list::empty();

} };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY117(yy1),YYMinorType::YY195(yy3),) => {

	yyres = sexpr::id_with_type(yy1.data, yy3);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY117(yy2),YYMinorType::YY24(yy4),) => {

vout!("found fail_stmt {:?}\n", yy2);
	yyres = sexpr::new(SexprType::Fail,
        list::cons(Val::hashtag(yy2.data),
        list::cons(yy4,
        Val::Nil,
        ))
    );

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY117(yy1),YYMinorType::YY24(yy3),) => {

	let letx =
        list::cons(Val::id(yy1.data),
        list::cons(yy3,
        Val::Nil
        ));
	yyres = sexpr::new(SexprType::Let, letx);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY117(yy1),YYMinorType::YY24(yy3),) => {

	let bind = list::cons(Val::new_str(yy1.data), list::singleton(yy3));
	yyres = sexpr::new(SexprType::Fork, bind);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            19 /* block ::= BLOCKARROW stmts */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY24(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            20 /* func_stmt ::= Func ID LPAREN dfunc_args RPAREN opt_typex block DOUBLEDASH */
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
 (YYMinorType::YY117(yy1),YYMinorType::YY24(yy3),YYMinorType::YY195(yy5),YYMinorType::YY24(yy6),) => {

	let id = Val::id(yy1.data);
	let typ = Val::Type(yy5);
	yyres = sexpr::defunc(id, yy3, typ, yy6)

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            21 /* func_stmt ::= Func ID LPAREN dfunc_args RPAREN opt_typex match_case DOUBLEDASH */
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
 (YYMinorType::YY117(yy1),YYMinorType::YY24(yy3),YYMinorType::YY195(yy5),YYMinorType::YY24(yy6),) => {

	let id = Val::id(yy1.data);
	let typ = Val::Type(yy5);
    let body = sexpr::match_expr(Val::CallParams, yy6);
	yyres = sexpr::defunc(id, yy3, typ, body)

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            23 /* dfunc_args ::= ID opt_typex */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY117(yy0),YYMinorType::YY195(yy1),) => {

	yyres = list::singleton(sexpr::id_with_type(yy0.data, yy1));

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY117(yy0),YYMinorType::YY195(yy1),YYMinorType::YY24(yy3),) => {

	yyres = list::cons(sexpr::id_with_type(yy0.data, yy1), yy3);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 YYMinorType::YY195(yyres)
}
            ,
            26 /* opt_typex ::= COLON typex */
            => 
{
let yyres :  Type ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY195(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY195(yyres)
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
 YYMinorType::YY195(yyres)
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
 YYMinorType::YY195(yyres)
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
 YYMinorType::YY195(yyres)
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
 YYMinorType::YY195(yyres)
}
            ,
            31 /* typex ::= TYPE_ID */
            => 
{
let yyres :  Type ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY117(yy0),) => {

	yyres = Type::Id(Arc::new(yy0.data));

},    _ => unreachable!() };
 YYMinorType::YY195(yyres)
}
            ,
            32 /* macro_stmt ::= MACRO ID LPAREN macro_args RPAREN block DOUBLEDASH */
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
 (YYMinorType::YY117(yy1),YYMinorType::YY24(yy3),YYMinorType::YY24(yy5),) => {

    verbose_out!("found macro {:?}\n", yy1);
    yyres = sexpr::new(SexprType::DefMacro,
        list::cons(Val::id(yy1.data),
        list::cons(yy3,
        list::cons(yy5,
        Val::Nil
    ))));

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 YYMinorType::YY24(yyres)
}
            ,
            34 /* macro_args ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY117(yy0),) => {

    yyres = list::singleton(Val::id(yy0.data));

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY117(yy0),YYMinorType::YY24(yy2),) => {

    yyres = list::cons(Val::id(yy0.data), yy2);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy1),YYMinorType::YY24(yy2),) => {

    /* if-only style */
    yyres = sexpr::ifstmt(yy1, yy2, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy1),YYMinorType::YY24(yy2),YYMinorType::YY24(yy3),) => {

    /* if-else style */
    yyres = sexpr::ifstmt(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy1),) => {

    /* case-expr style */
    yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy2),YYMinorType::YY24(yy3),YYMinorType::YY24(yy4),) => {

    yyres = sexpr::ifstmt(yy2, yy3, yy4);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy2),YYMinorType::YY24(yy3),) => {

    yyres = sexpr::ifstmt(yy2, yy3, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            41 /* else_if ::= ELSE block */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY24(yy1),) => {

    yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy1),YYMinorType::YY24(yy2),YYMinorType::YY24(yy3),) => {

    yyres = sexpr::ifstmt(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy2),) => {

    yyres = yy2;

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            45 /* call_expr ::= functerm RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY24(yy0),) => {

	verbose_out!("zero param function call!");
	yyres = sexpr::call(yy0, vec![]);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            46 /* call_expr ::= functerm expr RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY24(yy0),YYMinorType::YY24(yy1),) => {

	verbose_out!("one param function call!");
	yyres = sexpr::call(yy0, vec![yy1]);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            47 /* call_expr ::= functerm tuple_args RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY24(yy0),YYMinorType::YY24(yy1),) => {

	verbose_out!("multi param function call!");
	yyres = sexpr::call(yy0, list::to_vec(yy1));

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy0),YYMinorType::YY24(yy2),) => {

	/* yyres = Val::binaryop(yy0, yy2, D); */
	yyres = Val::Void;

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy1),) => {

    verbose_out!("parsed case expr\n");
	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy1),YYMinorType::YY24(yy2),YYMinorType::YY24(yy5),) => {

    verbose_out!("found cases base\n");
    yyres = sexpr::casex(yy1, yy2, yy5);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy1),YYMinorType::YY24(yy2),YYMinorType::YY24(yy3),) => {

    verbose_out!("found extra case\n");
    yyres = sexpr::casex(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy1),YYMinorType::YY24(yy2),) => {

    verbose_out!("parsed match expr\n");
    yyres = sexpr::match_expr(yy1, yy2);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy1),YYMinorType::YY24(yy2),YYMinorType::YY24(yy3),) => {

    verbose_out!("found cases base\n");
    yyres = sexpr::match_case(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy1),YYMinorType::YY24(yy2),) => {

    verbose_out!("parsed base match case\n");
    yyres = sexpr::match_case(yy1, yy2, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            56 /* pexpr ::= INT */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY60(yy0),) => {
 yyres = Val::Int(yy0); 
},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            57 /* pexpr ::= True */
          | 96 /* term ::= True */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Bool(true); 
} };
 YYMinorType::YY24(yyres)
}
            ,
            58 /* pexpr ::= False */
          | 97 /* term ::= False */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Bool(false); 
} };
 YYMinorType::YY24(yyres)
}
            ,
            59 /* pexpr ::= HASHTAG */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY117(yy0),) => {
 yyres = Val::Hashtag(Arc::new(yy0.data)); 
},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            60 /* pexpr ::= ID */
          | 92 /* term ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY117(yy0),) => {
 yyres = Val::id(yy0.data); 
},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 YYMinorType::YY24(yyres)
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
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy1),) => {

	yyres = Val::Tuple(vec![yy1]);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            64 /* ptuple ::= LPAREN pargs RPAREN */
          | 104 /* tuple ::= LPAREN tuple_args RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY24(yy1),) => {

	yyres = Val::tuple_from_list(yy1);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy0),YYMinorType::YY24(yy2),) => {

	yyres = list::cons(yy0,
        list::cons(yy2,
        Val::Nil
        ));

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            66 /* pargs ::= pexpr COMMA pargs */
          | 103 /* list_items ::= expr COMMA list_items */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY24(yy0),YYMinorType::YY24(yy2),) => {

	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy0),YYMinorType::YY117(yy2),) => {

    yyres = sexpr::new(SexprType::FieldAccess,
        list::cons(yy0,
        list::cons(Val::id(yy2.data),
        Val::Nil,
        ))
    );

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            70 /* expr ::= NOT expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY24(yy1),) => {

	yyres = sexpr::call(Val::id("bool_not".to_string()), vec![yy1]);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            71 /* expr ::= expr ConcatNewline */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY24(yy0),) => {

	let newline = Val::Str(Arc::new("\n".to_string()));
	let args = list::cons(yy0, list::singleton(newline));
	yyres = sexpr::new(SexprType::StrExpr, args)

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            72 /* expr ::= NEGATE term */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY24(yy1),) => {

	yyres = sexpr::call(Val::id("negate".to_string()), vec![yy1]);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy0),YYMinorType::YY24(yy2),) => {

	yyres = sexpr::binaryop("int_add".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy0),YYMinorType::YY24(yy2),) => {

	yyres = sexpr::binaryop("int_sub".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy0),YYMinorType::YY24(yy2),) => {

	yyres = sexpr::binaryop("int_mult".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy0),YYMinorType::YY24(yy2),) => {

	yyres = sexpr::binaryop("int_div".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy0),YYMinorType::YY24(yy2),) => {

	yyres = sexpr::binaryop("int_mod".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy0),YYMinorType::YY24(yy2),) => {

	yyres = sexpr::binaryop("and".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy0),YYMinorType::YY24(yy2),) => {

	yyres = sexpr::binaryop("or".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy0),YYMinorType::YY24(yy2),) => {

	yyres = sexpr::binaryop("xor".to_string(),yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy0),YYMinorType::YY24(yy2),) => {

	yyres = sexpr::binaryop("less_than".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy0),YYMinorType::YY24(yy2),) => {

	yyres = sexpr::binaryop("less_than_equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy0),YYMinorType::YY24(yy2),) => {

	yyres = sexpr::binaryop("greater_than".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy0),YYMinorType::YY24(yy2),) => {

	yyres = sexpr::binaryop("greater_than_equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy0),YYMinorType::YY24(yy2),) => {

	yyres = sexpr::binaryop("equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy0),YYMinorType::YY24(yy2),) => {

	let eq = sexpr::binaryop("equal".to_string(), yy0, yy2);
	yyres = sexpr::call(Val::id("bool_not".to_string()), vec![eq]);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            88 /* functerm ::= CALL_ID */
          | 89 /* functerm ::= CALL_TYPE_ID */
          | 111 /* strlist_term ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY117(yy0),) => {

    yyres = Val::id(yy0.data);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            90 /* term ::= LPAREN expr RPAREN */
          | 100 /* list ::= SquareL list_items SquareR */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY24(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            91 /* term ::= typex */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY195(yy0),) => {

    yyres = Val::Type(yy0);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            93 /* term ::= VOID */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Val::Void;

} };
 YYMinorType::YY24(yyres)
}
            ,
            94 /* term ::= DollarQuestion */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Val::id("$".to_string());

} };
 YYMinorType::YY24(yyres)
}
            ,
            95 /* term ::= INT */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY60(yy0),) => {

	yyres = Val::Int(yy0);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            98 /* term ::= HASHTAG */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY117(yy0),) => {

	yyres = Val::Hashtag(Arc::new(yy0.data));

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            102 /* list_items ::= expr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY24(yy0),) => {

	yyres = list::singleton(yy0);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            105 /* tuple_args ::= expr COMMA expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY24(yy0),YYMinorType::YY24(yy2),) => {

	vout!("base tuple args!");
	yyres = list::cons(yy0, list::singleton(yy2));

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            106 /* tuple_args ::= expr COMMA tuple_args */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY24(yy0),YYMinorType::YY24(yy2),) => {

	vout!("additional tuple arg!");
	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            107 /* strexpr ::= StrOpen strlist StrClose */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY24(yy1),) => {

	yyres = sexpr::strexpr(yy1);
    vout!("strexpr({:?})\n", yyres);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            108 /* strlist ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

	yyres = Val::Nil;

} };
 YYMinorType::YY24(yyres)
}
            ,
            109 /* strlist ::= StrLit strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY4(yy0),YYMinorType::YY24(yy1),) => {

	yyres = list::cons(Val::new_str(yy0), yy1);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            112 /* strlist_term ::= strlist_term DOT ID */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY24(yy0),YYMinorType::YY117(yy2),) => {

    yyres = sexpr::new(SexprType::FieldAccess,
        list::cons(yy0,
        list::cons(Val::id(yy2.data),
        Val::Nil,
    )))

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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

