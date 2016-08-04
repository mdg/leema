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
const YYNOCODE: i32 = 104;
type YYACTIONTYPE = u16;
const YYWILDCARD: YYCODETYPE = 1;
enum YYMinorType {
    YY0,
    YY13(TokenData<String>),
    YY36(String),
    YY37(Val),
    YY72(TokenLoc),
    YY79(Type),
    YY101(Ast),
    YY190(i64),
}
const YYNSTATE: i32 = 203;
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
    ELSE( TokenLoc ), //3
    HASHTAG( TokenData<String> ), //4
    ID( TokenData<String> ), //5
    INT( i64 ), //6
    PLUS( TokenLoc ), //7
    SLASH( TokenLoc ), //8
    StrLit( String ), //9
    TYPE_ID( TokenData<String> ), //10
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
    STRUCT, //34
    DOUBLEDASH, //35
    DOT, //36
    COLON, //37
    FAIL, //38
    Let, //39
    Fork, //40
    DollarGT, //41
    Func, //42
    TYPE_INT, //43
    TYPE_STR, //44
    TYPE_BOOL, //45
    TYPE_VOID, //46
    MACRO, //47
    IF, //48
    PIPE, //49
    DOLLAR, //50
    CASE, //51
    MATCH, //52
    True, //53
    False, //54
    UNDERSCORE, //55
    VOID, //56
    DollarQuestion, //57
    SquareL, //58
    SquareR, //59
    StrOpen, //60
    StrClose, //61
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
pub const TOKEN_STRUCT: i32 = 34;
pub const TOKEN_DOUBLEDASH: i32 = 35;
pub const TOKEN_DOT: i32 = 36;
pub const TOKEN_COLON: i32 = 37;
pub const TOKEN_FAIL: i32 = 38;
pub const TOKEN_Let: i32 = 39;
pub const TOKEN_Fork: i32 = 40;
pub const TOKEN_DollarGT: i32 = 41;
pub const TOKEN_Func: i32 = 42;
pub const TOKEN_TYPE_INT: i32 = 43;
pub const TOKEN_TYPE_STR: i32 = 44;
pub const TOKEN_TYPE_BOOL: i32 = 45;
pub const TOKEN_TYPE_VOID: i32 = 46;
pub const TOKEN_MACRO: i32 = 47;
pub const TOKEN_IF: i32 = 48;
pub const TOKEN_PIPE: i32 = 49;
pub const TOKEN_DOLLAR: i32 = 50;
pub const TOKEN_CASE: i32 = 51;
pub const TOKEN_MATCH: i32 = 52;
pub const TOKEN_True: i32 = 53;
pub const TOKEN_False: i32 = 54;
pub const TOKEN_UNDERSCORE: i32 = 55;
pub const TOKEN_VOID: i32 = 56;
pub const TOKEN_DollarQuestion: i32 = 57;
pub const TOKEN_SquareL: i32 = 58;
pub const TOKEN_SquareR: i32 = 59;
pub const TOKEN_StrOpen: i32 = 60;
pub const TOKEN_StrClose: i32 = 61;
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
        &Token::STRUCT => TOKEN_STRUCT,
        &Token::DOUBLEDASH => TOKEN_DOUBLEDASH,
        &Token::DOT => TOKEN_DOT,
        &Token::COLON => TOKEN_COLON,
        &Token::FAIL => TOKEN_FAIL,
        &Token::Let => TOKEN_Let,
        &Token::Fork => TOKEN_Fork,
        &Token::DollarGT => TOKEN_DollarGT,
        &Token::Func => TOKEN_Func,
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
        &Token::True => TOKEN_True,
        &Token::False => TOKEN_False,
        &Token::UNDERSCORE => TOKEN_UNDERSCORE,
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
        Token::COMMA(x) => YYMinorType::YY72(x),
        Token::ELSE(x) => YYMinorType::YY72(x),
        Token::HASHTAG(x) => YYMinorType::YY13(x),
        Token::ID(x) => YYMinorType::YY13(x),
        Token::INT(x) => YYMinorType::YY190(x),
        Token::PLUS(x) => YYMinorType::YY72(x),
        Token::SLASH(x) => YYMinorType::YY72(x),
        Token::StrLit(x) => YYMinorType::YY36(x),
        Token::TYPE_ID(x) => YYMinorType::YY13(x),
        _ => YYMinorType::YY0
  }
}
const YY_ACTTAB_COUNT: i32 = 910;
const YY_ACTION: [YYACTIONTYPE; 910] = [
 /*     0 */   175,  181,  178,   94,   71,  182,  187,  114,  166,  157,
 /*    10 */   155,  129,  154,  111,  174,   15,   53,   94,   75,  182,
 /*    20 */    54,  128,   14,   35,  155,    8,  154,  130,  174,  203,
 /*    30 */    50,   93,  166,  113,  123,  117,  110,   12,  108,  191,
 /*    40 */   190,  189,  188,  102,    3,   29,  204,   70,   16,  177,
 /*    50 */   176,  194,  180,  179,    6,  124,   55,  175,  181,  178,
 /*    60 */    94,   74,  182,  187,   30,   28,    4,  155,   34,  154,
 /*    70 */   150,  174,   15,   36,   94,   72,  182,  105,  186,   14,
 /*    80 */   182,  155,    8,  154,   95,  174,   51,   50,  104,  174,
 /*    90 */     4,  123,  117,  110,   12,  108,  191,  190,  189,  188,
 /*   100 */   102,    3,  126,   41,   70,   16,  177,  176,   49,  180,
 /*   110 */   179,    6,  187,   55,   58,  175,  181,  178,  141,   69,
 /*   120 */   166,  187,  169,  173,   94,   73,  182,   94,   92,  182,
 /*   130 */    15,  155,  112,  154,  155,  174,  154,   14,  174,  116,
 /*   140 */     8,    1,   94,   73,  182,  191,  190,  189,  188,  155,
 /*   150 */   152,  154,   17,  174,  191,  190,  189,  188,   38,  162,
 /*   160 */   161,  165,   70,   16,  177,  176,    1,  180,  179,    6,
 /*   170 */   168,   55,   56,  175,  181,  178,  183,   11,  182,  187,
 /*   180 */    32,   29,  193,   41,   37,  159,  158,  174,   15,   38,
 /*   190 */   156,  149,  151,  109,  170,   14,  182,   13,    8,   31,
 /*   200 */    30,   28,    4,   39,  107,  174,   66,   67,  164,  163,
 /*   210 */   160,   64,  191,  190,  189,  188,  147,  162,  161,  165,
 /*   220 */    70,   16,  177,  176,  146,  180,  179,    6,   96,   55,
 /*   230 */   175,  181,  178,  101,   63,  143,  187,   62,   61,   94,
 /*   240 */    45,  182,   37,  140,  131,   15,  155,  137,  154,   68,
 /*   250 */   174,  118,   14,   18,    1,    8,  132,  167,   65,   39,
 /*   260 */   106,   40,   48,  103,  144,  100,  164,  163,  160,  191,
 /*   270 */   190,  189,  188,   99,   98,   51,  142,   70,   16,  177,
 /*   280 */   176,   10,  180,  179,    6,  119,   55,  175,  181,  178,
 /*   290 */    94,   91,  182,  187,  139,    9,  136,  155,  135,  154,
 /*   300 */   134,  174,   15,   97,   94,  122,  182,  138,  133,   14,
 /*   310 */    57,  155,    8,  154,   42,  174,  115,  172,   52,  192,
 /*   320 */   171,   60,   59,  125,  145,  316,  191,  190,  189,  188,
 /*   330 */   316,  316,   10,  316,   70,   16,  177,  176,  316,  180,
 /*   340 */   179,    6,  316,   55,  175,  181,  178,  316,  316,  316,
 /*   350 */   187,  316,  316,  316,  316,  316,   94,  121,  182,   15,
 /*   360 */   316,  316,  316,  155,  316,  154,   14,  174,  316,    8,
 /*   370 */   316,  316,  316,  316,  316,  316,  316,  316,  316,  316,
 /*   380 */   316,  316,  316,  191,  190,  189,  188,  316,  316,  316,
 /*   390 */   316,   70,   16,  177,  176,  316,  180,  179,    6,    7,
 /*   400 */    55,  316,  316,  316,   32,   29,  316,  316,  316,  316,
 /*   410 */   316,  316,   26,   25,   27,  184,  316,   20,   19,   22,
 /*   420 */    21,   24,   23,   31,   30,   28,    4,  185,    7,   94,
 /*   430 */   120,  182,  316,   32,   29,  316,  155,  316,  154,  316,
 /*   440 */   174,   26,   25,   27,  184,  316,   20,   19,   22,   21,
 /*   450 */    24,   23,   31,   30,   28,    4,  153,   32,   29,  316,
 /*   460 */   316,  316,  316,  316,  316,   26,   25,   27,  184,  316,
 /*   470 */    20,   19,   22,   21,   24,   23,   31,   30,   28,    4,
 /*   480 */   153,    7,   94,   83,  182,  316,   32,   29,  316,  155,
 /*   490 */   316,  154,  316,  174,   26,   25,   27,  184,  316,   20,
 /*   500 */    19,   22,   21,   24,   23,   31,   30,   28,    4,    5,
 /*   510 */    94,   81,  182,  316,   32,   29,  316,  155,  316,  154,
 /*   520 */   316,  174,   26,   25,   27,  184,  316,   20,   19,   22,
 /*   530 */    21,   24,   23,   31,   30,   28,    4,   32,   29,  316,
 /*   540 */   316,  316,    1,  316,  316,   26,   25,   27,  184,  316,
 /*   550 */    20,   19,   22,   21,   24,   23,   31,   30,   28,    4,
 /*   560 */    32,   29,  316,  316,  316,  316,  316,  316,   26,   25,
 /*   570 */    27,  184,  316,   20,   19,   22,   21,   24,   23,   31,
 /*   580 */    30,   28,    4,  316,  316,   94,   80,  182,  316,  316,
 /*   590 */    32,   29,  155,  316,  154,  316,  174,  316,   26,   25,
 /*   600 */    27,  184,   39,   20,   19,   22,   21,   24,   23,   31,
 /*   610 */    30,   28,    4,  316,  315,  127,    2,  316,  316,  197,
 /*   620 */    94,   89,  182,  196,  316,  316,  195,  155,  316,  154,
 /*   630 */   316,  174,  316,  201,  316,  316,  200,  199,  198,   94,
 /*   640 */    77,  182,  316,  202,    2,  316,  155,  197,  154,  316,
 /*   650 */   174,  196,  316,  316,  195,  316,  316,  316,  316,  316,
 /*   660 */   316,  201,  316,  316,  200,  199,  198,   94,   77,  182,
 /*   670 */   316,  148,    2,  316,  155,  197,  154,  316,  174,  196,
 /*   680 */   316,  316,  195,  316,  175,  181,  178,  316,  316,  201,
 /*   690 */   187,  316,  200,  199,  198,   94,   77,  182,  316,  316,
 /*   700 */    32,   29,  155,  316,  154,  316,  174,  316,  316,   33,
 /*   710 */    27,  184,  316,   20,   19,   22,   21,   24,   23,   31,
 /*   720 */    30,   28,    4,  191,  190,  189,  188,  316,   32,   29,
 /*   730 */   316,  316,  316,  177,  176,  316,  180,  179,  316,  184,
 /*   740 */    55,   20,   19,   22,   21,   24,   23,   31,   30,   28,
 /*   750 */     4,   32,   29,  316,  316,  316,  316,  316,  316,  316,
 /*   760 */   316,  316,  316,  316,  314,  314,  314,  314,   24,   23,
 /*   770 */    31,   30,   28,    4,   94,   88,  182,  316,  316,  316,
 /*   780 */   316,  155,  316,  154,  316,  174,   94,   87,  182,   94,
 /*   790 */    86,  182,  316,  155,  316,  154,  155,  174,  154,  316,
 /*   800 */   174,  316,   94,   85,  182,   94,   84,  182,  316,  155,
 /*   810 */   316,  154,  155,  174,  154,  316,  174,   94,   47,  182,
 /*   820 */   316,  316,  316,  316,  155,  316,  154,  316,  174,   94,
 /*   830 */    79,  182,  316,  316,  316,  316,  155,  316,  154,  316,
 /*   840 */   174,   94,   46,  182,  316,  316,  316,  316,  155,  316,
 /*   850 */   154,  316,  174,   94,   82,  182,   94,   90,  182,  316,
 /*   860 */   155,  316,  154,  155,  174,  154,  316,  174,  316,  316,
 /*   870 */    94,   78,  182,  316,  316,  316,  316,  155,  316,  154,
 /*   880 */   316,  174,   94,   76,  182,   94,   44,  182,  316,  155,
 /*   890 */   316,  154,  155,  174,  154,  316,  174,  316,   94,   43,
 /*   900 */   182,  316,  316,  316,  316,  155,  316,  154,  316,  174,
];
const YY_LOOKAHEAD: [YYCODETYPE; 910] = [
 /*     0 */     4,    5,    6,   88,   89,   90,   10,   79,   80,   81,
 /*    10 */    95,   32,   97,   98,   99,   19,    5,   88,   89,   90,
 /*    20 */     9,   33,   26,    5,   95,   29,   97,   31,   99,    0,
 /*    30 */    34,   79,   80,   81,   38,   39,   40,   41,   42,   43,
 /*    40 */    44,   45,   46,   47,   48,    8,    0,   51,   52,   53,
 /*    50 */    54,   35,   56,   57,   58,    5,   60,    4,    5,    6,
 /*    60 */    88,   89,   90,   10,   27,   28,   29,   95,   50,   97,
 /*    70 */    98,   99,   19,    4,   88,   89,   90,   67,   88,   26,
 /*    80 */    90,   95,   29,   97,   98,   99,    3,   34,   78,   99,
 /*    90 */    29,   38,   39,   40,   41,   42,   43,   44,   45,   46,
 /*   100 */    47,   48,   83,   84,   51,   52,   53,   54,   37,   56,
 /*   110 */    57,   58,   10,   60,    3,    4,    5,    6,   35,   79,
 /*   120 */    80,   10,   35,   61,   88,   89,   90,   88,   89,   90,
 /*   130 */    19,   95,   96,   97,   95,   99,   97,   26,   99,    5,
 /*   140 */    29,   12,   88,   89,   90,   43,   44,   45,   46,   95,
 /*   150 */    96,   97,   20,   99,   43,   44,   45,   46,    2,    4,
 /*   160 */     5,    6,   51,   52,   53,   54,   12,   56,   57,   58,
 /*   170 */    35,   60,    3,    4,    5,    6,   88,   48,   90,   10,
 /*   180 */     7,    8,   83,   84,   29,   30,   30,   99,   19,    2,
 /*   190 */    30,   30,   59,    5,   88,   26,   90,   20,   29,   26,
 /*   200 */    27,   28,   29,   49,    5,   99,   30,   29,   53,   54,
 /*   210 */    55,    2,   43,   44,   45,   46,   35,    4,    5,    6,
 /*   220 */    51,   52,   53,   54,   35,   56,   57,   58,   77,   60,
 /*   230 */     4,    5,    6,    5,   29,   35,   10,   30,    2,   88,
 /*   240 */    89,   90,   29,   35,   30,   19,   95,   35,   97,   67,
 /*   250 */    99,   93,   26,   49,   12,   29,   30,   78,    5,   49,
 /*   260 */    69,   91,   37,   91,   69,   73,   53,   54,   55,   43,
 /*   270 */    44,   45,   46,   67,    5,    3,   73,   51,   52,   53,
 /*   280 */    54,   49,   56,   57,   58,  100,   60,    4,    5,    6,
 /*   290 */    88,   89,   90,   10,   76,   49,   77,   95,   67,   97,
 /*   300 */    67,   99,   19,   76,   88,   89,   90,   67,   93,   26,
 /*   310 */    67,   95,   29,   97,   90,   99,   78,  100,   67,   90,
 /*   320 */   100,   67,   67,   36,   90,  103,   43,   44,   45,   46,
 /*   330 */   103,  103,   49,  103,   51,   52,   53,   54,  103,   56,
 /*   340 */    57,   58,  103,   60,    4,    5,    6,  103,  103,  103,
 /*   350 */    10,  103,  103,  103,  103,  103,   88,   89,   90,   19,
 /*   360 */   103,  103,  103,   95,  103,   97,   26,   99,  103,   29,
 /*   370 */   103,  103,  103,  103,  103,  103,  103,  103,  103,  103,
 /*   380 */   103,  103,  103,   43,   44,   45,   46,  103,  103,  103,
 /*   390 */   103,   51,   52,   53,   54,  103,   56,   57,   58,    2,
 /*   400 */    60,  103,  103,  103,    7,    8,  103,  103,  103,  103,
 /*   410 */   103,  103,   15,   16,   17,   18,  103,   20,   21,   22,
 /*   420 */    23,   24,   25,   26,   27,   28,   29,   30,    2,   88,
 /*   430 */    89,   90,  103,    7,    8,  103,   95,  103,   97,  103,
 /*   440 */    99,   15,   16,   17,   18,  103,   20,   21,   22,   23,
 /*   450 */    24,   25,   26,   27,   28,   29,   30,    7,    8,  103,
 /*   460 */   103,  103,  103,  103,  103,   15,   16,   17,   18,  103,
 /*   470 */    20,   21,   22,   23,   24,   25,   26,   27,   28,   29,
 /*   480 */    30,    2,   88,   89,   90,  103,    7,    8,  103,   95,
 /*   490 */   103,   97,  103,   99,   15,   16,   17,   18,  103,   20,
 /*   500 */    21,   22,   23,   24,   25,   26,   27,   28,   29,    2,
 /*   510 */    88,   89,   90,  103,    7,    8,  103,   95,  103,   97,
 /*   520 */   103,   99,   15,   16,   17,   18,  103,   20,   21,   22,
 /*   530 */    23,   24,   25,   26,   27,   28,   29,    7,    8,  103,
 /*   540 */   103,  103,   12,  103,  103,   15,   16,   17,   18,  103,
 /*   550 */    20,   21,   22,   23,   24,   25,   26,   27,   28,   29,
 /*   560 */     7,    8,  103,  103,  103,  103,  103,  103,   15,   16,
 /*   570 */    17,   18,  103,   20,   21,   22,   23,   24,   25,   26,
 /*   580 */    27,   28,   29,  103,  103,   88,   89,   90,  103,  103,
 /*   590 */     7,    8,   95,  103,   97,  103,   99,  103,   15,   16,
 /*   600 */    17,   18,   49,   20,   21,   22,   23,   24,   25,   26,
 /*   610 */    27,   28,   29,  103,   63,   64,   65,  103,  103,   68,
 /*   620 */    88,   89,   90,   72,  103,  103,   75,   95,  103,   97,
 /*   630 */   103,   99,  103,   82,  103,  103,   85,   86,   87,   88,
 /*   640 */    89,   90,  103,   64,   65,  103,   95,   68,   97,  103,
 /*   650 */    99,   72,  103,  103,   75,  103,  103,  103,  103,  103,
 /*   660 */   103,   82,  103,  103,   85,   86,   87,   88,   89,   90,
 /*   670 */   103,   64,   65,  103,   95,   68,   97,  103,   99,   72,
 /*   680 */   103,  103,   75,  103,    4,    5,    6,  103,  103,   82,
 /*   690 */    10,  103,   85,   86,   87,   88,   89,   90,  103,  103,
 /*   700 */     7,    8,   95,  103,   97,  103,   99,  103,  103,   29,
 /*   710 */    17,   18,  103,   20,   21,   22,   23,   24,   25,   26,
 /*   720 */    27,   28,   29,   43,   44,   45,   46,  103,    7,    8,
 /*   730 */   103,  103,  103,   53,   54,  103,   56,   57,  103,   18,
 /*   740 */    60,   20,   21,   22,   23,   24,   25,   26,   27,   28,
 /*   750 */    29,    7,    8,  103,  103,  103,  103,  103,  103,  103,
 /*   760 */   103,  103,  103,  103,   20,   21,   22,   23,   24,   25,
 /*   770 */    26,   27,   28,   29,   88,   89,   90,  103,  103,  103,
 /*   780 */   103,   95,  103,   97,  103,   99,   88,   89,   90,   88,
 /*   790 */    89,   90,  103,   95,  103,   97,   95,   99,   97,  103,
 /*   800 */    99,  103,   88,   89,   90,   88,   89,   90,  103,   95,
 /*   810 */   103,   97,   95,   99,   97,  103,   99,   88,   89,   90,
 /*   820 */   103,  103,  103,  103,   95,  103,   97,  103,   99,   88,
 /*   830 */    89,   90,  103,  103,  103,  103,   95,  103,   97,  103,
 /*   840 */    99,   88,   89,   90,  103,  103,  103,  103,   95,  103,
 /*   850 */    97,  103,   99,   88,   89,   90,   88,   89,   90,  103,
 /*   860 */    95,  103,   97,   95,   99,   97,  103,   99,  103,  103,
 /*   870 */    88,   89,   90,  103,  103,  103,  103,   95,  103,   97,
 /*   880 */   103,   99,   88,   89,   90,   88,   89,   90,  103,   95,
 /*   890 */   103,   97,   95,   99,   97,  103,   99,  103,   88,   89,
 /*   900 */    90,  103,  103,  103,  103,   95,  103,   97,  103,   99,
];
const YY_SHIFT_USE_DFLT: i32 = -22;
const YY_SHIFT_COUNT: i32 = 130;
const YY_SHIFT_MIN: i32 = -21;
const YY_SHIFT_MAX: i32 = 744;
const YY_SHIFT_OFST: [i16; 131] = [
 /*     0 */    -4,   53,   53,  283,  226,  340,  340,  340,  340,  169,
 /*    10 */   111,  340,  340,  340,  340,  340,  340,  340,  340,  340,
 /*    20 */   340,  340,  340,  340,  340,  340,  340,  340,  340,  340,
 /*    30 */   340,  340,  340,  340,  680,  680,  680,  155,  213,  213,
 /*    40 */   154,  287,  287,  530,  530,  530,  553,  530,  102,  102,
 /*    50 */   102,  129,   83,   11,   11,   11,  242,  246,  242,  232,
 /*    60 */   272,  269,  242,  269,  253,  225,  225,  253,  210,  242,
 /*    70 */   204,  426,  397,  507,  479,  450,  583,  583,  583,  583,
 /*    80 */   693,  693,  721,  721,  744,  744,  744,  744,  173,  173,
 /*    90 */    37,   37,   37,  156,   18,  214,  212,  208,  236,  200,
 /*   100 */   207,  205,  228,  209,  189,  181,  176,  178,  199,  177,
 /*   110 */   188,  161,  133,  160,  187,  135,  132,  134,   87,   62,
 /*   120 */    61,   61,   61,   69,   71,   50,   16,   46,   29,  -12,
 /*   130 */   -21,
];
const YY_REDUCE_USE_DFLT: i32 = -86;
const YY_REDUCE_COUNT: i32 = 70;
const YY_REDUCE_MIN: i32 = -85;
const YY_REDUCE_MAX: i32 = 810;
const YY_REDUCE_OFST: [i16; 71] = [
 /*     0 */   551,  607,  579,  151,  -14,   54,   36,  -28,  -85,  729,
 /*    10 */   810,  797,  794,  782,  768,  765,  753,  741,  729,  717,
 /*    20 */   714,  701,  698,  686,  532,  497,  422,  394,  341,  268,
 /*    30 */   216,  202,   39,  -71,  106,   88,  -10,  -48,  -72,   40,
 /*    40 */    10,   99,   19,  255,  254,  251,  238,  243,  234,  229,
 /*    50 */   224,  240,  227,  220,  217,  185,  233,  215,  231,  219,
 /*    60 */   218,  203,  206,  192,  195,  172,  170,  191,  179,  182,
 /*    70 */   158,
];
const YY_DEFAULT: [YYACTIONTYPE; 203] = [
 /*     0 */   205,  205,  205,  314,  314,  304,  304,  314,  314,  314,
 /*    10 */   314,  314,  314,  314,  314,  314,  314,  314,  314,  314,
 /*    20 */   314,  314,  314,  314,  314,  314,  314,  314,  314,  314,
 /*    30 */   314,  314,  314,  314,  314,  314,  314,  314,  314,  314,
 /*    40 */   314,  216,  216,  314,  314,  314,  314,  314,  314,  314,
 /*    50 */   314,  314,  314,  311,  311,  311,  314,  314,  314,  314,
 /*    60 */   244,  237,  314,  237,  226,  229,  229,  226,  258,  314,
 /*    70 */   314,  314,  314,  305,  308,  314,  222,  221,  220,  219,
 /*    80 */   283,  282,  273,  281,  289,  288,  287,  286,  285,  284,
 /*    90 */   275,  277,  276,  314,  290,  314,  314,  314,  238,  314,
 /*   100 */   314,  314,  314,  227,  314,  314,  314,  314,  314,  314,
 /*   110 */   314,  314,  314,  314,  269,  314,  314,  314,  314,  314,
 /*   120 */   280,  279,  278,  314,  314,  314,  314,  314,  314,  314,
 /*   130 */   314,  250,  248,  255,  254,  247,  246,  242,  245,  243,
 /*   140 */   241,  240,  239,  236,  228,  230,  225,  224,  223,  307,
 /*   150 */   309,  303,  306,  291,  272,  271,  268,  270,  267,  266,
 /*   160 */   265,  264,  263,  262,  261,  260,  259,  257,  256,  253,
 /*   170 */   252,  313,  312,  310,  300,  299,  298,  297,  296,  295,
 /*   180 */   294,  293,  292,  251,  274,  249,  218,  235,  234,  233,
 /*   190 */   232,  231,  217,  215,  214,  213,  212,  211,  210,  209,
 /*   200 */   208,  207,  206,
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
  63,
  63,
  64,
  64,
  65,
  65,
  65,
  65,
  65,
  65,
  65,
  82,
  83,
  83,
  84,
  87,
  85,
  85,
  86,
  86,
  67,
  68,
  68,
  69,
  69,
  69,
  91,
  91,
  90,
  90,
  90,
  90,
  90,
  72,
  73,
  73,
  73,
  75,
  75,
  75,
  76,
  76,
  76,
  77,
  77,
  89,
  89,
  89,
  89,
  89,
  89,
  93,
  93,
  89,
  78,
  78,
  79,
  79,
  79,
  79,
  79,
  79,
  79,
  80,
  80,
  80,
  81,
  81,
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
  94,
  94,
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
 YYMinorType::YY101(yyres)
}
            ,
            1 /* program ::= stmts */
            => 
{
let yyres :  Ast ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY37(yy0),) => {

	if list::is_empty(&yy0) {
		panic!("null program");
	}
	// ignore yyres, it doesn't really go anywhere for program
	yyres = Ast::Nothing;
	// we're done, so put yy0 in extra
	self.extra = Ok(Ast::ReplRoot(yy0));

},    _ => unreachable!() };
 YYMinorType::YY101(yyres)
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
 YYMinorType::YY37(yyres)
}
            ,
            3 /* stmts ::= stmt stmts */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY37(yy0),YYMinorType::YY37(yy1),) => {

    verbose_out!("found new stmt: {:?}\n", yy0);
	yyres = list::cons(yy0, yy1);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            4 /* stmt ::= defstruct */
          | 5 /* stmt ::= let_stmt */
          | 7 /* stmt ::= fail_stmt */
          | 8 /* stmt ::= func_stmt */
          | 9 /* stmt ::= macro_stmt */
          | 10 /* stmt ::= if_stmt */
          | 56 /* pexpr ::= ptuple */
          | 68 /* expr ::= list */
          | 69 /* expr ::= tuple */
          | 87 /* expr ::= term */
          | 97 /* term ::= strexpr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY37(yy0),) => {
 yyres = yy0; 
},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            6 /* stmt ::= expr_stmt */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY37(yy0),) => {

    yyres = yy0;

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            11 /* defstruct ::= STRUCT typex defstruct_fields DOUBLEDASH */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY79(yy1),YYMinorType::YY37(yy2),) => {

    yyres = sexpr::def_struct(Val::Type(yy1), yy2);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            12 /* defstruct_fields ::= defstruct_field defstruct_fields */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY37(yy0),YYMinorType::YY37(yy1),) => {

	yyres = list::cons(yy0, yy1);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            13 /* defstruct_fields ::= */
          | 23 /* dfunc_args ::= */
          | 101 /* list_items ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

	yyres = list::empty();

} };
 YYMinorType::YY37(yyres)
}
            ,
            14 /* defstruct_field ::= DOT ID COLON typex */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY13(yy1),YYMinorType::YY79(yy3),) => {

	yyres = sexpr::id_with_type(yy1.data, yy3);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            15 /* fail_stmt ::= FAIL HASHTAG term */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY13(yy1),YYMinorType::YY37(yy2),) => {

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
 YYMinorType::YY37(yyres)
}
            ,
            16 /* let_stmt ::= Let ID EQ expr */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY13(yy1),YYMinorType::YY37(yy3),) => {

	let letx =
        list::cons(Val::id(yy1.data),
        list::cons(yy3,
        Val::Nil
        ));
	yyres = sexpr::new(SexprType::Let, letx);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            17 /* let_stmt ::= Fork ID EQ expr */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY13(yy1),YYMinorType::YY37(yy3),) => {

	let bind = list::cons(Val::new_str(yy1.data), list::singleton(yy3));
	yyres = sexpr::new(SexprType::Fork, bind);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            18 /* expr_stmt ::= expr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY37(yy0),) => {

	yyres = yy0;

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            19 /* expr_stmt ::= DollarGT expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY37(yy1),) => {
 yyres = yy1; 
},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            20 /* block ::= BLOCKARROW stmts */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY37(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            21 /* func_stmt ::= Func ID LPAREN dfunc_args RPAREN opt_typex block DOUBLEDASH */
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
 (YYMinorType::YY13(yy1),YYMinorType::YY37(yy3),YYMinorType::YY79(yy5),YYMinorType::YY37(yy6),) => {

	let id = Val::id(yy1.data);
	let typ = Val::Type(yy5);
	yyres = sexpr::defunc(id, yy3, typ, yy6)

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            22 /* func_stmt ::= Func ID LPAREN dfunc_args RPAREN opt_typex match_case DOUBLEDASH */
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
 (YYMinorType::YY13(yy1),YYMinorType::YY37(yy3),YYMinorType::YY79(yy5),YYMinorType::YY37(yy6),) => {

	let id = Val::id(yy1.data);
	let typ = Val::Type(yy5);
    let body = sexpr::match_expr(Val::CallParams, yy6);
	yyres = sexpr::defunc(id, yy3, typ, body)

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            24 /* dfunc_args ::= ID opt_typex */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY13(yy0),YYMinorType::YY79(yy1),) => {

	yyres = list::singleton(sexpr::id_with_type(yy0.data, yy1));

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
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
 (YYMinorType::YY13(yy0),YYMinorType::YY79(yy1),YYMinorType::YY37(yy3),) => {

	yyres = list::cons(sexpr::id_with_type(yy0.data, yy1), yy3);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
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
 YYMinorType::YY79(yyres)
}
            ,
            27 /* opt_typex ::= COLON typex */
            => 
{
let yyres :  Type ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY79(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY79(yyres)
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
 YYMinorType::YY79(yyres)
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
 YYMinorType::YY79(yyres)
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
 YYMinorType::YY79(yyres)
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
 YYMinorType::YY79(yyres)
}
            ,
            32 /* typex ::= TYPE_ID */
            => 
{
let yyres :  Type ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY13(yy0),) => {

	yyres = Type::Id(Arc::new(yy0.data));

},    _ => unreachable!() };
 YYMinorType::YY79(yyres)
}
            ,
            33 /* macro_stmt ::= MACRO ID LPAREN macro_args RPAREN block DOUBLEDASH */
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
 (YYMinorType::YY13(yy1),YYMinorType::YY37(yy3),YYMinorType::YY37(yy5),) => {

    verbose_out!("found macro {:?}\n", yy1);
    yyres = sexpr::new(SexprType::DefMacro,
        list::cons(Val::id(yy1.data),
        list::cons(yy3,
        list::cons(yy5,
        Val::Nil
    ))));

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
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
 YYMinorType::YY37(yyres)
}
            ,
            35 /* macro_args ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY13(yy0),) => {

    yyres = list::singleton(Val::id(yy0.data));

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
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
 (YYMinorType::YY13(yy0),YYMinorType::YY37(yy2),) => {

    yyres = list::cons(Val::id(yy0.data), yy2);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            37 /* if_stmt ::= IF expr block DOUBLEDASH */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY37(yy1),YYMinorType::YY37(yy2),) => {

    /* if-only style */
    yyres = sexpr::ifstmt(yy1, yy2, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            38 /* if_stmt ::= IF expr block else_if DOUBLEDASH */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,yyp3.minor,) {
 (YYMinorType::YY37(yy1),YYMinorType::YY37(yy2),YYMinorType::YY37(yy3),) => {

    /* if-else style */
    yyres = sexpr::ifstmt(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
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
 (YYMinorType::YY37(yy1),) => {

    /* case-expr style */
    yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            40 /* else_if ::= ELSE IF expr block else_if */
            => 
{
let yyres :  Val ;
let yyp4 = self.yystack.pop().unwrap();
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp2.minor,yyp3.minor,yyp4.minor,) {
 (YYMinorType::YY37(yy2),YYMinorType::YY37(yy3),YYMinorType::YY37(yy4),) => {

    yyres = sexpr::ifstmt(yy2, yy3, yy4);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            41 /* else_if ::= ELSE IF expr block */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp2.minor,yyp3.minor,) {
 (YYMinorType::YY37(yy2),YYMinorType::YY37(yy3),) => {

    yyres = sexpr::ifstmt(yy2, yy3, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            42 /* else_if ::= ELSE block */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY37(yy1),) => {

    yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            43 /* if_case ::= PIPE expr block if_case */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,yyp3.minor,) {
 (YYMinorType::YY37(yy1),YYMinorType::YY37(yy2),YYMinorType::YY37(yy3),) => {

    yyres = sexpr::ifstmt(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            44 /* if_case ::= PIPE ELSE block */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp2.minor,) {
 (YYMinorType::YY37(yy2),) => {

    yyres = yy2;

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            45 /* expr ::= expr LPAREN RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY37(yy0),) => {

	verbose_out!("zero param function call!");
	yyres = sexpr::call(yy0, vec![]);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            46 /* expr ::= expr LPAREN expr RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY37(yy0),YYMinorType::YY37(yy2),) => {

	verbose_out!("one param function call!");
	yyres = sexpr::call(yy0, vec![yy2]);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            47 /* expr ::= expr LPAREN tuple_args RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY37(yy0),YYMinorType::YY37(yy2),) => {

	verbose_out!("multi param function call!");
	yyres = sexpr::call(yy0, list::to_vec(yy2));

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
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
 (YYMinorType::YY37(yy0),YYMinorType::YY13(yy1),YYMinorType::YY37(yy2),) => {

	yyres = sexpr::binaryop(yy1.data, yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
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
 (YYMinorType::YY37(yy0),YYMinorType::YY37(yy2),) => {

	/* yyres = Val::binaryop(yy0, yy2, D); */
	yyres = Val::Void;

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
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
 (YYMinorType::YY37(yy1),) => {

    verbose_out!("parsed case expr\n");
	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            51 /* cases ::= PIPE expr block PIPE ELSE block */
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
 (YYMinorType::YY37(yy1),YYMinorType::YY37(yy2),YYMinorType::YY37(yy5),) => {

    verbose_out!("found cases base\n");
    yyres = sexpr::casex(yy1, yy2, yy5);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            52 /* cases ::= PIPE expr block cases */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,yyp3.minor,) {
 (YYMinorType::YY37(yy1),YYMinorType::YY37(yy2),YYMinorType::YY37(yy3),) => {

    verbose_out!("found extra case\n");
    yyres = sexpr::casex(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
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
 (YYMinorType::YY37(yy1),YYMinorType::YY37(yy2),) => {

    verbose_out!("parsed match expr\n");
    yyres = sexpr::match_expr(yy1, yy2);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            54 /* match_case ::= PIPE pexpr block match_case */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,yyp3.minor,) {
 (YYMinorType::YY37(yy1),YYMinorType::YY37(yy2),YYMinorType::YY37(yy3),) => {

    verbose_out!("found cases base\n");
    yyres = sexpr::match_case(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            55 /* match_case ::= PIPE pexpr block */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY37(yy1),YYMinorType::YY37(yy2),) => {

    verbose_out!("parsed base match case\n");
    yyres = sexpr::match_case(yy1, yy2, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            57 /* pexpr ::= INT */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY190(yy0),) => {
 yyres = Val::Int(yy0); 
},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            58 /* pexpr ::= True */
          | 94 /* term ::= True */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Bool(true); 
} };
 YYMinorType::YY37(yyres)
}
            ,
            59 /* pexpr ::= False */
          | 95 /* term ::= False */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Bool(false); 
} };
 YYMinorType::YY37(yyres)
}
            ,
            60 /* pexpr ::= HASHTAG */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY13(yy0),) => {
 yyres = Val::Hashtag(Arc::new(yy0.data)); 
},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            61 /* pexpr ::= ID */
          | 90 /* term ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY13(yy0),) => {
 yyres = Val::id(yy0.data); 
},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            62 /* pexpr ::= UNDERSCORE */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Wildcard; 
} };
 YYMinorType::YY37(yyres)
}
            ,
            63 /* ptuple ::= LPAREN RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match () {
 () => {

	panic!("an empty tuple is not a valid pattern");

} };
 YYMinorType::YY37(yyres)
}
            ,
            64 /* ptuple ::= LPAREN pexpr RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY37(yy1),) => {

	yyres = Val::Tuple(vec![yy1]);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            65 /* ptuple ::= LPAREN pargs RPAREN */
          | 104 /* tuple ::= LPAREN tuple_args RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY37(yy1),) => {

	yyres = Val::tuple_from_list(yy1);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            66 /* pargs ::= pexpr COMMA pexpr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY37(yy0),YYMinorType::YY37(yy2),) => {

	yyres = list::cons(yy0,
        list::cons(yy2,
        Val::Nil
        ));

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            67 /* pargs ::= pexpr COMMA pargs */
          | 103 /* list_items ::= expr COMMA list_items */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY37(yy0),YYMinorType::YY37(yy2),) => {

	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            70 /* expr ::= NOT expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY37(yy1),) => {

	yyres = sexpr::call(Val::id("bool_not".to_string()), vec![yy1]);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            71 /* expr ::= expr ConcatNewline */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY37(yy0),) => {

	let newline = Val::Str(Arc::new("\n".to_string()));
	let args = list::cons(yy0, list::singleton(newline));
	yyres = sexpr::new(SexprType::StrExpr, args)

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            72 /* expr ::= MINUS expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY37(yy1),) => {

	yyres = sexpr::call(Val::id("negate".to_string()), vec![yy1]);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
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
 (YYMinorType::YY37(yy0),YYMinorType::YY37(yy2),) => {

	yyres = sexpr::binaryop("int_add".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
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
 (YYMinorType::YY37(yy0),YYMinorType::YY37(yy2),) => {

	yyres = sexpr::binaryop("int_sub".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
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
 (YYMinorType::YY37(yy0),YYMinorType::YY37(yy2),) => {

	yyres = sexpr::binaryop("int_mult".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
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
 (YYMinorType::YY37(yy0),YYMinorType::YY37(yy2),) => {

	yyres = sexpr::binaryop("int_div".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
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
 (YYMinorType::YY37(yy0),YYMinorType::YY37(yy2),) => {

	yyres = sexpr::binaryop("int_mod".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
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
 (YYMinorType::YY37(yy0),YYMinorType::YY37(yy2),) => {

	yyres = sexpr::binaryop("and".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
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
 (YYMinorType::YY37(yy0),YYMinorType::YY37(yy2),) => {

	yyres = sexpr::binaryop("or".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
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
 (YYMinorType::YY37(yy0),YYMinorType::YY37(yy2),) => {

	yyres = sexpr::binaryop("xor".to_string(),yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
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
 (YYMinorType::YY37(yy0),YYMinorType::YY37(yy2),) => {

	yyres = sexpr::binaryop("less_than".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
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
 (YYMinorType::YY37(yy0),YYMinorType::YY37(yy2),) => {

	yyres = sexpr::binaryop("less_than_equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
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
 (YYMinorType::YY37(yy0),YYMinorType::YY37(yy2),) => {

	yyres = sexpr::binaryop("greater_than".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
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
 (YYMinorType::YY37(yy0),YYMinorType::YY37(yy2),) => {

	yyres = sexpr::binaryop("greater_than_equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
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
 (YYMinorType::YY37(yy0),YYMinorType::YY37(yy2),) => {

	yyres = sexpr::binaryop("equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
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
 (YYMinorType::YY37(yy0),YYMinorType::YY37(yy2),) => {

	let eq = sexpr::binaryop("equal".to_string(), yy0, yy2);
	yyres = sexpr::call(Val::id("bool_not".to_string()), vec![eq]);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            88 /* term ::= LPAREN expr RPAREN */
          | 100 /* list ::= SquareL list_items SquareR */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY37(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            89 /* term ::= typex */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY79(yy0),) => {

    yyres = Val::Type(yy0);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
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
 YYMinorType::YY37(yyres)
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
 YYMinorType::YY37(yyres)
}
            ,
            93 /* term ::= INT */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY190(yy0),) => {

	yyres = Val::Int(yy0);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            96 /* term ::= HASHTAG */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY13(yy0),) => {

	yyres = Val::Hashtag(Arc::new(yy0.data));

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            98 /* field_access ::= DOT ID */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY13(yy1),) => {

    yyres = list::singleton(Val::id(yy1.data));

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            99 /* field_access ::= DOT ID field_access */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY13(yy1),YYMinorType::YY37(yy2),) => {

    yyres = list::cons(Val::id(yy1.data), yy2);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            102 /* list_items ::= expr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY37(yy0),) => {

	yyres = list::singleton(yy0);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
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
 (YYMinorType::YY37(yy0),YYMinorType::YY37(yy2),) => {

	verbose_out!("base tuple args!");
	yyres = list::cons(yy0, list::singleton(yy2));

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
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
 (YYMinorType::YY37(yy0),YYMinorType::YY37(yy2),) => {

	verbose_out!("additional tuple arg!");
	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
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
 (YYMinorType::YY37(yy1),) => {

	yyres = sexpr::strexpr(yy1);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
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
 YYMinorType::YY37(yyres)
}
            ,
            109 /* strlist ::= StrLit strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY36(yy0),YYMinorType::YY37(yy1),) => {

	yyres = list::cons(Val::new_str(yy0), yy1);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
}
            ,
            110 /* strlist ::= ID strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY13(yy0),YYMinorType::YY37(yy1),) => {

	yyres = list::cons(Val::id(yy0.data), yy1);

},    _ => unreachable!() };
 YYMinorType::YY37(yyres)
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

