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
const YYNSTATE: i32 = 205;
const YYNRULE: i32 = 110;
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
        Token::COMMA(x) => YYMinorType::YY128(x),
        Token::ELSE(x) => YYMinorType::YY128(x),
        Token::HASHTAG(x) => YYMinorType::YY117(x),
        Token::ID(x) => YYMinorType::YY4(x),
        Token::INT(x) => YYMinorType::YY60(x),
        Token::PLUS(x) => YYMinorType::YY128(x),
        Token::SLASH(x) => YYMinorType::YY128(x),
        Token::StrLit(x) => YYMinorType::YY4(x),
        Token::TYPE_ID(x) => YYMinorType::YY117(x),
        _ => YYMinorType::YY0
  }
}
const YY_ACTTAB_COUNT: i32 = 1059;
const YY_ACTION: [YYACTIONTYPE; 1059] = [
 /*     0 */   176,  120,  179,  105,  121,  128,  189,   96,  114,  167,
 /*    10 */   158,  188,   32,   25,  104,   26,  125,   41,   94,   45,
 /*    20 */   184,  175,   14,   35,  156,    8,  155,  129,  175,  205,
 /*    30 */    50,   29,   27,   24,  122,  117,  110,   12,  108,  193,
 /*    40 */   192,  191,  190,  102,    3,   25,  121,   70,   28,  178,
 /*    50 */   177,  159,  181,  180,    6,  127,   55,  176,  120,  179,
 /*    60 */    94,   71,  184,  189,   27,   24,  156,   53,  155,  111,
 /*    70 */   175,   54,   26,   93,  167,  113,  195,   41,   37,   14,
 /*    80 */    69,  167,    8,    1,  206,  123,   51,   50,    1,  196,
 /*    90 */    38,  122,  117,  110,   12,  108,  193,  192,  191,  190,
 /*   100 */   102,    3,   49,    4,   70,   28,  178,  177,  253,  181,
 /*   110 */   180,    6,  121,   55,   58,  176,  120,  179,  139,   11,
 /*   120 */   174,  189,  170,   36,  116,   39,   94,   75,  184,  121,
 /*   130 */    26,  183,  156,   30,  155,  149,  175,   14,  171,  169,
 /*   140 */     8,  175,   35,   94,   74,  184,  157,  189,  175,  156,
 /*   150 */   112,  155,  150,  175,  193,  192,  191,  190,  148,  163,
 /*   160 */   162,  166,   70,   28,  178,  177,  109,  181,  180,    6,
 /*   170 */    13,   55,   56,  176,  120,  179,  107,   67,  145,  189,
 /*   180 */   193,  192,  191,  190,   34,  160,   66,  144,   26,   64,
 /*   190 */   101,   63,  141,   62,   61,   14,  130,   68,    8,  138,
 /*   200 */   135,  118,   31,    1,  168,  106,   65,   39,  165,  164,
 /*   210 */   161,   40,  193,  192,  191,  190,   48,  100,  103,  121,
 /*   220 */    70,   28,  178,  177,   98,  181,  180,    6,  142,   55,
 /*   230 */   176,  120,  179,   94,   74,  184,  189,   51,   10,  156,
 /*   240 */   151,  155,   99,  175,  121,   26,  140,  133,  163,  162,
 /*   250 */   166,  134,   14,  137,    9,    8,  187,  132,   94,   72,
 /*   260 */   184,  131,  136,   57,  156,  317,  155,   95,  175,  193,
 /*   270 */   192,  191,  190,   34,   97,  119,   52,   70,   28,  178,
 /*   280 */   177,  173,  181,  180,    6,  172,   55,  176,  120,  179,
 /*   290 */    42,  115,  194,  189,  143,   60,   59,  165,  164,  161,
 /*   300 */   124,  121,   26,  317,  317,  317,  317,  317,  317,   14,
 /*   310 */   317,  317,    8,  317,  317,   94,   73,  184,  317,  317,
 /*   320 */   317,  156,  317,  155,  317,  175,  193,  192,  191,  190,
 /*   330 */   317,  317,   10,  121,   70,   28,  178,  177,  317,  181,
 /*   340 */   180,    6,  317,   55,  176,  120,  179,   94,   90,  184,
 /*   350 */   189,  317,  317,  156,  317,  155,  317,  175,  121,   26,
 /*   360 */   317,  317,  317,  317,  317,  317,   14,  317,  317,    8,
 /*   370 */   317,  317,   94,   47,  184,  317,  317,  317,  156,  317,
 /*   380 */   155,  317,  175,  193,  192,  191,  190,  317,  317,  317,
 /*   390 */   317,   70,   28,  178,  177,  317,  181,  180,    6,    7,
 /*   400 */    55,  317,  317,  317,   32,   25,  317,  317,  317,  176,
 /*   410 */   182,  179,   22,   21,   23,  185,  317,   16,   15,   18,
 /*   420 */    17,   20,   19,   29,   27,   24,    7,  186,  317,  317,
 /*   430 */   317,   32,   25,  317,   33,  317,  317,  317,  317,   22,
 /*   440 */    21,   23,  185,  317,   16,   15,   18,   17,   20,   19,
 /*   450 */    29,   27,   24,  317,  152,  316,  126,    2,  178,  177,
 /*   460 */   199,  181,  180,  317,  198,   55,  121,  197,  317,  317,
 /*   470 */   317,  317,  317,  317,  203,  317,  317,  202,  201,  200,
 /*   480 */    94,   77,  184,  317,  317,  317,  156,  317,  155,    7,
 /*   490 */   175,  317,  317,  317,   32,   25,  317,  317,  317,  317,
 /*   500 */   317,  317,   22,   21,   23,  185,  317,   16,   15,   18,
 /*   510 */    17,   20,   19,   29,   27,   24,    5,  317,  317,  317,
 /*   520 */   317,   32,   25,  317,  317,  317,  317,  317,  317,   22,
 /*   530 */    21,   23,  185,  317,   16,   15,   18,   17,   20,   19,
 /*   540 */    29,   27,   24,   32,   25,  317,  317,  317,  317,  317,
 /*   550 */   317,   22,   21,   23,  185,  317,   16,   15,   18,   17,
 /*   560 */    20,   19,   29,   27,   24,  317,  152,   32,   25,  317,
 /*   570 */   317,  317,    1,  317,  317,   22,   21,   23,  185,  317,
 /*   580 */    16,   15,   18,   17,   20,   19,   29,   27,   24,   32,
 /*   590 */    25,  317,  317,  317,  317,  317,  317,   22,   21,   23,
 /*   600 */   185,  317,   16,   15,   18,   17,   20,   19,   29,   27,
 /*   610 */    24,  317,  317,  317,  317,  317,  204,    2,  317,  317,
 /*   620 */   199,  317,  317,  317,  198,  317,  121,  197,  317,  317,
 /*   630 */   317,   39,  317,  317,  203,  317,  317,  202,  201,  200,
 /*   640 */    94,   77,  184,  146,    2,  317,  156,  199,  155,  317,
 /*   650 */   175,  198,  317,  121,  197,  317,  317,  317,  317,  317,
 /*   660 */   317,  203,  317,  317,  202,  201,  200,   94,   77,  184,
 /*   670 */   317,  317,  317,  156,  317,  155,  317,  175,  317,  317,
 /*   680 */   317,   32,   25,  317,  317,  317,  317,  317,  317,   22,
 /*   690 */    21,   23,  185,  317,   16,   15,   18,   17,   20,   19,
 /*   700 */    29,   27,   24,   32,   25,  317,  317,  317,  317,  317,
 /*   710 */   317,  317,  317,   23,  185,  317,   16,   15,   18,   17,
 /*   720 */    20,   19,   29,   27,   24,   32,   25,  317,  317,  317,
 /*   730 */   317,  317,  317,  317,   32,   25,  185,  317,   16,   15,
 /*   740 */    18,   17,   20,   19,   29,   27,   24,  315,  315,  315,
 /*   750 */   315,   20,   19,   29,   27,   24,  121,  317,  317,  317,
 /*   760 */   317,  317,  317,  317,  317,  317,  317,  121,  317,  317,
 /*   770 */    94,   79,  184,  317,  317,  317,  156,  317,  155,  121,
 /*   780 */   175,   94,   91,  184,  317,  317,  317,  156,  317,  155,
 /*   790 */   317,  175,  317,   94,   46,  184,  121,  317,  317,  156,
 /*   800 */   317,  155,  317,  175,  317,  317,  317,  317,  317,  121,
 /*   810 */    94,  147,  184,  317,  317,  317,  156,  317,  155,  317,
 /*   820 */   175,  121,  317,   94,   83,  184,  317,  317,  317,  156,
 /*   830 */   317,  155,  317,  175,  317,   94,  154,  184,  317,  317,
 /*   840 */   317,  156,  121,  155,  317,  175,  317,  317,  317,  317,
 /*   850 */   317,  317,  317,  121,  317,  317,   94,  153,  184,  317,
 /*   860 */   317,  317,  156,  317,  155,  317,  175,   94,   82,  184,
 /*   870 */   121,  317,  317,  156,  317,  155,  317,  175,  317,  317,
 /*   880 */   317,  317,  317,  317,   94,   81,  184,  121,  317,  317,
 /*   890 */   156,  317,  155,  317,  175,  317,  317,  317,  121,  317,
 /*   900 */   317,   94,   80,  184,  317,  317,  317,  156,  317,  155,
 /*   910 */   121,  175,   94,   89,  184,  317,  317,  317,  156,  317,
 /*   920 */   155,  317,  175,  317,   94,   88,  184,  317,  121,  317,
 /*   930 */   156,  121,  155,  317,  175,  317,  317,  317,  317,  317,
 /*   940 */   317,  317,   94,   87,  184,   94,   86,  184,  156,  317,
 /*   950 */   155,  156,  175,  155,  317,  175,  317,  317,  317,  121,
 /*   960 */   317,  317,  121,  317,  317,  317,  317,  317,  317,  317,
 /*   970 */   317,  317,  317,   94,   85,  184,   94,   84,  184,  156,
 /*   980 */   317,  155,  156,  175,  155,  121,  175,  317,  121,  317,
 /*   990 */   317,  317,  317,  317,  317,  317,  317,  317,  317,   94,
 /*  1000 */    92,  184,   94,   78,  184,  156,  121,  155,  156,  175,
 /*  1010 */   155,  317,  175,  317,  317,  317,  317,  121,  317,  317,
 /*  1020 */    94,   76,  184,  317,  317,  317,  156,  317,  155,  317,
 /*  1030 */   175,   94,   44,  184,  121,  317,  317,  156,  317,  155,
 /*  1040 */   317,  175,  317,  317,  317,  317,  317,  317,   94,   43,
 /*  1050 */   184,  317,  317,  317,  156,  317,  155,  317,  175,
];
const YY_LOOKAHEAD: [YYCODETYPE; 1059] = [
 /*     0 */     4,    5,    6,   67,   74,   32,   10,   77,   79,   80,
 /*    10 */    81,   88,    7,    8,   78,   19,   83,   84,   88,   89,
 /*    20 */    90,   98,   26,    2,   94,   29,   96,   31,   98,    0,
 /*    30 */    34,   26,   27,   28,   38,   39,   40,   41,   42,   43,
 /*    40 */    44,   45,   46,   47,   48,    8,   74,   51,   52,   53,
 /*    50 */    54,   30,   56,   57,   58,   33,   60,    4,    5,    6,
 /*    60 */    88,   89,   90,   10,   27,   28,   94,    5,   96,   97,
 /*    70 */    98,    9,   19,   79,   80,   81,   83,   84,    5,   26,
 /*    80 */    79,   80,   29,   12,    0,    5,    3,   34,   12,   35,
 /*    90 */     4,   38,   39,   40,   41,   42,   43,   44,   45,   46,
 /*   100 */    47,   48,   37,   29,   51,   52,   53,   54,   29,   56,
 /*   110 */    57,   58,   74,   60,    3,    4,    5,    6,   35,   48,
 /*   120 */    61,   10,   35,   50,    5,   49,   88,   89,   90,   74,
 /*   130 */    19,   88,   94,   20,   96,   97,   98,   26,   88,   35,
 /*   140 */    29,   98,    2,   88,   89,   90,   30,   10,   98,   94,
 /*   150 */    95,   96,   59,   98,   43,   44,   45,   46,   30,    4,
 /*   160 */     5,    6,   51,   52,   53,   54,    5,   56,   57,   58,
 /*   170 */    20,   60,    3,    4,    5,    6,    5,   29,   35,   10,
 /*   180 */    43,   44,   45,   46,   29,   30,   30,   35,   19,    2,
 /*   190 */     5,   29,   35,   30,    2,   26,   30,   67,   29,   35,
 /*   200 */    35,   93,   49,   12,   78,   69,    5,   49,   53,   54,
 /*   210 */    55,   91,   43,   44,   45,   46,   37,   73,   91,   74,
 /*   220 */    51,   52,   53,   54,    5,   56,   57,   58,   69,   60,
 /*   230 */     4,    5,    6,   88,   89,   90,   10,    3,   49,   94,
 /*   240 */    95,   96,   67,   98,   74,   19,   73,   67,    4,    5,
 /*   250 */     6,   77,   26,   76,   49,   29,   30,   67,   88,   89,
 /*   260 */    90,   93,   67,   67,   94,  100,   96,   97,   98,   43,
 /*   270 */    44,   45,   46,   29,   76,   99,   67,   51,   52,   53,
 /*   280 */    54,   99,   56,   57,   58,   99,   60,    4,    5,    6,
 /*   290 */    90,   78,   90,   10,   90,   67,   67,   53,   54,   55,
 /*   300 */    36,   74,   19,  100,  100,  100,  100,  100,  100,   26,
 /*   310 */   100,  100,   29,  100,  100,   88,   89,   90,  100,  100,
 /*   320 */   100,   94,  100,   96,  100,   98,   43,   44,   45,   46,
 /*   330 */   100,  100,   49,   74,   51,   52,   53,   54,  100,   56,
 /*   340 */    57,   58,  100,   60,    4,    5,    6,   88,   89,   90,
 /*   350 */    10,  100,  100,   94,  100,   96,  100,   98,   74,   19,
 /*   360 */   100,  100,  100,  100,  100,  100,   26,  100,  100,   29,
 /*   370 */   100,  100,   88,   89,   90,  100,  100,  100,   94,  100,
 /*   380 */    96,  100,   98,   43,   44,   45,   46,  100,  100,  100,
 /*   390 */   100,   51,   52,   53,   54,  100,   56,   57,   58,    2,
 /*   400 */    60,  100,  100,  100,    7,    8,  100,  100,  100,    4,
 /*   410 */     5,    6,   15,   16,   17,   18,  100,   20,   21,   22,
 /*   420 */    23,   24,   25,   26,   27,   28,    2,   30,  100,  100,
 /*   430 */   100,    7,    8,  100,   29,  100,  100,  100,  100,   15,
 /*   440 */    16,   17,   18,  100,   20,   21,   22,   23,   24,   25,
 /*   450 */    26,   27,   28,  100,   30,   63,   64,   65,   53,   54,
 /*   460 */    68,   56,   57,  100,   72,   60,   74,   75,  100,  100,
 /*   470 */   100,  100,  100,  100,   82,  100,  100,   85,   86,   87,
 /*   480 */    88,   89,   90,  100,  100,  100,   94,  100,   96,    2,
 /*   490 */    98,  100,  100,  100,    7,    8,  100,  100,  100,  100,
 /*   500 */   100,  100,   15,   16,   17,   18,  100,   20,   21,   22,
 /*   510 */    23,   24,   25,   26,   27,   28,    2,  100,  100,  100,
 /*   520 */   100,    7,    8,  100,  100,  100,  100,  100,  100,   15,
 /*   530 */    16,   17,   18,  100,   20,   21,   22,   23,   24,   25,
 /*   540 */    26,   27,   28,    7,    8,  100,  100,  100,  100,  100,
 /*   550 */   100,   15,   16,   17,   18,  100,   20,   21,   22,   23,
 /*   560 */    24,   25,   26,   27,   28,  100,   30,    7,    8,  100,
 /*   570 */   100,  100,   12,  100,  100,   15,   16,   17,   18,  100,
 /*   580 */    20,   21,   22,   23,   24,   25,   26,   27,   28,    7,
 /*   590 */     8,  100,  100,  100,  100,  100,  100,   15,   16,   17,
 /*   600 */    18,  100,   20,   21,   22,   23,   24,   25,   26,   27,
 /*   610 */    28,  100,  100,  100,  100,  100,   64,   65,  100,  100,
 /*   620 */    68,  100,  100,  100,   72,  100,   74,   75,  100,  100,
 /*   630 */   100,   49,  100,  100,   82,  100,  100,   85,   86,   87,
 /*   640 */    88,   89,   90,   64,   65,  100,   94,   68,   96,  100,
 /*   650 */    98,   72,  100,   74,   75,  100,  100,  100,  100,  100,
 /*   660 */   100,   82,  100,  100,   85,   86,   87,   88,   89,   90,
 /*   670 */   100,  100,  100,   94,  100,   96,  100,   98,  100,  100,
 /*   680 */   100,    7,    8,  100,  100,  100,  100,  100,  100,   15,
 /*   690 */    16,   17,   18,  100,   20,   21,   22,   23,   24,   25,
 /*   700 */    26,   27,   28,    7,    8,  100,  100,  100,  100,  100,
 /*   710 */   100,  100,  100,   17,   18,  100,   20,   21,   22,   23,
 /*   720 */    24,   25,   26,   27,   28,    7,    8,  100,  100,  100,
 /*   730 */   100,  100,  100,  100,    7,    8,   18,  100,   20,   21,
 /*   740 */    22,   23,   24,   25,   26,   27,   28,   20,   21,   22,
 /*   750 */    23,   24,   25,   26,   27,   28,   74,  100,  100,  100,
 /*   760 */   100,  100,  100,  100,  100,  100,  100,   74,  100,  100,
 /*   770 */    88,   89,   90,  100,  100,  100,   94,  100,   96,   74,
 /*   780 */    98,   88,   89,   90,  100,  100,  100,   94,  100,   96,
 /*   790 */   100,   98,  100,   88,   89,   90,   74,  100,  100,   94,
 /*   800 */   100,   96,  100,   98,  100,  100,  100,  100,  100,   74,
 /*   810 */    88,   89,   90,  100,  100,  100,   94,  100,   96,  100,
 /*   820 */    98,   74,  100,   88,   89,   90,  100,  100,  100,   94,
 /*   830 */   100,   96,  100,   98,  100,   88,   89,   90,  100,  100,
 /*   840 */   100,   94,   74,   96,  100,   98,  100,  100,  100,  100,
 /*   850 */   100,  100,  100,   74,  100,  100,   88,   89,   90,  100,
 /*   860 */   100,  100,   94,  100,   96,  100,   98,   88,   89,   90,
 /*   870 */    74,  100,  100,   94,  100,   96,  100,   98,  100,  100,
 /*   880 */   100,  100,  100,  100,   88,   89,   90,   74,  100,  100,
 /*   890 */    94,  100,   96,  100,   98,  100,  100,  100,   74,  100,
 /*   900 */   100,   88,   89,   90,  100,  100,  100,   94,  100,   96,
 /*   910 */    74,   98,   88,   89,   90,  100,  100,  100,   94,  100,
 /*   920 */    96,  100,   98,  100,   88,   89,   90,  100,   74,  100,
 /*   930 */    94,   74,   96,  100,   98,  100,  100,  100,  100,  100,
 /*   940 */   100,  100,   88,   89,   90,   88,   89,   90,   94,  100,
 /*   950 */    96,   94,   98,   96,  100,   98,  100,  100,  100,   74,
 /*   960 */   100,  100,   74,  100,  100,  100,  100,  100,  100,  100,
 /*   970 */   100,  100,  100,   88,   89,   90,   88,   89,   90,   94,
 /*   980 */   100,   96,   94,   98,   96,   74,   98,  100,   74,  100,
 /*   990 */   100,  100,  100,  100,  100,  100,  100,  100,  100,   88,
 /*  1000 */    89,   90,   88,   89,   90,   94,   74,   96,   94,   98,
 /*  1010 */    96,  100,   98,  100,  100,  100,  100,   74,  100,  100,
 /*  1020 */    88,   89,   90,  100,  100,  100,   94,  100,   96,  100,
 /*  1030 */    98,   88,   89,   90,   74,  100,  100,   94,  100,   96,
 /*  1040 */   100,   98,  100,  100,  100,  100,  100,  100,   88,   89,
 /*  1050 */    90,  100,  100,  100,   94,  100,   96,  100,   98,
];
const YY_SHIFT_USE_DFLT: i32 = -28;
const YY_SHIFT_COUNT: i32 = 129;
const YY_SHIFT_MIN: i32 = -27;
const YY_SHIFT_MAX: i32 = 727;
const YY_SHIFT_OFST: [i16; 130] = [
 /*     0 */    -4,   53,   53,  283,  226,  340,  340,  340,  340,  169,
 /*    10 */   111,  340,  340,  340,  340,  340,  340,  340,  340,  340,
 /*    20 */   340,  340,  340,  340,  340,  340,  340,  340,  340,  340,
 /*    30 */   340,  340,  340,  340,  155,  244,  405,  405,  405,  244,
 /*    40 */    76,  264,  264,  560,  560,  560,  582,  560,  137,  137,
 /*    50 */   137,   71,   83,   62,   62,   62,  191,  205,  191,  189,
 /*    60 */   234,  219,  191,  219,  201,  179,  179,  201,  158,  191,
 /*    70 */   153,  424,  397,  536,  514,  487,  674,  674,  674,  674,
 /*    80 */   696,  696,  718,  718,  727,  727,  727,  727,    5,    5,
 /*    90 */    37,   37,   37,   21,   73,  166,  165,  164,  192,  157,
 /*   100 */   163,  162,  185,  187,  152,  143,  156,  148,  171,  150,
 /*   110 */   161,  128,   93,  116,  140,  104,  113,  119,   87,   59,
 /*   120 */    79,   74,   86,   65,   80,   54,   84,   29,   22,  -27,
];
const YY_REDUCE_USE_DFLT: i32 = -78;
const YY_REDUCE_COUNT: i32 = 70;
const YY_REDUCE_MIN: i32 = -77;
const YY_REDUCE_MAX: i32 = 960;
const YY_REDUCE_OFST: [i16; 71] = [
 /*     0 */   392,  579,  552,  -70,  170,  145,   55,   38,  -28,  284,
 /*    10 */   960,  943,  932,  914,  911,  888,  885,  857,  854,  836,
 /*    20 */   824,  813,  796,  779,  768,  747,  735,  722,  705,  693,
 /*    30 */   682,  284,  259,  227,   -6,  -71,   50,   43,  -77,    1,
 /*    40 */   -64,   -7,  -67,  229,  228,  209,  213,  196,  204,  202,
 /*    50 */   200,  195,  198,  186,  182,  176,  190,  168,  180,  174,
 /*    60 */   177,  173,  175,  144,  159,  127,  120,  136,  126,  130,
 /*    70 */   108,
];
const YY_DEFAULT: [YYACTIONTYPE; 205] = [
 /*     0 */   207,  207,  207,  315,  315,  305,  305,  315,  315,  315,
 /*    10 */   315,  315,  315,  315,  315,  315,  315,  315,  315,  315,
 /*    20 */   315,  315,  315,  315,  315,  315,  315,  315,  315,  315,
 /*    30 */   315,  315,  315,  315,  315,  315,  315,  315,  315,  315,
 /*    40 */   315,  218,  218,  315,  315,  315,  315,  315,  315,  315,
 /*    50 */   315,  315,  315,  312,  312,  312,  315,  315,  315,  315,
 /*    60 */   246,  239,  315,  239,  228,  231,  231,  228,  262,  315,
 /*    70 */   315,  315,  315,  315,  306,  309,  224,  223,  222,  221,
 /*    80 */   287,  286,  285,  277,  293,  292,  291,  290,  289,  288,
 /*    90 */   280,  281,  279,  315,  294,  315,  315,  315,  240,  315,
 /*   100 */   315,  315,  315,  229,  315,  315,  315,  315,  315,  315,
 /*   110 */   315,  315,  315,  315,  273,  315,  315,  315,  315,  315,
 /*   120 */   296,  315,  315,  315,  315,  315,  315,  315,  315,  315,
 /*   130 */   252,  259,  258,  249,  248,  244,  247,  245,  243,  242,
 /*   140 */   241,  238,  230,  232,  227,  226,  225,  282,  308,  310,
 /*   150 */   304,  307,  295,  284,  283,  276,  275,  272,  274,  271,
 /*   160 */   270,  269,  268,  267,  266,  265,  264,  263,  261,  260,
 /*   170 */   257,  256,  314,  313,  311,  303,  302,  301,  300,  299,
 /*   180 */   298,  297,  296,  255,  254,  278,  251,  250,  220,  237,
 /*   190 */   236,  235,  234,  233,  219,  217,  216,  215,  214,  213,
 /*   200 */   212,  211,  210,  209,  208,
];

/* TMPL: fallback tokens */

const YY_FALLBACK: [i32; 0] = [
];

/* TMPL: symbol names */


/* TMPL: rules */


/* TMPL: destructors */


/* TMPL: stack-overflow */


/* TMPL: stack-overflow */

const YY_RULE_INFO: [YYCODETYPE; 110] = [
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
  74,
  74,
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
  94,
  95,
  95,
  95,
  96,
  97,
  97,
  98,
  99,
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
 YYMinorType::YY197(yyres)
}
            ,
            1 /* program ::= stmts */
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
            2 /* stmts ::= */
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
            3 /* stmts ::= stmt stmts */
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
            4 /* stmt ::= def_struct */
          | 5 /* stmt ::= let_stmt */
          | 7 /* stmt ::= fail_stmt */
          | 8 /* stmt ::= func_stmt */
          | 9 /* stmt ::= macro_stmt */
          | 10 /* stmt ::= if_stmt */
          | 58 /* pexpr ::= ptuple */
          | 70 /* expr ::= list */
          | 71 /* expr ::= tuple */
          | 89 /* expr ::= term */
          | 98 /* term ::= strexpr */
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
            6 /* stmt ::= expr_stmt */
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
            11 /* def_struct ::= STRUCT typex struct_fields DOUBLEDASH */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
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
            12 /* struct_fields ::= struct_field struct_fields */
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
            13 /* struct_fields ::= */
          | 23 /* dfunc_args ::= */
          | 100 /* list_items ::= */
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
            14 /* struct_field ::= DOT ID COLON typex */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY4(yy1),YYMinorType::YY195(yy3),) => {

	yyres = Val::Tuple(vec![Val::id(yy1), Val::Type(yy3)]);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY117(yy1),YYMinorType::YY24(yy2),) => {

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
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY4(yy1),YYMinorType::YY24(yy3),) => {

	let letx =
        list::cons(Val::id(yy1),
        list::cons(yy3,
        Val::Nil
        ));
	yyres = sexpr::new(SexprType::Let, letx);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY4(yy1),YYMinorType::YY24(yy3),) => {

	let bind = list::cons(Val::new_str(yy1), list::singleton(yy3));
	yyres = sexpr::new(SexprType::Fork, bind);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            18 /* expr_stmt ::= expr */
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
            19 /* expr_stmt ::= DollarGT expr */
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
            20 /* block ::= BLOCKARROW stmts */
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
 (YYMinorType::YY4(yy1),YYMinorType::YY24(yy3),YYMinorType::YY195(yy5),YYMinorType::YY24(yy6),) => {

	let id = Val::id(yy1);
	let typ = Val::Type(yy5);
	yyres = sexpr::defunc(id, yy3, typ, yy6)

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY4(yy1),YYMinorType::YY24(yy3),YYMinorType::YY195(yy5),YYMinorType::YY24(yy6),) => {

	let id = Val::id(yy1);
	let typ = Val::Type(yy5);
    let body = sexpr::match_expr(Val::CallParams, yy6);
	yyres = sexpr::defunc(id, yy3, typ, body)

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            24 /* dfunc_args ::= ID opt_typex */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY4(yy0),YYMinorType::YY195(yy1),) => {

	yyres = list::singleton(sexpr::id_with_type(yy0, yy1));

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY4(yy0),YYMinorType::YY195(yy1),YYMinorType::YY24(yy3),) => {

	yyres = list::cons(sexpr::id_with_type(yy0, yy1), yy3);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 YYMinorType::YY195(yyres)
}
            ,
            27 /* opt_typex ::= COLON typex */
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
            28 /* typex ::= TYPE_INT */
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
            29 /* typex ::= TYPE_STR */
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
            30 /* typex ::= TYPE_BOOL */
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
            31 /* typex ::= TYPE_VOID */
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
            32 /* typex ::= TYPE_ID */
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
 (YYMinorType::YY4(yy1),YYMinorType::YY24(yy3),YYMinorType::YY24(yy5),) => {

    verbose_out!("found macro {:?}\n", yy1);
    yyres = sexpr::new(SexprType::DefMacro,
        list::cons(Val::id(yy1),
        list::cons(yy3,
        list::cons(yy5,
        Val::Nil
    ))));

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 YYMinorType::YY24(yyres)
}
            ,
            35 /* macro_args ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY4(yy0),) => {

    yyres = list::singleton(Val::id(yy0));

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY4(yy0),YYMinorType::YY24(yy2),) => {

    yyres = list::cons(Val::id(yy0), yy2);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy1),YYMinorType::YY24(yy2),) => {

    /* if-only style */
    yyres = sexpr::ifstmt(yy1, yy2, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy1),YYMinorType::YY24(yy2),YYMinorType::YY24(yy3),) => {

    /* if-else style */
    yyres = sexpr::ifstmt(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy1),) => {

    /* case-expr style */
    yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy2),YYMinorType::YY24(yy3),YYMinorType::YY24(yy4),) => {

    yyres = sexpr::ifstmt(yy2, yy3, yy4);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy2),YYMinorType::YY24(yy3),) => {

    yyres = sexpr::ifstmt(yy2, yy3, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            42 /* else_if ::= ELSE block */
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
            43 /* if_case ::= PIPE expr block if_case */
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
            44 /* if_case ::= PIPE ELSE block */
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
            45 /* expr ::= call_id LPAREN RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
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
            46 /* expr ::= call_id LPAREN expr RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY24(yy0),YYMinorType::YY24(yy2),) => {

	verbose_out!("one param function call!");
	yyres = sexpr::call(yy0, vec![yy2]);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            47 /* expr ::= call_id LPAREN tuple_args RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY24(yy0),YYMinorType::YY24(yy2),) => {

	verbose_out!("multi param function call!");
	yyres = sexpr::call(yy0, list::to_vec(yy2));

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            48 /* call_id ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY4(yy0),) => {

    yyres = Val::id(yy0);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            49 /* call_id ::= typex */
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
            50 /* expr ::= term ID term */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY24(yy0),YYMinorType::YY4(yy1),YYMinorType::YY24(yy2),) => {

	yyres = sexpr::binaryop(yy1, yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            51 /* expr ::= term DOLLAR term */
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
            52 /* expr ::= CASE cases DOUBLEDASH */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
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
            53 /* cases ::= PIPE expr block PIPE ELSE block */
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
            54 /* cases ::= PIPE expr block cases */
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
            55 /* expr ::= MATCH expr match_case DOUBLEDASH */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
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
            56 /* match_case ::= PIPE pexpr block match_case */
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
            57 /* match_case ::= PIPE pexpr block */
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
            59 /* pexpr ::= INT */
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
            60 /* pexpr ::= True */
          | 95 /* term ::= True */
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
            61 /* pexpr ::= False */
          | 96 /* term ::= False */
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
            62 /* pexpr ::= HASHTAG */
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
            63 /* pexpr ::= ID */
          | 91 /* term ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY4(yy0),) => {
 yyres = Val::id(yy0); 
},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            64 /* pexpr ::= UNDERSCORE */
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
            65 /* ptuple ::= LPAREN RPAREN */
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
            66 /* ptuple ::= LPAREN pexpr RPAREN */
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
            67 /* ptuple ::= LPAREN pargs RPAREN */
          | 103 /* tuple ::= LPAREN tuple_args RPAREN */
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
            68 /* pargs ::= pexpr COMMA pexpr */
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
            69 /* pargs ::= pexpr COMMA pargs */
          | 102 /* list_items ::= expr COMMA list_items */
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
            72 /* expr ::= NOT expr */
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
            73 /* expr ::= expr ConcatNewline */
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
            74 /* expr ::= MINUS expr */
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
            75 /* expr ::= expr PLUS expr */
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
            76 /* expr ::= expr MINUS expr */
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
            77 /* expr ::= expr TIMES expr */
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
            78 /* expr ::= expr SLASH expr */
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
            79 /* expr ::= expr MOD expr */
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
            80 /* expr ::= expr AND expr */
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
            81 /* expr ::= expr OR expr */
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
            82 /* expr ::= expr XOR expr */
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
            83 /* expr ::= expr LT expr */
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
            84 /* expr ::= expr LTEQ expr */
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
            85 /* expr ::= expr GT expr */
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
            86 /* expr ::= expr GTEQ expr */
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
            87 /* expr ::= expr EQ expr */
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
            88 /* expr ::= expr NEQ expr */
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
            90 /* term ::= LPAREN expr RPAREN */
          | 99 /* list ::= SquareL list_items SquareR */
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
            92 /* term ::= VOID */
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
            93 /* term ::= DollarQuestion */
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
            94 /* term ::= INT */
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
            97 /* term ::= HASHTAG */
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
            101 /* list_items ::= expr */
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
            104 /* tuple_args ::= expr COMMA expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY24(yy0),YYMinorType::YY24(yy2),) => {

	verbose_out!("base tuple args!");
	yyres = list::cons(yy0, list::singleton(yy2));

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            105 /* tuple_args ::= expr COMMA tuple_args */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY24(yy0),YYMinorType::YY24(yy2),) => {

	verbose_out!("additional tuple arg!");
	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            106 /* strexpr ::= StrOpen strlist StrClose */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY24(yy1),) => {

	yyres = sexpr::strexpr(yy1);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            107 /* strlist ::= */
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
            108 /* strlist ::= StrLit strlist */
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
            109 /* strlist ::= ID strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY4(yy0),YYMinorType::YY24(yy1),) => {

	yyres = list::cons(Val::id(yy0), yy1);

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

