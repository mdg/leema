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
const YYNSTATE: i32 = 203;
const YYNRULE: i32 = 109;
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
        Token::COMMA(x) => YYMinorType::YY128(x),
        Token::ELSE(x) => YYMinorType::YY128(x),
        Token::HASHTAG(x) => YYMinorType::YY117(x),
        Token::ID(x) => YYMinorType::YY117(x),
        Token::INT(x) => YYMinorType::YY60(x),
        Token::PLUS(x) => YYMinorType::YY128(x),
        Token::SLASH(x) => YYMinorType::YY128(x),
        Token::StrLit(x) => YYMinorType::YY4(x),
        Token::TYPE_ID(x) => YYMinorType::YY117(x),
        _ => YYMinorType::YY0
  }
}
const YY_ACTTAB_COUNT: i32 = 902;
const YY_ACTION: [YYACTIONTYPE; 902] = [
 /*     0 */   175,  181,  178,   94,   71,  182,  187,   96,  129,  155,
 /*    10 */    35,  154,  111,  174,   51,   15,  128,   53,   94,   45,
 /*    20 */   182,   54,   14,   38,  155,    8,  154,  130,  174,  203,
 /*    30 */    50,  114,  166,  157,  123,  117,  110,   12,  108,  191,
 /*    40 */   190,  189,  188,  102,    3,  204,  141,   70,   16,  177,
 /*    50 */   176,  158,  180,  179,    6,   34,   55,  175,  181,  178,
 /*    60 */    94,   74,  182,  187,  126,   41,  155,  194,  154,  150,
 /*    70 */   174,    1,   15,   69,  166,   94,   72,  182,  186,   14,
 /*    80 */   182,  155,    8,  154,   95,  174,  124,   50,  174,  193,
 /*    90 */    41,  123,  117,  110,   12,  108,  191,  190,  189,  188,
 /*   100 */   102,    3,   49,   29,   70,   16,  177,  176,   39,  180,
 /*   110 */   179,    6,   36,   55,   58,  175,  181,  178,   93,  166,
 /*   120 */   113,  187,   30,   28,    4,   94,   73,  182,  105,    4,
 /*   130 */    15,  155,  112,  154,  173,  174,  183,   14,  182,  104,
 /*   140 */     8,  169,   94,   73,  182,  116,  174,   17,  155,  152,
 /*   150 */   154,    1,  174,  168,  191,  190,  189,  188,   38,  162,
 /*   160 */   161,  165,   70,   16,  177,  176,  156,  180,  179,    6,
 /*   170 */   187,   55,   56,  175,  181,  178,  170,  149,  182,  187,
 /*   180 */    32,   29,  151,  109,   37,  159,  174,   11,   15,  107,
 /*   190 */    13,   67,  147,   66,  146,   14,   64,  101,    8,   31,
 /*   200 */    30,   28,    4,  191,  190,  189,  188,   62,  164,  163,
 /*   210 */   160,   63,  191,  190,  189,  188,  143,  162,  161,  165,
 /*   220 */    70,   16,  177,  176,   61,  180,  179,    6,  140,   55,
 /*   230 */   175,  181,  178,   94,   75,  182,  187,  131,  137,  155,
 /*   240 */    18,  154,   37,  174,  118,   15,   68,    1,   39,  167,
 /*   250 */    65,   48,   14,  100,  106,    8,  132,   40,  103,  144,
 /*   260 */    98,  142,   99,  139,   51,   10,  164,  163,  160,  191,
 /*   270 */   190,  189,  188,  136,    9,  135,  133,   70,   16,  177,
 /*   280 */   176,   97,  180,  179,    6,  134,   55,  175,  181,  178,
 /*   290 */    94,   92,  182,  187,  125,  138,  155,  119,  154,  172,
 /*   300 */   174,   42,   15,  171,   57,  192,   52,  145,  115,   14,
 /*   310 */    60,   59,    8,  314,  314,   94,   91,  182,  314,  314,
 /*   320 */   314,  155,  314,  154,  314,  174,  191,  190,  189,  188,
 /*   330 */   314,  314,   10,  314,   70,   16,  177,  176,  314,  180,
 /*   340 */   179,    6,  314,   55,  175,  181,  178,  314,  314,  314,
 /*   350 */   187,  314,  314,  314,  314,   94,  122,  182,  314,   15,
 /*   360 */   314,  155,  314,  154,  314,  174,   14,  314,  314,    8,
 /*   370 */   314,  314,   94,  121,  182,  314,  314,  314,  155,  314,
 /*   380 */   154,  314,  174,  191,  190,  189,  188,  314,  314,  314,
 /*   390 */   314,   70,   16,  177,  176,  314,  180,  179,    6,    7,
 /*   400 */    55,   94,  120,  182,   32,   29,  314,  155,  314,  154,
 /*   410 */   314,  174,   26,   25,   27,  184,  314,   20,   19,   22,
 /*   420 */    21,   24,   23,   31,   30,   28,    4,  185,    7,   94,
 /*   430 */    83,  182,  314,   32,   29,  155,  314,  154,  314,  174,
 /*   440 */   314,   26,   25,   27,  184,  314,   20,   19,   22,   21,
 /*   450 */    24,   23,   31,   30,   28,    4,  153,   32,   29,  314,
 /*   460 */   314,  314,  314,  314,  314,   26,   25,   27,  184,  314,
 /*   470 */    20,   19,   22,   21,   24,   23,   31,   30,   28,    4,
 /*   480 */   153,    7,   94,   81,  182,  314,   32,   29,  155,  314,
 /*   490 */   154,  314,  174,  314,   26,   25,   27,  184,  314,   20,
 /*   500 */    19,   22,   21,   24,   23,   31,   30,   28,    4,    5,
 /*   510 */    94,   80,  182,  314,   32,   29,  155,  314,  154,  314,
 /*   520 */   174,  314,   26,   25,   27,  184,  314,   20,   19,   22,
 /*   530 */    21,   24,   23,   31,   30,   28,    4,   32,   29,  314,
 /*   540 */   314,  314,    1,  314,  314,   26,   25,   27,  184,  314,
 /*   550 */    20,   19,   22,   21,   24,   23,   31,   30,   28,    4,
 /*   560 */    32,   29,  314,  314,  314,  314,  314,  314,   26,   25,
 /*   570 */    27,  184,  314,   20,   19,   22,   21,   24,   23,   31,
 /*   580 */    30,   28,    4,  314,  314,  314,   94,   89,  182,  314,
 /*   590 */    32,   29,  155,  314,  154,  314,  174,  314,   26,   25,
 /*   600 */    27,  184,   39,   20,   19,   22,   21,   24,   23,   31,
 /*   610 */    30,   28,    4,  314,  313,  127,    2,  314,  314,  197,
 /*   620 */   314,  314,  314,  196,  314,  314,  195,  314,  314,  314,
 /*   630 */   314,  314,  314,  201,  314,  314,  200,  199,  198,   94,
 /*   640 */    77,  182,  202,    2,  314,  155,  197,  154,  314,  174,
 /*   650 */   196,  314,  314,  195,  314,  314,  314,  314,  314,  314,
 /*   660 */   201,  314,  314,  200,  199,  198,   94,   77,  182,  148,
 /*   670 */     2,  314,  155,  197,  154,  314,  174,  196,  314,  314,
 /*   680 */   195,  314,  314,  314,  314,  314,  314,  201,  314,  314,
 /*   690 */   200,  199,  198,   94,   77,  182,   94,   88,  182,  155,
 /*   700 */   314,  154,  155,  174,  154,  314,  174,   32,   29,  314,
 /*   710 */   314,  314,  314,  175,  181,  178,  314,   27,  184,  187,
 /*   720 */    20,   19,   22,   21,   24,   23,   31,   30,   28,    4,
 /*   730 */   314,   94,   87,  182,   94,   86,  182,  155,   33,  154,
 /*   740 */   155,  174,  154,  314,  174,  314,  314,  314,  314,  314,
 /*   750 */   314,  314,  191,  190,  189,  188,  314,  314,  314,  314,
 /*   760 */    32,   29,  177,  176,  314,  180,  179,  314,  314,   55,
 /*   770 */   314,  184,  314,   20,   19,   22,   21,   24,   23,   31,
 /*   780 */    30,   28,    4,   32,   29,   94,   85,  182,  314,  314,
 /*   790 */   314,  155,  314,  154,  314,  174,  312,  312,  312,  312,
 /*   800 */    24,   23,   31,   30,   28,    4,   94,   84,  182,   94,
 /*   810 */    47,  182,  155,  314,  154,  155,  174,  154,  314,  174,
 /*   820 */    94,   79,  182,   94,   46,  182,  155,  314,  154,  155,
 /*   830 */   174,  154,  314,  174,  314,  314,  314,  314,  314,  314,
 /*   840 */   314,  314,   94,   82,  182,  314,  314,  314,  155,  314,
 /*   850 */   154,  314,  174,  314,   94,   90,  182,  314,  314,  314,
 /*   860 */   155,  314,  154,  314,  174,   94,   78,  182,   94,   76,
 /*   870 */   182,  155,  314,  154,  155,  174,  154,  314,  174,  314,
 /*   880 */    94,   44,  182,  314,  314,  314,  155,  314,  154,  314,
 /*   890 */   174,   94,   43,  182,  314,  314,  314,  155,  314,  154,
 /*   900 */   314,  174,
];
const YY_LOOKAHEAD: [YYCODETYPE; 902] = [
 /*     0 */     4,    5,    6,   88,   89,   90,   10,   77,   32,   94,
 /*    10 */     5,   96,   97,   98,    3,   19,   33,    5,   88,   89,
 /*    20 */    90,    9,   26,    2,   94,   29,   96,   31,   98,    0,
 /*    30 */    34,   79,   80,   81,   38,   39,   40,   41,   42,   43,
 /*    40 */    44,   45,   46,   47,   48,    0,   35,   51,   52,   53,
 /*    50 */    54,   30,   56,   57,   58,   50,   60,    4,    5,    6,
 /*    60 */    88,   89,   90,   10,   83,   84,   94,   35,   96,   97,
 /*    70 */    98,   12,   19,   79,   80,   88,   89,   90,   88,   26,
 /*    80 */    90,   94,   29,   96,   97,   98,    5,   34,   98,   83,
 /*    90 */    84,   38,   39,   40,   41,   42,   43,   44,   45,   46,
 /*   100 */    47,   48,   37,    8,   51,   52,   53,   54,   49,   56,
 /*   110 */    57,   58,    4,   60,    3,    4,    5,    6,   79,   80,
 /*   120 */    81,   10,   27,   28,   29,   88,   89,   90,   67,   29,
 /*   130 */    19,   94,   95,   96,   61,   98,   88,   26,   90,   78,
 /*   140 */    29,   35,   88,   89,   90,    5,   98,   20,   94,   95,
 /*   150 */    96,   12,   98,   35,   43,   44,   45,   46,    2,    4,
 /*   160 */     5,    6,   51,   52,   53,   54,   30,   56,   57,   58,
 /*   170 */    10,   60,    3,    4,    5,    6,   88,   30,   90,   10,
 /*   180 */     7,    8,   59,    5,   29,   30,   98,   48,   19,    5,
 /*   190 */    20,   29,   35,   30,   35,   26,    2,    5,   29,   26,
 /*   200 */    27,   28,   29,   43,   44,   45,   46,   30,   53,   54,
 /*   210 */    55,   29,   43,   44,   45,   46,   35,    4,    5,    6,
 /*   220 */    51,   52,   53,   54,    2,   56,   57,   58,   35,   60,
 /*   230 */     4,    5,    6,   88,   89,   90,   10,   30,   35,   94,
 /*   240 */    49,   96,   29,   98,   93,   19,   67,   12,   49,   78,
 /*   250 */     5,   37,   26,   73,   69,   29,   30,   91,   91,   69,
 /*   260 */     5,   73,   67,   76,    3,   49,   53,   54,   55,   43,
 /*   270 */    44,   45,   46,   77,   49,   67,   93,   51,   52,   53,
 /*   280 */    54,   76,   56,   57,   58,   67,   60,    4,    5,    6,
 /*   290 */    88,   89,   90,   10,   36,   67,   94,   99,   96,   99,
 /*   300 */    98,   90,   19,   99,   67,   90,   67,   90,   78,   26,
 /*   310 */    67,   67,   29,  100,  100,   88,   89,   90,  100,  100,
 /*   320 */   100,   94,  100,   96,  100,   98,   43,   44,   45,   46,
 /*   330 */   100,  100,   49,  100,   51,   52,   53,   54,  100,   56,
 /*   340 */    57,   58,  100,   60,    4,    5,    6,  100,  100,  100,
 /*   350 */    10,  100,  100,  100,  100,   88,   89,   90,  100,   19,
 /*   360 */   100,   94,  100,   96,  100,   98,   26,  100,  100,   29,
 /*   370 */   100,  100,   88,   89,   90,  100,  100,  100,   94,  100,
 /*   380 */    96,  100,   98,   43,   44,   45,   46,  100,  100,  100,
 /*   390 */   100,   51,   52,   53,   54,  100,   56,   57,   58,    2,
 /*   400 */    60,   88,   89,   90,    7,    8,  100,   94,  100,   96,
 /*   410 */   100,   98,   15,   16,   17,   18,  100,   20,   21,   22,
 /*   420 */    23,   24,   25,   26,   27,   28,   29,   30,    2,   88,
 /*   430 */    89,   90,  100,    7,    8,   94,  100,   96,  100,   98,
 /*   440 */   100,   15,   16,   17,   18,  100,   20,   21,   22,   23,
 /*   450 */    24,   25,   26,   27,   28,   29,   30,    7,    8,  100,
 /*   460 */   100,  100,  100,  100,  100,   15,   16,   17,   18,  100,
 /*   470 */    20,   21,   22,   23,   24,   25,   26,   27,   28,   29,
 /*   480 */    30,    2,   88,   89,   90,  100,    7,    8,   94,  100,
 /*   490 */    96,  100,   98,  100,   15,   16,   17,   18,  100,   20,
 /*   500 */    21,   22,   23,   24,   25,   26,   27,   28,   29,    2,
 /*   510 */    88,   89,   90,  100,    7,    8,   94,  100,   96,  100,
 /*   520 */    98,  100,   15,   16,   17,   18,  100,   20,   21,   22,
 /*   530 */    23,   24,   25,   26,   27,   28,   29,    7,    8,  100,
 /*   540 */   100,  100,   12,  100,  100,   15,   16,   17,   18,  100,
 /*   550 */    20,   21,   22,   23,   24,   25,   26,   27,   28,   29,
 /*   560 */     7,    8,  100,  100,  100,  100,  100,  100,   15,   16,
 /*   570 */    17,   18,  100,   20,   21,   22,   23,   24,   25,   26,
 /*   580 */    27,   28,   29,  100,  100,  100,   88,   89,   90,  100,
 /*   590 */     7,    8,   94,  100,   96,  100,   98,  100,   15,   16,
 /*   600 */    17,   18,   49,   20,   21,   22,   23,   24,   25,   26,
 /*   610 */    27,   28,   29,  100,   63,   64,   65,  100,  100,   68,
 /*   620 */   100,  100,  100,   72,  100,  100,   75,  100,  100,  100,
 /*   630 */   100,  100,  100,   82,  100,  100,   85,   86,   87,   88,
 /*   640 */    89,   90,   64,   65,  100,   94,   68,   96,  100,   98,
 /*   650 */    72,  100,  100,   75,  100,  100,  100,  100,  100,  100,
 /*   660 */    82,  100,  100,   85,   86,   87,   88,   89,   90,   64,
 /*   670 */    65,  100,   94,   68,   96,  100,   98,   72,  100,  100,
 /*   680 */    75,  100,  100,  100,  100,  100,  100,   82,  100,  100,
 /*   690 */    85,   86,   87,   88,   89,   90,   88,   89,   90,   94,
 /*   700 */   100,   96,   94,   98,   96,  100,   98,    7,    8,  100,
 /*   710 */   100,  100,  100,    4,    5,    6,  100,   17,   18,   10,
 /*   720 */    20,   21,   22,   23,   24,   25,   26,   27,   28,   29,
 /*   730 */   100,   88,   89,   90,   88,   89,   90,   94,   29,   96,
 /*   740 */    94,   98,   96,  100,   98,  100,  100,  100,  100,  100,
 /*   750 */   100,  100,   43,   44,   45,   46,  100,  100,  100,  100,
 /*   760 */     7,    8,   53,   54,  100,   56,   57,  100,  100,   60,
 /*   770 */   100,   18,  100,   20,   21,   22,   23,   24,   25,   26,
 /*   780 */    27,   28,   29,    7,    8,   88,   89,   90,  100,  100,
 /*   790 */   100,   94,  100,   96,  100,   98,   20,   21,   22,   23,
 /*   800 */    24,   25,   26,   27,   28,   29,   88,   89,   90,   88,
 /*   810 */    89,   90,   94,  100,   96,   94,   98,   96,  100,   98,
 /*   820 */    88,   89,   90,   88,   89,   90,   94,  100,   96,   94,
 /*   830 */    98,   96,  100,   98,  100,  100,  100,  100,  100,  100,
 /*   840 */   100,  100,   88,   89,   90,  100,  100,  100,   94,  100,
 /*   850 */    96,  100,   98,  100,   88,   89,   90,  100,  100,  100,
 /*   860 */    94,  100,   96,  100,   98,   88,   89,   90,   88,   89,
 /*   870 */    90,   94,  100,   96,   94,   98,   96,  100,   98,  100,
 /*   880 */    88,   89,   90,  100,  100,  100,   94,  100,   96,  100,
 /*   890 */    98,   88,   89,   90,  100,  100,  100,   94,  100,   96,
 /*   900 */   100,   98,
];
const YY_SHIFT_USE_DFLT: i32 = -25;
const YY_SHIFT_COUNT: i32 = 130;
const YY_SHIFT_MIN: i32 = -24;
const YY_SHIFT_MAX: i32 = 776;
const YY_SHIFT_OFST: [i16; 131] = [
 /*     0 */    -4,   53,   53,  283,  226,  340,  340,  340,  340,  169,
 /*    10 */   111,  340,  340,  340,  340,  340,  340,  340,  340,  340,
 /*    20 */   340,  340,  340,  340,  340,  340,  340,  340,  340,  340,
 /*    30 */   340,  340,  340,  340,  709,  709,  709,  155,  213,  213,
 /*    40 */    59,  258,  258,  530,  530,  530,  553,  530,  160,  160,
 /*    50 */   160,  139,   11,   12,   12,   12,  235,  225,  235,  216,
 /*    60 */   261,  255,  235,  255,  245,  214,  214,  245,  199,  235,
 /*    70 */   191,  426,  397,  507,  479,  450,  583,  583,  583,  583,
 /*    80 */   700,  700,  753,  753,  776,  776,  776,  776,  173,  173,
 /*    90 */    95,   95,   95,   21,    5,  207,  203,  193,  222,  181,
 /*   100 */   177,  182,  192,  194,  159,  157,  163,  162,  184,  170,
 /*   110 */   178,  147,  123,  136,  156,  118,  127,  140,  106,   73,
 /*   120 */   100,  100,  100,  108,   65,   81,   32,   45,   29,  -17,
 /*   130 */   -24,
];
const YY_REDUCE_USE_DFLT: i32 = -86;
const YY_REDUCE_COUNT: i32 = 70;
const YY_REDUCE_MIN: i32 = -85;
const YY_REDUCE_MAX: i32 = 803;
const YY_REDUCE_OFST: [i16; 71] = [
 /*     0 */   551,  605,  578,  -70,  -13,   54,   37,  -28,  -85,  721,
 /*    10 */   803,  792,  780,  777,  766,  754,  735,  732,  721,  718,
 /*    20 */   697,  646,  643,  608,  498,  422,  394,  341,  313,  284,
 /*    30 */   267,  227,  202,  145,   88,   48,  -10,   39,  -48,   -6,
 /*    40 */    61,    6,  -19,  244,  243,  239,  230,  237,  217,  215,
 /*    50 */   211,  228,  205,  204,  200,  198,  218,  183,  208,  196,
 /*    60 */   187,  188,  195,  180,  190,  167,  166,  185,  171,  179,
 /*    70 */   151,
];
const YY_DEFAULT: [YYACTIONTYPE; 203] = [
 /*     0 */   205,  205,  205,  312,  312,  302,  302,  312,  312,  312,
 /*    10 */   312,  312,  312,  312,  312,  312,  312,  312,  312,  312,
 /*    20 */   312,  312,  312,  312,  312,  312,  312,  312,  312,  312,
 /*    30 */   312,  312,  312,  312,  312,  312,  312,  312,  312,  312,
 /*    40 */   312,  216,  216,  312,  312,  312,  312,  312,  312,  312,
 /*    50 */   312,  312,  312,  309,  309,  309,  312,  312,  312,  312,
 /*    60 */   244,  237,  312,  237,  226,  229,  229,  226,  258,  312,
 /*    70 */   312,  312,  312,  303,  306,  312,  222,  221,  220,  219,
 /*    80 */   283,  282,  273,  281,  289,  288,  287,  286,  285,  284,
 /*    90 */   275,  277,  276,  312,  290,  312,  312,  312,  238,  312,
 /*   100 */   312,  312,  312,  227,  312,  312,  312,  312,  312,  312,
 /*   110 */   312,  312,  312,  312,  269,  312,  312,  312,  312,  312,
 /*   120 */   280,  279,  278,  312,  312,  312,  312,  312,  312,  312,
 /*   130 */   312,  250,  248,  255,  254,  247,  246,  242,  245,  243,
 /*   140 */   241,  240,  239,  236,  228,  230,  225,  224,  223,  305,
 /*   150 */   307,  301,  304,  291,  272,  271,  268,  270,  267,  266,
 /*   160 */   265,  264,  263,  262,  261,  260,  259,  257,  256,  253,
 /*   170 */   252,  311,  310,  308,  300,  299,  298,  297,  296,  295,
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

const YY_RULE_INFO: [YYCODETYPE; 109] = [
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
          | 99 /* list_items ::= */
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
 (YYMinorType::YY117(yy1),YYMinorType::YY195(yy3),) => {

	yyres = Val::Tuple(vec![Val::id(yy1.data), Val::Type(yy3)]);

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
            17 /* let_stmt ::= Fork ID EQ expr */
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
 (YYMinorType::YY117(yy1),YYMinorType::YY24(yy3),YYMinorType::YY195(yy5),YYMinorType::YY24(yy6),) => {

	let id = Val::id(yy1.data);
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
 (YYMinorType::YY117(yy1),YYMinorType::YY24(yy3),YYMinorType::YY195(yy5),YYMinorType::YY24(yy6),) => {

	let id = Val::id(yy1.data);
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
 (YYMinorType::YY117(yy0),YYMinorType::YY195(yy1),) => {

	yyres = list::singleton(sexpr::id_with_type(yy0.data, yy1));

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
 (YYMinorType::YY117(yy0),YYMinorType::YY195(yy1),YYMinorType::YY24(yy3),) => {

	yyres = list::cons(sexpr::id_with_type(yy0.data, yy1), yy3);

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
 (YYMinorType::YY117(yy0),) => {

    yyres = list::singleton(Val::id(yy0.data));

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
 (YYMinorType::YY117(yy0),YYMinorType::YY24(yy2),) => {

    yyres = list::cons(Val::id(yy0.data), yy2);

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
            45 /* expr ::= expr LPAREN RPAREN */
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
            46 /* expr ::= expr LPAREN expr RPAREN */
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
            47 /* expr ::= expr LPAREN tuple_args RPAREN */
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
            48 /* expr ::= term ID term */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY24(yy0),YYMinorType::YY117(yy1),YYMinorType::YY24(yy2),) => {

	yyres = sexpr::binaryop(yy1.data, yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy0),YYMinorType::YY24(yy2),) => {

	/* yyres = Val::binaryop(yy0, yy2, D); */
	yyres = Val::Void;

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy1),) => {

    verbose_out!("parsed case expr\n");
	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy1),YYMinorType::YY24(yy2),YYMinorType::YY24(yy5),) => {

    verbose_out!("found cases base\n");
    yyres = sexpr::casex(yy1, yy2, yy5);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy1),YYMinorType::YY24(yy2),YYMinorType::YY24(yy3),) => {

    verbose_out!("found extra case\n");
    yyres = sexpr::casex(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy1),YYMinorType::YY24(yy2),) => {

    verbose_out!("parsed match expr\n");
    yyres = sexpr::match_expr(yy1, yy2);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy1),YYMinorType::YY24(yy2),YYMinorType::YY24(yy3),) => {

    verbose_out!("found cases base\n");
    yyres = sexpr::match_case(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy1),YYMinorType::YY24(yy2),) => {

    verbose_out!("parsed base match case\n");
    yyres = sexpr::match_case(yy1, yy2, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            57 /* pexpr ::= INT */
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
 YYMinorType::YY24(yyres)
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
 YYMinorType::YY24(yyres)
}
            ,
            60 /* pexpr ::= HASHTAG */
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
            61 /* pexpr ::= ID */
          | 90 /* term ::= ID */
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
            62 /* pexpr ::= UNDERSCORE */
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
 YYMinorType::YY24(yyres)
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
 (YYMinorType::YY24(yy1),) => {

	yyres = Val::Tuple(vec![yy1]);

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            65 /* ptuple ::= LPAREN pargs RPAREN */
          | 102 /* tuple ::= LPAREN tuple_args RPAREN */
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
            66 /* pargs ::= pexpr COMMA pexpr */
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
            67 /* pargs ::= pexpr COMMA pargs */
          | 101 /* list_items ::= expr COMMA list_items */
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
            72 /* expr ::= MINUS expr */
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
            88 /* term ::= LPAREN expr RPAREN */
          | 98 /* list ::= SquareL list_items SquareR */
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
            89 /* term ::= typex */
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
            91 /* term ::= VOID */
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
            92 /* term ::= DollarQuestion */
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
            93 /* term ::= INT */
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
            96 /* term ::= HASHTAG */
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
            100 /* list_items ::= expr */
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
            103 /* tuple_args ::= expr COMMA expr */
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
            104 /* tuple_args ::= expr COMMA tuple_args */
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
            105 /* strexpr ::= StrOpen strlist StrClose */
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
            106 /* strlist ::= */
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
            107 /* strlist ::= StrLit strlist */
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
            108 /* strlist ::= ID strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY117(yy0),YYMinorType::YY24(yy1),) => {

	yyres = list::cons(Val::id(yy0.data), yy1);

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

