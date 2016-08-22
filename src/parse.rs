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
const YYNSTATE: i32 = 210;
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
 /*     0 */   167,  160,  159,  173,  170,   26,  141,   53,  200,  115,
 /*    10 */    43,   29,  126,  188,  179,  103,   15,   93,  188,  125,
 /*    20 */   151,   43,  210,   28,   25,  162,  130,  102,    8,   41,
 /*    30 */   117,   52,  206,  112,  109,  107,  105,  204,  203,  202,
 /*    40 */   201,  100,    3,   35,   70,   27,  169,  168,  195,   34,
 /*    50 */   172,  171,    6,  199,   42,   57,  167,  160,  159,  173,
 /*    60 */   170,  196,  190,  174,  200,  199,   37,    4,  129,   71,
 /*    70 */   174,  166,   15,  175,  177,  174,  176,  119,  166,    4,
 /*    80 */   129,   76,  174,  166,    8,    1,  177,    1,  176,  155,
 /*    90 */   166,   69,  188,  204,  203,  202,  201,   37,  180,  178,
 /*   100 */    70,   27,  169,  168,  165,   34,  172,  171,    6,  199,
 /*   110 */    42,   55,  167,  160,  159,  173,  170,  162,   38,   11,
 /*   120 */   200,   41,  199,    4,  129,   75,  174,  161,   15,  130,
 /*   130 */   177,  120,  176,  156,  166,  154,    4,  129,   75,  174,
 /*   140 */     8,  121,   67,  177,  157,  176,  153,  166,  200,  204,
 /*   150 */   203,  202,  201,  122,   39,  152,   70,   27,  169,  168,
 /*   160 */    51,   34,  172,  171,    6,  113,   42,  167,  160,  159,
 /*   170 */   173,  170,  164,   39,  111,  200,  199,  204,  203,  202,
 /*   180 */   201,  184,  110,   15,  183,  187,  163,   39,   14,  108,
 /*   190 */     4,  129,   72,  174,   13,    8,  198,  177,  106,  176,
 /*   200 */    94,  166,   12,   66,  204,  203,  202,  201,   65,   36,
 /*   210 */   181,   70,   27,  169,  168,  147,   34,  172,  171,    6,
 /*   220 */    63,   42,  167,  160,  159,  173,  170,  186,  185,  182,
 /*   230 */   200,  199,  146,   62,   61,   95,  143,   60,   15,  140,
 /*   240 */   137,  132,  128,   30,   68,    4,  129,   47,  174,    1,
 /*   250 */     8,   38,  177,   64,  176,  189,  166,   40,  104,  204,
 /*   260 */   203,  202,  201,   50,  116,   10,   70,   27,  169,  168,
 /*   270 */    99,   34,  172,  171,    6,  101,   42,  167,  160,  159,
 /*   280 */   173,  170,  144,  199,   97,  200,   98,  142,  139,   53,
 /*   290 */   136,  184,   10,   15,  183,  187,  135,    4,  129,   91,
 /*   300 */   174,  133,    9,  134,  177,    8,  176,   96,  166,   44,
 /*   310 */   114,  138,  150,  145,  204,  203,  202,  201,   56,   36,
 /*   320 */   127,   70,   27,  169,  168,    7,   34,  172,  171,    6,
 /*   330 */    54,   42,   59,   33,   26,   58,  325,  186,  185,  182,
 /*   340 */    23,   22,   24,  205,  325,   17,   16,   19,   18,   21,
 /*   350 */    20,   32,   28,   25,    7,  130,  325,  197,  325,  325,
 /*   360 */   325,  325,   33,   26,  325,  325,  325,  325,  325,   23,
 /*   370 */    22,   24,  205,  325,   17,   16,   19,   18,   21,   20,
 /*   380 */    32,   28,   25,    7,  130,  325,  158,  325,  325,  325,
 /*   390 */   325,   33,   26,  325,  325,  325,  325,  325,   23,   22,
 /*   400 */    24,  205,  325,   17,   16,   19,   18,   21,   20,   32,
 /*   410 */    28,   25,    5,  130,  325,  325,  325,  325,  325,  325,
 /*   420 */    33,   26,  325,  325,  325,  325,  325,   23,   22,   24,
 /*   430 */   205,  325,   17,   16,   19,   18,   21,   20,   32,   28,
 /*   440 */    25,  325,  130,   33,   26,  325,  325,  325,  325,  325,
 /*   450 */    23,   22,   24,  205,  325,   17,   16,   19,   18,   21,
 /*   460 */    20,   32,   28,   25,  325,  130,  325,  149,   33,   26,
 /*   470 */   325,  325,  325,  325,  325,   23,   22,   24,  205,  325,
 /*   480 */    17,   16,   19,   18,   21,   20,   32,   28,   25,  325,
 /*   490 */   130,  325,  158,   33,   26,  325,  325,  325,    1,  325,
 /*   500 */    23,   22,   24,  205,  325,   17,   16,   19,   18,   21,
 /*   510 */    20,   32,   28,   25,  325,  130,   33,   26,  325,  325,
 /*   520 */   325,  325,  325,   23,   22,   24,  205,  325,   17,   16,
 /*   530 */    19,   18,   21,   20,   32,   28,   25,  325,  130,  324,
 /*   540 */   131,    2,  325,  193,  325,  325,  325,  192,  325,  199,
 /*   550 */   325,  191,  325,  325,   38,  325,  325,  325,  208,  325,
 /*   560 */   325,  207,  194,    4,  129,   80,  174,  325,  325,  325,
 /*   570 */   177,  325,  176,  325,  166,   33,   26,  325,  325,  325,
 /*   580 */   325,  325,   23,   22,   24,  205,  325,   17,   16,   19,
 /*   590 */    18,   21,   20,   32,   28,   25,  325,  130,  209,    2,
 /*   600 */   325,  193,  325,  325,  325,  192,  325,  199,  325,  191,
 /*   610 */   325,  325,  325,  325,  325,  325,  208,  325,  325,  207,
 /*   620 */   194,    4,  129,   80,  174,  325,  148,    2,  177,  193,
 /*   630 */   176,  325,  166,  192,  325,  199,  325,  191,  325,  325,
 /*   640 */   325,  325,  325,  325,  208,  325,  325,  207,  194,    4,
 /*   650 */   129,   80,  174,  325,  325,  325,  177,  325,  176,  325,
 /*   660 */   166,   33,   26,  325,  167,  325,  325,  173,  170,  325,
 /*   670 */    24,  205,  200,   17,   16,   19,   18,   21,   20,   32,
 /*   680 */    28,   25,  325,  130,  325,  325,  325,   33,   26,  325,
 /*   690 */   325,  325,   31,  325,  325,  325,  325,  325,  325,  325,
 /*   700 */   325,  204,  203,  202,  201,   32,   28,   25,  325,  130,
 /*   710 */   169,  168,   33,   26,  172,  171,  325,  325,   42,  325,
 /*   720 */   325,  325,  205,  325,   17,   16,   19,   18,   21,   20,
 /*   730 */    32,   28,   25,  325,  130,   33,   26,  325,  325,  325,
 /*   740 */   325,  325,  325,  325,  199,  325,  325,  323,  323,  323,
 /*   750 */   323,   21,   20,   32,   28,   25,  199,  130,    4,  129,
 /*   760 */    92,  174,  325,  325,  325,  177,  325,  176,  325,  166,
 /*   770 */     4,  129,   73,  174,  325,  199,  325,  177,  325,  176,
 /*   780 */   325,  166,  325,  325,  325,  325,  325,  199,  325,    4,
 /*   790 */   129,   49,  174,  325,  325,  325,  177,  325,  176,  199,
 /*   800 */   166,    4,  129,   79,  174,  325,  325,  325,  177,  325,
 /*   810 */   176,  325,  166,    4,  129,  118,  174,  199,  325,  325,
 /*   820 */   177,  325,  176,  325,  166,  325,  325,  325,  325,  199,
 /*   830 */   325,    4,  129,   48,  174,  325,  325,  325,  177,  325,
 /*   840 */   176,  325,  166,    4,  129,  124,  174,  325,  199,  325,
 /*   850 */   177,  325,  176,  325,  166,  325,  325,  325,  325,  325,
 /*   860 */   199,  325,    4,  129,  123,  174,  325,  325,  325,  177,
 /*   870 */   325,  176,  199,  166,    4,  129,   84,  174,  325,  325,
 /*   880 */   325,  177,  325,  176,  325,  166,    4,  129,   82,  174,
 /*   890 */   199,  325,  325,  177,  325,  176,  325,  166,  325,  325,
 /*   900 */   325,  325,  199,  325,    4,  129,   81,  174,  325,  325,
 /*   910 */   325,  177,  325,  176,  325,  166,    4,  129,   90,  174,
 /*   920 */   325,  199,  325,  177,  325,  176,  325,  166,  325,  325,
 /*   930 */   325,  325,  325,  199,  325,    4,  129,   89,  174,  325,
 /*   940 */   325,  325,  177,  325,  176,  199,  166,    4,  129,   88,
 /*   950 */   174,  325,  325,  325,  177,  325,  176,  325,  166,    4,
 /*   960 */   129,   87,  174,  199,  325,  325,  177,  325,  176,  325,
 /*   970 */   166,  325,  325,  325,  325,  199,  325,    4,  129,   86,
 /*   980 */   174,  325,  325,  325,  177,  325,  176,  325,  166,    4,
 /*   990 */   129,   85,  174,  325,  199,  325,  177,  325,  176,  325,
 /*  1000 */   166,  325,  325,  325,  325,  325,  199,  325,    4,  129,
 /*  1010 */    83,  174,  325,  325,  325,  177,  325,  176,  199,  166,
 /*  1020 */     4,  129,   74,  174,  325,  325,  325,  177,  325,  176,
 /*  1030 */   325,  166,    4,  129,   78,  174,  199,  325,  325,  177,
 /*  1040 */   325,  176,  325,  166,  325,  325,  325,  325,  199,  325,
 /*  1050 */     4,  129,   77,  174,  325,  325,  325,  177,  325,  176,
 /*  1060 */   325,  166,    4,  129,   46,  174,  325,  199,  325,  177,
 /*  1070 */   325,  176,  325,  166,  325,  325,  325,  325,  325,  325,
 /*  1080 */   325,    4,  129,   45,  174,  325,  325,  325,  177,  325,
 /*  1090 */   176,  325,  166,
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
 /*   200 */    96,   97,   14,    6,   42,   43,   44,   45,   34,   33,
 /*   210 */    34,   49,   50,   51,   52,    3,   54,   55,   56,   57,
 /*   220 */     2,   59,    5,    6,    7,    8,    9,   51,   52,   53,
 /*   230 */    13,   72,    3,    6,   34,   76,    3,    2,   21,    3,
 /*   240 */     3,   34,   92,   48,   65,   86,   87,   88,   89,   15,
 /*   250 */    33,   48,   93,    8,   95,   77,   97,   90,   67,   42,
 /*   260 */    43,   44,   45,   37,   77,   48,   49,   50,   51,   52,
 /*   270 */    71,   54,   55,   56,   57,   90,   59,    5,    6,    7,
 /*   280 */     8,    9,   67,   72,    8,   13,   65,   71,   75,    4,
 /*   290 */    76,    5,   48,   21,    8,    9,   65,   86,   87,   88,
 /*   300 */    89,   92,   48,   65,   93,   33,   95,   75,   97,   89,
 /*   310 */    32,   65,   89,   89,   42,   43,   44,   45,   65,   33,
 /*   320 */    77,   49,   50,   51,   52,    2,   54,   55,   56,   57,
 /*   330 */    65,   59,   65,   10,   11,   65,  100,   51,   52,   53,
 /*   340 */    17,   18,   19,   20,  100,   22,   23,   24,   25,   26,
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
const YY_SHIFT_COUNT: i32 = 131;
const YY_SHIFT_MIN: i32 = -6;
const YY_SHIFT_MAX: i32 = 725;
const YY_SHIFT_OFST: [i16; 132] = [
 /*     0 */    -5,   -5,   -5,  217,  162,  272,  272,  272,  272,  107,
 /*    10 */    51,  272,  272,  272,  272,  272,  272,  272,  272,  272,
 /*    20 */   272,  272,  272,  272,  272,  272,  272,  272,  272,  272,
 /*    30 */   272,  272,  272,  272,  659,  659,  176,  286,  286,  109,
 /*    40 */    70,   17,   17,  278,  278,  483,  483,  483,  506,  483,
 /*    50 */   135,  135,  135,   72,    3,  234,  254,  234,  244,  285,
 /*    60 */   276,  234,  276,  245,  226,  226,  245,  203,  203,  234,
 /*    70 */   195,  352,  323,  458,  433,  410,  381,  565,  565,  565,
 /*    80 */   565,  651,  651,  702,  702,  725,  725,  725,  725,  677,
 /*    90 */   677,   -6,   -6,   64,  207,  237,  236,  235,  233,  200,
 /*   100 */   227,  218,  229,  212,  174,  197,  188,  190,  180,  181,
 /*   110 */   186,  177,  141,  123,  157,  152,  143,  134,   97,  101,
 /*   120 */    75,  119,   44,   97,   97,   65,   95,   59,   45,   12,
 /*   130 */    24,   22,
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
 /*    40 */   -50,   74,   55,  -62,  -73,  270,  267,  265,  243,  253,
 /*    50 */   224,  223,  220,  246,  232,  238,  209,  231,  214,  213,
 /*    60 */   216,  221,  199,  215,  185,  167,  191,  187,  178,  179,
 /*    70 */   150,
];
const YY_DEFAULT: [YYACTIONTYPE; 210] = [
 /*     0 */   211,  211,  211,  323,  323,  311,  311,  323,  323,  323,
 /*    10 */   323,  323,  323,  323,  323,  323,  323,  323,  323,  323,
 /*    20 */   323,  323,  323,  323,  323,  323,  323,  323,  323,  323,
 /*    30 */   323,  323,  323,  323,  323,  323,  323,  323,  323,  318,
 /*    40 */   323,  318,  318,  224,  224,  323,  323,  323,  323,  323,
 /*    50 */   323,  323,  323,  323,  323,  323,  323,  323,  323,  250,
 /*    60 */   243,  323,  243,  232,  235,  235,  232,  323,  264,  323,
 /*    70 */   323,  323,  323,  323,  323,  312,  315,  228,  227,  220,
 /*    80 */   215,  290,  289,  280,  288,  296,  295,  294,  293,  292,
 /*    90 */   291,  283,  284,  323,  323,  323,  323,  244,  323,  323,
 /*   100 */   323,  233,  323,  323,  323,  323,  323,  323,  323,  323,
 /*   110 */   323,  323,  323,  323,  323,  323,  323,  323,  285,  323,
 /*   120 */   323,  323,  323,  287,  286,  323,  275,  323,  323,  297,
 /*   130 */   323,  323,  257,  261,  260,  253,  252,  248,  251,  249,
 /*   140 */   247,  246,  245,  242,  234,  236,  231,  230,  229,  226,
 /*   150 */   225,  223,  222,  221,  314,  316,  310,  313,  300,  299,
 /*   160 */   298,  322,  321,  320,  319,  317,  309,  308,  307,  306,
 /*   170 */   305,  304,  303,  302,  301,  282,  279,  278,  274,  276,
 /*   180 */   273,  272,  271,  270,  269,  268,  267,  266,  265,  263,
 /*   190 */   262,  219,  218,  217,  216,  259,  258,  256,  255,  254,
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
            20 /* func_stmt ::= Func CALL_ID dfunc_args RPAREN opt_typex block DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp6 = self.yystack.pop().unwrap();
let yyp5 = self.yystack.pop().unwrap();
let yyp4 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,yyp4.minor,yyp5.minor,) {
 (YYMinorType::YY117(yy1),YYMinorType::YY24(yy2),YYMinorType::YY195(yy4),YYMinorType::YY24(yy5),) => {

	let id = Val::id(yy1.data);
	let typ = Val::Type(yy4);
	yyres = sexpr::defunc(id, yy2, typ, yy5)

},    _ => unreachable!() };
 YYMinorType::YY24(yyres)
}
            ,
            21 /* func_stmt ::= Func CALL_ID dfunc_args RPAREN opt_typex match_case DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp6 = self.yystack.pop().unwrap();
let yyp5 = self.yystack.pop().unwrap();
let yyp4 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,yyp4.minor,yyp5.minor,) {
 (YYMinorType::YY117(yy1),YYMinorType::YY24(yy2),YYMinorType::YY195(yy4),YYMinorType::YY24(yy5),) => {

	let id = Val::id(yy1.data);
	let typ = Val::Type(yy4);
    let body = sexpr::match_expr(Val::CallParams, yy5);
	yyres = sexpr::defunc(id, yy2, typ, body)

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
            32 /* macro_stmt ::= MACRO CALL_ID macro_args RPAREN block DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp5 = self.yystack.pop().unwrap();
let yyp4 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,yyp4.minor,) {
 (YYMinorType::YY117(yy1),YYMinorType::YY24(yy2),YYMinorType::YY24(yy4),) => {

    verbose_out!("found macro {:?}\n", yy1);
    yyres = sexpr::new(SexprType::DefMacro,
        list::cons(Val::id(yy1.data),
        list::cons(yy2,
        list::cons(yy4,
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
            89 /* functerm ::= CALL_TYPE_ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY117(yy0),) => {

	yyres = Val::Type(Type::Id(Arc::new(yy0.data)));

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

