#![allow(dead_code)]
#![allow(unused_variables)]
/* TMPL: %include */

use leema::ast::{Ast};
use leema::val::{Val, SexprType, Type};
use leema::list;
use leema::sexpr;
use std::sync::Arc;
/* TMPL: makeheader cruft */


/* TMPL: types */

type YYCODETYPE = i8;
const YYNOCODE: i32 = 89;
type YYACTIONTYPE = u8;
const YYWILDCARD: YYCODETYPE = 0;
enum YYMinorType {
    YY0,
    YY12(String),
    YY68(i64),
    YY72(Val),
    YY75(Type),
    YY125(Ast),
}
const YYNSTATE: i32 = 140;
const YYNRULE: i32 = 78;
const YYERRORSYMBOL: i32 = 0;

//const YY_NO_ACTION: i32 = YYNSTATE+YYNRULE+2;
//const YY_ACCEPT_ACTION: i32 = YYNSTATE+YYNRULE+1;
//const YY_ERROR_ACTION: i32 = YYNSTATE+YYNRULE+1;

/* TMPL: action tables */

#[derive(Debug,PartialEq
)]
pub enum Token {
    EOI, //0
    HASHTAG( String ), //1
    ID( String ), //2
    INT( i64 ), //3
    StrLit( String ), //4
    TYPE_ID( String ), //5
    ASSIGN, //6
    BLOCKARROW, //7
    WHERE, //8
    GIVEN, //9
    COMMA, //10
    OR, //11
    XOR, //12
    AND, //13
    ConcatNewline, //14
    NOT, //15
    LT, //16
    LTEQ, //17
    EQ, //18
    NEQ, //19
    GT, //20
    GTEQ, //21
    PLUS, //22
    MINUS, //23
    TIMES, //24
    DIVIDE, //25
    LPAREN, //26
    RPAREN, //27
    FAILED, //28
    WITH, //29
    CONTEXTID, //30
    NEWLINE, //31
    DT, //32
    FAIL, //33
    Let, //34
    DEFINE, //35
    Fork, //36
    DollarGT, //37
    CurlyL, //38
    CurlyR, //39
    Func, //40
    COLON, //41
    TYPE_INT, //42
    TYPE_STR, //43
    TYPE_BOOL, //44
    MACRO, //45
    DOLLAR, //46
    IF, //47
    ELSE, //48
    VOID, //49
    DollarQuestion, //50
    True, //51
    False, //52
    SquareL, //53
    SquareR, //54
    StrOpen, //55
    StrClose, //56
}
pub const TOKEN_EOI: i32 = 0;
pub const TOKEN_HASHTAG: i32 = 1;
pub const TOKEN_ID: i32 = 2;
pub const TOKEN_INT: i32 = 3;
pub const TOKEN_StrLit: i32 = 4;
pub const TOKEN_TYPE_ID: i32 = 5;
pub const TOKEN_ASSIGN: i32 = 6;
pub const TOKEN_BLOCKARROW: i32 = 7;
pub const TOKEN_WHERE: i32 = 8;
pub const TOKEN_GIVEN: i32 = 9;
pub const TOKEN_COMMA: i32 = 10;
pub const TOKEN_OR: i32 = 11;
pub const TOKEN_XOR: i32 = 12;
pub const TOKEN_AND: i32 = 13;
pub const TOKEN_ConcatNewline: i32 = 14;
pub const TOKEN_NOT: i32 = 15;
pub const TOKEN_LT: i32 = 16;
pub const TOKEN_LTEQ: i32 = 17;
pub const TOKEN_EQ: i32 = 18;
pub const TOKEN_NEQ: i32 = 19;
pub const TOKEN_GT: i32 = 20;
pub const TOKEN_GTEQ: i32 = 21;
pub const TOKEN_PLUS: i32 = 22;
pub const TOKEN_MINUS: i32 = 23;
pub const TOKEN_TIMES: i32 = 24;
pub const TOKEN_DIVIDE: i32 = 25;
pub const TOKEN_LPAREN: i32 = 26;
pub const TOKEN_RPAREN: i32 = 27;
pub const TOKEN_FAILED: i32 = 28;
pub const TOKEN_WITH: i32 = 29;
pub const TOKEN_CONTEXTID: i32 = 30;
pub const TOKEN_NEWLINE: i32 = 31;
pub const TOKEN_DT: i32 = 32;
pub const TOKEN_FAIL: i32 = 33;
pub const TOKEN_Let: i32 = 34;
pub const TOKEN_DEFINE: i32 = 35;
pub const TOKEN_Fork: i32 = 36;
pub const TOKEN_DollarGT: i32 = 37;
pub const TOKEN_CurlyL: i32 = 38;
pub const TOKEN_CurlyR: i32 = 39;
pub const TOKEN_Func: i32 = 40;
pub const TOKEN_COLON: i32 = 41;
pub const TOKEN_TYPE_INT: i32 = 42;
pub const TOKEN_TYPE_STR: i32 = 43;
pub const TOKEN_TYPE_BOOL: i32 = 44;
pub const TOKEN_MACRO: i32 = 45;
pub const TOKEN_DOLLAR: i32 = 46;
pub const TOKEN_IF: i32 = 47;
pub const TOKEN_ELSE: i32 = 48;
pub const TOKEN_VOID: i32 = 49;
pub const TOKEN_DollarQuestion: i32 = 50;
pub const TOKEN_True: i32 = 51;
pub const TOKEN_False: i32 = 52;
pub const TOKEN_SquareL: i32 = 53;
pub const TOKEN_SquareR: i32 = 54;
pub const TOKEN_StrOpen: i32 = 55;
pub const TOKEN_StrClose: i32 = 56;
#[inline]
fn token_major(t: &Token) -> i32 {
    match t {
        &Token::EOI => 0,
        &Token::HASHTAG(_) => TOKEN_HASHTAG,
        &Token::ID(_) => TOKEN_ID,
        &Token::INT(_) => TOKEN_INT,
        &Token::StrLit(_) => TOKEN_StrLit,
        &Token::TYPE_ID(_) => TOKEN_TYPE_ID,
        &Token::ASSIGN => TOKEN_ASSIGN,
        &Token::BLOCKARROW => TOKEN_BLOCKARROW,
        &Token::WHERE => TOKEN_WHERE,
        &Token::GIVEN => TOKEN_GIVEN,
        &Token::COMMA => TOKEN_COMMA,
        &Token::OR => TOKEN_OR,
        &Token::XOR => TOKEN_XOR,
        &Token::AND => TOKEN_AND,
        &Token::ConcatNewline => TOKEN_ConcatNewline,
        &Token::NOT => TOKEN_NOT,
        &Token::LT => TOKEN_LT,
        &Token::LTEQ => TOKEN_LTEQ,
        &Token::EQ => TOKEN_EQ,
        &Token::NEQ => TOKEN_NEQ,
        &Token::GT => TOKEN_GT,
        &Token::GTEQ => TOKEN_GTEQ,
        &Token::PLUS => TOKEN_PLUS,
        &Token::MINUS => TOKEN_MINUS,
        &Token::TIMES => TOKEN_TIMES,
        &Token::DIVIDE => TOKEN_DIVIDE,
        &Token::LPAREN => TOKEN_LPAREN,
        &Token::RPAREN => TOKEN_RPAREN,
        &Token::FAILED => TOKEN_FAILED,
        &Token::WITH => TOKEN_WITH,
        &Token::CONTEXTID => TOKEN_CONTEXTID,
        &Token::NEWLINE => TOKEN_NEWLINE,
        &Token::DT => TOKEN_DT,
        &Token::FAIL => TOKEN_FAIL,
        &Token::Let => TOKEN_Let,
        &Token::DEFINE => TOKEN_DEFINE,
        &Token::Fork => TOKEN_Fork,
        &Token::DollarGT => TOKEN_DollarGT,
        &Token::CurlyL => TOKEN_CurlyL,
        &Token::CurlyR => TOKEN_CurlyR,
        &Token::Func => TOKEN_Func,
        &Token::COLON => TOKEN_COLON,
        &Token::TYPE_INT => TOKEN_TYPE_INT,
        &Token::TYPE_STR => TOKEN_TYPE_STR,
        &Token::TYPE_BOOL => TOKEN_TYPE_BOOL,
        &Token::MACRO => TOKEN_MACRO,
        &Token::DOLLAR => TOKEN_DOLLAR,
        &Token::IF => TOKEN_IF,
        &Token::ELSE => TOKEN_ELSE,
        &Token::VOID => TOKEN_VOID,
        &Token::DollarQuestion => TOKEN_DollarQuestion,
        &Token::True => TOKEN_True,
        &Token::False => TOKEN_False,
        &Token::SquareL => TOKEN_SquareL,
        &Token::SquareR => TOKEN_SquareR,
        &Token::StrOpen => TOKEN_StrOpen,
        &Token::StrClose => TOKEN_StrClose,
    }
}
#[inline]
fn token_minor(t: Token) -> YYMinorType {
  match t {
        Token::HASHTAG(x) => YYMinorType::YY12(x),
        Token::ID(x) => YYMinorType::YY12(x),
        Token::INT(x) => YYMinorType::YY68(x),
        Token::StrLit(x) => YYMinorType::YY12(x),
        Token::TYPE_ID(x) => YYMinorType::YY12(x),
        _ => YYMinorType::YY0
  }
}
const YY_ACTTAB_COUNT: i32 = 528;
const YY_ACTION: [YYACTIONTYPE; 528] = [
 /*     0 */   119,   85,  122,   14,   13,   16,   15,   27,   25,   23,
 /*    10 */    22,   70,   33,   89,   24,   31,  113,   83,   37,  112,
 /*    20 */    38,  111,   12,  118,   88,    7,   31,  140,  108,  118,
 /*    30 */   138,  133,   86,   80,   29,   78,    9,  141,    3,   45,
 /*    40 */    26,  119,   85,  122,   40,   32,    8,  117,  124,  123,
 /*    50 */   121,  120,    6,   29,   39,   24,  105,   70,   33,   30,
 /*    60 */    11,   96,   92,   12,   79,  112,    7,  111,   90,  118,
 /*    70 */    30,   46,  133,   86,   80,  104,   78,    9,   77,   44,
 /*    80 */    45,   10,   23,   22,  130,   40,  103,    8,   43,  124,
 /*    90 */   123,  121,  120,    6,   41,   39,  118,   26,   99,   98,
 /*   100 */    97,   20,   19,   21,  127,   35,   18,   17,   14,   13,
 /*   110 */    16,   15,   27,   25,   23,   22,  218,  218,  218,  218,
 /*   120 */    27,   25,   23,   22,   20,   19,   21,  127,   46,   18,
 /*   130 */    17,   14,   13,   16,   15,   27,   25,   23,   22,   91,
 /*   140 */   128,   20,   19,   21,  127,    2,   18,   17,   14,   13,
 /*   150 */    16,   15,   27,   25,   23,   22,  139,  107,    5,   20,
 /*   160 */    19,   21,  127,    1,   18,   17,   14,   13,   16,   15,
 /*   170 */    27,   25,   23,   22,   20,   19,   21,  127,  102,   18,
 /*   180 */    17,   14,   13,   16,   15,   27,   25,   23,   22,  119,
 /*   190 */    85,  122,   70,   48,   75,   74,   42,   36,  126,   34,
 /*   200 */   112,   82,  111,   24,  118,  119,   85,  122,  219,   87,
 /*   210 */   118,   12,   73,   47,    7,  129,  114,  132,  131,   24,
 /*   220 */    95,  136,  135,  134,   70,   52,   94,   12,  118,  101,
 /*   230 */     7,   84,  112,  116,  111,    8,  118,  124,  123,  121,
 /*   240 */   120,    6,  115,   39,   93,  100,   72,  220,   26,  220,
 /*   250 */   220,    8,  220,  124,  123,  121,  120,    6,  220,   39,
 /*   260 */   220,  220,  220,  220,  220,   21,  127,  220,   18,   17,
 /*   270 */    14,   13,   16,   15,   27,   25,   23,   22,  137,   46,
 /*   280 */   220,  220,   47,  220,  220,  220,  132,  131,    4,  220,
 /*   290 */   136,  135,  134,   70,   52,  119,  125,  122,   76,  220,
 /*   300 */   220,  112,   47,  111,  220,  118,  132,  131,  220,  220,
 /*   310 */   136,  135,  134,   70,   52,  220,  220,  220,  220,  220,
 /*   320 */    28,  112,  220,  111,  220,  118,  127,  220,   18,   17,
 /*   330 */    14,   13,   16,   15,   27,   25,   23,   22,  220,  220,
 /*   340 */    66,   49,  220,  124,  123,  121,  120,  220,  112,   39,
 /*   350 */   111,   81,  118,   70,   48,  220,  220,  220,  220,  220,
 /*   360 */   220,  112,  106,  111,  220,  118,   66,   50,  220,  220,
 /*   370 */   220,   70,   49,  220,  112,  220,  111,   71,  118,  112,
 /*   380 */   220,  111,  220,  118,  220,   70,   67,  220,   70,   55,
 /*   390 */   220,  220,  220,  112,  220,  111,  112,  118,  111,  220,
 /*   400 */   118,   70,   68,  220,   70,   59,  220,   70,  110,  112,
 /*   410 */   220,  111,  112,  118,  111,  112,  118,  111,  220,  118,
 /*   420 */    70,  109,  220,   70,   58,  220,   70,   57,  112,  220,
 /*   430 */   111,  112,  118,  111,  112,  118,  111,  220,  118,  220,
 /*   440 */    70,   56,  220,  220,  220,  220,   70,   65,  112,  220,
 /*   450 */   111,  220,  118,  220,  112,  220,  111,  220,  118,  220,
 /*   460 */    70,   64,  220,   70,   63,  220,  220,  220,  112,  220,
 /*   470 */   111,  112,  118,  111,  220,  118,   70,   62,  220,   70,
 /*   480 */    61,  220,   70,   60,  112,  220,  111,  112,  118,  111,
 /*   490 */   112,  118,  111,  220,  118,   70,   69,  220,   70,   54,
 /*   500 */   220,   70,   53,  112,  220,  111,  112,  118,  111,  112,
 /*   510 */   118,  111,  220,  118,  220,   70,   51,  220,  220,  220,
 /*   520 */   220,  220,  220,  112,  220,  111,  220,  118,
];
const YY_LOOKAHEAD: [YYCODETYPE; 528] = [
 /*     0 */     1,    2,    3,   18,   19,   20,   21,   22,   23,   24,
 /*    10 */    25,   74,   75,   29,   15,    2,   79,   74,    2,   82,
 /*    20 */     4,   84,   23,   86,   30,   26,    2,    0,   85,   86,
 /*    30 */    31,   32,   33,   34,   10,   36,   37,    0,   26,   40,
 /*    40 */     7,    1,    2,    3,   45,    1,   47,   56,   49,   50,
 /*    50 */    51,   52,   53,   10,   55,   15,   54,   74,   75,   46,
 /*    60 */    35,    5,   79,   23,    2,   82,   26,   84,   28,   86,
 /*    70 */    46,   38,   32,   33,   34,   27,   36,   37,    2,   26,
 /*    80 */    40,   35,   24,   25,   74,   45,   39,   47,   27,   49,
 /*    90 */    50,   51,   52,   53,   10,   55,   86,    7,   42,   43,
 /*   100 */    44,   11,   12,   13,   14,   48,   16,   17,   18,   19,
 /*   110 */    20,   21,   22,   23,   24,   25,   18,   19,   20,   21,
 /*   120 */    22,   23,   24,   25,   11,   12,   13,   14,   38,   16,
 /*   130 */    17,   18,   19,   20,   21,   22,   23,   24,   25,   27,
 /*   140 */    27,   11,   12,   13,   14,   60,   16,   17,   18,   19,
 /*   150 */    20,   21,   22,   23,   24,   25,   31,   27,   10,   11,
 /*   160 */    12,   13,   14,   60,   16,   17,   18,   19,   20,   21,
 /*   170 */    22,   23,   24,   25,   11,   12,   13,   14,   70,   16,
 /*   180 */    17,   18,   19,   20,   21,   22,   23,   24,   25,    1,
 /*   190 */     2,    3,   74,   75,    2,   69,    2,   77,   74,   41,
 /*   200 */    82,   83,   84,   15,   86,    1,    2,    3,   58,   59,
 /*   210 */    86,   23,   77,   63,   26,   27,   74,   67,   68,   15,
 /*   220 */    69,   71,   72,   73,   74,   75,   70,   23,   86,   65,
 /*   230 */    26,   87,   82,   87,   84,   47,   86,   49,   50,   51,
 /*   240 */    52,   53,   87,   55,   65,   76,   65,   88,    7,   88,
 /*   250 */    88,   47,   88,   49,   50,   51,   52,   53,   88,   55,
 /*   260 */    88,   88,   88,   88,   88,   13,   14,   88,   16,   17,
 /*   270 */    18,   19,   20,   21,   22,   23,   24,   25,   59,   38,
 /*   280 */    88,   88,   63,   88,   88,   88,   67,   68,   47,   88,
 /*   290 */    71,   72,   73,   74,   75,    1,    2,    3,   59,   88,
 /*   300 */    88,   82,   63,   84,   88,   86,   67,   68,   88,   88,
 /*   310 */    71,   72,   73,   74,   75,   88,   88,   88,   88,   88,
 /*   320 */    26,   82,   88,   84,   88,   86,   14,   88,   16,   17,
 /*   330 */    18,   19,   20,   21,   22,   23,   24,   25,   88,   88,
 /*   340 */    74,   75,   88,   49,   50,   51,   52,   88,   82,   55,
 /*   350 */    84,   85,   86,   74,   75,   88,   88,   88,   88,   88,
 /*   360 */    88,   82,   83,   84,   88,   86,   74,   75,   88,   88,
 /*   370 */    88,   74,   75,   88,   82,   88,   84,   85,   86,   82,
 /*   380 */    88,   84,   88,   86,   88,   74,   75,   88,   74,   75,
 /*   390 */    88,   88,   88,   82,   88,   84,   82,   86,   84,   88,
 /*   400 */    86,   74,   75,   88,   74,   75,   88,   74,   75,   82,
 /*   410 */    88,   84,   82,   86,   84,   82,   86,   84,   88,   86,
 /*   420 */    74,   75,   88,   74,   75,   88,   74,   75,   82,   88,
 /*   430 */    84,   82,   86,   84,   82,   86,   84,   88,   86,   88,
 /*   440 */    74,   75,   88,   88,   88,   88,   74,   75,   82,   88,
 /*   450 */    84,   88,   86,   88,   82,   88,   84,   88,   86,   88,
 /*   460 */    74,   75,   88,   74,   75,   88,   88,   88,   82,   88,
 /*   470 */    84,   82,   86,   84,   88,   86,   74,   75,   88,   74,
 /*   480 */    75,   88,   74,   75,   82,   88,   84,   82,   86,   84,
 /*   490 */    82,   86,   84,   88,   86,   74,   75,   88,   74,   75,
 /*   500 */    88,   74,   75,   82,   88,   84,   82,   86,   84,   82,
 /*   510 */    86,   84,   88,   86,   88,   74,   75,   88,   88,   88,
 /*   520 */    88,   88,   88,   82,   88,   84,   88,   86,
];
const YY_SHIFT_USE_DFLT: i32 = -17;
const YY_SHIFT_COUNT: i32 = 90;
const YY_SHIFT_MIN: i32 = -16;
const YY_SHIFT_MAX: i32 = 312;
const YY_SHIFT_OFST: [i16; 91] = [
 /*     0 */    40,   -1,   -1,  188,  204,  204,  204,  204,  204,  204,
 /*    10 */   204,  204,  204,  204,  204,  204,  204,  204,  204,  204,
 /*    20 */   204,  204,  204,  204,  204,  204,  204,  204,  204,  294,
 /*    30 */   294,  294,  294,   90,   56,  241,   33,   16,   16,   16,
 /*    40 */   192,  194,  158,  158,  194,  192,  125,  125,  148,  130,
 /*    50 */   113,  163,  163,  163,  163,  163,  252,  252,  312,  312,
 /*    60 */    98,   98,   98,   98,  -15,  -15,   24,   58,   58,   58,
 /*    70 */    13,  112,   57,   84,   61,   53,   47,   46,   76,   25,
 /*    80 */    62,   48,    2,   43,   -9,   12,   44,   37,   27,   -6,
 /*    90 */   -16,
];
const YY_REDUCE_USE_DFLT: i32 = -64;
const YY_REDUCE_COUNT: i32 = 47;
const YY_REDUCE_MIN: i32 = -63;
const YY_REDUCE_MAX: i32 = 441;
const YY_REDUCE_OFST: [i16; 48] = [
 /*     0 */   150,  239,  219,  292,  -17,  279,  118,  266,  -63,  441,
 /*    10 */   427,  424,  421,  408,  405,  402,  389,  386,  372,  366,
 /*    20 */   352,  349,  346,  333,  330,  327,  314,  311,  297,  -57,
 /*    30 */   142,  124,   10,  181,  169,  179,  164,  155,  146,  144,
 /*    40 */   156,  151,  135,  120,  126,  108,  103,   85,
];
const YY_DEFAULT: [YYACTIONTYPE; 140] = [
 /*     0 */   144,  144,  144,  218,  218,  208,  208,  218,  218,  218,
 /*    10 */   218,  218,  218,  218,  218,  218,  218,  218,  218,  218,
 /*    20 */   218,  218,  218,  218,  218,  218,  218,  218,  218,  218,
 /*    30 */   218,  218,  218,  218,  218,  218,  218,  215,  215,  215,
 /*    40 */   218,  168,  161,  161,  168,  218,  218,  218,  209,  218,
 /*    50 */   218,  156,  155,  154,  153,  157,  190,  189,  188,  181,
 /*    60 */   196,  195,  194,  193,  192,  191,  197,  184,  185,  183,
 /*    70 */   197,  218,  218,  169,  218,  218,  218,  218,  218,  218,
 /*    80 */   218,  218,  218,  212,  218,  199,  218,  218,  218,  218,
 /*    90 */   218,  173,  178,  177,  167,  170,  166,  165,  164,  163,
 /*   100 */   162,  160,  159,  158,  211,  207,  210,  198,  213,  187,
 /*   110 */   186,  180,  179,  176,  175,  217,  216,  214,  206,  205,
 /*   120 */   204,  203,  202,  201,  200,  199,  174,  182,  172,  171,
 /*   130 */   152,  151,  150,  149,  148,  147,  146,  145,  143,  142,
];

/* TMPL: fallback tokens */

const YY_FALLBACK: [i32; 0] = [
];

/* TMPL: symbol names */


/* TMPL: rules */


/* TMPL: destructors */


/* TMPL: stack-overflow */


/* TMPL: stack-overflow */

const YY_RULE_INFO: [YYCODETYPE; 78] = [
  58,
  58,
  60,
  60,
  59,
  59,
  63,
  63,
  63,
  63,
  63,
  63,
  73,
  71,
  71,
  72,
  72,
  65,
  65,
  67,
  70,
  77,
  77,
  76,
  76,
  76,
  76,
  68,
  69,
  69,
  69,
  75,
  75,
  75,
  75,
  75,
  75,
  79,
  79,
  75,
  75,
  75,
  75,
  75,
  75,
  75,
  75,
  75,
  75,
  75,
  75,
  75,
  75,
  75,
  75,
  75,
  75,
  75,
  74,
  74,
  74,
  74,
  74,
  74,
  74,
  74,
  74,
  82,
  83,
  83,
  83,
  84,
  85,
  85,
  86,
  87,
  87,
  87,
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
 YYMinorType::YY125(yyres)
}
            ,
            1 /* program ::= stmts */
            => 
{
let yyres :  Ast ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY72(yy0),) => {

	if list::is_empty(&yy0) {
		panic!("null program");
	}
	// ignore yyres, it doesn't really go anywhere for program
	yyres = Ast::Nothing;
	// we're done, so put yy0 in extra
	self.extra = Ok(Ast::ReplRoot(yy0));

},    _ => unreachable!() };
 YYMinorType::YY125(yyres)
}
            ,
            2 /* linebreak ::= NEWLINE */
            => 
{
let yyres :  Ast ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Ast::Nothing;

} };
 YYMinorType::YY125(yyres)
}
            ,
            3 /* linebreak ::= linebreak NEWLINE */
            => 
{
let yyres :  Ast ;
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Ast::Nothing;

} };
 YYMinorType::YY125(yyres)
}
            ,
            4 /* stmts ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

	yyres = sexpr::new(SexprType::BlockExpr, list::empty());

} };
 YYMinorType::YY72(yyres)
}
            ,
            5 /* stmts ::= stmt linebreak stmts */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY72(yy0),YYMinorType::YY72(yy2),) => {

	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            6 /* stmt ::= let_stmt */
          | 7 /* stmt ::= expr_stmt */
          | 8 /* stmt ::= fail_stmt */
          | 10 /* stmt ::= func_stmt */
          | 11 /* stmt ::= macro_stmt */
          | 39 /* expr ::= list */
          | 40 /* expr ::= tuple */
          | 57 /* expr ::= term */
          | 66 /* term ::= strexpr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY72(yy0),) => {
 yyres = yy0; 
},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            9 /* stmt ::= DT */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Void; 
} };
 YYMinorType::YY72(yyres)
}
            ,
            12 /* fail_stmt ::= FAIL HASHTAG term */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY12(yy1),YYMinorType::YY72(yy2),) => {

println!("found fail_stmt {:?}", yy1);
	/*
	yyres = Val::list(
		list::push(yy0,
		list::push(yy1,
		list::push(yy2,
		NULL))));
		*/
	yyres = Val::Failure;

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            13 /* let_stmt ::= Let ID DEFINE expr */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY12(yy1),YYMinorType::YY72(yy3),) => {

	let bind = list::cons(Val::new_str(yy1), list::singleton(yy3));
	yyres = sexpr::new(SexprType::Bind, bind);

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            14 /* let_stmt ::= Fork ID DEFINE expr */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY12(yy1),YYMinorType::YY72(yy3),) => {

	let bind = list::cons(Val::new_str(yy1), list::singleton(yy3));
	yyres = sexpr::new(SexprType::Fork, bind);

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            15 /* expr_stmt ::= expr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY72(yy0),) => {

	yyres = yy0;

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            16 /* expr_stmt ::= DollarGT expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY72(yy1),) => {
 yyres = yy1; 
},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            17 /* arrow_block ::= BLOCKARROW expr */
          | 36 /* expr ::= IF if_expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY72(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            18 /* arrow_block ::= CurlyL linebreak stmts CurlyR */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp2.minor,) {
 (YYMinorType::YY72(yy2),) => {

	yyres = yy2;

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            19 /* func_stmt ::= Func dfunc_1 */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY72(yy1),) => {

	/*
	const Val *plist = Val::list(p);
	const Val *mcase = Val::tuple2(plist, C);
	const list *mblk = list::singleton(mcase);

	Val *func = Val::fexpr(mblk, T);
	yyres = Val::tuple2(yy1, func, LET_ASSIGN);
	*/
	yyres = Val::Sexpr(SexprType::DefFunc, Box::new(yy1));

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            20 /* dfunc_1 ::= ID LPAREN dfunc_args RPAREN opt_typex arrow_block */
            => 
{
let yyres :  Val ;
let yyp5 = self.yystack.pop().unwrap();
let yyp4 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,yyp4.minor,yyp5.minor,) {
 (YYMinorType::YY12(yy0),YYMinorType::YY72(yy2),YYMinorType::YY75(yy4),YYMinorType::YY72(yy5),) => {

	/*
	const Val *plist = Val::list(p);
	const Val *mcase = Val::tuple2(plist, yy5);
	const list *mblk = list::singleton(mcase);

	Val *func = Val::fexpr(mblk, T);
	yyres = Val::tuple2(yy0, func, LET_ASSIGN);
	*/
	let id = Val::id(yy0);
	let typ = Val::Type(yy4);
	yyres = sexpr::single_func_list(id, yy2, typ, yy5)

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            21 /* opt_typex ::= */
            => 
{
let yyres :  Type ;
match () {
 () => {

	yyres = Type::AnonVar;

} };
 YYMinorType::YY75(yyres)
}
            ,
            22 /* opt_typex ::= COLON typex */
            => 
{
let yyres :  Type ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY75(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY75(yyres)
}
            ,
            23 /* typex ::= TYPE_INT */
            => 
{
let yyres :  Type ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Type::Int;

} };
 YYMinorType::YY75(yyres)
}
            ,
            24 /* typex ::= TYPE_STR */
            => 
{
let yyres :  Type ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Type::Str;

} };
 YYMinorType::YY75(yyres)
}
            ,
            25 /* typex ::= TYPE_BOOL */
            => 
{
let yyres :  Type ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Type::Bool;

} };
 YYMinorType::YY75(yyres)
}
            ,
            26 /* typex ::= TYPE_ID */
            => 
{
let yyres :  Type ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY12(yy0),) => {

	yyres = Type::User(yy0);

},    _ => unreachable!() };
 YYMinorType::YY75(yyres)
}
            ,
            27 /* macro_stmt ::= MACRO dfunc_1 */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY72(yy1),) => {

	yyres = Val::Sexpr(SexprType::DefMacro, Box::new(yy1));

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            28 /* dfunc_args ::= */
          | 68 /* list_items ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

	yyres = list::empty();

} };
 YYMinorType::YY72(yyres)
}
            ,
            29 /* dfunc_args ::= ID opt_typex */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY12(yy0),YYMinorType::YY75(yy1),) => {

	yyres = list::singleton(sexpr::id_with_type(yy0, yy1));

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            30 /* dfunc_args ::= ID opt_typex COMMA dfunc_args */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY12(yy0),YYMinorType::YY75(yy1),YYMinorType::YY72(yy3),) => {

	yyres = list::cons(sexpr::id_with_type(yy0, yy1), yy3);

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            31 /* expr ::= ID LPAREN RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY12(yy0),) => {

	println!("zero param function call!");
	yyres = sexpr::call(yy0, Val::Nil);

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            32 /* expr ::= ID LPAREN expr RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY12(yy0),YYMinorType::YY72(yy2),) => {

	println!("1 param function call!");
	let args = list::singleton(yy2);
	yyres = sexpr::call(yy0, args);

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            33 /* expr ::= ID LPAREN tuple_args RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY12(yy0),YYMinorType::YY72(yy2),) => {

	println!("tuple function call");
	yyres = sexpr::call(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            34 /* expr ::= term ID term */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY72(yy0),YYMinorType::YY12(yy1),YYMinorType::YY72(yy2),) => {

	/* yyres = Val::binaryop(yy0, yy1, yy2); */
	yyres = Val::Void;

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            35 /* expr ::= term DOLLAR term */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY72(yy0),YYMinorType::YY72(yy2),) => {

	/* yyres = Val::binaryop(yy0, yy2, D); */
	yyres = Val::Void;

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            37 /* if_expr ::= expr arrow_block ELSE arrow_block */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY72(yy0),YYMinorType::YY72(yy1),YYMinorType::YY72(yy3),) => {

	yyres = sexpr::ifexpr(yy0, yy1, yy3);

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            38 /* if_expr ::= expr arrow_block ELSE IF if_expr */
            => 
{
let yyres :  Val ;
let yyp4 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,yyp4.minor,) {
 (YYMinorType::YY72(yy0),YYMinorType::YY72(yy1),YYMinorType::YY72(yy4),) => {

	yyres = sexpr::ifexpr(yy0, yy1, yy4);

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            41 /* expr ::= NOT expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY72(yy1),) => {

	yyres = sexpr::call("bool_not".to_string(), list::singleton(yy1));

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            42 /* expr ::= expr ConcatNewline */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY72(yy0),) => {

	let newline = Val::Str(Arc::new("\n".to_string()));
	let args = list::cons(yy0, list::singleton(newline));
	yyres = sexpr::new(SexprType::StrExpr, args)

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            43 /* expr ::= MINUS expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY72(yy1),) => {

	println!("found minus {:?}", yy1);
	yyres = sexpr::call("negate".to_string(), list::singleton(yy1));

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            44 /* expr ::= expr PLUS expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY72(yy0),YYMinorType::YY72(yy2),) => {

	yyres = sexpr::binaryop("int_add".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            45 /* expr ::= expr MINUS expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY72(yy0),YYMinorType::YY72(yy2),) => {

	yyres = sexpr::binaryop("int_sub".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            46 /* expr ::= expr TIMES expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY72(yy0),YYMinorType::YY72(yy2),) => {

	yyres = sexpr::binaryop("int_mult".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            47 /* expr ::= expr DIVIDE expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY72(yy0),YYMinorType::YY72(yy2),) => {

	yyres = sexpr::binaryop("int_div".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            48 /* expr ::= expr AND expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY72(yy0),YYMinorType::YY72(yy2),) => {

	yyres = sexpr::binaryop("and".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            49 /* expr ::= expr OR expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY72(yy0),YYMinorType::YY72(yy2),) => {

	yyres = sexpr::binaryop("or".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            50 /* expr ::= expr XOR expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY72(yy0),YYMinorType::YY72(yy2),) => {

	yyres = sexpr::binaryop("xor".to_string(),yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            51 /* expr ::= expr LT expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY72(yy0),YYMinorType::YY72(yy2),) => {

	yyres = sexpr::binaryop("less_than".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            52 /* expr ::= expr LTEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY72(yy0),YYMinorType::YY72(yy2),) => {

	yyres = sexpr::binaryop("less_than_equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            53 /* expr ::= expr GT expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY72(yy0),YYMinorType::YY72(yy2),) => {

	yyres = sexpr::binaryop("greater_than".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            54 /* expr ::= expr GTEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY72(yy0),YYMinorType::YY72(yy2),) => {

	yyres = sexpr::binaryop("greater_than_equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            55 /* expr ::= expr EQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY72(yy0),YYMinorType::YY72(yy2),) => {

	yyres = sexpr::binaryop("equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            56 /* expr ::= expr NEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY72(yy0),YYMinorType::YY72(yy2),) => {

	let eq = sexpr::binaryop("equal".to_string(), yy0, yy2);
	yyres = sexpr::call("bool_not".to_string(), list::singleton(eq));

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            58 /* term ::= LPAREN expr RPAREN */
          | 67 /* list ::= SquareL list_items SquareR */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY72(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            59 /* term ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY12(yy0),) => {
 yyres = Val::id(yy0); 
},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            60 /* term ::= VOID */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {

	println!("found literal void\n");
	yyres = Val::Void;

} };
 YYMinorType::YY72(yyres)
}
            ,
            61 /* term ::= DollarQuestion */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Val::id("$".to_string());

} };
 YYMinorType::YY72(yyres)
}
            ,
            62 /* term ::= INT */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY68(yy0),) => {

	yyres = Val::Int(yy0);

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            63 /* term ::= True */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Bool(true); 
} };
 YYMinorType::YY72(yyres)
}
            ,
            64 /* term ::= False */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Bool(false); 
} };
 YYMinorType::YY72(yyres)
}
            ,
            65 /* term ::= HASHTAG */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY12(yy0),) => {

	yyres = Val::Hashtag(Arc::new(yy0));

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            69 /* list_items ::= expr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY72(yy0),) => {

	yyres = list::singleton(yy0);

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            70 /* list_items ::= expr COMMA list_items */
          | 73 /* tuple_args ::= term COMMA tuple_args */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY72(yy0),YYMinorType::YY72(yy2),) => {

	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            71 /* tuple ::= LPAREN tuple_args RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY72(yy1),) => {

	yyres = Val::tuple_from_list(yy1);

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            72 /* tuple_args ::= term COMMA term */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY72(yy0),YYMinorType::YY72(yy2),) => {

	yyres = list::cons(yy0, list::singleton(yy2));

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            74 /* strexpr ::= StrOpen strlist StrClose */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY72(yy1),) => {

	yyres = sexpr::strexpr(yy1);

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            75 /* strlist ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

	yyres = Val::Nil;

} };
 YYMinorType::YY72(yyres)
}
            ,
            76 /* strlist ::= StrLit strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY12(yy0),YYMinorType::YY72(yy1),) => {

	yyres = list::cons(Val::new_str(yy0), yy1);

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
}
            ,
            77 /* strlist ::= ID strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY12(yy0),YYMinorType::YY72(yy1),) => {

	yyres = list::cons(Val::id(yy0), yy1);

},    _ => unreachable!() };
 YYMinorType::YY72(yyres)
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

	println!("syntax error");
	panic!("Syntax error. wtf? @ <line,column>?");
    }

    fn yy_accept(&mut self) {
        self.yystack.clear();

	println!("parse accepted");
    }
}

