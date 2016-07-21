#![allow(dead_code)]
#![allow(unused_variables)]
/* TMPL: %include */

use leema::ast::{Ast};
use leema::val::{Val, SexprType, Type};
use leema::list;
use leema::log;
use leema::sexpr;
use std::sync::Arc;
use std::io::{stderr, Write};
/* TMPL: makeheader cruft */


/* TMPL: types */

type YYCODETYPE = i8;
const YYNOCODE: i32 = 91;
type YYACTIONTYPE = u8;
const YYWILDCARD: YYCODETYPE = 0;
enum YYMinorType {
    YY0,
    YY4(String),
    YY44(Val),
    YY135(Type),
    YY140(i64),
    YY157(Ast),
}
const YYNSTATE: i32 = 149;
const YYNRULE: i32 = 83;
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
    Fork, //35
    DollarGT, //36
    CurlyL, //37
    CurlyR, //38
    Func, //39
    COLON, //40
    TYPE_INT, //41
    TYPE_STR, //42
    TYPE_BOOL, //43
    MACRO, //44
    DOLLAR, //45
    IF, //46
    ELSE, //47
    VOID, //48
    DollarQuestion, //49
    True, //50
    False, //51
    SquareL, //52
    SquareR, //53
    StrOpen, //54
    StrClose, //55
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
pub const TOKEN_Fork: i32 = 35;
pub const TOKEN_DollarGT: i32 = 36;
pub const TOKEN_CurlyL: i32 = 37;
pub const TOKEN_CurlyR: i32 = 38;
pub const TOKEN_Func: i32 = 39;
pub const TOKEN_COLON: i32 = 40;
pub const TOKEN_TYPE_INT: i32 = 41;
pub const TOKEN_TYPE_STR: i32 = 42;
pub const TOKEN_TYPE_BOOL: i32 = 43;
pub const TOKEN_MACRO: i32 = 44;
pub const TOKEN_DOLLAR: i32 = 45;
pub const TOKEN_IF: i32 = 46;
pub const TOKEN_ELSE: i32 = 47;
pub const TOKEN_VOID: i32 = 48;
pub const TOKEN_DollarQuestion: i32 = 49;
pub const TOKEN_True: i32 = 50;
pub const TOKEN_False: i32 = 51;
pub const TOKEN_SquareL: i32 = 52;
pub const TOKEN_SquareR: i32 = 53;
pub const TOKEN_StrOpen: i32 = 54;
pub const TOKEN_StrClose: i32 = 55;
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
        Token::HASHTAG(x) => YYMinorType::YY4(x),
        Token::ID(x) => YYMinorType::YY4(x),
        Token::INT(x) => YYMinorType::YY140(x),
        Token::StrLit(x) => YYMinorType::YY4(x),
        Token::TYPE_ID(x) => YYMinorType::YY4(x),
        _ => YYMinorType::YY0
  }
}
const YY_ACTTAB_COUNT: i32 = 531;
const YY_ACTION: [YYACTIONTYPE; 531] = [
 /*     0 */   128,   91,  131,   14,   13,   16,   15,   27,   25,   23,
 /*    10 */    22,   72,   35,   38,   24,   39,  122,   33,   95,  121,
 /*    20 */    48,  120,   12,  127,   94,    7,  108,  110,  109,    4,
 /*    30 */   147,  142,   92,   89,   84,   10,   33,  149,   47,   34,
 /*    40 */   128,   91,  131,   78,   29,    8,  150,  133,  132,  130,
 /*    50 */   129,    6,   87,   40,   24,  101,  110,  109,   68,   51,
 /*    60 */    32,    3,   12,  117,  127,    7,  121,   96,  120,   85,
 /*    70 */   127,  142,   92,   89,   84,   10,   23,   22,   47,   32,
 /*    80 */   139,  126,  135,   78,   88,    8,    9,  133,  132,  130,
 /*    90 */   129,    6,  127,   40,  127,   20,   19,   21,  136,   26,
 /*   100 */    18,   17,   14,   13,   16,   15,   27,   25,   23,   22,
 /*   110 */    29,  137,   20,   19,   21,  136,   48,   18,   17,   14,
 /*   120 */    13,   16,   15,   27,   25,   23,   22,  103,  116,    5,
 /*   130 */    20,   19,   21,  136,   83,   18,   17,   14,   13,   16,
 /*   140 */    15,   27,   25,   23,   22,   20,   19,   21,  136,  123,
 /*   150 */    18,   17,   14,   13,   16,   15,   27,   25,   23,   22,
 /*   160 */   114,  127,   11,  106,  105,  104,  113,   20,   19,   21,
 /*   170 */   136,   48,   18,   17,   14,   13,   16,   15,   27,   25,
 /*   180 */    23,   22,  128,   91,  131,  112,  232,  232,  232,  232,
 /*   190 */    27,   25,   23,   22,   45,   46,   24,   43,  128,   91,
 /*   200 */   131,   72,   50,   77,   12,   42,   41,    7,  138,  121,
 /*   210 */    86,  120,   24,  127,    2,   30,   72,   50,   37,   97,
 /*   220 */    12,  148,    1,    7,  121,  115,  120,    8,  127,  133,
 /*   230 */   132,  130,  129,    6,  111,   40,   81,   44,   31,  233,
 /*   240 */    93,   80,   36,    8,   49,  133,  132,  130,  129,    6,
 /*   250 */   141,   40,   79,  140,  102,  145,  144,  143,   72,   55,
 /*   260 */    99,   76,   75,  100,   90,  125,  121,  124,  120,   74,
 /*   270 */   127,   21,  136,  107,   18,   17,   14,   13,   16,   15,
 /*   280 */    27,   25,   23,   22,  234,  234,  146,  234,  234,  234,
 /*   290 */    49,  234,  234,  234,  234,  234,  141,  234,  234,  140,
 /*   300 */   234,  145,  144,  143,   72,   55,  234,   82,  234,  234,
 /*   310 */   234,   49,  121,  234,  120,  234,  127,  141,  234,  234,
 /*   320 */   140,  234,  145,  144,  143,   72,   55,  128,  134,  131,
 /*   330 */   234,  234,  234,  121,  234,  120,  234,  127,  136,  234,
 /*   340 */    18,   17,   14,   13,   16,   15,   27,   25,   23,   22,
 /*   350 */   234,  234,   28,   72,   35,  234,   68,   52,   98,  234,
 /*   360 */   234,  121,  234,  120,  121,  127,  120,   73,  127,  234,
 /*   370 */   234,  234,  234,  234,  133,  132,  130,  129,   72,   51,
 /*   380 */    40,   72,   69,  234,   72,   57,  121,  234,  120,  121,
 /*   390 */   127,  120,  121,  127,  120,  234,  127,   72,   70,  234,
 /*   400 */   234,  234,  234,   72,   61,  121,  234,  120,  234,  127,
 /*   410 */   234,  121,  234,  120,  234,  127,  234,   72,  119,  234,
 /*   420 */    72,  118,  234,   72,   60,  121,  234,  120,  121,  127,
 /*   430 */   120,  121,  127,  120,  234,  127,  234,  234,   72,   59,
 /*   440 */   234,   72,   58,  234,   72,   67,  121,  234,  120,  121,
 /*   450 */   127,  120,  121,  127,  120,  234,  127,   72,   66,  234,
 /*   460 */   234,  234,   72,   65,  234,  121,  234,  120,  234,  127,
 /*   470 */   121,  234,  120,  234,  127,   72,   64,  234,  234,  234,
 /*   480 */    72,   63,  234,  121,  234,  120,  234,  127,  121,  234,
 /*   490 */   120,  234,  127,  234,   72,   62,  234,   72,   71,  234,
 /*   500 */    72,   56,  121,  234,  120,  121,  127,  120,  121,  127,
 /*   510 */   120,  234,  127,  234,  234,   72,   54,  234,   72,   53,
 /*   520 */   234,  234,  234,  121,  234,  120,  121,  127,  120,  234,
 /*   530 */   127,
];
const YY_LOOKAHEAD: [YYCODETYPE; 531] = [
 /*     0 */     1,    2,    3,   18,   19,   20,   21,   22,   23,   24,
 /*    10 */    25,   76,   77,    2,   15,    4,   81,    2,   29,   84,
 /*    20 */    37,   86,   23,   88,   30,   26,   64,   65,   66,   46,
 /*    30 */    31,   32,   33,   34,   35,   36,    2,    0,   39,    1,
 /*    40 */     1,    2,    3,   44,   10,   46,    0,   48,   49,   50,
 /*    50 */    51,   52,   76,   54,   15,   64,   65,   66,   76,   77,
 /*    60 */    45,   26,   23,   87,   88,   26,   84,   28,   86,   87,
 /*    70 */    88,   32,   33,   34,   35,   36,   24,   25,   39,   45,
 /*    80 */    76,   55,   76,   44,    2,   46,    7,   48,   49,   50,
 /*    90 */    51,   52,   88,   54,   88,   11,   12,   13,   14,   18,
 /*   100 */    16,   17,   18,   19,   20,   21,   22,   23,   24,   25,
 /*   110 */    10,   27,   11,   12,   13,   14,   37,   16,   17,   18,
 /*   120 */    19,   20,   21,   22,   23,   24,   25,    5,   27,   10,
 /*   130 */    11,   12,   13,   14,    2,   16,   17,   18,   19,   20,
 /*   140 */    21,   22,   23,   24,   25,   11,   12,   13,   14,   76,
 /*   150 */    16,   17,   18,   19,   20,   21,   22,   23,   24,   25,
 /*   160 */    53,   88,   18,   41,   42,   43,   27,   11,   12,   13,
 /*   170 */    14,   37,   16,   17,   18,   19,   20,   21,   22,   23,
 /*   180 */    24,   25,    1,    2,    3,   38,   18,   19,   20,   21,
 /*   190 */    22,   23,   24,   25,   27,   26,   15,   10,    1,    2,
 /*   200 */     3,   76,   77,    2,   23,   26,   10,   26,   27,   84,
 /*   210 */    85,   86,   15,   88,   59,   27,   76,   77,   47,   27,
 /*   220 */    23,   31,   59,   26,   84,   85,   86,   46,   88,   48,
 /*   230 */    49,   50,   51,   52,   70,   54,    2,    2,   79,   57,
 /*   240 */    58,   69,   40,   46,   62,   48,   49,   50,   51,   52,
 /*   250 */    68,   54,   79,   71,   69,   73,   74,   75,   76,   77,
 /*   260 */    66,   72,    2,   72,   89,   89,   84,   89,   86,   66,
 /*   270 */    88,   13,   14,   78,   16,   17,   18,   19,   20,   21,
 /*   280 */    22,   23,   24,   25,   90,   90,   58,   90,   90,   90,
 /*   290 */    62,   90,   90,   90,   90,   90,   68,   90,   90,   71,
 /*   300 */    90,   73,   74,   75,   76,   77,   90,   58,   90,   90,
 /*   310 */    90,   62,   84,   90,   86,   90,   88,   68,   90,   90,
 /*   320 */    71,   90,   73,   74,   75,   76,   77,    1,    2,    3,
 /*   330 */    90,   90,   90,   84,   90,   86,   90,   88,   14,   90,
 /*   340 */    16,   17,   18,   19,   20,   21,   22,   23,   24,   25,
 /*   350 */    90,   90,   26,   76,   77,   90,   76,   77,   81,   90,
 /*   360 */    90,   84,   90,   86,   84,   88,   86,   87,   88,   90,
 /*   370 */    90,   90,   90,   90,   48,   49,   50,   51,   76,   77,
 /*   380 */    54,   76,   77,   90,   76,   77,   84,   90,   86,   84,
 /*   390 */    88,   86,   84,   88,   86,   90,   88,   76,   77,   90,
 /*   400 */    90,   90,   90,   76,   77,   84,   90,   86,   90,   88,
 /*   410 */    90,   84,   90,   86,   90,   88,   90,   76,   77,   90,
 /*   420 */    76,   77,   90,   76,   77,   84,   90,   86,   84,   88,
 /*   430 */    86,   84,   88,   86,   90,   88,   90,   90,   76,   77,
 /*   440 */    90,   76,   77,   90,   76,   77,   84,   90,   86,   84,
 /*   450 */    88,   86,   84,   88,   86,   90,   88,   76,   77,   90,
 /*   460 */    90,   90,   76,   77,   90,   84,   90,   86,   90,   88,
 /*   470 */    84,   90,   86,   90,   88,   76,   77,   90,   90,   90,
 /*   480 */    76,   77,   90,   84,   90,   86,   90,   88,   84,   90,
 /*   490 */    86,   90,   88,   90,   76,   77,   90,   76,   77,   90,
 /*   500 */    76,   77,   84,   90,   86,   84,   88,   86,   84,   88,
 /*   510 */    86,   90,   88,   90,   90,   76,   77,   90,   76,   77,
 /*   520 */    90,   90,   90,   84,   90,   86,   84,   88,   86,   90,
 /*   530 */    88,
];
const YY_SHIFT_USE_DFLT: i32 = -18;
const YY_SHIFT_COUNT: i32 = 96;
const YY_SHIFT_MIN: i32 = -17;
const YY_SHIFT_MAX: i32 = 326;
const YY_SHIFT_OFST: [i16; 97] = [
 /*     0 */    39,   -1,   -1,  181,  197,  197,  197,  197,  197,  197,
 /*    10 */   197,  197,  197,  197,  197,  197,  197,  197,  197,  197,
 /*    20 */   197,  197,  197,  197,  197,  197,  197,  197,  197,  326,
 /*    30 */    79,   79,  326,  326,  326,  134,  122,  -17,   11,   11,
 /*    40 */    11,  260,  260,  235,  202,  202,  235,  234,  190,  190,
 /*    50 */   119,  101,   84,  156,  156,  156,  156,  156,  258,  258,
 /*    60 */   324,  324,  168,  168,  168,  168,  -15,  -15,   34,   52,
 /*    70 */    52,   52,   15,  192,  171,  196,  188,  179,  201,  187,
 /*    80 */   167,  169,  147,  144,  132,  139,  107,  100,   81,   82,
 /*    90 */    26,   35,   38,   46,   37,   -6,  -11,
];
const YY_REDUCE_USE_DFLT: i32 = -66;
const YY_REDUCE_COUNT: i32 = 49;
const YY_REDUCE_MIN: i32 = -65;
const YY_REDUCE_MAX: i32 = 442;
const YY_REDUCE_OFST: [i16; 50] = [
 /*     0 */   182,  249,  228,  280,  277,  140,  125,  -18,  -65,  442,
 /*    10 */   439,  424,  421,  418,  404,  399,  386,  381,  368,  365,
 /*    20 */   362,  347,  344,  341,  327,  321,  308,  305,  302,  -24,
 /*    30 */    -9,  -38,   73,    6,    4,  203,  195,  194,  178,  176,
 /*    40 */   175,  191,  189,  185,  173,  159,  172,  164,  163,  155,
];
const YY_DEFAULT: [YYACTIONTYPE; 149] = [
 /*     0 */   153,  153,  153,  232,  232,  222,  222,  232,  232,  232,
 /*    10 */   232,  232,  232,  232,  232,  232,  232,  232,  232,  232,
 /*    20 */   232,  232,  232,  232,  232,  232,  232,  232,  232,  232,
 /*    30 */   232,  232,  232,  232,  232,  232,  232,  232,  229,  229,
 /*    40 */   229,  182,  182,  172,  175,  175,  172,  232,  232,  232,
 /*    50 */   223,  232,  232,  168,  165,  164,  163,  162,  204,  203,
 /*    60 */   202,  195,  210,  209,  208,  207,  206,  205,  211,  198,
 /*    70 */   199,  197,  211,  232,  232,  183,  232,  232,  232,  173,
 /*    80 */   232,  232,  232,  232,  232,  232,  232,  226,  232,  232,
 /*    90 */   232,  213,  232,  232,  232,  232,  232,  187,  192,  191,
 /*   100 */   184,  181,  174,  180,  179,  178,  177,  176,  171,  167,
 /*   110 */   166,  170,  169,  225,  221,  224,  212,  227,  201,  200,
 /*   120 */   194,  193,  190,  189,  231,  230,  228,  220,  219,  218,
 /*   130 */   217,  216,  215,  214,  213,  188,  196,  186,  185,  161,
 /*   140 */   160,  159,  158,  157,  156,  155,  154,  152,  151,
];

/* TMPL: fallback tokens */

const YY_FALLBACK: [i32; 0] = [
];

/* TMPL: symbol names */


/* TMPL: rules */


/* TMPL: destructors */


/* TMPL: stack-overflow */


/* TMPL: stack-overflow */

const YY_RULE_INFO: [YYCODETYPE; 83] = [
  57,
  57,
  59,
  59,
  58,
  58,
  62,
  62,
  62,
  62,
  62,
  62,
  75,
  73,
  73,
  74,
  74,
  64,
  64,
  65,
  66,
  68,
  70,
  69,
  69,
  69,
  79,
  79,
  78,
  78,
  78,
  78,
  71,
  72,
  72,
  72,
  77,
  77,
  77,
  77,
  77,
  77,
  81,
  81,
  77,
  77,
  77,
  77,
  77,
  77,
  77,
  77,
  77,
  77,
  77,
  77,
  77,
  77,
  77,
  77,
  77,
  77,
  77,
  76,
  76,
  76,
  76,
  76,
  76,
  76,
  76,
  76,
  84,
  85,
  85,
  85,
  86,
  87,
  87,
  88,
  89,
  89,
  89,
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
 YYMinorType::YY157(yyres)
}
            ,
            1 /* program ::= stmts */
            => 
{
let yyres :  Ast ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY44(yy0),) => {

	if list::is_empty(&yy0) {
		panic!("null program");
	}
	// ignore yyres, it doesn't really go anywhere for program
	yyres = Ast::Nothing;
	// we're done, so put yy0 in extra
	self.extra = Ok(Ast::ReplRoot(yy0));

},    _ => unreachable!() };
 YYMinorType::YY157(yyres)
}
            ,
            2 /* linebreak ::= NEWLINE */
            => 
{
let yyres :  Ast ;
self.yystack.pop().unwrap();
match () {
 () => {

    // set to avoid uninitialized variable
    yyres = Ast::Nothing;

} };
 YYMinorType::YY157(yyres)
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

    // set to avoid uninitialized variable
    yyres = Ast::Nothing;

} };
 YYMinorType::YY157(yyres)
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
 YYMinorType::YY44(yyres)
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
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy2),) => {

	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            6 /* stmt ::= let_stmt */
          | 8 /* stmt ::= fail_stmt */
          | 10 /* stmt ::= func_stmt */
          | 11 /* stmt ::= macro_stmt */
          | 44 /* expr ::= list */
          | 45 /* expr ::= tuple */
          | 62 /* expr ::= term */
          | 71 /* term ::= strexpr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY44(yy0),) => {
 yyres = yy0; 
},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            7 /* stmt ::= expr_stmt */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY44(yy0),) => {

    yyres = yy0;

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
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
 YYMinorType::YY44(yyres)
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
 (YYMinorType::YY4(yy1),YYMinorType::YY44(yy2),) => {

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
 YYMinorType::YY44(yyres)
}
            ,
            13 /* let_stmt ::= Let ID EQ expr */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY4(yy1),YYMinorType::YY44(yy3),) => {

	let bind = list::cons(Val::new_str(yy1), list::singleton(yy3));
	yyres = sexpr::new(SexprType::Bind, bind);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            14 /* let_stmt ::= Fork ID EQ expr */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY4(yy1),YYMinorType::YY44(yy3),) => {

	let bind = list::cons(Val::new_str(yy1), list::singleton(yy3));
	yyres = sexpr::new(SexprType::Fork, bind);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            15 /* expr_stmt ::= expr */
          | 17 /* block ::= arrow_block */
          | 18 /* block ::= curly_block */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY44(yy0),) => {

	yyres = yy0;

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            16 /* expr_stmt ::= DollarGT expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY44(yy1),) => {
 yyres = yy1; 
},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            19 /* arrow_block ::= BLOCKARROW expr */
          | 41 /* expr ::= IF if_expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY44(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            20 /* curly_block ::= CurlyL linebreak stmts CurlyR */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp2.minor,) {
 (YYMinorType::YY44(yy2),) => {

	yyres = yy2;

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            21 /* func_stmt ::= Func dfunc_1 */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY44(yy1),) => {

	/*
	const Val *plist = Val::list(p);
	const Val *mcase = Val::tuple2(plist, C);
	const list *mblk = list::singleton(mcase);

	Val *func = Val::fexpr(mblk, T);
	yyres = Val::tuple2(yy1, func, LET_ASSIGN);
	*/
	yyres = Val::Sexpr(SexprType::DefFunc, Box::new(yy1));

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            22 /* dfunc_1 ::= ID LPAREN dfunc_args RPAREN opt_typex block */
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
 (YYMinorType::YY4(yy0),YYMinorType::YY44(yy2),YYMinorType::YY135(yy4),YYMinorType::YY44(yy5),) => {

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
 YYMinorType::YY44(yyres)
}
            ,
            23 /* dfunc_args ::= */
          | 73 /* list_items ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

	yyres = list::empty();

} };
 YYMinorType::YY44(yyres)
}
            ,
            24 /* dfunc_args ::= ID opt_typex */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY4(yy0),YYMinorType::YY135(yy1),) => {

	yyres = list::singleton(sexpr::id_with_type(yy0, yy1));

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            25 /* dfunc_args ::= ID opt_typex COMMA dfunc_args */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY4(yy0),YYMinorType::YY135(yy1),YYMinorType::YY44(yy3),) => {

	yyres = list::cons(sexpr::id_with_type(yy0, yy1), yy3);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
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
 YYMinorType::YY135(yyres)
}
            ,
            27 /* opt_typex ::= COLON typex */
            => 
{
let yyres :  Type ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY135(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY135(yyres)
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
 YYMinorType::YY135(yyres)
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
 YYMinorType::YY135(yyres)
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
 YYMinorType::YY135(yyres)
}
            ,
            31 /* typex ::= TYPE_ID */
            => 
{
let yyres :  Type ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY4(yy0),) => {

	yyres = Type::User(yy0);

},    _ => unreachable!() };
 YYMinorType::YY135(yyres)
}
            ,
            32 /* macro_stmt ::= MACRO ID LPAREN macro_args RPAREN block */
            => 
{
let yyres :  Val ;
let yyp5 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,yyp5.minor,) {
 (YYMinorType::YY4(yy1),YYMinorType::YY44(yy3),YYMinorType::YY44(yy5),) => {

    yyres = sexpr::new(SexprType::DefMacro,
        list::cons(Val::id(yy1),
        list::cons(yy3,
        list::cons(yy5,
        Val::Nil
    ))));

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
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
 YYMinorType::YY44(yyres)
}
            ,
            34 /* macro_args ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY4(yy0),) => {

    yyres = list::singleton(Val::id(yy0));

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            35 /* macro_args ::= ID COMMA macro_args */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY4(yy0),YYMinorType::YY44(yy2),) => {

    yyres = list::cons(Val::id(yy0), yy2);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            36 /* expr ::= ID LPAREN RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY4(yy0),) => {

	println!("zero param function call!");
	yyres = sexpr::call(yy0, Val::Nil);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            37 /* expr ::= ID LPAREN expr RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY4(yy0),YYMinorType::YY44(yy2),) => {

	let args = list::singleton(yy2);
	yyres = sexpr::call(yy0, args);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            38 /* expr ::= ID LPAREN tuple_args RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY4(yy0),YYMinorType::YY44(yy2),) => {

	yyres = sexpr::call(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            39 /* expr ::= term ID term */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY44(yy0),YYMinorType::YY4(yy1),YYMinorType::YY44(yy2),) => {

	/* yyres = Val::binaryop(yy0, yy1, yy2); */
	yyres = Val::Void;

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            40 /* expr ::= term DOLLAR term */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy2),) => {

	/* yyres = Val::binaryop(yy0, yy2, D); */
	yyres = Val::Void;

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            42 /* if_expr ::= expr curly_block ELSE curly_block */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy1),YYMinorType::YY44(yy3),) => {

	yyres = sexpr::ifexpr(yy0, yy1, yy3);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            43 /* if_expr ::= expr curly_block ELSE IF if_expr */
            => 
{
let yyres :  Val ;
let yyp4 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,yyp4.minor,) {
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy1),YYMinorType::YY44(yy4),) => {

	yyres = sexpr::ifexpr(yy0, yy1, yy4);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            46 /* expr ::= NOT expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY44(yy1),) => {

	yyres = sexpr::call("bool_not".to_string(), list::singleton(yy1));

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            47 /* expr ::= expr ConcatNewline */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY44(yy0),) => {

	let newline = Val::Str(Arc::new("\n".to_string()));
	let args = list::cons(yy0, list::singleton(newline));
	yyres = sexpr::new(SexprType::StrExpr, args)

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            48 /* expr ::= MINUS expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY44(yy1),) => {

	yyres = sexpr::call("negate".to_string(), list::singleton(yy1));

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            49 /* expr ::= expr PLUS expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy2),) => {

	yyres = sexpr::binaryop("int_add".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            50 /* expr ::= expr MINUS expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy2),) => {

	yyres = sexpr::binaryop("int_sub".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            51 /* expr ::= expr TIMES expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy2),) => {

	yyres = sexpr::binaryop("int_mult".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            52 /* expr ::= expr DIVIDE expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy2),) => {

	yyres = sexpr::binaryop("int_div".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            53 /* expr ::= expr AND expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy2),) => {

	yyres = sexpr::binaryop("and".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            54 /* expr ::= expr OR expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy2),) => {

	yyres = sexpr::binaryop("or".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            55 /* expr ::= expr XOR expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy2),) => {

	yyres = sexpr::binaryop("xor".to_string(),yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            56 /* expr ::= expr LT expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy2),) => {

	yyres = sexpr::binaryop("less_than".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            57 /* expr ::= expr LTEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy2),) => {

	yyres = sexpr::binaryop("less_than_equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            58 /* expr ::= expr GT expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy2),) => {

	yyres = sexpr::binaryop("greater_than".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            59 /* expr ::= expr GTEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy2),) => {

	yyres = sexpr::binaryop("greater_than_equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            60 /* expr ::= expr EQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy2),) => {

	yyres = sexpr::binaryop("equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            61 /* expr ::= expr NEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy2),) => {

	let eq = sexpr::binaryop("equal".to_string(), yy0, yy2);
	yyres = sexpr::call("bool_not".to_string(), list::singleton(eq));

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            63 /* term ::= LPAREN expr RPAREN */
          | 72 /* list ::= SquareL list_items SquareR */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY44(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            64 /* term ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY4(yy0),) => {
 yyres = Val::id(yy0); 
},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            65 /* term ::= VOID */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Val::Void;

} };
 YYMinorType::YY44(yyres)
}
            ,
            66 /* term ::= DollarQuestion */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Val::id("$".to_string());

} };
 YYMinorType::YY44(yyres)
}
            ,
            67 /* term ::= INT */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY140(yy0),) => {

	yyres = Val::Int(yy0);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            68 /* term ::= True */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Bool(true); 
} };
 YYMinorType::YY44(yyres)
}
            ,
            69 /* term ::= False */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Bool(false); 
} };
 YYMinorType::YY44(yyres)
}
            ,
            70 /* term ::= HASHTAG */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY4(yy0),) => {

	yyres = Val::Hashtag(Arc::new(yy0));

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            74 /* list_items ::= expr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY44(yy0),) => {

	yyres = list::singleton(yy0);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            75 /* list_items ::= expr COMMA list_items */
          | 78 /* tuple_args ::= term COMMA tuple_args */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy2),) => {

	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            76 /* tuple ::= LPAREN tuple_args RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY44(yy1),) => {

	yyres = Val::tuple_from_list(yy1);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            77 /* tuple_args ::= term COMMA term */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY44(yy0),YYMinorType::YY44(yy2),) => {

	yyres = list::cons(yy0, list::singleton(yy2));

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            79 /* strexpr ::= StrOpen strlist StrClose */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY44(yy1),) => {

	yyres = sexpr::strexpr(yy1);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            80 /* strlist ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

	yyres = Val::Nil;

} };
 YYMinorType::YY44(yyres)
}
            ,
            81 /* strlist ::= StrLit strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY4(yy0),YYMinorType::YY44(yy1),) => {

	yyres = list::cons(Val::new_str(yy0), yy1);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            82 /* strlist ::= ID strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY4(yy0),YYMinorType::YY44(yy1),) => {

	yyres = list::cons(Val::id(yy0), yy1);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
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

