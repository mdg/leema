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
const YYNOCODE: i32 = 90;
type YYACTIONTYPE = u8;
const YYWILDCARD: YYCODETYPE = 0;
enum YYMinorType {
    YY0,
    YY66(i64),
    YY116(Val),
    YY117(Ast),
    YY164(String),
    YY173(Type),
}
const YYNSTATE: i32 = 147;
const YYNRULE: i32 = 81;
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
        Token::HASHTAG(x) => YYMinorType::YY164(x),
        Token::ID(x) => YYMinorType::YY164(x),
        Token::INT(x) => YYMinorType::YY66(x),
        Token::StrLit(x) => YYMinorType::YY164(x),
        Token::TYPE_ID(x) => YYMinorType::YY164(x),
        _ => YYMinorType::YY0
  }
}
const YY_ACTTAB_COUNT: i32 = 508;
const YY_ACTION: [YYACTIONTYPE; 508] = [
 /*     0 */   128,   90,  131,   14,   13,   16,   15,   27,   25,   23,
 /*    10 */    22,   70,   35,   38,   24,   39,  122,   23,   22,  121,
 /*    20 */   103,  120,   12,  127,  139,    7,   95,   96,  108,  110,
 /*    30 */   109,  142,   91,   87,   82,   10,  127,   33,   47,   94,
 /*    40 */   128,   90,  131,   76,  147,    8,  148,  133,  132,  130,
 /*    50 */   129,    6,  146,   40,   24,   92,  106,  105,  104,    2,
 /*    60 */    34,  141,   12,    3,  140,    7,  145,  144,  143,   70,
 /*    70 */    53,  142,   91,   87,   82,   10,  126,  121,   47,  120,
 /*    80 */    32,  127,  135,   76,   33,    8,    9,  133,  132,  130,
 /*    90 */   129,    6,   29,   40,  127,   20,   19,   21,  136,    1,
 /*   100 */    18,   17,   14,   13,   16,   15,   27,   25,   23,   22,
 /*   110 */    86,  137,   20,   19,   21,  136,   88,   18,   17,   14,
 /*   120 */    13,   16,   15,   27,   25,   23,   22,   32,  116,    5,
 /*   130 */    20,   19,   21,  136,   26,   18,   17,   14,   13,   16,
 /*   140 */    15,   27,   25,   23,   22,   20,   19,   21,  136,   29,
 /*   150 */    18,   17,   14,   13,   16,   15,   27,   25,   23,   22,
 /*   160 */   101,  110,  109,  114,  113,   81,  112,   20,   19,   21,
 /*   170 */   136,   88,   18,   17,   14,   13,   16,   15,   27,   25,
 /*   180 */    23,   22,  128,   90,  131,   88,  228,  228,  228,  228,
 /*   190 */    27,   25,   23,   22,    4,   11,   24,   46,  128,   90,
 /*   200 */   131,   70,   48,   45,   12,   43,   85,    7,  138,  121,
 /*   210 */    84,  120,   24,  127,  128,  134,  131,  117,  127,   75,
 /*   220 */    12,   30,  123,    7,   42,   41,   37,    8,   97,  133,
 /*   230 */   132,  130,  129,    6,  127,   40,  111,   79,   78,   28,
 /*   240 */   229,   93,   44,    8,   92,  133,  132,  130,  129,    6,
 /*   250 */   141,   40,   31,  140,   36,  145,  144,  143,   70,   53,
 /*   260 */    77,  133,  132,  130,  129,   73,  121,   40,  120,   99,
 /*   270 */   127,   21,  136,  102,   18,   17,   14,   13,   16,   15,
 /*   280 */    27,   25,   23,   22,   70,   48,   80,   74,   89,   92,
 /*   290 */   100,  125,  121,  115,  120,  141,  127,  107,  140,  124,
 /*   300 */   145,  144,  143,   70,   53,  230,   72,  230,  230,  230,
 /*   310 */   230,  121,  230,  120,  230,  127,  136,  230,   18,   17,
 /*   320 */    14,   13,   16,   15,   27,   25,   23,   22,  230,   66,
 /*   330 */    49,  230,  230,  230,  230,  230,  230,  121,  230,  120,
 /*   340 */    83,  127,   70,   35,  230,  230,  230,   98,   66,   50,
 /*   350 */   121,  230,  120,  230,  127,  230,  121,  230,  120,   71,
 /*   360 */   127,   70,   49,  230,  230,  230,  230,  230,  230,  121,
 /*   370 */   230,  120,  230,  127,   70,   67,  230,  230,  230,   70,
 /*   380 */    55,  230,  121,  230,  120,  230,  127,  121,  230,  120,
 /*   390 */   230,  127,   70,   68,  230,   70,   59,  230,   70,  119,
 /*   400 */   121,  230,  120,  121,  127,  120,  121,  127,  120,  230,
 /*   410 */   127,  230,  230,   70,  118,  230,   70,   58,  230,   70,
 /*   420 */    57,  121,  230,  120,  121,  127,  120,  121,  127,  120,
 /*   430 */   230,  127,   70,   56,  230,   70,   65,  230,   70,   64,
 /*   440 */   121,  230,  120,  121,  127,  120,  121,  127,  120,  230,
 /*   450 */   127,   70,   63,  230,  230,  230,   70,   62,  230,  121,
 /*   460 */   230,  120,  230,  127,  121,  230,  120,  230,  127,   70,
 /*   470 */    61,  230,   70,   60,  230,   70,   69,  121,  230,  120,
 /*   480 */   121,  127,  120,  121,  127,  120,  230,  127,  230,   70,
 /*   490 */    54,  230,   70,   52,  230,   70,   51,  121,  230,  120,
 /*   500 */   121,  127,  120,  121,  127,  120,  230,  127,
];
const YY_LOOKAHEAD: [YYCODETYPE; 508] = [
 /*     0 */     1,    2,    3,   18,   19,   20,   21,   22,   23,   24,
 /*    10 */    25,   75,   76,    2,   15,    4,   80,   24,   25,   83,
 /*    20 */     5,   85,   23,   87,   75,   26,   29,   28,   63,   64,
 /*    30 */    65,   32,   33,   34,   35,   36,   87,    2,   39,   30,
 /*    40 */     1,    2,    3,   44,    0,   46,    0,   48,   49,   50,
 /*    50 */    51,   52,   58,   54,   15,   61,   41,   42,   43,   31,
 /*    60 */     1,   67,   23,   26,   70,   26,   72,   73,   74,   75,
 /*    70 */    76,   32,   33,   34,   35,   36,   55,   83,   39,   85,
 /*    80 */    45,   87,   75,   44,    2,   46,    7,   48,   49,   50,
 /*    90 */    51,   52,   10,   54,   87,   11,   12,   13,   14,   31,
 /*   100 */    16,   17,   18,   19,   20,   21,   22,   23,   24,   25,
 /*   110 */     2,   27,   11,   12,   13,   14,   37,   16,   17,   18,
 /*   120 */    19,   20,   21,   22,   23,   24,   25,   45,   27,   10,
 /*   130 */    11,   12,   13,   14,   18,   16,   17,   18,   19,   20,
 /*   140 */    21,   22,   23,   24,   25,   11,   12,   13,   14,   10,
 /*   150 */    16,   17,   18,   19,   20,   21,   22,   23,   24,   25,
 /*   160 */    63,   64,   65,   53,   27,    2,   38,   11,   12,   13,
 /*   170 */    14,   37,   16,   17,   18,   19,   20,   21,   22,   23,
 /*   180 */    24,   25,    1,    2,    3,   37,   18,   19,   20,   21,
 /*   190 */    22,   23,   24,   25,   46,   18,   15,   26,    1,    2,
 /*   200 */     3,   75,   76,   27,   23,   10,   75,   26,   27,   83,
 /*   210 */    84,   85,   15,   87,    1,    2,    3,   86,   87,    2,
 /*   220 */    23,   27,   75,   26,   26,   10,   47,   46,   27,   48,
 /*   230 */    49,   50,   51,   52,   87,   54,   69,    2,   68,   26,
 /*   240 */    57,   58,    2,   46,   61,   48,   49,   50,   51,   52,
 /*   250 */    67,   54,   78,   70,   40,   72,   73,   74,   75,   76,
 /*   260 */    78,   48,   49,   50,   51,    2,   83,   54,   85,   65,
 /*   270 */    87,   13,   14,   68,   16,   17,   18,   19,   20,   21,
 /*   280 */    22,   23,   24,   25,   75,   76,   58,   71,   88,   61,
 /*   290 */    71,   88,   83,   84,   85,   67,   87,   77,   70,   88,
 /*   300 */    72,   73,   74,   75,   76,   89,   65,   89,   89,   89,
 /*   310 */    89,   83,   89,   85,   89,   87,   14,   89,   16,   17,
 /*   320 */    18,   19,   20,   21,   22,   23,   24,   25,   89,   75,
 /*   330 */    76,   89,   89,   89,   89,   89,   89,   83,   89,   85,
 /*   340 */    86,   87,   75,   76,   89,   89,   89,   80,   75,   76,
 /*   350 */    83,   89,   85,   89,   87,   89,   83,   89,   85,   86,
 /*   360 */    87,   75,   76,   89,   89,   89,   89,   89,   89,   83,
 /*   370 */    89,   85,   89,   87,   75,   76,   89,   89,   89,   75,
 /*   380 */    76,   89,   83,   89,   85,   89,   87,   83,   89,   85,
 /*   390 */    89,   87,   75,   76,   89,   75,   76,   89,   75,   76,
 /*   400 */    83,   89,   85,   83,   87,   85,   83,   87,   85,   89,
 /*   410 */    87,   89,   89,   75,   76,   89,   75,   76,   89,   75,
 /*   420 */    76,   83,   89,   85,   83,   87,   85,   83,   87,   85,
 /*   430 */    89,   87,   75,   76,   89,   75,   76,   89,   75,   76,
 /*   440 */    83,   89,   85,   83,   87,   85,   83,   87,   85,   89,
 /*   450 */    87,   75,   76,   89,   89,   89,   75,   76,   89,   83,
 /*   460 */    89,   85,   89,   87,   83,   89,   85,   89,   87,   75,
 /*   470 */    76,   89,   75,   76,   89,   75,   76,   83,   89,   85,
 /*   480 */    83,   87,   85,   83,   87,   85,   89,   87,   89,   75,
 /*   490 */    76,   89,   75,   76,   89,   75,   76,   83,   89,   85,
 /*   500 */    83,   87,   85,   83,   87,   85,   89,   87,
];
const YY_SHIFT_USE_DFLT: i32 = -16;
const YY_SHIFT_COUNT: i32 = 96;
const YY_SHIFT_MIN: i32 = -15;
const YY_SHIFT_MAX: i32 = 302;
const YY_SHIFT_OFST: [i16; 97] = [
 /*     0 */    -1,   39,   39,  181,  197,  197,  197,  197,  197,  197,
 /*    10 */   197,  197,  197,  197,  197,  197,  197,  197,  197,  197,
 /*    20 */   197,  197,  197,  197,  197,  197,  197,  197,  197,  213,
 /*    30 */    79,   79,  213,  213,  213,  134,   15,  148,   11,   11,
 /*    40 */    11,  263,  263,  240,  214,  214,  240,  235,  119,  101,
 /*    50 */    84,  156,  156,  156,  156,  156,  258,  258,  302,  302,
 /*    60 */   168,  168,  168,  168,  -15,  -15,   82,   -7,   -7,   -7,
 /*    70 */    35,  201,  179,  215,  194,  198,  217,  195,  176,  171,
 /*    80 */   128,  177,  163,  137,  110,  139,  116,  108,   68,   21,
 /*    90 */    37,   59,   28,   46,   44,    9,   -3,
];
const YY_REDUCE_USE_DFLT: i32 = -65;
const YY_REDUCE_COUNT: i32 = 47;
const YY_REDUCE_MIN: i32 = -64;
const YY_REDUCE_MAX: i32 = 420;
const YY_REDUCE_OFST: [i16; 48] = [
 /*     0 */   183,  228,   -6,  273,  267,  209,  126,  254,  -64,  420,
 /*    10 */   417,  414,  400,  397,  394,  381,  376,  363,  360,  357,
 /*    20 */   344,  341,  338,  323,  320,  317,  304,  299,  286,  131,
 /*    30 */    97,  -35,  147,    7,  -51,  241,  220,  204,  211,  203,
 /*    40 */   200,  219,  216,  205,  182,  174,  170,  167,
];
const YY_DEFAULT: [YYACTIONTYPE; 147] = [
 /*     0 */   149,  149,  149,  228,  228,  218,  218,  228,  228,  228,
 /*    10 */   228,  228,  228,  228,  228,  228,  228,  228,  228,  228,
 /*    20 */   228,  228,  228,  228,  228,  228,  228,  228,  228,  228,
 /*    30 */   228,  228,  228,  228,  228,  228,  228,  228,  225,  225,
 /*    40 */   225,  178,  178,  168,  171,  171,  168,  228,  219,  228,
 /*    50 */   228,  164,  161,  160,  159,  158,  200,  199,  198,  191,
 /*    60 */   206,  205,  204,  203,  202,  201,  207,  194,  195,  193,
 /*    70 */   207,  228,  228,  179,  228,  228,  228,  169,  228,  228,
 /*    80 */   228,  228,  228,  228,  228,  222,  228,  228,  228,  228,
 /*    90 */   209,  228,  228,  228,  228,  228,  228,  183,  188,  187,
 /*   100 */   180,  177,  170,  176,  175,  174,  173,  172,  167,  163,
 /*   110 */   162,  166,  165,  221,  217,  220,  208,  223,  197,  196,
 /*   120 */   190,  189,  186,  185,  227,  226,  224,  216,  215,  214,
 /*   130 */   213,  212,  211,  210,  209,  184,  192,  182,  181,  157,
 /*   140 */   156,  155,  154,  153,  152,  151,  150,
];

/* TMPL: fallback tokens */

const YY_FALLBACK: [i32; 0] = [
];

/* TMPL: symbol names */


/* TMPL: rules */


/* TMPL: destructors */


/* TMPL: stack-overflow */


/* TMPL: stack-overflow */

const YY_RULE_INFO: [YYCODETYPE; 81] = [
  57,
  57,
  58,
  58,
  61,
  61,
  61,
  61,
  61,
  61,
  74,
  72,
  72,
  73,
  73,
  63,
  63,
  64,
  65,
  67,
  69,
  68,
  68,
  68,
  78,
  78,
  77,
  77,
  77,
  77,
  70,
  71,
  71,
  71,
  76,
  76,
  76,
  76,
  76,
  76,
  80,
  80,
  76,
  76,
  76,
  76,
  76,
  76,
  76,
  76,
  76,
  76,
  76,
  76,
  76,
  76,
  76,
  76,
  76,
  76,
  76,
  75,
  75,
  75,
  75,
  75,
  75,
  75,
  75,
  75,
  83,
  84,
  84,
  84,
  85,
  86,
  86,
  87,
  88,
  88,
  88,
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
 YYMinorType::YY117(yyres)
}
            ,
            1 /* program ::= stmts */
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
 YYMinorType::YY117(yyres)
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
 YYMinorType::YY116(yyres)
}
            ,
            3 /* stmts ::= stmt NEWLINE stmts */
          | 73 /* list_items ::= expr COMMA list_items */
          | 76 /* tuple_args ::= term COMMA tuple_args */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY116(yy0),YYMinorType::YY116(yy2),) => {

	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            4 /* stmt ::= let_stmt */
          | 6 /* stmt ::= fail_stmt */
          | 8 /* stmt ::= func_stmt */
          | 9 /* stmt ::= macro_stmt */
          | 42 /* expr ::= list */
          | 43 /* expr ::= tuple */
          | 60 /* expr ::= term */
          | 69 /* term ::= strexpr */
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
            5 /* stmt ::= expr_stmt */
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
            7 /* stmt ::= DT */
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
            10 /* fail_stmt ::= FAIL HASHTAG term */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY164(yy1),YYMinorType::YY116(yy2),) => {

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
 YYMinorType::YY116(yyres)
}
            ,
            11 /* let_stmt ::= Let ID EQ expr */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY164(yy1),YYMinorType::YY116(yy3),) => {

	let bind = list::cons(Val::new_str(yy1), list::singleton(yy3));
	yyres = sexpr::new(SexprType::Bind, bind);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            12 /* let_stmt ::= Fork ID EQ expr */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY164(yy1),YYMinorType::YY116(yy3),) => {

	let bind = list::cons(Val::new_str(yy1), list::singleton(yy3));
	yyres = sexpr::new(SexprType::Fork, bind);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            13 /* expr_stmt ::= expr */
          | 15 /* block ::= arrow_block */
          | 16 /* block ::= curly_block */
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
            14 /* expr_stmt ::= DollarGT expr */
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
            17 /* arrow_block ::= BLOCKARROW expr */
          | 39 /* expr ::= IF if_expr */
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
            18 /* curly_block ::= CurlyL NEWLINE stmts CurlyR */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp2.minor,) {
 (YYMinorType::YY116(yy2),) => {

	yyres = yy2;

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            19 /* func_stmt ::= Func dfunc_1 */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY116(yy1),) => {

	/*
	const Val *plist = Val::list(p);
	const Val *mcase = Val::tuple2(plist, C);
	const list *mblk = list::singleton(mcase);

	Val *func = Val::fexpr(mblk, T);
	yyres = Val::tuple2(yy1, func, LET_ASSIGN);
	*/
	yyres = Val::Sexpr(SexprType::DefFunc, Box::new(yy1));

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            20 /* dfunc_1 ::= ID LPAREN dfunc_args RPAREN opt_typex block */
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
 (YYMinorType::YY164(yy0),YYMinorType::YY116(yy2),YYMinorType::YY173(yy4),YYMinorType::YY116(yy5),) => {

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
 YYMinorType::YY116(yyres)
}
            ,
            21 /* dfunc_args ::= */
          | 71 /* list_items ::= */
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
            22 /* dfunc_args ::= ID opt_typex */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY164(yy0),YYMinorType::YY173(yy1),) => {

	yyres = list::singleton(sexpr::id_with_type(yy0, yy1));

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            23 /* dfunc_args ::= ID opt_typex COMMA dfunc_args */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY164(yy0),YYMinorType::YY173(yy1),YYMinorType::YY116(yy3),) => {

	yyres = list::cons(sexpr::id_with_type(yy0, yy1), yy3);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            24 /* opt_typex ::= */
            => 
{
let yyres :  Type ;
match () {
 () => {

	yyres = Type::AnonVar;

} };
 YYMinorType::YY173(yyres)
}
            ,
            25 /* opt_typex ::= COLON typex */
            => 
{
let yyres :  Type ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY173(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY173(yyres)
}
            ,
            26 /* typex ::= TYPE_INT */
            => 
{
let yyres :  Type ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Type::Int;

} };
 YYMinorType::YY173(yyres)
}
            ,
            27 /* typex ::= TYPE_STR */
            => 
{
let yyres :  Type ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Type::Str;

} };
 YYMinorType::YY173(yyres)
}
            ,
            28 /* typex ::= TYPE_BOOL */
            => 
{
let yyres :  Type ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Type::Bool;

} };
 YYMinorType::YY173(yyres)
}
            ,
            29 /* typex ::= TYPE_ID */
            => 
{
let yyres :  Type ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY164(yy0),) => {

	yyres = Type::User(yy0);

},    _ => unreachable!() };
 YYMinorType::YY173(yyres)
}
            ,
            30 /* macro_stmt ::= MACRO ID LPAREN macro_args RPAREN block */
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
 (YYMinorType::YY164(yy1),YYMinorType::YY116(yy3),YYMinorType::YY116(yy5),) => {

    yyres = sexpr::new(SexprType::DefMacro,
        list::cons(Val::id(yy1),
        list::cons(yy3,
        list::cons(yy5,
        Val::Nil
    ))));

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            31 /* macro_args ::= */
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
            32 /* macro_args ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY164(yy0),) => {

    yyres = list::singleton(Val::id(yy0));

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            33 /* macro_args ::= ID COMMA macro_args */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY164(yy0),YYMinorType::YY116(yy2),) => {

    yyres = list::cons(Val::id(yy0), yy2);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            34 /* expr ::= ID LPAREN RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY164(yy0),) => {

	println!("zero param function call!");
	yyres = sexpr::call(yy0, Val::Nil);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            35 /* expr ::= ID LPAREN expr RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY164(yy0),YYMinorType::YY116(yy2),) => {

	let args = list::singleton(yy2);
	yyres = sexpr::call(yy0, args);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            36 /* expr ::= ID LPAREN tuple_args RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY164(yy0),YYMinorType::YY116(yy2),) => {

	yyres = sexpr::call(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            37 /* expr ::= term ID term */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY116(yy0),YYMinorType::YY164(yy1),YYMinorType::YY116(yy2),) => {

	/* yyres = Val::binaryop(yy0, yy1, yy2); */
	yyres = Val::Void;

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            38 /* expr ::= term DOLLAR term */
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
            40 /* if_expr ::= expr curly_block ELSE curly_block */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY116(yy0),YYMinorType::YY116(yy1),YYMinorType::YY116(yy3),) => {

	yyres = sexpr::ifexpr(yy0, yy1, yy3);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            41 /* if_expr ::= expr curly_block ELSE IF if_expr */
            => 
{
let yyres :  Val ;
let yyp4 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,yyp4.minor,) {
 (YYMinorType::YY116(yy0),YYMinorType::YY116(yy1),YYMinorType::YY116(yy4),) => {

	yyres = sexpr::ifexpr(yy0, yy1, yy4);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            44 /* expr ::= NOT expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY116(yy1),) => {

	yyres = sexpr::call("bool_not".to_string(), list::singleton(yy1));

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            45 /* expr ::= expr ConcatNewline */
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
            46 /* expr ::= MINUS expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY116(yy1),) => {

	yyres = sexpr::call("negate".to_string(), list::singleton(yy1));

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            47 /* expr ::= expr PLUS expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY116(yy0),YYMinorType::YY116(yy2),) => {

	yyres = sexpr::binaryop("int_add".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            48 /* expr ::= expr MINUS expr */
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
            49 /* expr ::= expr TIMES expr */
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
            50 /* expr ::= expr DIVIDE expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY116(yy0),YYMinorType::YY116(yy2),) => {

	yyres = sexpr::binaryop("int_div".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            51 /* expr ::= expr AND expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY116(yy0),YYMinorType::YY116(yy2),) => {

	yyres = sexpr::binaryop("and".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            52 /* expr ::= expr OR expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY116(yy0),YYMinorType::YY116(yy2),) => {

	yyres = sexpr::binaryop("or".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            53 /* expr ::= expr XOR expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY116(yy0),YYMinorType::YY116(yy2),) => {

	yyres = sexpr::binaryop("xor".to_string(),yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            54 /* expr ::= expr LT expr */
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
            55 /* expr ::= expr LTEQ expr */
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
            56 /* expr ::= expr GT expr */
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
            57 /* expr ::= expr GTEQ expr */
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
            58 /* expr ::= expr EQ expr */
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
            59 /* expr ::= expr NEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY116(yy0),YYMinorType::YY116(yy2),) => {

	let eq = sexpr::binaryop("equal".to_string(), yy0, yy2);
	yyres = sexpr::call("bool_not".to_string(), list::singleton(eq));

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            61 /* term ::= LPAREN expr RPAREN */
          | 70 /* list ::= SquareL list_items SquareR */
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
            62 /* term ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY164(yy0),) => {
 yyres = Val::id(yy0); 
},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            63 /* term ::= VOID */
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
            64 /* term ::= DollarQuestion */
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
            65 /* term ::= INT */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY66(yy0),) => {

	yyres = Val::Int(yy0);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            66 /* term ::= True */
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
            67 /* term ::= False */
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
            68 /* term ::= HASHTAG */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY164(yy0),) => {

	yyres = Val::Hashtag(Arc::new(yy0));

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            72 /* list_items ::= expr */
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
            74 /* tuple ::= LPAREN tuple_args RPAREN */
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
            75 /* tuple_args ::= term COMMA term */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY116(yy0),YYMinorType::YY116(yy2),) => {

	yyres = list::cons(yy0, list::singleton(yy2));

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            77 /* strexpr ::= StrOpen strlist StrClose */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY116(yy1),) => {

	yyres = sexpr::strexpr(yy1);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            78 /* strlist ::= */
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
            79 /* strlist ::= StrLit strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY164(yy0),YYMinorType::YY116(yy1),) => {

	yyres = list::cons(Val::new_str(yy0), yy1);

},    _ => unreachable!() };
 YYMinorType::YY116(yyres)
}
            ,
            80 /* strlist ::= ID strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY164(yy0),YYMinorType::YY116(yy1),) => {

	yyres = list::cons(Val::id(yy0), yy1);

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

