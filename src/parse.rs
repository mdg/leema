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
const YYNSTATE: i32 = 142;
const YYNRULE: i32 = 80;
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
        Token::HASHTAG(x) => YYMinorType::YY4(x),
        Token::ID(x) => YYMinorType::YY4(x),
        Token::INT(x) => YYMinorType::YY140(x),
        Token::StrLit(x) => YYMinorType::YY4(x),
        Token::TYPE_ID(x) => YYMinorType::YY4(x),
        _ => YYMinorType::YY0
  }
}
const YY_ACTTAB_COUNT: i32 = 527;
const YY_ACTION: [YYACTIONTYPE; 527] = [
 /*     0 */   121,   85,  124,   14,   13,   16,   15,   27,   25,   23,
 /*    10 */    22,   70,   34,   37,   24,   38,  115,   32,   89,  114,
 /*    20 */    46,  113,   12,  120,   88,    7,  101,  103,  102,    4,
 /*    30 */   140,  135,   86,   83,  142,   78,   10,   23,   22,   45,
 /*    40 */    33,  121,   85,  124,   40,  143,    8,    3,  126,  125,
 /*    50 */   123,  122,    6,  119,   39,   24,   82,   32,   26,   66,
 /*    60 */    49,   31,   29,   12,  107,   29,    7,  114,   90,  113,
 /*    70 */    79,  120,  135,   86,   83,  106,   78,   10,   77,   11,
 /*    80 */    45,    9,  132,   44,  128,   40,  105,    8,   43,  126,
 /*    90 */   125,  123,  122,    6,  120,   39,  120,   20,   19,   21,
 /*   100 */   129,   31,   18,   17,   14,   13,   16,   15,   27,   25,
 /*   110 */    23,   22,   46,  130,   20,   19,   21,  129,   41,   18,
 /*   120 */    17,   14,   13,   16,   15,   27,   25,   23,   22,   36,
 /*   130 */   109,    5,   20,   19,   21,  129,   91,   18,   17,   14,
 /*   140 */    13,   16,   15,   27,   25,   23,   22,   20,   19,   21,
 /*   150 */   129,    2,   18,   17,   14,   13,   16,   15,   27,   25,
 /*   160 */    23,   22,  222,  222,  222,  222,   27,   25,   23,   22,
 /*   170 */    20,   19,   21,  129,   46,   18,   17,   14,   13,   16,
 /*   180 */    15,   27,   25,   23,   22,  121,   85,  124,   70,   48,
 /*   190 */     1,  141,  104,   75,   74,   42,  114,   80,  113,   24,
 /*   200 */   120,  121,   85,  124,   70,   48,   96,   12,   81,  116,
 /*   210 */     7,  131,  114,  108,  113,   24,  120,   70,   34,  110,
 /*   220 */   120,  120,   92,   12,   30,  114,    7,  113,   35,  120,
 /*   230 */    73,    8,   95,  126,  125,  123,  122,    6,   94,   39,
 /*   240 */    93,   84,  118,   99,   98,   97,  117,    8,  100,  126,
 /*   250 */   125,  123,  122,    6,  224,   39,   66,   50,  224,   72,
 /*   260 */   223,   87,  224,  224,  114,   47,  113,   71,  120,  224,
 /*   270 */   224,  134,  133,   70,   49,  138,  137,  136,   70,   53,
 /*   280 */   224,  114,  224,  113,  224,  120,  114,  224,  113,  224,
 /*   290 */   120,   21,  129,  224,   18,   17,   14,   13,   16,   15,
 /*   300 */    27,   25,   23,   22,   70,   67,  224,  139,  224,  224,
 /*   310 */   224,   47,  114,  224,  113,  224,  120,  134,  133,  224,
 /*   320 */   224,  138,  137,  136,   70,   53,  121,  127,  124,  224,
 /*   330 */   224,   76,  114,  224,  113,   47,  120,  224,  224,  224,
 /*   340 */   224,  134,  133,  224,  224,  138,  137,  136,   70,   53,
 /*   350 */   224,   28,  224,  224,  224,  224,  114,  224,  113,  224,
 /*   360 */   120,  129,  224,   18,   17,   14,   13,   16,   15,   27,
 /*   370 */    25,   23,   22,  224,  126,  125,  123,  122,   70,   55,
 /*   380 */    39,   70,   68,  224,   70,   59,  114,  224,  113,  114,
 /*   390 */   120,  113,  114,  120,  113,  224,  120,  224,   70,  112,
 /*   400 */   224,  224,  224,   70,  111,  224,  114,  224,  113,  224,
 /*   410 */   120,  114,  224,  113,  224,  120,  224,  224,   70,   58,
 /*   420 */   224,  224,  224,  224,   70,   57,  114,  224,  113,  224,
 /*   430 */   120,  224,  114,  224,  113,  224,  120,   70,   56,  224,
 /*   440 */    70,   65,  224,   70,   64,  114,  224,  113,  114,  120,
 /*   450 */   113,  114,  120,  113,  224,  120,   70,   63,  224,   70,
 /*   460 */    62,  224,   70,   61,  114,  224,  113,  114,  120,  113,
 /*   470 */   114,  120,  113,  224,  120,   70,   60,  224,  224,  224,
 /*   480 */    70,   69,  224,  114,  224,  113,  224,  120,  114,  224,
 /*   490 */   113,  224,  120,  224,  224,   70,   54,  224,  224,  224,
 /*   500 */   224,   70,   52,  114,  224,  113,  224,  120,  224,  114,
 /*   510 */   224,  113,  224,  120,   70,   51,  224,  224,  224,  224,
 /*   520 */   224,  224,  114,  224,  113,  224,  120,
];
const YY_LOOKAHEAD: [YYCODETYPE; 527] = [
 /*     0 */     1,    2,    3,   18,   19,   20,   21,   22,   23,   24,
 /*    10 */    25,   76,   77,    2,   15,    4,   81,    2,   29,   84,
 /*    20 */    38,   86,   23,   88,   30,   26,   65,   66,   67,   47,
 /*    30 */    31,   32,   33,   34,    0,   36,   37,   24,   25,   40,
 /*    40 */     1,    1,    2,    3,   45,    0,   47,   26,   49,   50,
 /*    50 */    51,   52,   53,   56,   55,   15,    2,    2,   35,   76,
 /*    60 */    77,   46,   10,   23,   54,   10,   26,   84,   28,   86,
 /*    70 */    87,   88,   32,   33,   34,   27,   36,   37,    2,   35,
 /*    80 */    40,    7,   76,   26,   76,   45,   39,   47,   27,   49,
 /*    90 */    50,   51,   52,   53,   88,   55,   88,   11,   12,   13,
 /*   100 */    14,   46,   16,   17,   18,   19,   20,   21,   22,   23,
 /*   110 */    24,   25,   38,   27,   11,   12,   13,   14,   10,   16,
 /*   120 */    17,   18,   19,   20,   21,   22,   23,   24,   25,   48,
 /*   130 */    27,   10,   11,   12,   13,   14,   27,   16,   17,   18,
 /*   140 */    19,   20,   21,   22,   23,   24,   25,   11,   12,   13,
 /*   150 */    14,   60,   16,   17,   18,   19,   20,   21,   22,   23,
 /*   160 */    24,   25,   18,   19,   20,   21,   22,   23,   24,   25,
 /*   170 */    11,   12,   13,   14,   38,   16,   17,   18,   19,   20,
 /*   180 */    21,   22,   23,   24,   25,    1,    2,    3,   76,   77,
 /*   190 */    60,   31,   72,    2,   71,    2,   84,   85,   86,   15,
 /*   200 */    88,    1,    2,    3,   76,   77,    5,   23,   76,   76,
 /*   210 */    26,   27,   84,   85,   86,   15,   88,   76,   77,   87,
 /*   220 */    88,   88,   81,   23,   79,   84,   26,   86,   41,   88,
 /*   230 */    79,   47,   71,   49,   50,   51,   52,   53,   72,   55,
 /*   240 */    67,   89,   89,   42,   43,   44,   89,   47,   78,   49,
 /*   250 */    50,   51,   52,   53,   90,   55,   76,   77,   90,   67,
 /*   260 */    58,   59,   90,   90,   84,   63,   86,   87,   88,   90,
 /*   270 */    90,   69,   70,   76,   77,   73,   74,   75,   76,   77,
 /*   280 */    90,   84,   90,   86,   90,   88,   84,   90,   86,   90,
 /*   290 */    88,   13,   14,   90,   16,   17,   18,   19,   20,   21,
 /*   300 */    22,   23,   24,   25,   76,   77,   90,   59,   90,   90,
 /*   310 */    90,   63,   84,   90,   86,   90,   88,   69,   70,   90,
 /*   320 */    90,   73,   74,   75,   76,   77,    1,    2,    3,   90,
 /*   330 */    90,   59,   84,   90,   86,   63,   88,   90,   90,   90,
 /*   340 */    90,   69,   70,   90,   90,   73,   74,   75,   76,   77,
 /*   350 */    90,   26,   90,   90,   90,   90,   84,   90,   86,   90,
 /*   360 */    88,   14,   90,   16,   17,   18,   19,   20,   21,   22,
 /*   370 */    23,   24,   25,   90,   49,   50,   51,   52,   76,   77,
 /*   380 */    55,   76,   77,   90,   76,   77,   84,   90,   86,   84,
 /*   390 */    88,   86,   84,   88,   86,   90,   88,   90,   76,   77,
 /*   400 */    90,   90,   90,   76,   77,   90,   84,   90,   86,   90,
 /*   410 */    88,   84,   90,   86,   90,   88,   90,   90,   76,   77,
 /*   420 */    90,   90,   90,   90,   76,   77,   84,   90,   86,   90,
 /*   430 */    88,   90,   84,   90,   86,   90,   88,   76,   77,   90,
 /*   440 */    76,   77,   90,   76,   77,   84,   90,   86,   84,   88,
 /*   450 */    86,   84,   88,   86,   90,   88,   76,   77,   90,   76,
 /*   460 */    77,   90,   76,   77,   84,   90,   86,   84,   88,   86,
 /*   470 */    84,   88,   86,   90,   88,   76,   77,   90,   90,   90,
 /*   480 */    76,   77,   90,   84,   90,   86,   90,   88,   84,   90,
 /*   490 */    86,   90,   88,   90,   90,   76,   77,   90,   90,   90,
 /*   500 */    90,   76,   77,   84,   90,   86,   90,   88,   90,   84,
 /*   510 */    90,   86,   90,   88,   76,   77,   90,   90,   90,   90,
 /*   520 */    90,   90,   84,   90,   86,   90,   88,
];
const YY_SHIFT_USE_DFLT: i32 = -19;
const YY_SHIFT_COUNT: i32 = 90;
const YY_SHIFT_MIN: i32 = -18;
const YY_SHIFT_MAX: i32 = 347;
const YY_SHIFT_OFST: [i16; 91] = [
 /*     0 */    40,   -1,   -1,  184,  200,  200,  200,  200,  200,  200,
 /*    10 */   200,  200,  200,  200,  200,  200,  200,  200,  200,  200,
 /*    20 */   200,  200,  200,  200,  200,  200,  200,  200,  200,  325,
 /*    30 */    74,  325,  325,  325,  136,  201,  -18,   11,   11,   11,
 /*    40 */   191,  193,  187,  187,  193,  191,  160,  160,  121,  103,
 /*    50 */    86,  159,  159,  159,  159,  159,  278,  278,  347,  347,
 /*    60 */   144,  144,  144,  144,  -15,  -15,   55,   13,   13,   13,
 /*    70 */    15,  109,   81,  108,   61,   57,   47,   44,   76,   48,
 /*    80 */    10,   52,   23,   54,   -3,   21,   39,   45,   34,   -6,
 /*    90 */   -11,
];
const YY_REDUCE_USE_DFLT: i32 = -66;
const YY_REDUCE_COUNT: i32 = 47;
const YY_REDUCE_MIN: i32 = -65;
const YY_REDUCE_MAX: i32 = 438;
const YY_REDUCE_OFST: [i16; 48] = [
 /*     0 */   202,  272,  248,  180,  141,  128,  112,  -17,  -65,  438,
 /*    10 */   425,  419,  404,  399,  386,  383,  380,  367,  364,  361,
 /*    20 */   348,  342,  327,  322,  308,  305,  302,  228,  197,  132,
 /*    30 */   -39,  133,    8,    6,  192,  170,  173,  157,  153,  152,
 /*    40 */   166,  161,  151,  145,  123,  120,  130,   91,
];
const YY_DEFAULT: [YYACTIONTYPE; 142] = [
 /*     0 */   146,  146,  146,  222,  222,  212,  212,  222,  222,  222,
 /*    10 */   222,  222,  222,  222,  222,  222,  222,  222,  222,  222,
 /*    20 */   222,  222,  222,  222,  222,  222,  222,  222,  222,  222,
 /*    30 */   222,  222,  222,  222,  222,  222,  222,  219,  219,  219,
 /*    40 */   222,  172,  165,  165,  172,  222,  222,  222,  213,  222,
 /*    50 */   222,  161,  158,  157,  156,  155,  194,  193,  192,  185,
 /*    60 */   200,  199,  198,  197,  196,  195,  201,  188,  189,  187,
 /*    70 */   201,  222,  222,  173,  222,  222,  222,  222,  222,  222,
 /*    80 */   222,  216,  222,  222,  222,  203,  222,  222,  222,  222,
 /*    90 */   222,  177,  182,  181,  171,  174,  170,  169,  168,  167,
 /*   100 */   166,  164,  160,  159,  163,  162,  215,  211,  214,  202,
 /*   110 */   217,  191,  190,  184,  183,  180,  179,  221,  220,  218,
 /*   120 */   210,  209,  208,  207,  206,  205,  204,  203,  178,  186,
 /*   130 */   176,  175,  154,  153,  152,  151,  150,  149,  148,  147,
 /*   140 */   145,  144,
];

/* TMPL: fallback tokens */

const YY_FALLBACK: [i32; 0] = [
];

/* TMPL: symbol names */


/* TMPL: rules */


/* TMPL: destructors */


/* TMPL: stack-overflow */


/* TMPL: stack-overflow */

const YY_RULE_INFO: [YYCODETYPE; 80] = [
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
  75,
  73,
  73,
  74,
  74,
  65,
  65,
  66,
  67,
  69,
  72,
  79,
  79,
  78,
  78,
  78,
  78,
  70,
  71,
  71,
  71,
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
          | 7 /* stmt ::= expr_stmt */
          | 8 /* stmt ::= fail_stmt */
          | 10 /* stmt ::= func_stmt */
          | 11 /* stmt ::= macro_stmt */
          | 41 /* expr ::= list */
          | 42 /* expr ::= tuple */
          | 59 /* expr ::= term */
          | 68 /* term ::= strexpr */
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
 YYMinorType::YY44(yyres)
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
 (YYMinorType::YY4(yy1),YYMinorType::YY44(yy3),) => {

	let bind = list::cons(Val::new_str(yy1), list::singleton(yy3));
	yyres = sexpr::new(SexprType::Bind, bind);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
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
          | 38 /* expr ::= IF if_expr */
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
            23 /* opt_typex ::= */
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
            24 /* opt_typex ::= COLON typex */
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
            25 /* typex ::= TYPE_INT */
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
            26 /* typex ::= TYPE_STR */
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
            27 /* typex ::= TYPE_BOOL */
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
            28 /* typex ::= TYPE_ID */
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
            29 /* macro_stmt ::= MACRO dfunc_1 */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY44(yy1),) => {

	yyres = Val::Sexpr(SexprType::DefMacro, Box::new(yy1));

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            30 /* dfunc_args ::= */
          | 70 /* list_items ::= */
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
            31 /* dfunc_args ::= ID opt_typex */
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
            32 /* dfunc_args ::= ID opt_typex COMMA dfunc_args */
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
            33 /* expr ::= ID LPAREN RPAREN */
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
            34 /* expr ::= ID LPAREN expr RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY4(yy0),YYMinorType::YY44(yy2),) => {

	println!("1 param function call!");
	let args = list::singleton(yy2);
	yyres = sexpr::call(yy0, args);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            35 /* expr ::= ID LPAREN tuple_args RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY4(yy0),YYMinorType::YY44(yy2),) => {

	println!("tuple function call");
	yyres = sexpr::call(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            36 /* expr ::= term ID term */
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
            37 /* expr ::= term DOLLAR term */
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
            39 /* if_expr ::= expr curly_block ELSE curly_block */
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
            40 /* if_expr ::= expr curly_block ELSE IF if_expr */
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
            43 /* expr ::= NOT expr */
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
            44 /* expr ::= expr ConcatNewline */
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
            45 /* expr ::= MINUS expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY44(yy1),) => {

	println!("found minus {:?}", yy1);
	yyres = sexpr::call("negate".to_string(), list::singleton(yy1));

},    _ => unreachable!() };
 YYMinorType::YY44(yyres)
}
            ,
            46 /* expr ::= expr PLUS expr */
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
            47 /* expr ::= expr MINUS expr */
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
            48 /* expr ::= expr TIMES expr */
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
            49 /* expr ::= expr DIVIDE expr */
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
            50 /* expr ::= expr AND expr */
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
            51 /* expr ::= expr OR expr */
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
            52 /* expr ::= expr XOR expr */
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
            53 /* expr ::= expr LT expr */
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
            54 /* expr ::= expr LTEQ expr */
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
            55 /* expr ::= expr GT expr */
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
            56 /* expr ::= expr GTEQ expr */
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
            57 /* expr ::= expr EQ expr */
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
            58 /* expr ::= expr NEQ expr */
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
            60 /* term ::= LPAREN expr RPAREN */
          | 69 /* list ::= SquareL list_items SquareR */
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
            61 /* term ::= ID */
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
            62 /* term ::= VOID */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {

	println!("found literal void\n");
	yyres = Val::Void;

} };
 YYMinorType::YY44(yyres)
}
            ,
            63 /* term ::= DollarQuestion */
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
            64 /* term ::= INT */
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
            65 /* term ::= True */
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
            66 /* term ::= False */
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
            67 /* term ::= HASHTAG */
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
            71 /* list_items ::= expr */
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
            72 /* list_items ::= expr COMMA list_items */
          | 75 /* tuple_args ::= term COMMA tuple_args */
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
            73 /* tuple ::= LPAREN tuple_args RPAREN */
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
            74 /* tuple_args ::= term COMMA term */
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
            76 /* strexpr ::= StrOpen strlist StrClose */
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
            77 /* strlist ::= */
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
            78 /* strlist ::= StrLit strlist */
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
            79 /* strlist ::= ID strlist */
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

	println!("syntax error");
	panic!("Syntax error. wtf? @ <line,column>?");
    }

    fn yy_accept(&mut self) {
        self.yystack.clear();

	println!("parse accepted");
    }
}

