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
const YYNOCODE: i32 = 92;
type YYACTIONTYPE = u8;
const YYWILDCARD: YYCODETYPE = 0;
enum YYMinorType {
    YY0,
    YY29(Type),
    YY32(Val),
    YY63(Ast),
    YY66(String),
    YY108(i64),
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
        Token::HASHTAG(x) => YYMinorType::YY66(x),
        Token::ID(x) => YYMinorType::YY66(x),
        Token::INT(x) => YYMinorType::YY108(x),
        Token::StrLit(x) => YYMinorType::YY66(x),
        Token::TYPE_ID(x) => YYMinorType::YY66(x),
        _ => YYMinorType::YY0
  }
}
const YY_ACTTAB_COUNT: i32 = 512;
const YY_ACTION: [YYACTIONTYPE; 512] = [
 /*     0 */   128,   91,  131,   14,   13,   16,   15,   27,   25,   23,
 /*    10 */    22,   72,   35,   38,   24,   39,  122,   33,   95,  121,
 /*    20 */    94,  120,   12,  127,  149,    7,  108,  110,  109,  150,
 /*    30 */   147,  142,   92,   89,   34,   84,   10,   23,   22,   47,
 /*    40 */     3,  128,   91,  131,   78,   88,    8,   48,  133,  132,
 /*    50 */   130,  129,    6,  126,   40,   24,    4,   33,   26,   68,
 /*    60 */    51,   32,   29,   12,  114,   29,    7,  121,   96,  120,
 /*    70 */    85,  127,  142,   92,   89,  113,   84,   10,   83,   11,
 /*    80 */    47,    9,  101,  110,  109,   78,  112,    8,   46,  133,
 /*    90 */   132,  130,  129,    6,   45,   40,   77,   20,   19,   21,
 /*   100 */   136,   32,   18,   17,   14,   13,   16,   15,   27,   25,
 /*   110 */    23,   22,   48,  137,   20,   19,   21,  136,   43,   18,
 /*   120 */    17,   14,   13,   16,   15,   27,   25,   23,   22,   30,
 /*   130 */   116,    5,   20,   19,   21,  136,   42,   18,   17,   14,
 /*   140 */    13,   16,   15,   27,   25,   23,   22,   20,   19,   21,
 /*   150 */   136,   41,   18,   17,   14,   13,   16,   15,   27,   25,
 /*   160 */    23,   22,  232,  232,  232,  232,   27,   25,   23,   22,
 /*   170 */    20,   19,   21,  136,   48,   18,   17,   14,   13,   16,
 /*   180 */    15,   27,   25,   23,   22,  128,   91,  131,   72,   50,
 /*   190 */    37,   97,    2,  148,  139,    1,  121,   86,  120,   24,
 /*   200 */   127,  128,   91,  131,   72,   50,  127,   12,  111,   81,
 /*   210 */     7,  138,  121,  115,  120,   24,  127,  128,  134,  131,
 /*   220 */    72,   35,   80,   12,   44,   98,    7,   31,  121,   36,
 /*   230 */   120,    8,  127,  133,  132,  130,  129,    6,   79,   40,
 /*   240 */   102,   76,   28,  233,   93,   75,  135,    8,   49,  133,
 /*   250 */   132,  130,  129,    6,  141,   40,  100,  140,  127,  145,
 /*   260 */   144,  143,   72,   55,   90,  133,  132,  130,  129,  125,
 /*   270 */   121,   40,  120,   99,  127,   21,  136,  124,   18,   17,
 /*   280 */    14,   13,   16,   15,   27,   25,   23,   22,   68,   52,
 /*   290 */   123,  146,   74,  107,  234,   49,  121,  103,  120,   73,
 /*   300 */   127,  141,  127,   87,  140,  234,  145,  144,  143,   72,
 /*   310 */    55,  234,   82,  234,  117,  127,   49,  121,  234,  120,
 /*   320 */   234,  127,  141,  234,  234,  140,  234,  145,  144,  143,
 /*   330 */    72,   55,  234,  234,  106,  105,  104,  234,  121,  234,
 /*   340 */   120,  234,  127,  136,  234,   18,   17,   14,   13,   16,
 /*   350 */    15,   27,   25,   23,   22,  234,   72,   51,  234,   72,
 /*   360 */    69,  234,   72,   57,  121,  234,  120,  121,  127,  120,
 /*   370 */   121,  127,  120,  234,  127,  234,   72,   70,  234,   72,
 /*   380 */    61,  234,   72,  119,  121,  234,  120,  121,  127,  120,
 /*   390 */   121,  127,  120,  234,  127,   72,  118,  234,  234,  234,
 /*   400 */    72,   60,  234,  121,  234,  120,  234,  127,  121,  234,
 /*   410 */   120,  234,  127,  234,  234,  234,   72,   59,  234,  234,
 /*   420 */   234,   72,   58,  234,  121,  234,  120,  234,  127,  121,
 /*   430 */   234,  120,  234,  127,   72,   67,  234,   72,   66,  234,
 /*   440 */    72,   65,  121,  234,  120,  121,  127,  120,  121,  127,
 /*   450 */   120,  234,  127,  234,   72,   64,  234,   72,   63,  234,
 /*   460 */    72,   62,  121,  234,  120,  121,  127,  120,  121,  127,
 /*   470 */   120,  234,  127,   72,   71,  234,  234,  234,   72,   56,
 /*   480 */   234,  121,  234,  120,  234,  127,  121,  234,  120,  234,
 /*   490 */   127,  234,  234,  234,   72,   54,  234,  234,  234,   72,
 /*   500 */    53,  234,  121,  234,  120,  234,  127,  121,  234,  120,
 /*   510 */   234,  127,
];
const YY_LOOKAHEAD: [YYCODETYPE; 512] = [
 /*     0 */     1,    2,    3,   18,   19,   20,   21,   22,   23,   24,
 /*    10 */    25,   77,   78,    2,   15,    4,   82,    2,   29,   85,
 /*    20 */    30,   87,   23,   89,    0,   26,   65,   66,   67,    0,
 /*    30 */    31,   32,   33,   34,    1,   36,   37,   24,   25,   40,
 /*    40 */    26,    1,    2,    3,   45,    2,   47,   38,   49,   50,
 /*    50 */    51,   52,   53,   56,   55,   15,   47,    2,   35,   77,
 /*    60 */    78,   46,   10,   23,   54,   10,   26,   85,   28,   87,
 /*    70 */    88,   89,   32,   33,   34,   27,   36,   37,    2,   35,
 /*    80 */    40,    7,   65,   66,   67,   45,   39,   47,   26,   49,
 /*    90 */    50,   51,   52,   53,   27,   55,    2,   11,   12,   13,
 /*   100 */    14,   46,   16,   17,   18,   19,   20,   21,   22,   23,
 /*   110 */    24,   25,   38,   27,   11,   12,   13,   14,   10,   16,
 /*   120 */    17,   18,   19,   20,   21,   22,   23,   24,   25,   27,
 /*   130 */    27,   10,   11,   12,   13,   14,   26,   16,   17,   18,
 /*   140 */    19,   20,   21,   22,   23,   24,   25,   11,   12,   13,
 /*   150 */    14,   10,   16,   17,   18,   19,   20,   21,   22,   23,
 /*   160 */    24,   25,   18,   19,   20,   21,   22,   23,   24,   25,
 /*   170 */    11,   12,   13,   14,   38,   16,   17,   18,   19,   20,
 /*   180 */    21,   22,   23,   24,   25,    1,    2,    3,   77,   78,
 /*   190 */    48,   27,   60,   31,   77,   60,   85,   86,   87,   15,
 /*   200 */    89,    1,    2,    3,   77,   78,   89,   23,   71,    2,
 /*   210 */    26,   27,   85,   86,   87,   15,   89,    1,    2,    3,
 /*   220 */    77,   78,   70,   23,    2,   82,   26,   80,   85,   41,
 /*   230 */    87,   47,   89,   49,   50,   51,   52,   53,   80,   55,
 /*   240 */    70,   73,   26,   58,   59,    2,   77,   47,   63,   49,
 /*   250 */    50,   51,   52,   53,   69,   55,   73,   72,   89,   74,
 /*   260 */    75,   76,   77,   78,   90,   49,   50,   51,   52,   90,
 /*   270 */    85,   55,   87,   67,   89,   13,   14,   90,   16,   17,
 /*   280 */    18,   19,   20,   21,   22,   23,   24,   25,   77,   78,
 /*   290 */    77,   59,   67,   79,   91,   63,   85,    5,   87,   88,
 /*   300 */    89,   69,   89,   77,   72,   91,   74,   75,   76,   77,
 /*   310 */    78,   91,   59,   91,   88,   89,   63,   85,   91,   87,
 /*   320 */    91,   89,   69,   91,   91,   72,   91,   74,   75,   76,
 /*   330 */    77,   78,   91,   91,   42,   43,   44,   91,   85,   91,
 /*   340 */    87,   91,   89,   14,   91,   16,   17,   18,   19,   20,
 /*   350 */    21,   22,   23,   24,   25,   91,   77,   78,   91,   77,
 /*   360 */    78,   91,   77,   78,   85,   91,   87,   85,   89,   87,
 /*   370 */    85,   89,   87,   91,   89,   91,   77,   78,   91,   77,
 /*   380 */    78,   91,   77,   78,   85,   91,   87,   85,   89,   87,
 /*   390 */    85,   89,   87,   91,   89,   77,   78,   91,   91,   91,
 /*   400 */    77,   78,   91,   85,   91,   87,   91,   89,   85,   91,
 /*   410 */    87,   91,   89,   91,   91,   91,   77,   78,   91,   91,
 /*   420 */    91,   77,   78,   91,   85,   91,   87,   91,   89,   85,
 /*   430 */    91,   87,   91,   89,   77,   78,   91,   77,   78,   91,
 /*   440 */    77,   78,   85,   91,   87,   85,   89,   87,   85,   89,
 /*   450 */    87,   91,   89,   91,   77,   78,   91,   77,   78,   91,
 /*   460 */    77,   78,   85,   91,   87,   85,   89,   87,   85,   89,
 /*   470 */    87,   91,   89,   77,   78,   91,   91,   91,   77,   78,
 /*   480 */    91,   85,   91,   87,   91,   89,   85,   91,   87,   91,
 /*   490 */    89,   91,   91,   91,   77,   78,   91,   91,   91,   77,
 /*   500 */    78,   91,   85,   91,   87,   91,   89,   85,   91,   87,
 /*   510 */    91,   89,
];
const YY_SHIFT_USE_DFLT: i32 = -16;
const YY_SHIFT_COUNT: i32 = 96;
const YY_SHIFT_MIN: i32 = -15;
const YY_SHIFT_MAX: i32 = 329;
const YY_SHIFT_OFST: [i16; 97] = [
 /*     0 */    40,   -1,   -1,  184,  200,  200,  200,  200,  200,  200,
 /*    10 */   200,  200,  200,  200,  200,  200,  200,  200,  200,  200,
 /*    20 */   200,  200,  200,  200,  200,  200,  200,  200,  200,  216,
 /*    30 */    74,   74,  216,  216,  216,  136,  292,    9,   11,   11,
 /*    40 */    11,  243,  243,  222,  188,  188,  222,  207,  162,  162,
 /*    50 */   121,  103,   86,  159,  159,  159,  159,  159,  262,  262,
 /*    60 */   329,  329,  144,  144,  144,  144,  -15,  -15,   55,   13,
 /*    70 */    13,   13,   15,  164,  142,  141,  102,  110,   94,  108,
 /*    80 */    67,   62,   47,   44,   76,   48,   10,   52,   23,   43,
 /*    90 */    -3,   14,   33,   29,   24,  -10,  -11,
];
const YY_REDUCE_USE_DFLT: i32 = -67;
const YY_REDUCE_COUNT: i32 = 49;
const YY_REDUCE_MIN: i32 = -66;
const YY_REDUCE_MAX: i32 = 422;
const YY_REDUCE_OFST: [i16; 50] = [
 /*     0 */   185,  253,  232,  211,  143,  127,  111,  -18,  -66,  422,
 /*    10 */   417,  401,  396,  383,  380,  377,  363,  360,  357,  344,
 /*    20 */   339,  323,  318,  305,  302,  299,  285,  282,  279,  226,
 /*    30 */    17,  -39,  213,  169,  117,  225,  214,  206,  187,  179,
 /*    40 */   174,  183,  168,  170,  158,  147,  152,  137,  135,  132,
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
  76,
  74,
  74,
  75,
  75,
  65,
  65,
  66,
  67,
  69,
  71,
  70,
  70,
  70,
  80,
  80,
  79,
  79,
  79,
  79,
  72,
  73,
  73,
  73,
  78,
  78,
  78,
  78,
  78,
  78,
  82,
  82,
  78,
  78,
  78,
  78,
  78,
  78,
  78,
  78,
  78,
  78,
  78,
  78,
  78,
  78,
  78,
  78,
  78,
  78,
  78,
  77,
  77,
  77,
  77,
  77,
  77,
  77,
  77,
  77,
  85,
  86,
  86,
  86,
  87,
  88,
  88,
  89,
  90,
  90,
  90,
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
 YYMinorType::YY63(yyres)
}
            ,
            1 /* program ::= stmts */
            => 
{
let yyres :  Ast ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY32(yy0),) => {

	if list::is_empty(&yy0) {
		panic!("null program");
	}
	// ignore yyres, it doesn't really go anywhere for program
	yyres = Ast::Nothing;
	// we're done, so put yy0 in extra
	self.extra = Ok(Ast::ReplRoot(yy0));

},    _ => unreachable!() };
 YYMinorType::YY63(yyres)
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
 YYMinorType::YY63(yyres)
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
 YYMinorType::YY63(yyres)
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
 YYMinorType::YY32(yyres)
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
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy2),) => {

	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 (YYMinorType::YY32(yy0),) => {
 yyres = yy0; 
},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            7 /* stmt ::= expr_stmt */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY32(yy0),) => {

    yyres = yy0;

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 YYMinorType::YY32(yyres)
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
 (YYMinorType::YY66(yy1),YYMinorType::YY32(yy2),) => {

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
 YYMinorType::YY32(yyres)
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
 (YYMinorType::YY66(yy1),YYMinorType::YY32(yy3),) => {

	let bind = list::cons(Val::new_str(yy1), list::singleton(yy3));
	yyres = sexpr::new(SexprType::Bind, bind);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 (YYMinorType::YY66(yy1),YYMinorType::YY32(yy3),) => {

	let bind = list::cons(Val::new_str(yy1), list::singleton(yy3));
	yyres = sexpr::new(SexprType::Fork, bind);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 (YYMinorType::YY32(yy0),) => {

	yyres = yy0;

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            16 /* expr_stmt ::= DollarGT expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY32(yy1),) => {
 yyres = yy1; 
},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 (YYMinorType::YY32(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 (YYMinorType::YY32(yy2),) => {

	yyres = yy2;

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            21 /* func_stmt ::= Func dfunc_1 */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY32(yy1),) => {

	/*
	const Val *plist = Val::list(p);
	const Val *mcase = Val::tuple2(plist, C);
	const list *mblk = list::singleton(mcase);

	Val *func = Val::fexpr(mblk, T);
	yyres = Val::tuple2(yy1, func, LET_ASSIGN);
	*/
	yyres = Val::Sexpr(SexprType::DefFunc, Box::new(yy1));

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 (YYMinorType::YY66(yy0),YYMinorType::YY32(yy2),YYMinorType::YY29(yy4),YYMinorType::YY32(yy5),) => {

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
 YYMinorType::YY32(yyres)
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
 YYMinorType::YY32(yyres)
}
            ,
            24 /* dfunc_args ::= ID opt_typex */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY66(yy0),YYMinorType::YY29(yy1),) => {

	yyres = list::singleton(sexpr::id_with_type(yy0, yy1));

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 (YYMinorType::YY66(yy0),YYMinorType::YY29(yy1),YYMinorType::YY32(yy3),) => {

	yyres = list::cons(sexpr::id_with_type(yy0, yy1), yy3);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 YYMinorType::YY29(yyres)
}
            ,
            27 /* opt_typex ::= COLON typex */
            => 
{
let yyres :  Type ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY29(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY29(yyres)
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
 YYMinorType::YY29(yyres)
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
 YYMinorType::YY29(yyres)
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
 YYMinorType::YY29(yyres)
}
            ,
            31 /* typex ::= TYPE_ID */
            => 
{
let yyres :  Type ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY66(yy0),) => {

	yyres = Type::User(yy0);

},    _ => unreachable!() };
 YYMinorType::YY29(yyres)
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
 (YYMinorType::YY66(yy1),YYMinorType::YY32(yy3),YYMinorType::YY32(yy5),) => {

    yyres = sexpr::new(SexprType::DefMacro,
        list::cons(Val::id(yy1),
        list::cons(yy3,
        list::cons(yy5,
        Val::Nil
    ))));

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 YYMinorType::YY32(yyres)
}
            ,
            34 /* macro_args ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY66(yy0),) => {

    yyres = list::singleton(Val::id(yy0));

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 (YYMinorType::YY66(yy0),YYMinorType::YY32(yy2),) => {

    yyres = list::cons(Val::id(yy0), yy2);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 (YYMinorType::YY66(yy0),) => {

	println!("zero param function call!");
	yyres = sexpr::call(yy0, Val::Nil);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 (YYMinorType::YY66(yy0),YYMinorType::YY32(yy2),) => {

	let args = list::singleton(yy2);
	yyres = sexpr::call(yy0, args);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 (YYMinorType::YY66(yy0),YYMinorType::YY32(yy2),) => {

	println!("tuple function call");
	yyres = sexpr::call(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 (YYMinorType::YY32(yy0),YYMinorType::YY66(yy1),YYMinorType::YY32(yy2),) => {

	/* yyres = Val::binaryop(yy0, yy1, yy2); */
	yyres = Val::Void;

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy2),) => {

	/* yyres = Val::binaryop(yy0, yy2, D); */
	yyres = Val::Void;

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy1),YYMinorType::YY32(yy3),) => {

	yyres = sexpr::ifexpr(yy0, yy1, yy3);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy1),YYMinorType::YY32(yy4),) => {

	yyres = sexpr::ifexpr(yy0, yy1, yy4);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            46 /* expr ::= NOT expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY32(yy1),) => {

	yyres = sexpr::call("bool_not".to_string(), list::singleton(yy1));

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            47 /* expr ::= expr ConcatNewline */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY32(yy0),) => {

	let newline = Val::Str(Arc::new("\n".to_string()));
	let args = list::cons(yy0, list::singleton(newline));
	yyres = sexpr::new(SexprType::StrExpr, args)

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            48 /* expr ::= MINUS expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY32(yy1),) => {

	println!("found minus {:?}", yy1);
	yyres = sexpr::call("negate".to_string(), list::singleton(yy1));

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy2),) => {

	yyres = sexpr::binaryop("int_add".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy2),) => {

	yyres = sexpr::binaryop("int_sub".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy2),) => {

	yyres = sexpr::binaryop("int_mult".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy2),) => {

	yyres = sexpr::binaryop("int_div".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy2),) => {

	yyres = sexpr::binaryop("and".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy2),) => {

	yyres = sexpr::binaryop("or".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy2),) => {

	yyres = sexpr::binaryop("xor".to_string(),yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy2),) => {

	yyres = sexpr::binaryop("less_than".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy2),) => {

	yyres = sexpr::binaryop("less_than_equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy2),) => {

	yyres = sexpr::binaryop("greater_than".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy2),) => {

	yyres = sexpr::binaryop("greater_than_equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy2),) => {

	yyres = sexpr::binaryop("equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy2),) => {

	let eq = sexpr::binaryop("equal".to_string(), yy0, yy2);
	yyres = sexpr::call("bool_not".to_string(), list::singleton(eq));

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 (YYMinorType::YY32(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            64 /* term ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY66(yy0),) => {
 yyres = Val::id(yy0); 
},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            65 /* term ::= VOID */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {

	println!("found literal void\n");
	yyres = Val::Void;

} };
 YYMinorType::YY32(yyres)
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
 YYMinorType::YY32(yyres)
}
            ,
            67 /* term ::= INT */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY108(yy0),) => {

	yyres = Val::Int(yy0);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 YYMinorType::YY32(yyres)
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
 YYMinorType::YY32(yyres)
}
            ,
            70 /* term ::= HASHTAG */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY66(yy0),) => {

	yyres = Val::Hashtag(Arc::new(yy0));

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            74 /* list_items ::= expr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY32(yy0),) => {

	yyres = list::singleton(yy0);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy2),) => {

	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 (YYMinorType::YY32(yy1),) => {

	yyres = Val::tuple_from_list(yy1);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 (YYMinorType::YY32(yy0),YYMinorType::YY32(yy2),) => {

	yyres = list::cons(yy0, list::singleton(yy2));

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 (YYMinorType::YY32(yy1),) => {

	yyres = sexpr::strexpr(yy1);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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
 YYMinorType::YY32(yyres)
}
            ,
            81 /* strlist ::= StrLit strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY66(yy0),YYMinorType::YY32(yy1),) => {

	yyres = list::cons(Val::new_str(yy0), yy1);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
}
            ,
            82 /* strlist ::= ID strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY66(yy0),YYMinorType::YY32(yy1),) => {

	yyres = list::cons(Val::id(yy0), yy1);

},    _ => unreachable!() };
 YYMinorType::YY32(yyres)
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

