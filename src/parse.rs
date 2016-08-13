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
const YYNOCODE: i32 = 102;
type YYACTIONTYPE = u16;
const YYWILDCARD: YYCODETYPE = 1;
enum YYMinorType {
    YY0,
    YY21(TokenData<String>),
    YY60(TokenLoc),
    YY78(String),
    YY112(i64),
    YY129(Ast),
    YY153(Type),
    YY182(Val),
}
const YYNSTATE: i32 = 211;
const YYNRULE: i32 = 112;
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
    ID( TokenData<String> ), //6
    INT( i64 ), //7
    PLUS( TokenLoc ), //8
    SLASH( TokenLoc ), //9
    StrLit( String ), //10
    TYPE_ID( TokenData<String> ), //11
    ASSIGN, //12
    BLOCKARROW, //13
    WHERE, //14
    GIVEN, //15
    OR, //16
    XOR, //17
    AND, //18
    ConcatNewline, //19
    NOT, //20
    EQ, //21
    NEQ, //22
    GT, //23
    GTEQ, //24
    LT, //25
    LTEQ, //26
    MINUS, //27
    TIMES, //28
    MOD, //29
    DOT, //30
    LPAREN, //31
    RPAREN, //32
    FAILED, //33
    WITH, //34
    CONTEXTID, //35
    STRUCT, //36
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
pub const TOKEN_DOUBLEDASH: i32 = 3;
pub const TOKEN_ELSE: i32 = 4;
pub const TOKEN_HASHTAG: i32 = 5;
pub const TOKEN_ID: i32 = 6;
pub const TOKEN_INT: i32 = 7;
pub const TOKEN_PLUS: i32 = 8;
pub const TOKEN_SLASH: i32 = 9;
pub const TOKEN_StrLit: i32 = 10;
pub const TOKEN_TYPE_ID: i32 = 11;
pub const TOKEN_ASSIGN: i32 = 12;
pub const TOKEN_BLOCKARROW: i32 = 13;
pub const TOKEN_WHERE: i32 = 14;
pub const TOKEN_GIVEN: i32 = 15;
pub const TOKEN_OR: i32 = 16;
pub const TOKEN_XOR: i32 = 17;
pub const TOKEN_AND: i32 = 18;
pub const TOKEN_ConcatNewline: i32 = 19;
pub const TOKEN_NOT: i32 = 20;
pub const TOKEN_EQ: i32 = 21;
pub const TOKEN_NEQ: i32 = 22;
pub const TOKEN_GT: i32 = 23;
pub const TOKEN_GTEQ: i32 = 24;
pub const TOKEN_LT: i32 = 25;
pub const TOKEN_LTEQ: i32 = 26;
pub const TOKEN_MINUS: i32 = 27;
pub const TOKEN_TIMES: i32 = 28;
pub const TOKEN_MOD: i32 = 29;
pub const TOKEN_DOT: i32 = 30;
pub const TOKEN_LPAREN: i32 = 31;
pub const TOKEN_RPAREN: i32 = 32;
pub const TOKEN_FAILED: i32 = 33;
pub const TOKEN_WITH: i32 = 34;
pub const TOKEN_CONTEXTID: i32 = 35;
pub const TOKEN_STRUCT: i32 = 36;
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
        &Token::DOUBLEDASH(_) => TOKEN_DOUBLEDASH,
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
        &Token::DOT => TOKEN_DOT,
        &Token::LPAREN => TOKEN_LPAREN,
        &Token::RPAREN => TOKEN_RPAREN,
        &Token::FAILED => TOKEN_FAILED,
        &Token::WITH => TOKEN_WITH,
        &Token::CONTEXTID => TOKEN_CONTEXTID,
        &Token::STRUCT => TOKEN_STRUCT,
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
        Token::COMMA(x) => YYMinorType::YY60(x),
        Token::DOUBLEDASH(x) => YYMinorType::YY60(x),
        Token::ELSE(x) => YYMinorType::YY60(x),
        Token::HASHTAG(x) => YYMinorType::YY21(x),
        Token::ID(x) => YYMinorType::YY21(x),
        Token::INT(x) => YYMinorType::YY112(x),
        Token::PLUS(x) => YYMinorType::YY60(x),
        Token::SLASH(x) => YYMinorType::YY60(x),
        Token::StrLit(x) => YYMinorType::YY78(x),
        Token::TYPE_ID(x) => YYMinorType::YY21(x),
        _ => YYMinorType::YY0
  }
}
const YY_ACTTAB_COUNT: i32 = 974;
const YY_ACTION: [YYACTIONTYPE; 974] = [
 /*     0 */   165,  171,  168,   95,   71,  172,  195,  125,    4,  174,
 /*    10 */     1,  173,  115,  164,   38,   15,  134,   95,   73,  172,
 /*    20 */   131,   44,   14,  174,  116,  173,    8,  164,  135,   36,
 /*    30 */   211,   53,  212,  128,  123,  114,   12,  112,  199,  198,
 /*    40 */   197,  196,  106,    3,  177,   11,   70,   16,  167,  166,
 /*    50 */   133,  170,  169,    6,  190,   43,  172,  165,  171,  168,
 /*    60 */    95,   74,  172,  195,  164,  202,  174,  159,  173,  156,
 /*    70 */   164,   42,   15,   35,   95,   73,  172,  147,   54,   14,
 /*    80 */   174,  163,  173,    8,  164,  181,  180,  184,   53,  129,
 /*    90 */   128,  123,  114,   12,  112,  199,  198,  197,  196,  106,
 /*   100 */     3,  201,   44,   70,   16,  167,  166,   52,  170,  169,
 /*   110 */     6,   37,   43,   58,  165,  171,  168,   95,   72,  172,
 /*   120 */   195,  127,  138,  174,  172,  173,   99,  164,  126,   15,
 /*   130 */    34,   38,  164,  183,  182,  179,   14,  120,  185,  176,
 /*   140 */     8,   56,  165,  171,  168,  118,   40,  192,  195,  161,
 /*   150 */    40,   30,  199,  198,  197,  196,  188,   15,  160,   40,
 /*   160 */    70,   16,  167,  166,   14,  170,  169,    6,    8,   43,
 /*   170 */    31,   29,  125,    4,  159,   94,  185,  119,   42,  122,
 /*   180 */   199,  198,  197,  196,    1,   69,  185,   17,   70,   16,
 /*   190 */   167,  166,  109,  170,  169,    6,  100,   43,  117,  165,
 /*   200 */   171,  168,  187,  108,  175,  195,  162,   95,   48,  172,
 /*   210 */   158,  157,  155,  174,   15,  173,  113,  164,   13,  111,
 /*   220 */    39,   14,   67,  153,   66,    8,  137,  165,  171,  168,
 /*   230 */   152,   64,  105,  195,   63,  149,   61,  199,  198,  197,
 /*   240 */   196,   62,   15,  146,  143,   70,   16,  167,  166,   14,
 /*   250 */   170,  169,    6,    8,   43,  136,  124,   18,    1,  186,
 /*   260 */    68,   39,   65,  102,  110,  199,  198,  197,  196,   41,
 /*   270 */    51,   10,  103,   70,   16,  167,  166,    7,  170,  169,
 /*   280 */     6,  107,   43,   33,   30,  195,  150,  104,  148,   54,
 /*   290 */   139,   27,   26,   28,  191,  145,   21,   20,   23,   22,
 /*   300 */    25,   24,   32,   31,   29,  125,    4,  193,    7,   10,
 /*   310 */   141,    9,  142,  140,   33,   30,  144,  199,  198,  197,
 /*   320 */   196,   45,   27,   26,   28,  191,  101,   21,   20,   23,
 /*   330 */    22,   25,   24,   32,   31,   29,  125,    4,  189,  165,
 /*   340 */   171,  168,  200,  151,   57,  195,  121,   33,   30,   55,
 /*   350 */    60,   59,  130,  325,   15,  325,  325,  325,  325,  325,
 /*   360 */   325,   14,  325,  325,  325,    8,   32,   31,   29,  125,
 /*   370 */     4,  325,  325,  325,  325,  325,  325,  199,  198,  197,
 /*   380 */   196,  325,  325,  325,  325,   70,   16,  167,  166,  325,
 /*   390 */   170,  169,    6,  325,   43,   33,   30,  325,  181,  180,
 /*   400 */   184,  325,  325,   27,   26,   28,  191,  325,   21,   20,
 /*   410 */    23,   22,   25,   24,   32,   31,   29,  125,    4,  194,
 /*   420 */    33,   30,  325,  325,   37,  178,  325,  325,   27,   26,
 /*   430 */    28,  191,  325,   21,   20,   23,   22,   25,   24,   32,
 /*   440 */    31,   29,  125,    4,  189,    7,  183,  182,  179,  325,
 /*   450 */   325,   33,   30,  325,  325,  325,  325,  325,  325,   27,
 /*   460 */    26,   28,  191,  325,   21,   20,   23,   22,   25,   24,
 /*   470 */    32,   31,   29,  125,    4,    5,  325,   95,   76,  172,
 /*   480 */   325,   33,   30,  174,  325,  173,  325,  164,  325,   27,
 /*   490 */    26,   28,  191,  325,   21,   20,   23,   22,   25,   24,
 /*   500 */    32,   31,   29,  125,    4,   33,   30,  325,  325,  325,
 /*   510 */     1,  325,  325,   27,   26,   28,  191,  325,   21,   20,
 /*   520 */    23,   22,   25,   24,   32,   31,   29,  125,    4,   33,
 /*   530 */    30,  325,  325,  325,  325,  325,  325,   27,   26,   28,
 /*   540 */   191,  325,   21,   20,   23,   22,   25,   24,   32,   31,
 /*   550 */    29,  125,    4,  325,   95,   93,  172,  325,   33,   30,
 /*   560 */   174,  325,  173,  325,  164,  325,   27,   26,   28,  191,
 /*   570 */    39,   21,   20,   23,   22,   25,   24,   32,   31,   29,
 /*   580 */   125,    4,  324,  132,    2,  325,  325,  205,  325,  325,
 /*   590 */   325,  204,  325,  325,  203,  325,  325,  325,  325,  325,
 /*   600 */   325,  209,  325,  325,  208,  207,  206,   95,   78,  172,
 /*   610 */    33,   30,  325,  174,  325,  173,  325,  164,  325,  325,
 /*   620 */    28,  191,  325,   21,   20,   23,   22,   25,   24,   32,
 /*   630 */    31,   29,  125,    4,  210,    2,  325,  325,  205,  325,
 /*   640 */   325,  325,  204,  325,  325,  203,  325,  325,  325,  325,
 /*   650 */   325,  325,  209,  325,  325,  208,  207,  206,   95,   78,
 /*   660 */   172,  325,  325,  325,  174,  325,  173,  325,  164,  154,
 /*   670 */     2,  325,  325,  205,  325,  325,  325,  204,  325,  325,
 /*   680 */   203,  325,  325,  325,  325,  325,  325,  209,  325,  325,
 /*   690 */   208,  207,  206,   95,   78,  172,   33,   30,  325,  174,
 /*   700 */   325,  173,  325,  164,  325,  325,  325,  191,  325,   21,
 /*   710 */    20,   23,   22,   25,   24,   32,   31,   29,  125,    4,
 /*   720 */   165,  171,  168,   95,   92,  172,  195,  325,  325,  174,
 /*   730 */   325,  173,  325,  164,   95,   98,  172,  325,  325,  325,
 /*   740 */   174,  325,  173,  325,  164,  325,   19,   95,   97,  172,
 /*   750 */   325,  325,  325,  174,  325,  173,  325,  164,  199,  198,
 /*   760 */   197,  196,  325,  325,  325,  325,  325,  325,  167,  166,
 /*   770 */   325,  170,  169,  325,  325,   43,  325,   33,   30,   95,
 /*   780 */    96,  172,  325,  325,  325,  174,  325,  173,  325,  164,
 /*   790 */   323,  323,  323,  323,   25,   24,   32,   31,   29,  125,
 /*   800 */     4,   95,   84,  172,   95,   82,  172,  174,  325,  173,
 /*   810 */   174,  164,  173,  325,  164,   95,   81,  172,   95,   90,
 /*   820 */   172,  174,  325,  173,  174,  164,  173,  325,  164,   95,
 /*   830 */    89,  172,  325,  325,  325,  174,  325,  173,  325,  164,
 /*   840 */   325,  325,   95,   88,  172,  325,  325,  325,  174,  325,
 /*   850 */   173,  325,  164,  325,  325,  325,  325,  325,   95,   87,
 /*   860 */   172,   95,   86,  172,  174,  325,  173,  174,  164,  173,
 /*   870 */   325,  164,  325,  325,   95,   85,  172,  325,  325,  325,
 /*   880 */   174,  325,  173,  325,  164,   95,   75,  172,   95,   50,
 /*   890 */   172,  174,  325,  173,  174,  164,  173,  325,  164,   95,
 /*   900 */    80,  172,  325,  325,  325,  174,  325,  173,  325,  164,
 /*   910 */    95,   49,  172,   95,   83,  172,  174,  325,  173,  174,
 /*   920 */   164,  173,  325,  164,   95,   91,  172,  325,  325,  325,
 /*   930 */   174,  325,  173,  325,  164,  325,  325,   95,   79,  172,
 /*   940 */   325,  325,  325,  174,  325,  173,  325,  164,   95,   77,
 /*   950 */   172,   95,   47,  172,  174,  325,  173,  174,  164,  173,
 /*   960 */   325,  164,  325,   95,   46,  172,  325,  325,  325,  174,
 /*   970 */   325,  173,  325,  164,
];
const YY_LOOKAHEAD: [YYCODETYPE; 974] = [
 /*     0 */     5,    6,    7,   88,   89,   90,   11,   30,   31,   94,
 /*    10 */    13,   96,   97,   98,    2,   20,   34,   88,   89,   90,
 /*    20 */    83,   84,   27,   94,   95,   96,   31,   98,   33,    6,
 /*    30 */     0,   36,    0,   38,   39,   40,   41,   42,   43,   44,
 /*    40 */    45,   46,   47,   48,   32,   48,   51,   52,   53,   54,
 /*    50 */    35,   56,   57,   58,   88,   60,   90,    5,    6,    7,
 /*    60 */    88,   89,   90,   11,   98,    3,   94,    6,   96,   97,
 /*    70 */    98,   10,   20,   50,   88,   89,   90,    3,    4,   27,
 /*    80 */    94,   95,   96,   31,   98,    5,    6,    7,   36,    6,
 /*    90 */    38,   39,   40,   41,   42,   43,   44,   45,   46,   47,
 /*   100 */    48,   83,   84,   51,   52,   53,   54,   37,   56,   57,
 /*   110 */    58,   31,   60,    4,    5,    6,    7,   88,   89,   90,
 /*   120 */    11,   31,   88,   94,   90,   96,   97,   98,    5,   20,
 /*   130 */     2,    2,   98,   53,   54,   55,   27,   79,   80,   81,
 /*   140 */    31,    4,    5,    6,    7,   99,  100,    6,   11,   99,
 /*   150 */   100,    9,   43,   44,   45,   46,    3,   20,   99,  100,
 /*   160 */    51,   52,   53,   54,   27,   56,   57,   58,   31,   60,
 /*   170 */    28,   29,   30,   31,    6,   79,   80,   81,   10,    6,
 /*   180 */    43,   44,   45,   46,   13,   79,   80,   12,   51,   52,
 /*   190 */    53,   54,   67,   56,   57,   58,   77,   60,   30,    5,
 /*   200 */     6,    7,    3,   78,   32,   11,   61,   88,   89,   90,
 /*   210 */     6,   59,   32,   94,   20,   96,    6,   98,   12,    6,
 /*   220 */    49,   27,   31,    3,   32,   31,   32,    5,    6,    7,
 /*   230 */     3,    2,    6,   11,   31,    3,    2,   43,   44,   45,
 /*   240 */    46,   32,   20,    3,    3,   51,   52,   53,   54,   27,
 /*   250 */    56,   57,   58,   31,   60,   32,   93,   49,   13,   78,
 /*   260 */    67,   49,    6,    6,   69,   43,   44,   45,   46,   91,
 /*   270 */    37,   49,   67,   51,   52,   53,   54,    2,   56,   57,
 /*   280 */    58,   91,   60,    8,    9,   11,   69,   73,   73,    4,
 /*   290 */    93,   16,   17,   18,   19,   76,   21,   22,   23,   24,
 /*   300 */    25,   26,   27,   28,   29,   30,   31,   32,    2,   49,
 /*   310 */    67,   49,   77,   67,    8,    9,   67,   43,   44,   45,
 /*   320 */    46,   90,   16,   17,   18,   19,   76,   21,   22,   23,
 /*   330 */    24,   25,   26,   27,   28,   29,   30,   31,   32,    5,
 /*   340 */     6,    7,   90,   90,   67,   11,   78,    8,    9,   67,
 /*   350 */    67,   67,   30,  101,   20,  101,  101,  101,  101,  101,
 /*   360 */   101,   27,  101,  101,  101,   31,   27,   28,   29,   30,
 /*   370 */    31,  101,  101,  101,  101,  101,  101,   43,   44,   45,
 /*   380 */    46,  101,  101,  101,  101,   51,   52,   53,   54,  101,
 /*   390 */    56,   57,   58,  101,   60,    8,    9,  101,    5,    6,
 /*   400 */     7,  101,  101,   16,   17,   18,   19,  101,   21,   22,
 /*   410 */    23,   24,   25,   26,   27,   28,   29,   30,   31,   32,
 /*   420 */     8,    9,  101,  101,   31,   32,  101,  101,   16,   17,
 /*   430 */    18,   19,  101,   21,   22,   23,   24,   25,   26,   27,
 /*   440 */    28,   29,   30,   31,   32,    2,   53,   54,   55,  101,
 /*   450 */   101,    8,    9,  101,  101,  101,  101,  101,  101,   16,
 /*   460 */    17,   18,   19,  101,   21,   22,   23,   24,   25,   26,
 /*   470 */    27,   28,   29,   30,   31,    2,  101,   88,   89,   90,
 /*   480 */   101,    8,    9,   94,  101,   96,  101,   98,  101,   16,
 /*   490 */    17,   18,   19,  101,   21,   22,   23,   24,   25,   26,
 /*   500 */    27,   28,   29,   30,   31,    8,    9,  101,  101,  101,
 /*   510 */    13,  101,  101,   16,   17,   18,   19,  101,   21,   22,
 /*   520 */    23,   24,   25,   26,   27,   28,   29,   30,   31,    8,
 /*   530 */     9,  101,  101,  101,  101,  101,  101,   16,   17,   18,
 /*   540 */    19,  101,   21,   22,   23,   24,   25,   26,   27,   28,
 /*   550 */    29,   30,   31,  101,   88,   89,   90,  101,    8,    9,
 /*   560 */    94,  101,   96,  101,   98,  101,   16,   17,   18,   19,
 /*   570 */    49,   21,   22,   23,   24,   25,   26,   27,   28,   29,
 /*   580 */    30,   31,   63,   64,   65,  101,  101,   68,  101,  101,
 /*   590 */   101,   72,  101,  101,   75,  101,  101,  101,  101,  101,
 /*   600 */   101,   82,  101,  101,   85,   86,   87,   88,   89,   90,
 /*   610 */     8,    9,  101,   94,  101,   96,  101,   98,  101,  101,
 /*   620 */    18,   19,  101,   21,   22,   23,   24,   25,   26,   27,
 /*   630 */    28,   29,   30,   31,   64,   65,  101,  101,   68,  101,
 /*   640 */   101,  101,   72,  101,  101,   75,  101,  101,  101,  101,
 /*   650 */   101,  101,   82,  101,  101,   85,   86,   87,   88,   89,
 /*   660 */    90,  101,  101,  101,   94,  101,   96,  101,   98,   64,
 /*   670 */    65,  101,  101,   68,  101,  101,  101,   72,  101,  101,
 /*   680 */    75,  101,  101,  101,  101,  101,  101,   82,  101,  101,
 /*   690 */    85,   86,   87,   88,   89,   90,    8,    9,  101,   94,
 /*   700 */   101,   96,  101,   98,  101,  101,  101,   19,  101,   21,
 /*   710 */    22,   23,   24,   25,   26,   27,   28,   29,   30,   31,
 /*   720 */     5,    6,    7,   88,   89,   90,   11,  101,  101,   94,
 /*   730 */   101,   96,  101,   98,   88,   89,   90,  101,  101,  101,
 /*   740 */    94,  101,   96,  101,   98,  101,   31,   88,   89,   90,
 /*   750 */   101,  101,  101,   94,  101,   96,  101,   98,   43,   44,
 /*   760 */    45,   46,  101,  101,  101,  101,  101,  101,   53,   54,
 /*   770 */   101,   56,   57,  101,  101,   60,  101,    8,    9,   88,
 /*   780 */    89,   90,  101,  101,  101,   94,  101,   96,  101,   98,
 /*   790 */    21,   22,   23,   24,   25,   26,   27,   28,   29,   30,
 /*   800 */    31,   88,   89,   90,   88,   89,   90,   94,  101,   96,
 /*   810 */    94,   98,   96,  101,   98,   88,   89,   90,   88,   89,
 /*   820 */    90,   94,  101,   96,   94,   98,   96,  101,   98,   88,
 /*   830 */    89,   90,  101,  101,  101,   94,  101,   96,  101,   98,
 /*   840 */   101,  101,   88,   89,   90,  101,  101,  101,   94,  101,
 /*   850 */    96,  101,   98,  101,  101,  101,  101,  101,   88,   89,
 /*   860 */    90,   88,   89,   90,   94,  101,   96,   94,   98,   96,
 /*   870 */   101,   98,  101,  101,   88,   89,   90,  101,  101,  101,
 /*   880 */    94,  101,   96,  101,   98,   88,   89,   90,   88,   89,
 /*   890 */    90,   94,  101,   96,   94,   98,   96,  101,   98,   88,
 /*   900 */    89,   90,  101,  101,  101,   94,  101,   96,  101,   98,
 /*   910 */    88,   89,   90,   88,   89,   90,   94,  101,   96,   94,
 /*   920 */    98,   96,  101,   98,   88,   89,   90,  101,  101,  101,
 /*   930 */    94,  101,   96,  101,   98,  101,  101,   88,   89,   90,
 /*   940 */   101,  101,  101,   94,  101,   96,  101,   98,   88,   89,
 /*   950 */    90,   88,   89,   90,   94,  101,   96,   94,   98,   96,
 /*   960 */   101,   98,  101,   88,   89,   90,  101,  101,  101,   94,
 /*   970 */   101,   96,  101,   98,
];
const YY_SHIFT_USE_DFLT: i32 = -24;
const YY_SHIFT_COUNT: i32 = 135;
const YY_SHIFT_MIN: i32 = -23;
const YY_SHIFT_MAX: i32 = 769;
const YY_SHIFT_OFST: [i16; 136] = [
 /*     0 */    -5,   52,   52,  222,  194,  334,  334,  334,  334,  137,
 /*    10 */   109,  334,  334,  334,  334,  334,  334,  334,  334,  334,
 /*    20 */   334,  334,  334,  334,  334,  334,  334,  334,  334,  334,
 /*    30 */   334,  334,  334,  334,  334,  715,  715,  393,   80,   80,
 /*    40 */   168,  171,   61,   61,  322,  322,  497,  497,  497,  521,
 /*    50 */   497,  274,  274,  274,   -3,   74,  245,  262,  245,  260,
 /*    60 */   285,  257,  245,  257,  256,  233,  233,  256,  212,  245,
 /*    70 */   208,  306,  275,  473,  443,  412,  387,  550,  550,  550,
 /*    80 */   550,  602,  602,  688,  688,  769,  769,  769,  769,  339,
 /*    90 */   339,  142,  142,  142,   12,   23,  -23,  -23,  -23,  223,
 /*   100 */   241,  240,  234,  232,  209,  203,  226,  229,  227,  220,
 /*   110 */   192,  191,  213,  206,  210,  180,  152,  204,  145,  172,
 /*   120 */   129,  199,  175,  173,  153,  141,  128,  123,   90,   70,
 /*   130 */    83,   62,   32,   30,   15,  -18,
];
const YY_REDUCE_USE_DFLT: i32 = -86;
const YY_REDUCE_COUNT: i32 = 70;
const YY_REDUCE_MIN: i32 = -85;
const YY_REDUCE_MAX: i32 = 875;
const YY_REDUCE_OFST: [i16; 71] = [
 /*     0 */   519,  605,  570,  119,   29,  -14,  -71,  -28,  -85,  800,
 /*    10 */   875,  863,  860,  849,  836,  825,  822,  811,  800,  797,
 /*    20 */   786,  773,  770,  754,  741,  730,  727,  716,  713,  691,
 /*    30 */   659,  646,  635,  466,  389,   34,  -34,   96,   58,  106,
 /*    40 */    59,  125,   50,   46,   18,  -63,  284,  283,  282,  268,
 /*    50 */   277,  253,  252,  231,  249,  250,  246,  197,  243,  235,
 /*    60 */   219,  215,  205,  214,  217,  190,  178,  195,  181,  193,
 /*    70 */   163,
];
const YY_DEFAULT: [YYACTIONTYPE; 211] = [
 /*     0 */   213,  213,  213,  323,  323,  311,  311,  323,  323,  323,
 /*    10 */   323,  323,  323,  323,  323,  323,  323,  323,  323,  323,
 /*    20 */   323,  323,  323,  323,  323,  323,  323,  323,  323,  323,
 /*    30 */   323,  323,  323,  323,  323,  323,  323,  323,  323,  323,
 /*    40 */   318,  323,  318,  318,  224,  224,  323,  323,  323,  323,
 /*    50 */   323,  323,  323,  323,  323,  323,  323,  323,  323,  323,
 /*    60 */   252,  245,  323,  245,  234,  237,  237,  234,  266,  323,
 /*    70 */   323,  323,  323,  312,  315,  323,  323,  230,  229,  228,
 /*    80 */   227,  292,  291,  282,  290,  298,  297,  296,  295,  294,
 /*    90 */   293,  284,  286,  285,  323,  299,  289,  288,  287,  323,
 /*   100 */   323,  323,  246,  323,  323,  323,  323,  235,  323,  323,
 /*   110 */   323,  323,  323,  323,  323,  323,  323,  323,  323,  323,
 /*   120 */   277,  323,  323,  323,  323,  323,  323,  323,  323,  323,
 /*   130 */   323,  323,  323,  323,  323,  323,  258,  256,  260,  263,
 /*   140 */   262,  255,  254,  250,  253,  251,  249,  248,  247,  244,
 /*   150 */   236,  238,  233,  232,  231,  314,  316,  310,  322,  321,
 /*   160 */   320,  319,  317,  313,  309,  308,  307,  306,  305,  304,
 /*   170 */   303,  302,  301,  281,  280,  276,  278,  275,  274,  273,
 /*   180 */   272,  271,  270,  269,  268,  267,  265,  264,  261,  300,
 /*   190 */   259,  283,  279,  257,  226,  243,  242,  241,  240,  239,
 /*   200 */   225,  223,  222,  221,  220,  219,  218,  217,  216,  215,
 /*   210 */   214,
];

/* TMPL: fallback tokens */

const YY_FALLBACK: [i32; 0] = [
];

/* TMPL: symbol names */


/* TMPL: rules */


/* TMPL: destructors */


/* TMPL: stack-overflow */


/* TMPL: stack-overflow */

const YY_RULE_INFO: [YYCODETYPE; 112] = [
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
 YYMinorType::YY129(yyres)
}
            ,
            1 /* program ::= stmts */
            => 
{
let yyres :  Ast ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY182(yy0),) => {

	if list::is_empty(&yy0) {
		panic!("null program");
	}
	// ignore yyres, it doesn't really go anywhere for program
	yyres = Ast::Nothing;
	// we're done, so put yy0 in extra
	self.extra = Ok(Ast::ReplRoot(yy0));

},    _ => unreachable!() };
 YYMinorType::YY129(yyres)
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
 YYMinorType::YY182(yyres)
}
            ,
            3 /* stmts ::= stmt stmts */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY182(yy0),YYMinorType::YY182(yy1),) => {

    verbose_out!("found new stmt: {:?}\n", yy0);
	yyres = list::cons(yy0, yy1);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            4 /* stmt ::= defstruct */
          | 5 /* stmt ::= let_stmt */
          | 7 /* stmt ::= fail_stmt */
          | 8 /* stmt ::= func_stmt */
          | 9 /* stmt ::= macro_stmt */
          | 10 /* stmt ::= if_stmt */
          | 56 /* pexpr ::= ptuple */
          | 69 /* expr ::= list */
          | 70 /* expr ::= tuple */
          | 88 /* expr ::= term */
          | 98 /* term ::= strexpr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY182(yy0),) => {
 yyres = yy0; 
},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            6 /* stmt ::= expr_stmt */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY182(yy0),) => {

    yyres = yy0;

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            11 /* defstruct ::= STRUCT typex defstruct_fields DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY153(yy1),YYMinorType::YY182(yy2),) => {

    yyres = sexpr::def_struct(Val::Type(yy1), yy2);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            12 /* defstruct_fields ::= defstruct_field defstruct_fields */
          | 109 /* strlist ::= strlist_term strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY182(yy0),YYMinorType::YY182(yy1),) => {

	yyres = list::cons(yy0, yy1);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            13 /* defstruct_fields ::= */
          | 23 /* dfunc_args ::= */
          | 100 /* list_items ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

	yyres = list::empty();

} };
 YYMinorType::YY182(yyres)
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
 (YYMinorType::YY21(yy1),YYMinorType::YY153(yy3),) => {

	yyres = sexpr::id_with_type(yy1.data, yy3);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            15 /* fail_stmt ::= FAIL LPAREN HASHTAG COMMA expr RPAREN */
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
 (YYMinorType::YY21(yy2),YYMinorType::YY182(yy4),) => {

vout!("found fail_stmt {:?}\n", yy2);
	yyres = sexpr::new(SexprType::Fail,
        list::cons(Val::hashtag(yy2.data),
        list::cons(yy4,
        Val::Nil,
        ))
    );

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            16 /* let_stmt ::= Let ID ASSIGN expr */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY21(yy1),YYMinorType::YY182(yy3),) => {

	let letx =
        list::cons(Val::id(yy1.data),
        list::cons(yy3,
        Val::Nil
        ));
	yyres = sexpr::new(SexprType::Let, letx);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            17 /* let_stmt ::= Fork ID ASSIGN expr */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY21(yy1),YYMinorType::YY182(yy3),) => {

	let bind = list::cons(Val::new_str(yy1.data), list::singleton(yy3));
	yyres = sexpr::new(SexprType::Fork, bind);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            18 /* expr_stmt ::= expr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY182(yy0),) => {

	yyres = yy0;

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            19 /* expr_stmt ::= DollarGT expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY182(yy1),) => {
 yyres = yy1; 
},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            20 /* block ::= BLOCKARROW stmts */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY182(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            21 /* func_stmt ::= Func ID LPAREN dfunc_args RPAREN opt_typex block DOUBLEDASH */
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
 (YYMinorType::YY21(yy1),YYMinorType::YY182(yy3),YYMinorType::YY153(yy5),YYMinorType::YY182(yy6),) => {

	let id = Val::id(yy1.data);
	let typ = Val::Type(yy5);
	yyres = sexpr::defunc(id, yy3, typ, yy6)

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            22 /* func_stmt ::= Func ID LPAREN dfunc_args RPAREN opt_typex match_case DOUBLEDASH */
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
 (YYMinorType::YY21(yy1),YYMinorType::YY182(yy3),YYMinorType::YY153(yy5),YYMinorType::YY182(yy6),) => {

	let id = Val::id(yy1.data);
	let typ = Val::Type(yy5);
    let body = sexpr::match_expr(Val::CallParams, yy6);
	yyres = sexpr::defunc(id, yy3, typ, body)

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            24 /* dfunc_args ::= ID opt_typex */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY21(yy0),YYMinorType::YY153(yy1),) => {

	yyres = list::singleton(sexpr::id_with_type(yy0.data, yy1));

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
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
 (YYMinorType::YY21(yy0),YYMinorType::YY153(yy1),YYMinorType::YY182(yy3),) => {

	yyres = list::cons(sexpr::id_with_type(yy0.data, yy1), yy3);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
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
 YYMinorType::YY153(yyres)
}
            ,
            27 /* opt_typex ::= COLON typex */
            => 
{
let yyres :  Type ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY153(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY153(yyres)
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
 YYMinorType::YY153(yyres)
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
 YYMinorType::YY153(yyres)
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
 YYMinorType::YY153(yyres)
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
 YYMinorType::YY153(yyres)
}
            ,
            32 /* typex ::= TYPE_ID */
            => 
{
let yyres :  Type ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY21(yy0),) => {

	yyres = Type::Id(Arc::new(yy0.data));

},    _ => unreachable!() };
 YYMinorType::YY153(yyres)
}
            ,
            33 /* macro_stmt ::= MACRO ID LPAREN macro_args RPAREN block DOUBLEDASH */
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
 (YYMinorType::YY21(yy1),YYMinorType::YY182(yy3),YYMinorType::YY182(yy5),) => {

    verbose_out!("found macro {:?}\n", yy1);
    yyres = sexpr::new(SexprType::DefMacro,
        list::cons(Val::id(yy1.data),
        list::cons(yy3,
        list::cons(yy5,
        Val::Nil
    ))));

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
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
 YYMinorType::YY182(yyres)
}
            ,
            35 /* macro_args ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY21(yy0),) => {

    yyres = list::singleton(Val::id(yy0.data));

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
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
 (YYMinorType::YY21(yy0),YYMinorType::YY182(yy2),) => {

    yyres = list::cons(Val::id(yy0.data), yy2);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            37 /* if_stmt ::= IF expr block DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY182(yy1),YYMinorType::YY182(yy2),) => {

    /* if-only style */
    yyres = sexpr::ifstmt(yy1, yy2, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            38 /* if_stmt ::= IF expr block else_if DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp4 = self.yystack.pop().unwrap();
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,yyp3.minor,) {
 (YYMinorType::YY182(yy1),YYMinorType::YY182(yy2),YYMinorType::YY182(yy3),) => {

    /* if-else style */
    yyres = sexpr::ifstmt(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            39 /* if_stmt ::= IF if_case DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY182(yy1),) => {

    /* case-expr style */
    yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
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
 (YYMinorType::YY182(yy2),YYMinorType::YY182(yy3),YYMinorType::YY182(yy4),) => {

    yyres = sexpr::ifstmt(yy2, yy3, yy4);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
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
 (YYMinorType::YY182(yy2),YYMinorType::YY182(yy3),) => {

    yyres = sexpr::ifstmt(yy2, yy3, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            42 /* else_if ::= ELSE block */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY182(yy1),) => {

    yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
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
 (YYMinorType::YY182(yy1),YYMinorType::YY182(yy2),YYMinorType::YY182(yy3),) => {

    yyres = sexpr::ifstmt(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
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
 (YYMinorType::YY182(yy2),) => {

    yyres = yy2;

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
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
 (YYMinorType::YY182(yy0),) => {

	verbose_out!("zero param function call!");
	yyres = sexpr::call(yy0, vec![]);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
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
 (YYMinorType::YY182(yy0),YYMinorType::YY182(yy2),) => {

	verbose_out!("one param function call!");
	yyres = sexpr::call(yy0, vec![yy2]);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
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
 (YYMinorType::YY182(yy0),YYMinorType::YY182(yy2),) => {

	verbose_out!("multi param function call!");
	yyres = sexpr::call(yy0, list::to_vec(yy2));

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
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
 (YYMinorType::YY182(yy0),YYMinorType::YY21(yy1),YYMinorType::YY182(yy2),) => {

	yyres = sexpr::binaryop(yy1.data, yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
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
 (YYMinorType::YY182(yy0),YYMinorType::YY182(yy2),) => {

	/* yyres = Val::binaryop(yy0, yy2, D); */
	yyres = Val::Void;

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            50 /* expr ::= CASE cases DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY182(yy1),) => {

    verbose_out!("parsed case expr\n");
	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
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
 (YYMinorType::YY182(yy1),YYMinorType::YY182(yy2),YYMinorType::YY182(yy5),) => {

    verbose_out!("found cases base\n");
    yyres = sexpr::casex(yy1, yy2, yy5);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
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
 (YYMinorType::YY182(yy1),YYMinorType::YY182(yy2),YYMinorType::YY182(yy3),) => {

    verbose_out!("found extra case\n");
    yyres = sexpr::casex(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            53 /* expr ::= MATCH expr match_case DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY182(yy1),YYMinorType::YY182(yy2),) => {

    verbose_out!("parsed match expr\n");
    yyres = sexpr::match_expr(yy1, yy2);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
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
 (YYMinorType::YY182(yy1),YYMinorType::YY182(yy2),YYMinorType::YY182(yy3),) => {

    verbose_out!("found cases base\n");
    yyres = sexpr::match_case(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
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
 (YYMinorType::YY182(yy1),YYMinorType::YY182(yy2),) => {

    verbose_out!("parsed base match case\n");
    yyres = sexpr::match_case(yy1, yy2, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            57 /* pexpr ::= INT */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY112(yy0),) => {
 yyres = Val::Int(yy0); 
},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            58 /* pexpr ::= True */
          | 95 /* term ::= True */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Bool(true); 
} };
 YYMinorType::YY182(yyres)
}
            ,
            59 /* pexpr ::= False */
          | 96 /* term ::= False */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Bool(false); 
} };
 YYMinorType::YY182(yyres)
}
            ,
            60 /* pexpr ::= HASHTAG */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY21(yy0),) => {
 yyres = Val::Hashtag(Arc::new(yy0.data)); 
},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            61 /* pexpr ::= ID */
          | 91 /* term ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY21(yy0),) => {
 yyres = Val::id(yy0.data); 
},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
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
 YYMinorType::YY182(yyres)
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
 YYMinorType::YY182(yyres)
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
 (YYMinorType::YY182(yy1),) => {

	yyres = Val::Tuple(vec![yy1]);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            65 /* ptuple ::= LPAREN pargs RPAREN */
          | 103 /* tuple ::= LPAREN tuple_args RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY182(yy1),) => {

	yyres = Val::tuple_from_list(yy1);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
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
 (YYMinorType::YY182(yy0),YYMinorType::YY182(yy2),) => {

	yyres = list::cons(yy0,
        list::cons(yy2,
        Val::Nil
        ));

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            67 /* pargs ::= pexpr COMMA pargs */
          | 102 /* list_items ::= expr COMMA list_items */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY182(yy0),YYMinorType::YY182(yy2),) => {

	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            68 /* expr ::= expr DOT ID */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY182(yy0),YYMinorType::YY21(yy2),) => {

    yyres = sexpr::new(SexprType::FieldAccess,
        list::cons(yy0,
        list::cons(Val::id(yy2.data),
        Val::Nil,
        ))
    );

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            71 /* expr ::= NOT expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY182(yy1),) => {

	yyres = sexpr::call(Val::id("bool_not".to_string()), vec![yy1]);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            72 /* expr ::= expr ConcatNewline */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY182(yy0),) => {

	let newline = Val::Str(Arc::new("\n".to_string()));
	let args = list::cons(yy0, list::singleton(newline));
	yyres = sexpr::new(SexprType::StrExpr, args)

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            73 /* expr ::= MINUS expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY182(yy1),) => {

	yyres = sexpr::call(Val::id("negate".to_string()), vec![yy1]);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            74 /* expr ::= expr PLUS expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY182(yy0),YYMinorType::YY182(yy2),) => {

	yyres = sexpr::binaryop("int_add".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            75 /* expr ::= expr MINUS expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY182(yy0),YYMinorType::YY182(yy2),) => {

	yyres = sexpr::binaryop("int_sub".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            76 /* expr ::= expr TIMES expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY182(yy0),YYMinorType::YY182(yy2),) => {

	yyres = sexpr::binaryop("int_mult".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            77 /* expr ::= expr SLASH expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY182(yy0),YYMinorType::YY182(yy2),) => {

	yyres = sexpr::binaryop("int_div".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            78 /* expr ::= expr MOD expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY182(yy0),YYMinorType::YY182(yy2),) => {

	yyres = sexpr::binaryop("int_mod".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            79 /* expr ::= expr AND expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY182(yy0),YYMinorType::YY182(yy2),) => {

	yyres = sexpr::binaryop("and".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            80 /* expr ::= expr OR expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY182(yy0),YYMinorType::YY182(yy2),) => {

	yyres = sexpr::binaryop("or".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            81 /* expr ::= expr XOR expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY182(yy0),YYMinorType::YY182(yy2),) => {

	yyres = sexpr::binaryop("xor".to_string(),yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            82 /* expr ::= expr LT expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY182(yy0),YYMinorType::YY182(yy2),) => {

	yyres = sexpr::binaryop("less_than".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            83 /* expr ::= expr LTEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY182(yy0),YYMinorType::YY182(yy2),) => {

	yyres = sexpr::binaryop("less_than_equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            84 /* expr ::= expr GT expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY182(yy0),YYMinorType::YY182(yy2),) => {

	yyres = sexpr::binaryop("greater_than".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            85 /* expr ::= expr GTEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY182(yy0),YYMinorType::YY182(yy2),) => {

	yyres = sexpr::binaryop("greater_than_equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            86 /* expr ::= expr EQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY182(yy0),YYMinorType::YY182(yy2),) => {

	yyres = sexpr::binaryop("equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            87 /* expr ::= expr NEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY182(yy0),YYMinorType::YY182(yy2),) => {

	let eq = sexpr::binaryop("equal".to_string(), yy0, yy2);
	yyres = sexpr::call(Val::id("bool_not".to_string()), vec![eq]);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            89 /* term ::= LPAREN expr RPAREN */
          | 99 /* list ::= SquareL list_items SquareR */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY182(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            90 /* term ::= typex */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY153(yy0),) => {

    yyres = Val::Type(yy0);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
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
 YYMinorType::YY182(yyres)
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
 YYMinorType::YY182(yyres)
}
            ,
            94 /* term ::= INT */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY112(yy0),) => {

	yyres = Val::Int(yy0);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            97 /* term ::= HASHTAG */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY21(yy0),) => {

	yyres = Val::Hashtag(Arc::new(yy0.data));

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            101 /* list_items ::= expr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY182(yy0),) => {

	yyres = list::singleton(yy0);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
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
 (YYMinorType::YY182(yy0),YYMinorType::YY182(yy2),) => {

	vout!("base tuple args!");
	yyres = list::cons(yy0, list::singleton(yy2));

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
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
 (YYMinorType::YY182(yy0),YYMinorType::YY182(yy2),) => {

	vout!("additional tuple arg!");
	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
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
 (YYMinorType::YY182(yy1),) => {

	yyres = sexpr::strexpr(yy1);
    vout!("strexpr({:?})\n", yyres);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
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
 YYMinorType::YY182(yyres)
}
            ,
            108 /* strlist ::= StrLit strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY78(yy0),YYMinorType::YY182(yy1),) => {

	yyres = list::cons(Val::new_str(yy0), yy1);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            110 /* strlist_term ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY21(yy0),) => {

    yyres = Val::id(yy0.data);

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
}
            ,
            111 /* strlist_term ::= strlist_term DOT ID */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY182(yy0),YYMinorType::YY21(yy2),) => {

    yyres = sexpr::new(SexprType::FieldAccess,
        list::cons(yy0,
        list::cons(Val::id(yy2.data),
        Val::Nil,
    )))

},    _ => unreachable!() };
 YYMinorType::YY182(yyres)
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

