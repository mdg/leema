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
const YYNOCODE: i32 = 97;
type YYACTIONTYPE = u16;
const YYWILDCARD: YYCODETYPE = 1;
enum YYMinorType {
    YY0,
    YY61(Ast),
    YY68(i64),
    YY75(Type),
    YY76(String),
    YY104(Val),
    YY117(TokenData<String>),
    YY184(TokenLoc),
}
const YYNSTATE: i32 = 212;
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
    RETURN, //14
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
    DOLLAR, //29
    DOT, //30
    LPAREN, //31
    RPAREN, //32
    FAILED, //33
    STRUCT, //34
    COLON, //35
    FAIL, //36
    Let, //37
    Fork, //38
    Func, //39
    TYPE_INT, //40
    TYPE_STR, //41
    TYPE_BOOL, //42
    TYPE_VOID, //43
    MACRO, //44
    IF, //45
    PIPE, //46
    CASE, //47
    MATCH, //48
    True, //49
    False, //50
    UNDERSCORE, //51
    VOID, //52
    DollarQuestion, //53
    SquareL, //54
    SquareR, //55
    StrOpen, //56
    StrClose, //57
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
pub const TOKEN_RETURN: i32 = 14;
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
pub const TOKEN_DOLLAR: i32 = 29;
pub const TOKEN_DOT: i32 = 30;
pub const TOKEN_LPAREN: i32 = 31;
pub const TOKEN_RPAREN: i32 = 32;
pub const TOKEN_FAILED: i32 = 33;
pub const TOKEN_STRUCT: i32 = 34;
pub const TOKEN_COLON: i32 = 35;
pub const TOKEN_FAIL: i32 = 36;
pub const TOKEN_Let: i32 = 37;
pub const TOKEN_Fork: i32 = 38;
pub const TOKEN_Func: i32 = 39;
pub const TOKEN_TYPE_INT: i32 = 40;
pub const TOKEN_TYPE_STR: i32 = 41;
pub const TOKEN_TYPE_BOOL: i32 = 42;
pub const TOKEN_TYPE_VOID: i32 = 43;
pub const TOKEN_MACRO: i32 = 44;
pub const TOKEN_IF: i32 = 45;
pub const TOKEN_PIPE: i32 = 46;
pub const TOKEN_CASE: i32 = 47;
pub const TOKEN_MATCH: i32 = 48;
pub const TOKEN_True: i32 = 49;
pub const TOKEN_False: i32 = 50;
pub const TOKEN_UNDERSCORE: i32 = 51;
pub const TOKEN_VOID: i32 = 52;
pub const TOKEN_DollarQuestion: i32 = 53;
pub const TOKEN_SquareL: i32 = 54;
pub const TOKEN_SquareR: i32 = 55;
pub const TOKEN_StrOpen: i32 = 56;
pub const TOKEN_StrClose: i32 = 57;
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
        Token::COMMA(x) => YYMinorType::YY184(x),
        Token::DOUBLEDASH(x) => YYMinorType::YY184(x),
        Token::ELSE(x) => YYMinorType::YY184(x),
        Token::HASHTAG(x) => YYMinorType::YY117(x),
        Token::ID(x) => YYMinorType::YY117(x),
        Token::INT(x) => YYMinorType::YY68(x),
        Token::PLUS(x) => YYMinorType::YY184(x),
        Token::SLASH(x) => YYMinorType::YY184(x),
        Token::StrLit(x) => YYMinorType::YY76(x),
        Token::TYPE_ID(x) => YYMinorType::YY117(x),
        _ => YYMinorType::YY0
  }
}
const YY_ACTTAB_COUNT: i32 = 954;
const YY_ACTION: [YYACTIONTYPE; 954] = [
 /*     0 */   165,  171,  168,  159,  134,    4,  199,   42,  212,   34,
 /*    10 */    96,   72,  172,  197,   15,  195,  174,  172,  173,  118,
 /*    20 */   164,   14,  130,  190,  181,  164,    8,  120,  132,   53,
 /*    30 */    38,  125,  117,  115,  113,  203,  202,  201,  200,  107,
 /*    40 */     3,  193,   71,   16,  167,  166,  199,  170,  169,    6,
 /*    50 */    30,   43,   58,  165,  171,  168,   95,  190,  129,  199,
 /*    60 */   182,   70,   33,   30,   36,  128,   44,   15,   31,   29,
 /*    70 */     1,  134,    4,  192,   14,  203,  202,  201,  200,    8,
 /*    80 */    32,   31,   29,  110,  134,    4,   38,   35,  203,  202,
 /*    90 */   201,  200,  147,   54,  109,   71,   16,  167,  166,    1,
 /*   100 */   170,  169,    6,   39,   43,   56,  165,  171,  168,  178,
 /*   110 */    44,  180,  199,   96,   75,  172,  138,  179,  172,  174,
 /*   120 */    15,  173,  156,  164,   69,  190,  164,   14,  121,   40,
 /*   130 */   159,   11,    8,  126,   42,   52,  124,  123,  186,  185,
 /*   140 */   189,  203,  202,  201,  200,  161,   40,   17,   71,   16,
 /*   150 */   167,  166,  175,  170,  169,    6,  162,   43,  158,  165,
 /*   160 */   171,  168,  160,   40,   37,  199,  157,  155,   96,   74,
 /*   170 */   172,  116,   13,   15,  174,  119,  173,  114,  164,   12,
 /*   180 */    14,  112,  188,  187,  184,    8,  137,   67,  153,   66,
 /*   190 */   152,   64,  106,   63,  203,  202,  201,  200,   62,  149,
 /*   200 */    61,   71,   16,  167,  166,  146,  170,  169,    6,  143,
 /*   210 */    43,  136,  165,  171,  168,   18,  133,  131,  199,   39,
 /*   220 */    68,    1,   96,   74,  172,  111,   15,   65,  174,  163,
 /*   230 */   173,  105,  164,   14,   96,   73,  172,  191,    8,   41,
 /*   240 */   174,  108,  173,  100,  164,   51,  150,  203,  202,  201,
 /*   250 */   200,  103,  104,   10,   71,   16,  167,  166,    7,  170,
 /*   260 */   169,    6,  148,   43,   33,   30,  145,   54,   10,  142,
 /*   270 */     9,   27,   26,   28,  196,  141,   21,   20,   23,   22,
 /*   280 */    25,   24,   32,   31,   29,    7,  134,    4,  198,  139,
 /*   290 */   140,   33,   30,  144,  186,  185,  189,  102,   27,   26,
 /*   300 */    28,  196,   45,   21,   20,   23,   22,   25,   24,   32,
 /*   310 */    31,   29,  177,  134,    4,  194,  165,  171,  168,  151,
 /*   320 */    37,  183,  199,   57,  122,  127,   96,   81,  172,   55,
 /*   330 */    15,   60,  174,   59,  173,  326,  164,   14,  188,  187,
 /*   340 */   184,  326,    8,  326,  326,  326,  326,  326,  326,  326,
 /*   350 */   326,  203,  202,  201,  200,  326,  326,  326,   71,   16,
 /*   360 */   167,  166,  326,  170,  169,    6,  326,   43,   33,   30,
 /*   370 */   326,  326,  326,  326,  326,   27,   26,   28,  196,  326,
 /*   380 */    21,   20,   23,   22,   25,   24,   32,   31,   29,  326,
 /*   390 */   134,    4,  194,   33,   30,  326,  326,  326,  326,  326,
 /*   400 */    27,   26,   28,  196,  326,   21,   20,   23,   22,   25,
 /*   410 */    24,   32,   31,   29,    7,  134,    4,  176,  326,  326,
 /*   420 */    33,   30,  326,  326,  326,  326,  326,   27,   26,   28,
 /*   430 */   196,  326,   21,   20,   23,   22,   25,   24,   32,   31,
 /*   440 */    29,    5,  134,    4,  326,  326,  326,   33,   30,  326,
 /*   450 */   326,  326,  326,  326,   27,   26,   28,  196,  326,   21,
 /*   460 */    20,   23,   22,   25,   24,   32,   31,   29,  326,  134,
 /*   470 */     4,   33,   30,  326,  326,  326,    1,  326,   27,   26,
 /*   480 */    28,  196,  326,   21,   20,   23,   22,   25,   24,   32,
 /*   490 */    31,   29,  326,  134,    4,   33,   30,  326,  326,  326,
 /*   500 */   326,  326,   27,   26,   28,  196,  326,   21,   20,   23,
 /*   510 */    22,   25,   24,   32,   31,   29,  326,  134,    4,  326,
 /*   520 */   326,  326,   33,   30,  326,  326,  326,  326,  326,   27,
 /*   530 */    26,   28,  196,   39,   21,   20,   23,   22,   25,   24,
 /*   540 */    32,   31,   29,  326,  134,    4,  326,  325,  135,    2,
 /*   550 */   326,  206,  326,  326,  326,  205,  326,  326,  204,  326,
 /*   560 */   326,  326,  326,  326,  326,  210,  326,  326,  209,  208,
 /*   570 */   207,   96,   78,  172,   33,   30,  326,  174,  326,  173,
 /*   580 */   326,  164,  326,   28,  196,  326,   21,   20,   23,   22,
 /*   590 */    25,   24,   32,   31,   29,  326,  134,    4,  211,    2,
 /*   600 */   326,  206,  326,  326,  326,  205,  326,  326,  204,  326,
 /*   610 */   326,  326,  326,  326,  326,  210,  326,  326,  209,  208,
 /*   620 */   207,   96,   78,  172,  326,  326,  326,  174,  326,  173,
 /*   630 */   326,  164,  154,    2,  326,  206,  326,  326,  326,  205,
 /*   640 */   326,  326,  204,  326,  326,  326,  326,  326,  326,  210,
 /*   650 */   326,  326,  209,  208,  207,   96,   78,  172,   33,   30,
 /*   660 */   326,  174,  326,  173,  326,  164,  326,  326,  196,  326,
 /*   670 */    21,   20,   23,   22,   25,   24,   32,   31,   29,  326,
 /*   680 */   134,    4,  165,  171,  168,   96,   94,  172,  199,  326,
 /*   690 */   326,  174,  101,  173,  326,  164,  326,  326,  326,  326,
 /*   700 */   326,  326,  326,   96,   48,  172,  326,  326,   19,  174,
 /*   710 */   326,  173,  326,  164,  326,  326,  326,  203,  202,  201,
 /*   720 */   200,  326,  326,  326,  326,  326,  167,  166,  326,  170,
 /*   730 */   169,  326,  326,   43,  326,   33,   30,  326,  326,  326,
 /*   740 */   326,  326,  326,  326,  326,  326,  326,  324,  324,  324,
 /*   750 */   324,   25,   24,   32,   31,   29,  326,  134,    4,   96,
 /*   760 */    93,  172,   96,   99,  172,  174,  326,  173,  174,  164,
 /*   770 */   173,  326,  164,  326,  326,   96,   98,  172,  326,  326,
 /*   780 */   326,  174,  326,  173,  326,  164,  326,   96,   97,  172,
 /*   790 */   326,  326,  326,  174,  326,  173,  326,  164,  326,  326,
 /*   800 */   326,   96,   85,  172,   96,   83,  172,  174,  326,  173,
 /*   810 */   174,  164,  173,  326,  164,  326,  326,   96,   82,  172,
 /*   820 */    96,   91,  172,  174,  326,  173,  174,  164,  173,  326,
 /*   830 */   164,   96,   90,  172,   96,   89,  172,  174,  326,  173,
 /*   840 */   174,  164,  173,  326,  164,   96,   88,  172,   96,   87,
 /*   850 */   172,  174,  326,  173,  174,  164,  173,  326,  164,   96,
 /*   860 */    86,  172,  326,  326,  326,  174,  326,  173,  326,  164,
 /*   870 */   326,   96,   77,  172,  326,  326,  326,  174,  326,  173,
 /*   880 */   326,  164,  326,  326,  326,   96,   50,  172,   96,   76,
 /*   890 */   172,  174,  326,  173,  174,  164,  173,  326,  164,  326,
 /*   900 */   326,   96,   49,  172,   96,   84,  172,  174,  326,  173,
 /*   910 */   174,  164,  173,  326,  164,   96,   92,  172,   96,   80,
 /*   920 */   172,  174,  326,  173,  174,  164,  173,  326,  164,   96,
 /*   930 */    79,  172,   96,   47,  172,  174,  326,  173,  174,  164,
 /*   940 */   173,  326,  164,   96,   46,  172,  326,  326,  326,  174,
 /*   950 */   326,  173,  326,  164,
];
const YY_LOOKAHEAD: [YYCODETYPE; 954] = [
 /*     0 */     5,    6,    7,    6,   30,   31,   11,   10,    0,   14,
 /*    10 */    83,   84,   85,    6,   19,   83,   89,   85,   91,   92,
 /*    20 */    93,   26,   74,   75,   76,   93,   31,   30,   33,   34,
 /*    30 */     2,   36,   37,   38,   39,   40,   41,   42,   43,   44,
 /*    40 */    45,    3,   47,   48,   49,   50,   11,   52,   53,   54,
 /*    50 */     9,   56,    4,    5,    6,    7,   74,   75,   76,   11,
 /*    60 */    32,    6,    8,    9,    6,   78,   79,   19,   27,   28,
 /*    70 */    13,   30,   31,    3,   26,   40,   41,   42,   43,   31,
 /*    80 */    26,   27,   28,   62,   30,   31,    2,   29,   40,   41,
 /*    90 */    42,   43,    3,    4,   73,   47,   48,   49,   50,   13,
 /*   100 */    52,   53,   54,   46,   56,    4,    5,    6,    7,   78,
 /*   110 */    79,   32,   11,   83,   84,   85,   83,    3,   85,   89,
 /*   120 */    19,   91,   92,   93,   74,   75,   93,   26,   94,   95,
 /*   130 */     6,   45,   31,    6,   10,   35,   31,    5,    5,    6,
 /*   140 */     7,   40,   41,   42,   43,   94,   95,    2,   47,   48,
 /*   150 */    49,   50,    3,   52,   53,   54,   57,   56,    6,    5,
 /*   160 */     6,    7,   94,   95,   31,   11,   55,   32,   83,   84,
 /*   170 */    85,    6,   12,   19,   89,   90,   91,    6,   93,   12,
 /*   180 */    26,    6,   49,   50,   51,   31,   32,   31,    3,   32,
 /*   190 */     3,    2,    6,   31,   40,   41,   42,   43,   32,    3,
 /*   200 */     2,   47,   48,   49,   50,    3,   52,   53,   54,    3,
 /*   210 */    56,   32,    5,    6,    7,   46,   88,   73,   11,   46,
 /*   220 */    62,   13,   83,   84,   85,   64,   19,    6,   89,   90,
 /*   230 */    91,   68,   93,   26,   83,   84,   85,   73,   31,   86,
 /*   240 */    89,   86,   91,   92,   93,   35,   64,   40,   41,   42,
 /*   250 */    43,    6,   62,   46,   47,   48,   49,   50,    2,   52,
 /*   260 */    53,   54,   68,   56,    8,    9,   71,    4,   46,   72,
 /*   270 */    46,   15,   16,   17,   18,   62,   20,   21,   22,   23,
 /*   280 */    24,   25,   26,   27,   28,    2,   30,   31,   32,   88,
 /*   290 */    62,    8,    9,   62,    5,    6,    7,   71,   15,   16,
 /*   300 */    17,   18,   85,   20,   21,   22,   23,   24,   25,   26,
 /*   310 */    27,   28,   85,   30,   31,   32,    5,    6,    7,   85,
 /*   320 */    31,   32,   11,   62,   73,   30,   83,   84,   85,   62,
 /*   330 */    19,   62,   89,   62,   91,   96,   93,   26,   49,   50,
 /*   340 */    51,   96,   31,   96,   96,   96,   96,   96,   96,   96,
 /*   350 */    96,   40,   41,   42,   43,   96,   96,   96,   47,   48,
 /*   360 */    49,   50,   96,   52,   53,   54,   96,   56,    8,    9,
 /*   370 */    96,   96,   96,   96,   96,   15,   16,   17,   18,   96,
 /*   380 */    20,   21,   22,   23,   24,   25,   26,   27,   28,   96,
 /*   390 */    30,   31,   32,    8,    9,   96,   96,   96,   96,   96,
 /*   400 */    15,   16,   17,   18,   96,   20,   21,   22,   23,   24,
 /*   410 */    25,   26,   27,   28,    2,   30,   31,   32,   96,   96,
 /*   420 */     8,    9,   96,   96,   96,   96,   96,   15,   16,   17,
 /*   430 */    18,   96,   20,   21,   22,   23,   24,   25,   26,   27,
 /*   440 */    28,    2,   30,   31,   96,   96,   96,    8,    9,   96,
 /*   450 */    96,   96,   96,   96,   15,   16,   17,   18,   96,   20,
 /*   460 */    21,   22,   23,   24,   25,   26,   27,   28,   96,   30,
 /*   470 */    31,    8,    9,   96,   96,   96,   13,   96,   15,   16,
 /*   480 */    17,   18,   96,   20,   21,   22,   23,   24,   25,   26,
 /*   490 */    27,   28,   96,   30,   31,    8,    9,   96,   96,   96,
 /*   500 */    96,   96,   15,   16,   17,   18,   96,   20,   21,   22,
 /*   510 */    23,   24,   25,   26,   27,   28,   96,   30,   31,   96,
 /*   520 */    96,   96,    8,    9,   96,   96,   96,   96,   96,   15,
 /*   530 */    16,   17,   18,   46,   20,   21,   22,   23,   24,   25,
 /*   540 */    26,   27,   28,   96,   30,   31,   96,   59,   60,   61,
 /*   550 */    96,   63,   96,   96,   96,   67,   96,   96,   70,   96,
 /*   560 */    96,   96,   96,   96,   96,   77,   96,   96,   80,   81,
 /*   570 */    82,   83,   84,   85,    8,    9,   96,   89,   96,   91,
 /*   580 */    96,   93,   96,   17,   18,   96,   20,   21,   22,   23,
 /*   590 */    24,   25,   26,   27,   28,   96,   30,   31,   60,   61,
 /*   600 */    96,   63,   96,   96,   96,   67,   96,   96,   70,   96,
 /*   610 */    96,   96,   96,   96,   96,   77,   96,   96,   80,   81,
 /*   620 */    82,   83,   84,   85,   96,   96,   96,   89,   96,   91,
 /*   630 */    96,   93,   60,   61,   96,   63,   96,   96,   96,   67,
 /*   640 */    96,   96,   70,   96,   96,   96,   96,   96,   96,   77,
 /*   650 */    96,   96,   80,   81,   82,   83,   84,   85,    8,    9,
 /*   660 */    96,   89,   96,   91,   96,   93,   96,   96,   18,   96,
 /*   670 */    20,   21,   22,   23,   24,   25,   26,   27,   28,   96,
 /*   680 */    30,   31,    5,    6,    7,   83,   84,   85,   11,   96,
 /*   690 */    96,   89,   72,   91,   96,   93,   96,   96,   96,   96,
 /*   700 */    96,   96,   96,   83,   84,   85,   96,   96,   31,   89,
 /*   710 */    96,   91,   96,   93,   96,   96,   96,   40,   41,   42,
 /*   720 */    43,   96,   96,   96,   96,   96,   49,   50,   96,   52,
 /*   730 */    53,   96,   96,   56,   96,    8,    9,   96,   96,   96,
 /*   740 */    96,   96,   96,   96,   96,   96,   96,   20,   21,   22,
 /*   750 */    23,   24,   25,   26,   27,   28,   96,   30,   31,   83,
 /*   760 */    84,   85,   83,   84,   85,   89,   96,   91,   89,   93,
 /*   770 */    91,   96,   93,   96,   96,   83,   84,   85,   96,   96,
 /*   780 */    96,   89,   96,   91,   96,   93,   96,   83,   84,   85,
 /*   790 */    96,   96,   96,   89,   96,   91,   96,   93,   96,   96,
 /*   800 */    96,   83,   84,   85,   83,   84,   85,   89,   96,   91,
 /*   810 */    89,   93,   91,   96,   93,   96,   96,   83,   84,   85,
 /*   820 */    83,   84,   85,   89,   96,   91,   89,   93,   91,   96,
 /*   830 */    93,   83,   84,   85,   83,   84,   85,   89,   96,   91,
 /*   840 */    89,   93,   91,   96,   93,   83,   84,   85,   83,   84,
 /*   850 */    85,   89,   96,   91,   89,   93,   91,   96,   93,   83,
 /*   860 */    84,   85,   96,   96,   96,   89,   96,   91,   96,   93,
 /*   870 */    96,   83,   84,   85,   96,   96,   96,   89,   96,   91,
 /*   880 */    96,   93,   96,   96,   96,   83,   84,   85,   83,   84,
 /*   890 */    85,   89,   96,   91,   89,   93,   91,   96,   93,   96,
 /*   900 */    96,   83,   84,   85,   83,   84,   85,   89,   96,   91,
 /*   910 */    89,   93,   91,   96,   93,   83,   84,   85,   83,   84,
 /*   920 */    85,   89,   96,   91,   89,   93,   91,   96,   93,   83,
 /*   930 */    84,   85,   83,   84,   85,   89,   96,   91,   89,   93,
 /*   940 */    91,   96,   93,   83,   84,   85,   96,   96,   96,   89,
 /*   950 */    96,   91,   96,   93,
];
const YY_SHIFT_USE_DFLT: i32 = -27;
const YY_SHIFT_COUNT: i32 = 135;
const YY_SHIFT_MIN: i32 = -26;
const YY_SHIFT_MAX: i32 = 727;
const YY_SHIFT_OFST: [i16; 136] = [
 /*     0 */    -5,   -5,   -5,  207,  154,  311,  311,  311,  311,  101,
 /*    10 */    48,  311,  311,  311,  311,  311,  311,  311,  311,  311,
 /*    20 */   311,  311,  311,  311,  311,  311,  311,  311,  311,  311,
 /*    30 */   311,  311,  311,  311,  311,  677,  677,  289,  133,  133,
 /*    40 */    -3,   57,  124,  124,  295,  295,  463,  463,  463,  487,
 /*    50 */   463,   35,   35,   35,   86,   89,  208,  224,  208,  222,
 /*    60 */   263,  245,  208,  245,  221,  210,  210,  221,  173,  208,
 /*    70 */   173,  169,  283,  256,  439,  412,  385,  360,  514,  514,
 /*    80 */   514,  514,  566,  566,  650,  650,  727,  727,  727,  727,
 /*    90 */    54,   54,   41,   41,   41,   28,   58,  -26,  -26,  -26,
 /*   100 */   179,  206,  202,  198,  196,  166,  162,  186,  189,  187,
 /*   110 */   185,  157,  156,  175,  167,  171,  160,  165,  135,  111,
 /*   120 */   152,   99,  149,  145,  132,  105,  100,  127,  114,   79,
 /*   130 */    84,   70,   55,   38,    7,    8,
];
const YY_REDUCE_USE_DFLT: i32 = -74;
const YY_REDUCE_COUNT: i32 = 71;
const YY_REDUCE_MIN: i32 = -73;
const YY_REDUCE_MAX: i32 = 860;
const YY_REDUCE_OFST: [i16; 72] = [
 /*     0 */   488,  572,  538,  620,  151,  139,   85,   30,  -73,  802,
 /*    10 */   860,  849,  846,  835,  832,  821,  818,  805,  802,  788,
 /*    20 */   776,  765,  762,  751,  748,  737,  734,  721,  718,  704,
 /*    30 */   692,  679,  676,  602,  243,   33,  -68,  -18,  -52,   50,
 /*    40 */    68,   21,   51,   34,   31,  -13,  271,  269,  267,  251,
 /*    50 */   261,  234,  227,  217,  231,  226,  228,  201,  213,  197,
 /*    60 */   195,  194,  190,  163,  182,  155,  153,  161,  164,  158,
 /*    70 */   144,  128,
];
const YY_DEFAULT: [YYACTIONTYPE; 212] = [
 /*     0 */   213,  213,  213,  324,  324,  312,  312,  324,  324,  324,
 /*    10 */   324,  324,  324,  324,  324,  324,  324,  324,  324,  324,
 /*    20 */   324,  324,  324,  324,  324,  324,  324,  324,  324,  324,
 /*    30 */   324,  324,  324,  324,  324,  324,  324,  324,  324,  324,
 /*    40 */   319,  324,  319,  319,  226,  226,  324,  324,  324,  324,
 /*    50 */   324,  324,  324,  324,  324,  324,  324,  324,  324,  324,
 /*    60 */   253,  246,  324,  246,  235,  238,  238,  235,  267,  324,
 /*    70 */   324,  324,  324,  324,  313,  316,  324,  324,  231,  230,
 /*    80 */   229,  222,  293,  292,  283,  291,  299,  298,  297,  296,
 /*    90 */   295,  294,  285,  287,  286,  324,  300,  290,  289,  288,
 /*   100 */   324,  324,  324,  247,  324,  324,  324,  324,  236,  324,
 /*   110 */   324,  324,  324,  324,  324,  324,  324,  324,  324,  324,
 /*   120 */   324,  324,  324,  324,  324,  324,  324,  324,  324,  324,
 /*   130 */   278,  324,  324,  324,  324,  324,  259,  257,  261,  264,
 /*   140 */   263,  256,  255,  251,  254,  252,  250,  249,  248,  245,
 /*   150 */   237,  239,  234,  233,  232,  315,  317,  311,  323,  322,
 /*   160 */   321,  320,  318,  314,  310,  309,  308,  307,  306,  305,
 /*   170 */   304,  303,  302,  282,  281,  265,  228,  227,  225,  224,
 /*   180 */   277,  279,  276,  275,  274,  273,  272,  271,  270,  269,
 /*   190 */   268,  266,  223,  262,  301,  260,  284,  280,  258,  244,
 /*   200 */   243,  242,  241,  240,  221,  220,  219,  218,  217,  216,
 /*   210 */   215,  214,
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
  59,
  60,
  60,
  61,
  61,
  61,
  61,
  61,
  61,
  61,
  61,
  61,
  77,
  78,
  78,
  79,
  82,
  80,
  80,
  81,
  62,
  63,
  63,
  64,
  64,
  64,
  86,
  86,
  85,
  85,
  85,
  85,
  85,
  67,
  68,
  68,
  68,
  70,
  70,
  70,
  71,
  71,
  71,
  72,
  72,
  84,
  84,
  84,
  84,
  84,
  84,
  88,
  88,
  84,
  73,
  73,
  74,
  74,
  74,
  74,
  74,
  74,
  74,
  75,
  75,
  75,
  76,
  76,
  84,
  84,
  84,
  84,
  84,
  84,
  84,
  84,
  84,
  84,
  84,
  84,
  84,
  84,
  84,
  84,
  84,
  84,
  84,
  84,
  84,
  83,
  83,
  83,
  83,
  83,
  83,
  83,
  83,
  83,
  83,
  89,
  90,
  90,
  90,
  91,
  92,
  92,
  93,
  94,
  94,
  94,
  95,
  95,
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
 (YYMinorType::YY104(yy0),) => {

	if list::is_empty(&yy0) {
		panic!("null program");
	}
	// ignore yyres, it doesn't really go anywhere for program
	yyres = Ast::Nothing;
	// we're done, so put yy0 in extra
	self.extra = Ok(Ast::ReplRoot(yy0));

},    _ => unreachable!() };
 YYMinorType::YY61(yyres)
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
 YYMinorType::YY104(yyres)
}
            ,
            2 /* stmts ::= stmt stmts */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY104(yy0),YYMinorType::YY104(yy1),) => {

    verbose_out!("found new stmt: {:?}\n", yy0);
	yyres = list::cons(yy0, yy1);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
}
            ,
            3 /* stmt ::= defstruct */
          | 4 /* stmt ::= let_stmt */
          | 6 /* stmt ::= fail_stmt */
          | 7 /* stmt ::= func_stmt */
          | 8 /* stmt ::= macro_stmt */
          | 9 /* stmt ::= if_stmt */
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
 (YYMinorType::YY104(yy0),) => {
 yyres = yy0; 
},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
}
            ,
            5 /* stmt ::= expr_stmt */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY104(yy0),) => {

    yyres = yy0;

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
}
            ,
            10 /* stmt ::= RETURN expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY104(yy1),) => {

    yyres = sexpr::new(SexprType::Return, yy1);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY117(yy1),YYMinorType::YY104(yy2),) => {

	yyres = sexpr::new(SexprType::MatchFailed,
        list::cons(Val::id(yy1.data),
        list::cons(yy2,
        Val::Nil
        ))
    );

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY75(yy1),YYMinorType::YY104(yy2),) => {

    yyres = sexpr::def_struct(Val::Type(yy1), yy2);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
}
            ,
            13 /* defstruct_fields ::= defstruct_field defstruct_fields */
          | 109 /* strlist ::= strlist_term strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY104(yy0),YYMinorType::YY104(yy1),) => {

	yyres = list::cons(yy0, yy1);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
}
            ,
            14 /* defstruct_fields ::= */
          | 23 /* dfunc_args ::= */
          | 100 /* list_items ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

	yyres = list::empty();

} };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY117(yy1),YYMinorType::YY75(yy3),) => {

	yyres = sexpr::id_with_type(yy1.data, yy3);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY117(yy2),YYMinorType::YY104(yy4),) => {

vout!("found fail_stmt {:?}\n", yy2);
	yyres = sexpr::new(SexprType::Fail,
        list::cons(Val::hashtag(yy2.data),
        list::cons(yy4,
        Val::Nil,
        ))
    );

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY117(yy1),YYMinorType::YY104(yy3),) => {

	let letx =
        list::cons(Val::id(yy1.data),
        list::cons(yy3,
        Val::Nil
        ));
	yyres = sexpr::new(SexprType::Let, letx);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY117(yy1),YYMinorType::YY104(yy3),) => {

	let bind = list::cons(Val::new_str(yy1.data), list::singleton(yy3));
	yyres = sexpr::new(SexprType::Fork, bind);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
}
            ,
            19 /* expr_stmt ::= expr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY104(yy0),) => {

	yyres = yy0;

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
}
            ,
            20 /* block ::= BLOCKARROW stmts */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY104(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY117(yy1),YYMinorType::YY104(yy3),YYMinorType::YY75(yy5),YYMinorType::YY104(yy6),) => {

	let id = Val::id(yy1.data);
	let typ = Val::Type(yy5);
	yyres = sexpr::defunc(id, yy3, typ, yy6)

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY117(yy1),YYMinorType::YY104(yy3),YYMinorType::YY75(yy5),YYMinorType::YY104(yy6),) => {

	let id = Val::id(yy1.data);
	let typ = Val::Type(yy5);
    let body = sexpr::match_expr(Val::CallParams, yy6);
	yyres = sexpr::defunc(id, yy3, typ, body)

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
}
            ,
            24 /* dfunc_args ::= ID opt_typex */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY117(yy0),YYMinorType::YY75(yy1),) => {

	yyres = list::singleton(sexpr::id_with_type(yy0.data, yy1));

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY117(yy0),YYMinorType::YY75(yy1),YYMinorType::YY104(yy3),) => {

	yyres = list::cons(sexpr::id_with_type(yy0.data, yy1), yy3);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 YYMinorType::YY75(yyres)
}
            ,
            27 /* opt_typex ::= COLON typex */
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
            28 /* typex ::= TYPE_INT */
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
            29 /* typex ::= TYPE_STR */
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
            30 /* typex ::= TYPE_BOOL */
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
            31 /* typex ::= TYPE_VOID */
            => 
{
let yyres :  Type ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Type::Void;

} };
 YYMinorType::YY75(yyres)
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
 YYMinorType::YY75(yyres)
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
 (YYMinorType::YY117(yy1),YYMinorType::YY104(yy3),YYMinorType::YY104(yy5),) => {

    verbose_out!("found macro {:?}\n", yy1);
    yyres = sexpr::new(SexprType::DefMacro,
        list::cons(Val::id(yy1.data),
        list::cons(yy3,
        list::cons(yy5,
        Val::Nil
    ))));

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 YYMinorType::YY104(yyres)
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
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY117(yy0),YYMinorType::YY104(yy2),) => {

    yyres = list::cons(Val::id(yy0.data), yy2);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy1),YYMinorType::YY104(yy2),) => {

    /* if-only style */
    yyres = sexpr::ifstmt(yy1, yy2, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy1),YYMinorType::YY104(yy2),YYMinorType::YY104(yy3),) => {

    /* if-else style */
    yyres = sexpr::ifstmt(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy1),) => {

    /* case-expr style */
    yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy2),YYMinorType::YY104(yy3),YYMinorType::YY104(yy4),) => {

    yyres = sexpr::ifstmt(yy2, yy3, yy4);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy2),YYMinorType::YY104(yy3),) => {

    yyres = sexpr::ifstmt(yy2, yy3, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
}
            ,
            42 /* else_if ::= ELSE block */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY104(yy1),) => {

    yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy1),YYMinorType::YY104(yy2),YYMinorType::YY104(yy3),) => {

    yyres = sexpr::ifstmt(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy2),) => {

    yyres = yy2;

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy0),) => {

	verbose_out!("zero param function call!");
	yyres = sexpr::call(yy0, vec![]);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy0),YYMinorType::YY104(yy2),) => {

	verbose_out!("one param function call!");
	yyres = sexpr::call(yy0, vec![yy2]);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy0),YYMinorType::YY104(yy2),) => {

	verbose_out!("multi param function call!");
	yyres = sexpr::call(yy0, list::to_vec(yy2));

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy0),YYMinorType::YY117(yy1),YYMinorType::YY104(yy2),) => {

	yyres = sexpr::binaryop(yy1.data, yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy0),YYMinorType::YY104(yy2),) => {

	/* yyres = Val::binaryop(yy0, yy2, D); */
	yyres = Val::Void;

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy1),) => {

    verbose_out!("parsed case expr\n");
	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy1),YYMinorType::YY104(yy2),YYMinorType::YY104(yy5),) => {

    verbose_out!("found cases base\n");
    yyres = sexpr::casex(yy1, yy2, yy5);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy1),YYMinorType::YY104(yy2),YYMinorType::YY104(yy3),) => {

    verbose_out!("found extra case\n");
    yyres = sexpr::casex(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy1),YYMinorType::YY104(yy2),) => {

    verbose_out!("parsed match expr\n");
    yyres = sexpr::match_expr(yy1, yy2);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy1),YYMinorType::YY104(yy2),YYMinorType::YY104(yy3),) => {

    verbose_out!("found cases base\n");
    yyres = sexpr::match_case(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy1),YYMinorType::YY104(yy2),) => {

    verbose_out!("parsed base match case\n");
    yyres = sexpr::match_case(yy1, yy2, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
}
            ,
            57 /* pexpr ::= INT */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY68(yy0),) => {
 yyres = Val::Int(yy0); 
},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 YYMinorType::YY104(yyres)
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
 YYMinorType::YY104(yyres)
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
 YYMinorType::YY104(yyres)
}
            ,
            61 /* pexpr ::= ID */
          | 91 /* term ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY117(yy0),) => {
 yyres = Val::id(yy0.data); 
},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 YYMinorType::YY104(yyres)
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
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy1),) => {

	yyres = Val::Tuple(vec![yy1]);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy1),) => {

	yyres = Val::tuple_from_list(yy1);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy0),YYMinorType::YY104(yy2),) => {

	yyres = list::cons(yy0,
        list::cons(yy2,
        Val::Nil
        ));

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy0),YYMinorType::YY104(yy2),) => {

	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy0),YYMinorType::YY117(yy2),) => {

    yyres = sexpr::new(SexprType::FieldAccess,
        list::cons(yy0,
        list::cons(Val::id(yy2.data),
        Val::Nil,
        ))
    );

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
}
            ,
            71 /* expr ::= NOT expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY104(yy1),) => {

	yyres = sexpr::call(Val::id("bool_not".to_string()), vec![yy1]);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
}
            ,
            72 /* expr ::= expr ConcatNewline */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY104(yy0),) => {

	let newline = Val::Str(Arc::new("\n".to_string()));
	let args = list::cons(yy0, list::singleton(newline));
	yyres = sexpr::new(SexprType::StrExpr, args)

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
}
            ,
            73 /* expr ::= MINUS expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY104(yy1),) => {

	yyres = sexpr::call(Val::id("negate".to_string()), vec![yy1]);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy0),YYMinorType::YY104(yy2),) => {

	yyres = sexpr::binaryop("int_add".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy0),YYMinorType::YY104(yy2),) => {

	yyres = sexpr::binaryop("int_sub".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy0),YYMinorType::YY104(yy2),) => {

	yyres = sexpr::binaryop("int_mult".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy0),YYMinorType::YY104(yy2),) => {

	yyres = sexpr::binaryop("int_div".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy0),YYMinorType::YY104(yy2),) => {

	yyres = sexpr::binaryop("int_mod".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy0),YYMinorType::YY104(yy2),) => {

	yyres = sexpr::binaryop("and".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy0),YYMinorType::YY104(yy2),) => {

	yyres = sexpr::binaryop("or".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy0),YYMinorType::YY104(yy2),) => {

	yyres = sexpr::binaryop("xor".to_string(),yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy0),YYMinorType::YY104(yy2),) => {

	yyres = sexpr::binaryop("less_than".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy0),YYMinorType::YY104(yy2),) => {

	yyres = sexpr::binaryop("less_than_equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy0),YYMinorType::YY104(yy2),) => {

	yyres = sexpr::binaryop("greater_than".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy0),YYMinorType::YY104(yy2),) => {

	yyres = sexpr::binaryop("greater_than_equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy0),YYMinorType::YY104(yy2),) => {

	yyres = sexpr::binaryop("equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy0),YYMinorType::YY104(yy2),) => {

	let eq = sexpr::binaryop("equal".to_string(), yy0, yy2);
	yyres = sexpr::call(Val::id("bool_not".to_string()), vec![eq]);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
}
            ,
            90 /* term ::= typex */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY75(yy0),) => {

    yyres = Val::Type(yy0);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 YYMinorType::YY104(yyres)
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
 YYMinorType::YY104(yyres)
}
            ,
            94 /* term ::= INT */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY68(yy0),) => {

	yyres = Val::Int(yy0);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 YYMinorType::YY104(yyres)
}
            ,
            101 /* list_items ::= expr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY104(yy0),) => {

	yyres = list::singleton(yy0);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy0),YYMinorType::YY104(yy2),) => {

	vout!("base tuple args!");
	yyres = list::cons(yy0, list::singleton(yy2));

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy0),YYMinorType::YY104(yy2),) => {

	vout!("additional tuple arg!");
	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy1),) => {

	yyres = sexpr::strexpr(yy1);
    vout!("strexpr({:?})\n", yyres);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 YYMinorType::YY104(yyres)
}
            ,
            108 /* strlist ::= StrLit strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY76(yy0),YYMinorType::YY104(yy1),) => {

	yyres = list::cons(Val::new_str(yy0), yy1);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
}
            ,
            110 /* strlist_term ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY117(yy0),) => {

    yyres = Val::id(yy0.data);

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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
 (YYMinorType::YY104(yy0),YYMinorType::YY117(yy2),) => {

    yyres = sexpr::new(SexprType::FieldAccess,
        list::cons(yy0,
        list::cons(Val::id(yy2.data),
        Val::Nil,
    )))

},    _ => unreachable!() };
 YYMinorType::YY104(yyres)
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

