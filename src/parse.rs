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
const YYNSTATE: i32 = 208;
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
    DOT, //29
    LPAREN, //30
    RPAREN, //31
    FAILED, //32
    WITH, //33
    CONTEXTID, //34
    STRUCT, //35
    DOUBLEDASH, //36
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
pub const TOKEN_DOT: i32 = 29;
pub const TOKEN_LPAREN: i32 = 30;
pub const TOKEN_RPAREN: i32 = 31;
pub const TOKEN_FAILED: i32 = 32;
pub const TOKEN_WITH: i32 = 33;
pub const TOKEN_CONTEXTID: i32 = 34;
pub const TOKEN_STRUCT: i32 = 35;
pub const TOKEN_DOUBLEDASH: i32 = 36;
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
        &Token::DOT => TOKEN_DOT,
        &Token::LPAREN => TOKEN_LPAREN,
        &Token::RPAREN => TOKEN_RPAREN,
        &Token::FAILED => TOKEN_FAILED,
        &Token::WITH => TOKEN_WITH,
        &Token::CONTEXTID => TOKEN_CONTEXTID,
        &Token::STRUCT => TOKEN_STRUCT,
        &Token::DOUBLEDASH => TOKEN_DOUBLEDASH,
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
const YY_ACTTAB_COUNT: i32 = 917;
const YY_ACTION: [YYACTIONTYPE; 917] = [
 /*     0 */   179,  185,  182,   94,   71,  186,  192,  124,    4,  157,
 /*    10 */   174,  156,  114,  178,   42,   15,  131,   94,   73,  186,
 /*    20 */   128,   44,   14,  157,  115,  156,    8,  178,  132,   35,
 /*    30 */   208,   53,  198,   44,  125,  120,  113,   12,  111,  196,
 /*    40 */   195,  194,  193,  105,    3,   32,   29,   70,   16,  181,
 /*    50 */   180,  174,  184,  183,    6,   42,   43,  179,  185,  182,
 /*    60 */   117,  168,  159,  192,   31,   30,   28,  124,    4,   93,
 /*    70 */   168,  116,   15,   38,   34,  122,   94,   74,  186,   14,
 /*    80 */    69,  168,  157,    8,  156,  152,  178,   54,   53,  209,
 /*    90 */   130,  125,  120,  113,   12,  111,  196,  195,  194,  193,
 /*   100 */   105,    3,  160,  199,   70,   16,  181,  180,  126,  184,
 /*   110 */   183,    6,  108,   43,   58,  179,  185,  182,  123,   40,
 /*   120 */   143,  192,    1,  107,   52,   94,   73,  186,  176,   40,
 /*   130 */    15,  157,  154,  156,   36,  178,  189,   14,   94,   72,
 /*   140 */   186,    8,  177,  191,  157,  186,  156,   98,  178,  173,
 /*   150 */     1,  175,   40,  178,  196,  195,  194,  193,   11,  164,
 /*   160 */   163,  167,   70,   16,  181,  180,  119,  184,  183,    6,
 /*   170 */   171,   43,   56,  179,  185,  182,   17,  170,   38,  192,
 /*   180 */   158,  153,  151,  112,   13,   37,  161,   39,   15,  110,
 /*   190 */   164,  163,  167,   67,  149,   14,   94,   75,  186,    8,
 /*   200 */    64,   66,  157,  148,  156,  104,  178,   63,  166,  165,
 /*   210 */   162,   62,  196,  195,  194,  193,   37,  145,   61,  142,
 /*   220 */    70,   16,  181,  180,  139,  184,  183,    6,  133,   43,
 /*   230 */   179,  185,  182,  187,   18,  186,  192,    1,   68,  166,
 /*   240 */   165,  162,   99,  178,  169,   15,  121,   65,  172,   51,
 /*   250 */   186,   39,   14,   94,   48,  186,    8,  134,  178,  157,
 /*   260 */   109,  156,  146,  178,   41,  106,  101,  103,  102,  196,
 /*   270 */   195,  194,  193,   54,  118,  141,  144,   70,   16,  181,
 /*   280 */   180,  138,  184,  183,    6,   10,   43,  179,  185,  182,
 /*   290 */   137,  136,    9,  192,  100,   94,   92,  186,   94,   91,
 /*   300 */   186,  157,   15,  156,  157,  178,  156,  135,  178,   14,
 /*   310 */    94,   97,  186,    8,  140,   57,  157,   45,  156,  197,
 /*   320 */   178,  147,   55,  127,   60,   59,  196,  195,  194,  193,
 /*   330 */   322,  322,   10,  322,   70,   16,  181,  180,  322,  184,
 /*   340 */   183,    6,    7,   43,   94,   96,  186,   32,   29,  322,
 /*   350 */   157,  322,  156,  322,  178,   26,   25,   27,  188,  322,
 /*   360 */    20,   19,   22,   21,   24,   23,   31,   30,   28,  124,
 /*   370 */     4,  190,    7,   94,   95,  186,  322,   32,   29,  157,
 /*   380 */   322,  156,  322,  178,  322,   26,   25,   27,  188,  322,
 /*   390 */    20,   19,   22,   21,   24,   23,   31,   30,   28,  124,
 /*   400 */     4,  155,  179,  185,  182,   94,   83,  186,  192,  322,
 /*   410 */   322,  157,  192,  156,   29,  178,  322,   15,  322,  322,
 /*   420 */   322,   94,   81,  186,   14,  322,  322,  157,    8,  156,
 /*   430 */   322,  178,  322,   30,   28,  124,    4,  322,  322,  322,
 /*   440 */   322,  196,  195,  194,  193,  196,  195,  194,  193,   70,
 /*   450 */    16,  181,  180,  322,  184,  183,    6,  322,   43,   32,
 /*   460 */    29,  322,  322,  322,  322,  322,  322,   26,   25,   27,
 /*   470 */   188,  322,   20,   19,   22,   21,   24,   23,   31,   30,
 /*   480 */    28,  124,    4,  155,    7,   94,   80,  186,  322,   32,
 /*   490 */    29,  157,  322,  156,  322,  178,  322,   26,   25,   27,
 /*   500 */   188,  322,   20,   19,   22,   21,   24,   23,   31,   30,
 /*   510 */    28,  124,    4,    5,  322,  322,  322,  322,   32,   29,
 /*   520 */   322,  322,  322,  322,  322,  322,   26,   25,   27,  188,
 /*   530 */   322,   20,   19,   22,   21,   24,   23,   31,   30,   28,
 /*   540 */   124,    4,   32,   29,  322,  322,  322,    1,  322,  322,
 /*   550 */    26,   25,   27,  188,  322,   20,   19,   22,   21,   24,
 /*   560 */    23,   31,   30,   28,  124,    4,   32,   29,  322,  322,
 /*   570 */   322,  322,  322,  322,   26,   25,   27,  188,  322,   20,
 /*   580 */    19,   22,   21,   24,   23,   31,   30,   28,  124,    4,
 /*   590 */   322,  322,   94,   89,  186,  322,   32,   29,  157,  322,
 /*   600 */   156,  322,  178,  322,   26,   25,   27,  188,   39,   20,
 /*   610 */    19,   22,   21,   24,   23,   31,   30,   28,  124,    4,
 /*   620 */   321,  129,    2,  322,  322,  202,  322,  322,  322,  201,
 /*   630 */   322,  322,  200,  322,  322,  322,  322,  322,  322,  206,
 /*   640 */   322,  322,  205,  204,  203,   94,   77,  186,   94,   88,
 /*   650 */   186,  157,  322,  156,  157,  178,  156,  322,  178,   32,
 /*   660 */    29,  322,  322,  322,  322,  322,  322,  322,  322,   27,
 /*   670 */   188,  322,   20,   19,   22,   21,   24,   23,   31,   30,
 /*   680 */    28,  124,    4,  207,    2,  322,  322,  202,  322,  322,
 /*   690 */   322,  201,  322,  322,  200,  322,  322,  322,  322,  322,
 /*   700 */   322,  206,  322,  322,  205,  204,  203,   94,   77,  186,
 /*   710 */   150,    2,  322,  157,  202,  156,  322,  178,  201,  322,
 /*   720 */   322,  200,  322,  322,  322,  322,  322,  322,  206,  322,
 /*   730 */   322,  205,  204,  203,   94,   77,  186,   94,   87,  186,
 /*   740 */   157,  322,  156,  157,  178,  156,  322,  178,   32,   29,
 /*   750 */   322,  322,  322,  322,  179,  185,  182,  322,  322,  188,
 /*   760 */   192,   20,   19,   22,   21,   24,   23,   31,   30,   28,
 /*   770 */   124,    4,  322,   94,   86,  186,   94,   85,  186,  157,
 /*   780 */    33,  156,  157,  178,  156,  322,  178,  322,  322,  322,
 /*   790 */   322,  322,  322,  196,  195,  194,  193,  322,  322,  322,
 /*   800 */   322,   32,   29,  181,  180,  322,  184,  183,  322,  322,
 /*   810 */    43,  322,  322,  322,  320,  320,  320,  320,   24,   23,
 /*   820 */    31,   30,   28,  124,    4,   94,   84,  186,   94,   50,
 /*   830 */   186,  157,  322,  156,  157,  178,  156,  322,  178,  322,
 /*   840 */   322,  322,  322,   94,   79,  186,  322,  322,  322,  157,
 /*   850 */   322,  156,  322,  178,   94,   49,  186,   94,   82,  186,
 /*   860 */   157,  322,  156,  157,  178,  156,  322,  178,   94,   90,
 /*   870 */   186,   94,   78,  186,  157,  322,  156,  157,  178,  156,
 /*   880 */   322,  178,  322,   94,   76,  186,  322,  322,  322,  157,
 /*   890 */   322,  156,  322,  178,  322,   94,   47,  186,  322,  322,
 /*   900 */   322,  157,  322,  156,  322,  178,   94,   46,  186,  322,
 /*   910 */   322,  322,  157,  322,  156,  322,  178,
];
const YY_LOOKAHEAD: [YYCODETYPE; 917] = [
 /*     0 */     4,    5,    6,   88,   89,   90,   10,   29,   30,   94,
 /*    10 */     5,   96,   97,   98,    9,   19,   33,   88,   89,   90,
 /*    20 */    83,   84,   26,   94,   95,   96,   30,   98,   32,    5,
 /*    30 */     0,   35,   83,   84,   38,   39,   40,   41,   42,   43,
 /*    40 */    44,   45,   46,   47,   48,    7,    8,   51,   52,   53,
 /*    50 */    54,    5,   56,   57,   58,    9,   60,    4,    5,    6,
 /*    60 */    79,   80,   81,   10,   26,   27,   28,   29,   30,   79,
 /*    70 */    80,   81,   19,    2,   50,   29,   88,   89,   90,   26,
 /*    80 */    79,   80,   94,   30,   96,   97,   98,    3,   35,    0,
 /*    90 */    34,   38,   39,   40,   41,   42,   43,   44,   45,   46,
 /*   100 */    47,   48,   31,   36,   51,   52,   53,   54,    5,   56,
 /*   110 */    57,   58,   67,   60,    3,    4,    5,    6,   99,  100,
 /*   120 */    36,   10,   12,   78,   37,   88,   89,   90,   99,  100,
 /*   130 */    19,   94,   95,   96,    4,   98,    5,   26,   88,   89,
 /*   140 */    90,   30,   61,   88,   94,   90,   96,   97,   98,    5,
 /*   150 */    12,   99,  100,   98,   43,   44,   45,   46,   48,    4,
 /*   160 */     5,    6,   51,   52,   53,   54,    5,   56,   57,   58,
 /*   170 */    36,   60,    3,    4,    5,    6,   20,   36,    2,   10,
 /*   180 */    31,   59,   31,    5,   20,   30,   31,   49,   19,    5,
 /*   190 */     4,    5,    6,   30,   36,   26,   88,   89,   90,   30,
 /*   200 */     2,   31,   94,   36,   96,    5,   98,   30,   53,   54,
 /*   210 */    55,   31,   43,   44,   45,   46,   30,   36,    2,   36,
 /*   220 */    51,   52,   53,   54,   36,   56,   57,   58,   31,   60,
 /*   230 */     4,    5,    6,   88,   49,   90,   10,   12,   67,   53,
 /*   240 */    54,   55,   77,   98,   78,   19,   93,    5,   88,   37,
 /*   250 */    90,   49,   26,   88,   89,   90,   30,   31,   98,   94,
 /*   260 */    69,   96,   69,   98,   91,   91,    5,   73,   67,   43,
 /*   270 */    44,   45,   46,    3,   78,   76,   73,   51,   52,   53,
 /*   280 */    54,   77,   56,   57,   58,   49,   60,    4,    5,    6,
 /*   290 */    67,   67,   49,   10,   76,   88,   89,   90,   88,   89,
 /*   300 */    90,   94,   19,   96,   94,   98,   96,   93,   98,   26,
 /*   310 */    88,   89,   90,   30,   67,   67,   94,   90,   96,   90,
 /*   320 */    98,   90,   67,   29,   67,   67,   43,   44,   45,   46,
 /*   330 */   101,  101,   49,  101,   51,   52,   53,   54,  101,   56,
 /*   340 */    57,   58,    2,   60,   88,   89,   90,    7,    8,  101,
 /*   350 */    94,  101,   96,  101,   98,   15,   16,   17,   18,  101,
 /*   360 */    20,   21,   22,   23,   24,   25,   26,   27,   28,   29,
 /*   370 */    30,   31,    2,   88,   89,   90,  101,    7,    8,   94,
 /*   380 */   101,   96,  101,   98,  101,   15,   16,   17,   18,  101,
 /*   390 */    20,   21,   22,   23,   24,   25,   26,   27,   28,   29,
 /*   400 */    30,   31,    4,    5,    6,   88,   89,   90,   10,  101,
 /*   410 */   101,   94,   10,   96,    8,   98,  101,   19,  101,  101,
 /*   420 */   101,   88,   89,   90,   26,  101,  101,   94,   30,   96,
 /*   430 */   101,   98,  101,   27,   28,   29,   30,  101,  101,  101,
 /*   440 */   101,   43,   44,   45,   46,   43,   44,   45,   46,   51,
 /*   450 */    52,   53,   54,  101,   56,   57,   58,  101,   60,    7,
 /*   460 */     8,  101,  101,  101,  101,  101,  101,   15,   16,   17,
 /*   470 */    18,  101,   20,   21,   22,   23,   24,   25,   26,   27,
 /*   480 */    28,   29,   30,   31,    2,   88,   89,   90,  101,    7,
 /*   490 */     8,   94,  101,   96,  101,   98,  101,   15,   16,   17,
 /*   500 */    18,  101,   20,   21,   22,   23,   24,   25,   26,   27,
 /*   510 */    28,   29,   30,    2,  101,  101,  101,  101,    7,    8,
 /*   520 */   101,  101,  101,  101,  101,  101,   15,   16,   17,   18,
 /*   530 */   101,   20,   21,   22,   23,   24,   25,   26,   27,   28,
 /*   540 */    29,   30,    7,    8,  101,  101,  101,   12,  101,  101,
 /*   550 */    15,   16,   17,   18,  101,   20,   21,   22,   23,   24,
 /*   560 */    25,   26,   27,   28,   29,   30,    7,    8,  101,  101,
 /*   570 */   101,  101,  101,  101,   15,   16,   17,   18,  101,   20,
 /*   580 */    21,   22,   23,   24,   25,   26,   27,   28,   29,   30,
 /*   590 */   101,  101,   88,   89,   90,  101,    7,    8,   94,  101,
 /*   600 */    96,  101,   98,  101,   15,   16,   17,   18,   49,   20,
 /*   610 */    21,   22,   23,   24,   25,   26,   27,   28,   29,   30,
 /*   620 */    63,   64,   65,  101,  101,   68,  101,  101,  101,   72,
 /*   630 */   101,  101,   75,  101,  101,  101,  101,  101,  101,   82,
 /*   640 */   101,  101,   85,   86,   87,   88,   89,   90,   88,   89,
 /*   650 */    90,   94,  101,   96,   94,   98,   96,  101,   98,    7,
 /*   660 */     8,  101,  101,  101,  101,  101,  101,  101,  101,   17,
 /*   670 */    18,  101,   20,   21,   22,   23,   24,   25,   26,   27,
 /*   680 */    28,   29,   30,   64,   65,  101,  101,   68,  101,  101,
 /*   690 */   101,   72,  101,  101,   75,  101,  101,  101,  101,  101,
 /*   700 */   101,   82,  101,  101,   85,   86,   87,   88,   89,   90,
 /*   710 */    64,   65,  101,   94,   68,   96,  101,   98,   72,  101,
 /*   720 */   101,   75,  101,  101,  101,  101,  101,  101,   82,  101,
 /*   730 */   101,   85,   86,   87,   88,   89,   90,   88,   89,   90,
 /*   740 */    94,  101,   96,   94,   98,   96,  101,   98,    7,    8,
 /*   750 */   101,  101,  101,  101,    4,    5,    6,  101,  101,   18,
 /*   760 */    10,   20,   21,   22,   23,   24,   25,   26,   27,   28,
 /*   770 */    29,   30,  101,   88,   89,   90,   88,   89,   90,   94,
 /*   780 */    30,   96,   94,   98,   96,  101,   98,  101,  101,  101,
 /*   790 */   101,  101,  101,   43,   44,   45,   46,  101,  101,  101,
 /*   800 */   101,    7,    8,   53,   54,  101,   56,   57,  101,  101,
 /*   810 */    60,  101,  101,  101,   20,   21,   22,   23,   24,   25,
 /*   820 */    26,   27,   28,   29,   30,   88,   89,   90,   88,   89,
 /*   830 */    90,   94,  101,   96,   94,   98,   96,  101,   98,  101,
 /*   840 */   101,  101,  101,   88,   89,   90,  101,  101,  101,   94,
 /*   850 */   101,   96,  101,   98,   88,   89,   90,   88,   89,   90,
 /*   860 */    94,  101,   96,   94,   98,   96,  101,   98,   88,   89,
 /*   870 */    90,   88,   89,   90,   94,  101,   96,   94,   98,   96,
 /*   880 */   101,   98,  101,   88,   89,   90,  101,  101,  101,   94,
 /*   890 */   101,   96,  101,   98,  101,   88,   89,   90,  101,  101,
 /*   900 */   101,   94,  101,   96,  101,   98,   88,   89,   90,  101,
 /*   910 */   101,  101,   94,  101,   96,  101,   98,
];
const YY_SHIFT_USE_DFLT: i32 = -23;
const YY_SHIFT_COUNT: i32 = 132;
const YY_SHIFT_MIN: i32 = -22;
const YY_SHIFT_MAX: i32 = 794;
const YY_SHIFT_OFST: [i16; 133] = [
 /*     0 */    -4,   53,   53,  283,  226,  398,  398,  398,  398,  169,
 /*    10 */   111,  398,  398,  398,  398,  398,  398,  398,  398,  398,
 /*    20 */   398,  398,  398,  398,  398,  398,  398,  398,  398,  398,
 /*    30 */   398,  398,  398,  398,  750,  750,  750,  155,  186,  186,
 /*    40 */    46,  138,    5,    5,  294,  294,  535,  535,  535,  559,
 /*    50 */   535,  402,  402,  402,  110,   84,  225,  243,  225,  236,
 /*    60 */   270,  261,  225,  261,  242,  212,  212,  242,  202,  225,
 /*    70 */   185,  370,  340,  511,  482,  452,  589,  589,  589,  589,
 /*    80 */   652,  652,  741,  741,  794,  794,  794,  794,   38,   38,
 /*    90 */   406,  406,  406,   71,   24,  -22,  -22,  -22,  197,  188,
 /*   100 */   183,  216,  181,  180,  177,  200,  198,  167,  158,  170,
 /*   110 */   163,  184,  164,  178,  151,  122,  149,  176,  141,  156,
 /*   120 */   161,  134,  144,   81,  131,  130,   87,  103,   67,   89,
 /*   130 */    30,   56,  -17,
];
const YY_REDUCE_USE_DFLT: i32 = -86;
const YY_REDUCE_COUNT: i32 = 70;
const YY_REDUCE_MIN: i32 = -85;
const YY_REDUCE_MAX: i32 = 818;
const YY_REDUCE_OFST: [i16; 71] = [
 /*     0 */   557,  646,  619,  165,   50,   37,  -71,  -12,  -85,  740,
 /*    10 */   818,  807,  795,  783,  780,  769,  766,  755,  740,  737,
 /*    20 */   688,  685,  649,  560,  504,  397,  333,  317,  285,  256,
 /*    30 */   222,  210,  207,  108,  160,  145,   55,  -10,  -19,    1,
 /*    40 */    52,   45,   29,   19,  -51,  -63,  258,  257,  255,  196,
 /*    50 */   248,  231,  229,  227,  247,  218,  224,  214,  223,  204,
 /*    60 */   199,  203,  201,  194,  193,  174,  173,  191,  166,  171,
 /*    70 */   153,
];
const YY_DEFAULT: [YYACTIONTYPE; 208] = [
 /*     0 */   210,  210,  210,  320,  320,  308,  308,  320,  320,  320,
 /*    10 */   320,  320,  320,  320,  320,  320,  320,  320,  320,  320,
 /*    20 */   320,  320,  320,  320,  320,  320,  320,  320,  320,  320,
 /*    30 */   320,  320,  320,  320,  320,  320,  320,  320,  320,  320,
 /*    40 */   315,  320,  315,  315,  221,  221,  320,  320,  320,  320,
 /*    50 */   320,  320,  320,  320,  320,  320,  320,  320,  320,  320,
 /*    60 */   249,  242,  320,  242,  231,  234,  234,  231,  263,  320,
 /*    70 */   320,  320,  320,  309,  312,  320,  227,  226,  225,  224,
 /*    80 */   289,  288,  279,  287,  295,  294,  293,  292,  291,  290,
 /*    90 */   281,  283,  282,  320,  296,  286,  285,  284,  320,  320,
 /*   100 */   320,  243,  320,  320,  320,  320,  232,  320,  320,  320,
 /*   110 */   320,  320,  320,  320,  320,  320,  320,  274,  320,  320,
 /*   120 */   320,  320,  320,  320,  320,  320,  320,  320,  320,  320,
 /*   130 */   320,  320,  320,  255,  253,  260,  259,  252,  251,  247,
 /*   140 */   250,  248,  246,  245,  244,  241,  233,  235,  230,  229,
 /*   150 */   228,  311,  313,  307,  310,  297,  278,  277,  273,  275,
 /*   160 */   272,  271,  270,  269,  268,  267,  266,  265,  264,  262,
 /*   170 */   261,  258,  257,  319,  318,  317,  316,  314,  306,  305,
 /*   180 */   304,  303,  302,  301,  300,  299,  298,  256,  280,  276,
 /*   190 */   254,  223,  240,  239,  238,  237,  236,  222,  220,  219,
 /*   200 */   218,  217,  216,  215,  214,  213,  212,  211,
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
self.yystack.pop().unwrap();
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
            15 /* fail_stmt ::= FAIL HASHTAG term */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY21(yy1),YYMinorType::YY182(yy2),) => {

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
 YYMinorType::YY182(yyres)
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
            17 /* let_stmt ::= Fork ID EQ expr */
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
self.yystack.pop().unwrap();
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
self.yystack.pop().unwrap();
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
self.yystack.pop().unwrap();
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
self.yystack.pop().unwrap();
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
self.yystack.pop().unwrap();
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
self.yystack.pop().unwrap();
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
self.yystack.pop().unwrap();
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
self.yystack.pop().unwrap();
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

	verbose_out!("base tuple args!");
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

	verbose_out!("additional tuple arg!");
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

