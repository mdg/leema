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
const YYNOCODE: i32 = 94;
type YYACTIONTYPE = u16;
const YYWILDCARD: YYCODETYPE = 1;
enum YYMinorType {
    YY0,
    YY2(Val),
    YY7(Ast),
    YY46(TokenLoc),
    YY74(i64),
    YY111(Type),
    YY118(String),
    YY123(TokenData<String>),
}
const YYNSTATE: i32 = 166;
const YYNRULE: i32 = 89;
const YYERRORSYMBOL: i32 = 59;

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
    ID( String ), //5
    INT( i64 ), //6
    NEWLINE( TokenLoc ), //7
    PLUS( TokenLoc ), //8
    SLASH( TokenLoc ), //9
    StrLit( String ), //10
    TYPE_ID( String ), //11
    ASSIGN, //12
    BLOCKARROW, //13
    WHERE, //14
    GIVEN, //15
    OR, //16
    XOR, //17
    AND, //18
    ConcatNewline, //19
    NOT, //20
    LT, //21
    LTEQ, //22
    EQ, //23
    NEQ, //24
    GT, //25
    GTEQ, //26
    MINUS, //27
    TIMES, //28
    LPAREN, //29
    RPAREN, //30
    FAILED, //31
    WITH, //32
    CONTEXTID, //33
    DT, //34
    IF, //35
    FAIL, //36
    Let, //37
    Fork, //38
    DollarGT, //39
    Func, //40
    COLON, //41
    TYPE_INT, //42
    TYPE_STR, //43
    TYPE_BOOL, //44
    TYPE_VOID, //45
    MACRO, //46
    DOLLAR, //47
    CASE, //48
    PIPE, //49
    MOD, //50
    VOID, //51
    DollarQuestion, //52
    True, //53
    False, //54
    SquareL, //55
    SquareR, //56
    StrOpen, //57
    StrClose, //58
}
pub const TOKEN_EOI: i32 = 0;
pub const TOKEN_ANY: i32 = 1;
pub const TOKEN_COMMA: i32 = 2;
pub const TOKEN_ELSE: i32 = 3;
pub const TOKEN_HASHTAG: i32 = 4;
pub const TOKEN_ID: i32 = 5;
pub const TOKEN_INT: i32 = 6;
pub const TOKEN_NEWLINE: i32 = 7;
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
pub const TOKEN_LT: i32 = 21;
pub const TOKEN_LTEQ: i32 = 22;
pub const TOKEN_EQ: i32 = 23;
pub const TOKEN_NEQ: i32 = 24;
pub const TOKEN_GT: i32 = 25;
pub const TOKEN_GTEQ: i32 = 26;
pub const TOKEN_MINUS: i32 = 27;
pub const TOKEN_TIMES: i32 = 28;
pub const TOKEN_LPAREN: i32 = 29;
pub const TOKEN_RPAREN: i32 = 30;
pub const TOKEN_FAILED: i32 = 31;
pub const TOKEN_WITH: i32 = 32;
pub const TOKEN_CONTEXTID: i32 = 33;
pub const TOKEN_DT: i32 = 34;
pub const TOKEN_IF: i32 = 35;
pub const TOKEN_FAIL: i32 = 36;
pub const TOKEN_Let: i32 = 37;
pub const TOKEN_Fork: i32 = 38;
pub const TOKEN_DollarGT: i32 = 39;
pub const TOKEN_Func: i32 = 40;
pub const TOKEN_COLON: i32 = 41;
pub const TOKEN_TYPE_INT: i32 = 42;
pub const TOKEN_TYPE_STR: i32 = 43;
pub const TOKEN_TYPE_BOOL: i32 = 44;
pub const TOKEN_TYPE_VOID: i32 = 45;
pub const TOKEN_MACRO: i32 = 46;
pub const TOKEN_DOLLAR: i32 = 47;
pub const TOKEN_CASE: i32 = 48;
pub const TOKEN_PIPE: i32 = 49;
pub const TOKEN_MOD: i32 = 50;
pub const TOKEN_VOID: i32 = 51;
pub const TOKEN_DollarQuestion: i32 = 52;
pub const TOKEN_True: i32 = 53;
pub const TOKEN_False: i32 = 54;
pub const TOKEN_SquareL: i32 = 55;
pub const TOKEN_SquareR: i32 = 56;
pub const TOKEN_StrOpen: i32 = 57;
pub const TOKEN_StrClose: i32 = 58;
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
        &Token::NEWLINE(_) => TOKEN_NEWLINE,
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
        &Token::LT => TOKEN_LT,
        &Token::LTEQ => TOKEN_LTEQ,
        &Token::EQ => TOKEN_EQ,
        &Token::NEQ => TOKEN_NEQ,
        &Token::GT => TOKEN_GT,
        &Token::GTEQ => TOKEN_GTEQ,
        &Token::MINUS => TOKEN_MINUS,
        &Token::TIMES => TOKEN_TIMES,
        &Token::LPAREN => TOKEN_LPAREN,
        &Token::RPAREN => TOKEN_RPAREN,
        &Token::FAILED => TOKEN_FAILED,
        &Token::WITH => TOKEN_WITH,
        &Token::CONTEXTID => TOKEN_CONTEXTID,
        &Token::DT => TOKEN_DT,
        &Token::IF => TOKEN_IF,
        &Token::FAIL => TOKEN_FAIL,
        &Token::Let => TOKEN_Let,
        &Token::Fork => TOKEN_Fork,
        &Token::DollarGT => TOKEN_DollarGT,
        &Token::Func => TOKEN_Func,
        &Token::COLON => TOKEN_COLON,
        &Token::TYPE_INT => TOKEN_TYPE_INT,
        &Token::TYPE_STR => TOKEN_TYPE_STR,
        &Token::TYPE_BOOL => TOKEN_TYPE_BOOL,
        &Token::TYPE_VOID => TOKEN_TYPE_VOID,
        &Token::MACRO => TOKEN_MACRO,
        &Token::DOLLAR => TOKEN_DOLLAR,
        &Token::CASE => TOKEN_CASE,
        &Token::PIPE => TOKEN_PIPE,
        &Token::MOD => TOKEN_MOD,
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
        Token::COMMA(x) => YYMinorType::YY46(x),
        Token::ELSE(x) => YYMinorType::YY46(x),
        Token::HASHTAG(x) => YYMinorType::YY123(x),
        Token::ID(x) => YYMinorType::YY118(x),
        Token::INT(x) => YYMinorType::YY74(x),
        Token::NEWLINE(x) => YYMinorType::YY46(x),
        Token::PLUS(x) => YYMinorType::YY46(x),
        Token::SLASH(x) => YYMinorType::YY46(x),
        Token::StrLit(x) => YYMinorType::YY118(x),
        Token::TYPE_ID(x) => YYMinorType::YY118(x),
        _ => YYMinorType::YY0
  }
}
const YY_ACTTAB_COUNT: i32 = 841;
const YY_ACTION: [YYACTIONTYPE; 841] = [
 /*     0 */   141,  107,  144,   86,   44,  129,  157,  156,   86,   48,
 /*    10 */   149,  110,  148,  102,  140,  149,   31,  148,  134,  140,
 /*    20 */   121,  157,  156,   18,   71,   10,  123,  111,  109,   72,
 /*    30 */   161,   36,  101,  100,   98,   15,   96,  166,  141,  107,
 /*    40 */   144,   43,   92,  167,  106,    6,    2,  146,  145,  143,
 /*    50 */   142,    8,    5,   73,   31,   86,   45,  127,  126,  125,
 /*    60 */   124,   18,  149,   10,  148,   88,  140,   85,  161,   36,
 /*    70 */   101,  100,   98,   15,   96,  151,  141,  107,  144,    3,
 /*    80 */    92,  138,  106,   41,  152,  146,  145,  143,  142,    8,
 /*    90 */     9,   73,   31,  133,  135,  140,   11,   29,   29,   18,
 /*   100 */   116,   10,   42,   17,   26,   25,   27,  155,  132,   24,
 /*   110 */    23,   20,   19,   22,   21,   32,   30,   30,  153,  140,
 /*   120 */   106,   86,   68,  146,  145,  143,  142,    8,  149,   73,
 /*   130 */   148,  117,  140,   99,   11,   29,   35,   97,   28,   28,
 /*   140 */    16,    9,  140,  158,  157,  156,   95,   11,   29,   20,
 /*   150 */    19,   22,   21,   32,   30,   26,   25,   27,  155,   82,
 /*   160 */    24,   23,   20,   19,   22,   21,   32,   30,   81,  147,
 /*   170 */    79,   35,   91,  256,  108,   78,   28,   74,  141,  131,
 /*   180 */   144,   76,   38,   77,  160,  115,  114,  105,  159,   28,
 /*   190 */   164,  163,  162,   86,   52,   35,   33,  150,   11,   29,
 /*   200 */   149,   28,  148,   34,  140,   94,   26,   25,   27,  155,
 /*   210 */    80,   24,   23,   20,   19,   22,   21,   32,   30,   67,
 /*   220 */    39,  122,   89,   93,   90,  146,  145,  143,  142,  120,
 /*   230 */   118,   73,   12,  119,   13,    1,  104,   86,   47,   14,
 /*   240 */    28,  257,  137,    9,  149,  103,  148,  128,  140,   11,
 /*   250 */    29,  136,  257,  257,  257,  257,  257,   26,   25,   27,
 /*   260 */   155,  257,   24,   23,   20,   19,   22,   21,   32,   30,
 /*   270 */   257,   86,   47,  257,   86,   37,  257,  257,  149,  139,
 /*   280 */   148,  149,  140,  148,  257,  140,  257,  257,   86,   50,
 /*   290 */   257,   28,  257,  257,    7,  149,  257,  148,  257,  140,
 /*   300 */    11,   29,  257,  257,  257,  257,  257,  257,   26,   25,
 /*   310 */    27,  155,  257,   24,   23,   20,   19,   22,   21,   32,
 /*   320 */    30,  257,  257,  257,   86,   46,  257,  165,  257,  257,
 /*   330 */    74,  149,  257,  148,  257,  140,  257,  160,  257,  257,
 /*   340 */   257,  159,   28,  164,  163,  162,   86,   52,  257,  257,
 /*   350 */   257,   11,   29,  149,  257,  148,  257,  140,  257,   26,
 /*   360 */    25,   27,  155,  257,   24,   23,   20,   19,   22,   21,
 /*   370 */    32,   30,  257,  147,  257,   86,   40,  257,  130,  257,
 /*   380 */   257,   74,  149,  257,  148,  257,  140,  257,  160,  257,
 /*   390 */   257,  257,  159,   28,  164,  163,  162,   86,   52,  257,
 /*   400 */   257,  257,   11,   29,  149,  257,  148,   13,  140,  257,
 /*   410 */    26,   25,   27,  155,  257,   24,   23,   20,   19,   22,
 /*   420 */    21,   32,   30,  257,   86,   69,  257,   86,   60,   87,
 /*   430 */   257,  149,   74,  148,  149,  140,  148,  257,  140,  160,
 /*   440 */   257,  257,  257,  159,   28,  164,  163,  162,   86,   52,
 /*   450 */   257,  257,  257,   11,   29,  149,  257,  148,   14,  140,
 /*   460 */   257,   26,   25,   27,  155,  257,   24,   23,   20,   19,
 /*   470 */    22,   21,   32,   30,  257,   86,   84,  257,   86,   83,
 /*   480 */   113,  257,  149,   74,  148,  149,  140,  148,  257,  140,
 /*   490 */   160,  257,  257,  257,  159,   28,  164,  163,  162,   86,
 /*   500 */    52,  257,  257,  257,   11,   29,  149,  257,  148,  257,
 /*   510 */   140,  257,   26,   25,   27,  155,  257,   24,   23,   20,
 /*   520 */    19,   22,   21,   32,   30,  141,  107,  144,    4,   86,
 /*   530 */    55,  257,   75,  141,  107,  144,  149,  257,  148,  257,
 /*   540 */   140,   31,  257,  257,   86,   59,   28,  257,   18,   31,
 /*   550 */    10,  149,  257,  148,  257,  140,   18,  257,   10,   86,
 /*   560 */    58,  257,  257,  257,  257,  257,  149,  257,  148,  106,
 /*   570 */   140,  257,  146,  145,  143,  142,    8,  106,   73,  257,
 /*   580 */   146,  145,  143,  142,    8,  257,   73,  141,  107,  144,
 /*   590 */   257,   86,   57,  257,  257,  257,  257,  257,  149,  257,
 /*   600 */   148,  257,  140,   31,  257,  257,  257,   86,   66,  257,
 /*   610 */    18,  257,   10,  154,  149,  257,  148,  257,  140,   86,
 /*   620 */    65,  257,  257,  257,  257,  257,  149,  257,  148,  257,
 /*   630 */   140,  106,  257,  257,  146,  145,  143,  142,    8,  257,
 /*   640 */    73,  257,  257,   11,   29,  257,  257,  257,  141,  107,
 /*   650 */   144,  257,  257,   27,  155,  257,   24,   23,   20,   19,
 /*   660 */    22,   21,   32,   30,   31,   86,   64,  257,   86,   63,
 /*   670 */   257,   18,  149,   10,  148,  149,  140,  148,  257,  140,
 /*   680 */   257,  257,  257,  257,  257,   28,  257,  257,  257,  257,
 /*   690 */   257,  257,  106,  257,  257,  146,  145,  143,  142,    8,
 /*   700 */   257,   73,  257,  257,   11,   29,  257,  257,  257,  257,
 /*   710 */   257,  257,  257,  257,  257,  155,  257,   24,   23,   20,
 /*   720 */    19,   22,   21,   32,   30,  112,  257,  257,   74,   86,
 /*   730 */    62,  257,  257,  257,  257,  160,  149,  257,  148,  159,
 /*   740 */   140,  164,  163,  162,   86,   52,   28,  257,   11,   29,
 /*   750 */   257,  149,  257,  148,  257,  140,  257,  257,  257,  257,
 /*   760 */   257,  257,  257,  255,  255,  255,  255,   32,   30,  257,
 /*   770 */   257,  257,  257,   86,   61,  257,  257,  257,   86,   70,
 /*   780 */   149,  257,  148,  257,  140,  149,  257,  148,  257,  140,
 /*   790 */    28,  257,  257,  257,  257,  257,   86,   54,  257,   86,
 /*   800 */    53,  257,  257,  149,  257,  148,  149,  140,  148,  257,
 /*   810 */   140,   86,   51,  257,  257,  257,   86,   56,  149,  257,
 /*   820 */   148,  257,  140,  149,  257,  148,  257,  140,  257,   86,
 /*   830 */    49,  257,  257,  257,  257,  257,  149,  257,  148,  257,
 /*   840 */   140,
];
const YY_LOOKAHEAD: [YYCODETYPE; 841] = [
 /*     0 */     4,    5,    6,   80,   81,   66,   67,   68,   80,   81,
 /*    10 */    87,   32,   89,   90,   91,   87,   20,   89,   90,   91,
 /*    20 */    66,   67,   68,   27,    5,   29,   11,   31,   33,   10,
 /*    30 */    34,   35,   36,   37,   38,   39,   40,    0,    4,    5,
 /*    40 */     6,    5,   46,    0,   48,   29,    1,   51,   52,   53,
 /*    50 */    54,   55,    7,   57,   20,   80,   81,   42,   43,   44,
 /*    60 */    45,   27,   87,   29,   89,   90,   91,    7,   34,   35,
 /*    70 */    36,   37,   38,   39,   40,    9,    4,    5,    6,    7,
 /*    80 */    46,   58,   48,   47,   80,   51,   52,   53,   54,   55,
 /*    90 */     2,   57,   20,   30,   56,   91,    8,    9,    9,   27,
 /*   100 */    59,   29,    4,   23,   16,   17,   18,   19,   80,   21,
 /*   110 */    22,   23,   24,   25,   26,   27,   28,   28,   30,   91,
 /*   120 */    48,   80,   81,   51,   52,   53,   54,   55,   87,   57,
 /*   130 */    89,   80,   91,    5,    8,    9,   59,    5,   50,   50,
 /*   140 */    23,    2,   91,   66,   67,   68,    5,    8,    9,   23,
 /*   150 */    24,   25,   26,   27,   28,   16,   17,   18,   19,   29,
 /*   160 */    21,   22,   23,   24,   25,   26,   27,   28,   30,   30,
 /*   170 */     2,   59,    5,   60,   61,   29,   50,   64,    4,    5,
 /*   180 */     6,   69,   30,    2,   71,   30,    9,   85,   75,   50,
 /*   190 */    77,   78,   79,   80,   81,   59,   49,    7,    8,    9,
 /*   200 */    87,   50,   89,   29,   91,   72,   16,   17,   18,   19,
 /*   210 */     5,   21,   22,   23,   24,   25,   26,   27,   28,   41,
 /*   220 */    83,   72,    5,   83,   76,   51,   52,   53,   54,   76,
 /*   230 */    85,   57,   49,   69,   13,   59,   92,   80,   81,   13,
 /*   240 */    50,   93,   92,    2,   87,   88,   89,   82,   91,    8,
 /*   250 */     9,   92,   93,   93,   93,   93,   93,   16,   17,   18,
 /*   260 */    19,   93,   21,   22,   23,   24,   25,   26,   27,   28,
 /*   270 */    93,   80,   81,   93,   80,   81,   93,   93,   87,   88,
 /*   280 */    89,   87,   91,   89,   93,   91,   93,   93,   80,   81,
 /*   290 */    93,   50,   93,   93,    2,   87,   93,   89,   93,   91,
 /*   300 */     8,    9,   93,   93,   93,   93,   93,   93,   16,   17,
 /*   310 */    18,   19,   93,   21,   22,   23,   24,   25,   26,   27,
 /*   320 */    28,   93,   93,   93,   80,   81,   93,   61,   93,   93,
 /*   330 */    64,   87,   93,   89,   93,   91,   93,   71,   93,   93,
 /*   340 */    93,   75,   50,   77,   78,   79,   80,   81,   93,   93,
 /*   350 */    93,    8,    9,   87,   93,   89,   93,   91,   93,   16,
 /*   360 */    17,   18,   19,   93,   21,   22,   23,   24,   25,   26,
 /*   370 */    27,   28,   93,   30,   93,   80,   81,   93,   61,   93,
 /*   380 */    93,   64,   87,   93,   89,   93,   91,   93,   71,   93,
 /*   390 */    93,   93,   75,   50,   77,   78,   79,   80,   81,   93,
 /*   400 */    93,   93,    8,    9,   87,   93,   89,   13,   91,   93,
 /*   410 */    16,   17,   18,   19,   93,   21,   22,   23,   24,   25,
 /*   420 */    26,   27,   28,   93,   80,   81,   93,   80,   81,   61,
 /*   430 */    93,   87,   64,   89,   87,   91,   89,   93,   91,   71,
 /*   440 */    93,   93,   93,   75,   50,   77,   78,   79,   80,   81,
 /*   450 */    93,   93,   93,    8,    9,   87,   93,   89,   13,   91,
 /*   460 */    93,   16,   17,   18,   19,   93,   21,   22,   23,   24,
 /*   470 */    25,   26,   27,   28,   93,   80,   81,   93,   80,   81,
 /*   480 */    61,   93,   87,   64,   89,   87,   91,   89,   93,   91,
 /*   490 */    71,   93,   93,   93,   75,   50,   77,   78,   79,   80,
 /*   500 */    81,   93,   93,   93,    8,    9,   87,   93,   89,   93,
 /*   510 */    91,   93,   16,   17,   18,   19,   93,   21,   22,   23,
 /*   520 */    24,   25,   26,   27,   28,    4,    5,    6,    7,   80,
 /*   530 */    81,   93,    3,    4,    5,    6,   87,   93,   89,   93,
 /*   540 */    91,   20,   93,   93,   80,   81,   50,   93,   27,   20,
 /*   550 */    29,   87,   93,   89,   93,   91,   27,   93,   29,   80,
 /*   560 */    81,   93,   93,   93,   93,   93,   87,   93,   89,   48,
 /*   570 */    91,   93,   51,   52,   53,   54,   55,   48,   57,   93,
 /*   580 */    51,   52,   53,   54,   55,   93,   57,    4,    5,    6,
 /*   590 */    93,   80,   81,   93,   93,   93,   93,   93,   87,   93,
 /*   600 */    89,   93,   91,   20,   93,   93,   93,   80,   81,   93,
 /*   610 */    27,   93,   29,   30,   87,   93,   89,   93,   91,   80,
 /*   620 */    81,   93,   93,   93,   93,   93,   87,   93,   89,   93,
 /*   630 */    91,   48,   93,   93,   51,   52,   53,   54,   55,   93,
 /*   640 */    57,   93,   93,    8,    9,   93,   93,   93,    4,    5,
 /*   650 */     6,   93,   93,   18,   19,   93,   21,   22,   23,   24,
 /*   660 */    25,   26,   27,   28,   20,   80,   81,   93,   80,   81,
 /*   670 */    93,   27,   87,   29,   89,   87,   91,   89,   93,   91,
 /*   680 */    93,   93,   93,   93,   93,   50,   93,   93,   93,   93,
 /*   690 */    93,   93,   48,   93,   93,   51,   52,   53,   54,   55,
 /*   700 */    93,   57,   93,   93,    8,    9,   93,   93,   93,   93,
 /*   710 */    93,   93,   93,   93,   93,   19,   93,   21,   22,   23,
 /*   720 */    24,   25,   26,   27,   28,   61,   93,   93,   64,   80,
 /*   730 */    81,   93,   93,   93,   93,   71,   87,   93,   89,   75,
 /*   740 */    91,   77,   78,   79,   80,   81,   50,   93,    8,    9,
 /*   750 */    93,   87,   93,   89,   93,   91,   93,   93,   93,   93,
 /*   760 */    93,   93,   93,   23,   24,   25,   26,   27,   28,   93,
 /*   770 */    93,   93,   93,   80,   81,   93,   93,   93,   80,   81,
 /*   780 */    87,   93,   89,   93,   91,   87,   93,   89,   93,   91,
 /*   790 */    50,   93,   93,   93,   93,   93,   80,   81,   93,   80,
 /*   800 */    81,   93,   93,   87,   93,   89,   87,   91,   89,   93,
 /*   810 */    91,   80,   81,   93,   93,   93,   80,   81,   87,   93,
 /*   820 */    89,   93,   91,   87,   93,   89,   93,   91,   93,   80,
 /*   830 */    81,   93,   93,   93,   93,   93,   87,   93,   89,   93,
 /*   840 */    91,
];
const YY_SHIFT_USE_DFLT: i32 = -22;
const YY_SHIFT_COUNT: i32 = 111;
const YY_SHIFT_MIN: i32 = -21;
const YY_SHIFT_MAX: i32 = 740;
const YY_SHIFT_OFST: [i16; 112] = [
 /*     0 */    -4,   34,   34,   34,   34,   34,  583,  644,  644,  644,
 /*    10 */   644,  644,  529,  521,   72,  644,  644,  644,  644,  644,
 /*    20 */   644,  644,  644,  644,  644,  644,  644,  644,  644,  644,
 /*    30 */   644,  644,  644,  644,  644,  644,  644,  445,  226,  226,
 /*    40 */   394,  174,  174,  174,  139,   88,  343,  292,  241,  190,
 /*    50 */   496,  496,  496,  496,  496,  496,  496,  635,  635,  696,
 /*    60 */   696,  740,  740,  740,  740,  126,  126,   15,   89,   89,
 /*    70 */    89,   19,   19,   19,   45,  221,  183,  217,  217,  205,
 /*    80 */   178,  178,  205,  151,  151,  147,   36,  177,  155,  181,
 /*    90 */   152,  146,  167,  168,  138,  130,  141,  117,  132,   80,
 /*   100 */   128,   98,   63,   38,   23,   66,   60,   16,   43,   37,
 /*   110 */    -5,  -21,
];
const YY_REDUCE_USE_DFLT: i32 = -78;
const YY_REDUCE_COUNT: i32 = 85;
const YY_REDUCE_MIN: i32 = -77;
const YY_REDUCE_MAX: i32 = 749;
const YY_REDUCE_OFST: [i16; 86] = [
 /*     0 */   113,  664,  419,  368,  317,  266,  -25,  191,  157,  -72,
 /*    10 */   -77,   41,  295,  749,  736,  731,  719,  716,  698,  693,
 /*    20 */   649,  588,  585,  539,  527,  511,  479,  464,  449,  398,
 /*    30 */   395,  347,  344,  295,  244,  208,  194,   77,  -46,  -61,
 /*    40 */   112,   51,   28,    4,  136,  136,  136,  136,  136,  136,
 /*    50 */   136,  136,  136,  136,  136,  136,  136,  136,  136,  136,
 /*    60 */   136,  136,  136,  136,  136,  136,  136,  165,  136,  136,
 /*    70 */   136,  159,  150,  144,  176,  164,  145,  153,  148,  149,
 /*    80 */   140,  137,  133,  136,  136,  102,
];
const YY_DEFAULT: [YYACTIONTYPE; 166] = [
 /*     0 */   168,  168,  168,  168,  168,  168,  255,  245,  245,  255,
 /*    10 */   255,  255,  255,  255,  255,  255,  255,  255,  255,  255,
 /*    20 */   255,  255,  255,  255,  255,  255,  255,  255,  255,  255,
 /*    30 */   255,  255,  255,  255,  255,  255,  255,  255,  255,  255,
 /*    40 */   255,  255,  255,  255,  255,  255,  255,  246,  249,  255,
 /*    50 */   218,  183,  182,  181,  180,  224,  186,  227,  226,  225,
 /*    60 */   215,  233,  232,  231,  230,  229,  228,  255,  219,  221,
 /*    70 */   217,  252,  252,  252,  255,  255,  255,  202,  202,  191,
 /*    80 */   194,  194,  191,  223,  222,  255,  234,  255,  255,  203,
 /*    90 */   255,  255,  255,  192,  255,  255,  255,  255,  255,  255,
 /*   100 */   255,  255,  255,  255,  255,  255,  255,  236,  255,  255,
 /*   110 */   255,  255,  171,  170,  187,  207,  220,  209,  212,  211,
 /*   120 */   204,  201,  193,  200,  199,  198,  197,  196,  195,  190,
 /*   130 */   189,  236,  179,  248,  250,  244,  254,  253,  251,  247,
 /*   140 */   243,  242,  241,  240,  239,  238,  237,  235,  214,  213,
 /*   150 */   188,  210,  208,  206,  205,  216,  185,  184,  178,  177,
 /*   160 */   176,  175,  174,  173,  172,  169,
];

/* TMPL: fallback tokens */

const YY_FALLBACK: [i32; 0] = [
];

/* TMPL: symbol names */


/* TMPL: rules */


/* TMPL: destructors */


/* TMPL: stack-overflow */


/* TMPL: stack-overflow */

const YY_RULE_INFO: [YYCODETYPE; 89] = [
  60,
  60,
  61,
  61,
  61,
  61,
  64,
  64,
  64,
  64,
  64,
  64,
  64,
  79,
  77,
  77,
  78,
  78,
  66,
  66,
  67,
  68,
  69,
  69,
  71,
  72,
  72,
  72,
  83,
  83,
  82,
  82,
  82,
  82,
  82,
  75,
  76,
  76,
  76,
  81,
  81,
  81,
  81,
  81,
  81,
  85,
  85,
  81,
  81,
  81,
  81,
  81,
  81,
  81,
  81,
  81,
  81,
  81,
  81,
  81,
  81,
  81,
  81,
  81,
  81,
  81,
  81,
  81,
  81,
  80,
  80,
  80,
  80,
  80,
  80,
  80,
  80,
  80,
  87,
  88,
  88,
  88,
  89,
  90,
  90,
  91,
  92,
  92,
  92,
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
 YYMinorType::YY7(yyres)
}
            ,
            1 /* program ::= stmts */
            => 
{
let yyres :  Ast ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY2(yy0),) => {

	if list::is_empty(&yy0) {
		panic!("null program");
	}
	// ignore yyres, it doesn't really go anywhere for program
	yyres = Ast::Nothing;
	// we're done, so put yy0 in extra
	self.extra = Ok(Ast::ReplRoot(yy0));

},    _ => unreachable!() };
 YYMinorType::YY7(yyres)
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
 YYMinorType::YY2(yyres)
}
            ,
            3 /* stmts ::= stmt NEWLINE stmts */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY2(yy0),YYMinorType::YY2(yy2),) => {

    verbose_out!("found new stmt: {:?}\n", yy0);
	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            4 /* stmts ::= stmt ANY stmts */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match () {
 () => {

    verbose_out!("expected NEWLINE after stmt, found something else\n");
	yyres = Val::Void;

} };
 YYMinorType::YY2(yyres)
}
            ,
            5 /* stmts ::= stmt error stmts */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match () {
 () => {

    verbose_out!("error parsing newline after stmt\n");
	yyres = Val::Void;

} };
 YYMinorType::YY2(yyres)
}
            ,
            6 /* stmt ::= let_stmt */
          | 8 /* stmt ::= fail_stmt */
          | 10 /* stmt ::= func_stmt */
          | 11 /* stmt ::= macro_stmt */
          | 47 /* expr ::= list */
          | 48 /* expr ::= tuple */
          | 68 /* expr ::= term */
          | 77 /* term ::= strexpr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY2(yy0),) => {
 yyres = yy0; 
},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            7 /* stmt ::= expr_stmt */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY2(yy0),) => {

    yyres = yy0;

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
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
 YYMinorType::YY2(yyres)
}
            ,
            12 /* stmt ::= IF expr block */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY2(yy1),YYMinorType::YY2(yy2),) => {

    yyres = sexpr::ifexpr(yy1, yy2, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            13 /* fail_stmt ::= FAIL HASHTAG term */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY123(yy1),YYMinorType::YY2(yy2),) => {

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
 YYMinorType::YY2(yyres)
}
            ,
            14 /* let_stmt ::= Let ID EQ expr */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY118(yy1),YYMinorType::YY2(yy3),) => {

	let letx =
        list::cons(Val::id(yy1),
        list::cons(yy3,
        Val::Nil
        ));
	yyres = sexpr::new(SexprType::Let, letx);

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            15 /* let_stmt ::= Fork ID EQ expr */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY118(yy1),YYMinorType::YY2(yy3),) => {

	let bind = list::cons(Val::new_str(yy1), list::singleton(yy3));
	yyres = sexpr::new(SexprType::Fork, bind);

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            16 /* expr_stmt ::= expr */
          | 18 /* block ::= block_one */
          | 19 /* block ::= block_many */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY2(yy0),) => {

	yyres = yy0;

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            17 /* expr_stmt ::= DollarGT expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY2(yy1),) => {
 yyres = yy1; 
},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            20 /* block_one ::= BLOCKARROW expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY2(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            21 /* block_many ::= BLOCKARROW NEWLINE stmts SLASH */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp2.minor,) {
 (YYMinorType::YY2(yy2),) => {

	yyres = yy2;

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            22 /* midblock ::= BLOCKARROW expr NEWLINE */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY2(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            23 /* midblock ::= BLOCKARROW NEWLINE stmts */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp2.minor,) {
 (YYMinorType::YY2(yy2),) => {

	yyres = yy2;

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            24 /* func_stmt ::= Func ID LPAREN dfunc_args RPAREN opt_typex block */
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
match (yyp1.minor,yyp3.minor,yyp5.minor,yyp6.minor,) {
 (YYMinorType::YY118(yy1),YYMinorType::YY2(yy3),YYMinorType::YY111(yy5),YYMinorType::YY2(yy6),) => {

	let id = Val::id(yy1);
	let typ = Val::Type(yy5);
	yyres = sexpr::defunc(id, yy3, typ, yy6)

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            25 /* dfunc_args ::= */
          | 79 /* list_items ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

	yyres = list::empty();

} };
 YYMinorType::YY2(yyres)
}
            ,
            26 /* dfunc_args ::= ID opt_typex */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY118(yy0),YYMinorType::YY111(yy1),) => {

	yyres = list::singleton(sexpr::id_with_type(yy0, yy1));

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            27 /* dfunc_args ::= ID opt_typex COMMA dfunc_args */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY118(yy0),YYMinorType::YY111(yy1),YYMinorType::YY2(yy3),) => {

	yyres = list::cons(sexpr::id_with_type(yy0, yy1), yy3);

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            28 /* opt_typex ::= */
            => 
{
let yyres :  Type ;
match () {
 () => {

	yyres = Type::AnonVar;

} };
 YYMinorType::YY111(yyres)
}
            ,
            29 /* opt_typex ::= COLON typex */
            => 
{
let yyres :  Type ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY111(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY111(yyres)
}
            ,
            30 /* typex ::= TYPE_INT */
            => 
{
let yyres :  Type ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Type::Int;

} };
 YYMinorType::YY111(yyres)
}
            ,
            31 /* typex ::= TYPE_STR */
            => 
{
let yyres :  Type ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Type::Str;

} };
 YYMinorType::YY111(yyres)
}
            ,
            32 /* typex ::= TYPE_BOOL */
            => 
{
let yyres :  Type ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Type::Bool;

} };
 YYMinorType::YY111(yyres)
}
            ,
            33 /* typex ::= TYPE_VOID */
            => 
{
let yyres :  Type ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Type::Void;

} };
 YYMinorType::YY111(yyres)
}
            ,
            34 /* typex ::= TYPE_ID */
            => 
{
let yyres :  Type ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY118(yy0),) => {

	yyres = Type::Id(Arc::new(yy0));

},    _ => unreachable!() };
 YYMinorType::YY111(yyres)
}
            ,
            35 /* macro_stmt ::= MACRO ID LPAREN macro_args RPAREN block */
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
 (YYMinorType::YY118(yy1),YYMinorType::YY2(yy3),YYMinorType::YY2(yy5),) => {

    verbose_out!("found macro {:?}\n", yy1);
    yyres = sexpr::new(SexprType::DefMacro,
        list::cons(Val::id(yy1),
        list::cons(yy3,
        list::cons(yy5,
        Val::Nil
    ))));

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            36 /* macro_args ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

    yyres = Val::Nil;

} };
 YYMinorType::YY2(yyres)
}
            ,
            37 /* macro_args ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY118(yy0),) => {

    yyres = list::singleton(Val::id(yy0));

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            38 /* macro_args ::= ID COMMA macro_args */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY118(yy0),YYMinorType::YY2(yy2),) => {

    yyres = list::cons(Val::id(yy0), yy2);

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            39 /* expr ::= ID LPAREN RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY118(yy0),) => {

	verbose_out!("zero param function call!");
	yyres = sexpr::call(yy0, Val::Nil);

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            40 /* expr ::= ID LPAREN expr RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY118(yy0),YYMinorType::YY2(yy2),) => {

	verbose_out!("one param function call!");
	let args = list::singleton(yy2);
	yyres = sexpr::call(yy0, args);

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            41 /* expr ::= ID LPAREN tuple_args RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY118(yy0),YYMinorType::YY2(yy2),) => {

	verbose_out!("multi param function call!");
	yyres = sexpr::call(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            42 /* expr ::= term ID term */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY2(yy0),YYMinorType::YY118(yy1),YYMinorType::YY2(yy2),) => {

	yyres = sexpr::binaryop(yy1, yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            43 /* expr ::= term DOLLAR term */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY2(yy0),YYMinorType::YY2(yy2),) => {

	/* yyres = Val::binaryop(yy0, yy2, D); */
	yyres = Val::Void;

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            44 /* expr ::= CASE NEWLINE cases SLASH */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp2.minor,) {
 (YYMinorType::YY2(yy2),) => {

    verbose_out!("parsed case expr\n");
	yyres = yy2;

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            45 /* cases ::= PIPE expr midblock PIPE ELSE midblock */
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
 (YYMinorType::YY2(yy1),YYMinorType::YY2(yy2),YYMinorType::YY2(yy5),) => {

    verbose_out!("found cases base\n");
    yyres = sexpr::casex(yy1, yy2, yy5);

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            46 /* cases ::= PIPE expr midblock cases */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,yyp3.minor,) {
 (YYMinorType::YY2(yy1),YYMinorType::YY2(yy2),YYMinorType::YY2(yy3),) => {

    verbose_out!("found extra case\n");
    yyres = sexpr::casex(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            49 /* expr ::= NOT expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY2(yy1),) => {

	yyres = sexpr::call("bool_not".to_string(), list::singleton(yy1));

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            50 /* expr ::= expr ConcatNewline */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY2(yy0),) => {

	let newline = Val::Str(Arc::new("\n".to_string()));
	let args = list::cons(yy0, list::singleton(newline));
	yyres = sexpr::new(SexprType::StrExpr, args)

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            51 /* expr ::= MINUS expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY2(yy1),) => {

	yyres = sexpr::call("negate".to_string(), list::singleton(yy1));

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            52 /* expr ::= expr error expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY2(yy0),YYMinorType::YY2(yy2),) => {

    write!(stderr(), "binaryop error: {:?} err {:?}\n", yy0, yy2).ok();
    yyres = Val::Void;

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            53 /* expr ::= expr PLUS expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY2(yy0),YYMinorType::YY2(yy2),) => {

	yyres = sexpr::binaryop("int_add".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            54 /* expr ::= expr PLUS error */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY2(yy0),) => {

    write!(stderr(), "wtf PLUS error: {:?} + error\n", yy0).ok();
    yyres = Val::Void;

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            55 /* expr ::= expr MINUS expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY2(yy0),YYMinorType::YY2(yy2),) => {

	yyres = sexpr::binaryop("int_sub".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            56 /* expr ::= expr TIMES expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY2(yy0),YYMinorType::YY2(yy2),) => {

	yyres = sexpr::binaryop("int_mult".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            57 /* expr ::= expr SLASH expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY2(yy0),YYMinorType::YY2(yy2),) => {

	yyres = sexpr::binaryop("int_div".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            58 /* expr ::= expr MOD expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY2(yy0),YYMinorType::YY2(yy2),) => {

	yyres = sexpr::binaryop("int_mod".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            59 /* expr ::= expr AND expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY2(yy0),YYMinorType::YY2(yy2),) => {

	yyres = sexpr::binaryop("and".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            60 /* expr ::= expr OR expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY2(yy0),YYMinorType::YY2(yy2),) => {

	yyres = sexpr::binaryop("or".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            61 /* expr ::= expr XOR expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY2(yy0),YYMinorType::YY2(yy2),) => {

	yyres = sexpr::binaryop("xor".to_string(),yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            62 /* expr ::= expr LT expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY2(yy0),YYMinorType::YY2(yy2),) => {

	yyres = sexpr::binaryop("less_than".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            63 /* expr ::= expr LTEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY2(yy0),YYMinorType::YY2(yy2),) => {

	yyres = sexpr::binaryop("less_than_equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            64 /* expr ::= expr GT expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY2(yy0),YYMinorType::YY2(yy2),) => {

	yyres = sexpr::binaryop("greater_than".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            65 /* expr ::= expr GTEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY2(yy0),YYMinorType::YY2(yy2),) => {

	yyres = sexpr::binaryop("greater_than_equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            66 /* expr ::= expr EQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY2(yy0),YYMinorType::YY2(yy2),) => {

	yyres = sexpr::binaryop("equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            67 /* expr ::= expr NEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY2(yy0),YYMinorType::YY2(yy2),) => {

	let eq = sexpr::binaryop("equal".to_string(), yy0, yy2);
	yyres = sexpr::call("bool_not".to_string(), list::singleton(eq));

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            69 /* term ::= LPAREN expr RPAREN */
          | 78 /* list ::= SquareL list_items SquareR */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY2(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            70 /* term ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY118(yy0),) => {
 yyres = Val::id(yy0); 
},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            71 /* term ::= VOID */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Val::Void;

} };
 YYMinorType::YY2(yyres)
}
            ,
            72 /* term ::= DollarQuestion */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Val::id("$".to_string());

} };
 YYMinorType::YY2(yyres)
}
            ,
            73 /* term ::= INT */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY74(yy0),) => {

	yyres = Val::Int(yy0);

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            74 /* term ::= True */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Bool(true); 
} };
 YYMinorType::YY2(yyres)
}
            ,
            75 /* term ::= False */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Bool(false); 
} };
 YYMinorType::YY2(yyres)
}
            ,
            76 /* term ::= HASHTAG */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY123(yy0),) => {

	yyres = Val::Hashtag(Arc::new(yy0.data));

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            80 /* list_items ::= expr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY2(yy0),) => {

	yyres = list::singleton(yy0);

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            81 /* list_items ::= expr COMMA list_items */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY2(yy0),YYMinorType::YY2(yy2),) => {

	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            82 /* tuple ::= LPAREN tuple_args RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY2(yy1),) => {

	yyres = Val::tuple_from_list(yy1);

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            83 /* tuple_args ::= expr COMMA expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY2(yy0),YYMinorType::YY2(yy2),) => {

	verbose_out!("base tuple args!");
	yyres = list::cons(yy0, list::singleton(yy2));

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            84 /* tuple_args ::= expr COMMA tuple_args */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY2(yy0),YYMinorType::YY2(yy2),) => {

	verbose_out!("additional tuple arg!");
	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            85 /* strexpr ::= StrOpen strlist StrClose */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY2(yy1),) => {

	yyres = sexpr::strexpr(yy1);

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            86 /* strlist ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

	yyres = Val::Nil;

} };
 YYMinorType::YY2(yyres)
}
            ,
            87 /* strlist ::= StrLit strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY118(yy0),YYMinorType::YY2(yy1),) => {

	yyres = list::cons(Val::new_str(yy0), yy1);

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
}
            ,
            88 /* strlist ::= ID strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY118(yy0),YYMinorType::YY2(yy1),) => {

	yyres = list::cons(Val::id(yy0), yy1);

},    _ => unreachable!() };
 YYMinorType::YY2(yyres)
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

