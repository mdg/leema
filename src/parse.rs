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
const YYNOCODE: i32 = 93;
type YYACTIONTYPE = u8;
const YYWILDCARD: YYCODETYPE = 1;
enum YYMinorType {
    YY0,
    YY13(Ast),
    YY80(Val),
    YY100(String),
    YY104(TokenLoc),
    YY115(Type),
    YY141(TokenData<String>),
    YY148(i64),
}
const YYNSTATE: i32 = 160;
const YYNRULE: i32 = 88;
const YYERRORSYMBOL: i32 = 59;

//const YY_NO_ACTION: i32 = YYNSTATE+YYNRULE+2;
//const YY_ACCEPT_ACTION: i32 = YYNSTATE+YYNRULE+1;
//const YY_ERROR_ACTION: i32 = YYNSTATE+YYNRULE+1;

/* TMPL: action tables */

#[derive(Debug,PartialEq
)]
pub enum Token {
    EOI, //0
    ANY( TokenLoc ), //1
    COMMA( TokenLoc ), //2
    HASHTAG( TokenData<String> ), //3
    ID( String ), //4
    INT( i64 ), //5
    NEWLINE( TokenLoc ), //6
    PLUS( TokenLoc ), //7
    StrLit( String ), //8
    TYPE_ID( String ), //9
    ASSIGN, //10
    BLOCKARROW, //11
    WHERE, //12
    GIVEN, //13
    OR, //14
    XOR, //15
    AND, //16
    ConcatNewline, //17
    NOT, //18
    LT, //19
    LTEQ, //20
    EQ, //21
    NEQ, //22
    GT, //23
    GTEQ, //24
    MINUS, //25
    TIMES, //26
    DIVIDE, //27
    LPAREN, //28
    RPAREN, //29
    FAILED, //30
    WITH, //31
    CONTEXTID, //32
    DT, //33
    IF, //34
    FAIL, //35
    Let, //36
    Fork, //37
    DollarGT, //38
    CurlyL, //39
    CurlyR, //40
    Func, //41
    COLON, //42
    TYPE_INT, //43
    TYPE_STR, //44
    TYPE_BOOL, //45
    TYPE_VOID, //46
    MACRO, //47
    DOLLAR, //48
    ELSE, //49
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
pub const TOKEN_HASHTAG: i32 = 3;
pub const TOKEN_ID: i32 = 4;
pub const TOKEN_INT: i32 = 5;
pub const TOKEN_NEWLINE: i32 = 6;
pub const TOKEN_PLUS: i32 = 7;
pub const TOKEN_StrLit: i32 = 8;
pub const TOKEN_TYPE_ID: i32 = 9;
pub const TOKEN_ASSIGN: i32 = 10;
pub const TOKEN_BLOCKARROW: i32 = 11;
pub const TOKEN_WHERE: i32 = 12;
pub const TOKEN_GIVEN: i32 = 13;
pub const TOKEN_OR: i32 = 14;
pub const TOKEN_XOR: i32 = 15;
pub const TOKEN_AND: i32 = 16;
pub const TOKEN_ConcatNewline: i32 = 17;
pub const TOKEN_NOT: i32 = 18;
pub const TOKEN_LT: i32 = 19;
pub const TOKEN_LTEQ: i32 = 20;
pub const TOKEN_EQ: i32 = 21;
pub const TOKEN_NEQ: i32 = 22;
pub const TOKEN_GT: i32 = 23;
pub const TOKEN_GTEQ: i32 = 24;
pub const TOKEN_MINUS: i32 = 25;
pub const TOKEN_TIMES: i32 = 26;
pub const TOKEN_DIVIDE: i32 = 27;
pub const TOKEN_LPAREN: i32 = 28;
pub const TOKEN_RPAREN: i32 = 29;
pub const TOKEN_FAILED: i32 = 30;
pub const TOKEN_WITH: i32 = 31;
pub const TOKEN_CONTEXTID: i32 = 32;
pub const TOKEN_DT: i32 = 33;
pub const TOKEN_IF: i32 = 34;
pub const TOKEN_FAIL: i32 = 35;
pub const TOKEN_Let: i32 = 36;
pub const TOKEN_Fork: i32 = 37;
pub const TOKEN_DollarGT: i32 = 38;
pub const TOKEN_CurlyL: i32 = 39;
pub const TOKEN_CurlyR: i32 = 40;
pub const TOKEN_Func: i32 = 41;
pub const TOKEN_COLON: i32 = 42;
pub const TOKEN_TYPE_INT: i32 = 43;
pub const TOKEN_TYPE_STR: i32 = 44;
pub const TOKEN_TYPE_BOOL: i32 = 45;
pub const TOKEN_TYPE_VOID: i32 = 46;
pub const TOKEN_MACRO: i32 = 47;
pub const TOKEN_DOLLAR: i32 = 48;
pub const TOKEN_ELSE: i32 = 49;
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
        &Token::ANY(_) => TOKEN_ANY,
        &Token::COMMA(_) => TOKEN_COMMA,
        &Token::HASHTAG(_) => TOKEN_HASHTAG,
        &Token::ID(_) => TOKEN_ID,
        &Token::INT(_) => TOKEN_INT,
        &Token::NEWLINE(_) => TOKEN_NEWLINE,
        &Token::PLUS(_) => TOKEN_PLUS,
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
        &Token::DIVIDE => TOKEN_DIVIDE,
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
        &Token::CurlyL => TOKEN_CurlyL,
        &Token::CurlyR => TOKEN_CurlyR,
        &Token::Func => TOKEN_Func,
        &Token::COLON => TOKEN_COLON,
        &Token::TYPE_INT => TOKEN_TYPE_INT,
        &Token::TYPE_STR => TOKEN_TYPE_STR,
        &Token::TYPE_BOOL => TOKEN_TYPE_BOOL,
        &Token::TYPE_VOID => TOKEN_TYPE_VOID,
        &Token::MACRO => TOKEN_MACRO,
        &Token::DOLLAR => TOKEN_DOLLAR,
        &Token::ELSE => TOKEN_ELSE,
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
        Token::ANY(x) => YYMinorType::YY104(x),
        Token::COMMA(x) => YYMinorType::YY104(x),
        Token::HASHTAG(x) => YYMinorType::YY141(x),
        Token::ID(x) => YYMinorType::YY100(x),
        Token::INT(x) => YYMinorType::YY148(x),
        Token::NEWLINE(x) => YYMinorType::YY104(x),
        Token::PLUS(x) => YYMinorType::YY104(x),
        Token::StrLit(x) => YYMinorType::YY100(x),
        Token::TYPE_ID(x) => YYMinorType::YY100(x),
        _ => YYMinorType::YY0
  }
}
const YY_ACTTAB_COUNT: i32 = 738;
const YY_ACTION: [YYACTIONTYPE; 738] = [
 /*     0 */   141,  100,  144,   40,   82,   38,  121,  123,  122,  135,
 /*    10 */   106,   14,  134,  160,  133,   32,  140,   30,   29,  113,
 /*    20 */   123,  122,   18,    2,    6,   10,  105,  107,    4,  102,
 /*    30 */   155,   13,  101,   94,   92,   15,   68,  161,   79,  102,
 /*    40 */    69,   28,   71,   41,   86,    3,  152,   39,  146,  145,
 /*    50 */   143,  142,    8,  148,   70,  141,  100,  144,  140,   82,
 /*    60 */    37,  136,    5,   33,  135,  140,   33,  134,  139,  133,
 /*    70 */    32,  140,  103,  140,  130,   98,  128,   18,  126,   93,
 /*    80 */    10,  115,   91,   17,   16,  155,   13,  101,   94,   92,
 /*    90 */    15,  125,   78,   79,  141,  147,  144,   77,   75,   86,
 /*   100 */    35,   85,   74,  146,  145,  143,  142,    8,    9,   70,
 /*   110 */    73,   89,   33,   11,  124,  119,  118,  117,  116,   34,
 /*   120 */    26,   25,   27,  149,   76,   24,   23,   20,   19,   22,
 /*   130 */    21,   31,   30,   29,   28,  150,   88,   36,   87,   82,
 /*   140 */    42,   64,  146,  145,  143,  142,   83,  134,   70,  133,
 /*   150 */    96,  140,  114,   84,  112,  111,   28,    1,   99,    9,
 /*   160 */   138,  137,  120,  250,   11,  250,  250,  250,  250,  250,
 /*   170 */   250,   26,   25,   27,  149,  250,   24,   23,   20,   19,
 /*   180 */    22,   21,   31,   30,   29,  250,  132,  159,  250,  250,
 /*   190 */    72,  250,  250,  250,  250,  250,  154,  250,  250,  153,
 /*   200 */   250,  158,  157,  156,   82,   49,  250,   28,  250,  250,
 /*   210 */     9,  250,  134,  250,  133,   11,  140,  250,  250,  250,
 /*   220 */   250,  250,   26,   25,   27,  149,  250,   24,   23,   20,
 /*   230 */    19,   22,   21,   31,   30,   29,  250,  249,  104,  250,
 /*   240 */   250,   72,  250,  250,  250,  250,  250,  154,  250,  250,
 /*   250 */   153,  250,  158,  157,  156,   82,   49,  250,   28,  250,
 /*   260 */   250,    7,  250,  134,  250,  133,   11,  140,  250,  250,
 /*   270 */   250,  250,  250,   26,   25,   27,  149,  250,   24,   23,
 /*   280 */    20,   19,   22,   21,   31,   30,   29,  250,   11,  250,
 /*   290 */   250,  250,  127,  250,  250,   26,   25,   27,  149,  250,
 /*   300 */    24,   23,   20,   19,   22,   21,   31,   30,   29,   28,
 /*   310 */   132,   82,   65,  250,   82,   46,  250,  250,   11,  134,
 /*   320 */   250,  133,  134,  140,  133,  129,  140,  250,  250,  250,
 /*   330 */   250,   28,   20,   19,   22,   21,   31,   30,   29,   11,
 /*   340 */   250,  250,  250,  250,  250,  250,   26,   25,   27,  149,
 /*   350 */   250,   24,   23,   20,   19,   22,   21,   31,   30,   29,
 /*   360 */    11,   28,  250,  250,  250,  250,  250,   26,   25,   27,
 /*   370 */   149,  102,   24,   23,   20,   19,   22,   21,   31,   30,
 /*   380 */    29,  250,   28,  141,  100,  144,  250,   82,   45,  250,
 /*   390 */   250,  250,  250,  250,  250,  134,   97,  133,   32,  140,
 /*   400 */   250,  250,  250,   28,  250,   18,   11,  250,   10,  151,
 /*   410 */   250,  250,  250,  250,   12,   27,  149,  250,   24,   23,
 /*   420 */    20,   19,   22,   21,   31,   30,   29,  250,  141,  100,
 /*   430 */   144,  146,  145,  143,  142,    8,  250,   70,  250,  250,
 /*   440 */   250,  250,  250,   32,  250,  250,   82,   45,  250,   28,
 /*   450 */    18,   11,  250,   10,  134,  131,  133,  250,  140,   12,
 /*   460 */   250,  149,  250,   24,   23,   20,   19,   22,   21,   31,
 /*   470 */    30,   29,  250,  250,  250,  250,  146,  145,  143,  142,
 /*   480 */     8,   90,   70,  250,   72,  250,  250,  250,  250,  250,
 /*   490 */   154,  250,  250,  153,   28,  158,  157,  156,   82,   49,
 /*   500 */   250,  250,  250,  250,  250,  250,  134,  250,  133,  250,
 /*   510 */   140,  109,  250,  250,   72,  250,  250,  250,  250,  250,
 /*   520 */   154,  250,  250,  153,  250,  158,  157,  156,   82,   49,
 /*   530 */   108,  250,  250,   72,  250,  250,  134,  250,  133,  154,
 /*   540 */   140,  250,  153,  250,  158,  157,  156,   82,   49,  250,
 /*   550 */    11,  250,  250,  250,  250,  134,  250,  133,  250,  140,
 /*   560 */   250,  250,  250,  250,  248,  248,  248,  248,   31,   30,
 /*   570 */    29,  250,   82,   37,  250,   82,   43,  110,  250,  250,
 /*   580 */   134,  250,  133,  134,  140,  133,   95,  140,  250,   82,
 /*   590 */    44,  250,  250,   28,  250,  250,  250,  134,  250,  133,
 /*   600 */   250,  140,   82,   52,  250,   82,   57,  250,   82,   67,
 /*   610 */   134,  250,  133,  134,  140,  133,  134,  140,  133,  250,
 /*   620 */   140,  250,  250,   82,   81,  250,   82,   80,  250,   82,
 /*   630 */    53,  134,  250,  133,  134,  140,  133,  134,  140,  133,
 /*   640 */   250,  140,  250,   82,   56,  250,   82,   55,  250,   82,
 /*   650 */    54,  134,  250,  133,  134,  140,  133,  134,  140,  133,
 /*   660 */   250,  140,   82,   63,  250,   82,   62,  250,   82,   61,
 /*   670 */   134,  250,  133,  134,  140,  133,  134,  140,  133,  250,
 /*   680 */   140,   82,   60,  250,   82,   59,  250,   82,   58,  134,
 /*   690 */   250,  133,  134,  140,  133,  134,  140,  133,  250,  140,
 /*   700 */   250,  250,   82,   66,  250,   82,   51,  250,   82,   50,
 /*   710 */   134,  250,  133,  134,  140,  133,  134,  140,  133,  250,
 /*   720 */   140,  250,   82,   48,  250,   82,   47,  250,  250,  250,
 /*   730 */   134,  250,  133,  134,  140,  133,  250,  140,
];
const YY_LOOKAHEAD: [YYCODETYPE; 738] = [
 /*     0 */     3,    4,    5,    4,   78,   79,   66,   67,   68,   83,
 /*    10 */    31,   11,   86,    0,   88,   18,   90,   26,   27,   66,
 /*    20 */    67,   68,   25,    1,   34,   28,   32,   30,    6,   39,
 /*    30 */    33,   34,   35,   36,   37,   38,    4,    0,   41,   39,
 /*    40 */     8,   50,   49,    3,   47,    6,   78,   48,   51,   52,
 /*    50 */    53,   54,   55,   78,   57,    3,    4,    5,   90,   78,
 /*    60 */    79,   78,   28,   59,   83,   90,   59,   86,   58,   88,
 /*    70 */    18,   90,   68,   90,   56,   68,   29,   25,   29,    4,
 /*    80 */    28,    9,    4,   21,   21,   33,   34,   35,   36,   37,
 /*    90 */    38,   40,   28,   41,    3,    4,    5,   29,    2,   47,
 /*   100 */    29,    4,   28,   51,   52,   53,   54,   55,    2,   57,
 /*   110 */     2,    4,   59,    7,   72,   43,   44,   45,   46,   28,
 /*   120 */    14,   15,   16,   17,    4,   19,   20,   21,   22,   23,
 /*   130 */    24,   25,   26,   27,   50,   29,   71,   81,   81,   78,
 /*   140 */    79,   42,   51,   52,   53,   54,    4,   86,   57,   88,
 /*   150 */    89,   90,   71,   74,   74,   68,   50,   59,   91,    2,
 /*   160 */    91,   91,   80,   92,    7,   92,   92,   92,   92,   92,
 /*   170 */    92,   14,   15,   16,   17,   92,   19,   20,   21,   22,
 /*   180 */    23,   24,   25,   26,   27,   92,   29,   61,   92,   92,
 /*   190 */    64,   92,   92,   92,   92,   92,   70,   92,   92,   73,
 /*   200 */    92,   75,   76,   77,   78,   79,   92,   50,   92,   92,
 /*   210 */     2,   92,   86,   92,   88,    7,   90,   92,   92,   92,
 /*   220 */    92,   92,   14,   15,   16,   17,   92,   19,   20,   21,
 /*   230 */    22,   23,   24,   25,   26,   27,   92,   60,   61,   92,
 /*   240 */    92,   64,   92,   92,   92,   92,   92,   70,   92,   92,
 /*   250 */    73,   92,   75,   76,   77,   78,   79,   92,   50,   92,
 /*   260 */    92,    2,   92,   86,   92,   88,    7,   90,   92,   92,
 /*   270 */    92,   92,   92,   14,   15,   16,   17,   92,   19,   20,
 /*   280 */    21,   22,   23,   24,   25,   26,   27,   92,    7,   92,
 /*   290 */    92,   92,   59,   92,   92,   14,   15,   16,   17,   92,
 /*   300 */    19,   20,   21,   22,   23,   24,   25,   26,   27,   50,
 /*   310 */    29,   78,   79,   92,   78,   79,   92,   92,    7,   86,
 /*   320 */    92,   88,   86,   90,   88,   89,   90,   92,   92,   92,
 /*   330 */    92,   50,   21,   22,   23,   24,   25,   26,   27,    7,
 /*   340 */    92,   92,   92,   92,   92,   92,   14,   15,   16,   17,
 /*   350 */    92,   19,   20,   21,   22,   23,   24,   25,   26,   27,
 /*   360 */     7,   50,   92,   92,   92,   92,   92,   14,   15,   16,
 /*   370 */    17,   39,   19,   20,   21,   22,   23,   24,   25,   26,
 /*   380 */    27,   92,   50,    3,    4,    5,   92,   78,   79,   92,
 /*   390 */    92,   92,   92,   92,   92,   86,   87,   88,   18,   90,
 /*   400 */    92,   92,   92,   50,   92,   25,    7,   92,   28,   29,
 /*   410 */    92,   92,   92,   92,   34,   16,   17,   92,   19,   20,
 /*   420 */    21,   22,   23,   24,   25,   26,   27,   92,    3,    4,
 /*   430 */     5,   51,   52,   53,   54,   55,   92,   57,   92,   92,
 /*   440 */    92,   92,   92,   18,   92,   92,   78,   79,   92,   50,
 /*   450 */    25,    7,   92,   28,   86,   87,   88,   92,   90,   34,
 /*   460 */    92,   17,   92,   19,   20,   21,   22,   23,   24,   25,
 /*   470 */    26,   27,   92,   92,   92,   92,   51,   52,   53,   54,
 /*   480 */    55,   61,   57,   92,   64,   92,   92,   92,   92,   92,
 /*   490 */    70,   92,   92,   73,   50,   75,   76,   77,   78,   79,
 /*   500 */    92,   92,   92,   92,   92,   92,   86,   92,   88,   92,
 /*   510 */    90,   61,   92,   92,   64,   92,   92,   92,   92,   92,
 /*   520 */    70,   92,   92,   73,   92,   75,   76,   77,   78,   79,
 /*   530 */    61,   92,   92,   64,   92,   92,   86,   92,   88,   70,
 /*   540 */    90,   92,   73,   92,   75,   76,   77,   78,   79,   92,
 /*   550 */     7,   92,   92,   92,   92,   86,   92,   88,   92,   90,
 /*   560 */    92,   92,   92,   92,   21,   22,   23,   24,   25,   26,
 /*   570 */    27,   92,   78,   79,   92,   78,   79,   83,   92,   92,
 /*   580 */    86,   92,   88,   86,   90,   88,   89,   90,   92,   78,
 /*   590 */    79,   92,   92,   50,   92,   92,   92,   86,   92,   88,
 /*   600 */    92,   90,   78,   79,   92,   78,   79,   92,   78,   79,
 /*   610 */    86,   92,   88,   86,   90,   88,   86,   90,   88,   92,
 /*   620 */    90,   92,   92,   78,   79,   92,   78,   79,   92,   78,
 /*   630 */    79,   86,   92,   88,   86,   90,   88,   86,   90,   88,
 /*   640 */    92,   90,   92,   78,   79,   92,   78,   79,   92,   78,
 /*   650 */    79,   86,   92,   88,   86,   90,   88,   86,   90,   88,
 /*   660 */    92,   90,   78,   79,   92,   78,   79,   92,   78,   79,
 /*   670 */    86,   92,   88,   86,   90,   88,   86,   90,   88,   92,
 /*   680 */    90,   78,   79,   92,   78,   79,   92,   78,   79,   86,
 /*   690 */    92,   88,   86,   90,   88,   86,   90,   88,   92,   90,
 /*   700 */    92,   92,   78,   79,   92,   78,   79,   92,   78,   79,
 /*   710 */    86,   92,   88,   86,   90,   88,   86,   90,   88,   92,
 /*   720 */    90,   92,   78,   79,   92,   78,   79,   92,   92,   92,
 /*   730 */    86,   92,   88,   86,   90,   88,   92,   90,
];
const YY_SHIFT_USE_DFLT: i32 = -22;
const YY_SHIFT_COUNT: i32 = 107;
const YY_SHIFT_MIN: i32 = -21;
const YY_SHIFT_MAX: i32 = 543;
const YY_SHIFT_OFST: [i16; 108] = [
 /*     0 */    -3,   52,   52,   52,   52,  380,  425,  425,  425,  425,
 /*    10 */   425,  425,  425,  425,  425,  425,  425,  425,  425,  425,
 /*    20 */   425,  425,  425,  425,  425,  425,  425,  425,  425,  425,
 /*    30 */   425,  425,  425,  425,  425,    0,    0,  332,  332,   91,
 /*    40 */    91,   91,  157,  106,  281,  259,  208,  353,  353,  353,
 /*    50 */   353,  353,  353,  353,  399,  399,  444,  444,  543,  543,
 /*    60 */   543,  543,  311,  311,   72,   -9,   -9,   -9,   32,   32,
 /*    70 */    32,  -10,   22,  142,  142,  120,   99,   99,  120,  107,
 /*    80 */    84,   84,   -1,  108,   71,   74,   97,   96,   68,   64,
 /*    90 */    51,   63,   78,   62,   75,   49,   47,   18,   -7,   10,
 /*   100 */    34,   40,   39,   -7,   37,   13,   -6,  -21,
];
const YY_REDUCE_USE_DFLT: i32 = -75;
const YY_REDUCE_COUNT: i32 = 81;
const YY_REDUCE_MIN: i32 = -74;
const YY_REDUCE_MAX: i32 = 647;
const YY_REDUCE_OFST: [i16; 82] = [
 /*     0 */   177,  469,  450,  420,  126,  497,  494,  368,  309,  236,
 /*    10 */    61,  233,  -19,  -74,  647,  644,  630,  627,  624,  609,
 /*    20 */   606,  603,  590,  587,  584,  571,  568,  565,  551,  548,
 /*    30 */   545,  530,  527,  524,  511,  -47,  -60,    7,    4,  -17,
 /*    40 */   -25,  -32,   53,   53,   53,   53,   53,   53,   53,   53,
 /*    50 */    53,   53,   53,   53,   53,   53,   53,   53,   53,   53,
 /*    60 */    53,   53,   53,   53,   82,   53,   53,   53,   70,   69,
 /*    70 */    67,   87,   98,   80,   79,   81,   57,   56,   65,   42,
 /*    80 */    53,   53,
];
const YY_DEFAULT: [YYACTIONTYPE; 160] = [
 /*     0 */   162,  162,  162,  162,  162,  248,  248,  238,  238,  248,
 /*    10 */   248,  248,  248,  248,  248,  248,  248,  248,  248,  248,
 /*    20 */   248,  248,  248,  248,  248,  248,  248,  248,  248,  248,
 /*    30 */   248,  248,  248,  248,  248,  248,  248,  248,  248,  248,
 /*    40 */   248,  248,  248,  248,  248,  239,  242,  180,  177,  176,
 /*    50 */   175,  174,  211,  217,  220,  219,  218,  208,  226,  225,
 /*    60 */   224,  223,  222,  221,  248,  212,  210,  214,  245,  245,
 /*    70 */   245,  248,  248,  195,  195,  184,  187,  187,  184,  248,
 /*    80 */   216,  215,  227,  196,  248,  248,  248,  185,  248,  248,
 /*    90 */   248,  248,  248,  248,  248,  248,  248,  248,  248,  248,
 /*   100 */   229,  248,  248,  172,  248,  248,  248,  248,  165,  164,
 /*   110 */   205,  204,  197,  194,  186,  193,  192,  191,  190,  189,
 /*   120 */   188,  183,  179,  178,  182,  181,  200,  213,  241,  243,
 /*   130 */   237,  240,  228,  207,  206,  203,  202,  247,  246,  244,
 /*   140 */   236,  235,  234,  233,  232,  231,  230,  229,  201,  209,
 /*   150 */   199,  198,  173,  171,  170,  169,  168,  167,  166,  163,
];

/* TMPL: fallback tokens */

const YY_FALLBACK: [i32; 0] = [
];

/* TMPL: symbol names */


/* TMPL: rules */


/* TMPL: destructors */


/* TMPL: stack-overflow */


/* TMPL: stack-overflow */

const YY_RULE_INFO: [YYCODETYPE; 88] = [
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
  77,
  75,
  75,
  76,
  76,
  66,
  66,
  67,
  68,
  70,
  72,
  71,
  71,
  71,
  81,
  81,
  80,
  80,
  80,
  80,
  80,
  73,
  74,
  74,
  74,
  79,
  79,
  79,
  79,
  79,
  79,
  83,
  83,
  79,
  79,
  79,
  79,
  79,
  79,
  79,
  79,
  79,
  79,
  79,
  79,
  79,
  79,
  79,
  79,
  79,
  79,
  79,
  79,
  79,
  79,
  78,
  78,
  78,
  78,
  78,
  78,
  78,
  78,
  78,
  86,
  87,
  87,
  87,
  88,
  89,
  89,
  90,
  91,
  91,
  91,
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
 YYMinorType::YY13(yyres)
}
            ,
            1 /* program ::= stmts */
            => 
{
let yyres :  Ast ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY80(yy0),) => {

	if list::is_empty(&yy0) {
		panic!("null program");
	}
	// ignore yyres, it doesn't really go anywhere for program
	yyres = Ast::Nothing;
	// we're done, so put yy0 in extra
	self.extra = Ok(Ast::ReplRoot(yy0));

},    _ => unreachable!() };
 YYMinorType::YY13(yyres)
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
 YYMinorType::YY80(yyres)
}
            ,
            3 /* stmts ::= stmt NEWLINE stmts */
          | 80 /* list_items ::= expr COMMA list_items */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY80(yy0),YYMinorType::YY80(yy2),) => {

	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            4 /* stmts ::= stmt ANY stmts */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY104(yy1),) => {

	yyres = Val::Void;

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
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

	yyres = Val::Void;

} };
 YYMinorType::YY80(yyres)
}
            ,
            6 /* stmt ::= let_stmt */
          | 8 /* stmt ::= fail_stmt */
          | 10 /* stmt ::= func_stmt */
          | 11 /* stmt ::= macro_stmt */
          | 46 /* expr ::= list */
          | 47 /* expr ::= tuple */
          | 67 /* expr ::= term */
          | 76 /* term ::= strexpr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY80(yy0),) => {
 yyres = yy0; 
},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            7 /* stmt ::= expr_stmt */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY80(yy0),) => {

    yyres = yy0;

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
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
 YYMinorType::YY80(yyres)
}
            ,
            12 /* stmt ::= IF expr curly_block */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY80(yy1),YYMinorType::YY80(yy2),) => {

    yyres = sexpr::ifexpr(yy1, yy2, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
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
 (YYMinorType::YY141(yy1),YYMinorType::YY80(yy2),) => {

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
 YYMinorType::YY80(yyres)
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
 (YYMinorType::YY100(yy1),YYMinorType::YY80(yy3),) => {

	let letx =
        list::cons(Val::id(yy1),
        list::cons(yy3,
        Val::Nil
        ));
	yyres = sexpr::new(SexprType::Let, letx);

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
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
 (YYMinorType::YY100(yy1),YYMinorType::YY80(yy3),) => {

	let bind = list::cons(Val::new_str(yy1), list::singleton(yy3));
	yyres = sexpr::new(SexprType::Fork, bind);

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            16 /* expr_stmt ::= expr */
          | 18 /* block ::= arrow_block */
          | 19 /* block ::= curly_block */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY80(yy0),) => {

	yyres = yy0;

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            17 /* expr_stmt ::= DollarGT expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY80(yy1),) => {
 yyres = yy1; 
},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            20 /* arrow_block ::= BLOCKARROW expr */
          | 43 /* expr ::= IF if_expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY80(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            21 /* curly_block ::= CurlyL NEWLINE stmts CurlyR */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp2.minor,) {
 (YYMinorType::YY80(yy2),) => {

	yyres = yy2;

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            22 /* func_stmt ::= Func dfunc_1 */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY80(yy1),) => {

	/*
	const Val *plist = Val::list(p);
	const Val *mcase = Val::tuple2(plist, C);
	const list *mblk = list::singleton(mcase);

	Val *func = Val::fexpr(mblk, T);
	yyres = Val::tuple2(yy1, func, LET_ASSIGN);
	*/
	yyres = Val::Sexpr(SexprType::DefFunc, Box::new(yy1));

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            23 /* dfunc_1 ::= ID LPAREN dfunc_args RPAREN opt_typex block */
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
 (YYMinorType::YY100(yy0),YYMinorType::YY80(yy2),YYMinorType::YY115(yy4),YYMinorType::YY80(yy5),) => {

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
 YYMinorType::YY80(yyres)
}
            ,
            24 /* dfunc_args ::= */
          | 78 /* list_items ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

	yyres = list::empty();

} };
 YYMinorType::YY80(yyres)
}
            ,
            25 /* dfunc_args ::= ID opt_typex */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY100(yy0),YYMinorType::YY115(yy1),) => {

	yyres = list::singleton(sexpr::id_with_type(yy0, yy1));

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            26 /* dfunc_args ::= ID opt_typex COMMA dfunc_args */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY100(yy0),YYMinorType::YY115(yy1),YYMinorType::YY80(yy3),) => {

	yyres = list::cons(sexpr::id_with_type(yy0, yy1), yy3);

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            27 /* opt_typex ::= */
            => 
{
let yyres :  Type ;
match () {
 () => {

	yyres = Type::AnonVar;

} };
 YYMinorType::YY115(yyres)
}
            ,
            28 /* opt_typex ::= COLON typex */
            => 
{
let yyres :  Type ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY115(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY115(yyres)
}
            ,
            29 /* typex ::= TYPE_INT */
            => 
{
let yyres :  Type ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Type::Int;

} };
 YYMinorType::YY115(yyres)
}
            ,
            30 /* typex ::= TYPE_STR */
            => 
{
let yyres :  Type ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Type::Str;

} };
 YYMinorType::YY115(yyres)
}
            ,
            31 /* typex ::= TYPE_BOOL */
            => 
{
let yyres :  Type ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Type::Bool;

} };
 YYMinorType::YY115(yyres)
}
            ,
            32 /* typex ::= TYPE_VOID */
            => 
{
let yyres :  Type ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Type::Void;

} };
 YYMinorType::YY115(yyres)
}
            ,
            33 /* typex ::= TYPE_ID */
            => 
{
let yyres :  Type ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY100(yy0),) => {

	yyres = Type::Id(Arc::new(yy0));

},    _ => unreachable!() };
 YYMinorType::YY115(yyres)
}
            ,
            34 /* macro_stmt ::= MACRO ID LPAREN macro_args RPAREN block */
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
 (YYMinorType::YY100(yy1),YYMinorType::YY80(yy3),YYMinorType::YY80(yy5),) => {

    yyres = sexpr::new(SexprType::DefMacro,
        list::cons(Val::id(yy1),
        list::cons(yy3,
        list::cons(yy5,
        Val::Nil
    ))));

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            35 /* macro_args ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

    yyres = Val::Nil;

} };
 YYMinorType::YY80(yyres)
}
            ,
            36 /* macro_args ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY100(yy0),) => {

    yyres = list::singleton(Val::id(yy0));

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            37 /* macro_args ::= ID COMMA macro_args */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY100(yy0),YYMinorType::YY80(yy2),) => {

    yyres = list::cons(Val::id(yy0), yy2);

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            38 /* expr ::= ID LPAREN RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY100(yy0),) => {

	verbose_out!("zero param function call!");
	yyres = sexpr::call(yy0, Val::Nil);

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            39 /* expr ::= ID LPAREN expr RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY100(yy0),YYMinorType::YY80(yy2),) => {

	verbose_out!("one param function call!");
	let args = list::singleton(yy2);
	yyres = sexpr::call(yy0, args);

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            40 /* expr ::= ID LPAREN tuple_args RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY100(yy0),YYMinorType::YY80(yy2),) => {

	verbose_out!("multi param function call!");
	yyres = sexpr::call(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            41 /* expr ::= term ID term */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY80(yy0),YYMinorType::YY100(yy1),YYMinorType::YY80(yy2),) => {

	yyres = sexpr::binaryop(yy1, yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            42 /* expr ::= term DOLLAR term */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY80(yy0),YYMinorType::YY80(yy2),) => {

	/* yyres = Val::binaryop(yy0, yy2, D); */
	yyres = Val::Void;

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            44 /* if_expr ::= expr curly_block ELSE curly_block */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY80(yy0),YYMinorType::YY80(yy1),YYMinorType::YY80(yy3),) => {

	yyres = sexpr::ifexpr(yy0, yy1, yy3);

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            45 /* if_expr ::= expr curly_block ELSE IF if_expr */
            => 
{
let yyres :  Val ;
let yyp4 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,yyp4.minor,) {
 (YYMinorType::YY80(yy0),YYMinorType::YY80(yy1),YYMinorType::YY80(yy4),) => {

	yyres = sexpr::ifexpr(yy0, yy1, yy4);

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            48 /* expr ::= NOT expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY80(yy1),) => {

	yyres = sexpr::call("bool_not".to_string(), list::singleton(yy1));

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            49 /* expr ::= expr ConcatNewline */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY80(yy0),) => {

	let newline = Val::Str(Arc::new("\n".to_string()));
	let args = list::cons(yy0, list::singleton(newline));
	yyres = sexpr::new(SexprType::StrExpr, args)

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            50 /* expr ::= MINUS expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY80(yy1),) => {

	yyres = sexpr::call("negate".to_string(), list::singleton(yy1));

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            51 /* expr ::= expr error expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY80(yy0),YYMinorType::YY80(yy2),) => {

    write!(stderr(), "binaryop error: {:?} err {:?}\n", yy0, yy2).ok();
    yyres = Val::Void;

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            52 /* expr ::= expr PLUS expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY80(yy0),YYMinorType::YY80(yy2),) => {

	yyres = sexpr::binaryop("int_add".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            53 /* expr ::= expr PLUS error */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY80(yy0),) => {

    write!(stderr(), "wtf PLUS error: {:?} + error\n", yy0).ok();
    yyres = Val::Void;

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            54 /* expr ::= expr MINUS expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY80(yy0),YYMinorType::YY80(yy2),) => {

	yyres = sexpr::binaryop("int_sub".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            55 /* expr ::= expr TIMES expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY80(yy0),YYMinorType::YY80(yy2),) => {

	yyres = sexpr::binaryop("int_mult".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            56 /* expr ::= expr DIVIDE expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY80(yy0),YYMinorType::YY80(yy2),) => {

	yyres = sexpr::binaryop("int_div".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            57 /* expr ::= expr MOD expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY80(yy0),YYMinorType::YY80(yy2),) => {

	yyres = sexpr::binaryop("int_mod".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            58 /* expr ::= expr AND expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY80(yy0),YYMinorType::YY80(yy2),) => {

	yyres = sexpr::binaryop("and".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            59 /* expr ::= expr OR expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY80(yy0),YYMinorType::YY80(yy2),) => {

	yyres = sexpr::binaryop("or".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            60 /* expr ::= expr XOR expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY80(yy0),YYMinorType::YY80(yy2),) => {

	yyres = sexpr::binaryop("xor".to_string(),yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            61 /* expr ::= expr LT expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY80(yy0),YYMinorType::YY80(yy2),) => {

	yyres = sexpr::binaryop("less_than".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            62 /* expr ::= expr LTEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY80(yy0),YYMinorType::YY80(yy2),) => {

	yyres = sexpr::binaryop("less_than_equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            63 /* expr ::= expr GT expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY80(yy0),YYMinorType::YY80(yy2),) => {

	yyres = sexpr::binaryop("greater_than".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            64 /* expr ::= expr GTEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY80(yy0),YYMinorType::YY80(yy2),) => {

	yyres = sexpr::binaryop("greater_than_equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            65 /* expr ::= expr EQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY80(yy0),YYMinorType::YY80(yy2),) => {

	yyres = sexpr::binaryop("equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            66 /* expr ::= expr NEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY80(yy0),YYMinorType::YY80(yy2),) => {

	let eq = sexpr::binaryop("equal".to_string(), yy0, yy2);
	yyres = sexpr::call("bool_not".to_string(), list::singleton(eq));

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            68 /* term ::= LPAREN expr RPAREN */
          | 77 /* list ::= SquareL list_items SquareR */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY80(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            69 /* term ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY100(yy0),) => {
 yyres = Val::id(yy0); 
},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            70 /* term ::= VOID */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Val::Void;

} };
 YYMinorType::YY80(yyres)
}
            ,
            71 /* term ::= DollarQuestion */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Val::id("$".to_string());

} };
 YYMinorType::YY80(yyres)
}
            ,
            72 /* term ::= INT */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY148(yy0),) => {

	yyres = Val::Int(yy0);

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            73 /* term ::= True */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Bool(true); 
} };
 YYMinorType::YY80(yyres)
}
            ,
            74 /* term ::= False */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Bool(false); 
} };
 YYMinorType::YY80(yyres)
}
            ,
            75 /* term ::= HASHTAG */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY141(yy0),) => {

	yyres = Val::Hashtag(Arc::new(yy0.data));

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            79 /* list_items ::= expr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY80(yy0),) => {

	yyres = list::singleton(yy0);

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            81 /* tuple ::= LPAREN tuple_args RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY80(yy1),) => {

	yyres = Val::tuple_from_list(yy1);

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            82 /* tuple_args ::= expr COMMA expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY80(yy0),YYMinorType::YY80(yy2),) => {

	verbose_out!("base tuple args!");
	yyres = list::cons(yy0, list::singleton(yy2));

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            83 /* tuple_args ::= expr COMMA tuple_args */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY80(yy0),YYMinorType::YY80(yy2),) => {

	verbose_out!("additional tuple arg!");
	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            84 /* strexpr ::= StrOpen strlist StrClose */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY80(yy1),) => {

	yyres = sexpr::strexpr(yy1);

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            85 /* strlist ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

	yyres = Val::Nil;

} };
 YYMinorType::YY80(yyres)
}
            ,
            86 /* strlist ::= StrLit strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY100(yy0),YYMinorType::YY80(yy1),) => {

	yyres = list::cons(Val::new_str(yy0), yy1);

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
}
            ,
            87 /* strlist ::= ID strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY100(yy0),YYMinorType::YY80(yy1),) => {

	yyres = list::cons(Val::id(yy0), yy1);

},    _ => unreachable!() };
 YYMinorType::YY80(yyres)
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

