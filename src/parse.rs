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
const YYNOCODE: i32 = 108;
type YYACTIONTYPE = u16;
const YYWILDCARD: YYCODETYPE = 1;
enum YYMinorType {
    YY0,
    YY39(Type),
    YY49(Ast),
    YY82(Val),
    YY132(TokenLoc),
    YY158(String),
    YY186(i64),
    YY209(TokenData<String>),
}
const YYNSTATE: i32 = 239;
const YYNRULE: i32 = 126;
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
    COLON( TokenLoc ), //2
    COMMA( TokenLoc ), //3
    DOUBLEDASH( TokenLoc ), //4
    ELSE( TokenLoc ), //5
    HASHTAG( TokenData<String> ), //6
    ID( TokenData<String> ), //7
    INT( i64 ), //8
    PLUS( TokenLoc ), //9
    SEMICOLON( TokenLoc ), //10
    SLASH( TokenLoc ), //11
    SquareL( TokenLoc ), //12
    SquareR( TokenLoc ), //13
    StrLit( String ), //14
    TYPE_ID( TokenData<String> ), //15
    ASSIGN, //16
    BLOCKARROW, //17
    RETURN, //18
    OR, //19
    XOR, //20
    AND, //21
    ConcatNewline, //22
    NOT, //23
    EQ, //24
    NEQ, //25
    GT, //26
    GTEQ, //27
    LT, //28
    LTEQ, //29
    MINUS, //30
    TIMES, //31
    MOD, //32
    DOLLAR, //33
    DOT, //34
    LPAREN, //35
    RPAREN, //36
    STRUCT, //37
    FAIL, //38
    FAILED, //39
    Let, //40
    Fork, //41
    Func, //42
    PARENCALL, //43
    TYPE_INT, //44
    TYPE_STR, //45
    TYPE_HASHTAG, //46
    TYPE_BOOL, //47
    TYPE_VOID, //48
    PS, //49
    MACRO, //50
    IF, //51
    PIPE, //52
    CASE, //53
    MATCH, //54
    True, //55
    False, //56
    UNDERSCORE, //57
    NEGATE, //58
    VOID, //59
    DollarQuestion, //60
    StrOpen, //61
    StrClose, //62
}
pub const TOKEN_EOI: i32 = 0;
pub const TOKEN_ANY: i32 = 1;
pub const TOKEN_COLON: i32 = 2;
pub const TOKEN_COMMA: i32 = 3;
pub const TOKEN_DOUBLEDASH: i32 = 4;
pub const TOKEN_ELSE: i32 = 5;
pub const TOKEN_HASHTAG: i32 = 6;
pub const TOKEN_ID: i32 = 7;
pub const TOKEN_INT: i32 = 8;
pub const TOKEN_PLUS: i32 = 9;
pub const TOKEN_SEMICOLON: i32 = 10;
pub const TOKEN_SLASH: i32 = 11;
pub const TOKEN_SquareL: i32 = 12;
pub const TOKEN_SquareR: i32 = 13;
pub const TOKEN_StrLit: i32 = 14;
pub const TOKEN_TYPE_ID: i32 = 15;
pub const TOKEN_ASSIGN: i32 = 16;
pub const TOKEN_BLOCKARROW: i32 = 17;
pub const TOKEN_RETURN: i32 = 18;
pub const TOKEN_OR: i32 = 19;
pub const TOKEN_XOR: i32 = 20;
pub const TOKEN_AND: i32 = 21;
pub const TOKEN_ConcatNewline: i32 = 22;
pub const TOKEN_NOT: i32 = 23;
pub const TOKEN_EQ: i32 = 24;
pub const TOKEN_NEQ: i32 = 25;
pub const TOKEN_GT: i32 = 26;
pub const TOKEN_GTEQ: i32 = 27;
pub const TOKEN_LT: i32 = 28;
pub const TOKEN_LTEQ: i32 = 29;
pub const TOKEN_MINUS: i32 = 30;
pub const TOKEN_TIMES: i32 = 31;
pub const TOKEN_MOD: i32 = 32;
pub const TOKEN_DOLLAR: i32 = 33;
pub const TOKEN_DOT: i32 = 34;
pub const TOKEN_LPAREN: i32 = 35;
pub const TOKEN_RPAREN: i32 = 36;
pub const TOKEN_STRUCT: i32 = 37;
pub const TOKEN_FAIL: i32 = 38;
pub const TOKEN_FAILED: i32 = 39;
pub const TOKEN_Let: i32 = 40;
pub const TOKEN_Fork: i32 = 41;
pub const TOKEN_Func: i32 = 42;
pub const TOKEN_PARENCALL: i32 = 43;
pub const TOKEN_TYPE_INT: i32 = 44;
pub const TOKEN_TYPE_STR: i32 = 45;
pub const TOKEN_TYPE_HASHTAG: i32 = 46;
pub const TOKEN_TYPE_BOOL: i32 = 47;
pub const TOKEN_TYPE_VOID: i32 = 48;
pub const TOKEN_PS: i32 = 49;
pub const TOKEN_MACRO: i32 = 50;
pub const TOKEN_IF: i32 = 51;
pub const TOKEN_PIPE: i32 = 52;
pub const TOKEN_CASE: i32 = 53;
pub const TOKEN_MATCH: i32 = 54;
pub const TOKEN_True: i32 = 55;
pub const TOKEN_False: i32 = 56;
pub const TOKEN_UNDERSCORE: i32 = 57;
pub const TOKEN_NEGATE: i32 = 58;
pub const TOKEN_VOID: i32 = 59;
pub const TOKEN_DollarQuestion: i32 = 60;
pub const TOKEN_StrOpen: i32 = 61;
pub const TOKEN_StrClose: i32 = 62;
#[inline]
fn token_major(t: &Token) -> i32 {
    match t {
        &Token::EOI => 0,
        &Token::ANY => TOKEN_ANY,
        &Token::COLON(_) => TOKEN_COLON,
        &Token::COMMA(_) => TOKEN_COMMA,
        &Token::DOUBLEDASH(_) => TOKEN_DOUBLEDASH,
        &Token::ELSE(_) => TOKEN_ELSE,
        &Token::HASHTAG(_) => TOKEN_HASHTAG,
        &Token::ID(_) => TOKEN_ID,
        &Token::INT(_) => TOKEN_INT,
        &Token::PLUS(_) => TOKEN_PLUS,
        &Token::SEMICOLON(_) => TOKEN_SEMICOLON,
        &Token::SLASH(_) => TOKEN_SLASH,
        &Token::SquareL(_) => TOKEN_SquareL,
        &Token::SquareR(_) => TOKEN_SquareR,
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
        &Token::STRUCT => TOKEN_STRUCT,
        &Token::FAIL => TOKEN_FAIL,
        &Token::FAILED => TOKEN_FAILED,
        &Token::Let => TOKEN_Let,
        &Token::Fork => TOKEN_Fork,
        &Token::Func => TOKEN_Func,
        &Token::PARENCALL => TOKEN_PARENCALL,
        &Token::TYPE_INT => TOKEN_TYPE_INT,
        &Token::TYPE_STR => TOKEN_TYPE_STR,
        &Token::TYPE_HASHTAG => TOKEN_TYPE_HASHTAG,
        &Token::TYPE_BOOL => TOKEN_TYPE_BOOL,
        &Token::TYPE_VOID => TOKEN_TYPE_VOID,
        &Token::PS => TOKEN_PS,
        &Token::MACRO => TOKEN_MACRO,
        &Token::IF => TOKEN_IF,
        &Token::PIPE => TOKEN_PIPE,
        &Token::CASE => TOKEN_CASE,
        &Token::MATCH => TOKEN_MATCH,
        &Token::True => TOKEN_True,
        &Token::False => TOKEN_False,
        &Token::UNDERSCORE => TOKEN_UNDERSCORE,
        &Token::NEGATE => TOKEN_NEGATE,
        &Token::VOID => TOKEN_VOID,
        &Token::DollarQuestion => TOKEN_DollarQuestion,
        &Token::StrOpen => TOKEN_StrOpen,
        &Token::StrClose => TOKEN_StrClose,
    }
}
#[inline]
fn token_minor(t: Token) -> YYMinorType {
  match t {
        Token::COLON(x) => YYMinorType::YY132(x),
        Token::COMMA(x) => YYMinorType::YY132(x),
        Token::DOUBLEDASH(x) => YYMinorType::YY132(x),
        Token::ELSE(x) => YYMinorType::YY132(x),
        Token::HASHTAG(x) => YYMinorType::YY209(x),
        Token::ID(x) => YYMinorType::YY209(x),
        Token::INT(x) => YYMinorType::YY186(x),
        Token::PLUS(x) => YYMinorType::YY132(x),
        Token::SEMICOLON(x) => YYMinorType::YY132(x),
        Token::SLASH(x) => YYMinorType::YY132(x),
        Token::SquareL(x) => YYMinorType::YY132(x),
        Token::SquareR(x) => YYMinorType::YY132(x),
        Token::StrLit(x) => YYMinorType::YY158(x),
        Token::TYPE_ID(x) => YYMinorType::YY209(x),
        _ => YYMinorType::YY0
  }
}
const YY_ACTTAB_COUNT: i32 = 1206;
const YY_ACTION: [YYACTIONTYPE; 1206] = [
 /*     0 */   189,  195,  192,   37,   38,   34,    5,   27,   11,  228,
 /*    10 */    39,  152,   30,  144,  214,  204,  213,   16,  234,  239,
 /*    20 */    18,   17,   20,   19,   22,   21,   33,   29,   26,    8,
 /*    30 */   149,   59,  131,  128,  126,  124,  122,  205,  233,  232,
 /*    40 */   231,  230,  229,  235,  114,    3,  227,   79,   28,  191,
 /*    50 */   190,   42,   41,  194,  193,   46,   64,  189,  195,  192,
 /*    60 */     4,  294,   34,    5,   27,  221,  228,    1,   27,  104,
 /*    70 */   214,  216,  213,  201,   16,  164,   60,  365,  365,  365,
 /*    80 */   365,   22,   21,   33,   29,   26,    8,  149,   29,   26,
 /*    90 */    38,  149,  189,  195,  192,  233,  232,  231,  230,  229,
 /*   100 */    57,  184,   40,  228,   79,   28,  191,  190,   45,   41,
 /*   110 */   194,  193,   46,   62,  189,  195,  192,  135,   49,  202,
 /*   120 */     5,   32,  187,  228,  105,  214,  142,  213,  138,  177,
 /*   130 */    49,   16,  233,  232,  231,  230,  229,   78,  214,  199,
 /*   140 */   213,  191,  190,    8,    1,  194,  193,   46,  200,  214,
 /*   150 */   119,  213,  233,  232,  231,  230,  229,   47,  117,  149,
 /*   160 */   183,   79,   28,  191,  190,  116,   41,  194,  193,   46,
 /*   170 */   189,  195,  192,  226,  147,  180,    5,  153,   12,  228,
 /*   180 */    34,  184,   27,   47,  170,  139,   43,   16,   45,  178,
 /*   190 */   106,   83,  222,  133,  186,   43,  198,  154,  197,    8,
 /*   200 */   188,   33,   29,   26,   58,  149,  185,   43,  233,  232,
 /*   210 */   231,  230,  229,  130,  129,  174,   15,   79,   28,  191,
 /*   220 */   190,   76,   41,  194,  193,   46,  189,  195,  192,  223,
 /*   230 */   104,  214,    5,  213,  143,  228,  196,  125,   14,  188,
 /*   240 */   123,   13,  121,   16,   75,   74,  188,   73,   48,  171,
 /*   250 */    72,   70,   68,   69,  113,    8,  225,  166,   67,  155,
 /*   260 */   163,  160,  146,   77,  233,  232,  231,  230,  229,   31,
 /*   270 */   215,    1,  127,   79,   28,  191,  190,   71,   41,  194,
 /*   280 */   193,   46,  189,  195,  192,   56,   40,  120,    5,  226,
 /*   290 */   147,  228,   44,  172,  115,  167,  169,  110,  118,   16,
 /*   300 */    60,   50,   10,  112,  111,  165,  106,   80,  222,  159,
 /*   310 */     9,    8,  198,  134,  197,  137,  188,  128,  162,  158,
 /*   320 */   233,  232,  231,  230,  229,  367,  157,  161,   10,   79,
 /*   330 */    28,  191,  190,   63,   41,  194,  193,   46,  189,  195,
 /*   340 */   192,  156,  109,  367,    5,  226,  147,  228,   61,  145,
 /*   350 */   367,   66,  209,  208,  212,   16,   65,  176,   35,  132,
 /*   360 */   168,  367,  106,   86,  222,  367,  367,    8,  198,  367,
 /*   370 */   197,  181,  188,  367,  367,  367,  233,  232,  231,  230,
 /*   380 */   229,   36,  367,  367,  367,   79,   28,  191,  190,    7,
 /*   390 */    41,  194,  193,   46,  367,   34,  367,   27,  367,  367,
 /*   400 */   367,  211,  210,  207,  367,   24,   23,   25,  234,  367,
 /*   410 */    18,   17,   20,   19,   22,   21,   33,   29,   26,    7,
 /*   420 */   149,  367,  224,  367,  367,   34,  367,   27,  367,  367,
 /*   430 */   367,  367,  367,  367,  367,   24,   23,   25,  234,  367,
 /*   440 */    18,   17,   20,   19,   22,   21,   33,   29,   26,  367,
 /*   450 */   149,  367,  182,  366,  150,    2,  367,  219,  367,  367,
 /*   460 */   367,  367,  179,  367,  218,  367,  226,  147,  217,  367,
 /*   470 */   367,  367,  367,  367,  367,  367,  367,  237,  367,  367,
 /*   480 */   236,  220,  367,  106,   90,  222,  367,  367,    7,  198,
 /*   490 */   367,  197,  367,  188,   34,  367,   27,  367,  367,  367,
 /*   500 */   367,  367,  367,  367,   24,   23,   25,  234,  367,   18,
 /*   510 */    17,   20,   19,   22,   21,   33,   29,   26,  367,  149,
 /*   520 */    34,  367,   27,  367,  367,  367,  367,  367,  367,  367,
 /*   530 */    24,   23,   25,  234,  367,   18,   17,   20,   19,   22,
 /*   540 */    21,   33,   29,   26,  367,  149,   34,  175,   27,  367,
 /*   550 */   367,  367,  367,  367,  367,  367,   24,   23,   25,  234,
 /*   560 */   367,   18,   17,   20,   19,   22,   21,   33,   29,   26,
 /*   570 */     6,  149,  367,  182,  367,  367,   34,  367,   27,  367,
 /*   580 */   367,  367,  367,  367,  367,  367,   24,   23,   25,  234,
 /*   590 */   367,   18,   17,   20,   19,   22,   21,   33,   29,   26,
 /*   600 */   367,  149,   34,  367,   27,  367,  151,  367,  367,  367,
 /*   610 */   367,  367,   24,   23,   25,  234,  367,   18,   17,   20,
 /*   620 */    19,   22,   21,   33,   29,   26,  367,  149,   34,  367,
 /*   630 */    27,  367,  367,  367,  367,  367,    1,  367,   24,   23,
 /*   640 */    25,  234,  367,   18,   17,   20,   19,   22,   21,   33,
 /*   650 */    29,   26,  367,  149,   34,  367,   27,  367,  367,  367,
 /*   660 */   367,  367,  367,  367,   24,   23,   25,  234,  367,   18,
 /*   670 */    17,   20,   19,   22,   21,   33,   29,   26,  367,  149,
 /*   680 */   238,    2,  367,  219,  367,  367,  367,  367,  179,  367,
 /*   690 */   218,  367,  226,  147,  217,  367,  367,   40,  367,  367,
 /*   700 */   367,  367,  367,  237,  367,  367,  236,  220,  367,  106,
 /*   710 */    90,  222,  367,  173,    2,  198,  219,  197,  367,  188,
 /*   720 */   367,  179,  367,  218,  367,  226,  147,  217,  367,  367,
 /*   730 */   367,  367,  367,  367,  367,  367,  237,  367,  367,  236,
 /*   740 */   220,  367,  106,   90,  222,  367,  367,  367,  198,   34,
 /*   750 */   197,   27,  188,  367,  367,  367,  367,  367,  367,   24,
 /*   760 */    23,   25,  234,  367,   18,   17,   20,   19,   22,   21,
 /*   770 */    33,   29,   26,  367,  149,   34,  367,   27,  209,  208,
 /*   780 */   212,  367,  367,  367,   35,  367,  367,   25,  234,  367,
 /*   790 */    18,   17,   20,   19,   22,   21,   33,   29,   26,  367,
 /*   800 */   149,  367,  367,  209,  208,  212,  367,   36,  206,   35,
 /*   810 */   203,  367,  367,  367,  367,  367,  367,  367,  226,  147,
 /*   820 */   367,  226,  147,  367,  367,  367,  367,  211,  210,  207,
 /*   830 */   367,  367,   36,  367,  367,  106,   83,  148,  106,   81,
 /*   840 */   222,  198,  103,  197,  198,  188,  197,  107,  188,  367,
 /*   850 */   367,  367,  211,  210,  207,  226,  147,  367,  367,  108,
 /*   860 */   367,  367,  367,  367,  367,  367,  226,  147,  367,  226,
 /*   870 */   147,  367,  106,   53,  222,  367,  367,  367,  198,  367,
 /*   880 */   197,  367,  188,  106,  101,  222,  106,  102,  222,  198,
 /*   890 */   367,  197,  198,  188,  197,  367,  188,  226,  147,  367,
 /*   900 */   226,  147,  367,  367,  367,  367,  367,  367,  367,  367,
 /*   910 */   367,  367,  367,  367,  106,   84,  222,  106,   55,  222,
 /*   920 */   198,  367,  197,  198,  188,  197,  367,  188,  367,  367,
 /*   930 */   226,  147,  367,  226,  147,  367,  367,  367,  367,  367,
 /*   940 */   367,  367,  367,  367,  226,  147,  367,  106,   89,  222,
 /*   950 */   106,  136,  222,  198,  367,  197,  198,  188,  197,  367,
 /*   960 */   188,  106,   54,  222,  226,  147,  367,  198,  367,  197,
 /*   970 */   367,  188,  367,  367,  367,  226,  147,  367,  226,  147,
 /*   980 */   367,  106,  141,  222,  367,  367,  367,  198,  367,  197,
 /*   990 */   367,  188,  106,  140,  222,  106,   94,  222,  198,  367,
 /*  1000 */   197,  198,  188,  197,  367,  188,  367,  367,  226,  147,
 /*  1010 */   367,  226,  147,  367,  367,  367,  367,  367,  367,  367,
 /*  1020 */   367,  367,  226,  147,  367,  106,   92,  222,  106,   91,
 /*  1030 */   222,  198,  367,  197,  198,  188,  197,  367,  188,  106,
 /*  1040 */   100,  222,  226,  147,  367,  198,  367,  197,  367,  188,
 /*  1050 */   367,  367,  367,  226,  147,  367,  226,  147,  367,  106,
 /*  1060 */    99,  222,  367,  367,  367,  198,  367,  197,  367,  188,
 /*  1070 */   106,   98,  222,  106,   97,  222,  198,  367,  197,  198,
 /*  1080 */   188,  197,  367,  188,  367,  367,  226,  147,  367,  226,
 /*  1090 */   147,  367,  367,  367,  367,  367,  367,  367,  367,  367,
 /*  1100 */   226,  147,  367,  106,   96,  222,  106,   95,  222,  198,
 /*  1110 */   367,  197,  198,  188,  197,  367,  188,  106,   93,  222,
 /*  1120 */   226,  147,  367,  198,  367,  197,  367,  188,  367,  367,
 /*  1130 */   367,  226,  147,  367,  226,  147,  367,  106,   85,  222,
 /*  1140 */   367,  367,  367,  198,  367,  197,  367,  188,  106,   88,
 /*  1150 */   222,  106,   87,  222,  198,  367,  197,  198,  188,  197,
 /*  1160 */   367,  188,  367,  367,  226,  147,  367,  226,  147,  367,
 /*  1170 */   367,  367,  367,  367,  367,  367,  367,  367,  226,  147,
 /*  1180 */   367,  106,   52,  222,  106,   82,  222,  198,  367,  197,
 /*  1190 */   198,  188,  197,  367,  188,  106,   51,  222,  367,  367,
 /*  1200 */   367,  198,  367,  197,  367,  188,
];
const YY_LOOKAHEAD: [YYCODETYPE; 1206] = [
 /*     0 */     6,    7,    8,    3,    3,    9,   12,   11,   10,   15,
 /*    10 */    10,   13,   18,   83,   84,   85,   86,   23,   22,    0,
 /*    20 */    24,   25,   26,   27,   28,   29,   30,   31,   32,   35,
 /*    30 */    34,   37,   38,   39,   40,   41,   42,   36,   44,   45,
 /*    40 */    46,   47,   48,    7,   50,   51,   13,   53,   54,   55,
 /*    50 */    56,   33,   58,   59,   60,   61,    5,    6,    7,    8,
 /*    60 */    43,   43,    9,   12,   11,    4,   15,   17,   11,   83,
 /*    70 */    84,    4,   86,   87,   23,    4,    5,   24,   25,   26,
 /*    80 */    27,   28,   29,   30,   31,   32,   35,   34,   31,   32,
 /*    90 */     3,   34,    6,    7,    8,   44,   45,   46,   47,   48,
 /*   100 */    12,    7,   52,   15,   53,   54,   55,   56,   14,   58,
 /*   110 */    59,   60,   61,    5,    6,    7,    8,   89,   90,   13,
 /*   120 */    12,   35,   62,   15,   83,   84,   85,   86,   34,   89,
 /*   130 */    90,   23,   44,   45,   46,   47,   48,   83,   84,   36,
 /*   140 */    86,   55,   56,   35,   17,   59,   60,   61,   83,   84,
 /*   150 */    67,   86,   44,   45,   46,   47,   48,   73,   74,   34,
 /*   160 */     7,   53,   54,   55,   56,   82,   58,   59,   60,   61,
 /*   170 */     6,    7,    8,   77,   78,   36,   12,   13,   51,   15,
 /*   180 */     9,    7,   11,   73,   74,  105,  106,   23,   14,    4,
 /*   190 */    94,   95,   96,    7,  105,  106,  100,  101,  102,   35,
 /*   200 */   104,   30,   31,   32,    2,   34,  105,  106,   44,   45,
 /*   210 */    46,   47,   48,   35,    6,    4,    3,   53,   54,   55,
 /*   220 */    56,    7,   58,   59,   60,   61,    6,    7,    8,   94,
 /*   230 */    83,   84,   12,   86,   87,   15,   94,    7,   16,  104,
 /*   240 */     7,   16,    7,   23,   43,   36,  104,    4,   17,    4,
 /*   250 */     4,    3,   36,   43,    7,   35,   36,    4,    3,   36,
 /*   260 */     4,    4,   99,   67,   44,   45,   46,   47,   48,   52,
 /*   270 */    82,   17,   82,   53,   54,   55,   56,    7,   58,   59,
 /*   280 */    60,   61,    6,    7,    8,    2,   52,   69,   12,   77,
 /*   290 */    78,   15,   97,   72,   97,   69,   72,    7,   49,   23,
 /*   300 */     5,   96,   52,   76,   67,   76,   94,   95,   96,   81,
 /*   310 */    52,   35,  100,   34,  102,  103,  104,   39,   80,   67,
 /*   320 */    44,   45,   46,   47,   48,  107,   67,   67,   52,   53,
 /*   330 */    54,   55,   56,   67,   58,   59,   60,   61,    6,    7,
 /*   340 */     8,   99,   80,  107,   12,   77,   78,   15,   67,   82,
 /*   350 */   107,   67,    6,    7,    8,   23,   67,   96,   12,   96,
 /*   360 */    96,  107,   94,   95,   96,  107,  107,   35,  100,  107,
 /*   370 */   102,  103,  104,  107,  107,  107,   44,   45,   46,   47,
 /*   380 */    48,   35,  107,  107,  107,   53,   54,   55,   56,    3,
 /*   390 */    58,   59,   60,   61,  107,    9,  107,   11,  107,  107,
 /*   400 */   107,   55,   56,   57,  107,   19,   20,   21,   22,  107,
 /*   410 */    24,   25,   26,   27,   28,   29,   30,   31,   32,    3,
 /*   420 */    34,  107,   36,  107,  107,    9,  107,   11,  107,  107,
 /*   430 */   107,  107,  107,  107,  107,   19,   20,   21,   22,  107,
 /*   440 */    24,   25,   26,   27,   28,   29,   30,   31,   32,  107,
 /*   450 */    34,  107,   36,   64,   65,   66,  107,   68,  107,  107,
 /*   460 */   107,  107,   73,  107,   75,  107,   77,   78,   79,  107,
 /*   470 */   107,  107,  107,  107,  107,  107,  107,   88,  107,  107,
 /*   480 */    91,   92,  107,   94,   95,   96,  107,  107,    3,  100,
 /*   490 */   107,  102,  107,  104,    9,  107,   11,  107,  107,  107,
 /*   500 */   107,  107,  107,  107,   19,   20,   21,   22,  107,   24,
 /*   510 */    25,   26,   27,   28,   29,   30,   31,   32,  107,   34,
 /*   520 */     9,  107,   11,  107,  107,  107,  107,  107,  107,  107,
 /*   530 */    19,   20,   21,   22,  107,   24,   25,   26,   27,   28,
 /*   540 */    29,   30,   31,   32,  107,   34,    9,   36,   11,  107,
 /*   550 */   107,  107,  107,  107,  107,  107,   19,   20,   21,   22,
 /*   560 */   107,   24,   25,   26,   27,   28,   29,   30,   31,   32,
 /*   570 */     3,   34,  107,   36,  107,  107,    9,  107,   11,  107,
 /*   580 */   107,  107,  107,  107,  107,  107,   19,   20,   21,   22,
 /*   590 */   107,   24,   25,   26,   27,   28,   29,   30,   31,   32,
 /*   600 */   107,   34,    9,  107,   11,  107,   13,  107,  107,  107,
 /*   610 */   107,  107,   19,   20,   21,   22,  107,   24,   25,   26,
 /*   620 */    27,   28,   29,   30,   31,   32,  107,   34,    9,  107,
 /*   630 */    11,  107,  107,  107,  107,  107,   17,  107,   19,   20,
 /*   640 */    21,   22,  107,   24,   25,   26,   27,   28,   29,   30,
 /*   650 */    31,   32,  107,   34,    9,  107,   11,  107,  107,  107,
 /*   660 */   107,  107,  107,  107,   19,   20,   21,   22,  107,   24,
 /*   670 */    25,   26,   27,   28,   29,   30,   31,   32,  107,   34,
 /*   680 */    65,   66,  107,   68,  107,  107,  107,  107,   73,  107,
 /*   690 */    75,  107,   77,   78,   79,  107,  107,   52,  107,  107,
 /*   700 */   107,  107,  107,   88,  107,  107,   91,   92,  107,   94,
 /*   710 */    95,   96,  107,   65,   66,  100,   68,  102,  107,  104,
 /*   720 */   107,   73,  107,   75,  107,   77,   78,   79,  107,  107,
 /*   730 */   107,  107,  107,  107,  107,  107,   88,  107,  107,   91,
 /*   740 */    92,  107,   94,   95,   96,  107,  107,  107,  100,    9,
 /*   750 */   102,   11,  104,  107,  107,  107,  107,  107,  107,   19,
 /*   760 */    20,   21,   22,  107,   24,   25,   26,   27,   28,   29,
 /*   770 */    30,   31,   32,  107,   34,    9,  107,   11,    6,    7,
 /*   780 */     8,  107,  107,  107,   12,  107,  107,   21,   22,  107,
 /*   790 */    24,   25,   26,   27,   28,   29,   30,   31,   32,  107,
 /*   800 */    34,  107,  107,    6,    7,    8,  107,   35,   36,   12,
 /*   810 */    13,  107,  107,  107,  107,  107,  107,  107,   77,   78,
 /*   820 */   107,   77,   78,  107,  107,  107,  107,   55,   56,   57,
 /*   830 */   107,  107,   35,  107,  107,   94,   95,   96,   94,   95,
 /*   840 */    96,  100,  101,  102,  100,  104,  102,  103,  104,  107,
 /*   850 */   107,  107,   55,   56,   57,   77,   78,  107,  107,   81,
 /*   860 */   107,  107,  107,  107,  107,  107,   77,   78,  107,   77,
 /*   870 */    78,  107,   94,   95,   96,  107,  107,  107,  100,  107,
 /*   880 */   102,  107,  104,   94,   95,   96,   94,   95,   96,  100,
 /*   890 */   107,  102,  100,  104,  102,  107,  104,   77,   78,  107,
 /*   900 */    77,   78,  107,  107,  107,  107,  107,  107,  107,  107,
 /*   910 */   107,  107,  107,  107,   94,   95,   96,   94,   95,   96,
 /*   920 */   100,  107,  102,  100,  104,  102,  107,  104,  107,  107,
 /*   930 */    77,   78,  107,   77,   78,  107,  107,  107,  107,  107,
 /*   940 */   107,  107,  107,  107,   77,   78,  107,   94,   95,   96,
 /*   950 */    94,   95,   96,  100,  107,  102,  100,  104,  102,  107,
 /*   960 */   104,   94,   95,   96,   77,   78,  107,  100,  107,  102,
 /*   970 */   107,  104,  107,  107,  107,   77,   78,  107,   77,   78,
 /*   980 */   107,   94,   95,   96,  107,  107,  107,  100,  107,  102,
 /*   990 */   107,  104,   94,   95,   96,   94,   95,   96,  100,  107,
 /*  1000 */   102,  100,  104,  102,  107,  104,  107,  107,   77,   78,
 /*  1010 */   107,   77,   78,  107,  107,  107,  107,  107,  107,  107,
 /*  1020 */   107,  107,   77,   78,  107,   94,   95,   96,   94,   95,
 /*  1030 */    96,  100,  107,  102,  100,  104,  102,  107,  104,   94,
 /*  1040 */    95,   96,   77,   78,  107,  100,  107,  102,  107,  104,
 /*  1050 */   107,  107,  107,   77,   78,  107,   77,   78,  107,   94,
 /*  1060 */    95,   96,  107,  107,  107,  100,  107,  102,  107,  104,
 /*  1070 */    94,   95,   96,   94,   95,   96,  100,  107,  102,  100,
 /*  1080 */   104,  102,  107,  104,  107,  107,   77,   78,  107,   77,
 /*  1090 */    78,  107,  107,  107,  107,  107,  107,  107,  107,  107,
 /*  1100 */    77,   78,  107,   94,   95,   96,   94,   95,   96,  100,
 /*  1110 */   107,  102,  100,  104,  102,  107,  104,   94,   95,   96,
 /*  1120 */    77,   78,  107,  100,  107,  102,  107,  104,  107,  107,
 /*  1130 */   107,   77,   78,  107,   77,   78,  107,   94,   95,   96,
 /*  1140 */   107,  107,  107,  100,  107,  102,  107,  104,   94,   95,
 /*  1150 */    96,   94,   95,   96,  100,  107,  102,  100,  104,  102,
 /*  1160 */   107,  104,  107,  107,   77,   78,  107,   77,   78,  107,
 /*  1170 */   107,  107,  107,  107,  107,  107,  107,  107,   77,   78,
 /*  1180 */   107,   94,   95,   96,   94,   95,   96,  100,  107,  102,
 /*  1190 */   100,  104,  102,  107,  104,   94,   95,   96,  107,  107,
 /*  1200 */   107,  100,  107,  102,  107,  104,
];
const YY_SHIFT_USE_DFLT: i32 = -7;
const YY_SHIFT_COUNT: i32 = 150;
const YY_SHIFT_MIN: i32 = -6;
const YY_SHIFT_MAX: i32 = 797;
const YY_SHIFT_OFST: [i16; 151] = [
 /*     0 */    -6,   -6,   -6,  276,  220,  164,  332,  332,  332,  108,
 /*    10 */    51,  332,  332,  332,  332,  332,  332,  332,  332,  332,
 /*    20 */   332,  332,  332,  332,  332,  332,  332,  332,  332,  332,
 /*    30 */   332,  332,  332,  332,  332,  797,  772,  346,  346,  346,
 /*    40 */   346,   86,   86,   94,   50,  174,  174,  278,  278,  279,
 /*    50 */   279,  619,  619,  619,  645,  619,   88,   88,   88,   88,
 /*    60 */   127,   71,  254,  258,  254,  250,  295,  290,  254,  290,
 /*    70 */   270,  283,  249,  249,  283,  270,  234,  234,  254,  217,
 /*    80 */   416,  386,  593,  567,  537,  511,  485,  740,  740,  740,
 /*    90 */   740,  766,  766,   -4,   -4,   53,   53,   53,   53,  171,
 /*   100 */   171,   57,   57,   -2,    0,    1,   18,  223,  257,  256,
 /*   110 */   255,  253,  216,  210,  247,  248,  246,  245,  231,  243,
 /*   120 */   209,  201,  235,  225,  233,  222,  230,  211,  214,  213,
 /*   130 */   208,  178,   33,  202,  186,  185,  125,  139,  153,   60,
 /*   140 */   125,  125,  103,  106,   87,   67,   61,   17,   33,   36,
 /*   150 */    19,
];
const YY_REDUCE_USE_DFLT: i32 = -71;
const YY_REDUCE_COUNT: i32 = 79;
const YY_REDUCE_MIN: i32 = -70;
const YY_REDUCE_MAX: i32 = 1101;
const YY_REDUCE_OFST: [i16; 80] = [
 /*     0 */   389,  648,  615,  778,  744,  741,   96,  268,  212,  823,
 /*    10 */  1101, 1090, 1087, 1057, 1054, 1043, 1023, 1012, 1009,  979,
 /*    20 */   976,  965,  945,  934,  931,  901,  898,  887,  867,  856,
 /*    30 */   853,  823,  820,  792,  789,  147,   41,  -14,  -70,   65,
 /*    40 */    54,  142,  135,  101,   83,   89,   80,  110,   84,   40,
 /*    50 */    28,  289,  284,  281,  267,  266,  264,  263,  261,  205,
 /*    60 */   260,  262,  259,  242,  252,  228,  238,  229,  237,  227,
 /*    70 */   226,  197,  224,  221,  195,  218,  190,  188,  196,  163,
];
const YY_DEFAULT: [YYACTIONTYPE; 239] = [
 /*     0 */   240,  240,  240,  365,  365,  365,  365,  365,  365,  365,
 /*    10 */   365,  365,  365,  365,  365,  365,  365,  365,  365,  365,
 /*    20 */   365,  365,  365,  365,  365,  365,  365,  365,  365,  365,
 /*    30 */   365,  365,  365,  365,  365,  365,  365,  365,  365,  365,
 /*    40 */   365,  365,  365,  360,  365,  360,  360,  276,  365,  253,
 /*    50 */   253,  365,  365,  365,  365,  365,  365,  365,  365,  365,
 /*    60 */   365,  365,  365,  365,  365,  365,  286,  279,  365,  279,
 /*    70 */   262,  265,  274,  274,  265,  262,  365,  302,  365,  365,
 /*    80 */   365,  365,  365,  354,  365,  365,  357,  258,  257,  249,
 /*    90 */   244,  334,  333,  324,  332,  340,  339,  338,  337,  336,
 /*   100 */   335,  327,  328,  365,  318,  365,  341,  365,  365,  365,
 /*   110 */   280,  365,  365,  365,  365,  263,  365,  365,  365,  365,
 /*   120 */   365,  365,  365,  365,  365,  365,  365,  365,  365,  365,
 /*   130 */   365,  365,  365,  365,  365,  365,  329,  365,  365,  365,
 /*   140 */   331,  330,  365,  365,  314,  365,  365,  365,  295,  365,
 /*   150 */   365,  353,  352,  351,  355,  293,  299,  298,  289,  288,
 /*   160 */   284,  287,  285,  283,  282,  281,  278,  264,  266,  261,
 /*   170 */   277,  275,  260,  259,  256,  255,  254,  252,  251,  250,
 /*   180 */   356,  358,  342,  364,  363,  362,  361,  359,  350,  349,
 /*   190 */   348,  347,  346,  345,  344,  343,  326,  323,  322,  313,
 /*   200 */   320,  319,  317,  316,  315,  312,  311,  310,  309,  308,
 /*   210 */   307,  306,  305,  304,  303,  301,  300,  248,  247,  246,
 /*   220 */   245,  297,  295,  296,  292,  291,  290,  273,  272,  271,
 /*   230 */   270,  269,  268,  267,  325,  321,  243,  242,  241,
];

/* TMPL: fallback tokens */

const YY_FALLBACK: [i32; 0] = [
];

/* TMPL: symbol names */


/* TMPL: rules */


/* TMPL: destructors */


/* TMPL: stack-overflow */


/* TMPL: stack-overflow */

const YY_RULE_INFO: [YYCODETYPE; 126] = [
  64,
  65,
  65,
  66,
  66,
  66,
  66,
  66,
  66,
  66,
  66,
  66,
  88,
  89,
  89,
  90,
  92,
  73,
  91,
  91,
  67,
  68,
  68,
  69,
  69,
  69,
  97,
  97,
  96,
  96,
  96,
  96,
  96,
  96,
  96,
  72,
  72,
  74,
  74,
  75,
  76,
  76,
  76,
  79,
  79,
  79,
  80,
  80,
  80,
  81,
  81,
  95,
  77,
  77,
  77,
  78,
  78,
  95,
  95,
  99,
  99,
  95,
  82,
  82,
  83,
  83,
  83,
  83,
  83,
  83,
  83,
  83,
  84,
  84,
  84,
  85,
  85,
  86,
  86,
  87,
  87,
  87,
  95,
  95,
  95,
  95,
  95,
  95,
  95,
  95,
  95,
  95,
  95,
  95,
  95,
  95,
  95,
  95,
  95,
  95,
  95,
  95,
  95,
  94,
  94,
  94,
  94,
  94,
  94,
  94,
  94,
  94,
  100,
  100,
  100,
  101,
  101,
  102,
  103,
  103,
  104,
  105,
  105,
  105,
  106,
  106,
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
 (YYMinorType::YY82(yy0),) => {

	if list::is_empty(&yy0) {
		panic!("null program");
	}
	// ignore yyres, it doesn't really go anywhere for program
	yyres = Ast::Nothing;
	// we're done, so put yy0 in extra
	self.extra = Ok(Ast::ReplRoot(yy0));

},    _ => unreachable!() };
 YYMinorType::YY49(yyres)
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
 YYMinorType::YY82(yyres)
}
            ,
            2 /* stmts ::= stmt stmts */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy1),) => {

    vout!("found new stmt: {:?}\n", yy0);
	yyres = list::cons(yy0, yy1);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            3 /* stmt ::= defstruct */
          | 4 /* stmt ::= let_stmt */
          | 6 /* stmt ::= fail_stmt */
          | 7 /* stmt ::= func_stmt */
          | 8 /* stmt ::= macro_stmt */
          | 9 /* stmt ::= if_stmt */
          | 11 /* stmt ::= failed_stmt */
          | 64 /* pexpr ::= ptuple */
          | 65 /* pexpr ::= plist */
          | 83 /* expr ::= list */
          | 84 /* expr ::= tuple */
          | 102 /* expr ::= term */
          | 111 /* term ::= strexpr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY82(yy0),) => {
 yyres = yy0; 
},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            5 /* stmt ::= expr */
          | 51 /* expr ::= call_expr */
          | 55 /* func_term ::= term */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY82(yy0),) => {

    yyres = yy0;

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            10 /* stmt ::= RETURN expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY82(yy1),) => {

    yyres = sexpr::new(SexprType::Return, yy1);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
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
 (YYMinorType::YY39(yy1),YYMinorType::YY82(yy2),) => {

    yyres = sexpr::def_struct(Val::Type(yy1), yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            13 /* defstruct_fields ::= defstruct_field defstruct_fields */
          | 123 /* strlist ::= strlist_term strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy1),) => {

	yyres = list::cons(yy0, yy1);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            14 /* defstruct_fields ::= */
          | 23 /* dfunc_args ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

	yyres = list::empty();

} };
 YYMinorType::YY82(yyres)
}
            ,
            15 /* defstruct_field ::= DOT ID COLON typex */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY209(yy1),YYMinorType::YY39(yy3),) => {

	yyres = sexpr::id_with_type(yy1.data, yy3);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
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
 (YYMinorType::YY209(yy2),YYMinorType::YY82(yy4),) => {

vout!("found fail_stmt {:?}\n", yy2);
	yyres = sexpr::new(SexprType::Fail,
        list::cons(Val::hashtag(yy2.data),
        list::cons(yy4,
        Val::Nil,
        ))
    );

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            17 /* failed_stmt ::= FAILED ID match_case DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY209(yy1),YYMinorType::YY82(yy2),) => {

	yyres = sexpr::new(SexprType::MatchFailed,
        list::cons(Val::id(yy1.data),
        list::cons(yy2,
        Val::Nil
        ))
    );

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            18 /* let_stmt ::= Let ID ASSIGN expr */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY209(yy1),YYMinorType::YY82(yy3),) => {

	let letx =
        list::cons(Val::id(yy1.data),
        list::cons(yy3,
        Val::Nil
        ));
	yyres = sexpr::new(SexprType::Let, letx);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            19 /* let_stmt ::= Fork ID ASSIGN expr */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY209(yy1),YYMinorType::YY82(yy3),) => {

	let bind = list::cons(Val::new_str(yy1.data), list::singleton(yy3));
	yyres = sexpr::new(SexprType::Fork, bind);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            20 /* block ::= BLOCKARROW stmts */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY82(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            21 /* func_stmt ::= Func ID PARENCALL dfunc_args RPAREN opt_typex block DOUBLEDASH opt_ps */
            => 
{
let yyres :  Val ;
let yyp8 = self.yystack.pop().unwrap();
let yyp7 = self.yystack.pop().unwrap();
let yyp6 = self.yystack.pop().unwrap();
let yyp5 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,yyp5.minor,yyp6.minor,yyp8.minor,) {
 (YYMinorType::YY209(yy1),YYMinorType::YY82(yy3),YYMinorType::YY39(yy5),YYMinorType::YY82(yy6),YYMinorType::YY82(yy8),) => {

	let id = Val::id(yy1.data);
	let typ = Val::Type(yy5);
	yyres = sexpr::defunc(id, yy3, typ, yy6, yy8)

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            22 /* func_stmt ::= Func ID PARENCALL dfunc_args RPAREN opt_typex match_case DOUBLEDASH opt_ps */
            => 
{
let yyres :  Val ;
let yyp8 = self.yystack.pop().unwrap();
let yyp7 = self.yystack.pop().unwrap();
let yyp6 = self.yystack.pop().unwrap();
let yyp5 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp3 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,yyp5.minor,yyp6.minor,yyp8.minor,) {
 (YYMinorType::YY209(yy1),YYMinorType::YY82(yy3),YYMinorType::YY39(yy5),YYMinorType::YY82(yy6),YYMinorType::YY82(yy8),) => {

	let id = Val::id(yy1.data);
	let typ = Val::Type(yy5);
    let body = sexpr::match_expr(Val::CallParams, yy6);
	yyres = sexpr::defunc(id, yy3, typ, body, yy8)

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            24 /* dfunc_args ::= ID opt_typex */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY209(yy0),YYMinorType::YY39(yy1),) => {

	yyres = list::singleton(sexpr::id_with_type(yy0.data, yy1));

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
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
 (YYMinorType::YY209(yy0),YYMinorType::YY39(yy1),YYMinorType::YY82(yy3),) => {

	yyres = list::cons(sexpr::id_with_type(yy0.data, yy1), yy3);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
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
 YYMinorType::YY39(yyres)
}
            ,
            27 /* opt_typex ::= COLON typex */
            => 
{
let yyres :  Type ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY39(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY39(yyres)
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
 YYMinorType::YY39(yyres)
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
 YYMinorType::YY39(yyres)
}
            ,
            30 /* typex ::= TYPE_HASHTAG */
            => 
{
let yyres :  Type ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Type::Hashtag;

} };
 YYMinorType::YY39(yyres)
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
 YYMinorType::YY39(yyres)
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
 YYMinorType::YY39(yyres)
}
            ,
            33 /* typex ::= TYPE_ID */
            => 
{
let yyres :  Type ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY209(yy0),) => {

	yyres = Type::Id(Arc::new(yy0.data));

},    _ => unreachable!() };
 YYMinorType::YY39(yyres)
}
            ,
            34 /* typex ::= SquareL typex SquareR */
            => 
{
let yyres :  Type ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY39(yy1),) => {

	yyres = Type::StrictList(Box::new(yy1));

},    _ => unreachable!() };
 YYMinorType::YY39(yyres)
}
            ,
            35 /* opt_ps ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

    yyres = Val::Void;

} };
 YYMinorType::YY82(yyres)
}
            ,
            36 /* opt_ps ::= PS BLOCKARROW failed_stmts DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp2.minor,) {
 (YYMinorType::YY82(yy2),) => {

    yyres = yy2;

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            37 /* failed_stmts ::= failed_stmt */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY82(yy0),) => {

    yyres = list::singleton(yy0);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            38 /* failed_stmts ::= failed_stmt failed_stmts */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy1),) => {

    yyres = list::cons(yy0, yy1);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            39 /* macro_stmt ::= MACRO ID PARENCALL macro_args RPAREN block DOUBLEDASH */
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
 (YYMinorType::YY209(yy1),YYMinorType::YY82(yy3),YYMinorType::YY82(yy5),) => {

    vout!("found macro {:?}\n", yy1);
    yyres = sexpr::new(SexprType::DefMacro,
        list::cons(Val::id(yy1.data),
        list::cons(yy3,
        list::cons(yy5,
        Val::Nil
    ))));

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            40 /* macro_args ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

    yyres = Val::Nil;

} };
 YYMinorType::YY82(yyres)
}
            ,
            41 /* macro_args ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY209(yy0),) => {

    yyres = list::singleton(Val::id(yy0.data));

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            42 /* macro_args ::= ID COMMA macro_args */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY209(yy0),YYMinorType::YY82(yy2),) => {

    yyres = list::cons(Val::id(yy0.data), yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            43 /* if_stmt ::= IF expr block DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy1),YYMinorType::YY82(yy2),) => {

    /* if-only style */
    yyres = sexpr::ifstmt(yy1, yy2, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            44 /* if_stmt ::= IF expr block else_if DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp4 = self.yystack.pop().unwrap();
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,yyp3.minor,) {
 (YYMinorType::YY82(yy1),YYMinorType::YY82(yy2),YYMinorType::YY82(yy3),) => {

    /* if-else style */
    yyres = sexpr::ifstmt(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            45 /* if_stmt ::= IF if_case DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY82(yy1),) => {

    /* case-expr style */
    yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            46 /* else_if ::= ELSE IF expr block else_if */
            => 
{
let yyres :  Val ;
let yyp4 = self.yystack.pop().unwrap();
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp2.minor,yyp3.minor,yyp4.minor,) {
 (YYMinorType::YY82(yy2),YYMinorType::YY82(yy3),YYMinorType::YY82(yy4),) => {

    yyres = sexpr::ifstmt(yy2, yy3, yy4);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            47 /* else_if ::= ELSE IF expr block */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp2.minor,yyp3.minor,) {
 (YYMinorType::YY82(yy2),YYMinorType::YY82(yy3),) => {

    yyres = sexpr::ifstmt(yy2, yy3, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            48 /* else_if ::= ELSE block */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY82(yy1),) => {

    yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            49 /* if_case ::= PIPE expr block if_case */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,yyp3.minor,) {
 (YYMinorType::YY82(yy1),YYMinorType::YY82(yy2),YYMinorType::YY82(yy3),) => {

    yyres = sexpr::ifstmt(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            50 /* if_case ::= PIPE ELSE block */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp2.minor,) {
 (YYMinorType::YY82(yy2),) => {

    yyres = yy2;

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            52 /* call_expr ::= func_term PARENCALL RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY82(yy0),) => {

	vout!("zero param function call!");
	yyres = sexpr::call(yy0, vec![]);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            53 /* call_expr ::= func_term PARENCALL expr RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	vout!("one param function call!");
	yyres = sexpr::call(yy0, vec![yy2]);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            54 /* call_expr ::= func_term PARENCALL tuple_args RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	vout!("multi param function call!");
	yyres = sexpr::call(yy0, list::to_vec(yy2));

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            56 /* func_term ::= typex */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY39(yy0),) => {

    yyres = Val::Type(yy0);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            57 /* expr ::= term DOLLAR term */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	/* yyres = Val::binaryop(yy0, yy2, D); */
	yyres = Val::Void;

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            58 /* expr ::= CASE cases DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY82(yy1),) => {

    vout!("parsed case expr\n");
	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            59 /* cases ::= PIPE expr block PIPE ELSE block */
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
 (YYMinorType::YY82(yy1),YYMinorType::YY82(yy2),YYMinorType::YY82(yy5),) => {

    vout!("found cases base\n");
    yyres = sexpr::casex(yy1, yy2, yy5);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            60 /* cases ::= PIPE expr block cases */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,yyp3.minor,) {
 (YYMinorType::YY82(yy1),YYMinorType::YY82(yy2),YYMinorType::YY82(yy3),) => {

    vout!("found extra case\n");
    yyres = sexpr::casex(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            61 /* expr ::= MATCH expr match_case DOUBLEDASH */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy1),YYMinorType::YY82(yy2),) => {

    vout!("parsed match expr\n");
    yyres = sexpr::match_expr(yy1, yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            62 /* match_case ::= PIPE pexpr block match_case */
            => 
{
let yyres :  Val ;
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,yyp3.minor,) {
 (YYMinorType::YY82(yy1),YYMinorType::YY82(yy2),YYMinorType::YY82(yy3),) => {

    vout!("found cases base\n");
    yyres = sexpr::match_case(yy1, yy2, yy3);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            63 /* match_case ::= PIPE pexpr block */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy1),YYMinorType::YY82(yy2),) => {

    vout!("parsed base match case\n");
    yyres = sexpr::match_case(yy1, yy2, Val::Void);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            66 /* pexpr ::= INT */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY186(yy0),) => {
 yyres = Val::Int(yy0); 
},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            67 /* pexpr ::= True */
          | 108 /* term ::= True */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Bool(true); 
} };
 YYMinorType::YY82(yyres)
}
            ,
            68 /* pexpr ::= False */
          | 109 /* term ::= False */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Bool(false); 
} };
 YYMinorType::YY82(yyres)
}
            ,
            69 /* pexpr ::= HASHTAG */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY209(yy0),) => {
 yyres = Val::Hashtag(Arc::new(yy0.data)); 
},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            70 /* pexpr ::= ID */
          | 104 /* term ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY209(yy0),) => {
 yyres = Val::id(yy0.data); 
},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            71 /* pexpr ::= UNDERSCORE */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {
 yyres = Val::Wildcard; 
} };
 YYMinorType::YY82(yyres)
}
            ,
            72 /* ptuple ::= LPAREN RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match () {
 () => {

	panic!("an empty tuple is not a valid pattern");

} };
 YYMinorType::YY82(yyres)
}
            ,
            73 /* ptuple ::= LPAREN pexpr RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY82(yy1),) => {

	yyres = Val::Tuple(vec![yy1]);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            74 /* ptuple ::= LPAREN pargs RPAREN */
          | 117 /* tuple ::= LPAREN tuple_args RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY82(yy1),) => {

	yyres = Val::tuple_from_list(yy1);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            75 /* pargs ::= pexpr COMMA pexpr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	yyres = list::cons(yy0,
        list::cons(yy2,
        Val::Nil
        ));

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            76 /* pargs ::= pexpr COMMA pargs */
          | 81 /* plist_items ::= pexpr SEMICOLON pexpr */
          | 116 /* list_items ::= expr COMMA list_items */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            77 /* plist ::= SquareL SquareR */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match () {
 () => {

    yyres = list::empty();

} };
 YYMinorType::YY82(yyres)
}
            ,
            78 /* plist ::= SquareL plist_items SquareR */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY82(yy1),) => {

    yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            79 /* plist_items ::= pexpr */
          | 115 /* list_items ::= expr */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY82(yy0),) => {

	yyres = list::singleton(yy0);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            80 /* plist_items ::= pexpr COMMA plist_items */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

    yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            82 /* expr ::= expr DOT ID */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY209(yy2),) => {

    yyres = sexpr::new(SexprType::FieldAccess,
        list::cons(yy0,
        list::cons(Val::id(yy2.data),
        Val::Nil,
        ))
    );

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            85 /* expr ::= NOT expr */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY82(yy1),) => {

	yyres = sexpr::call(Val::id("bool_not".to_string()), vec![yy1]);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            86 /* expr ::= expr ConcatNewline */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY82(yy0),) => {

	let newline = Val::Str(Arc::new("\n".to_string()));
	let args = list::cons(yy0, list::singleton(newline));
	yyres = sexpr::new(SexprType::StrExpr, args)

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            87 /* expr ::= NEGATE term */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY82(yy1),) => {

	yyres = sexpr::call(Val::id("negate".to_string()), vec![yy1]);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            88 /* expr ::= expr PLUS expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	yyres = sexpr::binaryop("int_add".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            89 /* expr ::= expr MINUS expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	yyres = sexpr::binaryop("int_sub".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            90 /* expr ::= expr TIMES expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	yyres = sexpr::binaryop("int_mult".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            91 /* expr ::= expr SLASH expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	yyres = sexpr::binaryop("int_div".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            92 /* expr ::= expr MOD expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	yyres = sexpr::binaryop("int_mod".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            93 /* expr ::= expr AND expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	yyres = sexpr::binaryop("boolean_and".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            94 /* expr ::= expr OR expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	yyres = sexpr::binaryop("boolean_or".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            95 /* expr ::= expr XOR expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	yyres = sexpr::binaryop("boolean_xor".to_string(),yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            96 /* expr ::= expr LT expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	yyres = sexpr::binaryop("less_than".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            97 /* expr ::= expr LTEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	yyres = sexpr::binaryop("less_than_equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            98 /* expr ::= expr GT expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	yyres = sexpr::binaryop("greater_than".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            99 /* expr ::= expr GTEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	yyres = sexpr::binaryop("greater_than_equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            100 /* expr ::= expr EQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	yyres = sexpr::binaryop("equal".to_string(), yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            101 /* expr ::= expr NEQ expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	let eq = sexpr::binaryop("equal".to_string(), yy0, yy2);
	yyres = sexpr::call(Val::id("bool_not".to_string()), vec![eq]);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            103 /* term ::= LPAREN expr RPAREN */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY82(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            105 /* term ::= VOID */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Val::Void;

} };
 YYMinorType::YY82(yyres)
}
            ,
            106 /* term ::= DollarQuestion */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
match () {
 () => {

	yyres = Val::id("$".to_string());

} };
 YYMinorType::YY82(yyres)
}
            ,
            107 /* term ::= INT */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY186(yy0),) => {

	yyres = Val::Int(yy0);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            110 /* term ::= HASHTAG */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY209(yy0),) => {

	yyres = Val::Hashtag(Arc::new(yy0.data));

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            112 /* list ::= SquareL SquareR */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match () {
 () => {

	yyres = list::empty();

} };
 YYMinorType::YY82(yyres)
}
            ,
            113 /* list ::= SquareL list_items SquareR */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY82(yy1),) => {

	yyres = yy1;

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            114 /* list ::= SquareL list_items SEMICOLON expr SquareR */
            => 
{
let yyres :  Val ;
let yyp4 = self.yystack.pop().unwrap();
let yyp3 = self.yystack.pop().unwrap();
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp1.minor,yyp3.minor,) {
 (YYMinorType::YY82(yy1),YYMinorType::YY82(yy3),) => {

	yyres = sexpr::call(Val::id("list_cons".to_string()), vec![yy1, yy3]);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            118 /* tuple_args ::= expr COMMA expr */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	vout!("base tuple args!");
	yyres = list::cons(yy0, list::singleton(yy2));

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            119 /* tuple_args ::= expr COMMA tuple_args */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY82(yy2),) => {

	vout!("additional tuple arg!");
	yyres = list::cons(yy0, yy2);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            120 /* strexpr ::= StrOpen strlist StrClose */
            => 
{
let yyres :  Val ;
self.yystack.pop().unwrap();
let yyp1 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
match (yyp1.minor,) {
 (YYMinorType::YY82(yy1),) => {

	yyres = sexpr::strexpr(yy1);
    vout!("strexpr({:?})\n", yyres);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            121 /* strlist ::= */
            => 
{
let yyres :  Val ;
match () {
 () => {

	yyres = Val::Nil;

} };
 YYMinorType::YY82(yyres)
}
            ,
            122 /* strlist ::= StrLit strlist */
            => 
{
let yyres :  Val ;
let yyp1 = self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp1.minor,) {
 (YYMinorType::YY158(yy0),YYMinorType::YY82(yy1),) => {

	yyres = list::cons(Val::new_str(yy0), yy1);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            124 /* strlist_term ::= ID */
            => 
{
let yyres :  Val ;
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,) {
 (YYMinorType::YY209(yy0),) => {

    yyres = Val::id(yy0.data);

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
}
            ,
            125 /* strlist_term ::= strlist_term DOT ID */
            => 
{
let yyres :  Val ;
let yyp2 = self.yystack.pop().unwrap();
self.yystack.pop().unwrap();
let yyp0 = self.yystack.pop().unwrap();
match (yyp0.minor,yyp2.minor,) {
 (YYMinorType::YY82(yy0),YYMinorType::YY209(yy2),) => {

    yyres = sexpr::new(SexprType::FieldAccess,
        list::cons(yy0,
        list::cons(Val::id(yy2.data),
        Val::Nil,
    )))

},    _ => unreachable!() };
 YYMinorType::YY82(yyres)
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

