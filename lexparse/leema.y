%include {
use leema::ast::{self, Ast, TokenData};
use leema::val::{SrcLoc};
use leema::lstr::{Lstr};
use leema::log;

use std::collections::linked_list::{LinkedList};
use std::io::{Write};
}

%start_symbol {program}
%derive_token {Debug,Clone,PartialEq}
%wildcard ANY.
%extra_argument { Result<Ast, i32> }

%type AND { SrcLoc }
%type BLOCKARROW { SrcLoc }
%type COLON { SrcLoc }
%type COMMA { SrcLoc }
%type ConcatNewline { SrcLoc }
%type DBLCOLON { SrcLoc }
%type DOUBLEDASH { SrcLoc }
%type ELSE { SrcLoc }
%type ENUM { SrcLoc }
%type EQ { SrcLoc }
%type EQ1 { SrcLoc }
%type FAILED { SrcLoc }
%type Fork { SrcLoc }
%type Func { SrcLoc }
%type GT { SrcLoc }
%type GTEQ { SrcLoc }
%type HASHTAG { TokenData<String> }
%type ID { TokenData<String> }
%type IF { SrcLoc }
%type IMPORT { SrcLoc }
%type INT { i64 }
%type Let { SrcLoc }
%type LT { SrcLoc }
%type LTEQ { SrcLoc }
%type MATCH { SrcLoc }
%type MINUS { SrcLoc }
%type MOD { SrcLoc }
%type NEGATE { SrcLoc }
%type NEQ { SrcLoc }
%type NOT { SrcLoc }
%type OR { SrcLoc }
%type LPAREN { SrcLoc }
%type RPAREN { SrcLoc }
%type PARENCALL { SrcLoc }
%type PIPE { SrcLoc }
%type PLUS { SrcLoc }
%type RETURN { SrcLoc }
%type SEMICOLON { SrcLoc }
%type SLASH { SrcLoc }
%type SquareL { SrcLoc }
%type SquareCall { SrcLoc }
%type SquareR { SrcLoc }
%type StrLit { String }
%type StrOpen { SrcLoc }
%type STRUCT { SrcLoc }
%type TIMES { SrcLoc }
%type TYPE_VAR { TokenData<String> }
%type XOR { SrcLoc }

%type program { Ast }
%type stmts { Vec<Ast> }

%type stmt { Ast }
%type block { Ast }
%type func_stmt { Ast }
%type dfunc_one { Ast }
%type dfunc_many { Ast }
%type failed_stmt { Ast }
%type macro_stmt { Ast }
%type call_expr { Ast }
%type if_stmt { Ast }
%type else_if { ast::IfCase }
%type expr_list { LinkedList<Ast> }
%type id_item { Ast }
%type id_list { LinkedList<Ast> }
%type if_expr { Ast }
%type if_case { ast::IfCase }
%type keyed_expr { Ast }
%type localid { Ast }
%type lri { Ast }
%type lri_base { (Vec<Lstr>, SrcLoc) }
%type match_expr { Ast }
%type defstruct { Ast }
%type defstruct_fields { LinkedList<Ast> }
%type defstruct_field { Ast }
%type defenum { Ast }
%type defenum_variants { LinkedList<Ast> }
%type defenum_variant { Ast }
%type defnamedtuple { Ast }
%type let_stmt { Ast }
%type term { Ast }
%type expr { Ast }
%type typex { Ast }
%type opt_typex { Ast }
%type arrow_typex { (Vec<Ast>, SrcLoc) }
%type type_term { Ast }
%type mod_type { Ast }

%type list { Ast }
%type tuple { Ast }
%type strexpr { Ast }
%type strlist { Vec<Ast> }


%nonassoc ASSIGN BLOCKARROW RETURN.
%left OR XOR.
%left AND.
%right ConcatNewline NOT.
%nonassoc EQ NEQ GT GTEQ.
%left LT LTEQ.
%right SEMICOLON.
%left PLUS MINUS.
%left TIMES SLASH MOD.
%right DOLLAR.
%left DOT.
%left LPAREN RPAREN.

%parse_accept {
	//println!("parse accepted");
}

%syntax_error {
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

%parse_failure {
	println!("parse failure");
	panic!("Parse failed.");
}

program(A) ::= stmts(B). {
    // ignore A, it doesn't really go anywhere for program
    A = Ast::ConstVoid;
    // we're done, so put B in extra
    self.extra = Ok(Ast::Block(B)); // , SrcLoc::default()));
}

stmts(A) ::= . {
    // dunno what the location is now. will have to replace it later
    A = Vec::new();
}
stmts(A) ::= stmts(B) stmt(C). {
    let mut tmp = B;
    tmp.push(C);
    A = tmp;
}

block(A) ::= BLOCKARROW(C) stmts(B). {
    A = Ast::Block(B); // , C);
}


stmt(A) ::= defstruct(B). { A = B; }
stmt(A) ::= defenum(B). { A = B; }
stmt(A) ::= defnamedtuple(B). { A = B; }
stmt(A) ::= IMPORT(C) localid(B). {
    A = Ast::Import(Box::new(B), C);
}
stmt(A) ::= let_stmt(B). { A = B; }
stmt(A) ::= failed_stmt(B). { A = B; }
stmt(A) ::= func_stmt(B). { A = B; }
stmt(A) ::= macro_stmt(B). { A = B; }
stmt(A) ::= if_stmt(B). { A = B; }
stmt(A) ::= expr(B). { A = B; }
stmt(A) ::= RETURN(C) expr(B). {
    A = Ast::Return(Box::new(B), C);
}


/** Data Structures */
defstruct(A) ::= STRUCT(D) lri(B) defstruct_fields(C) DOUBLEDASH. {
    A = Ast::DefData(ast::DataType::Struct, Box::new(B), C, D);
}
defstruct_fields(A) ::= defstruct_fields(B) defstruct_field(C). {
    let mut tmp = B;
    tmp.push_back(C);
    A = tmp;
}
defstruct_fields(A) ::= . {
    A = LinkedList::new();
}
defstruct_field(A) ::= DOT ID(B) COLON term(C). {
    A = Ast::KeyedExpr(Lstr::from_string(B.data), Box::new(C), B.loc);
}

/** Enum Definitions */
defenum(A) ::= ENUM(D) lri(B) defenum_variants(C) DOUBLEDASH. {
    A = Ast::DefData(ast::DataType::Enum, Box::new(B), C, D);
}
defenum_variants(A) ::= defenum_variants(B) defenum_variant(C). {
    let mut tmp = B;
    tmp.push_back(C);
    A = tmp;
}
defenum_variants(A) ::= defenum_variant(B). {
    let mut tmp = LinkedList::new();
    tmp.push_back(B);
    A = tmp;
}
defenum_variant(A) ::= PIPE(D) localid(B) PARENCALL expr_list(C) RPAREN. {
    A = Ast::DefData(ast::DataType::NamedTuple, Box::new(B), C, D);
}
defenum_variant(A) ::= PIPE(D) localid(B) defstruct_fields(C). {
    A = Ast::DefData(ast::DataType::Enum, Box::new(B), C, D);
}

/** named tuple definition **/
defnamedtuple(A) ::= STRUCT(L) lri(C) PARENCALL expr_list(D) RPAREN.
{
    A = Ast::DefData(ast::DataType::NamedTuple, Box::new(C), D, L);
}


failed_stmt(A) ::= FAILED(L) localid(B) if_case(C) DOUBLEDASH. {
    A = Ast::IfExpr(ast::IfType::MatchFailure
        , Box::new(B), Box::new(C), L);
}

let_stmt(A) ::= Let(D) expr(B) ASSIGN expr(C). {
    A = Ast::Let(ast::LetType::Inline, Box::new(B), Box::new(C), D);
}
let_stmt(A) ::= Fork(D) expr(B) ASSIGN expr(C). {
    A = Ast::Let(ast::LetType::Forked, Box::new(B), Box::new(C), D);
}
let_stmt ::= Let expr EQ1(A) expr. {
    panic!("Found let x =... @ {:?}\ninstead it should be let x := ...\n", A);
}

/* rust func declaration */
func_stmt(A) ::= Func(Z) lri(B) PARENCALL id_list(D) RPAREN COLON typex(E)
    RUSTBLOCK.
{
    A = Ast::DefFunc(ast::FuncClass::Func
        , Box::new(B), D, Box::new(E), Box::new(Ast::RustBlock), Z);
}

/* func one case, no matching */
func_stmt(A) ::= Func(Z) lri(B) PARENCALL id_list(D) RPAREN opt_typex(E)
    block(C) DOUBLEDASH.
{
    A = Ast::DefFunc(ast::FuncClass::Func
        , Box::new(B), D, Box::new(E), Box::new(C), Z);
}
/* func w/ pattern matching */
func_stmt(A) ::= Func(Z) lri(B) PARENCALL id_list(C) RPAREN opt_typex(D)
    if_case(E) DOUBLEDASH.
{
    // extract field names from args to pass them through to match expr
    let body = Ast::matchfunc_body(&C, E, Z);
    A = Ast::DefFunc(ast::FuncClass::Func
        , Box::new(B), C, Box::new(D), Box::new(body), Z);
}


opt_typex(A) ::= . {
    A = Ast::TypeAnon;
}
opt_typex(A) ::= COLON typex(B). {
    A = B;
}

typex(A) ::= term(B). {
    A = B;
}
typex(A) ::= arrow_typex(B). {
    A = Ast::TypeFunc(B.0, B.1);
}

arrow_typex(A) ::= term(B) GT(L) term(C). {
    A = (vec![B, C], L);
}
arrow_typex(A) ::= arrow_typex(B) GT term(C). {
    let mut tmp = B;
    tmp.0.push(C);
    A = tmp;
}


/* defining a macro */
macro_stmt(A) ::= MACRO ID(B) PARENCALL id_list(D) RPAREN block(C) DOUBLEDASH. {
    vout!("found macro {:?}\n", B);
    let name = Ast::Lri(vec![Lstr::from_string(B.data)], None, B.loc.clone());
    A = Ast::DefFunc(ast::FuncClass::Macro
        , Box::new(name), D, Box::new(Ast::TypeAnon), Box::new(C), B.loc);
}

expr(A) ::= if_expr(B). { A = B; }
expr(A) ::= match_expr(B). { A = B; }
expr(A) ::= call_expr(B). { A = B; }
expr(A) ::= block(B) DOUBLEDASH. { A = B; }
expr(A) ::= term(B). { A = B; }

/* if statements can go 3 different ways
* might be that one of these should be the only one, but I'm not sure
* now and not ready to commit, so leaving all 3 in.
*
* part of the reason for indecision is concern about incorrectly
* expecting a full block to be inside of a single expr if/else block
* when it is not
*
* if only style (requires stmt block to avoid if-block bugs):
*     if x ->
*         y
*     --
*
* if/else style:
*     if x -> y
*     else if y -> z
*     else -> whatever
*     --
*
* case expr style:
*     if
*     |x -- y
*     |y -- z
*     |z -- whatever
*     --
*/
if_stmt(A) ::= IF(D) expr(B) block(C) DOUBLEDASH. {
    /* if-only style */
    let case = ast::IfCase::new(B, C, None, D.clone());
    A = Ast::IfExpr(ast::IfType::If
        , Box::new(Ast::ConstVoid), Box::new(case), D);
}
if_stmt(A) ::= IF(L) expr(B) block(C) else_if(D) DOUBLEDASH. {
    /* if-else style */
    let case = ast::IfCase::new(B, C, Some(D), L);
    A = Ast::IfExpr(ast::IfType::If
        , Box::new(Ast::ConstVoid), Box::new(case), L);
}
else_if(A) ::= ELSE IF(L) expr(B) block(C) else_if(D). {
    A = ast::IfCase::new(B, C, Some(D), L);
}
else_if(A) ::= ELSE IF(L) expr(B) block(C). {
    A = ast::IfCase::new(B, C, None, L);
}
else_if(A) ::= ELSE(L) block(B). {
    A = ast::IfCase::new(Ast::Wildcard, B, None, L);
}


/* regular function call */
call_expr(A) ::= term(B) PARENCALL(D) expr_list(C) RPAREN. {
    A = Ast::Call(Box::new(B), C, D);
}

expr(A) ::= term(B) DOLLAR term(C). {
    /* A = Val::binaryop(B, C, D); */
    A = Ast::ConstVoid;
}
expr(A) ::= term(B) DOT ID(C). {
    A = Ast::DotAccess(Box::new(B), Lstr::from_string(C.data));
}

/* IF expression */
if_expr(A) ::= IF(L) if_case(B) DOUBLEDASH. {
    /* case-expr style */
    A = Ast::IfExpr(ast::IfType::If, Box::new(Ast::ConstVoid), Box::new(B), L);
}
if_case(A) ::= PIPE(L) expr(B) block(C). {
    A = ast::IfCase::new(B, C, None, L);
}
if_case(A) ::= PIPE(L) expr(B) block(C) if_case(D). {
    A = ast::IfCase::new(B, C, Some(D), L);
}
if_case(A) ::= PIPE ELSE(L) block(B). {
    A = ast::IfCase::new(Ast::Wildcard, B, None, L);
}

/* match expression */
match_expr(A) ::= MATCH(D) expr(B) if_case(C) DOUBLEDASH. {
    A = Ast::IfExpr(ast::IfType::Match, Box::new(B), Box::new(C), D);
}


expr(A) ::= NOT(C) expr(B). {
    let call = vec![Lstr::from_sref("prefab"), Lstr::from_sref("bool_not")];
    let mut args = LinkedList::new();
    args.push_back(B);
    A = Ast::Call(Box::new(Ast::Lri(call, None, C.clone())), args, C);
}
expr(A) ::= expr(B) ConcatNewline(C). {
    A = Ast::StrExpr(vec![B, Ast::ConstStr(Lstr::from_sref("\n"))], C);
}
/* arithmetic */
expr(A) ::= NEGATE(C) term(B). {
    let call = vec![Lstr::from_sref("prefab"), Lstr::from_sref("int_negate")];
    let mut args = LinkedList::new();
    args.push_back(B);
    A = Ast::Call(Box::new(Ast::Lri(call, None, C.clone())), args, C);
}
expr(A) ::= expr(B) PLUS(D) expr(C). {
    let call = vec![Lstr::from_sref("prefab"), Lstr::from_sref("int_add")];
    A = Ast::binaryop(call, B, C, D);
}
expr(A) ::= expr(B) MINUS(D) expr(C). {
    let call = vec![Lstr::from_sref("prefab"), Lstr::from_sref("int_sub")];
    A = Ast::binaryop(call, B, C, D);
}
expr(A) ::= expr(B) TIMES(D) expr(C). {
    let call = vec![Lstr::from_sref("prefab"), Lstr::from_sref("int_mult")];
    A = Ast::binaryop(call, B, C, D);
}
expr(A) ::= expr(B) SLASH(D) expr(C). {
    let call = vec![Lstr::from_sref("prefab"), Lstr::from_sref("int_div")];
    A = Ast::binaryop(call, B, C, D);
}
expr(A) ::= expr(B) MOD(D) expr(C). {
    let call = vec![Lstr::from_sref("prefab"), Lstr::from_sref("int_mod")];
    A = Ast::binaryop(call, B, C, D);
}
expr(A) ::= expr(B) SEMICOLON expr(C). {
    A = Ast::Cons(Box::new(B), Box::new(C));
}
expr(A) ::= expr(B) AND(D) expr(C). {
    let call = vec![Lstr::from_sref("prefab"), Lstr::from_sref("boolean_and")];
    A = Ast::binaryop(call, B, C, D);
}
expr(A) ::= expr(B) OR(D) expr(C). {
    let call = vec![Lstr::from_sref("prefab"), Lstr::from_sref("boolean_or")];
    A = Ast::binaryop(call, B, C, D);
}
expr(A) ::= expr(B) XOR(D) expr(C). {
    let call = vec![Lstr::from_sref("prefab"), Lstr::from_sref("boolean_xor")];
    A = Ast::binaryop(call, B, C, D);
}

/* comparisons */
expr(A) ::= expr(B) LT(D) expr(C). {
    let call = vec![Lstr::from_sref("prefab"), Lstr::from_sref("less_than")];
    A = Ast::binaryop(call, B, C, D);
}
expr(A) ::= expr(B) LTEQ(D) expr(C). {
    let call =
        vec![Lstr::from_sref("prefab"), Lstr::from_sref("less_than_equal")];
    A = Ast::binaryop(call, B, C, D);
}
expr(A) ::= expr(B) GT(D) expr(C). {
    let call = vec![Lstr::from_sref("prefab"), Lstr::from_sref("greater_than")];
    A = Ast::binaryop(call, B, C, D);
}
expr(A) ::= expr(B) GTEQ(D) expr(C). {
    let call =
        vec![Lstr::from_sref("prefab"), Lstr::from_sref("greater_than_equal")];
    A = Ast::binaryop(call, B, C, D);
}
expr(A) ::= expr(B) EQ(P) expr(C). {
    let call = vec![Lstr::from_sref("prefab"), Lstr::from_sref("equal")];
    A = Ast::binaryop(call, B, C, P);
}
expr(A) ::= expr(B) NEQ(P) expr(C). {
    let inner_call = vec![Lstr::from_sref("prefab"), Lstr::from_sref("equal")];
    let mut inner_op = LinkedList::new();
    inner_op.push_back(Ast::binaryop(inner_call, B, C, P));
    let not_call = Ast::Lri(vec![Lstr::from_sref("bool_not")], None, P.clone());
    A = Ast::Call(Box::new(not_call), inner_op, P);
}
/*
expr(A) ::= expr(B) LT expr(C) LT expr(D). {
	let l1 = sxpr::binaryop("less_than".to_string(), B, C);
	let l2 = sxpr::binaryop("less_than".to_string(), C, D);
	A = sxpr::binaryop("and".to_string(), l1, l2);
}
expr(A) ::= expr(B) LT expr(C) LTEQ expr(D). {
	let l1 = sxpr::binaryop("less_than".to_string(), B, C);
	let l2 = sxpr::binaryop("less_than_equal".to_string(), C, D);
	A = sxpr::binaryop("and".to_string(), l1, l2);
}
expr(A) ::= expr(B) LTEQ expr(C) LT expr(D). {
	let l1 = sxpr::binaryop("less_than_equal".to_string(), B, C);
	let l2 = sxpr::binaryop("less_than".to_string(), C, D);
	A = sxpr::binaryop("and".to_string(), l1, l2);
}
expr(A) ::= expr(B) LTEQ expr(C) LTEQ expr(D). {
	let l1 = sxpr::binaryop("less_than_equal".to_string(), B, C);
	let l2 = sxpr::binaryop("less_than_equal".to_string(), C, D);
	A = sxpr::binaryop("and".to_string(), l1, l2);
}
*/

term(A) ::= tuple(B). {
    A = B;
}
term(A) ::= list(B). {
    A = B;
}
term(A) ::= lri(B). {
    A = B;
}
term(A) ::= VOID. {
    A = Ast::ConstVoid;
}
term(A) ::= DollarQuestion. {
    // A = Ast::id("$".to_string());
    A = Ast::ConstVoid;
}
term(A) ::= INT(B). {
    A = Ast::ConstInt(B);
}
term(A) ::= True. { A = Ast::ConstBool(true); }
term(A) ::= False. { A = Ast::ConstBool(false); }
term(A) ::= HASHTAG(B). {
    A = Ast::ConstHashtag(Lstr::from_string(B.data));
}
term(A) ::= strexpr(B). { A = B; }
term(A) ::= UNDERSCORE. { A = Ast::Wildcard; }
term(A) ::= type_term(B). {
    A = B;
}

type_term(A) ::= TYPE_INT. {
    A = Ast::TypeInt;
}
type_term(A) ::= TYPE_STR. {
    A = Ast::TypeStr;
}
type_term(A) ::= TYPE_HASHTAG. {
    A = Ast::TypeHashtag;
}
type_term(A) ::= TYPE_BOOL. {
    A = Ast::TypeBool;
}
type_term(A) ::= TYPE_VOID. {
    A = Ast::TypeVoid;
}
type_term(A) ::= TYPE_VAR(B). {
    A = Ast::TypeVar(Lstr::from_string(B.data), B.loc);
}

list(A) ::= SquareL expr_list(B) SquareR. {
    A = Ast::List(B);
}

/* tuple
 * (4 + 4, 6 - 7)
 */
tuple(A) ::= LPAREN expr_list(B) RPAREN. {
    A = Ast::Tuple(B);
}

localid(A) ::= ID(B). {
    A = Ast::Localid(Lstr::from_string(B.data), B.loc);
}

lri(A) ::= lri_base(B). {
    A = Ast::Lri(B.0, None, B.1);
}
lri(A) ::= lri_base(B) SquareCall expr_list(C) SquareR. {
    A = Ast::Lri(B.0, Some(C), B.1);
}
lri_base(A) ::= ID(B). {
println!("found lri: {}", B.data);
    A = (vec![Lstr::from_string(B.data)], B.loc);
}
lri_base(A) ::= lri_base(B) DBLCOLON ID(C). {
    let mut tmp = B;
    tmp.0.push(Lstr::from_string(C.data));
    A = tmp;
}


id_list(A) ::= . {
    A = LinkedList::new();
}
id_list(A) ::= id_item(B). {
    let mut tmp = LinkedList::new();
    tmp.push_back(B);
    A = tmp;
}
id_list(A) ::= id_item(B) COMMA id_list(C). {
    let mut tmp = C;
    tmp.push_front(B);
    A = tmp;
}
id_item(A) ::= ID(B). {
    A = Ast::Localid(Lstr::from_string(B.data), B.loc);
}
id_item(A) ::= ID(B) COLON expr(C). {
    A = Ast::KeyedExpr(Lstr::from_string(B.data), Box::new(C), B.loc);
}

expr_list(A) ::= . {
    A = LinkedList::new();
}
expr_list(A) ::= keyed_expr(B). {
    let mut tmp = LinkedList::new();
    tmp.push_front(B);
    A = tmp;
}
expr_list(A) ::= keyed_expr(B) COMMA expr_list(C). {
    let mut tmp = C;
    tmp.push_front(B);
    A = tmp;
}
keyed_expr(A) ::= expr(B). {
    A = B;
}
keyed_expr(A) ::= ID(B) COLON expr(C). {
    A = Ast::KeyedExpr(Lstr::from_string(B.data), Box::new(C), B.loc);
}


strexpr(A) ::= StrOpen(C) strlist(B) StrClose. {
    A = Ast::StrExpr(B, C);
    vout!("strexpr({:?})\n", A);
}
strlist(A) ::= . {
    A = Vec::new();
}
strlist(A) ::= strlist(B) StrLit(C). {
    let mut tmp = B;
    tmp.push(Ast::ConstStr(Lstr::from_string(C)));
    A = tmp;
}
strlist(A) ::= strlist(B) term(C). {
    let mut tmp = B;
    tmp.push(C);
    A = tmp;
}
