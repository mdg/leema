%include {
#![allow(non_camel_case_types)]
#![allow(non_upper_case_globals)]

use leema::ast::{self, Ast, TokenData, Kxpr};
use leema::val::{SrcLoc};
use leema::lstr::{Lstr};

use std::collections::linked_list::{LinkedList};
}

%start_symbol {program}
%derive_token {Debug,Clone,PartialEq}
%wildcard ANY.
%extra_argument { (Result<Ast, i32>, Ast) }

%type AND { SrcLoc }
%type BLOCKARROW { SrcLoc }
%type COLON { SrcLoc }
%type COMMA { SrcLoc }
%type ConcatNewline { SrcLoc }
%type CurlyL { SrcLoc }
%type CurlyR { SrcLoc }
%type DBLCOLON { SrcLoc }
%type DOUBLEARROW { SrcLoc }
%type DOUBLEDASH { SrcLoc }
%type ELSE { SrcLoc }
%type ENUM { SrcLoc }
%type EQ { SrcLoc }
%type EQ1 { SrcLoc }
%type FAILED { SrcLoc }
%type FN { SrcLoc }
%type FTYPE { SrcLoc }
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
%type MACRO { SrcLoc }
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
%type PCT { SrcLoc }
%type PIPE { SrcLoc }
%type PLUS { SrcLoc }
%type QUESTION { SrcLoc }
%type RETURN { SrcLoc }
%type SEMICOLON { SrcLoc }
%type SLASH { SrcLoc }
%type SquareCall { SrcLoc }
%type SquareL { SrcLoc }
%type SquareR { SrcLoc }
%type StrLit { String }
%type StrOpen { SrcLoc }
%type STRUCT { SrcLoc }
%type TIMES { SrcLoc }
%type XOR { SrcLoc }

%type program { Ast }
%type stmts { Vec<Ast> }

%type stmt { Ast }
%type block { Ast }
%type defunc_id { Ast }
%type func_stmt { Ast }
%type func_decl { ast::FuncDecl }
%type func_body { Ast }
%type failed_stmt { Ast }
%type macro_stmt { Ast }
%type call_expr { Ast }
%type closure_expr { Ast }
%type if_stmt { Ast }
%type else_if { ast::IfCase }
%type expr_list { LinkedList<Ast> }
%type k_maybe_x { Kxpr }
%type k_list { LinkedList<Kxpr> }
%type x_maybe_k { Kxpr }
%type x_list { LinkedList<Kxpr> }
%type ktype_list { LinkedList<Kxpr> }
%type k_maybe_type { Kxpr }
%type typex_list { LinkedList<Kxpr> }
%type typex_maybe_k { Kxpr }
%type id_spec { Ast }
%type if_expr { Ast }
%type if_case { ast::IfCase }
%type local_generic { Ast }
%type localid { Ast }
%type localid_spec { Ast }
%type match_expr { Ast }
%type modid { Ast }
%type modid_spec { Ast }
%type defstruple { Ast }
%type defstruple_block { LinkedList<Kxpr> }
%type defstruple_block_field { Kxpr }
%type defenum { Ast }
%type defenum_variants { LinkedList<Kxpr> }
%type defenum_variant { Kxpr }
%type let_stmt { Ast }
%type term { Ast }
%type expr { Ast }
%type opt_typex { Ast }
%type typex { Ast }
%type type_term { Ast }
%type mod_type { Ast }

%type list { Ast }
%type map { Ast }
%type tuple { Ast }
%type strexpr { Ast }
%type strlist { Vec<Ast> }
%type strconst { String }


%nonassoc ASSIGN COMMA RETURN.
%right BLOCKARROW DOUBLEARROW.
%right COLON FN FTYPE.
%left OR XOR.
%left AND.
%right ConcatNewline NOT.
%nonassoc EQ NEQ GT GTEQ.
%left LT LTEQ.
%right SEMICOLON.
%left PLUS MINUS.
%left TIMES SLASH MOD.
%right DOLLAR.
%left DOT PCT.
%left LPAREN RPAREN.

%parse_accept {
	//println!("parse accepted");
}

%syntax_error {
    match yymajor {
        TOKEN_EOI => {
	        panic!("Unexpected end of file. Maybe add newline?");
        }
        TOKEN_COLON => {
            self.extra.0 = Err(1)
        }
        _ => {
	        println!("syntax error at token {:?}\n", yymajor);
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
    self.extra.0 = Ok(Ast::Block(B)); // , SrcLoc::default()));
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

block(A) ::= block_arrow(C) stmts(B). {
    A = Ast::Block(B); // , C);
}

block_arrow ::= BLOCKARROW. {}
block_arrow ::= DOUBLEARROW. {}


stmt(A) ::= defstruple(B). { A = B; }
stmt(A) ::= defenum(B). { A = B; }
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


/** Struct Definitions */

defstruple(A) ::= STRUCT(Z) localid_spec(B) PARENCALL typex_list(C) RPAREN.
{
    A = Ast::DefData(ast::DataType::Struple, Box::new(B), C, Z);
}

defstruple(A) ::= STRUCT(Z) localid_spec(B) defstruple_block(C) DOUBLEDASH. {
    A = Ast::DefData(ast::DataType::Struple, Box::new(B), C, Z);
}
defstruple_block(A) ::= . {
    A = LinkedList::new();
}
defstruple_block(A) ::= defstruple_block(B) defstruple_block_field(C). {
    let mut tmp = B;
    tmp.push_back(C);
    A = tmp;
}
defstruple_block_field(A) ::= DOT ID(B) COLON typex(C). {
    A = Kxpr::new(Lstr::from(B.data), C);
}

/** Enum Definitions */
defenum(A) ::= ENUM(D) localid_spec(B) defenum_variants(C) DOUBLEDASH. {
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
defenum_variant(A) ::= PIPE(D) ID(B) PARENCALL typex_list(C) RPAREN. {
    let name = Lstr::from(B.data);
    let name_ast = Box::new(Ast::Localid(name.clone(), B.loc));
    let ds = Ast::DefData(ast::DataType::Struple, name_ast, C, D);
    A = Kxpr::new(name, ds);
}
defenum_variant(A) ::= PIPE(D) ID(B) defstruple_block(C). {
    let name = Lstr::from(B.data);
    let name_ast = Box::new(Ast::Localid(name.clone(), B.loc));
    let ds = Ast::DefData(ast::DataType::Struple, name_ast, C, D);
    A = Kxpr::new(name, ds);
}


failed_stmt(A) ::= FAILED(L) localid(B) if_case(C) DOUBLEDASH. {
    A = Ast::IfExpr(ast::IfType::MatchFailure
        , Box::new(B), Box::new(C), L);
}

let_stmt(A) ::= Let(Z) expr(B) opt_typex(C) ASSIGN expr(D). {
    A = Ast::Let(Box::new(B), Box::new(C), Box::new(D), Z);
}

func_stmt(A) ::= func_decl(B) func_body(C). {
    A = Ast::DefFunc(ast::FuncClass::Func, Box::new(B), Box::new(C));
}

/* traditional parens func declaration */
func_decl(A) ::= Func(Z) defunc_id(B) PARENCALL ktype_list(D) RPAREN opt_typex(E). {
    vout!("declare paren func {}\n", B);
    A = ast::FuncDecl{ name: B, args: D, result: E, loc: Z };
}

/* struct-style func declaration */
func_decl(A) ::= Func(Z) defunc_id(B) opt_typex(C) defstruple_block(D). {
    vout!("declare struct func {}\n", B);
    A = ast::FuncDecl{ name: B, args: D, result: C, loc: Z };
}

defunc_id(A) ::= localid(B). {
    A = B;
}
defunc_id(A) ::= local_generic(B). {
    A = B;
}

/* func one case, no matching */
func_body(A) ::= block(B) DOUBLEDASH. {
    vout!("parse func block body {}\n", B);
    A = B;
}

/* match func declaration */
func_body(A) ::= DOUBLEARROW(Z) if_case(B) DOUBLEDASH. {
    vout!("parse pattern func {:?}\n", B);
    // extract field names from args to pass them through to match expr
    A = Ast::matchfunc_body(B, Z);
}

/* rust func declaration */
func_body(A) ::= RUSTBLOCK. {
    A = Ast::RustBlock;
}


opt_typex(A) ::= . {
    A = Ast::TypeAnon;
}
opt_typex(A) ::= COLON typex(B). {
    A = B;
}


/* defining a macro */
macro_stmt(A) ::=
    MACRO(Z) localid(B) PARENCALL k_list(D) RPAREN block(C) DOUBLEDASH.
{
    vout!("found macro {:?}\n", B);
    let decl = ast::FuncDecl {
        name: B,
        args: D,
        result: Ast::TypeAnon,
        loc: Z,
    };
    A = Ast::DefFunc(ast::FuncClass::Macro, Box::new(decl), Box::new(C));
}

expr(A) ::= if_expr(B). { A = B; }
expr(A) ::= match_expr(B). { A = B; }
expr(A) ::= call_expr(B). { A = B; }
expr(A) ::= closure_expr(B). { A = B; }
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
*     |x -> y
*     |y -> z
*     |else -> whatever
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
call_expr(A) ::= term(B) PARENCALL(D) x_list(C) RPAREN. {
    A = Ast::Call(Box::new(B), C, D);
}

closure_expr(A) ::= FN(L) LPAREN ktype_list(B) RPAREN expr(C). {
    A = Ast::closure(B, C, L);
}

typex(A) ::= type_term(B). {
    A = B;
}
type_term(A) ::= id_spec(B). {
    A = B;
}
type_term(A) ::= TYPE_FAILURE. {
    A = Ast::TypeFailure;
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
type_term(A) ::= SquareL typex(B) SquareR. {
    let mut inner = LinkedList::new();
    inner.push_back(B);
    A = Ast::List(inner);
}
type_term(A) ::= LPAREN typex_list(B) RPAREN. {
    A = Ast::Tuple(B);
}
type_term(A) ::= CurlyL(Z) typex(B) COLON typex(C) CurlyR. {
    A = Ast::type_call("map", "T", vec![B, C], Z);
}
type_term(A) ::= type_term(B) PCT(Z). {
    A = Ast::type_call("task", "Future", vec![B], Z);
}
type_term(A) ::= type_term(B) TIMES(Z). {
    A = Ast::type_call("seq", "T", vec![B], Z);
}
type_term(A) ::= type_term(B) QUESTION(Z). {
    A = Ast::type_call("option", "T", vec![B], Z);
}
typex(A) ::= FTYPE(Z) PARENCALL typex_list(B) RPAREN COLON typex(C). {
    let typevec: Vec<Kxpr> = B.into_iter().collect();
    A = Ast::TypeFunc(typevec, Box::new(C), Z);
}

ktype_list(A) ::= . {
    A = LinkedList::new();
}
ktype_list(A) ::= k_maybe_type(B). {
    let mut tmp = LinkedList::new();
    tmp.push_back(B);
    A = tmp;
}
ktype_list(A) ::= k_maybe_type(B) COMMA ktype_list(C). {
    let mut tmp = C;
    tmp.push_front(B);
    A = tmp;
}
k_maybe_type(A) ::= ID(B). {
    A = Kxpr::new_k(Lstr::from(B.data));
}
k_maybe_type(A) ::= ID(B) COLON typex(C). {
    A = Kxpr::new(Lstr::from(B.data), C);
}

typex_list(A) ::= . {
    A = LinkedList::new();
}
typex_list(A) ::= typex_maybe_k(B). {
    let mut tmp = LinkedList::new();
    tmp.push_front(B);
    A = tmp;
}
typex_list(A) ::= typex_maybe_k(B) COMMA typex_list(C). {
    let mut tmp = C;
    tmp.push_front(B);
    A = tmp;
}
typex_maybe_k(A) ::= typex(B). {
    A = Kxpr::new_x(B);
}
typex_maybe_k(A) ::= ID(B) COLON typex(C). {
    A = Kxpr::new(Lstr::from(B.data), C);
}


expr(A) ::= term(B) DOLLAR term(C). {
    /* A = Val::binaryop(B, C, D); */
    A = Ast::ConstVoid;
}
expr(A) ::= expr(B) DOT ID(C). {
    A = Ast::DotAccess(Box::new(B), Lstr::from(C.data));
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
    let call = Ast::Modid(Lstr::Sref("prefab"), Lstr::Sref("bool_not"), C);
    let mut args = LinkedList::new();
    args.push_back(Kxpr::new_x(B));
    A = Ast::Call(Box::new(call), args, C);
}
expr(A) ::= expr(B) ConcatNewline(C). {
    A = Ast::StrExpr(vec![B, Ast::ConstStr(Lstr::from("\n"))], C);
}
/* arithmetic */
expr(A) ::= NEGATE(C) term(B). {
    let call = Ast::Modid(Lstr::Sref("prefab"), Lstr::Sref("int_negate"), C);
    let mut args = LinkedList::new();
    args.push_back(Kxpr::new_x(B));
    A = Ast::Call(Box::new(call), args, C);
}
expr(A) ::= expr(B) PLUS(D) expr(C). {
    A = Ast::binaryop("prefab", "int_add", B, C, D);
}
expr(A) ::= expr(B) MINUS(D) expr(C). {
    A = Ast::binaryop("prefab", "int_sub", B, C, D);
}
expr(A) ::= expr(B) TIMES(D) expr(C). {
    A = Ast::binaryop("prefab", "int_mult", B, C, D);
}
expr(A) ::= expr(B) SLASH(D) expr(C). {
    A = Ast::binaryop("prefab", "int_div", B, C, D);
}
expr(A) ::= expr(B) MOD(D) expr(C). {
    A = Ast::binaryop("prefab", "int_mod", B, C, D);
}
expr(A) ::= expr(B) SEMICOLON expr(C). {
    A = Ast::Cons(Box::new(B), Box::new(C));
}
expr(A) ::= expr(B) AND(D) expr(C). {
    A = Ast::binaryop("prefab", "boolean_and", B, C, D);
}
expr(A) ::= expr(B) OR(D) expr(C). {
    A = Ast::binaryop("prefab", "boolean_or", B, C, D);
}
expr(A) ::= expr(B) XOR(D) expr(C). {
    A = Ast::binaryop("prefab", "boolean_xor", B, C, D);
}

/* comparisons */
expr(A) ::= expr(B) LT(D) expr(C). {
    A = Ast::binaryop("prefab", "less_than", B, C, D);
}
expr(A) ::= expr(B) LTEQ(D) expr(C). {
    A = Ast::binaryop("prefab", "less_than_equal", B, C, D);
}
expr(A) ::= expr(B) GT(D) expr(C). {
    A = Ast::binaryop("prefab", "greater_than", B, C, D);
}
expr(A) ::= expr(B) GTEQ(D) expr(C). {
    A = Ast::binaryop("prefab", "greater_than_equal", B, C, D);
}
expr(A) ::= expr(B) EQ(P) expr(C). {
    A = Ast::binaryop("prefab", "equal", B, C, P);
}
expr(A) ::= expr(B) NEQ(P) expr(C). {
    let inner_call = Ast::binaryop("prefab", "equal", B, C, P);
    let mut inner_op = LinkedList::new();
    inner_op.push_back(Kxpr::new_x(inner_call));
    let not_call = Ast::Modid(Lstr::Sref("prefab"), Lstr::Sref("bool_not"), P);
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
term(A) ::= id_spec(B). {
    A = B;
}
term(A) ::= map(B). {
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
    A = Ast::ConstHashtag(Lstr::from(B.data));
}
term(A) ::= strexpr(B). { A = B; }
term(A) ::= QUESTION. { A = Ast::Question; }
term(A) ::= UNDERSCORE. { A = Ast::Wildcard; }


/* map
 * {}
 */
map(A) ::= CurlyL(Z) x_list(B) CurlyR. {
    let modid = Ast::Modid(Lstr::Sref("map"), Lstr::Sref("new"), Z);
    A = Ast::Call(Box::new(modid), B, Z);
}

list(A) ::= SquareL expr_list(B) SquareR. {
    A = Ast::List(B);
}

/* tuple
 * (4 + 4, 6 - 7)
 */
tuple(A) ::= LPAREN x_list(B) RPAREN. {
    A = Ast::Tuple(B);
}

localid(A) ::= ID(B). {
    A = Ast::Localid(Lstr::from(B.data), B.loc);
}

modid(A) ::= ID(B) DBLCOLON ID(C). {
    A = Ast::Modid(Lstr::from(B.data), Lstr::from(C.data), B.loc);
}

local_generic(A) ::= ID(B) SquareCall(Z) typex_list(C) SquareR. {
    A = Ast::LocalGeneric(Lstr::from(B.data), C, B.loc);
}

localid_spec(A) ::= localid(B). {
    A = B;
}
localid_spec(A) ::= localid(B) SquareCall(Z) typex_list(C) SquareR. {
    A = Ast::TypeCall(Box::new(B), C, Z);
}

modid_spec(A) ::= modid(B). {
    A = B;
}
modid_spec(A) ::= modid(B) SquareCall(Z) typex_list(C) SquareR. {
    A = Ast::TypeCall(Box::new(B), C, Z);
}

id_spec(A) ::= localid_spec(B). {
    A = B;
}
id_spec(A) ::= modid_spec(B). {
    A = B;
}


/** 3 list types
expr_list: list of bare expressions
k_list: list of keys, each key might have an expr
x_list: list of expressions, each expr might have a key
*/

expr_list(A) ::= . {
    A = LinkedList::new();
}
expr_list(A) ::= expr(B). {
    let mut tmp = LinkedList::new();
    tmp.push_front(B);
    A = tmp;
}
expr_list(A) ::= expr(B) COMMA expr_list(C). {
    let mut tmp = C;
    tmp.push_front(B);
    A = tmp;
}

k_list(A) ::= . {
    A = LinkedList::new();
}
k_list(A) ::= k_maybe_x(B). {
    let mut tmp = LinkedList::new();
    tmp.push_back(B);
    A = tmp;
}
k_list(A) ::= k_maybe_x(B) COMMA k_list(C). {
    let mut tmp = C;
    tmp.push_front(B);
    A = tmp;
}
k_maybe_x(A) ::= ID(B). {
    A = Kxpr::new_k(Lstr::from(B.data));
}
k_maybe_x(A) ::= ID(B) COLON expr(C). {
    A = Kxpr::new(Lstr::from(B.data), C);
}

x_list(A) ::= . {
    A = LinkedList::new();
}
x_list(A) ::= x_maybe_k(B). {
    let mut tmp = LinkedList::new();
    tmp.push_front(B);
    A = tmp;
}
x_list(A) ::= x_maybe_k(B) COMMA x_list(C). {
    let mut tmp = C;
    tmp.push_front(B);
    A = tmp;
}
x_maybe_k(A) ::= expr(B). {
    A = Kxpr::new_x(B);
}
x_maybe_k(A) ::= ID(B) COLON expr(C). {
    A = Kxpr::new(Lstr::from(B.data), C);
}


strexpr(A) ::= StrOpen(L) StrClose. {
    A = Ast::ConstStr(Lstr::empty());
}
strexpr(A) ::= StrOpen(L) strconst(B) StrClose. {
    A = Ast::ConstStr(Lstr::from(B));
}
strexpr(A) ::= StrOpen(C) strlist(B) StrClose. {
    A = Ast::StrExpr(B, C);
    vout!("strexpr({:?})\n", A);
}
strlist(A) ::= expr(B). {
    A = vec![B];
}
strlist(A) ::= strconst(B) expr(C). {
    A = vec![Ast::ConstStr(Lstr::from(B)), C];
}
strlist(A) ::= strlist(B) StrLit(C). {
    let mut tmp = B;
    tmp.push(Ast::ConstStr(Lstr::from(C)));
    A = tmp;
}
strlist(A) ::= strlist(B) expr(C). {
    let mut tmp = B;
    tmp.push(C);
    A = tmp;
}
strconst(A) ::= StrLit(B). {
    A = B;
}
strconst(A) ::= strconst(B) StrLit(C). {
    let mut tmp = B;
    tmp.push_str(&C);
    A = tmp;
}
