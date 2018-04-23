%include {
use leema::ast::{Ast, TokenData};
use leema::val::{Val, SxprType, Type, SrcLoc};
use leema::list;
use leema::log;
use leema::sxpr;
use std::io::{Write};
use std::rc::{Rc};
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
%type dfunc_args { Vec<Ast> }
%type dfunc_one { Ast }
%type dfunc_many { Ast }
%type failed_stmt { Ast }
%type macro_stmt { Ast }
%type call_expr { Ast }
%type call_args { Vec<Ast> }
%type call_arg { Ast }
%type if_stmt { Ast }
%type else_if { Ast }
%type if_expr { Ast }
%type if_case { ast::IfCase }
%type keyed_lris { Vec<Ast> }
%type keyed_types { Vec<Ast> }
%type lri { Ast }
%type lri_base { Vec<Lstr> }
%type lri_list { Vec<Ast> }
%type match_expr { Ast }
%type defstruct { Ast }
%type defstruct_fields { Vec<Ast> }
%type defstruct_field { Ast }
%type defenum { Ast }
%type defenum_variants { Vec<Ast> }
%type defenum_variant { Ast }
%type defnamedtuple { Ast }
%type let_stmt { Ast }
%type term { Ast }
%type expr { Ast }
%type typex { Ast }
%type opt_typex { Ast }
%type arrow_typex { Vec<Ast> }
%type type_term { Ast }
%type mod_type { Ast }

%type list { Ast }
%type list_items { Vec<Ast> }
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
    A = B;
    A.push(C);
}

block(A) ::= BLOCKARROW(C) stmts(B). {
    A = Ast::Block(B); // , C);
}


stmt(A) ::= defstruct(B). { A = B; }
stmt(A) ::= defenum(B). { A = B; }
stmt(A) ::= defnamedtuple(B). { A = B; }
stmt(A) ::= IMPORT(C) lri(B). {
    A = sxpr::new_import(B, C);
}
stmt(A) ::= let_stmt(B). { A = B; }
stmt(A) ::= failed_stmt(B). { A = B; }
stmt(A) ::= func_stmt(B). { A = B; }
stmt(A) ::= macro_stmt(B). { A = B; }
stmt(A) ::= if_stmt(B). { A = B; }
stmt(A) ::= expr(B). { A = B; }
stmt(A) ::= RETURN(C) expr(B). {
    A = Ast::Return(B, C);
}


/** Data Structures */
defstruct(A) ::= STRUCT(D) lri(B) defstruct_fields(C) DOUBLEDASH. {
    A = Ast::DefData(ast::DataType::Struct, B, C, D);
}
defstruct_fields(A) ::= defstruct_fields(B) defstruct_field(C). {
    A = B;
    A.push(C);
}
defstruct_fields(A) ::= . {
    A = Vec::new();
}
defstruct_field(A) ::= DOT ID(B) COLON typex(C). {
    A = Val::typed_id(&B.data, C);
}

/** Enum Definitions */
defenum(A) ::= ENUM(D) lri(B) defenum_variants(C) DOUBLEDASH. {
    A = Ast::DefData(ast::DataType::Enum, B, C, D);
}
defenum_variants(A) ::= defenum_variants(B) defenum_variant(C). {
    A = B;
    A.push(C);
}
defenum_variants(A) ::= defenum_variant(B). {
    A = vec![B];
}
defenum_variant(A) ::= PIPE(D) ID(B) PARENCALL lri_list(C) RPAREN. {
    A = Ast::DefData(ast::DataType::NamedTuple, B, C, D);
}
defenum_variant(A) ::= PIPE(D) ID(B) defstruct_fields(C). {
    let variant_name = Ast::Lri(vec![Lstr::new_str(B.data)], None);
    A = Ast::DefData(ast::DataType::Enum, variant_name, C, D);
}

/** named tuple definition **/
defnamedtuple(A) ::= STRUCT(B) typex(C) PARENCALL lri_list(D) RPAREN.
{
    A = sxpr::defnamedtuple(C, D, B);
}


failed_stmt(A) ::= FAILED ID(B) if_case(C) DOUBLEDASH. {
    let var = Val::id(B.data);
    A = sxpr::match_failed(var, C, B.loc);
}

let_stmt(A) ::= Let(D) expr(B) ASSIGN expr(C). {
    A = Ast::Let(ast::LetType::Inline, B, C, D);
}
let_stmt(A) ::= Fork(D) expr(B) ASSIGN expr(C). {
    A = Ast::Let(ast::LetType::Forked, B, C, D);
}
let_stmt ::= Let expr EQ1(A) expr. {
    panic!("Found let x =... @ {:?}\ninstead it should be let x := ...\n", A);
}

/* rust func declaration */
func_stmt(A) ::= Func(Z) lri(B) PARENCALL dfunc_args(D) RPAREN COLON typex(E)
    RUSTBLOCK.
{
	let typ = Val::Type(E);
	A = sxpr::defunc(B, D, typ, Val::RustBlock, Z)
}

/* func one case, no matching */
func_stmt(A) ::= Func(Z) lri(B) PARENCALL dfunc_args(D) RPAREN opt_typex(E)
    block(C) DOUBLEDASH.
{
	let typ = Val::Type(E);
	A = sxpr::defunc(B, D, typ, C, Z)
}
/* func w/ pattern matching */
func_stmt(A) ::= Func(Z) lri(B) PARENCALL dfunc_args(C) RPAREN opt_typex(D)
    if_case(E) DOUBLEDASH.
{
    let typ = Val::Type(D);
    // extract field names from args to pass them through to match expr
    let args = Val::Tuple(list::map_ref_to_vec(&C, |arg| {
        Val::Id(arg.id_name())
    }));
    let body = sxpr::match_expr(args, E, Z);
    A = sxpr::defunc(B, C, typ, body, Z)
}

dfunc_args(A) ::= . {
	A = list::empty();
}
dfunc_args(A) ::= ID(B) opt_typex(C). {
	A = list::singleton(Val::typed_id(&B.data, C));
}
dfunc_args(A) ::= ID(B) opt_typex(C) COMMA dfunc_args(D). {
	A = list::cons(Val::typed_id(&B.data, C), D);
}


opt_typex(A) ::= . {
	A = Type::AnonVar;
}
opt_typex(A) ::= COLON typex(B). {
	A = B;
}

typex(A) ::= type_term(B). {
    A = B;
}
typex(A) ::= arrow_typex(B). {
    let argc = B.len() - 1;
    let mut items = B;
    let last = items.remove(argc);
    A = Type::Func(items, Box::new(last));
}

arrow_typex(A) ::= type_term(B) GT type_term(C). {
    A = vec![B, C];
}
arrow_typex(A) ::= arrow_typex(B) GT type_term(C). {
    let mut tmp = B;
    tmp.push(C);
    A = tmp;
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
	A = Type::Var(Rc::new(B.data));
}
type_term(A) ::= lri(B). {
    // A = Type::from_lri(B);
    A = Type::Unknown;
}
type_term(A) ::= SquareL typex(B) SquareR. {
	A = Type::StrictList(Box::new(B));
}
type_term(A) ::= LPAREN lri_list(B) RPAREN. {
    A = Type::Tuple(list::map_ref_to_vec(&B, |v| {
        v.to_type()
    }));
}


/* defining a macro */
macro_stmt(A) ::= MACRO ID(B) PARENCALL call_args(D) RPAREN block(C) DOUBLEDASH. {
    vout!("found macro {:?}\n", B);
    A = sxpr::new(SxprType::DefMacro,
        list::cons(Val::id(B.data),
            list::cons(D,
            list::cons(C,
            Val::Nil
        ))),
        B.loc,
    );
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
    A = sxpr::ifx(B, C, Val::Void, D);
}
if_stmt(A) ::= IF(E) expr(B) block(C) else_if(D) DOUBLEDASH. {
    /* if-else style */
    A = sxpr::ifx(B, C, D, E);
}
else_if(A) ::= ELSE IF(E) expr(B) block(C) else_if(D). {
    A = sxpr::ifx(B, C, D, E);
}
else_if(A) ::= ELSE IF(D) expr(B) block(C). {
    A = sxpr::ifx(B, C, Val::Void, D);
}
else_if(A) ::= ELSE block(B). {
    A = B;
}


/* regular function call */
call_expr(A) ::= term(B) PARENCALL(D) call_args(C) RPAREN. {
	A = sxpr::call(B, C, D);
}

call_arg(A) ::= expr(B). {
    A = B;
}
call_arg(A) ::= ID(B) COLON expr(C). {
    let name = Val::id(B.data);
    A = sxpr::named_param(name, C, B.loc);
}
call_args(A) ::= . {
    A = list::empty();
}
call_args(A) ::= call_arg(B). {
    A = list::singleton(B);
}
call_args(A) ::= call_arg(B) COMMA call_args(C). {
    A = list::cons(B, C);
}

expr(A) ::= term(B) DOLLAR term(C). {
    /* A = Val::binaryop(B, C, D); */
    A = Ast::ConstVoid;
}

/* IF expression */
if_expr(A) ::= IF(L) if_case(B) DOUBLEDASH. {
    /* case-expr style */
    A = Ast::IfExpr(ast::IfType::If, Box::new(Ast::ConstVoid), Box::new(B), L);
}
if_case(A) ::= PIPE(L) expr(B) block(C). {
    A = ast::IfCase(B, C, None, L);
}
if_case(A) ::= PIPE(L) expr(B) block(C) if_case(D). {
    A = ast::IfCase(B, C, Some(Box::new(D)), L);
}
if_case(A) ::= PIPE ELSE(L) block(B). {
    A = ast::IfCase(Ast::Wildcard, B, None);
}

/* match expression */
match_expr(A) ::= MATCH(D) expr(B) if_case(C) DOUBLEDASH. {
    A = Ast::IfExpr(ast::IfType::Match, Box::new(B), Box::new(C), D);
}


expr(A) ::= list(B). { A = B; }

expr(A) ::= NOT(C) expr(B). {
    let call = vec![Lstr::from_str("prefab"), Lstr::from_str("bool_not")];
    A = Ast::Call(Box::new(Ast::Lri(call, None)), vec![B], C);
}
expr(A) ::= expr(B) ConcatNewline(C). {
    A = Ast::StrExpr(vec![B, Ast::ConstStr(Lstr::from_str("\n"))], C);
}
/* arithmetic */
expr(A) ::= NEGATE(C) term(B). {
    let call = vec![Lstr::from_str("prefab"), Lstr::from_str("int_negate")];
    A = Ast::Call(Box::new(Ast::Lri(call, None)), vec![B], C);
}
expr(A) ::= expr(B) PLUS(D) expr(C). {
    let call = vec![Lstr::from_str("prefab"), Lstr::from_str("int_add")];
    A = Ast::binaryop(call, B, C, D);
}
expr(A) ::= expr(B) MINUS(D) expr(C). {
    let call = vec![Lstr::from_str("prefab"), Lstr::from_str("int_sub")];
    A = Ast::binaryop(call, B, C, D);
}
expr(A) ::= expr(B) TIMES(D) expr(C). {
    let call = vec![Lstr::from_str("prefab"), Lstr::from_str("int_mult")];
    A = Ast::binaryop(call, B, C, D);
}
expr(A) ::= expr(B) SLASH(D) expr(C). {
    let call = vec![Lstr::from_str("prefab"), Lstr::from_str("int_div")];
    A = Ast::binaryop(call, B, C, D);
}
expr(A) ::= expr(B) MOD(D) expr(C). {
    let call = vec![Lstr::from_str("prefab"), Lstr::from_str("int_mod")];
    A = Ast::binaryop(call, B, C, D);
}
expr(A) ::= expr(B) SEMICOLON expr(C). {
    A = Ast::Cons(Box::new(B), Box::new(C));
}
expr(A) ::= expr(B) AND(D) expr(C). {
    let call = vec![Lstr::from_str("prefab"), Lstr::from_str("boolean_and")];
    A = Ast::binaryop(call, B, C, D);
}
expr(A) ::= expr(B) OR(D) expr(C). {
    let call = vec![Lstr::from_str("prefab"), Lstr::from_str("boolean_or")];
    A = Ast::binaryop(call, B, C, D);
}
expr(A) ::= expr(B) XOR(D) expr(C). {
    let call = vec![Lstr::from_str("prefab"), Lstr::from_str("boolean_xor")];
    A = Ast::binaryop(call, B, C, D);
}

/* comparisons */
expr(A) ::= expr(B) LT(D) expr(C). {
    let call = vec![Lstr::from_str("prefab"), Lstr::from_str("less_than")];
    A = Ast::binaryop(call, B, C, D);
}
expr(A) ::= expr(B) LTEQ(D) expr(C). {
    let call =
        vec![Lstr::from_str("prefab"), Lstr::from_str("less_than_equal")];
    A = Ast::binaryop(call, B, C, D);
}
expr(A) ::= expr(B) GT(D) expr(C). {
    let call = vec![Lstr::from_str("prefab"), Lstr::from_str("greater_than")];
    A = Ast::binaryop(call, B, C, D);
}
expr(A) ::= expr(B) GTEQ(D) expr(C). {
    let call =
        vec![Lstr::from_str("prefab"), Lstr::from_str("greater_than_equal")];
    A = Ast::binaryop(call, B, C, D);
}
expr(A) ::= expr(B) EQ(P) expr(C). {
    let call = vec![Lstr::from_str("prefab"), Lstr::from_str("equal")];
    A = Ast::binaryop(call, B, C, P);
}
expr(A) ::= expr(B) NEQ(P) expr(C). {
    let inner_call = vec![Lstr::from_str("prefab"), Lstr::from_str("equal")];
    let inner_op = Ast::binaryop(inner_call, B, C, P);
    let not_call = Ast::Lri(vec![Lstr::from_str("bool_not")], None);
    A = Ast::Call(Box::new(not_call), vec![inner_op], P);
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
term(A) ::= term(B) DOT ID(C). {
    A = Ast::DotAccess(Box::new(B), Lstr::new_str(C.data));
}

list(A) ::= SquareL SquareR. {
    A = Ast::List(Vec::with_capacity(0));
}
list(A) ::= SquareL list_items(B) SquareR. {
    A = Ast::List(B);
}
list_items(A) ::= expr(B). {
    A = vec![B];
}
list_items(A) ::= list_items(B) COMMA expr(C). {
    A = B;
    A.push(C);
}

/* tuple
 * (4 + 4, 6 - 7)
 */
tuple(A) ::= LPAREN call_args(B) RPAREN. {
    A = Ast::Tuple(B);
}


lri(A) ::= lri_base(B). {
    A = Ast::Lri(B, None);
}
lri(A) ::= lri_base(B) SquareCall(D) lri_list(C) SquareR. {
    A = Ast::Lri(B, Some(C)); // , D);
}
lri_base(A) ::= ID(B). {
println!("found lri: {}", B.data);
    A = vec![Lstr::new_str(B.data)];
}
lri_base(A) ::= lri_base(B) DBLCOLON ID(C). {
    A = B;
    A.push(Lstr::new_str(C.data));
}


key w/ optional expr
expr w/ optional key

id_list(A) ::= . {
    A = LinkedList::new();
}
id_list(A) ::= id_item(B) COMMA id_list(C). {
    A = C;
    A.push_front(B);
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
expr_list(A) ::= expr(B) COMMA expr_list(C). {
    A = C;
    A.push_front(B);
}
expr_list(A) ::= keyed_expr(B) COMMA expr_list(C). {
    A = C;
    A.push_front(B);
}
keyed_expr(A) ::= ID(B) COLON expr(C). {
    A = Ast::KeyedExpr(Lstr::from_string(B.data), Box::new(C), B.loc);
}


lri_list(A) ::= . {
    A = Vec::new();
}
lri_list(A) ::= lri(B). {
    A = vec![B];
}
lri_list(A) ::= lri_list(B) COMMA lri(C). {
    A = B;
    A.push(C);
}

keyed_lri_list(A) ::= . {
    A = Vec::new();
}
keyed_lri_list(A) ::= keyed_lri(B). {
    A = vec![B];
}
keyed_lri_list(A) ::= keyed_lri_list(B) COMMA. {
    A = B;
}
keyed_lri_list(A) ::= keyed_lri_list(B) COMMA keyed_lri(C). {
    A = B;
    B.push(C);
}
keyed_lri(A) ::= ID(B) COLON lri(C). {
    A = vec![B];
}


strexpr(A) ::= StrOpen(C) strlist(B) StrClose. {
    A = Ast::StrExpr(B, C);
    vout!("strexpr({:?})\n", A);
}
strlist(A) ::= . {
    A = Vec::new();
}
strlist(A) ::= strlist(B) StrLit(C). {
    A = B;
    A.push(Ast::ConstStr(Lstr::new(C)));
}
strlist(A) ::= strlist(B) term(C). {
    A = B;
    A.push(C);
}
