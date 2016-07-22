%include {
use leema::ast::{Ast, TokenLoc, TokenData};
use leema::val::{Val, SexprType, Type};
use leema::list;
use leema::log;
use leema::sexpr;
use std::sync::Arc;
use std::io::{stderr, Write};
}

%start_symbol {program}
%derive_token {Debug,PartialEq}
%wildcard ANY.
%extra_argument { Result<Ast, i32> }

%type HASHTAG { TokenData<String> }
%type ID { String }
%type INT { i64 }
%type NEWLINE { TokenLoc }
%type StrLit { String }
%type TYPE_ID { String }

%type program { Ast }
%type stmts { Val }

%type pattern_args { Val }
%type more_pattern_args { Val }
%type stmt { Val }
%type match_block { Val }
%type block { Val }
%type arrow_block { Val }
%type curly_block { Val }
%type pattern { Val }
%type func_stmt { Val }
%type dfunc_args { Val }
%type dfunc_1 { Val }
%type macro_stmt { Val }
%type macro_args { Val }
%type let_stmt { Val }
%type expr_stmt { Val }
%type fail_stmt { Val }
%type term { Val }
%type expr { Val }
%type typex { Type }
%type opt_typex { Type }
%type id_type { Val }
%type if_expr { Val }
%type else_case { Val }
%type elses { Val }
/* %type var_field { Ast } */

%type list { Val }
%type list_items { Val }
%type tuple { Val }
%type tuple_args { Val }
%type strexpr { Val }
%type strlist { Val }


%nonassoc ASSIGN BLOCKARROW WHERE GIVEN.
%left COMMA.
%left OR XOR.
%left AND.
%right ConcatNewline NOT.
%left LT LTEQ.
%nonassoc EQ NEQ GT GTEQ.
%left PLUS MINUS.
%left TIMES DIVIDE.
%nonassoc LPAREN RPAREN.

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

program(A) ::= FAILED WITH CONTEXTID. {
	A = Ast::Nothing;
}
program(A) ::= stmts(B). {
	if list::is_empty(&B) {
		panic!("null program");
	}
	// ignore A, it doesn't really go anywhere for program
	A = Ast::Nothing;
	// we're done, so put B in extra
	self.extra = Ok(Ast::ReplRoot(B));
}

stmts(A) ::= . {
	A = sexpr::new(SexprType::BlockExpr, list::empty());
}
stmts(A) ::= stmt(C) NEWLINE stmts(B). {
	A = list::cons(C, B);
}


/* stmt(A) ::= short_func_stmt(B). { A = B; } */
/* stmt(A) ::= matched_func_stmt(B). { A = B; } */
stmt(A) ::= let_stmt(B). { A = B; }
stmt(A) ::= expr_stmt(B). {
    A = B;
}
stmt(A) ::= fail_stmt(B). { A = B; }
/* stmt(A) ::= data_decl(B). { A = B; } */
stmt(A) ::= DT. { A = Val::Void; }
stmt(A) ::= func_stmt(B). { A = B; }
stmt(A) ::= macro_stmt(B). { A = B; }

/*
stmt(A) ::= FAILED(B) ID(C) match_block(D). {
	A = new FailureStmt(C->text, D);
	A->merge_span(*B, *D);
}
*/

/*
matched_func_stmt(A) ::= long_func_decl(B) match_block(C). {
	A = B;
	A->match_block = C;
	cerr << "function, with matching: " << B->func_name << endl;
}
long_func_decl(A) ::= ID(B) FUNCDECL(C). {
	A = new MatchFuncDefStmt(B->text);
	A->merge_span(*B, *C);
	cerr << "func decl, no type: " << B->text <<" @ ";
	cerr << B->span_front << endl;
}
long_func_decl(A) ::= ID(B) typedecl(C) FUNCDECL(D). {
	A = new MatchFuncDefStmt(B->text);
	A->type = C;
	A->merge_span(*B, *D);
	cerr << "func decl, with type: "<< B->text <<" @ ";
	cerr << A->span_str() << endl;
}

match_block(A) ::= match_case(B). {
	A = list::singleton(B);
}
match_block(A) ::= match_case(B) match_block(C). {
	A = list::cons(B, C);
}
match_case(A) ::= pattern(B) arrow_block(D) NEWLINE. {
	A = Val::tuple2(B, D);
}

pattern(A) ::= pexpr(B). {
	A = Val::list(list::singleton(B));
}
pattern(A) ::= LPAREN pattern_args(B) RPAREN. {
	A = Val::list(B);
}
pattern_args(A) ::= pexpr(B) more_pattern_args(C). {
	A = list::cons(B, C);
}
more_pattern_args(A) ::= . {
	A = NULL;
}
more_pattern_args(A) ::= COMMA pexpr(B) more_pattern_args(C). {
	A = list::cons(B, C);
}

pexpr(A) ::= ID(B). {
	A = B;
}
pexpr(A) ::= INT(B). {
	A = B;
}
*/


/** Data Types
 *
 * Algebraic Data Types or simple structures (which are just ADTs with
 * only one variant whose name matches the data type.
data_decl(A) ::= DT TYPEID(B) NEWLINE struct_fields(C) END_BLOCK. {
	/* just pretend that there are more than one * /
	Val *constructor = Val::tuple2(B, C);
	Val *constructors(Val::list(list::singleton(constructor)));
	A = Val::dtype(B, constructors);
}
data_decl(A) ::= DT TYPEID(B) NEWLINE constructors(C) END_BLOCK. {
	A = Val::dtype(B, C);
}

constructors(A) ::= constructor(B). {
	A = Val::list(list::singleton(B));
}
constructors(A) ::= constructor(B) constructors(C). {
	A = Val::cons(B, C);
}

constructor(A) ::= TYPEID(B) NEWLINE struct_fields(C). {
	A = Val::tuple2(B, C);
}

struct_fields(A) ::= struct_field(B) NEWLINE struct_fields(C). {
	A = Val::cons(B, C);
}
struct_fields(A) ::= . {
	A = Val::empty_list();
}
struct_field(A) ::= DOT ID(C) TYPEDECL TYPEID(D). {
	A = Val::tuple(
		list::cons(C,
		list::cons(D,
		NULL)));
}
 */


/*
result_stmt(A) ::= expr_stmt(B). { A = B; }
result_stmt(A) ::= fail_stmt(B). { A = B; }
*/
fail_stmt(A) ::= FAIL(B) HASHTAG(C) term(D). {
verbose_out!("found fail_stmt {:?}\n", C);
	/*
	A = Val::list(
		list::push(B,
		list::push(C,
		list::push(D,
		NULL))));
		*/
	A = Val::Failure;
}

let_stmt(A) ::= Let ID(B) EQ expr(C). {
	let bind = list::cons(Val::new_str(B), list::singleton(C));
	A = sexpr::new(SexprType::Bind, bind);
}
let_stmt(A) ::= Fork ID(B) EQ expr(C). {
	let bind = list::cons(Val::new_str(B), list::singleton(C));
	A = sexpr::new(SexprType::Fork, bind);
}

expr_stmt(A) ::= expr(B). {
	A = B;
}
expr_stmt(A) ::= DollarGT expr(B). { A = B; }

block(A) ::= arrow_block(B). {
	A = B;
}
block(A) ::= curly_block(B). {
	A = B;
}
arrow_block(A) ::= BLOCKARROW expr(B). {
	A = B;
}
curly_block(A) ::= CurlyL NEWLINE stmts(B) CurlyR. {
	A = B;
}

/* alternate syntax for defining a short function */
func_stmt(A) ::= Func dfunc_1(B).
{
	/*
	const Val *plist = Val::list(p);
	const Val *mcase = Val::tuple2(plist, C);
	const list *mblk = list::singleton(mcase);

	Val *func = Val::fexpr(mblk, T);
	A = Val::tuple2(B, func, LET_ASSIGN);
	*/
	A = Val::Sexpr(SexprType::DefFunc, Box::new(B));
}

dfunc_1(A) ::= ID(B) LPAREN dfunc_args(D) RPAREN opt_typex(E)
	block(C).
{
	/*
	const Val *plist = Val::list(p);
	const Val *mcase = Val::tuple2(plist, C);
	const list *mblk = list::singleton(mcase);

	Val *func = Val::fexpr(mblk, T);
	A = Val::tuple2(B, func, LET_ASSIGN);
	*/
	let id = Val::id(B);
	let typ = Val::Type(E);
	A = sexpr::single_func_list(id, D, typ, C)
}

dfunc_args(A) ::= . {
	A = list::empty();
}
dfunc_args(A) ::= ID(B) opt_typex(C). {
	A = list::singleton(sexpr::id_with_type(B, C));
}
dfunc_args(A) ::= ID(B) opt_typex(C) COMMA dfunc_args(D). {
	A = list::cons(sexpr::id_with_type(B, C), D);
}



opt_typex(A) ::= . {
	A = Type::AnonVar;
}
opt_typex(A) ::= COLON typex(B). {
	A = B;
}

typex(A) ::= TYPE_INT. {
	A = Type::Int;
}
typex(A) ::= TYPE_STR. {
	A = Type::Str;
}
typex(A) ::= TYPE_BOOL. {
	A = Type::Bool;
}
typex(A) ::= TYPE_ID(B). {
	A = Type::User(B);
}


/* defining a macro */
macro_stmt(A) ::= MACRO ID(B) LPAREN macro_args(D) RPAREN block(C). {
    A = sexpr::new(SexprType::DefMacro,
        list::cons(Val::id(B),
        list::cons(D,
        list::cons(C,
        Val::Nil
    ))));
}

macro_args(A) ::= . {
    A = Val::Nil;
}
macro_args(A) ::= ID(B). {
    A = list::singleton(Val::id(B));
}
macro_args(A) ::= ID(B) COMMA macro_args(C). {
    A = list::cons(Val::id(B), C);
}


/* function with pattern matching
func_stmt(A) ::= F ID(B) typedecl(T) NEWLINE match_block(C) END_BLOCK. {
	Val *func = Val::fexpr(C, T);
	A = Val::tuple2(B, func, LET_ASSIGN);
}

idtype(A) ::= ID(B). {
	A = Sexpr::new_idtype(B, Type::AnonVar);
}
idtype(A) ::= ID(B) COLON texpr(C). {
	A = sexpr::idtype(B, C);
}
*/

/* regular function call */
expr(A) ::= ID(B) LPAREN RPAREN. {
	println!("zero param function call!");
	A = sexpr::call(B, Val::Nil);
}
expr(A) ::= ID(B) LPAREN expr(C) RPAREN. {
	let args = list::singleton(C);
	A = sexpr::call(B, args);
}
expr(A) ::= ID(B) LPAREN tuple_args(C) RPAREN. {
	A = sexpr::call(B, C);
}
/* postfix function call, are we really doing this?
seems like this should be pretty achievable w/ `[] | empty?`
expr(A) ::= term(B) ID(C). {
	A = Sexpr::Nothing;
}*/
/* infix function call */
expr(A) ::= term(B) ID(C) term(D). {
	/* A = Val::binaryop(B, C, D); */
	A = Val::Void;
}
expr(A) ::= term(B) DOLLAR term(C). {
	/* A = Val::binaryop(B, C, D); */
	A = Val::Void;
}
/* IF expression */
expr(A) ::= IF if_expr(B). {
	A = B;
}
/*
if_expr(A) ::= expr(B) arrow_block(C). {
	A = sexpr::ifexpr(B, C, Val::Void);
}
*/
if_expr(A) ::= expr(B) curly_block(C) ELSE curly_block(D). {
	A = sexpr::ifexpr(B, C, D);
}
if_expr(A) ::= expr(B) curly_block(C) ELSE IF if_expr(D). {
	A = sexpr::ifexpr(B, C, D);
}

expr(A) ::= list(B). { A = B; }
/* tuple
 * (4 + 4, 6 - 7)
 */
expr(A) ::= tuple(B). { A = B; }
expr(A) ::= NOT expr(B). {
	A = sexpr::call("bool_not".to_string(), list::singleton(B));
}
expr(A) ::= expr(B) ConcatNewline. {
	let newline = Val::Str(Arc::new("\n".to_string()));
	let args = list::cons(B, list::singleton(newline));
	A = sexpr::new(SexprType::StrExpr, args)
}
/* arithmetic */
expr(A) ::= MINUS expr(B). {
	A = sexpr::call("negate".to_string(), list::singleton(B));
}
expr(A) ::= expr(B) PLUS expr(C). {
	A = sexpr::binaryop("int_add".to_string(), B, C);
}
expr(A) ::= expr(B) MINUS expr(C). {
	A = sexpr::binaryop("int_sub".to_string(), B, C);
}
expr(A) ::= expr(B) TIMES expr(C). {
	A = sexpr::binaryop("int_mult".to_string(), B, C);
}
expr(A) ::= expr(B) DIVIDE expr(C). {
	A = sexpr::binaryop("int_div".to_string(), B, C);
}
expr(A) ::= expr(B) AND expr(C). {
	A = sexpr::binaryop("and".to_string(), B, C);
}
expr(A) ::= expr(B) OR expr(C). {
	A = sexpr::binaryop("or".to_string(), B, C);
}
expr(A) ::= expr(B) XOR expr(C). {
	A = sexpr::binaryop("xor".to_string(),B, C);
}

/* comparisons */
expr(A) ::= expr(B) LT expr(C). {
	A = sexpr::binaryop("less_than".to_string(), B, C);
}
expr(A) ::= expr(B) LTEQ expr(C). {
	A = sexpr::binaryop("less_than_equal".to_string(), B, C);
}
expr(A) ::= expr(B) GT expr(C). {
	A = sexpr::binaryop("greater_than".to_string(), B, C);
}
expr(A) ::= expr(B) GTEQ expr(C). {
	A = sexpr::binaryop("greater_than_equal".to_string(), B, C);
}
expr(A) ::= expr(B) EQ(P) expr(C). {
	A = sexpr::binaryop("equal".to_string(), B, C);
}
expr(A) ::= expr(B) NEQ(P) expr(C). {
	let eq = sexpr::binaryop("equal".to_string(), B, C);
	A = sexpr::call("bool_not".to_string(), list::singleton(eq));
}
/*
expr(A) ::= expr(B) LT expr(C) LT expr(D). {
	let l1 = sexpr::binaryop("less_than".to_string(), B, C);
	let l2 = sexpr::binaryop("less_than".to_string(), C, D);
	A = sexpr::binaryop("and".to_string(), l1, l2);
}
expr(A) ::= expr(B) LT expr(C) LTEQ expr(D). {
	let l1 = sexpr::binaryop("less_than".to_string(), B, C);
	let l2 = sexpr::binaryop("less_than_equal".to_string(), C, D);
	A = sexpr::binaryop("and".to_string(), l1, l2);
}
expr(A) ::= expr(B) LTEQ expr(C) LT expr(D). {
	let l1 = sexpr::binaryop("less_than_equal".to_string(), B, C);
	let l2 = sexpr::binaryop("less_than".to_string(), C, D);
	A = sexpr::binaryop("and".to_string(), l1, l2);
}
expr(A) ::= expr(B) LTEQ expr(C) LTEQ expr(D). {
	let l1 = sexpr::binaryop("less_than_equal".to_string(), B, C);
	let l2 = sexpr::binaryop("less_than_equal".to_string(), C, D);
	A = sexpr::binaryop("and".to_string(), l1, l2);
}
*/

expr(A) ::= term(B). { A = B; }


term(A) ::= LPAREN expr(C) RPAREN. {
	A = C;
}
term(A) ::= ID(B). { A = Val::id(B); }
/* term(A) ::= var_field(B). { A = Ast::Nothing; } */
term(A) ::= VOID. {
	A = Val::Void;
}
term(A) ::= DollarQuestion. {
	A = Val::id("$".to_string());
}
term(A) ::= INT(B). {
	A = Val::Int(B);
}
term(A) ::= True. { A = Val::Bool(true); }
term(A) ::= False. { A = Val::Bool(false); }
term(A) ::= HASHTAG(B). {
	A = Val::Hashtag(Arc::new(B.data));
}
term(A) ::= strexpr(B). { A = B; }

	/*
var_field(A) ::= ID(B) DOT ID(C) var_field_depth(D). {
	A = Val::list(
		list::cons(B,
		list::cons(C,
		D->data.list)), LET_SUBSYMBOL);
}
var_field_depth(A) ::= . {
	// A = Val::empty_list(LET_SUBSYMBOL);
}
var_field_depth(A) ::= DOT ID(B) var_field_depth(C). {
	// A = Val::cons(B, C);
}
		*/

list(A) ::= SquareL list_items(B) SquareR. {
	A = B;
}
list_items(A) ::= . {
	A = list::empty();
}
list_items(A) ::= expr(B). {
	A = list::singleton(B);
}
list_items(A) ::= expr(B) COMMA list_items(C). {
	A = list::cons(B, C);
}

tuple(A) ::= LPAREN tuple_args(B) RPAREN. {
	A = Val::tuple_from_list(B);
}
tuple_args(A) ::= term(B) COMMA term(C). {
	A = list::cons(B, list::singleton(C));
}
tuple_args(A) ::= term(B) COMMA tuple_args(C). {
	A = list::cons(B, C);
}

strexpr(A) ::= StrOpen strlist(B) StrClose. {
	A = sexpr::strexpr(B);
}
strlist(A) ::= . {
	A = Val::Nil;
}
strlist(A) ::= StrLit(B) strlist(C). {
	A = list::cons(Val::new_str(B), C);
}
strlist(A) ::= ID(B) strlist(C). {
	A = list::cons(Val::id(B), C);
}
