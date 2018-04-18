
%top {
#include <stdio.h>
#include "leema.h"


typedef void * yyscan_t;

int set_token(yyscan_t, int tok);
int set_token_val(yyscan_t, int tok, const char *val);
void set_newline(yyscan_t);
void skip_token(yyscan_t);
int add_block_comment_depth(yyscan_t, int delta);
}

DIGIT	[0-9]
INT	[0-9]+
ID      [a-zA-Z_][a-zA-Z_0-9]*[?']?
HASHTAG	#[a-zA-Z_0-9]+[?!]?
LINECOMMENT	##.*
STRC	[^\$\\\"]
SPACE	[ \t]
EOL	(\n|\r|\r\n)
/* block comment char */
BCC	[^#\\]*
EOI	\0

%x STR
%x STRID
%x STREXPR
%x LINECOMMENT
%x BLOCKCOMMENT
%s CALLPAREN

%option reentrant
%option noyywrap
%option yylineno
%option outfile="lexparse/lex.c"
/* %option nodefault */

%%


<<EOF>>	{
	return set_token(yyg, TOKEN_EOI);
}
{EOL}   {
    BEGIN(INITIAL);
    set_newline(yyg);
}
"\""	{
	BEGIN(STR);
	return set_token(yyg, TOKEN_StrOpen);
}
{INT}	{
    BEGIN(INITIAL);
	return set_token_val(yyg, TOKEN_INT, yytext);
}
void	{ return set_token(yyg, TOKEN_VOID); }

{SPACE}+	{
    BEGIN(INITIAL);
}
"\."	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_DOT);
}
"-RUST-"	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_RUSTBLOCK);
}
"->"	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_BLOCKARROW);
}
"--"	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_DOUBLEDASH);
}
":="	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_ASSIGN);
}
"=="	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_EQ);
}
"="	{
    // only here to make it easier to report errors that it cannot be used
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_EQ1);
}
"<"	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_LT);
}
"<="	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_LTEQ);
}
">"	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_GT);
}
">="	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_GTEQ);
}
"!="	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_NEQ);
}
<INITIAL>"("	{
    return set_token(yyg, TOKEN_LPAREN);
}
<CALLPAREN>"("	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_PARENCALL);
}
")"	{
    BEGIN(CALLPAREN);
    return set_token(yyg, TOKEN_RPAREN);
}
<INITIAL>"["	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_SquareL);
}
<CALLPAREN>"["	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_SquareCall);
}
"]"	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_SquareR);
}
","	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_COMMA);
}
";"	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_SEMICOLON);
}
"::"	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_DBLCOLON);
}
":"	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_COLON);
}
"~"	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_NEGATE);
}
"+"	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_PLUS);
}
"-"	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_MINUS);
}
"*"	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_TIMES);
}
"/"	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_SLASH);
}
"|"	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_PIPE);
}
"_"	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_UNDERSCORE);
}
"\\n"	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_ConcatNewline);
}
else	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_ELSE);
}
enum	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_ENUM);
}
failed	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_FAILED);
}
func	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_Func);
}
if	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_IF);
}
import	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_IMPORT);
}
true	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_True);
}
false	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_False);
}
fork	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_Fork);
}
let	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_Let);
}
macro	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_MACRO);
}
match	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_MATCH);
}
mod	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_MOD);
}
struct	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_STRUCT);
}
return  {
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_RETURN);
}
and	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_AND);
}
or	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_OR);
}
xor	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_XOR);
}
not	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_NOT);
}
Int	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_TYPE_INT);
}
Str	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_TYPE_STR);
}
Bool	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_TYPE_BOOL);
}
Void	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_TYPE_VOID);
}
"#:"	{
	add_block_comment_depth(yyg, +1);
	BEGIN(BLOCKCOMMENT);
}
{LINECOMMENT}	{
	// have to update the cursor info here? hopefully not
}
{HASHTAG}	{ return set_token_val(yyg, TOKEN_HASHTAG, yytext); }
"#"	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_TYPE_HASHTAG);
}
{ID}	{
    BEGIN(CALLPAREN);
    return set_token_val(yyg, TOKEN_ID, yytext);
}
${ID}	{
    BEGIN(INITIAL);
    return set_token_val(yyg, TOKEN_TYPE_VAR, yytext);
}
"$\?"	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_DollarQuestion);
}
"$"	{
    BEGIN(INITIAL);
    return set_token(yyg, TOKEN_DOLLAR);
}
.	{
	printf("unrecognized character: '%s'\n", yytext);
}



<STR>"\""	{
	BEGIN(INITIAL);
	return set_token(yyg, TOKEN_StrClose);
}
<STR>"${"	{
	BEGIN(STREXPR);
}
<STR>"$"	{
    skip_token(yyg);
	BEGIN(STRID);
}
<STR>"\\\""	{ return set_token_val(yyg, TOKEN_StrLit, "\""); }
<STR>"\\\\"	{ return set_token_val(yyg, TOKEN_StrLit, "\\"); }
<STR>"\\$"	{ return set_token_val(yyg, TOKEN_StrLit, "$"); }
<STR>"\\n"	{ return set_token_val(yyg, TOKEN_StrLit, "\n"); }
<STR>{STRC}+	{ return set_token_val(yyg, TOKEN_StrLit, yytext); }

<STRID>{ID}	{
	BEGIN(STR);
	return set_token_val(yyg, TOKEN_ID, yytext);
}
<STREXPR>{ID}	{
	return set_token_val(yyg, TOKEN_ID, yytext);
}
<STREXPR>"."	{
	return set_token(yyg, TOKEN_DOT);
}
<STREXPR>"}"	{
	BEGIN(STR);
}


<BLOCKCOMMENT>"#:" {
	// go another level deeper in the nested comments
	add_block_comment_depth(yyg, +1);
	//return set_token_val(yyg, TOKEN_Comment, yytext);
}
<BLOCKCOMMENT>"#;" {
	// go up a level in the nested comments
	if (add_block_comment_depth(yyg, -1) == 0) {
		// if at level 0, exit block comment context
		BEGIN(INITIAL);
	} else {
		//return set_token_val(yyg, TOKEN_Comment, yytext);
	}
}
<BLOCKCOMMENT>"\\#" {
	// allow escaping the #
	//return set_token_val(yyg, TOKEN_Comment, yytext);
}
<BLOCKCOMMENT>"#" {
	// if not followed by a : or ; it's fine to use plain #
	//return set_token_val(yyg, TOKEN_Comment, yytext);
}
<BLOCKCOMMENT>{BCC}	{
	//return set_token_val(yyg, TOKEN_Comment, yytext);
}


%%

// nothing to see here
