
%top {
#include <stdio.h>
#include "leema.h"


typedef void * yyscan_t;

int set_token(yyscan_t, int tok);
int set_token_val(yyscan_t, int tok, const char *val);
int add_block_comment_depth(yyscan_t, int delta);
}

DIGIT	[0-9]
INT	[0-9]+
ID      [a-z_][a-zA-Z_0-9]*[?']?
TYPE_ID [A-Z_][a-zA-Z_0-9]+(\?|\*)?
HASHTAG	#[a-zA-Z_0-9]+[?!]?
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

%option reentrant
%option noyywrap
%option yylineno
%option outfile="lexparse/lex.c"
/* %option nodefault */

%%


<<EOF>>	{
	return set_token(yyg, TOKEN_EOI);
}
{EOL}+	{
	return set_token(yyg, TOKEN_NEWLINE);
}
"\""	{
	BEGIN(STR);
	return set_token(yyg, TOKEN_StrOpen);
}
{INT}	{
	return set_token_val(yyg, TOKEN_INT, yytext);
}
void	{ return set_token(yyg, TOKEN_VOID); }

{SPACE}+	{}
"\."	{ /* return set_token(yyg, TOKEN_DOT); */ }
"$"	{ return set_token(yyg, TOKEN_DOLLAR); }
"$?"	{ return set_token(yyg, TOKEN_DollarQuestion); }
"$>"	{ return set_token(yyg, TOKEN_DollarGT); }
":"	{ return set_token(yyg, TOKEN_COLON); }
"=>"	{ return set_token(yyg, TOKEN_BLOCKARROW); }
"="	{ return set_token(yyg, TOKEN_EQ); }
"<"	{ return set_token(yyg, TOKEN_LT); }
"<="	{ return set_token(yyg, TOKEN_LTEQ); }
">"	{ return set_token(yyg, TOKEN_GT); }
">="	{ return set_token(yyg, TOKEN_GTEQ); }
"!="	{ return set_token(yyg, TOKEN_NEQ); }
"("	{ return set_token(yyg, TOKEN_LPAREN); }
")"	{ return set_token(yyg, TOKEN_RPAREN); }
"["	{ return set_token(yyg, TOKEN_SquareL); }
"]"	{ return set_token(yyg, TOKEN_SquareR); }
","	{ return set_token(yyg, TOKEN_COMMA); }
"+"	{ return set_token(yyg, TOKEN_PLUS); }
"-"	{ return set_token(yyg, TOKEN_MINUS); }
"*"	{ return set_token(yyg, TOKEN_TIMES); }
"/"	{ return set_token(yyg, TOKEN_SLASH); }
"|"	{ return set_token(yyg, TOKEN_PIPE); }
"\\n"	{ return set_token(yyg, TOKEN_ConcatNewline); }
case	{ return set_token(yyg, TOKEN_CASE); }
else	{ return set_token(yyg, TOKEN_ELSE); }
failed	{ return set_token(yyg, TOKEN_FAILED); }
fail	{ return set_token(yyg, TOKEN_FAIL); }
func	{ return set_token(yyg, TOKEN_Func); }
if	{ return set_token(yyg, TOKEN_IF); }
true	{ return set_token(yyg, TOKEN_True); }
false	{ return set_token(yyg, TOKEN_False); }
fork	{ return set_token(yyg, TOKEN_Fork); }
given	{ return set_token(yyg, TOKEN_GIVEN); }
let	{ return set_token(yyg, TOKEN_Let); }
macro	{ return set_token(yyg, TOKEN_MACRO); }
mod	{ return set_token(yyg, TOKEN_MOD); }
where	{ return set_token(yyg, TOKEN_WHERE); }
with	{ return set_token(yyg, TOKEN_WITH); }
native_and	{ return set_token(yyg, TOKEN_AND); }
native_or	{ return set_token(yyg, TOKEN_OR); }
xor	{ return set_token(yyg, TOKEN_XOR); }
not	{ return set_token(yyg, TOKEN_NOT); }
Int	{ return set_token(yyg, TOKEN_TYPE_INT); }
Str	{ return set_token(yyg, TOKEN_TYPE_STR); }
Bool	{ return set_token(yyg, TOKEN_TYPE_BOOL); }
Void	{ return set_token(yyg, TOKEN_TYPE_VOID); }
"#:"	{
	add_block_comment_depth(yyg, +1);
	BEGIN(BLOCKCOMMENT);
}
"##"	{
	BEGIN(LINECOMMENT);
}
{HASHTAG}	{ return set_token_val(yyg, TOKEN_HASHTAG, yytext); }
"@"{ID}	{ /* return set_token_val(yyg, TOKEN_CONTEXTID, yytext); */ }
{ID}	{ return set_token_val(yyg, TOKEN_ID, yytext); }
{TYPE_ID}	{ return set_token_val(yyg, TOKEN_TYPE_ID, yytext); }
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
<STREXPR>{ID}"}"	{
	BEGIN(STR);
	return set_token_val(yyg, TOKEN_ID, yytext);
}


<LINECOMMENT>.*	{
	BEGIN(INITIAL);
	//return set_token_val(yyg, TOKEN_Comment, yytext);
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
