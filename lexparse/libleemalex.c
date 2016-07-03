#include "lex.c"
#include "token.h"

yyscan_t lib_lexscan(const char *input) {
	struct TokenBuffer *tok;
	tok = (struct TokenBuffer *) malloc(sizeof(struct TokenBuffer));
	tok->token = 0;
	tok->val = NULL;
	tok->length = 0;

	tok->lineno = 1;
	tok->column = 1;
	tok->block_comment_depth = 0;

	yyscan_t scanner = NULL;
	yylex_init(&scanner);
	yyset_extra(tok, scanner);
	printf("yy_scan_string(%s)\n", input);
	yy_scan_string(input, scanner);
	return scanner;
}

void lib_lexclose(yyscan_t scanner) {
	struct TokenBuffer *tok;
	tok = (struct TokenBuffer *) yyget_extra(scanner);
	yyset_extra(NULL, scanner);
	free(tok);
	yylex_destroy(scanner);
}

struct TokenBuffer * lib_lexone(yyscan_t scanner) {
	int tok = yylex(scanner);
	return (struct TokenBuffer *) yyget_extra(scanner);
}


int set_token(yyscan_t scanner, int tok) {
	return set_token_val(scanner, tok, NULL);
}

int set_token_val(yyscan_t scanner, int tok, const char *val) {
	struct TokenBuffer *buf;
	buf = (struct TokenBuffer *) yyget_extra(scanner);
	buf->token = tok;
	if (val) {
		//printf("set_token_val(%d,%s (%p))\n", tok, val, val);
		buf->val = val;
		buf->length = strlen(val);
	} else {
		//printf("set_token(%d)\n", tok);
		buf->val = NULL;
		buf->length = 0;
	}
	buf->lineno = yyget_lineno(scanner);
	buf->column = yyget_column(scanner);
	return tok;
}

int add_block_comment_depth(yyscan_t scanner, int delta)
{
	struct TokenBuffer *buf;
	buf = (struct TokenBuffer *) yyget_extra(scanner);
	buf->block_comment_depth += delta;
	return buf->block_comment_depth;
}
