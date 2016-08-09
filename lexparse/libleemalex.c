#include "lex.c"
#include "token.h"

yyscan_t lib_lexscan(const char *input)
{
	struct TokenBuffer *tok;
	tok = (struct TokenBuffer *) malloc(sizeof(struct TokenBuffer));
	tok->token = 0;
	tok->val = NULL;
	tok->length = 0;

	tok->lineno = 1;
	tok->token_column = 1;
	tok->next_column = 1;
	tok->block_comment_depth = 0;

	yyscan_t scanner = NULL;
	yylex_init(&scanner);
	yyset_extra(tok, scanner);
	yy_scan_string(input, scanner);
	return scanner;
}

void lib_lexclose(yyscan_t scanner)
{
	struct TokenBuffer *tok;
	tok = (struct TokenBuffer *) yyget_extra(scanner);
	yyset_extra(NULL, scanner);
	free(tok);
	yylex_destroy(scanner);
}

struct TokenBuffer * lib_lexone(yyscan_t scanner)
{
	int tok = yylex(scanner);
	return (struct TokenBuffer *) yyget_extra(scanner);
}


int set_token(yyscan_t scanner, int tok)
{
	return set_token_val(scanner, tok, NULL);
}

int set_token_val(yyscan_t scanner, int tok, const char *val)
{
	struct TokenBuffer *buf;
    int len = yyget_leng(scanner);
	buf = (struct TokenBuffer *) yyget_extra(scanner);
	buf->token = tok;
	if (val) {
		//printf("set_token_val(%d,%s (%p))\n", tok, val, val);
		buf->val = val;
		buf->length = strlen(val);
	} else {
		buf->val = NULL;
		buf->length = 0;
	}
    buf->token_column = buf->next_column;
    buf->next_column += len;
	//buf->lineno = yyget_lineno(scanner) + 1;
    //printf("lineno,column: %d,%d\n", buf->lineno, buf->column);
	return tok;
}

void set_newline(yyscan_t scanner)
{
	struct TokenBuffer *buf;
	buf = (struct TokenBuffer *) yyget_extra(scanner);
    buf->token_column = 1;
    buf->next_column = 1;
    buf->lineno += 1;
    buf->val = NULL;
    buf->length = 0;
}

int add_block_comment_depth(yyscan_t scanner, int delta)
{
	struct TokenBuffer *buf;
	buf = (struct TokenBuffer *) yyget_extra(scanner);
	buf->block_comment_depth += delta;
	return buf->block_comment_depth;
}

void skip_token(yyscan_t scanner)
{
	struct TokenBuffer *buf;
    int len = yyget_leng(scanner);
	buf = (struct TokenBuffer *) yyget_extra(scanner);
    buf->next_column += len;
}

