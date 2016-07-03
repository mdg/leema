#ifndef LEEMA_TOKEN_H
#define LEEMA_TOKEN_H

#include <stdlib.h>
#include <stdint.h>
#include "leema.h"


typedef void * yyscan_t;

struct TokenBuffer {
	int32_t token;
	const char *val;
	size_t length;

	int32_t lineno;
	int32_t column;
	int32_t block_comment_depth;
};

int set_token(yyscan_t, int tok);
int set_token_val(yyscan_t, int tok, const char *val);
int add_block_comment_depth(yyscan_t, int delta);

#endif
