use crate::leema::ast2::{Ast, AstNode, AstResult};
use crate::leema::failure::Lresult;
use crate::leema::parser::{
    Assoc, BinaryOpParser, InfixParser, ParseTable, Parser, Precedence,
    PrefixParser,
};
use crate::leema::struple::StrupleKV;
use crate::leema::token::{Token, TokenSrc};
use crate::leema::val::Val;

enum OpPrecedence
{
    Minimum,
    Equal,
    LessThan,
    Add,
    Multiply,
    Xor,
    Semicolon,
    Dollar,
    Pipe,
    Func,
    Dot,
}

#[derive(Debug)]
struct ParseBool;

impl PrefixParser for ParseBool
{
    fn parse<'input>(
        &self,
        _p: &mut Parser<'input>,
        left: TokenSrc<'input>,
    ) -> AstResult<'input>
    {
        let b = match left.src {
            "False" => false,
            "True" => true,
            _ => {
                return Err(rustfail!(
                    "parse_failure",
                    "bool token is not True or False: '{}'",
                    left.src,
                ));
            }
        };
        Ok(AstNode::new_constval(Val::Bool(b), Ast::loc(&left)))
    }
}

#[derive(Debug)]
struct ParseDefConst;

impl PrefixParser for ParseDefConst
{
    fn parse<'input>(
        &self,
        p: &mut Parser<'input>,
        left: TokenSrc<'input>,
    ) -> AstResult<'input>
    {
        let id = p.expect_next(Token::Id)?;
        let _assign = p.expect_next(Token::Assignment)?;
        let rhs = p.parse_expr()?;
        Ok(AstNode::new(Ast::DefConst(id.src, rhs), Ast::loc(&left)))
    }
}

#[derive(Debug)]
struct ParseDefFunc;

impl PrefixParser for ParseDefFunc
{
    fn parse<'input>(
        &self,
        p: &mut Parser<'input>,
        left: TokenSrc<'input>,
    ) -> AstResult<'input>
    {
        let name = Grammar::parse_id(p)?;
        let args = Grammar::parse_idtypes(p, Token::DoubleArrow)?;
        let body = Grammar::parse_funcbody(p)?;
        p.expect_next(Token::DoubleDash)?;
        Ok(AstNode::new(
            Ast::DefFunc(name, args, body),
            Ast::loc(&left),
        ))
    }
}

#[derive(Debug)]
struct DefTypeParser;

#[derive(Debug)]
struct ParseLet;

impl PrefixParser for ParseLet
{
    fn parse<'input>(
        &self,
        p: &mut Parser<'input>,
        left: TokenSrc<'input>,
    ) -> AstResult<'input>
    {
        let lhs = p.parse_expr()?;
        let _assign = p.expect_next(Token::Assignment)?;
        let rhs = p.parse_expr()?;
        Ok(AstNode::new(
            Ast::Let(lhs, AstNode::void(), rhs),
            Ast::loc(&left),
        ))
    }
}

#[derive(Debug)]
struct BlockParser;

impl PrefixParser for BlockParser
{
    fn parse<'input>(
        &self,
        _p: &mut Parser<'input>,
        _left: TokenSrc<'input>,
    ) -> AstResult<'input>
    {
        Ok(AstNode::void())
    }
}

#[derive(Debug)]
struct ParseId;

impl PrefixParser for ParseId
{
    fn parse<'input>(
        &self,
        _p: &mut Parser<'input>,
        left: TokenSrc<'input>,
    ) -> AstResult<'input>
    {
        Ok(AstNode::new(Ast::Id1(left.src), Ast::loc(&left)))
    }
}

#[derive(Debug)]
struct IfParser;

impl PrefixParser for IfParser
{
    fn parse<'input>(
        &self,
        _p: &mut Parser<'input>,
        _left: TokenSrc<'input>,
    ) -> AstResult<'input>
    {
        Ok(AstNode::void())
    }
}

#[derive(Debug)]
struct ParseInt;

impl PrefixParser for ParseInt
{
    fn parse<'input>(
        &self,
        _p: &mut Parser<'input>,
        left: TokenSrc<'input>,
    ) -> AstResult<'input>
    {
        let i: i64 = left.src.parse().map_err(|parsef| {
            rustfail!(
                "parse_failure",
                "int token is not an integer: {:?}",
                parsef,
            )
        })?;
        Ok(AstNode::new(Ast::ConstVal(Val::Int(i)), Ast::loc(&left)))
    }
}

#[derive(Debug)]
struct ListParser;

#[derive(Debug)]
struct MatchParser;

#[derive(Debug)]
struct TupleParser;

const OP_MULTIPLY: &'static BinaryOpParser = &BinaryOpParser {
    op: "*",
    pre: Precedence(13, 0, Assoc::Left),
};

const OP_DIVIDE: &'static BinaryOpParser = &BinaryOpParser {
    op: "/",
    pre: Precedence(13, 0, Assoc::Left),
};

const OP_ADD: &'static BinaryOpParser = &BinaryOpParser {
    op: "+",
    pre: Precedence(13, 0, Assoc::Left),
};

const OP_SUBTRACT: &'static BinaryOpParser = &BinaryOpParser {
    op: "*",
    pre: Precedence(13, 0, Assoc::Left),
};

// struct ConsParser;
// struct DollarParser;
// struct DotParser;
// struct PipeParser;

#[derive(Debug)]
struct ParseCall;

impl InfixParser for ParseCall
{
    fn parse<'input>(
        &self,
        p: &mut Parser<'input>,
        left: AstNode<'input>,
        _tok: TokenSrc<'input>,
    ) -> AstResult<'input>
    {
        let args = Grammar::parse_xlist(p, Token::ParenR)?;
        let loc = left.loc;
        Ok(AstNode::new(Ast::Call(left, args), loc))
    }

    fn precedence(&self) -> Precedence
    {
        Precedence(15, 0, Assoc::Left)
    }
}

#[derive(Debug)]
struct LessThanParser;

#[derive(Debug)]
struct TypeParamParser;

const PARSE_TABLE: ParseTable = [
    (Token::Id, None, Some(&ParseId), None),
    (Token::Int, None, Some(&ParseInt), None),
    (Token::Bool, None, Some(&ParseBool), None),
    (Token::Hashtag, None, None, None),
    (Token::StrLit, None, None, None),
    (Token::DollarId, None, None, None),
    // brackets
    (Token::ParenL, None, None, Some(&ParseCall)),
    (Token::ParenR, None, None, None),
    (Token::SquareL, None, None, None),
    (Token::SquareR, None, None, None),
    (Token::CurlyL, None, None, None),
    (Token::CurlyR, None, None, None),
    (Token::AngleL, None, None, None),
    (Token::AngleR, None, None, None),
    (Token::DoubleQuoteL, None, None, None),
    (Token::DoubleQuoteR, None, None, None),
    // keywords
    (Token::Const, Some(&ParseDefConst), None, None),
    (Token::Failed, None, None, None),
    (Token::Fork, None, None, None),
    (Token::Func, Some(&ParseDefFunc), None, None),
    (Token::If, None, Some(&IfParser), None),
    (Token::Import, None, None, None),
    (Token::Let, Some(&ParseLet), None, None),
    (Token::Macro, None, None, None),
    (Token::Match, None, None, None),
    (Token::Return, None, None, None),
    (Token::Type, None, None, None),
    (Token::Underscore, None, None, None),
    // operators (arithmetic)
    (Token::Plus, None, None, Some(OP_ADD)),
    (Token::Dash, None, None, Some(OP_SUBTRACT)),
    (Token::Star, None, None, Some(OP_MULTIPLY)),
    (Token::Slash, None, None, Some(OP_DIVIDE)),
    (Token::Modulo, None, None, None),
    // operators (boolean)
    (Token::And, None, None, None),
    (Token::Not, None, None, None),
    (Token::Or, None, None, None),
    (Token::Xor, None, None, None),
    // operators (comparison)
    (Token::Equal, None, None, None),
    (Token::EqualNot, None, None, None),
    (Token::GreaterThanEqual, None, None, None),
    (Token::LessThanEqual, None, None, None),
    // separators
    (Token::Assignment, None, None, None),
    (Token::CasePipe, None, None, None),
    (Token::Colon, None, None, None),
    (Token::Comma, None, None, None),
    (Token::ConcatNewline, None, None, None),
    (Token::Dot, None, None, None),
    (Token::DoubleArrow, None, Some(&BlockParser), None),
    (Token::DoubleColon, None, None, None),
    (Token::DoubleDash, None, None, None),
    (Token::Pipe, None, None, None),
    (Token::Semicolon, None, None, None),
    (Token::StatementSep, None, None, None),
    // comments
    (Token::CommentBlockStart, None, None, None),
    (Token::CommentBlockStop, None, None, None),
    (Token::CommentLine, None, None, None),
    // whitespace
    (Token::EmptyLine, None, None, None),
    (Token::LineBegin, None, None, None),
    (Token::LineEnd, None, None, None),
    (Token::Spaces, None, None, None),
    (Token::Tabs, None, None, None),
    // EOF
    (Token::EOF, None, None, None),
];

pub struct Grammar<'input>
{
    p: Parser<'input>,
}

impl<'input> Grammar<'input>
{
    pub fn new(items: Vec<TokenSrc<'input>>) -> Grammar<'input>
    {
        Grammar {
            p: Parser::new(&PARSE_TABLE, items),
        }
    }

    pub fn parse_module(&mut self) -> Lresult<Vec<AstNode<'input>>>
    {
        let mut result = vec![];
        while !self.p.match_next(Token::EOF)? {
            result.push(self.p.parse_stmt()?);
        }
        Ok(result)
    }

    /// Parse the body of a function. Also eat the trailing
    fn parse_funcbody(p: &mut Parser<'input>) -> AstResult<'input>
    {
        match p.peek_token()? {
            Token::LineBegin => {
                p.next()?;
                match p.peek_token()? {
                    Token::Pipe => {
                        let cases = Grammar::parse_cases(p);
                        cases
                    }
                    _ => p.parse_stmt(),
                }
            }
            Token::DoubleDash => {
                Ok(AstNode::void())
            }
            Token::EOF => {
                Err(rustfail!(
                    "parse_failure",
                    "expected function body, found EOF",
                ))
            }
            _ => {
                let x = p.parse_expr()?;
                Ok(x)
            }
        }
    }

    /// Parse the cases of a function body that matches on all parameters
    fn parse_cases(_p: &mut Parser<'input>) -> AstResult<'input>
    {
        Ok(AstNode::void())
    }

    /// Parse an idtype pair, ".id:Type"
    /// skip a LineBegin before the idtype if there is one
    pub fn parse_idtypes(
        p: &mut Parser<'input>,
        end: Token,
    ) -> Lresult<StrupleKV<Option<&'input str>, Option<AstNode<'input>>>>
    {
        let mut idtypes = vec![];
        loop {
            p.next_if(Token::LineBegin)?;
            let id = match p.peek_token()? {
                Token::Dot => {
                    let _dot = p.expect_next(Token::Dot)?;
                    let name = p.expect_next(Token::Id)?;
                    Some(name.src)
                }
                Token::Colon => {
                    // no ID, fall through to type
                    None
                }
                Token::EOF => {
                    return Err(rustfail!(
                        "parse_failure",
                        "expected {:?}, found EOF",
                        end,
                    ));
                }
                t if t == end => {
                    p.next()?;
                    break;
                }
                t => {
                    return Err(rustfail!(
                        "parse_failue",
                        "expected . or :, found {:?}",
                        t,
                    ));
                }
            };

            let typ = match p.peek_token()? {
                Token::Dot => {
                    // no type, fall through
                    None
                }
                Token::Colon => {
                    p.next()?;
                    Some(p.parse_expr()?)
                }
                Token::EOF => {
                    return Err(rustfail!(
                        "parse_failure",
                        "expected {:?}, found EOF",
                        end,
                    ));
                }
                t if t == end => None,
                t => {
                    return Err(rustfail!(
                        "parse_failure",
                        "expected ')' or ',' found {:?}",
                        t,
                    ));
                }
            };

            idtypes.push((id, typ));
        }
        Ok(StrupleKV::from(idtypes))
    }

    pub fn parse_xlist(
        p: &mut Parser<'input>,
        end: Token,
    ) -> Lresult<StrupleKV<Option<&'input str>, AstNode<'input>>>
    {
        let mut args = vec![];
        loop {
            match p.peek_token()? {
                Token::EOF => {
                    return Err(rustfail!(
                        "parse_failure",
                        "expected {:?}, found EOF",
                        end,
                    ));
                }
                t if t == end => {
                    p.next()?;
                    break;
                }
                _ => {
                    args.push(p.parse_expr()?);
                }
            };

            let comma = p.next()?;
            match comma.tok {
                Token::Comma => {
                    // continue
                }
                Token::EOF => {
                    return Err(rustfail!(
                        "parse_failure",
                        "expected {:?}, found EOF",
                        end,
                    ));
                }
                t if t == end => {
                    break;
                }
                _ => {
                    return Err(rustfail!(
                        "parse_failure",
                        "expected ')' or ',' found {:?}",
                        comma,
                    ));
                }
            }
        }
        Ok(StrupleKV::from(args))
    }

    pub fn parse_id(p: &mut Parser<'input>) -> AstResult<'input>
    {
        let id = p.expect_next(Token::Id)?;
        Ok(AstNode::new(Ast::Id1(id.src), Ast::loc(&id)))
    }
}


#[cfg(test)]
mod tests
{
    use super::Grammar;
    use crate::leema::token::Tokenz;

    #[test]
    fn test_parse_call_no_params()
    {
        let input = "let x := 5 + f()";
        Grammar::new(Tokenz::lexp(input).unwrap())
            .parse_module()
            .unwrap();
    }

    #[test]
    fn test_parse_call_two_params()
    {
        let input = "let x := 5 + f(9, 3)";
        Grammar::new(Tokenz::lexp(input).unwrap())
            .parse_module()
            .unwrap();
    }

    #[test]
    fn test_parse_call_two_params_trailing_comma()
    {
        let input = "let x := 5 + f(9, 3,)";
        Grammar::new(Tokenz::lexp(input).unwrap())
            .parse_module()
            .unwrap();
    }

    #[test]
    fn test_parse_const()
    {
        let input = "const X := 5";
        let toks = Tokenz::lexp(input).unwrap();
        let mut p = Grammar::new(toks);
        p.parse_module().unwrap();
    }

    #[test]
    fn test_parse_deffunc()
    {
        let input = "func zero >> 0 --";
        let toks = Tokenz::lexp(input).unwrap();
        let mut p = Grammar::new(toks);
        p.parse_module().unwrap();
    }

    #[test]
    fn test_parse_let()
    {
        let input = "let x := 5 + y";
        let toks = Tokenz::lexp(input).unwrap();
        let mut p = Grammar::new(toks);
        p.parse_module().unwrap();
    }

    #[test]
    fn test_parse_table_tokens()
    {
        for (i, row) in super::PARSE_TABLE.iter().enumerate() {
            let token_index = row.0 as usize;
            if i != token_index {
               panic!("parse table row at wrong index for {:?}", row.0);
            }
        }
    }
}
