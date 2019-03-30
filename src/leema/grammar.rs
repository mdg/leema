use crate::leema::ast2::{self, Ast, AstNode, AstResult, CaseType, FuncClass};
use crate::leema::failure::Lresult;
use crate::leema::lstr::Lstr;
use crate::leema::parser::{
    Assoc, BinaryOpParser, InfixParser, ParseTable, Parser, Precedence,
    PrefixOpParser, PrefixParser,
};
use crate::leema::struple::StrupleKV;
use crate::leema::token::{Token, TokenSrc};
use crate::leema::val::Val;

enum Lprec
{
    Minimum,
    Comma,
    Or,
    And,
    Not,
    Equal,
    LessThan,
    Add,
    Multiply,
    Cons,
    Dollar,
    Pipe,
    Func,
    Dot,
}

impl From<Lprec> for Precedence
{
    fn from(p: Lprec) -> Self
    {
        Precedence(p as u8, 0, Assoc::Left)
    }
}

/* Statement Parsers */

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
        let id = expect_next!(p, Token::Id)?;
        let _assign = expect_next!(p, Token::Assignment)?;
        let rhs = p.parse_new_expr()?;
        Ok(AstNode::new(Ast::DefConst(id.src, rhs), Ast::loc(&left)))
    }
}

#[derive(Debug)]
struct ParseDefFunc(FuncClass);

impl PrefixParser for ParseDefFunc
{
    fn parse<'input>(
        &self,
        p: &mut Parser<'input>,
        left: TokenSrc<'input>,
    ) -> AstResult<'input>
    {
        let name = Grammar::parse_id(p)?;
        let args = Grammar::parse_idtypes(p)?;
        expect_next!(p, Token::DoubleArrow)?;
        let body = Grammar::parse_block(p)?;
        expect_next!(p, Token::DoubleDash)?;
        Ok(AstNode::new(
            Ast::DefFunc(self.0, name, args, body),
            Ast::loc(&left),
        ))
    }
}

#[derive(Debug)]
struct ParseDefType;

impl PrefixParser for ParseDefType
{
    fn parse<'input>(
        &self,
        p: &mut Parser<'input>,
        left: TokenSrc<'input>,
    ) -> AstResult<'input>
    {
        let name = Grammar::parse_id(p)?;
        p.skip_if(Token::LineBegin)?;
        let tok = p.peek()?;
        let data = match tok.tok {
            Token::Dot => {
                let args = Grammar::parse_idtypes(p)?;
                Ast::DefType(ast2::DataType::Struct, name, args)
            }
            Token::CasePipe => {
                let variants = Grammar::parse_variants(p)?;
                Ast::DefType(ast2::DataType::Union, name, variants)
            }
            _ => {
                return Err(rustfail!(
                    "parse_failure",
                    "expected . or | found {:?}",
                    tok,
                ));
            }
        };
        expect_next!(p, Token::DoubleDash)?;
        Ok(AstNode::new(data, Ast::loc(&left)))
    }
}

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
        let lhs = p.parse_new_expr()?;
        let _assign = expect_next!(p, Token::Assignment)?;
        let rhs = p.parse_new_expr()?;
        Ok(AstNode::new(
            Ast::Let(lhs, AstNode::void(), rhs),
            Ast::loc(&left),
        ))
    }
}

#[derive(Debug)]
struct ParseParen;

impl PrefixParser for ParseParen
{
    fn parse<'input>(
        &self,
        p: &mut Parser<'input>,
        _left: TokenSrc<'input>,
    ) -> AstResult<'input>
    {
        let inner = p.parse_new_expr()?;
        expect_next!(p, Token::ParenR)?;
        Ok(inner)
    }
}

/* Expression Parsers */

#[derive(Debug)]
struct ParseBlock;

impl PrefixParser for ParseBlock
{
    fn parse<'input>(
        &self,
        p: &mut Parser<'input>,
        _left: TokenSrc<'input>,
    ) -> AstResult<'input>
    {
        let block = Grammar::parse_block(p)?;
        expect_next!(p, Token::DoubleDash)?;
        Ok(block)
    }
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
struct ParseHashtag;

impl PrefixParser for ParseHashtag
{
    fn parse<'input>(
        &self,
        _p: &mut Parser<'input>,
        left: TokenSrc<'input>,
    ) -> AstResult<'input>
    {
        Ok(AstNode::new_constval(
            Val::Hashtag(Lstr::from(left.src.to_string())),
            Ast::loc(&left),
        ))
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
struct ParseIf;

impl PrefixParser for ParseIf
{
    fn parse<'input>(
        &self,
        p: &mut Parser<'input>,
        if_token: TokenSrc<'input>,
    ) -> AstResult<'input>
    {
        let cases = Grammar::parse_cases(p)?;
        expect_next!(p, Token::DoubleDash)?;
        Ok(AstNode::new(
            Ast::Case(CaseType::If, None, cases),
            Ast::loc(&if_token),
        ))
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
struct ParseMatch;

impl PrefixParser for ParseMatch
{
    fn parse<'input>(
        &self,
        p: &mut Parser<'input>,
        left: TokenSrc<'input>,
    ) -> AstResult<'input>
    {
        let tok = p.peek()?;
        let input = match tok.tok {
            Token::CasePipe => {
                // no expression, go straight to cases
                None
            }
            _ => {
                // get the expression, then go to cases
                Some(p.parse_new_expr()?)
            }
        };
        let cases = Grammar::parse_cases(p)?;
        let m = Ast::Case(CaseType::Match, input, cases);
        Ok(AstNode::new(m, Ast::loc(&left)))
    }
}

#[derive(Debug)]
struct ParseStr;

impl PrefixParser for ParseStr
{
    fn parse<'input>(
        &self,
        p: &mut Parser<'input>,
        left: TokenSrc<'input>,
    ) -> AstResult<'input>
    {
        let mut found_strlit = false;
        let mut strs = vec![];
        loop {
            let tok = p.next()?;
            let x = match tok.tok {
                Token::StrLit => {
                    found_strlit = true;
                    let lstr = Lstr::from(tok.src.to_string());
                    AstNode::new(Ast::ConstVal(Val::Str(lstr)), Ast::loc(&tok))
                }
                Token::DollarId => {
                    AstNode::new(Ast::Id1(tok.src), Ast::loc(&tok))
                }
                Token::DoubleQuoteR => {
                    break;
                }
                _ => {
                    return Err(rustfail!(
                        "parse_failure",
                        "expected str or id, found {:?}",
                        tok,
                    ));
                }
            };
            strs.push(x);
        }
        let node = match strs.len() {
            // empty vec reduces to constant ""
            0 => AstNode::new(Ast::ConstVal(Val::empty_str()), Ast::loc(&left)),
            // single item w/ constant string reduces to just that
            // constant string. single IDs stay in the strexpr so
            // they get stringified if they aren't already
            1 if found_strlit => strs.pop().unwrap(),
            _ => AstNode::new(Ast::StrExpr(strs), Ast::loc(&left)),
        };
        Ok(node)
    }
}

#[derive(Debug)]
struct TupleParser;

const OP_NOT: &'static PrefixOpParser = &PrefixOpParser {
    op: "not",
    pre: Precedence(Lprec::Not as u8, 0, Assoc::Right),
};

const OP_AND: &'static BinaryOpParser = &BinaryOpParser {
    op: "and",
    pre: Precedence(Lprec::And as u8, 0, Assoc::Left),
};

const OP_OR: &'static BinaryOpParser = &BinaryOpParser {
    op: "or",
    pre: Precedence(Lprec::Or as u8, 0, Assoc::Left),
};

const OP_XOR: &'static BinaryOpParser = &BinaryOpParser {
    op: "xor",
    pre: Precedence(Lprec::Or as u8, 0, Assoc::Left),
};

const OP_MULTIPLY: &'static BinaryOpParser = &BinaryOpParser {
    op: "*",
    pre: Precedence(Lprec::Multiply as u8, 0, Assoc::Left),
};

const OP_DIVIDE: &'static BinaryOpParser = &BinaryOpParser {
    op: "/",
    pre: Precedence(Lprec::Multiply as u8, 0, Assoc::Left),
};

const OP_MODULO: &'static BinaryOpParser = &BinaryOpParser {
    op: "mod",
    pre: Precedence(Lprec::Multiply as u8, 0, Assoc::Left),
};

const OP_CONS: &'static BinaryOpParser = &BinaryOpParser {
    op: ";",
    pre: Precedence(Lprec::Cons as u8, 0, Assoc::Right),
};

const OP_ADD: &'static BinaryOpParser = &BinaryOpParser {
    op: "+",
    pre: Precedence(Lprec::Add as u8, 0, Assoc::Left),
};

const OP_SUBTRACT: &'static BinaryOpParser = &BinaryOpParser {
    op: "*",
    pre: Precedence(Lprec::Add as u8, 0, Assoc::Left),
};

const OP_EQ: &'static BinaryOpParser = &BinaryOpParser {
    op: "==",
    pre: Precedence(Lprec::Equal as u8, 0, Assoc::Left),
};

const OP_NEQ: &'static BinaryOpParser = &BinaryOpParser {
    op: "!=",
    pre: Precedence(Lprec::Equal as u8, 0, Assoc::Left),
};

const OP_GT: &'static BinaryOpParser = &BinaryOpParser {
    op: ">",
    pre: Precedence(Lprec::LessThan as u8, 0, Assoc::Left),
};

const OP_GTE: &'static BinaryOpParser = &BinaryOpParser {
    op: ">=",
    pre: Precedence(Lprec::LessThan as u8, 0, Assoc::Left),
};

const OP_LT: &'static BinaryOpParser = &BinaryOpParser {
    op: "<",
    pre: Precedence(Lprec::LessThan as u8, 0, Assoc::Left),
};

const OP_LTE: &'static BinaryOpParser = &BinaryOpParser {
    op: "<=",
    pre: Precedence(Lprec::LessThan as u8, 0, Assoc::Left),
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
    (Token::ParenL, None, Some(&ParseParen), Some(&ParseCall)),
    (Token::ParenR, None, None, None),
    (Token::SquareL, None, None, None),
    (Token::SquareR, None, None, None),
    (Token::CurlyL, None, None, None),
    (Token::CurlyR, None, None, None),
    (Token::AngleL, None, None, Some(OP_LT)),
    (Token::AngleR, None, None, Some(OP_GT)),
    (Token::DoubleQuoteL, None, Some(&ParseStr), None),
    (Token::DoubleQuoteR, None, None, None),
    // statement keywords
    (Token::Const, Some(&ParseDefConst), None, None),
    (Token::Else, None, None, None),
    (Token::Failed, None, None, None),
    (Token::Fork, None, None, None),
    (
        Token::Func,
        Some(&ParseDefFunc(FuncClass::Func)),
        None,
        None,
    ),
    (Token::If, None, Some(&ParseIf), None),
    (Token::Import, None, None, None),
    (Token::Let, Some(&ParseLet), None, None),
    (
        Token::Macro,
        Some(&ParseDefFunc(FuncClass::Macro)),
        None,
        None,
    ),
    (Token::Match, None, Some(&ParseMatch), None),
    (Token::Return, None, None, None),
    (Token::Type, Some(&ParseDefType), None, None),
    (Token::Underscore, None, None, None),
    // operators (arithmetic)
    (Token::Plus, None, None, Some(OP_ADD)),
    (Token::Dash, None, None, Some(OP_SUBTRACT)),
    (Token::Star, None, None, Some(OP_MULTIPLY)),
    (Token::Slash, None, None, Some(OP_DIVIDE)),
    (Token::Modulo, None, None, Some(OP_MODULO)),
    (Token::Dollar, None, None, None),
    // operators (boolean)
    (Token::And, None, None, Some(OP_AND)),
    (Token::Not, None, Some(OP_NOT), None),
    (Token::Or, None, None, Some(OP_OR)),
    (Token::Xor, None, None, Some(OP_XOR)),
    // operators (comparison)
    (Token::Equal, None, None, Some(OP_EQ)),
    (Token::EqualNot, None, None, Some(OP_NEQ)),
    (Token::GreaterThanEqual, None, None, Some(OP_GTE)),
    (Token::LessThanEqual, None, None, Some(OP_LTE)),
    // separators
    (Token::Assignment, None, None, None),
    (Token::CasePipe, None, None, None),
    (Token::Colon, None, None, None),
    (Token::Comma, None, None, None),
    (Token::ConcatNewline, None, None, None),
    (Token::Dot, None, None, None),
    (Token::DoubleArrow, None, Some(&ParseBlock), None),
    (Token::DoubleColon, None, None, None),
    (Token::DoubleDash, None, None, None),
    (Token::Pipe, None, None, None),
    (Token::Semicolon, None, None, Some(OP_CONS)),
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
        self.p.parse_stmts()
    }

    /// Parse the body of a function. Also eat the trailing
    fn parse_block(p: &mut Parser<'input>) -> AstResult<'input>
    {
        let tok = p.peek()?;
        match tok.tok {
            Token::LineBegin => {
                let mut stmts = p.parse_stmts()?;
                let node = match stmts.len() {
                    0 => AstNode::void(),
                    1 => stmts.pop().unwrap(),
                    _ => AstNode::new(Ast::Block(stmts), Ast::loc(&tok)),
                };
                Ok(node)
            }
            // if it's on the same line, take only one expr
            _ => p.parse_new_expr(),
        }
    }

    /// Parse the cases of a function body that matches on all parameters
    fn parse_cases(p: &mut Parser<'input>) -> Lresult<Vec<ast2::Case<'input>>>
    {
        let mut cases = vec![];
        loop {
            p.skip_if(Token::LineBegin)?;
            let peeked = p.peek()?;
            match peeked.tok {
                Token::CasePipe => {
                    p.next()?;
                    let cond = match p.next_if(Token::Else)? {
                        Some(tok) => {
                            let v = Ast::ConstVal(Val::Bool(true));
                            AstNode::new(v, Ast::loc(&tok))
                        }
                        None => p.parse_new_expr()?,
                    };
                    expect_next!(p, Token::DoubleArrow)?;
                    let body = Grammar::parse_block(p)?;
                    cases.push(ast2::Case::new(cond, body));
                }
                Token::DoubleDash => {
                    break;
                }
                _ => {
                    return Err(rustfail!(
                        "parse_failure",
                        "expected | or -- found {:?}",
                        peeked,
                    ));
                }
            }
        }
        Ok(cases)
    }

    /// Parse an idtype pair, ".id:Type"
    /// skip a LineBegin before the idtype if there is one
    pub fn parse_idtypes(
        p: &mut Parser<'input>,
    ) -> Lresult<StrupleKV<Option<&'input str>, Option<AstNode<'input>>>>
    {
        let mut idtypes = vec![];
        loop {
            p.skip_if(Token::LineBegin)?;
            let peeked = p.peek()?;
            let id = match peeked.tok {
                Token::Dot => {
                    let _dot = expect_next!(p, Token::Dot)?;
                    let name = expect_next!(p, Token::Id)?;
                    Some(name.src)
                }
                Token::Colon => {
                    // no ID, fall through to type
                    None
                }
                Token::DoubleArrow => {
                    // for parameters in a function declaration
                    break;
                }
                Token::DoubleDash => {
                    // for parameters in a structure declaration
                    break;
                }
                Token::CasePipe => {
                    // for fields in a union variant
                    break;
                }
                Token::EOF => {
                    return Err(rustfail!(
                        "parse_failure",
                        "expected : or . or | found EOF",
                    ));
                }
                _ => {
                    return Err(rustfail!(
                        "parse_failue",
                        "expected . or : or | found {:?}",
                        peeked,
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
                    Some(p.parse_new_expr()?)
                }
                Token::DoubleArrow => None,
                Token::DoubleDash => None,
                Token::CasePipe => None,
                Token::EOF => {
                    return Err(rustfail!(
                        "parse_failure",
                        "expected : or . found EOF",
                    ));
                }
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

    /// Parse the variants in a union declaration
    pub fn parse_variants(
        p: &mut Parser<'input>,
    ) -> Lresult<StrupleKV<Option<&'input str>, Option<AstNode<'input>>>>
    {
        let mut variants = vec![];
        loop {
            let pipe = p.peek()?;
            match pipe.tok {
                Token::CasePipe => {
                    p.next()?; // consume the pipe
                    let name = expect_next!(p, Token::Id)?;
                    let id = AstNode::new(Ast::Id1(name.src), Ast::loc(&name));
                    let fields = Self::parse_idtypes(p)?;
                    let var = if fields.is_empty() {
                        None
                    } else {
                        Some(AstNode::new(
                            Ast::DefType(ast2::DataType::Struct, id, fields),
                            Ast::loc(&name),
                        ))
                    };
                    variants.push((Some(name.src), var));
                }
                Token::DoubleDash => {
                    // leave the doubledash unconsumed
                    break;
                }
                _ => {
                    return Err(rustfail!(
                        "parse_failure",
                        "expected | or -- found {:?}",
                        pipe,
                    ));
                }
            }
        }
        Ok(StrupleKV::from(variants))
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
                    args.push(p.parse_new_expr()?);
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
        let id = expect_next!(p, Token::Id)?;
        Ok(AstNode::new(Ast::Id1(id.src), Ast::loc(&id)))
    }
}


#[cfg(test)]
mod tests
{
    use super::Grammar;
    use crate::leema::ast2::{Ast, DataType};
    use crate::leema::lstr::Lstr;
    use crate::leema::token::Tokenz;
    use crate::leema::val::Val;

    use matches::assert_matches;


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
    fn test_parse_deffunc_noparams_oneline()
    {
        let input = "func zero >> 0 --";
        let toks = Tokenz::lexp(input).unwrap();
        let mut p = Grammar::new(toks);
        p.parse_module().unwrap();
    }

    #[test]
    fn test_parse_deffunc_noparams_multiline()
    {
        let input = r#"func three >>
            1 + 2
        --

        func five
        >>
            3 + 2
        --
        "#;
        let toks = Tokenz::lexp(input).unwrap();
        let mut p = Grammar::new(toks);
        p.parse_module().unwrap();
    }

    #[test]
    fn test_parse_hashtag()
    {
        let input = "const H := #hash_tag";
        let toks = Tokenz::lexp(input).unwrap();
        let mut p = Grammar::new(toks);
        p.parse_module().unwrap();
    }

    #[test]
    fn test_parse_ifcases()
    {
        let input = "
        if
        |b == 3 >> 100
        |b == 0 >>
            let a := 5
            int_abs(a)
        |else >>
            let r := a mod b
            gcd(b, min(r, b - r))
        --
        ";
        let toks = Tokenz::lexp(input).unwrap();
        let ast = Grammar::new(toks).parse_module().unwrap();
        assert_eq!(1, ast.len());
    }

    #[test]
    fn test_parse_let_add()
    {
        let input = "let x := 5 + y";
        let toks = Tokenz::lexp(input).unwrap();
        let mut p = Grammar::new(toks);
        p.parse_module().unwrap();
    }

    #[test]
    fn test_parse_let_block()
    {
        let input = "
            let a := >>
                b * 3
            --
            ";
        let toks = Tokenz::lexp(input).unwrap();
        let ast = Grammar::new(toks).parse_module().unwrap();
        assert_eq!(1, ast.len());

        let l = &ast[0];
        assert_matches!(*l.node, Ast::Let(_, _, _));
        if let Ast::Let(lhp, _, rhs) = &*l.node {
            assert_eq!(Ast::Id1("a"), *lhp.node);
            assert_matches!(*rhs.node, Ast::Op2("*", _, _));
            if let Ast::Op2("*", b, three) = &*rhs.node {
                assert_eq!(Ast::Id1("b"), *b.node);
                assert_eq!(Ast::ConstVal(Val::Int(3)), *three.node);
            }
        }
    }

    #[test]
    fn test_parse_match_noinput()
    {
        let input = "
        match
        |0 >> 1
        |i >> do(i)
        --
        ";
        let toks = Tokenz::lexp(input).unwrap();
        let ast = Grammar::new(toks).parse_module().unwrap();
        assert_eq!(1, ast.len());
    }

    #[test]
    fn test_parse_match_withinput()
    {
        let input = "
        match tacos
        |0 >> 1
        |i >> do(i)
        --
        ";
        let toks = Tokenz::lexp(input).unwrap();
        let ast = Grammar::new(toks).parse_module().unwrap();
        assert_eq!(1, ast.len());
    }

    #[test]
    fn test_parse_operators()
    {
        let input = r#"func ops >>
            1 - 2 - 3
            x * y * z
            9 / 3
            5 mod 4
            4 == 4
            7 != 8
            1 < 2
            2 <= 3
            3 > 2
            2 >= 1
            not True
            a and b
            c or d
            m xor n
            h;t
        --
        "#;
        let toks = Tokenz::lexp(input).unwrap();
        let mut p = Grammar::new(toks);
        p.parse_module().unwrap();
    }

    #[test]
    fn test_parse_parens()
    {
        let input = r#"not (x == y)"#;
        let toks = Tokenz::lexp(input).unwrap();
        let mut p = Grammar::new(toks);
        p.parse_module().unwrap();
    }

    #[test]
    fn test_parse_precedence()
    {
        let input = r#"
        1 - 2 * 3
        x / y + z
        "#;
        let toks = Tokenz::lexp(input).unwrap();
        let mut p = Grammar::new(toks);
        let ast = p.parse_module().unwrap();
        assert_eq!(2, ast.len());

        {
            let sub = &ast[0];
            assert_matches!(*sub.node, Ast::Op2("-", _, _));
            if let Ast::Op2("-", i1, mult) = &*sub.node {
                assert_matches!(*i1.node, Ast::ConstVal(Val::Int(1)));
                assert_matches!(*mult.node, Ast::Op2("*", _, _));
                if let Ast::Op2("*", i2, i3) = &*mult.node {
                    assert_matches!(*i2.node, Ast::ConstVal(Val::Int(2)));
                    assert_matches!(*i3.node, Ast::ConstVal(Val::Int(3)));
                }
            }
        }

        {
            let add = &ast[1];
            assert_matches!(*add.node, Ast::Op2("+", _, _));
            if let Ast::Op2("+", div, x) = &*add.node {
                assert_matches!(*div.node, Ast::Op2("/", _, _));
                assert_matches!(*x.node, Ast::Id1("z"));
                if let Ast::Op2("/", x, y) = &*div.node {
                    assert_matches!(*x.node, Ast::Id1("x"));
                    assert_matches!(*y.node, Ast::Id1("y"));
                }
            }
        }
    }

    #[test]
    fn test_parse_strlit()
    {
        let input = r#"
        "tacos"
        "$burritos"
        "cats $dogs mice"
        "#;
        let toks = Tokenz::lexp(input).unwrap();
        let mut p = Grammar::new(toks);
        let ast = p.parse_module().unwrap();
        assert_eq!(3, ast.len());

        {
            let tacos = &ast[0];
            assert_matches!(*tacos.node, Ast::ConstVal(Val::Str(_)));
            if let Ast::ConstVal(Val::Str(inner)) = &*tacos.node {
                assert_eq!("tacos", inner);
            }
        }

        {
            let s = &ast[1];
            assert_matches!(*s.node, Ast::StrExpr(_));
            if let Ast::StrExpr(items) = &*s.node {
                assert_eq!(1, items.len());
                assert_eq!(*items[0].node, Ast::Id1("$burritos"));
            }
        }

        {
            let s = &ast[2];
            assert_matches!(*s.node, Ast::StrExpr(_));
            if let Ast::StrExpr(items) = &*s.node {
                assert_eq!(3, items.len());
                assert_eq!(
                    Ast::ConstVal(Val::Str(Lstr::from("cats "))),
                    *items[0].node,
                );
                assert_eq!(*items[1].node, Ast::Id1("$dogs"));
                assert_eq!(
                    Ast::ConstVal(Val::Str(Lstr::from(" mice"))),
                    *items[2].node,
                );
            }
        }
    }

    #[test]
    fn test_parse_type_struct()
    {
        let input = "
        type Point
        .x:Int
        .y:Int
        --
        ";
        let toks = Tokenz::lexp(input).unwrap();
        let ast = Grammar::new(toks).parse_module().unwrap();
        assert_eq!(1, ast.len());

        let t = &ast[0];
        assert_matches!(*t.node, Ast::DefType(DataType::Struct, _, _));
        if let Ast::DefType(_, name, fields) = &*t.node {
            assert_matches!(*name.node, Ast::Id1("Point"));
            assert_eq!(2, fields.len());
            assert_eq!("x", fields[0].k.unwrap());
            assert_eq!("y", fields[1].k.unwrap());
            assert_eq!(Ast::Id1("Int"), *fields[0].v.as_ref().unwrap().node);
            assert_eq!(Ast::Id1("Int"), *fields[1].v.as_ref().unwrap().node);
        }
    }

    #[test]
    fn test_parse_type_union_color()
    {
        let input = "
        type Color
        |Red
        |Green
        |Blue
        --
        ";
        let toks = Tokenz::lexp(input).unwrap();
        let ast = Grammar::new(toks).parse_module().unwrap();
        assert_eq!(1, ast.len());

        let t = &ast[0];
        assert_matches!(*t.node, Ast::DefType(DataType::Union, _, _));
        // match_let!(Ast::DefType(DataType::Union, name, vars), t);
        if let Ast::DefType(_, name, vars) = &*t.node {
            assert_eq!(Ast::Id1("Color"), *name.node);
            assert_eq!(3, vars.len());
            assert_eq!("Red", vars[0].k.unwrap());
            assert_eq!("Green", vars[1].k.unwrap());
            assert_eq!("Blue", vars[2].k.unwrap());
            assert_eq!(None, vars[0].v);
            assert_eq!(None, vars[1].v);
            assert_eq!(None, vars[2].v);
        }
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
