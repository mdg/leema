use crate::leema::ast2::{self, Ast, AstNode, AstResult, CaseType, FuncClass};
use crate::leema::failure::Lresult;
use crate::leema::lstr::Lstr;
use crate::leema::parser::{
    Assoc, BinaryOpParser, InfixParser, ParseTable, Parser, Precedence,
    PrefixParser,
};
use crate::leema::struple::StrupleKV;
use crate::leema::token::{Token, TokenSrc};
use crate::leema::val::Val;

enum Lprec
{
    Minimum,
    Comma,
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
        let id = p.expect_next(Token::Id)?;
        let _assign = p.expect_next(Token::Assignment)?;
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
        let body = Grammar::parse_block(p)?;
        p.expect_next(Token::DoubleDash)?;
        Ok(AstNode::new(
            Ast::DefFunc(self.0, name, args, body),
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
        let lhs = p.parse_new_expr()?;
        let _assign = p.expect_next(Token::Assignment)?;
        let rhs = p.parse_new_expr()?;
        Ok(AstNode::new(
            Ast::Let(lhs, AstNode::void(), rhs),
            Ast::loc(&left),
        ))
    }
}

/* Expression Parsers */

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
        p.expect_next(Token::DoubleDash)?;
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
struct MatchParser;

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

const OP_MULTIPLY: &'static BinaryOpParser = &BinaryOpParser {
    op: "*",
    pre: Precedence(Lprec::Multiply as u8, 0, Assoc::Left),
};

const OP_DIVIDE: &'static BinaryOpParser = &BinaryOpParser {
    op: "/",
    pre: Precedence(Lprec::Multiply as u8, 0, Assoc::Left),
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
    (Token::Dollar, None, None, None),
    // operators (boolean)
    (Token::And, None, None, None),
    (Token::Not, None, None, None),
    (Token::Or, None, None, None),
    (Token::Xor, None, None, None),
    // operators (comparison)
    (Token::Equal, None, None, Some(OP_EQ)),
    (Token::EqualNot, None, None, Some(OP_NEQ)),
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
        self.p.parse_stmts()
    }

    /// Parse the body of a function. Also eat the trailing
    fn parse_block(p: &mut Parser<'input>) -> AstResult<'input>
    {
        let arrow = p.expect_next(Token::DoubleArrow)?;
        match p.peek_token()? {
            Token::LineBegin => {
                let mut stmts = p.parse_stmts()?;
                let node = match stmts.len() {
                    0 => AstNode::void(),
                    1 => stmts.pop().unwrap(),
                    _ => AstNode::new(Ast::Block(stmts), Ast::loc(&arrow)),
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
            p.expect_next(Token::LineBegin)?;
            let peeked = p.peek()?;
            match peeked.tok {
                Token::Pipe => {
                    p.next()?;
                    let cond = match p.next_if(Token::Else)? {
                        Some(tok) => {
                            let v = Ast::ConstVal(Val::Bool(true));
                            AstNode::new(v, Ast::loc(&tok))
                        }
                        None => p.parse_new_expr()?,
                    };
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
            p.next_if(Token::LineBegin)?;
            let peeked = p.peek()?;
            let id = match peeked.tok {
                Token::Dot => {
                    let _dot = p.expect_next(Token::Dot)?;
                    let name = p.expect_next(Token::Id)?;
                    Some(name.src)
                }
                Token::Colon => {
                    // no ID, fall through to type
                    None
                }
                Token::DoubleArrow => {
                    break;
                }
                Token::DoubleDash => {
                    break;
                }
                Token::EOF => {
                    return Err(rustfail!(
                        "parse_failure",
                        "expected : or . found EOF",
                    ));
                }
                _ => {
                    return Err(rustfail!(
                        "parse_failue",
                        "expected . or :, found {:?}",
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
        let id = p.expect_next(Token::Id)?;
        Ok(AstNode::new(Ast::Id1(id.src), Ast::loc(&id)))
    }
}


#[cfg(test)]
mod tests
{
    use super::Grammar;
    use crate::leema::ast2::Ast;
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
    fn test_parse_let()
    {
        let input = "let x := 5 + y";
        let toks = Tokenz::lexp(input).unwrap();
        let mut p = Grammar::new(toks);
        p.parse_module().unwrap();
    }

    #[test]
    fn test_parse_operators()
    {
        let input = r#"func ops >>
            1 - 2 - 3
            x * y * z
            9 / 3
            4 == 4
            7 != 8
        --
        "#;
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
