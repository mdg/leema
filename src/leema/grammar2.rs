use crate::leema::ast2::{Ast, AstNode, AstResult, Loc};
use crate::leema::failure::Lresult;
// use crate::leema::lstr::Lstr;
use crate::leema::parsl::{Assoc, InfixParser, MIN_PRECEDENCE, Parsl, ParslMode, Precedence, PrefixParser};
// use crate::leema::struple::StrupleKV;
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

#[derive(Debug)]
struct ParseDefConst;

impl<'i> PrefixParser<'i> for ParseDefConst
{
    type Item = AstNode<'i>;

    fn parse(&self, p: &mut Parsl<'i>, tok: TokenSrc<'i>) -> AstResult<'i>
    {
        let id = expect_next!(p, Token::Id)?;
        let _assign = expect_next!(p, Token::Assignment)?;
        let rhs = p.parse_new(&ExprMode)?;
        Ok(AstNode::new(Ast::DefConst(id.src, rhs), Ast::loc(&tok)))
    }
}

/// Parse mode for a sequence of statements
#[derive(Debug)]
struct StmtsMode;

impl<'i> ParslMode<'i> for StmtsMode
{
    type Item = Vec<AstNode<'i>>;

    fn prefix(&self, tok: Token) -> Option<&'static PrefixParser<'i, Item=Vec<AstNode<'i>>>>
    {
        Some(match tok {
            Token::LineBegin => &ParseStmt,
            _ => {
eprintln!("no parser for token in StmtsMode: {:?}", tok);
                return None;
            }
        })
    }

    fn infix(&self, tok: Token) -> Option<&'static InfixParser<'i, Item=Vec<AstNode<'i>>>>
    {
        Some(match tok {
            Token::LineBegin => &ParseStmt,
            Token::DoubleDash => {
                return None;
            }
            _ => {
                return None;
            }
        })
    }
}

impl<'i> PrefixParser<'i> for ParseStmt
{
    type Item = Vec<AstNode<'i>>;

    fn parse(&self, p: &mut Parsl<'i>, tok: TokenSrc<'i>) -> Lresult<Vec<AstNode<'i>>>
    {
        let stmt = self.parse_stmt(p, tok)?;
        Ok(vec![stmt])
    }
}

impl<'i> InfixParser<'i> for ParseStmt
{
    type Item = Vec<AstNode<'i>>;

    fn parse(&self, p: &mut Parsl<'i>, mut left: Vec<AstNode<'i>>, tok: TokenSrc<'i>) -> Lresult<Vec<AstNode<'i>>>
    {
        let right = self.parse_stmt(p, tok)?;
        left.push(right);
        Ok(left)
    }

    fn precedence(&self) -> Precedence
    {
        MIN_PRECEDENCE
    }
}

/*
#[derive(Debug)]
struct ParseStmts;

impl<'i> PrefixParser<'i> for ParseStmts
{
    type Item = Vec<AstNode<'i>>;

    fn parse(&self, p: &mut Parsl<'i>, tok: TokenSrc<'i>) -> Lresult<Vec<AstNode<'i>>>
    {
        // skip LineBegin in tok
        let stmt = p.parse_new(&StmtMode)?;
        Ok(vec![stmt])
    }
}

/// Parse mode for a single statement
#[derive(Debug)]
struct StmtMode;

impl<'i> ParslMode<'i> for StmtMode
{
    type Item = Vec<AstNode<'i>>;

    fn prefix(&self, tok: Token) -> Option<&'static PrefixParser<'i, Item=Vec<AstNode<'i>>>>
    {
        match tok {
            Token::Const => ParseDefConst,
            Token::Let => ParseLet,
            _ => {
                ParseExpr,
            }
        }
    }
}
*/

#[derive(Debug)]
struct ParseStmt;

impl ParseStmt
{
    fn parse_stmt<'i>(&self, p: &mut Parsl<'i>, mut tok: TokenSrc<'i>) -> AstResult<'i>
    {
        // skip LineBegin for stmts
        if tok.tok == Token::LineBegin {
            p.next_if(Token::LineBegin)?;
            tok = p.next()?;
        }
        match tok.tok {
            Token::Const => self.parse_defconst(p, tok),
            Token::Let => self.parse_let(p, tok),
            _ => {
                p.reparse(&ExprMode, MIN_PRECEDENCE, tok)
            }
            /*
            Token::DefFunc => {
                // StmtMode::parse_deffunc(p, FuncClass::Func)
                AstNode::void()
            }
            Token::DefType => {
                // StmtMode::parse_deftype(p)
                AstNode::void()
            }
            Token::Import => {
                // StmtMode::parse_import(p)
                AstNode::void()
            }
            Token::DefMacro => {
                // StmtMode::parse_deffunc(p, FuncClass::Macro)
                AstNode::void()
            }
            */
                /*
                return Err(rustfail!(
                    "parse_failure",
                    "cannot parse token as statement: {}",
                    stmt_tok,
                ));
                */
        }
    }

    fn parse_defconst<'i>(&self, p: &mut Parsl<'i>, tok: TokenSrc<'i>) -> AstResult<'i>
    {
        let id = expect_next!(p, Token::Id)?;
        let _assign = expect_next!(p, Token::Assignment)?;
        let rhs = p.parse_new(&ExprMode)?;
        Ok(AstNode::new(Ast::DefConst(id.src, rhs), Ast::loc(&tok)))
    }

    fn parse_let<'i>(&self, p: &mut Parsl<'i>, tok: TokenSrc<'i>) -> AstResult<'i>
    {
        let lhs = p.parse_new(&ExprMode)?;
        let _assign = expect_next!(p, Token::Assignment)?;
        let rhs = p.parse_new(&ExprMode)?;
        let loc = Ast::loc(&tok);
        Ok(AstNode::new(Ast::Let(lhs, AstNode::void(), rhs), loc))
    }

    /*
    fn parse_deffunc<'i>(p: Parsl<'i>, fc: FuncClass) -> AstResult<'i>
    {
        let name = Grammar::parse_id(p)?;
        let loc = name.loc;
        let args = Grammar::parse_idtypes(p)?;
        expect_next!(p, Token::DoubleArrow)?;
        let body = Grammar::parse_block(p)?;
        expect_next!(p, Token::DoubleDash)?;
        Ok(AstNode::new(
            Ast::DefFunc(fc, name, args, body),
            loc,
        ))
    }

    fn parse_deftype<'i>(p: Parsl<'i>) -> AstResult<'i>
    {
        let name = Grammar::parse_id(p)?;
        let loc = name.loc;
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
        Ok(AstNode::new(data, loc))
    }

    fn parse_import<'i>(p: Parsl<'i>) -> AstResult<'i>
    {
        let module = expect_next!(p, Token::Id)?;
        Ok(AstNode::new(Ast::Import(module.src), Ast::loc(&module)))
    }
    */
}

#[derive(Debug)]
pub struct BinaryOpParser
{
    pub op: &'static str,
    pub pre: Precedence,
}

impl<'i> InfixParser<'i> for BinaryOpParser
{
    type Item = AstNode<'i>;

    fn parse(
        &self,
        p: &mut Parsl<'i>,
        left: AstNode<'i>,
        op: TokenSrc<'i>,
    ) -> AstResult<'i>
    {
        let right = p.parse_more(&ExprMode, self.pre)?;
        let ast = Ast::Op2(op.src, left, right);
        Ok(AstNode::new(ast, Ast::loc(&op)))
    }

    fn precedence(&self) -> Precedence
    {
        self.pre
    }
}

#[derive(Debug)]
struct ParseBlockx;

impl<'i> PrefixParser<'i> for ParseBlockx
{
    type Item = AstNode<'i>;

    fn parse(&self, p: &mut Parsl<'i>, tok: TokenSrc<'i>) -> AstResult<'i>
    {
        let block = Grammar::parse_block(p, Ast::loc(&tok))?;
        expect_next!(p, Token::DoubleDash)?;
        Ok(block)
    }
}

#[derive(Debug)]
struct ParseId;

impl<'i> PrefixParser<'i> for ParseId
{
    type Item = AstNode<'i>;

    fn parse(&self, _p: &mut Parsl<'i>, tok: TokenSrc<'i>) -> AstResult<'i>
    {
        Ok(AstNode::new(Ast::Id1(tok.src), Ast::loc(&tok)))
    }
}

#[derive(Debug)]
struct ParseInt;

impl<'i> PrefixParser<'i> for ParseInt
{
    type Item = AstNode<'i>;

    fn parse(&self, _p: &mut Parsl<'i>, tok: TokenSrc<'i>) -> AstResult<'i>
    {
        let i: i64 = tok.src.parse().map_err(|parsef| {
            rustfail!(
                "parse_failure",
                "int token is not an integer: {:?}",
                parsef,
            )
        })?;
        Ok(AstNode::new(Ast::ConstVal(Val::Int(i)), Ast::loc(&tok)))
    }
}

#[derive(Debug)]
struct ExprMode;

impl ExprMode
{
    fn parse_bool<'i>(tok: TokenSrc<'i>) -> AstResult<'i>
    {
        let b = match tok.src {
            "False" => false,
            "True" => true,
            _ => {
                return Err(rustfail!(
                    "parse_failure",
                    "bool token is not True or False: '{}'",
                    tok.src,
                ));
            }
        };
        Ok(AstNode::new_constval(Val::Bool(b), Ast::loc(&tok)))
    }

    /*
    fn parse_str<'i>(p: &mut Parsl<'i>, loc: Loc) -> AstResult<'i>
    {
        let mut strs = p.parse_n(ExprMode::parse_stritem)?;
        let node = match strs.len() {
            // empty vec reduces to constant ""
            0 => AstNode::new(Ast::ConstVal(Val::empty_str()), loc),
            // single item w/ constant string reduces to just that
            // constant string. single IDs stay in the strexpr so
            // they get stringified if they aren't already
            1 if strs[0].node.is_const() => strs.pop().unwrap(),
            _ => AstNode::new(Ast::StrExpr(strs), loc),
        };
        Ok(node)
    }

    fn parse_stritem<'i>(p: Parsl<'i>) -> Lresult<Option<AstNode<'i>>>
    {
        let tok = p.next()?;
        let x = match tok.tok {
            Token::StrLit => {
                let lstr = Lstr::from(tok.src.to_string());
                AstNode::new(Ast::ConstVal(Val::Str(lstr)), Ast::loc(&tok))
            }
            Token::DollarId => {
                AstNode::new(Ast::Id1(tok.src), Ast::loc(&tok))
            }
            Token::DoubleQuoteR => {
                return Ok(None);
            }
            _ => {
                return Err(rustfail!(
                    "parse_failure",
                    "expected str or id, found {:?}",
                    tok,
                ));
            }
        };
        Ok(Some(x))
    }
    */

    /*
    fn less_than<'i>(p: Parsl<'i>, left: AstNode<'i>, tok: TokenSrc<'i>) -> AstResult<'i>
    {
        ParseOutput::ok(AstNode::void())
    }
    */
}

impl<'i> ParslMode<'i> for ExprMode
{
    type Item = AstNode<'i>;

    fn prefix(&self, tok: Token) -> Option<&'static PrefixParser<'i, Item=AstNode<'i>>>
    {
        Some(match tok {
            Token::Id => &ParseId,
            Token::Int => &ParseInt,
            Token::DoubleArrow => &ParseBlockx,
            _ => {
                return None;
            }
        })
    }

    /*
    fn prefix(&self, p: Parsl<'i>, tok: TokenSrc<'i>) -> AstResult<'i>
    {
        let loc = Ast::loc(&tok);
        let expr = match tok.tok {
            Token::Bool => ExprMode::parse_bool(tok),
            /*
            Token::DoubleArrow => {
                let block = Grammar::parse_block(p)?;
                expect_next!(p, Token::DoubleDash)?;
                Ok(block)
            }
            */
            // Token::DoubleQuoteL => ExprMode::parse_str(p, loc),
            Token::Hashtag => {
                AstNode::new_constval(
                    Val::Hashtag(Lstr::from(tok.src.to_string())),
                    loc,
                )
            }
            Token::Id => {
                AstNode::new(Ast::Id1(tok.src), loc)
            }
            // Token::If => Grammar::parse_casex(p, CaseType::If, &loc),
            Token::Int => ExprMode::parse_int(tok),
            // Token::Match => Grammar::parse_casex(p, CaseType::Match, &loc),
            Token::Not => {
                let x = p.parse(Lprec::Not)?;
                AstNode::new(Ast::Op1(tok.src), loc)
            }
            Token::ParenL => {
                let inner = p.parse_new(&ExprMode)?;
                expect_next!(p, Token::ParenR)?;
                inner
            }
            /*
            Token::SquareL => {
                let items = p.parse_n(ParseXMaybeK(Token::SquareR))?;
                expect_next!(p, Token::SquareR)?;
                Ok(AstNode::new(Ast::List(items), Ast::loc(&tok)))
            }
            */
        };
        Ok(expr)
    }
    */

    fn infix(&self, tok: Token) -> Option<&'static InfixParser<'i, Item=AstNode<'i>>>
    {
        let parse = match tok {
            Token::Plus => OP_ADD,
            /*
            Token::And => OP_AND,
            Token::Dash => OP_SUBTRACT,
            Token::Equal => OP_EQ,
            Token::EqualNot => OP_NEQ,
            Token::GreaterThanEqual => OP_GTE,
            Token::LessThan => OP_LT,
            Token::LessThanEqual => OP_LTE,
            Token::Modulo => OP_MODULO,
            Token::Or => OP_OR,
            // Token::ParenL => &ParseCall,
            Token::Semicolon => OP_CONS,
            Token::Slash => OP_DIVIDE,
            Token::Star => OP_MULTIPLY,
            Token::Xor => OP_XOR,
            */
            _ => {
                return None;
            }
        };
        Some(parse)
    }
}

const OP_ADD: &'static BinaryOpParser = &BinaryOpParser {
    op: "+",
    pre: Precedence(Lprec::Add as u8, 0, Assoc::Left),
};

/*
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
*/

// Expression Parsers

// struct ConsParser;
// struct DollarParser;
// struct DotParser;
// struct PipeParser;

/*
#[derive(Debug)]
struct ParseCall;

impl InfixParser for ParseCall
{
    fn parse<'input>(
        &self,
        p: &mut Parsl<'input>,
        left: AstNode<'input>,
        _tok: TokenSrc<'input>,
    ) -> AstResult<'input>
    {
        let args = p.parse_n(&ParseXMaybeK(Token::ParenR))?;
        expect_next!(p, Token::ParenR)?;
        let loc = left.loc;
        Ok(AstNode::new(Ast::Call(left, args), loc))
    }

    fn precedence(&self) -> Precedence
    {
        Precedence(15, 0, Assoc::Left)
    }
}

#[derive(Debug)]
struct ParseXMaybeK(Token);

impl<'i> ItemParser<'i> for ParseXMaybeK
{
    type Item = (Option<&'i str>, AstNode<'i>);

    fn parse(&self, p: &mut Parsl<'i>) -> Lresult<(Self::Item, bool)>
    {
        let first = p.parse_new(&ExprMode)?;

        let comma = p.peek()?;
        let result =
            match comma.tok {
                Token::Colon => {
                    self.next()?;
                    let value = p.parse_new(&ExprMode)?;
                    let more = p.next_if(Token::Comma)?.is_some();
                    ((Some(first), value), more)
                }
                Token::Comma => {
                    self.next()?;
                    ((None, first), true)
                }
                t if t == self.0 => {
                    ((None, first), false)
                }
                _ => {
                    return Err(rustfail!(
                        "parse_failure",
                        "expected ',' or ':' or '{}' found {:?}",
                        self.0,
                        comma,
                    ));
                }
            };
        Ok(result)
    }
}

/// Parse the cases of an if or match expression
#[derive(Debug)]
struct ParseCase(Token);

impl<'i> ItemParser<'i> for ParseCase
{
    type Item = ast2::Case<'i>;

    fn parse(&self, p: &mut Parsl<'i>) -> Lresult<(Self::Item, bool)>
    {
        p.skip_if(Token::LineBegin)?;
        let peeked = p.peek()?;
        let case = match peeked.tok {
            Token::CasePipe => {
                p.next()?;
                let cond = match p.next_if(Token::Else)? {
                    Some(tok) => {
                        let v = Ast::ConstVal(Val::Bool(true));
                        AstNode::new(v, Ast::loc(&tok))
                    }
                    None => p.parse_new(&ExprMode)?,
                };
                expect_next!(p, Token::DoubleArrow)?;
                let body = Grammar::parse_block(p)?;
                Some(ast2::Case::new(cond, body))
            }
            Token::DoubleDash => {
                None
            }
            _ => {
                return Err(rustfail!(
                    "parse_failure",
                    "expected | or -- found {:?}",
                    peeked,
                ));
            }
        };
        Ok(case)
    }
}
*/


/// Grammar is a collection of functions for parsing a stream of tokens
pub struct Grammar<'input>
{
    p: Parsl<'input>,
}

impl<'input> Grammar<'input>
{
    pub fn new(items: Vec<TokenSrc<'input>>) -> Grammar<'input>
    {
        Grammar {
            p: Parsl::new(items),
        }
    }

    pub fn parse_module(&mut self) -> Lresult<Vec<AstNode<'input>>>
    {
        let result = self.p.parse_new(&StmtsMode)?;
        let tok = self.p.peek()?;
        if tok.tok != Token::EOF {
            Err(rustfail!(
                "parse_failure",
                "failed to complete parsing. stopped at {}",
                tok,
            ))
        } else {
            Ok(result)
        }
    }

    /// Parse the body of a function. Also eat the trailing
    fn parse_block(p: &mut Parsl<'input>, loc: Loc) -> AstResult<'input>
    {
        let mut stmts = p.parse_new(&StmtsMode)?;
        let node = match stmts.len() {
            0 => AstNode::void(),
            1 => stmts.pop().unwrap(),
            _ => AstNode::new(Ast::Block(stmts), loc),
        };
        Ok(node)
    }

    /*
    /// Parse an if or match expression (which have the same structure
    fn parse_casex(p: Parsl<'input>, ct: CaseType, loc: &Loc) -> AstResult<'input>
    {
        let tok = p.peek()?;
        let input = match tok.tok {
            Token::CasePipe => {
                // no expression, go straight to cases
                None
            }
            _ => {
                // get the expression, then go to cases
                Some(p.parse_new(&ExprMode)?)
            }
        };
        let cases = p.parse_n(Grammar::parse_case)?;
        expect_next!(p, Token::DoubleDash)?;
        let node = Ast::Case(ct, input, cases);
        Ok(AstNode::new(node, *loc))
    }

    /// Parse an idtype pair, ".id:Type"
    /// skip a LineBegin before the idtype if there is one
    pub fn parse_idtypes(
        p: &mut Parsl<'input>,
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
                    Some(p.parse_new(&ExprMode)?)
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
    pub fn parse_variant(
        p: &mut Parsl<'input>,
    ) -> Lresult<Option<(Option<&'input str>, Option<AstNode<'input>>)>>
    {
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
                Ok(Some((Some(name.src), var)))
            }
            Token::DoubleDash => {
                // leave the doubledash unconsumed
                Ok(None)
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
    */
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
    fn test_parse_blockx()
    {
        let input = ">>
            let a := 1
            let b := 2
            a + b
            --
        ";
        Grammar::new(Tokenz::lexp(input).unwrap())
            .parse_module()
            .unwrap();
    }

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
        let ast = p.parse_module().unwrap();

        assert_eq!(1, ast.len());
        assert_matches!(*ast[0].node, Ast::Let(_, _, _));
        if let Ast::Let(lhs, _, rhs) = &*ast[0].node {
            assert_eq!(Ast::Id1("x"), *lhs.node);
            assert_matches!(*rhs.node, Ast::Op2("+", _, _));
            if let Ast::Op2(_, op1, op2) = &*rhs.node {
                assert_eq!(Ast::ConstVal(Val::Int(5)), *op1.node);
                assert_eq!(Ast::Id1("y"), *op2.node);
            }
        }
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
    fn test_parse_list_oneline()
    {
        let input = "let l := [1, 2, 3, 4]";
        let toks = Tokenz::lexp(input).unwrap();
        let ast = Grammar::new(toks).parse_module().unwrap();
        assert_eq!(1, ast.len());
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
}
