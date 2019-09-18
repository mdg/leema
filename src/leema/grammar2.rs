use crate::leema::ast2::{self, Ast, AstNode, AstResult, Loc};
use crate::leema::failure::Lresult;
use crate::leema::lstr::Lstr;
use crate::leema::parsl::{
    Assoc, InfixParser, ParseFirst, ParseMore, Parsl, ParslMode, Precedence,
    PrefixParser, MIN_PRECEDENCE,
};
use crate::leema::struple::StrupleKV;
use crate::leema::token::{Token, TokenSrc};
use crate::leema::val::Val;


enum Lprec
{
    Minimum,
    Comma,
    Dollar,
    Pipe,
    Cons,
    Or,
    And,
    Not,
    Equal,
    LessThan,
    Add,
    Multiply,
    Negative,
    Call,
    Generic,
    Dot,
    DoubleColon,
    Postfix,
}

const COMMA_PRECEDENCE: Precedence =
    Precedence(Lprec::Comma as u8, 0, Assoc::Left);

const PARSE_FAIL: &'static str = "parse_failure";

impl From<Lprec> for Precedence
{
    fn from(p: Lprec) -> Self
    {
        Precedence(p as u8, 0, Assoc::Left)
    }
}

#[derive(Debug)]
struct ParseDefConst;

impl PrefixParser for ParseDefConst
{
    type Item = AstNode;

    fn parse(&self, p: &mut Parsl, tok: TokenSrc) -> AstResult
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

impl ParslMode for StmtsMode
{
    type Item = Vec<AstNode>;

    fn prefix(
        &self,
        tok: Token,
    ) -> Option<&'static PrefixParser<Item = Vec<AstNode>>>
    {
        Some(match tok {
            Token::LineBegin => &ParseStmt,
            _ => {
                eprintln!("no parser for token in StmtsMode: {:?}", tok);
                return None;
            }
        })
    }

    fn infix(
        &self,
        tok: Token,
    ) -> Option<&'static InfixParser<Item = Vec<AstNode>>>
    {
        match tok {
            Token::LineBegin => Some(&ParseStmt),
            Token::DoubleDash => None,
            Token::Else => None,
            Token::Pipe => None,
            _ => None,
        }
    }
}

impl PrefixParser for ParseStmt
{
    type Item = Vec<AstNode>;

    fn parse(&self, p: &mut Parsl, mut tok: TokenSrc) -> Lresult<Vec<AstNode>>
    {
        // skip LineBegin for stmts
        if tok.tok == Token::LineBegin {
            p.next_if(Token::LineBegin)?;
            tok = p.next()?;
        }
        let stmt = self.parse_stmt(p, tok)?;
        Ok(vec![stmt])
    }
}

impl InfixParser for ParseStmt
{
    type Item = Vec<AstNode>;

    fn parse(
        &self,
        p: &mut Parsl,
        mut left: Vec<AstNode>,
        tok: TokenSrc,
    ) -> Lresult<Vec<AstNode>>
    {
        if tok.tok != Token::LineBegin {
            return Err(rustfail!(
                "parse_failure",
                "expected LineBegin, found {}",
                tok,
            ));
        }
        match p.peek_token()? {
            Token::DoubleDash => {}
            Token::Pipe => {}
            _ => {
                let next = p.next()?;
                let right = self.parse_stmt(p, next)?;
                left.push(right);
            }
        }
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

impl PrefixParser for ParseStmts
{
    type Item = Vec<AstNode>;

    fn parse(&self, p: &mut Parsl, tok: TokenSrc) -> Lresult<Vec<AstNode>>
    {
        // skip LineBegin in tok
        let stmt = p.parse_new(&StmtMode)?;
        Ok(vec![stmt])
    }
}

/// Parse mode for a single statement
#[derive(Debug)]
struct StmtMode;

impl ParslMode for StmtMode
{
    type Item = Vec<AstNode>;

    fn prefix(&self, tok: Token) -> Option<&'static PrefixParser<Item=Vec<AstNode>>>
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
    fn parse_stmt(&self, p: &mut Parsl, tok: TokenSrc) -> AstResult
    {
        match tok.tok {
            Token::Const => ParseStmt::parse_defconst(p, tok),
            Token::Func => ParseStmt::parse_deffunc(p),
            Token::Macro => ParseStmt::parse_defmacro(p),
            Token::Import => ParseStmt::parse_import(p),
            Token::Let => ParseStmt::parse_let(p, tok),
            Token::Return => ParseStmt::parse_return(p, Ast::loc(&tok)),
            Token::Type => ParseStmt::parse_deftype(p),
            _ => p.reparse(&ExprMode, MIN_PRECEDENCE, tok),
        }
    }

    fn parse_defconst(p: &mut Parsl, tok: TokenSrc) -> AstResult
    {
        let id = expect_next!(p, Token::Id)?;
        let _assign = expect_next!(p, Token::Assignment)?;
        let rhs = p.parse_new(&ExprMode)?;
        Ok(AstNode::new(Ast::DefConst(id.src, rhs), Ast::loc(&tok)))
    }

    fn parse_deffunc(p: &mut Parsl) -> AstResult
    {
        let base_name = Grammar::parse_id(p)?;
        // skip a newline if there is one
        p.skip_if(Token::LineBegin)?;
        let loc = base_name.loc;
        let name = match p.next_if(Token::SquareL)? {
            Some(_) => {
                let generic_args = IdTypeMode::parse(p)?;
                let gname =
                    AstNode::new(Ast::Generic(base_name, generic_args), loc);
                expect_next!(p, Token::SquareR)?;
                gname
            }
            None => base_name,
        };
        let args = if p.peek_token()? == Token::DoubleArrow {
            StrupleKV::new()
        } else {
            IdTypeMode::parse(p)?
        };
        p.skip_if(Token::LineBegin)?;

        let body_start = p.peek()?;
        let body = match body_start.tok {
            Token::DoubleArrow => {
                let arrow = expect_next!(p, Token::DoubleArrow)?;
                if let Some(rust_block) = p.next_if(Token::RustBlock)? {
                    AstNode::new(Ast::RustBlock, Ast::loc(&rust_block))
                } else {
                    Grammar::parse_block(p, Ast::loc(&arrow))?
                }
            }
            Token::CasePipe => {
                let cases = p.parse_new(&CaseMode)?;
                let ast = Ast::Case(ast2::CaseType::Match, None, cases);
                AstNode::new(ast, Ast::loc(&body_start))
            }
            _ => {
                return Err(rustfail!(
                    "parse_failure",
                    "expected >> or | found {:?}",
                    body_start,
                ));
            }
        };
        p.skip_if(Token::LineBegin)?;
        expect_next!(p, Token::DoubleDash)?;
        Ok(AstNode::new(Ast::DefFunc(name, args, body), loc))
    }

    fn parse_defmacro(p: &mut Parsl) -> AstResult
    {
        let name = expect_next!(p, Token::Id)?;
        // skip a newline if there is one
        p.skip_if(Token::LineBegin)?;
        let args = Self::parse_defmacro_args(p)?;
        p.skip_if(Token::LineBegin)?;

        let body_start = p.peek()?;
        let body = match body_start.tok {
            Token::DoubleArrow => {
                let arrow = expect_next!(p, Token::DoubleArrow)?;
                Grammar::parse_block(p, Ast::loc(&arrow))?
            }
            Token::CasePipe => {
                let cases = p.parse_new(&CaseMode)?;
                let ast = Ast::Case(ast2::CaseType::Match, None, cases);
                AstNode::new(ast, Ast::loc(&body_start))
            }
            _ => {
                return Err(rustfail!(
                    "parse_failure",
                    "expected >> or | found {:?}",
                    body_start,
                ));
            }
        };
        p.skip_if(Token::LineBegin)?;
        expect_next!(p, Token::DoubleDash)?;
        Ok(AstNode::new(
            Ast::DefMacro(name.src, args, body),
            Ast::loc(&name),
        ))
    }

    fn parse_defmacro_args(p: &mut Parsl) -> Lresult<Vec<&'static str>>
    {
        let mut args = vec![];
        loop {
            let tok = p.peek()?;
            match tok.tok {
                Token::DoubleArrow => {
                    break;
                }
                Token::Id => {
                    let arg = p.next()?;
                    args.push(arg.src);
                }
                _ => {
                    return Err(rustfail!(
                        "parse_failure",
                        "expected Id or >> found {:?}",
                        tok
                    ));
                }
            }
        }
        Ok(args)
    }

    fn parse_deftype(p: &mut Parsl) -> AstResult
    {
        let base_name = Grammar::parse_id(p)?;
        let loc = base_name.loc;
        let name = match p.next_if(Token::SquareL)? {
            Some(_) => {
                let gen_args = IdTypeMode::parse(p)?;
                let gen = AstNode::new(Ast::Generic(base_name, gen_args), loc);
                expect_next!(p, Token::SquareR)?;
                gen
            }
            None => base_name,
        };
        p.skip_if(Token::LineBegin)?;
        let tok = p.peek()?;
        let data = match tok.tok {
            // id and : both indicate struct, probably just id?
            Token::Id => {
                let fields = IdTypeMode::parse(p)?;
                Ast::DefType(ast2::DataType::Struct, name, fields)
            }
            Token::Colon => {
                let fields = IdTypeMode::parse(p)?;
                Ast::DefType(ast2::DataType::Struct, name, fields)
            }
            // -- indicates token type
            Token::DoubleDash => {
                Ast::DefType(ast2::DataType::Struct, name, ast2::Xlist::new())
            }
            // | indicates enum type
            Token::CasePipe => {
                let variantvec = p.parse_new(&DefVariantsMode)?;
                let variants = StrupleKV::from(variantvec);
                Ast::DefType(ast2::DataType::Union, name, variants)
            }
            // anything else is a failure
            _ => {
                return Err(rustfail!(
                    "parse_failure",
                    "expected id or : or | or -- found {:?}",
                    tok,
                ));
            }
        };
        p.skip_if(Token::LineBegin)?;
        expect_next!(p, Token::DoubleDash)?;
        Ok(AstNode::new(data, loc))
    }

    fn parse_import(p: &mut Parsl) -> AstResult
    {
        let module = expect_next!(p, Token::Id)?;
        Ok(AstNode::new(Ast::Import(module.src), Ast::loc(&module)))
    }

    fn parse_let(p: &mut Parsl, tok: TokenSrc) -> AstResult
    {
        let lhs = p.parse_new(&ExprMode)?;
        let _assign = expect_next!(p, Token::Assignment)?;
        let rhs = p.parse_new(&ExprMode)?;
        let loc = Ast::loc(&tok);
        Ok(AstNode::new(Ast::Let(lhs, AstNode::void(), rhs), loc))
    }

    fn parse_return(p: &mut Parsl, loc: Loc) -> AstResult
    {
        let result = p.parse_new(&ExprMode)?;
        Ok(AstNode::new(Ast::Return(result), loc))
    }
}

// IdTypeMode

/// IdTypeMode for lists of: "id:Type" | ":Type"
#[derive(Debug)]
struct IdTypeMode;

impl IdTypeMode
{
    pub fn parse(p: &mut Parsl) -> Lresult<ast2::Xlist>
    {
        let idtypes = p.parse_new(&IdTypeMode)?;
        let fields = StrupleKV::from(idtypes);
        Ok(fields)
    }
}

impl ParslMode for IdTypeMode
{
    type Item = Vec<(Option<&'static str>, AstNode)>;

    fn prefix(
        &self,
        tok: Token,
    ) -> Option<&'static PrefixParser<Item = Self::Item>>
    {
        match tok {
            Token::Id => Some(&ParseFirst(&ParseIdType)),
            Token::Colon => Some(&ParseFirst(&ParseIdType)),
            _ => None,
        }
    }

    fn infix(
        &self,
        tok: Token,
    ) -> Option<&'static InfixParser<Item = Self::Item>>
    {
        match tok {
            Token::Id => Some(&ParseMore(&ParseIdType, MIN_PRECEDENCE)),
            Token::Colon => Some(&ParseMore(&ParseIdType, MIN_PRECEDENCE)),
            _ => None,
        }
    }
}

#[derive(Debug)]
struct ParseIdType;

impl PrefixParser for ParseIdType
{
    type Item = (Option<&'static str>, AstNode);

    fn parse(&self, p: &mut Parsl, tok: TokenSrc) -> Lresult<Self::Item>
    {
        let idtype = match tok.tok {
            Token::Id => {
                expect_next!(p, Token::Colon)?;
                let typ = p.parse_new(&TypexMode)?;
                (Some(tok.src), typ)
            }
            Token::Colon => {
                let typ = p.parse_new(&TypexMode)?;
                (None, typ)
            }
            _ => {
                return Err(rustfail!(
                    "parse_failure",
                    "expected id or : found {:?}",
                    tok,
                ));
            }
        };
        p.skip_if(Token::LineBegin)?;
        Ok(idtype)
    }
}

// TYPEX MODE

/// Typex mode for Type expressions
/// which are slightly different than value expressions
#[derive(Debug)]
struct TypexMode;

impl ParslMode for TypexMode
{
    type Item = AstNode;

    fn prefix(
        &self,
        tok: Token,
    ) -> Option<&'static PrefixParser<Item = Self::Item>>
    {
        match tok {
            Token::Id => Some(&ParseId),
            Token::ParenL => Some(&ParseTypeTuple),
            Token::SquareL => Some(&ParseListType),
            _ => None,
        }
    }
}

#[derive(Debug)]
struct ParseTypeTuple;

impl PrefixParser for ParseTypeTuple
{
    type Item = AstNode;

    fn parse(&self, p: &mut Parsl, tok: TokenSrc) -> AstResult
    {
        assert_eq!(Token::ParenL, tok.tok);
        let inner = IdTypeMode::parse(p)?;
        expect_next!(p, Token::ParenR)?;
        Ok(AstNode::new(
            Ast::Tuple(StrupleKV::from(inner)),
            Ast::loc(&tok),
        ))
    }
}

/// DefVariantsMode for type variants
#[derive(Debug)]
struct DefVariantsMode;

impl ParslMode for DefVariantsMode
{
    type Item = Vec<(Option<&'static str>, AstNode)>;

    fn prefix(
        &self,
        tok: Token,
    ) -> Option<&'static PrefixParser<Item = Self::Item>>
    {
        match tok {
            Token::CasePipe => Some(&ParseFirst(&ParseVariant)),
            _ => None,
        }
    }

    fn infix(
        &self,
        tok: Token,
    ) -> Option<&'static InfixParser<Item = Self::Item>>
    {
        match tok {
            Token::CasePipe => Some(&ParseMore(&ParseVariant, MIN_PRECEDENCE)),
            _ => None,
        }
    }
}

#[derive(Debug)]
struct ParseVariant;

impl PrefixParser for ParseVariant
{
    type Item = (Option<&'static str>, AstNode);

    fn parse(&self, p: &mut Parsl, tok: TokenSrc) -> Lresult<Self::Item>
    {
        assert_eq!(Token::CasePipe, tok.tok);
        let name = expect_next!(p, Token::Id)?;
        // check if it's an empty token variant
        let peeked = p.peek_token()?;
        if peeked == Token::CasePipe || peeked == Token::LineBegin {
            Ok((Some(name.src), AstNode::new(Ast::Void, Ast::loc(&name))))
        } else {
            let name_id = AstNode::new(Ast::Id1(name.src), Ast::loc(&name));
            let fields = StrupleKV::from(p.parse_new(&IdTypeMode)?);
            let loc = name_id.loc;
            let var = Ast::DefType(ast2::DataType::Struct, name_id, fields);
            Ok((Some(name.src), AstNode::new(var, loc)))
        }
    }
}

// EXPRESSION PARSING

#[derive(Debug)]
pub struct BinaryOpParser
{
    pub op: &'static str,
    pub pre: Precedence,
}

impl InfixParser for BinaryOpParser
{
    type Item = AstNode;

    fn parse(&self, p: &mut Parsl, left: AstNode, op: TokenSrc) -> AstResult
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
pub struct ParsePostfixOp;

impl InfixParser for ParsePostfixOp
{
    type Item = AstNode;

    fn parse(&self, _: &mut Parsl, left: AstNode, op: TokenSrc) -> AstResult
    {
        let ast = Ast::Op1(op.src, left);
        Ok(AstNode::new(ast, Ast::loc(&op)))
    }

    fn precedence(&self) -> Precedence
    {
        Precedence(Lprec::Postfix as u8, 0, Assoc::Left)
    }
}

#[derive(Debug)]
struct ParseBlockx;

impl PrefixParser for ParseBlockx
{
    type Item = AstNode;

    fn parse(&self, p: &mut Parsl, tok: TokenSrc) -> AstResult
    {
        let block = Grammar::parse_block(p, Ast::loc(&tok))?;
        expect_next!(p, Token::DoubleDash)?;
        Ok(block)
    }
}

#[derive(Debug)]
struct ParseBool;

impl PrefixParser for ParseBool
{
    type Item = AstNode;

    fn parse(&self, _p: &mut Parsl, tok: TokenSrc) -> AstResult
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
}

#[derive(Debug)]
struct ParseHashtag;

impl PrefixParser for ParseHashtag
{
    type Item = AstNode;

    fn parse(&self, _p: &mut Parsl, tok: TokenSrc) -> AstResult
    {
        let tag = Val::Hashtag(Lstr::from(tok.src.to_string()));
        Ok(AstNode::new(Ast::ConstVal(tag), Ast::loc(&tok)))
    }
}

#[derive(Debug)]
struct ParseId;

impl PrefixParser for ParseId
{
    type Item = AstNode;

    fn parse(&self, _p: &mut Parsl, tok: TokenSrc) -> AstResult
    {
        match tok.tok {
            Token::Id => Ok(AstNode::new(Ast::Id1(tok.src), Ast::loc(&tok))),
            Token::DollarId => {
                Ok(AstNode::new(Ast::Id1(&tok.src[1..]), Ast::loc(&tok)))
            }
            _ => {
                Err(rustfail!(
                    PARSE_FAIL,
                    "cannot parse token as id: {:?}",
                    tok,
                ))
            }
        }
    }
}

impl InfixParser for ParseId
{
    type Item = AstNode;

    fn parse(&self, p: &mut Parsl, left: AstNode, _tok: TokenSrc) -> AstResult
    {
        if let Ast::Id1(first) = *left.node {
            let second = expect_next!(p, Token::Id)?;
            Ok(AstNode::new(Ast::Id2(first, second.src), left.loc))
        } else {
            Err(rustfail!(
                "parse_failure",
                "expected id1::id2, but id1 is {:?}",
                left,
            ))
        }
    }

    fn precedence(&self) -> Precedence
    {
        Precedence(Lprec::DoubleColon as u8, 0, Assoc::Right)
    }
}

#[derive(Debug)]
struct ParseInt;

impl PrefixParser for ParseInt
{
    type Item = AstNode;

    fn parse(&self, _p: &mut Parsl, tok: TokenSrc) -> AstResult
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
struct ParseNegative;

impl PrefixParser for ParseNegative
{
    type Item = AstNode;

    fn parse(&self, p: &mut Parsl, tok: TokenSrc) -> AstResult
    {
        let x = p.parse_more(&ExprMode, Precedence::from(Lprec::Negative))?;
        Ok(AstNode::new(Ast::Op1("-", x), Ast::loc(&tok)))
    }
}

#[derive(Debug)]
struct ParseNot;

impl PrefixParser for ParseNot
{
    type Item = AstNode;

    fn parse(&self, p: &mut Parsl, tok: TokenSrc) -> AstResult
    {
        let x = p.parse_more(&ExprMode, Precedence::from(Lprec::Not))?;
        Ok(AstNode::new(Ast::Op1("not", x), Ast::loc(&tok)))
    }
}

#[derive(Debug)]
struct ParseParen;

impl PrefixParser for ParseParen
{
    type Item = AstNode;

    fn parse(&self, p: &mut Parsl, tok: TokenSrc) -> AstResult
    {
        p.skip_if(Token::LineBegin)?;
        if p.next_if(Token::ParenR)?.is_some() {
            return Err(rustfail!(
                "parse_failure",
                "empty tuples () are not allowed: {}",
                tok,
            ));
        }
        let mut inner = XlistMode::parse(p)?;
        expect_next!(p, Token::ParenR)?;
        let node = if inner.len() == 1 && inner[0].k.is_none() {
            inner.0.pop().unwrap().v
        } else {
            AstNode::new(Ast::Tuple(inner), Ast::loc(&tok))
        };
        Ok(node)
    }
}

#[derive(Debug)]
struct ParseList;

impl PrefixParser for ParseList
{
    type Item = AstNode;

    fn parse(&self, p: &mut Parsl, tok: TokenSrc) -> AstResult
    {
        assert_eq!(Token::SquareL, tok.tok);
        let items = if p.peek_token()? == Token::SquareR {
            StrupleKV::new()
        } else {
            XlistMode::parse(p)?
        };
        expect_next!(p, Token::SquareR)?;
        Ok(AstNode::new(Ast::List(items), Ast::loc(&tok)))
    }
}

#[derive(Debug)]
struct ParseListType;

impl PrefixParser for ParseListType
{
    type Item = AstNode;

    fn parse(&self, p: &mut Parsl, tok: TokenSrc) -> AstResult
    {
        assert_eq!(Token::SquareL, tok.tok);
        let inner = p.parse_new(&TypexMode)?;
        expect_next!(p, Token::SquareR)?;
        let items = StrupleKV::from(vec![(None, inner)]);
        Ok(AstNode::new(Ast::List(items), Ast::loc(&tok)))
    }
}

#[derive(Debug)]
struct ParseStr;

impl PrefixParser for ParseStr
{
    type Item = AstNode;

    fn parse(&self, p: &mut Parsl, tok: TokenSrc) -> AstResult
    {
        let loc = Ast::loc(&tok);
        if p.next_if(Token::DoubleQuoteR)?.is_some() {
            return Ok(AstNode::new(Ast::ConstVal(Val::empty_str()), loc));
        }

        let mut strs = p.parse_new(&StrxMode)?;
        expect_next!(p, Token::DoubleQuoteR)?;
        let node = match strs.len() {
            // single item w/ constant string reduces to just that
            // constant string. single IDs stay in the strexpr so
            // they get stringified if they aren't already
            1 if strs[0].node.is_const() => strs.pop().unwrap(),
            _ => AstNode::new(Ast::StrExpr(strs), loc),
        };
        Ok(node)
    }
}

#[derive(Debug)]
struct ParseUnderscore;

impl PrefixParser for ParseUnderscore
{
    type Item = AstNode;

    fn parse(&self, _p: &mut Parsl, tok: TokenSrc) -> AstResult
    {
        Ok(AstNode::new(Ast::Wildcard, Ast::loc(&tok)))
    }
}

#[derive(Debug)]
struct ExprMode;

impl ExprMode
{
    /*
    fn less_than(p: Parsl, left: AstNode, tok: TokenSrc) -> AstResult
    {
        ParseOutput::ok(AstNode::void())
    }
    */
}

impl ParslMode for ExprMode
{
    type Item = AstNode;

    fn prefix(
        &self,
        tok: Token,
    ) -> Option<&'static PrefixParser<Item = AstNode>>
    {
        Some(match tok {
            Token::Bool => &ParseBool,
            Token::Dash => &ParseNegative,
            Token::DoubleArrow => &ParseBlockx,
            Token::DoubleQuoteL => &ParseStr,
            Token::Hashtag => &ParseHashtag,
            Token::Id => &ParseId,
            Token::If => &ParseIf,
            Token::Int => &ParseInt,
            Token::Match => &ParseMatch,
            Token::Not => &ParseNot,
            Token::ParenL => &ParseParen,
            Token::SquareL => &ParseList,
            Token::Underscore => &ParseUnderscore,
            _ => {
                return None;
            }
        })
    }

    fn infix(&self, tok: Token)
        -> Option<&'static InfixParser<Item = AstNode>>
    {
        Some(match tok {
            // boolean operators
            Token::And => OP_AND,
            Token::Or => OP_OR,
            Token::Xor => OP_XOR,
            // comparison operators
            Token::Equal => OP_EQ,
            Token::EqualNot => OP_NEQ,
            Token::AngleL => OP_LT,
            Token::AngleR => OP_GT,
            Token::GreaterThanEqual => OP_GTE,
            Token::LessThanEqual => OP_LTE,
            // arithmetic operators
            Token::Dash => OP_SUBTRACT,
            Token::Modulo => OP_MODULO,
            Token::Plus => OP_ADD,
            Token::Slash => OP_DIVIDE,
            Token::Star => OP_MULTIPLY,
            // other operators
            Token::ConcatNewline => &ParsePostfixOp,
            Token::DoubleColon => &ParseId,
            Token::ParenL => &ParseCall,
            Token::SquareL => &ParseGeneric,
            Token::Semicolon => OP_CONS,
            _ => {
                return None;
            }
        })
    }
}

// arithmetic operators

/// Addition parser
const OP_ADD: &'static BinaryOpParser = &BinaryOpParser {
    op: "+",
    pre: Precedence(Lprec::Add as u8, 0, Assoc::Left),
};

/// Subtraction parser
const OP_SUBTRACT: &'static BinaryOpParser = &BinaryOpParser {
    op: "-",
    pre: Precedence(Lprec::Add as u8, 0, Assoc::Left),
};

/// Multiplication parser
const OP_MULTIPLY: &'static BinaryOpParser = &BinaryOpParser {
    op: "*",
    pre: Precedence(Lprec::Multiply as u8, 0, Assoc::Left),
};

/// Division parser
const OP_DIVIDE: &'static BinaryOpParser = &BinaryOpParser {
    op: "/",
    pre: Precedence(Lprec::Multiply as u8, 0, Assoc::Left),
};

/// Modulo parser
const OP_MODULO: &'static BinaryOpParser = &BinaryOpParser {
    op: "mod",
    pre: Precedence(Lprec::Multiply as u8, 0, Assoc::Left),
};

/// List cons parser
const OP_CONS: &'static BinaryOpParser = &BinaryOpParser {
    op: ";",
    pre: Precedence(Lprec::Cons as u8, 0, Assoc::Right),
};

// BOOLEAN OPERATORS

/// Boolean AND operator
const OP_AND: &'static BinaryOpParser = &BinaryOpParser {
    op: "and",
    pre: Precedence(Lprec::And as u8, 0, Assoc::Left),
};

/// Boolean OR operator
const OP_OR: &'static BinaryOpParser = &BinaryOpParser {
    op: "or",
    pre: Precedence(Lprec::Or as u8, 0, Assoc::Left),
};

/// Boolean XOR operator
const OP_XOR: &'static BinaryOpParser = &BinaryOpParser {
    op: "xor",
    pre: Precedence(Lprec::Or as u8, 0, Assoc::Left),
};

// COMPARISON OPERATORS

/// Equality operator
const OP_EQ: &'static BinaryOpParser = &BinaryOpParser {
    op: "==",
    pre: Precedence(Lprec::Equal as u8, 0, Assoc::Left),
};

/// Inequality parser
const OP_NEQ: &'static BinaryOpParser = &BinaryOpParser {
    op: "!=",
    pre: Precedence(Lprec::Equal as u8, 0, Assoc::Left),
};

/// Greater than parser
const OP_GT: &'static BinaryOpParser = &BinaryOpParser {
    op: ">",
    pre: Precedence(Lprec::LessThan as u8, 0, Assoc::Left),
};

/// Greater than or equal parser
const OP_GTE: &'static BinaryOpParser = &BinaryOpParser {
    op: ">=",
    pre: Precedence(Lprec::LessThan as u8, 0, Assoc::Left),
};

/// Less than parser
const OP_LT: &'static BinaryOpParser = &BinaryOpParser {
    op: "<",
    pre: Precedence(Lprec::LessThan as u8, 0, Assoc::Left),
};

/// Less than or equal parser
const OP_LTE: &'static BinaryOpParser = &BinaryOpParser {
    op: "<=",
    pre: Precedence(Lprec::LessThan as u8, 0, Assoc::Left),
};

// struct DollarParser;
// struct DotParser;
// struct PipeParser;

#[derive(Debug)]
struct StrxMode;

impl ParslMode for StrxMode
{
    type Item = Vec<AstNode>;

    fn prefix(
        &self,
        tok: Token,
    ) -> Option<&'static PrefixParser<Item = Vec<AstNode>>>
    {
        match tok {
            Token::StrLit => Some(&ParseFirst(&ParseStrLit)),
            Token::DollarId => Some(&ParseFirst(&ParseId)),
            Token::DoubleQuoteR => None,
            _ => None,
        }
    }

    fn infix(
        &self,
        tok: Token,
    ) -> Option<&'static InfixParser<Item = Vec<AstNode>>>
    {
        match tok {
            Token::StrLit => Some(&ParseMore(&ParseStrLit, MIN_PRECEDENCE)),
            Token::DollarId => Some(&ParseMore(&ParseId, MIN_PRECEDENCE)),
            Token::DoubleQuoteR => None,
            _ => None,
        }
    }
}

#[derive(Debug)]
struct ParseStrLit;

impl PrefixParser for ParseStrLit
{
    type Item = AstNode;

    fn parse(&self, _p: &mut Parsl, tok: TokenSrc) -> AstResult
    {
        let lstr = Lstr::from(tok.src.to_string());
        Ok(AstNode::new(Ast::ConstVal(Val::Str(lstr)), Ast::loc(&tok)))
    }
}

/// XlistMode for lists of: "x," or "k: x,", trailing comma is allowed/optional
#[derive(Debug)]
struct XlistMode;

impl XlistMode
{
    pub fn parse(p: &mut Parsl) -> Lresult<ast2::Xlist>
    {
        let items = p.parse_new(&XlistMode)?;
        p.skip_if(Token::Comma)?;
        Ok(StrupleKV::from(items))
    }
}

impl ParslMode for XlistMode
{
    type Item = Vec<(Option<&'static str>, AstNode)>;

    fn prefix(
        &self,
        tok: Token,
    ) -> Option<&'static PrefixParser<Item = Self::Item>>
    {
        match tok {
            Token::Comma => None,
            _ => Some(&ParseFirst(&ParseXMaybeK)),
        }
    }

    fn infix(
        &self,
        tok: Token,
    ) -> Option<&'static InfixParser<Item = Self::Item>>
    {
        match tok {
            Token::Comma => Some(&ParseXMaybeK),
            _ => None,
        }
    }
}

/// Parses a single item in a list, tuple, call args
#[derive(Debug)]
struct ParseXMaybeK;

impl PrefixParser for ParseXMaybeK
{
    type Item = (Option<&'static str>, AstNode);

    fn parse(&self, p: &mut Parsl, tok: TokenSrc) -> Lresult<Self::Item>
    {
        let first = p.reparse(&ExprMode, MIN_PRECEDENCE, tok)?;
        if p.next_if(Token::Colon)?.is_some() {
            if let Ast::Id1(key) = *first.node {
                let v = p.parse_new(&ExprMode)?;
                Ok((Some(key), v))
            } else {
                Err(rustfail!(
                    "parse_failure",
                    "key must be a single id, found {:?}",
                    first,
                ))
            }
        } else {
            Ok((None, first))
        }
    }
}

impl InfixParser for ParseXMaybeK
{
    type Item = Vec<(Option<&'static str>, AstNode)>;

    fn parse(
        &self,
        p: &mut Parsl,
        mut left: Self::Item,
        tok: TokenSrc,
    ) -> Lresult<Self::Item>
    {
        assert_eq!(Token::Comma, tok.tok);
        p.skip_if(Token::LineBegin)?;
        {
            // special stop logic, stop if there isn't another expression
            // things like ] or ) or } won't start new expressions
            let peeked = p.peek_token()?;
            if ExprMode.prefix(peeked).is_none() {
                return Ok(left);
            }
        }

        let first = p.parse_new(&ExprMode)?;
        let item = if p.next_if(Token::Colon)?.is_some() {
            if let Ast::Id1(key) = *first.node {
                let v = p.parse_new(&ExprMode)?;
                (Some(key), v)
            } else {
                return Err(rustfail!(
                    "parse_failure",
                    "key must be a single id, found {:?}",
                    first,
                ));
            }
        } else {
            (None, first)
        };
        left.push(item);
        Ok(left)
    }

    fn precedence(&self) -> Precedence
    {
        Precedence::from(Lprec::Comma)
    }
}

#[derive(Debug)]
struct ParseCall;

impl InfixParser for ParseCall
{
    type Item = AstNode;

    fn parse(&self, p: &mut Parsl, left: AstNode, _tok: TokenSrc) -> AstResult
    {
        let args = if p.peek_token()? == Token::ParenR {
            StrupleKV::new()
        } else {
            XlistMode::parse(p)?
        };
        expect_next!(p, Token::ParenR)?;
        let loc = left.loc;
        Ok(AstNode::new(Ast::Call(left, args), loc))
    }

    fn precedence(&self) -> Precedence
    {
        Precedence(Lprec::Call as u8, 0, Assoc::Left)
    }
}

#[derive(Debug)]
struct ParseGeneric;

impl InfixParser for ParseGeneric
{
    type Item = AstNode;

    fn parse(&self, p: &mut Parsl, left: AstNode, tok: TokenSrc) -> AstResult
    {
        if p.peek_token()? == Token::SquareR {
            return Err(rustfail!(
                PARSE_FAIL,
                "cannot have empty generic types at {:?}",
                tok,
            ));
        }
        let args = IdTypeMode::parse(p)?;
        expect_next!(p, Token::SquareR)?;
        let loc = left.loc;
        Ok(AstNode::new(Ast::Generic(left, args), loc))
    }

    fn precedence(&self) -> Precedence
    {
        Precedence(Lprec::Generic as u8, 0, Assoc::Left)
    }
}

/// Parse the cases of an if or match expression
#[derive(Debug)]
struct ParseIf;

impl PrefixParser for ParseIf
{
    type Item = AstNode;

    fn parse(&self, p: &mut Parsl, tok: TokenSrc) -> Lresult<Self::Item>
    {
        let peeked = p.peek()?;
        let mut cases: Vec<ast2::Case>;
        if peeked.tok == Token::CasePipe {
            cases = p.parse_new(&CaseMode)?;
            p.skip_if(Token::LineBegin)?;
            expect_next!(p, Token::DoubleDash)?;
        } else {
            let if_x = p.parse_new(&ExprMode)?;
            let if_arrow = expect_next!(p, Token::DoubleArrow)?;
            p.skip_if(Token::LineBegin)?;
            let if_body = Grammar::parse_block(p, Ast::loc(&if_arrow))?;
            cases = vec![ast2::Case{ cond: if_x, body: if_body }];

            p.skip_if(Token::LineBegin)?;
            let next = p.next()?;
            match next.tok {
                Token::DoubleDash => {
                    // end of expr
                }
                Token::Else => {
                    // get the else block
                    let else_arrow = expect_next!(p, Token::DoubleArrow)?;
                    let else_body = Grammar::parse_block(p, Ast::loc(&else_arrow))?;
                    expect_next!(p, Token::DoubleDash)?;
                    cases.push(ast2::Case{ cond: AstNode::void(), body: else_body });
                }
                _ => {
                    return Err(rustfail!(
                        PARSE_FAIL,
                        "expected else or --, found {:?}",
                        next.src,
                    ));
                }
            }
        };
        Ok(AstNode::new(
            Ast::Case(ast2::CaseType::If, None, cases),
            Ast::loc(&tok),
        ))
    }
}

/// Parse the cases of an if or match expression
#[derive(Debug)]
struct ParseMatch;

impl PrefixParser for ParseMatch
{
    type Item = AstNode;

    fn parse(&self, p: &mut Parsl, tok: TokenSrc) -> Lresult<Self::Item>
    {
        let peeked = p.peek()?;
        let input = if peeked.tok == Token::CasePipe {
            None
        } else {
            Some(p.parse_new(&ExprMode)?)
        };
        let cases = p.parse_new(&CaseMode)?;
        p.skip_if(Token::LineBegin)?;
        expect_next!(p, Token::DoubleDash)?;
        Ok(AstNode::new(
            Ast::Case(ast2::CaseType::Match, input, cases),
            Ast::loc(&tok),
        ))
    }
}

/// CaseMode for cases in if and match expressions
#[derive(Debug)]
struct CaseMode;

impl ParslMode for CaseMode
{
    type Item = Vec<ast2::Case>;

    fn prefix(
        &self,
        tok: Token,
    ) -> Option<&'static PrefixParser<Item = Self::Item>>
    {
        match tok {
            Token::CasePipe => Some(&ParseFirst(&ParseCase)),
            _ => None,
        }
    }

    fn infix(
        &self,
        tok: Token,
    ) -> Option<&'static InfixParser<Item = Self::Item>>
    {
        match tok {
            Token::CasePipe => Some(&ParseMore(&ParseCase, MIN_PRECEDENCE)),
            _ => None,
        }
    }
}

/// Parse a single case for an if or match expression
#[derive(Debug)]
struct ParseCase;

impl PrefixParser for ParseCase
{
    type Item = ast2::Case;

    fn parse(&self, p: &mut Parsl, tok: TokenSrc) -> Lresult<Self::Item>
    {
        assert_eq!(Token::CasePipe, tok.tok);
        let condition = match p.next_if(Token::Else)? {
            Some(else_tok) => AstNode::new(Ast::Void, Ast::loc(&else_tok)),
            None => p.parse_new(&ExprMode)?,
        };
        let arrow = expect_next!(p, Token::DoubleArrow)?;
        let body = Grammar::parse_block(p, Ast::loc(&arrow))?;
        Ok(ast2::Case::new(condition, body))
    }
}


/// Grammar is a collection of functions for parsing a stream of tokens
pub struct Grammar
{
    p: Parsl,
}

impl Grammar
{
    pub fn new(items: Vec<TokenSrc>) -> Grammar
    {
        Grammar {
            p: Parsl::new(items),
        }
    }

    pub fn parse_module(&mut self) -> Lresult<Vec<AstNode>>
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
    fn parse_block(p: &mut Parsl, loc: Loc) -> AstResult
    {
        let node = if p.peek_token()? == Token::LineBegin {
            let stmts = p.parse_new(&StmtsMode)?;
            match stmts.len() {
                0 => AstNode::void(),
                _ => AstNode::new(Ast::Block(stmts), loc),
            }
        } else {
            p.parse_new(&ExprMode)?
        };
        Ok(node)
    }

    /// Parse an id
    fn parse_id(p: &mut Parsl) -> AstResult
    {
        let tok = expect_next!(p, Token::Id)?;
        PrefixParser::parse(&ParseId, p, tok)
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
        let input = r#"const X := 5
        """#;
        let toks = Tokenz::lexp(input).unwrap();
        let mut p = Grammar::new(toks);
        let ast = p.parse_module().unwrap();
        assert_eq!(2, ast.len());
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
    fn test_parse_deffunc_params()
    {
        let input = r#"func add x:Int y:Int :Int >>
            x + y
        --

        func add x:Int y:Int :Int
        >>
            x + y
        --

        func add
        x:Int
        y:Int
         :Int
        >>
            x + y
        --
        "#;
        let toks = Tokenz::lexp(input).unwrap();
        let mut p = Grammar::new(toks);
        let ast = p.parse_module().unwrap();
        assert_eq!(3, ast.len());
    }

    #[test]
    fn test_parse_deffunc_match()
    {
        let input = r#"func add x:Int y:Int :Int
        |(0, 0) >> 0
        |(x, y) >> x + y
        --

        func fact x:Int :Int
        |0 >> 1
        |x >> x * fact(f-1)
        --
        "#;
        let toks = Tokenz::lexp(input).unwrap();
        let mut p = Grammar::new(toks);
        p.parse_module().unwrap();
    }

    #[test]
    fn test_parse_defmacro()
    {
        let input = r#"macro test_and a b >>
            if
            |a >> b
            |else >> false
            --
        --
        "#;
        let toks = Tokenz::lexp(input).unwrap();
        let mut p = Grammar::new(toks);
        p.parse_module().unwrap();
    }

    #[test]
    fn test_parse_generic_call()
    {
        let input = r#"swap[:Int :Str](5, "tacos")"#;
        let toks = Tokenz::lexp(input).unwrap();
        let mut p = Grammar::new(toks);
        let _ast = p.parse_module().unwrap();
    }

    #[test]
    fn test_parse_generic_deffunc_types()
    {
        let input = r#"func swap[:A :B] a:A b:B :(:B :A)
        >>
            (b, a)
        --
        "#;
        let toks = Tokenz::lexp(input).unwrap();
        let mut p = Grammar::new(toks);
        let _ast = p.parse_module().unwrap();
    }

    #[test]
    fn test_parse_generic_enum_tuple()
    {
        let input = "
        type Opt[:T]
        |Some :T
        |None
        --
        ";
        let toks = Tokenz::lexp(input).unwrap();
        let ast = Grammar::new(toks).parse_module().unwrap();
        assert_eq!(1, ast.len());

        let t = &ast[0];
        assert_matches!(&*t.node, Ast::DefType(DataType::Union, _, _));
        if let Ast::DefType(_, gen, variants) = &*t.node {
            assert_matches!(&*gen.node, Ast::Generic(_, _));
            if let Ast::Generic(name, gen_args) = &*gen.node {
                assert_eq!(Ast::Id1("Opt"), *name.node);
                assert_eq!(Ast::Id1("T"), *gen_args[0].v.node);
                assert_eq!(1, gen_args.len());
            }

            assert_eq!("Some", variants[0].k.unwrap());
            assert_eq!("None", variants[1].k.unwrap());
            assert_eq!(2, variants.len());

            assert_matches!(
                *variants[0].v.node,
                Ast::DefType(DataType::Struct, _, _)
            );
            if let Ast::DefType(_, some, some_fields) = &*variants[0].v.node {
                assert_eq!(Ast::Id1("Some"), *some.node);
                assert_eq!(None, some_fields[0].k);
                assert_eq!(Ast::Id1("T"), *some_fields[0].v.node);
                assert_eq!(1, some_fields.len());
            }
            assert_eq!(Ast::Void, *variants[1].v.node);
            assert_eq!(2, variants.len());
        }
    }

    #[test]
    fn test_parse_generic_enum_fields()
    {
        let input = "
        type Foo[:T]
        |Bar a:T b:Int
        |Baz
        --
        ";
        let toks = Tokenz::lexp(input).unwrap();
        let ast = Grammar::new(toks).parse_module().unwrap();
        assert_eq!(1, ast.len());

        let t = &ast[0];
        assert_matches!(&*t.node, Ast::DefType(DataType::Union, _, _));
        if let Ast::DefType(_, gen, variants) = &*t.node {
            assert_matches!(&*gen.node, Ast::Generic(_, _));
            if let Ast::Generic(name, gen_args) = &*gen.node {
                assert_eq!(Ast::Id1("Foo"), *name.node);
                assert_eq!(Ast::Id1("T"), *gen_args[0].v.node);
                assert_eq!(1, gen_args.len());
            }
            assert_eq!("Bar", variants[0].k.unwrap());
            assert_eq!("Baz", variants[1].k.unwrap());
            assert_eq!(2, variants.len());

            assert_matches!(
                *variants[0].v.node,
                Ast::DefType(DataType::Struct, _, _)
            );
            if let Ast::DefType(_, bar, bar_fields) = &*variants[0].v.node {
                assert_eq!(Ast::Id1("Bar"), *bar.node);
                assert_eq!("a", bar_fields[0].k.unwrap());
                assert_eq!("b", bar_fields[1].k.unwrap());
                assert_eq!(Ast::Id1("T"), *bar_fields[0].v.node);
                assert_eq!(Ast::Id1("Int"), *bar_fields[1].v.node);
                assert_eq!(2, bar_fields.len());
            }

            assert_eq!(Ast::Void, *variants[1].v.node);
        }
    }

    #[test]
    fn test_parse_generic_struct_tuple()
    {
        let input = "
        type Foo[:T :U]
        :T
        :U
        :Int
        --
        ";
        let toks = Tokenz::lexp(input).unwrap();
        let ast = Grammar::new(toks).parse_module().unwrap();
        assert_eq!(1, ast.len());

        let t = &ast[0];
        assert_matches!(&*t.node, Ast::DefType(DataType::Struct, _, _));
        if let Ast::DefType(_, gen, fields) = &*t.node {
            assert_matches!(&*gen.node, Ast::Generic(_, _));
            if let Ast::Generic(name, gen_args) = &*gen.node {
                assert_eq!(Ast::Id1("Foo"), *name.node);
                assert_eq!(Ast::Id1("T"), *gen_args[0].v.node);
                assert_eq!(Ast::Id1("U"), *gen_args[1].v.node);
                assert_eq!(2, gen_args.len());
            }
            assert!(fields[0].k.is_none());
            assert!(fields[1].k.is_none());
            assert!(fields[2].k.is_none());
            assert_eq!(Ast::Id1("T"), *fields[0].v.node);
            assert_eq!(Ast::Id1("U"), *fields[1].v.node);
            assert_eq!(Ast::Id1("Int"), *fields[2].v.node);
            assert_eq!(3, fields.len());
        }
    }

    #[test]
    fn test_parse_generic_struct_fields()
    {
        let input = "
        type Foo[:T]
        apple:T
        banana:Int
        --
        ";
        let toks = Tokenz::lexp(input).unwrap();
        let ast = Grammar::new(toks).parse_module().unwrap();
        assert_eq!(1, ast.len());

        let t = &ast[0];
        assert_matches!(&*t.node, Ast::DefType(DataType::Struct, _, _));
        if let Ast::DefType(_, gen, fields) = &*t.node {
            assert_matches!(&*gen.node, Ast::Generic(_, _));
            if let Ast::Generic(name, gen_args) = &*gen.node {
                assert_eq!(Ast::Id1("Foo"), *name.node);
                assert_eq!(Ast::Id1("T"), *gen_args[0].v.node);
                assert_eq!(1, gen_args.len());
            }
            assert_eq!("apple", fields[0].k.unwrap());
            assert_eq!("banana", fields[1].k.unwrap());
            assert_eq!(Ast::Id1("T"), *fields[0].v.node);
            assert_eq!(Ast::Id1("Int"), *fields[1].v.node);
            assert_eq!(2, fields.len());
        }
    }

    #[test]
    fn test_parse_hashtag()
    {
        let input = "#hash_tag";
        let toks = Tokenz::lexp(input).unwrap();
        let mut p = Grammar::new(toks);
        let ast = p.parse_module().unwrap();

        assert_eq!(1, ast.len());
        assert_eq!(
            Ast::ConstVal(Val::Hashtag(Lstr::from("#hash_tag"))),
            *ast[0].node,
        );
    }

    #[test]
    fn test_parse_id2()
    {
        let input = "x::y";
        let toks = Tokenz::lexp(input).unwrap();
        let mut p = Grammar::new(toks);
        let ast = p.parse_module().unwrap();

        assert_eq!(Ast::Id2("x", "y"), *ast[0].node);
        assert_eq!(1, ast.len());
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
    fn test_parse_ifstmt()
    {
        let input = "
        if True >>
            1
        else >>
            3
        --
        if True >>
            2
        --
        ";
        let toks = Tokenz::lexp(input).unwrap();
        let ast = Grammar::new(toks).parse_module().unwrap();
        assert_eq!(2, ast.len());
    }

    #[test]
    fn test_parse_import()
    {
        let input = "import tacos";
        let toks = Tokenz::lexp(input).unwrap();
        let ast = Grammar::new(toks).parse_module().unwrap();
        assert_eq!(1, ast.len());
        assert_eq!(Ast::Import("tacos"), *ast[0].node);
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
            assert_matches!(*rhs.node, Ast::Block(_));
            if let Ast::Block(items) = &*rhs.node {
                assert_matches!(*items[0].node, Ast::Op2("*", _, _));
                if let Ast::Op2("*", b, three) = &*items[0].node {
                    assert_eq!(Ast::Id1("b"), *b.node);
                    assert_eq!(Ast::ConstVal(Val::Int(3)), *three.node);
                }
            }
        }
    }

    #[test]
    fn test_parse_lists()
    {
        let input = "[1, 2, 3, 4]
        []
        [5, 6, 7,]";
        let toks = Tokenz::lexp(input).unwrap();
        let ast = Grammar::new(toks).parse_module().unwrap();
        assert_eq!(3, ast.len());
    }

    #[test]
    fn test_parse_lt3()
    {
        let input = "x < y <= z
        x < y <= z
        a < b + 1 < c - 1
        ";
        let toks = Tokenz::lexp(input).unwrap();
        let ast = Grammar::new(toks).parse_module().unwrap();
        assert_eq!(3, ast.len());

        assert_matches!(*ast[0].node, Ast::Op2("<", _, _));
        if let Ast::Op2(_, x, yltez) = &*ast[0].node {
            assert_eq!(Ast::Id1("x"), *x.node);
            assert_matches!(*yltez.node, Ast::Op2("<=", _, _));
            if let Ast::Op2(_, y, z) = &*yltez.node {
                assert_eq!(Ast::Id1("y"), *y.node);
                assert_eq!(Ast::Id1("z"), *z.node);
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
    fn test_parse_newline_dash()
    {
        let input = "9
        - 5";
        let toks = Tokenz::lexp(input).unwrap();
        let ast = Grammar::new(toks).parse_module().unwrap();
        assert_eq!(Ast::ConstVal(Val::Int(9)), *ast[0].node);
        assert_matches!(*ast[1].node, Ast::Op1("-", _));
        assert_eq!(2, ast.len());
    }

    #[test]
    fn test_parse_operators()
    {
        let input = r#"
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
        a_module::a_func
        h;t
        "#;
        let toks = Tokenz::lexp(input).unwrap();
        let mut p = Grammar::new(toks);
        p.parse_module().unwrap();
    }

    #[test]
    fn test_parse_parens()
    {
        let input = r#"not (x == y)
        (3, 4, 5)
        (6, 7, 8,)
        "#;
        let toks = Tokenz::lexp(input).unwrap();
        let mut p = Grammar::new(toks);
        let ast = p.parse_module().unwrap();
        assert_eq!(3, ast.len());
    }

    #[test]
    fn test_parse_postfix_newline()
    {
        let input = "name \\n";
        let toks = Tokenz::lexp(input).unwrap();
        let mut p = Grammar::new(toks);
        let ast = p.parse_module().unwrap();
        assert_eq!(1, ast.len());
    }

    #[test]
    fn test_parse_precedence()
    {
        let input = r#"
        1 - 2 * 3
        x / y + z
        a * - b
        "#;
        let toks = Tokenz::lexp(input).unwrap();
        let mut p = Grammar::new(toks);
        let ast = p.parse_module().unwrap();
        assert_eq!(3, ast.len());

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

        {
            let times = &ast[2];
            assert_matches!(*times.node, Ast::Op2("*", _, _));
            if let Ast::Op2("*", a, negb) = &*times.node {
                assert_eq!(Ast::Id1("a"), *a.node);
                assert_matches!(*negb.node, Ast::Op1("-", _));
                if let Ast::Op1("-", b) = &*negb.node {
                    assert_eq!(Ast::Id1("b"), *b.node);
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
                assert_eq!(*items[0].node, Ast::Id1("burritos"));
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
                assert_eq!(*items[1].node, Ast::Id1("dogs"));
                assert_eq!(
                    Ast::ConstVal(Val::Str(Lstr::from(" mice"))),
                    *items[2].node,
                );
            }
        }
    }

    #[test]
    fn test_parse_type_expressions()
    {
        let input = "
        type TypeX
        :Y
        :[Int]
        :(:Int :Bool)
        --
        ";
        let toks = Tokenz::lexp(input).unwrap();
        let ast = Grammar::new(toks).parse_module().unwrap();
        assert_eq!(1, ast.len());

        let t = &ast[0];
        if let Ast::DefType(DataType::Struct, _name, fields) = &*t.node {
            assert_eq!(3, fields.len());
            assert_eq!(Ast::Id1("Y"), *fields[0].v.node);
            let int_list = &*fields[1].v.node;
            assert_matches!(int_list, Ast::List(_));
            if let Ast::List(inner) = int_list {
                assert_eq!(Ast::Id1("Int"), *inner[0].v.node);
            }
        }
    }

    #[test]
    fn test_parse_type_struct()
    {
        let input = "
        type Point2 x:Int y:Int --

        type Point3
        x:Int
        y:Int
        z:Int
        --
        ";
        let toks = Tokenz::lexp(input).unwrap();
        let ast = Grammar::new(toks).parse_module().unwrap();
        assert_eq!(2, ast.len());

        let t = &ast[0];
        assert_matches!(*t.node, Ast::DefType(DataType::Struct, _, _));
        if let Ast::DefType(_, name, fields) = &*t.node {
            assert_matches!(*name.node, Ast::Id1("Point2"));
            assert_eq!(2, fields.len());
            assert_eq!("x", fields[0].k.unwrap());
            assert_eq!("y", fields[1].k.unwrap());
            assert_eq!(Ast::Id1("Int"), *fields[0].v.node);
            assert_eq!(Ast::Id1("Int"), *fields[1].v.node);
        }
    }

    #[test]
    fn test_parse_type_token()
    {
        let input = "type What --";
        let toks = Tokenz::lexp(input).unwrap();
        let ast = Grammar::new(toks).parse_module().unwrap();
        assert_eq!(1, ast.len());

        let t = &ast[0];
        assert_matches!(*t.node, Ast::DefType(DataType::Struct, _, _));
        if let Ast::DefType(_, name, fields) = &*t.node {
            assert_matches!(*name.node, Ast::Id1("What"));
            assert_eq!(0, fields.len());
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
            assert_eq!(Ast::Void, *vars[0].v.node);
            assert_eq!(Ast::Void, *vars[1].v.node);
            assert_eq!(Ast::Void, *vars[2].v.node);
        }
    }
}
