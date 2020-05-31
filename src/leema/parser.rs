use crate::leema::ast2;
use crate::leema::ast2::{Ast, AstNode, AstResult, ModAction, ModTree, Xlist};
use crate::leema::failure::{Failure, Lresult};
use crate::leema::lstr::Lstr;
use crate::leema::struple::StrupleItem;
use crate::leema::val::Val;

use lazy_static::lazy_static;
use pest::iterators::Pair;
use pest::Parser;
use pratt::{Affix, Arity, Associativity, Op, PrattParser, Precedence};

use std::collections::HashMap;

#[derive(Parser)]
#[grammar = "leema/leema.pest"]
pub struct LeemaParser;


pub fn parse_tokens(r: Rule, text: &'static str) -> Lresult<Vec<Pair<Rule>>>
{
    let it = LeemaParser::parse(r, text).map_err(|e| {
        println!("parse error: {:?}", e);
        rustfail!("parse failure", "{:?}", e,)
    })?;
    Ok(it.collect())
}

pub fn parse(r: Rule, text: &'static str) -> Lresult<Vec<AstNode>>
{
    let it = LeemaParser::parse(r, text).map_err(|e| {
        println!("parse error: {:?}", e);
        rustfail!("parse failure", "{:?}", e,)
    })?;
    let mut pratt = LeemaPratt{};
    it.map(|i| pratt.primary(i)).collect()
}

pub fn nodeloc(pair: &Pair<Rule>) -> ast2::Loc
{
    let (line, col) = pair.as_span().start_pos().line_col();
    ast2::Loc::new(line as u16, col as u8)
}

pub struct LeemaPratt;

pub fn parse_mxline(pair: Pair<'static, Rule>) -> Lresult<ModTree>
{
    match pair.as_rule() {
        Rule::mxline => {
            let mut it = pair.into_inner();
            let head = it.next().unwrap();
            let opt_tail = it.next();
            match (head.as_rule(), opt_tail) {
                (Rule::mxmod, None) => {
                    Ok(ModTree::FinalMod(head.as_str(), nodeloc(&head)))
                }
                (Rule::mxid, None) => {
                    Ok(ModTree::FinalId(head.as_str(), nodeloc(&head)))
                }
                (Rule::mxmod, Some(next)) => {
                    let tail = match next.as_rule() {
                        Rule::id => ModTree::FinalId(next.as_str(), nodeloc(&next)),
                        _ => {
                            panic!("unexpected import line tail: {:?}", next);
                        }
                    };
                    Ok(ModTree::sub(head.as_str(), tail))
                }
                un => panic!("unexpected import line: {:?}", un),
            }
        }
        Rule::star => Ok(ModTree::All(nodeloc(&pair))),
        _ => {
            Err(rustfail!(
                "compile_failure",
                "expected import/export line, found {:?}",
                pair,
            ))
        }
    }
}

type OpVal = (&'static str, Rule, Affix, Arity);
type PrecKey = HashMap<Rule, Op>;

pub struct ParserBuilder
{
    prec: Vec<Vec<OpVal>>,
}

impl ParserBuilder
{
    pub fn new() -> Self
    {
        ParserBuilder{
            prec: vec![],
        }
    }

    pub fn left(self) -> PrecBuilder
    {
        PrecBuilder::open(self, Associativity::Left)
    }

    pub fn right(self) -> PrecBuilder
    {
        PrecBuilder::open(self, Associativity::Right)
    }
}

pub struct PrecBuilder
{
    parser: ParserBuilder,
    assoc: Associativity,
    prec: Vec<OpVal>,
}

impl PrecBuilder
{
    pub fn open(parser: ParserBuilder, assoc: Associativity) -> Self
    {
        PrecBuilder{
            parser,
            assoc,
            prec: vec![],
        }
    }

    pub fn left(self) -> PrecBuilder
    {
        self.close().left()
    }

    pub fn right(self) -> PrecBuilder
    {
        self.close().right()
    }

    pub fn infix(mut self, src: &'static str, r: Rule) -> Self
    {
        self.prec.push((src, r, Affix::Infix(self.assoc), Arity::Binary));
        self
    }

    pub fn prefix(mut self, src: &'static str, r: Rule) -> Self
    {
        if self.assoc == Associativity::Left {
            panic!("prefix rules cannot be left associative: {:?} {:?}", src, r);
        }
        self.prec.push((src, r, Affix::Prefix, Arity::Unary));
        self
    }

    pub fn postfix(mut self, src: &'static str, r: Rule) -> PrecBuilder
    {
        if self.assoc == Associativity::Right {
            panic!("postfix rules cannot be right associative: {:?} {:?}", src, r);
        }
        self.prec.push((src, r, Affix::Postfix, Arity::Unary));
        self
    }

    fn close(mut self) -> ParserBuilder
    {
        self.parser.prec.push(self.prec);
        self.parser
    }
}

impl Into<PrecKey> for PrecBuilder
{
    fn into(self) -> PrecKey
    {
        let mut key = HashMap::new();
        for (iprec, p) in self.close().prec.into_iter().rev().enumerate() {
            for opval in p.into_iter() {
                let prec = Precedence(iprec as u32);
                key.insert(opval.1, Op::new(opval.0, opval.2, opval.3, prec));
            }
        }
        key
    }
}

impl LeemaPratt
{
    fn primary(&mut self, n: Pair<'static, Rule>) -> AstResult
    {
        let loc = nodeloc(&n);
        match n.as_rule() {
            Rule::id => Ok(AstNode::new(Ast::Id1(n.as_str()), loc)),
            Rule::int => {
                let i = n.as_str().parse().unwrap();
                Ok(AstNode::new_constval(Val::Int(i), loc))
            }
            Rule::expr => self.parse(&mut n.into_inner()),
            Rule::strlit => {
                let s = Val::Str(Lstr::Sref(n.as_str()));
                Ok(AstNode::new_constval(s, loc))
            }
            Rule::str => {
                println!("str: %{:#?}", n);
                let strs: Lresult<Vec<AstNode>> =
                    n.into_inner().map(|i| self.primary(i)).collect();
                let mut s = strs?;
                let result = match s.len() {
                    0 => AstNode::new_constval(Val::Str(Lstr::Sref("")), loc),
                    1 => s.remove(0),
                    _ => AstNode::new(Ast::StrExpr(s), loc),
                };
                Ok(result)
            }
            Rule::and
            | Rule::or
            | Rule::not
            | Rule::less_than
            | Rule::equality
            | Rule::greater_than => {
                Ok(AstNode::new(Ast::Id1(n.as_str()), loc))
            }
            Rule::stmt_block | Rule::file => {
                let inner: Lresult<Vec<AstNode>> =
                    n.into_inner().map(|i| self.primary(i)).collect();
                Ok(AstNode::new(Ast::Block(inner?), loc))
            }
            Rule::def_func => {
                let mut inner = n.into_inner();
                let _func_mode = inner.next().unwrap();
                let func_name = self.primary(inner.next().unwrap())?;
                let func_result = self.primary(inner.next().unwrap())?;
                let func_arg_it = inner.next().unwrap().into_inner();
                let func_args: Xlist = self.parse_xlist(func_arg_it)?;
                let block = self.primary(inner.next().unwrap())?;
                let df = Ast::DefFunc(func_name, func_args, func_result, block);
                Ok(AstNode::new(df, loc))
            }
            Rule::def_func_result => {
                match n.into_inner().next() {
                    Some(result) => self.primary(result),
                    None => Ok(AstNode::new(Ast::Id1("Void"), loc)),
                }
            }
            Rule::mxstmt => {
                let mut inner = n.into_inner();
                let mxpair = inner.next().unwrap();
                let mx = match mxpair.as_str() {
                    "import" => ModAction::Import,
                    "export" => ModAction::Export,
                    _ => panic!("expected import or export, found {:?}", mxpair),
                };
                let mxline = parse_mxline(inner.next().unwrap())?;
                Ok(AstNode::new(Ast::ModAction(mx, mxline), loc))
            }
            Rule::EOI => Ok(AstNode::void()),
            Rule::rust_block => Ok(AstNode::new(Ast::RustBlock, loc)),
            Rule::x1|Rule::prefix1|Rule::postfix1 => {
                panic!("cannot parse silent rule: {:?}", n);
            }
            _ => {
                println!("unsupported rule: {:?}", n);
                self.parse(&mut n.into_inner())
            }
        }
    }

    pub fn parse_xlist<Inputs>(&mut self, it: Inputs) -> Lresult<Xlist>
        where Inputs: Iterator<Item = Pair<'static, Rule>>,
    {
        it
            .map(|p| {
                self.parse_x_maybe_k(p)
            })
            .collect()
    }

    pub fn parse_x_maybe_k(
        &mut self,
        pair: Pair<'static, Rule>,
    ) -> Lresult<StrupleItem<Option<&'static str>, AstNode>>
    {
        match pair.as_rule() {
            Rule::x_maybe_k => {
                let mut inner = pair.into_inner();
                let k_or_x: Pair<'static, Rule> = inner.next().unwrap();
                let maybe_x = inner.next();
                match (k_or_x, maybe_x) {
                    (xpair, None) => {
                        let x = self.primary(xpair)?;
                        Ok(StrupleItem::new_v(x))
                    }
                    (kpair, Some(xpair)) => {
                        let x = self.primary(xpair)?;
                        Ok(StrupleItem::new(Some(kpair.as_str()), x))
                    }
                }
            }
            Rule::def_func_arg => {
                let mut inner = pair.into_inner();
                let name = inner.next().unwrap();
                let typ = self.primary(inner.next().unwrap())?;
                Ok(StrupleItem::new(Some(name.as_str()), typ))
            }
            unexpected => {
                Err(rustfail!(
                    "compile_error",
                    "expected x_maybe_k, found {:?}",
                    unexpected,
                ))
            }
        }
    }
}

impl<Inputs> PrattParser<Inputs> for LeemaPratt
    where Inputs: Iterator<Item = Pair<'static, Rule>>,
{
    type Input = Pair<'static, Rule>;
    type Output = AstNode;
    type Error = Failure;

    fn query(&mut self, p: &Self::Input) -> Lresult<Op>
    {
        lazy_static! {
            static ref KEY: PrecKey = ParserBuilder::new()
                .left()
                    .infix(".", Rule::dot)
                    .infix("'", Rule::tick)
                    .postfix("()", Rule::call_args)
                .left()
                    .prefix("-", Rule::negative)
                .left()
                    .infix("*", Rule::star)
                    .infix("/", Rule::slash)
                    .infix("modulo", Rule::modulo)
                .left()
                    .infix("+", Rule::plus)
                    .infix("-", Rule::dash)
                .left()
                    .infix("<", Rule::less_than)
                    .infix("==", Rule::equality)
                    .infix(">", Rule::greater_than)
                .left()
                    .infix("and", Rule::and)
                .left()
                    .infix("or", Rule::or)
                .into();
        }
        KEY.get(&p.as_rule())
            .map(|op| *op)
            .ok_or_else(|| {
                rustfail!(
                    "compile_error",
                    "unsupported operator: {:?}",
                    p,
                )
            })
    }

    fn nullary(&mut self, n: Self::Input) -> AstResult
    {
        self.primary(n)
    }

    fn unary(&mut self, op: Self::Input, x: AstNode) -> AstResult
    {
        println!("unary {:?} {:?}", op, x);
        let loc = nodeloc(&op);
        match op.as_rule() {
            Rule::call_args => {
                let call_args = self.parse_xlist(op.into_inner())?;
                let call_loc = x.loc;
                Ok(AstNode::new(Ast::Call(x, call_args), call_loc))
            }
            Rule::negative
            | Rule::not
            | Rule::add_newline => {
                println!("unary {} {:?}", op.as_str(), x);
                let ast = Ast::Op1(op.as_str(), x);
                Ok(AstNode::new(ast, loc))
            }
            _ => {
                panic!("unknown unary operator: {:?}", op);
            }
        }
    }

    fn binary(&mut self, op: Self::Input, a: AstNode, b: AstNode) -> AstResult
    {
        match op.as_rule() {
            Rule::and
            | Rule::or
            | Rule::plus
            | Rule::dash
            | Rule::star
            | Rule::slash
            | Rule::modulo
            | Rule::less_than
            | Rule::equality
            | Rule::greater_than => {
                println!("{} {:?} {:?}", op.as_str(), a, b);
                let ast = Ast::Op2(op.as_str(), a, b);
                Ok(AstNode::new(ast, nodeloc(&op)))
            }
            _ => {
                panic!("unknown operator: {:?}", op);
            }
        }
    }
}

#[cfg(test)]
mod tests
{
    use super::{parse, LeemaParser, Rule};
    use crate::leema::ast2::{Ast, ModAction, ModTree};
    use crate::leema::lstr::Lstr;
    use crate::leema::val::Val;

    use matches::assert_matches;
    use pest::{consumes_to, parses_to};

    #[test]
    fn bool_true()
    {
        parses_to!(
            parser: LeemaParser,
            input: "True",
            rule: Rule::x1,
            tokens: [
                id(0, 4)
            ]
        )
    }

    #[test]
    fn call_expr()
    {
        parses_to!(
            parser: LeemaParser,
            input: "foo(5, x: y)",
            rule: Rule::expr,
            tokens: [expr(0, 12, [
                id(0, 3),
                call_args(3, 12, [
                    x_maybe_k(4, 5, [expr(4, 5, [int(4, 5)])]),
                    x_maybe_k(7, 11, [
                        id(7, 8),
                        expr(10, 11, [id(10, 11)]),
                    ]),
                ])
            ])]
        )
    }

    #[test]
    fn def_func_arrowblock()
    {
        let input = "func foo ->
            bar(x, y)
            baz()
        --
        ";
        let actual = parse(Rule::file, input).unwrap();
        println!("{:#?}", actual);
        parses_to!(
            parser: LeemaParser,
            input: input,
            rule: Rule::file,
            tokens: [file(0, 71, [
                def_func(0, 62, [
                    func_mode(0, 4),
                    id(5, 8),
                    def_func_result(9, 9),
                    def_func_args(9, 9),
                    stmt_block(11, 52, [
                        expr(24, 33, [
                            id(24, 27),
                            call_args(27, 33, [
                                x_maybe_k(28, 29, [expr(28, 29, [id(28, 29)])]),
                                x_maybe_k(31, 32, [expr(31, 32, [id(31, 32)])]),
                            ])
                        ]),
                        expr(46, 51, [
                            id(46, 49),
                            call_args(49, 51),
                        ]),
                    ]),
                ]),
                EOI(71, 71)
            ])]
        )
    }

    #[test]
    fn def_func_rustblock()
    {
        let input = "func foo -RUST-";
        let actual = parse(Rule::file, input).unwrap();
        println!("{:#?}", actual);
        parses_to!(
            parser: LeemaParser,
            input: input,
            rule: Rule::file,
            tokens: [file(0, 15, [
                def_func(0, 15, [
                    func_mode(0, 4),
                    id(5, 8),
                    def_func_result(9, 9),
                    def_func_args(9, 9),
                    rust_block(9, 15),
                ]),
                EOI(15, 15)
            ])]
        )
    }

    #[test]
    fn def_func_args()
    {
        let input = ":: s:Str x:Int";
        parses_to!(
            parser: LeemaParser,
            input: input,
            rule: Rule::def_func_args,
            tokens: [def_func_args(0, 14, [
                def_func_arg(3, 8, [
                    id(3, 4),
                    id(5, 8),
                ]),
                def_func_arg(9, 14, [
                    id(9, 10),
                    id(11, 14),
                ])
            ])]
        )
    }

    #[test]
    fn def_func_result()
    {
        let input = ":Str";
        parses_to!(
            parser: LeemaParser,
            input: input,
            rule: Rule::def_func_result,
            tokens: [def_func_result(0, 4, [
                id(1, 4),
            ])]
        )
    }

    #[test]
    fn def_func_result_with_args()
    {
        let input = "func format:Str :: x:Int ->
            do_it
        --
        ";
        let actual = parse(Rule::def_func, input).unwrap();
        println!("{:#?}", actual);
        if let Ast::DefFunc(name, args, _result, _body) = &*actual[0].node {
            assert_eq!(Ast::Id1("format"), *name.node);
            assert_eq!(*"x", *args[0].k.unwrap());
            assert_eq!(Ast::Id1("Int"), *args[0].v.node);
            // assert_eq!(Ast::Id1("Str"), *_result.node);
        } else {
            panic!("expected DefFunc, found {:?}", actual[0]);
        }
        assert_eq!(1, actual.len());
    }

    #[test]
    fn test_id()
    {
        parses_to!(
            parser: LeemaParser,
            input: "foo",
            rule: Rule::expr,
            tokens: [expr(0, 3, [id(0, 3)])]
        );
        parses_to!(
            parser: LeemaParser,
            input: "foo_bar",
            rule: Rule::expr,
            tokens: [expr(0, 7, [id(0, 7)])]
        );
        // id breaks before the dot
        parses_to!(
            parser: LeemaParser,
            input: "foo.bar",
            rule: Rule::id,
            tokens: [id(0, 3)]
        );
    }

    #[test]
    fn generic_id()
    {
        parses_to!(
            parser: LeemaParser,
            input: "foo'A",
            rule: Rule::expr,
            tokens: [
                expr(0, 5, [
                    id(0, 3),
                    tick(3, 4),
                    id(4, 5),
                ])
            ]
        )
    }

    #[test]
    fn mxid()
    {
        let input = ".taco";
        parses_to!(
            parser: LeemaParser,
            input: input,
            rule: Rule::mxid,
            tokens: [id(1, 5)]
        )
    }

    #[test]
    fn mxmod_absolute()
    {
        let input = "/root/path";
        parses_to!(
            parser: LeemaParser,
            input: input,
            rule: Rule::mxmod,
            tokens: [mxmod(0, 10)]
        )
    }

    #[test]
    fn mxmod_relative()
    {
        let input = "child/path";
        parses_to!(
            parser: LeemaParser,
            input: input,
            rule: Rule::mxmod,
            tokens: [mxmod(0, 10)]
        )
    }

    #[test]
    fn mxmod_sibling()
    {
        let input = "../sibling/path";
        parses_to!(
            parser: LeemaParser,
            input: input,
            rule: Rule::mxmod,
            tokens: [mxmod(0, 15)]
        )
    }

    #[test]
    fn mxmod_id()
    {
        let input = "root/path.taco";
        parses_to!(
            parser: LeemaParser,
            input: input,
            rule: Rule::mxline,
            tokens: [mxline(0, 14, [
                mxmod(0, 9),
                id(10, 14),
            ])]
        )
    }

    #[test]
    fn mxlines()
    {
        let input = "
        import /root/path
        import ../sibling/path
        import child/path
        import child.funky
        ";
        let actual = parse(Rule::file, input).unwrap();
        println!("{:#?}", actual);
        assert_eq!(1, actual.len());
        if let Ast::Block(imps) = &*actual[0].node {
            // /root/path
            if let Ast::ModAction(ModAction::Import, root) = &*imps[0].node {
                assert_matches!(root, ModTree::FinalMod("/root/path", _));
            } else {
                panic!("expected import, found {:?}", imps[0]);
            }
            // ../sibling/path
            if let Ast::ModAction(ModAction::Import, sib) = &*imps[1].node {
                assert_matches!(sib, ModTree::FinalMod("../sibling/path", _));
            } else {
                panic!("expected import, found {:?}", imps[0]);
            }
            // child/path
            if let Ast::ModAction(ModAction::Import, ch) = &*imps[2].node {
                assert_matches!(ch, ModTree::FinalMod("child/path", _));
            } else {
                panic!("expected import, found {:?}", imps[0]);
            }
            // child.funky
            if let Ast::ModAction(ModAction::Import, ch) = &*imps[3].node {
                if let ModTree::Sub("child", funky) = ch {
                    assert_matches!(**funky, ModTree::FinalId("funky", _));
                } else {
                    panic!("expected FinalId, found {:?}", ch);
                }
            } else {
                panic!("expected import, found {:?}", imps[0]);
            }
        } else {
            panic!("expected block, found: {:?}", actual[0]);
        }
    }

    #[test]
    fn infix_equality()
    {
        let input = "3 == x";
        let actual = parse(Rule::expr, input).unwrap();
        println!("{:#?}", actual);
        parses_to!(
            parser: LeemaParser,
            input: input,
            rule: Rule::expr,
            tokens: [
                expr(0, 6, [
                    int(0, 1),
                    equality(2, 4),
                    id(5, 6)
                ])
            ]
        )
    }

    #[test]
    fn infix_and_or()
    {
        let input = "a and b or c";
        let actual = parse(Rule::expr, input).unwrap();
        println!("{:#?}", actual);

        parses_to!(
            parser: LeemaParser,
            input: input,
            rule: Rule::expr,
            tokens: [
                expr(0, 12, [
                    id(0, 1),
                    and(2, 5),
                    id(6, 7),
                    or(8, 10),
                    id(11, 12)
                ])
            ]
        );

        /*
        // assertion syntax like this would be nice
        ast_match!(actual, Ast::Op2("or", a_and_b, c), [
            ast_match!(a_and_b, Ast::Op2("and", a, b), [
                ast_eq!(b, Ast::Id1("a")),
                ast_eq!(c, Ast::Id1("b")),
            ])
            ast_eq!(c, Ast::Id1("c")),
        ]);
        */

        assert_eq!(1, actual.len());
        let t = &actual[0];
        if let Ast::Op2("or", a_and_b, c) = &*t.node {
            if let Ast::Op2("and", a, b) = &*a_and_b.node {
                assert_eq!(Ast::Id1("a"), *a.node);
                assert_eq!(Ast::Id1("b"), *b.node);
            } else {
                panic!("expected and operation, found {:?}", a_and_b);
            }
            assert_eq!(Ast::Id1("c"), *c.node);
        } else {
            panic!("expected or operation, found {:?}", t);
        }
    }

    #[test]
    fn infix_or_and()
    {
        let input = "a or b and c";
        let actual = parse(Rule::expr, input).unwrap();
        println!("{:#?}", actual);

        parses_to!(
            parser: LeemaParser,
            input: input,
            rule: Rule::expr,
            tokens: [
                expr(0, 12, [
                    id(0, 1),
                    or(2, 4),
                    id(5, 6),
                    and(7, 10),
                    id(11, 12)
                ])
            ]
        );

        assert_eq!(1, actual.len());
        let t = &actual[0];
        if let Ast::Op2("or", a, b_and_c) = &*t.node {
            assert_eq!(Ast::Id1("a"), *a.node);
            if let Ast::Op2("and", b, c) = &*b_and_c.node {
                assert_eq!(Ast::Id1("b"), *b.node);
                assert_eq!(Ast::Id1("c"), *c.node);
            } else {
                panic!("expected and operation, found {:?}", b_and_c);
            }
        } else {
            panic!("expected or operation, found {:?}", t);
        }
    }

    #[test]
    fn infix_not_and()
    {
        let input = "not a and b";
        let actual = parse(Rule::expr, input).unwrap();
        println!("{:#?}", actual);

        parses_to!(
            parser: LeemaParser,
            input: input,
            rule: Rule::expr,
            tokens: [
                expr(0, 11, [
                    not(0, 3),
                    id(4, 5),
                    and(6, 9),
                    id(10, 11)
                ])
            ]
        );

        let t = &actual[0];
        if let Ast::Op2("and", not_a, b) = &*t.node {
            if let Ast::Op1("not", a) = &*not_a.node {
                assert_eq!(Ast::Id1("a"), *a.node);
            } else {
                panic!("expected not op1, found {:?}", not_a);
            }
            assert_eq!(Ast::Id1("b"), *b.node);
        } else {
            panic!("expected and op2, found {:?}", t);
        }
        assert_eq!(1, actual.len());
    }

    #[test]
    fn infix_and_not()
    {
        let input = "a and not b";
        let actual = parse(Rule::expr, input).unwrap();
        println!("{:#?}", actual);

        parses_to!(
            parser: LeemaParser,
            input: input,
            rule: Rule::expr,
            tokens: [
                expr(0, 11, [
                    id(0, 1),
                    and(2, 5),
                    not(6, 9),
                    id(10, 11),
                ])
            ]
        );

        let t = &actual[0];
        if let Ast::Op2("and", a, not_b) = &*t.node {
            assert_eq!(Ast::Id1("a"), *a.node);
            if let Ast::Op1("not", b) = &*not_b.node {
                assert_eq!(Ast::Id1("b"), *b.node);
            } else {
                panic!("expected not op1, found {:?}", not_b);
            }
        } else {
            panic!("expected and op2, found {:?}", t);
        }
        assert_eq!(1, actual.len());
    }

    #[test]
    fn infix_less_than()
    {
        let input = "a < b";
        let actual = parse(Rule::expr, input).unwrap();
        println!("{:#?}", actual);

        parses_to!(
            parser: LeemaParser,
            input: input,
            rule: Rule::expr,
            tokens: [
                expr(0, 5, [
                    id(0, 1),
                    less_than(2, 3),
                    id(4, 5),
                ])
            ]
        );

        let t = &actual[0];
        if let Ast::Op2("<", a, b) = &*t.node {
            assert_eq!(Ast::Id1("a"), *a.node);
            assert_eq!(Ast::Id1("b"), *b.node);
        } else {
            panic!("expected < op2, found {:?}", t);
        }
        assert_eq!(1, actual.len());
    }

    /*
    #[test]
    fn less_than_3()
    {
        let input = "3 < x <= 10";
        let actual = parse(Rule::expr, input).unwrap();
        println!("{:?}", actual);
        parses_to!(
            parser: LeemaParser,
            input: input,
            rule: Rule::x1,
            tokens: [
                id(0, 4)
            ]
        )
    }
    */

    #[test]
    fn number_float()
    {
        parses_to!(
            parser: LeemaParser,
            input: "3.14159",
            rule: Rule::expr,
            tokens: [expr(0, 7, [float(0, 7)])]
        )
    }

    #[test]
    fn number_int()
    {
        parses_to!(
            parser: LeemaParser,
            input: "1234",
            rule: Rule::expr,
            tokens: [expr(0, 4, [int(0, 4)])]
        )
    }

    #[test]
    fn negative_int()
    {
        parses_to!(
            parser: LeemaParser,
            input: "-34",
            rule: Rule::expr,
            tokens: [expr(0, 3, [dash(0, 1), int(1, 3)])]
        )
    }

    #[test]
    fn negative_int_plus()
    {
        // does the negative 34 get handled correctly?
        parses_to!(
            parser: LeemaParser,
            input: "-3 + 5",
            rule: Rule::expr,
            tokens: [
                expr(0, 6, [
                    dash(0, 1),
                    int(1, 2),
                    plus(3, 4),
                    int(5, 6)
                ])
            ]
        )
    }

    #[test]
    fn int_minus_negative()
    {
        // does the negative int get subtracted from
        parses_to!(
            parser: LeemaParser,
            input: "3 - -56",
            rule: Rule::expr,
            tokens: [
                expr(0, 7, [
                    int(0, 1),
                    dash(2, 3),
                    dash(4, 5),
                    int(5, 7),
                ])
            ]
        )
    }

    #[test]
    fn str_empty()
    {
        let ast = parse(Rule::expr, r#""""#).unwrap();
        assert_eq!(Ast::ConstVal(Val::Str(Lstr::Sref(""))), *ast[0].node);
    }

    #[test]
    fn str_const()
    {
        let ast = parse(Rule::expr, r#""taco""#).unwrap();
        assert_eq!(Ast::ConstVal(Val::Str(Lstr::Sref("taco"))), *ast[0].node);
    }

    #[test]
    fn str_expr()
    {
        let actual = parse(Rule::expr, r#""hello $world""#).unwrap();
        if let Ast::StrExpr(strx) = &*actual[0].node {
            assert_eq!(
                Ast::ConstVal(Val::Str(Lstr::Sref("hello "))),
                *strx[0].node,
            );
            assert_eq!(Ast::Id1("world"), *strx[1].node);
            assert_eq!(2, strx.len());
        } else {
            panic!("expected StrExpr, found {:?}", actual[0]);
        }
        assert_eq!(1, actual.len());

        parses_to!(
            parser: LeemaParser,
            input: r#""hello $world""#,
            rule: Rule::expr,
            tokens: [
                expr(0, 14, [
                    str(0, 14, [
                        strlit(1, 7),
                        id(8, 13),
                    ]),
                ])
            ]
        );
    }
}
