use crate::leema::ast2;
use crate::leema::ast2::{Ast, AstNode, AstResult, ModAction, ModTree, Xlist};
use crate::leema::failure::Lresult;
use crate::leema::lstr::Lstr;
use crate::leema::struple::StrupleItem;
use crate::leema::val::Val;

use pest::iterators::Pair;
use pest::prec_climber::{Assoc, Operator, PrecClimber};
use pest::Parser;

use lazy_static::lazy_static;

#[derive(Parser)]
#[grammar = "leema/leema.pest"]
pub struct LeemaParser;


pub fn loc(pair: &Pair<Rule>) -> ast2::Loc
{
    let (line, col) = pair.as_span().start_pos().line_col();
    ast2::Loc::new(line as u16, col as u8)
}

pub fn infix(
    lhsr: AstResult,
    op: Pair<'static, Rule>,
    rhsr: AstResult,
) -> AstResult
{
    let lhs = ltry!(lhsr);
    let rhs = ltry!(rhsr);
    match op.as_rule() {
        Rule::and
        | Rule::or
        | Rule::less_than
        | Rule::equality
        | Rule::greater_than => {
            let ast = Ast::Op2(op.as_str(), lhs, rhs);
            Ok(AstNode::new(ast, loc(&op)))
        }
        _ => {
            panic!("unknown operator: {:?}", op);
        }
    }
}

pub fn consume_x_maybe_k(pair: Pair<'static, Rule>, climber: &PrecClimber<Rule>) -> Lresult<StrupleItem<Option<&'static str>, AstNode>>
{
    match pair.as_rule() {
        Rule::x_maybe_k => {
            let mut inner = pair.into_inner();
            let k_or_x = inner.next().unwrap();
            let maybe_x = inner.next();
            match (k_or_x, maybe_x) {
                (xpair, None) => {
                    let x = consume(xpair, climber)?;
                    Ok(StrupleItem::new_v(x))
                }
                (kpair, Some(xpair)) => {
                    let x = consume(xpair, climber)?;
                    Ok(StrupleItem::new(Some(kpair.as_str()), x))
                }
            }
        }
        Rule::def_func_arg => {
            let mut inner = pair.into_inner();
            let name = inner.next().unwrap();
            let typ = consume(inner.next().unwrap(), climber)?;
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

pub fn consume_mxline(pair: Pair<'static, Rule>) -> Lresult<ModTree>
{
    match pair.as_rule() {
        Rule::mxline => {
            let mut it = pair.into_inner();
            let head = it.next().unwrap();
            let opt_tail = it.next();
            match (head.as_rule(), opt_tail) {
                (Rule::mxmod, None) => {
                    Ok(ModTree::FinalMod(head.as_str(), loc(&head)))
                }
                (Rule::mxid, None) => {
                    Ok(ModTree::FinalId(head.as_str(), loc(&head)))
                }
                (Rule::mxmod, Some(next)) => {
                    let tail = match next.as_rule() {
                        Rule::mxid => {
                            ModTree::FinalId(next.as_str(), loc(&next))
                        }
                        _ => {
                            panic!("unexpected import line tail: {:?}", next);
                        }
                    };
                    Ok(ModTree::sub(head.as_str(), tail))
                }
                un => {
                    panic!("unexpected import line: {:?}", un)
                }
            }
        }
        Rule::star => {
            Ok(ModTree::All(loc(&pair)))
        }
        _ => {
            Err(rustfail!(
                "compile_failure",
                "expected import/export line, found {:?}",
                pair,
            ))
        }
    }
}

pub fn consume(
    pair: Pair<'static, Rule>,
    climber: &PrecClimber<Rule>,
) -> AstResult
{
    let primary = |p| consume(p, climber);

    let pair_loc = loc(&pair);
    match pair.as_rule() {
        Rule::x1 => {
            let inner = pair.into_inner();
            climber.climb(inner, primary, infix)
        }
        Rule::expr => {
            let inner = pair.into_inner();
            climber.climb(inner, primary, infix)
        }
        Rule::call_expr => {
            let mut inner = pair.into_inner();
            let fx = consume(inner.next().unwrap(), climber)?;
            let args: Lresult<Xlist> = inner.map(|i| {
                consume_x_maybe_k(i, climber)
            }).collect();
            Ok(AstNode::new(Ast::Call(fx, args?), pair_loc))
        }
        Rule::prefix_expr => {
            let mut inner = pair.into_inner();
            let op = inner.next().unwrap();
            let arg = consume(inner.next().unwrap(), climber)?;
            Ok(AstNode::new(Ast::Op1(op.as_str(), arg), loc(&op)))
        }
        Rule::id => Ok(AstNode::new(Ast::Id1(pair.as_str()), pair_loc)),
        Rule::int => {
            let i = pair.as_str().parse().unwrap();
            Ok(AstNode::new_constval(Val::Int(i), pair_loc))
        }
        Rule::strlit => {
            let s = Val::Str(Lstr::Sref(pair.as_str()));
            Ok(AstNode::new_constval(s, pair_loc))
        }
        Rule::str => {
            let strs: Lresult<Vec<AstNode>> = pair.into_inner().map(|i| {
                consume(i, climber)
            }).collect();
            let mut s = strs?;
            let result = match s.len() {
                0 => AstNode::new_constval(Val::Str(Lstr::Sref("")), pair_loc),
                1 => s.remove(0),
                _ => AstNode::new(Ast::StrExpr(s), pair_loc),
            };
            Ok(result)
        }
        Rule::and
        | Rule::or
        | Rule::not
        | Rule::less_than
        | Rule::equality
        | Rule::greater_than => {
            Ok(AstNode::new(Ast::Id1(pair.as_str()), pair_loc))
        }
        Rule::stmt_block | Rule::file => {
            let inner: Lresult<Vec<AstNode>> =
                pair.into_inner().map(|i| consume(i, climber)).collect();
            Ok(AstNode::new(Ast::Block(inner?), pair_loc))
        }
        Rule::def_func => {
            let mut inner = pair.into_inner();
            let _func_mode = inner.next().unwrap();
            let func_name = consume(inner.next().unwrap(), climber)?;
            let func_result = consume(inner.next().unwrap(), climber)?;
            let func_arg_it = inner.next().unwrap().into_inner();
            let func_args: Lresult<Xlist> = func_arg_it.map(|arg| {
                consume_x_maybe_k(arg, climber)
            }).collect();
            let block = consume(inner.next().unwrap(), climber)?;
            let df = Ast::DefFunc(func_name, func_args?, func_result, block);
            Ok(AstNode::new(df, pair_loc))
        }
        Rule::def_func_result => {
            match pair.into_inner().next() {
                Some(result) => {
                    consume(result, climber)
                }
                None => {
                    Ok(AstNode::new(Ast::Id1("Void"), pair_loc))
                }
            }
        }
        Rule::mxstmt => {
            let mut inner = pair.into_inner();
            let mxpair = inner.next().unwrap();
            let mx = match mxpair.as_str() {
                "import" => ModAction::Import,
                "export" => ModAction::Export,
                _ => panic!("expected import or export, found {:?}", mxpair),
            };
            let mxline = consume_mxline(inner.next().unwrap())?;
            Ok(AstNode::new(Ast::ModAction(mx, mxline), pair_loc))
        }
        Rule::EOI => Ok(AstNode::void()),
        Rule::rust_block => Ok(AstNode::new(Ast::RustBlock, pair_loc)),
        _ => {
            println!("unsupported rule: {:?}", pair);
            let inner = pair.into_inner();
            if inner.as_str().is_empty() {
                // needs to be a better terminal case than this
                Ok(AstNode::void())
            } else {
                climber.climb(inner, primary, infix)
            }
        }
    }
}

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
    lazy_static! {
        static ref CLIMBER: PrecClimber<Rule> = PrecClimber::new(vec![
            Operator::new(Rule::or, Assoc::Left),
            Operator::new(Rule::and, Assoc::Left),
            Operator::new(Rule::less_than, Assoc::Left)
                | Operator::new(Rule::equality, Assoc::Left)
                | Operator::new(Rule::greater_than, Assoc::Left),
        ]);
    }
    let it = LeemaParser::parse(r, text).map_err(|e| {
        println!("parse error: {:?}", e);
        rustfail!("parse failure", "{:?}", e,)
    })?;
    it.map(|p| consume(p, &CLIMBER)).collect()
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
                call_expr(0, 12, [
                    id(0, 3),
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
                            call_expr(24, 33, [
                                id(24, 27),
                                x_maybe_k(28, 29, [expr(28, 29, [id(28, 29)])]),
                                x_maybe_k(31, 32, [expr(31, 32, [id(31, 32)])]),
                            ])
                        ]),
                        expr(46, 51, [
                            call_expr(46, 51, [
                                id(46, 49),
                            ])
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
                    typecall_expr(0, 5, [
                        id(0, 3),
                        id(4, 5),
                    ])
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
            tokens: [mxid(0, 5)]
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
        let input = "root/path.func";
        // let input = "root/path";
        parses_to!(
            parser: LeemaParser,
            input: input,
            rule: Rule::mxline,
            tokens: [mxline(0, 14, [
                mxmod(0, 9),
                mxid(9, 14),
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
        import child.func
        ";
        let actual = parse(Rule::file, input).unwrap();
        println!("{:#?}", actual);
        assert_eq!(1, actual.len());
        if let Ast::Block(imps) = &*actual[0].node {
            if let Ast::ModAction(ModAction::Import, root) = &*imps[0].node {
                assert_matches!(root, ModTree::FinalMod("/root/path", _));
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
                    prefix_expr(0, 5, [
                        not(0, 3),
                        id(4, 5),
                    ]),
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
                    prefix_expr(6, 11, [
                        not(6, 9),
                        id(10, 11)
                    ]),
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
            tokens: [expr(0, 3, [
                prefix_expr(0, 3, [dash(0, 1), int(1, 3)])
            ])]
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
                    prefix_expr(0, 2, [
                        dash(0, 1),
                        int(1, 2)
                    ]),
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
                    prefix_expr(4, 7, [
                        dash(4, 5),
                        int(5, 7),
                    ])
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
