use crate::leema::ast2;
use crate::leema::ast2::{
    Ast, AstNode, AstResult, Case, DataType, ModAction, ModTree, Xlist,
};
use crate::leema::failure::Lresult;
use crate::leema::lstr::Lstr;
use crate::leema::pratt;
use crate::leema::struple::StrupleItem;
use crate::leema::val::Val;

use pest::iterators::Pair;
use pest::Parser;

use std::cmp::Ordering;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};


#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Prec(i32);

impl Prec
{
    pub fn higher(&self) -> Prec
    {
        Prec(self.0 + 1)
    }
}

impl Default for Prec
{
    fn default() -> Prec
    {
        Prec(0)
    }
}

impl PartialOrd for Prec
{
    fn partial_cmp(&self, other: &Prec) -> Option<Ordering>
    {
        self.0.partial_cmp(&other.0)
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Assoc
{
    Left,
    Right,
    None,
}

impl Assoc
{
    pub fn next_prec(self, prec: Prec) -> Prec
    {
        match self {
            Assoc::Left => prec.higher(),
            Assoc::Right | Assoc::None => prec,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq)]
pub enum Placement
{
    Nofix,
    Prefix,
    Postfix,
    Infix(Assoc),
}

/// This is even sketchier than the Hash implementation
/// Now it will be impossible to compare Placement and
/// have it match the Infix association
impl PartialEq for Placement
{
    fn eq(&self, other: &Self) -> bool
    {
        match (*self, *other) {
            (Placement::Nofix, Placement::Nofix) => true,
            (Placement::Prefix, Placement::Prefix) => true,
            (Placement::Postfix, Placement::Postfix) => true,
            (Placement::Infix(_), Placement::Infix(_)) => true,
            _ => false,
        }
    }
}

/// This seems pretty sketchy, but I'm doing it anyway.
/// I want infix to always match regardless of association
/// so implementing the hash function to ignore infix association
impl Hash for Placement
{
    fn hash<H: Hasher>(&self, state: &mut H)
    {
        match self {
            Placement::Nofix => 1.hash(state),
            Placement::Prefix => 2.hash(state),
            Placement::Postfix => 3.hash(state),
            Placement::Infix(_) => 4.hash(state),
        }
    }
}

pub struct PrecRule
{
    place: Placement,
    prec: Prec,
}

type PrecKey = HashMap<(Placement, Rule), Prec>;


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

pub fn parse_file(text: &'static str) -> Lresult<Vec<AstNode>>
{
    let mut file_node = parse(Rule::file, text)?;
    match *file_node.pop().unwrap().node {
        Ast::Block(mut stmts) => {
            // pop the EOF off the end of the list
            stmts.pop();
            Ok(stmts)
        }
        what => panic!("expected block, found {:?}", what),
    }
}

pub fn parse(r: Rule, text: &'static str) -> Lresult<Vec<AstNode>>
{
    let it = LeemaParser::parse(r, text).map_err(|e| {
        println!("parse error: {:?}", e);
        rustfail!("parse failure", "{:?}", e,)
    })?;
    let prec = LeemaPrec::new();
    it.map(|i| prec.primary(i)).collect()
}

pub fn nodeloc(pair: &Pair<Rule>) -> ast2::Loc
{
    let (line, col) = pair.as_span().start_pos().line_col();
    ast2::Loc::new(line as u16, col as u8)
}

pub fn parse_mxline(pair: Pair<'static, Rule>) -> Lresult<ModTree>
{
    match pair.as_rule() {
        Rule::mxline => {
            let mut it = pair.into_inner();
            let head = it.next().unwrap();
            let opt_tail = it.next();
            match (head.as_rule(), opt_tail) {
                (Rule::mxmod, None) => {
                    Ok(ModTree::Leaf(head.as_str(), nodeloc(&head)))
                }
                (Rule::mxmod, Some(next)) => {
                    if next.as_rule() != Rule::mxblock {
                        panic!("unexpected import line block: {:?}", next);
                    }
                    let block: Lresult<Vec<ModTree>>;
                    block =
                        next.into_inner().map(|i| parse_mxline(i)).collect();
                    Ok(ModTree::branch(head.as_str(), block?))
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

pub struct LeemaPrec
{
    key: PrecKey,
}

impl LeemaPrec
{
    pub fn new() -> LeemaPrec
    {
        LeemaPrec {
            key: Self::init_key(),
        }
    }

    pub fn prefix_prec(&self, r: Rule) -> Option<Prec>
    {
        self.prec(Placement::Prefix, r)
    }

    pub fn postfix_prec(&self, r: Rule) -> Option<Prec>
    {
        self.prec(Placement::Postfix, r)
    }

    pub fn infix_prec(&self, r: Rule) -> Option<(Prec, Assoc)>
    {
        self.key
            .get_key_value(&(Placement::Infix(Assoc::None), r))
            .and_then(|((place, _), prec)| {
                match place {
                    Placement::Infix(assoc) => Some((*prec, *assoc)),
                    up => panic!("unexpected placement: {:?}", up),
                }
            })
    }

    fn prec(&self, p: Placement, r: Rule) -> Option<Prec>
    {
        self.key.get(&(p, r)).map(|result| *result)
    }

    fn init_key() -> PrecKey
    {
        ParserBuilder::new()
            //----
            .left()
            .infix(Rule::dot)
            .postfix(Rule::tuple)
            //----
            .right()
            .prefix(Rule::negative)
            .prefix(Rule::star)
            //----
            .left()
            .infix(Rule::star)
            .infix(Rule::slash)
            .infix(Rule::modulo)
            //----
            .left()
            .infix(Rule::plus)
            .infix(Rule::dash)
            .postfix(Rule::add_newline)
            .postfix(Rule::question)
            //----
            .left()
            .infix(Rule::less_than)
            .infix(Rule::equality)
            .infix(Rule::greater_than)
            //----
            .right()
            .prefix(Rule::not)
            //----
            .left()
            .infix(Rule::and)
            //----
            .left()
            .infix(Rule::or)
            //----
            .none()
            .nofix(Rule::expr)
            .nofix(Rule::float)
            .nofix(Rule::id)
            .nofix(Rule::int)
            .nofix(Rule::str)
            .into()
    }

    pub fn primary(&self, n: Pair<'static, Rule>) -> AstResult
    {
        let loc = nodeloc(&n);
        match n.as_rule() {
            Rule::id => Ok(AstNode::new(Ast::Id1(n.as_str()), loc)),
            Rule::int => {
                let i = n.as_str().parse().unwrap();
                Ok(AstNode::new_constval(Val::Int(i), loc))
            }
            Rule::expr => pratt::parse(self, &mut n.into_inner()),
            Rule::typex => pratt::parse(self, &mut n.into_inner()),
            Rule::strlit => {
                let s = Val::Str(Lstr::Sref(n.as_str()));
                Ok(AstNode::new_constval(s, loc))
            }
            Rule::str => {
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
            Rule::stresc => {
                let unescaped = match n.as_str() {
                    "\\n" => "\n",
                    "\\\"" => "\"",
                    keep => keep,
                };
                let s = Val::Str(Lstr::Sref(unescaped));
                Ok(AstNode::new_constval(s, loc))
            }
            Rule::hashtag => {
                let val = Val::Hashtag(Lstr::Sref(n.as_str()));
                Ok(AstNode::new_constval(val, loc))
            }
            Rule::tuple => {
                let mut tuple = self.parse_xlist(n.into_inner())?;
                if tuple.len() == 1 && tuple[0].k.is_none() {
                    Ok(tuple.pop().unwrap().v)
                } else {
                    Ok(AstNode::new(Ast::Tuple(tuple), loc))
                }
            }
            Rule::list => {
                let items = self.parse_xlist(n.into_inner())?;
                Ok(AstNode::new(Ast::List(items), loc))
            }
            Rule::and
            | Rule::or
            | Rule::not
            | Rule::less_than
            | Rule::equality
            | Rule::greater_than => Ok(AstNode::new(Ast::Id1(n.as_str()), loc)),
            Rule::stmt_block | Rule::file => {
                let inner: Lresult<Vec<AstNode>> =
                    n.into_inner().map(|i| self.primary(i)).collect();
                Ok(AstNode::new(Ast::Block(inner?), loc))
            }
            Rule::gen_type => {
                let mut inner = n.into_inner();
                let base = self.primary(inner.next().unwrap())?;
                let args = self.parse_xlist(inner)?;
                Ok(AstNode::new(Ast::Generic(base, args), loc))
            }
            Rule::let_stmt => {
                let mut inner = n.into_inner();
                let let_mode = inner.next().unwrap();
                let stmt = match let_mode.as_str() {
                    "let" => {
                        let id = self.primary(inner.next().unwrap())?;
                        let x = self.primary(inner.next().unwrap())?;
                        Ast::Let(id, AstNode::void(), x)
                    }
                    "const" => {
                        let id = inner.next().unwrap();
                        let x = self.primary(inner.next().unwrap())?;
                        Ast::DefConst(id.as_str(), x)
                    }
                    other => {
                        panic!("unexpected let mode {:?}", other);
                    }
                };
                Ok(AstNode::new(stmt, loc))
            }
            Rule::if_stmt => {
                let if_case = self.parse_case(n)?;
                Ok(AstNode::new(Ast::Ifx(vec![if_case]), loc))
            }
            Rule::ifx => {
                let cases = self.parse_cases(n.into_inner())?;
                Ok(AstNode::new(Ast::Ifx(cases), loc))
            }
            Rule::matchargs => {
                let cases = self.parse_cases(n.into_inner())?;
                Ok(AstNode::new(Ast::Matchx(None, cases), loc))
            }
            Rule::matchx => {
                let mut inner = n.into_inner();
                let x = self.primary(inner.next().unwrap())?;
                let cases = self.parse_cases(inner)?;
                Ok(AstNode::new(Ast::Matchx(Some(x), cases), loc))
            }
            Rule::case_block => {
                let cases = self.parse_cases(n.into_inner())?;
                Ok(AstNode::new(Ast::Matchx(None, cases), loc))
            }
            Rule::underscore => Ok(AstNode::new(Ast::Wildcard, loc)),
            Rule::def_enum => {
                let mut inner = n.into_inner();
                let id = self.primary(inner.next().unwrap())?;
                let vars: Lresult<Xlist> = inner
                    .map(|var| {
                        let mut vi = var.into_inner();
                        let vname = vi.next().unwrap();
                        let vloc = nodeloc(&vname);
                        let vname_str = vname.as_str();
                        let vname_ast = self.primary(vname)?;
                        let varg_it = vi.next().unwrap().into_inner();
                        let vargs: Xlist = self.parse_xlist(varg_it)?;
                        let df =
                            Ast::DefType(DataType::Struct, vname_ast, vargs);
                        let node = AstNode::new(df, vloc);
                        Ok(StrupleItem::new(Some(vname_str), node))
                    })
                    .collect();
                let df = Ast::DefType(DataType::Union, id, vars?);
                Ok(AstNode::new(df, loc))
            }
            Rule::def_struct => {
                let mut inner = n.into_inner();
                let id = self.primary(inner.next().unwrap())?;
                let arg_it = inner.next().unwrap().into_inner();
                let args: Xlist = self.parse_xlist(arg_it)?;
                let df = Ast::DefType(DataType::Struct, id, args);
                Ok(AstNode::new(df, loc))
            }
            Rule::def_func => {
                let mut inner = n.into_inner();
                let func_mode = inner.next().unwrap();
                let def = match func_mode.as_str() {
                    "func" => {
                        let func_name = self.primary(inner.next().unwrap())?;
                        let func_result =
                            self.primary(inner.next().unwrap())?;
                        let func_arg_it = inner.next().unwrap().into_inner();
                        let func_args: Xlist = self.parse_xlist(func_arg_it)?;
                        let block = self.primary(inner.next().unwrap())?;
                        Ast::DefFunc(func_name, func_args, func_result, block)
                    }
                    "macro" => {
                        let func_name = inner.next().unwrap();
                        // func result should be Void
                        let _func_result =
                            self.primary(inner.next().unwrap())?;
                        let func_arg_it = inner.next().unwrap().into_inner();
                        let func_args =
                            func_arg_it.map(|it| it.as_str()).collect();
                        let block = self.primary(inner.next().unwrap())?;
                        Ast::DefMacro(func_name.as_str(), func_args, block)
                    }
                    unknown => {
                        return Err(rustfail!(
                            "compile_error",
                            "unrecognized func mode: {:?}",
                            unknown,
                        ));
                    }
                };
                Ok(AstNode::new(def, loc))
            }
            Rule::def_func_result => {
                match n.into_inner().next() {
                    Some(result) => self.primary(result),
                    None => Ok(AstNode::new(Ast::Id1("Void"), loc)),
                }
            }
            Rule::list_type => {
                let mut inner = n.into_inner();
                let inner = self.primary(inner.next().unwrap())?;
                let inner_type = vec![StrupleItem::new_v(inner)];
                Ok(AstNode::new(Ast::List(inner_type), loc))
            }
            Rule::tuple_type => {
                let items = self.parse_xlist(n.into_inner())?;
                Ok(AstNode::new(Ast::Tuple(items), loc))
            }
            Rule::mxstmt => {
                let mut inner = n.into_inner();
                let mxpair = inner.next().unwrap();
                let mx = match mxpair.as_str() {
                    "import" => ModAction::Import,
                    "export" => ModAction::Export,
                    _ => {
                        panic!("expected import or export, found {:?}", mxpair)
                    }
                };
                let mxline = parse_mxline(inner.next().unwrap())?;
                Ok(AstNode::new(Ast::ModAction(mx, mxline), loc))
            }
            Rule::return_stmt => {
                let mut inner = n.into_inner();
                let stmt = Ast::Return(self.primary(inner.next().unwrap())?);
                Ok(AstNode::new(stmt, loc))
            }
            Rule::def_rust_type => {
                let mut inner = n.into_inner();
                let id = self.primary(inner.next().unwrap())?;
                let df = Ast::DefType(DataType::Rust, id, vec![]);
                Ok(AstNode::new(df, loc))
            }
            Rule::EOI => Ok(AstNode::void()),
            Rule::rust_block => Ok(AstNode::new(Ast::RustBlock, loc)),
            // ignore this level and go one deeper
            Rule::blockx | Rule::tx_maybe_k => {
                pratt::parse(self, &mut n.into_inner())
            }

            // unexpected and invalid
            Rule::x1 | Rule::prefix1 | Rule::postfix1 => {
                panic!("cannot parse silent rule: {:?}", n);
            }
            _ => {
                panic!("unsupported rule: {:?}", n);
            }
        }
    }

    pub fn unary(&self, op: Pair<'static, Rule>, x: AstNode) -> AstResult
    {
        let loc = nodeloc(&op);
        match op.as_rule() {
            Rule::tuple => {
                let tuple = self.parse_xlist(op.into_inner())?;
                let call_loc = x.loc;
                Ok(AstNode::new(Ast::Call(x, tuple), call_loc))
            }
            Rule::negative | Rule::not | Rule::star => {
                let ast = Ast::Op1(op.as_str(), x);
                Ok(AstNode::new(ast, loc))
            }
            Rule::question => {
                let ast = Ast::Generic(
                    AstNode::new(Ast::Id1("Option"), loc),
                    vec![StrupleItem::new_v(x)],
                );
                Ok(AstNode::new(ast, loc))
            }
            Rule::add_newline => {
                let strx = vec![x, AstNode::new(Ast::NEWLINE, loc)];
                Ok(AstNode::new(Ast::StrExpr(strx), loc))
            }
            _ => {
                panic!("unknown unary operator: {:?}", op);
            }
        }
    }

    pub fn binary(
        &self,
        a: AstNode,
        op: Pair<'static, Rule>,
        b: AstNode,
    ) -> AstResult
    {
        match op.as_rule() {
            Rule::dot
            | Rule::and
            | Rule::or
            | Rule::plus
            | Rule::dash
            | Rule::star
            | Rule::slash
            | Rule::modulo
            | Rule::less_than
            | Rule::equality
            | Rule::greater_than => {
                let ast = Ast::Op2(op.as_str(), a, b);
                Ok(AstNode::new(ast, nodeloc(&op)))
            }
            _ => {
                panic!("unknown operator: {:?}", op);
            }
        }
    }

    fn parse_case(&self, pair: Pair<'static, Rule>) -> Lresult<Case>
    {
        let mut inner = pair.into_inner();
        let patt = self.primary(inner.next().unwrap())?;
        let block = self.primary(inner.next().unwrap())?;
        Ok(Case::new(patt, block))
    }

    fn parse_cases<Inputs>(&self, it: Inputs) -> Lresult<Vec<Case>>
    where
        Inputs: Iterator<Item = Pair<'static, Rule>>,
    {
        it.map(|x| self.parse_case(x)).collect()
    }

    fn parse_xlist<Inputs>(&self, it: Inputs) -> Lresult<Xlist>
    where
        Inputs: Iterator<Item = Pair<'static, Rule>>,
    {
        it.map(|p| self.parse_x_maybe_k(p)).collect()
    }

    fn parse_x_maybe_k(
        &self,
        pair: Pair<'static, Rule>,
    ) -> Lresult<StrupleItem<Option<&'static str>, AstNode>>
    {
        match pair.as_rule() {
            Rule::x_maybe_k | Rule::tx_maybe_k => {
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

struct Preop(Placement, Rule);

pub struct ParserBuilder
{
    prec: Vec<Vec<Preop>>,
}

impl ParserBuilder
{
    pub fn new() -> Self
    {
        ParserBuilder { prec: vec![] }
    }

    pub fn left(self) -> PrecBuilder
    {
        PrecBuilder::open(self, Assoc::Left)
    }

    pub fn right(self) -> PrecBuilder
    {
        PrecBuilder::open(self, Assoc::Right)
    }

    pub fn none(self) -> PrecBuilder
    {
        PrecBuilder::open(self, Assoc::None)
    }
}

pub struct PrecBuilder
{
    parser: ParserBuilder,
    assoc: Assoc,
    prec: Vec<Preop>,
}

impl PrecBuilder
{
    pub fn open(parser: ParserBuilder, assoc: Assoc) -> Self
    {
        PrecBuilder {
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

    pub fn none(self) -> PrecBuilder
    {
        self.close().none()
    }

    pub fn infix(mut self, r: Rule) -> Self
    {
        self.prec.push(Preop(Placement::Infix(self.assoc), r));
        self
    }

    pub fn prefix(mut self, r: Rule) -> Self
    {
        if self.assoc == Assoc::Left {
            panic!("prefix rules cannot be left associative: {:?}", r);
        }
        self.prec.push(Preop(Placement::Prefix, r));
        self
    }

    pub fn postfix(mut self, r: Rule) -> PrecBuilder
    {
        if self.assoc == Assoc::Right {
            panic!("postfix rules cannot be right associative: {:?}", r);
        }
        self.prec.push(Preop(Placement::Postfix, r));
        self
    }

    pub fn nofix(mut self, r: Rule) -> PrecBuilder
    {
        self.prec.push(Preop(Placement::Nofix, r));
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
            let prec = Prec(iprec as i32);
            for preop in p.into_iter() {
                key.insert((preop.0, preop.1), prec);
            }
        }
        key
    }
}


#[cfg(test)]
mod tests
{
    use super::{parse, parse_file, LeemaParser, Rule};
    use crate::leema::ast2::{Ast, DataType, ModAction, ModTree};
    use crate::leema::lstr::Lstr;
    use crate::leema::val::Val;

    use matches::assert_matches;
    use pest::{consumes_to, parses_to};

    #[test]
    fn add_newline()
    {
        // call as postfix + add_newline postfix
        let input = r#"f(x) \n"#;
        let actual = parse(Rule::expr, input).unwrap();
        println!("{:#?}", actual);
        parses_to!(
            parser: LeemaParser,
            input: input,
            rule: Rule::expr,
            tokens: [
                expr(0, 7, [
                    id(0, 1),
                    tuple(1, 4, [
                        x_maybe_k(2, 3, [expr(2, 3, [id(2, 3)])]),
                    ]),
                    add_newline(5, 7),
                ])
            ]
        )
    }

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
        let input = "foo(5, x: y)";
        let actual = parse(Rule::expr, input).unwrap();
        println!("{:#?}", actual);
        parses_to!(
            parser: LeemaParser,
            input: input,
            rule: Rule::expr,
            tokens: [expr(0, 12, [
                id(0, 3),
                tuple(3, 12, [
                    x_maybe_k(4, 5, [expr(4, 5, [int(4, 5)])]),
                    x_maybe_k(7, 11, [
                        id(7, 8),
                        expr(10, 11, [id(10, 11)]),
                    ]),
                ])
            ])]
        );

        if let Ast::Call(name, args) = &*actual[0].node {
            assert_eq!(Ast::Id1("foo"), *name.node);
            assert_eq!(None, args[0].k);
            assert_eq!(Some("x"), args[1].k);
            assert_eq!(Ast::ConstVal(Val::Int(5)), *args[0].v.node);
            assert_eq!(Ast::Id1("y"), *args[1].v.node);
            assert_eq!(2, args.len());
        } else {
            panic!("expected DefUnion, found {:?}", actual[0]);
        }
        assert_eq!(1, actual.len());
    }

    #[test]
    fn call_no_args()
    {
        let input = "foo()";
        let actual = parse(Rule::expr, input).unwrap();
        println!("{:#?}", actual);
        parses_to!(
            parser: LeemaParser,
            input: input,
            rule: Rule::expr,
            tokens: [expr(0, 5, [
                id(0, 3),
                tuple(3, 5)
            ])]
        )
    }

    #[test]
    fn def_enum_bool()
    {
        let input = "datatype Bool
        |False
        |True
        --
        ";
        let actual = parse(Rule::def_enum, input).unwrap();
        println!("{:#?}", actual);
        if let Ast::DefType(DataType::Union, name, vars) = &*actual[0].node {
            assert_eq!(Ast::Id1("Bool"), *name.node);
            assert_eq!("False", vars[0].k.unwrap());
            assert_eq!("True", vars[1].k.unwrap());
            if let Ast::DefType(DataType::Struct, n, flds) = &*vars[0].v.node {
                assert_eq!(Ast::Id1("False"), *n.node);
                assert_eq!(0, flds.len());
            } else {
                panic!("expected False, found {:?}", vars[0].v);
            }
            if let Ast::DefType(DataType::Struct, n, flds) = &*vars[1].v.node {
                assert_eq!(Ast::Id1("True"), *n.node);
                assert_eq!(0, flds.len());
            } else {
                panic!("expected True, found {:?}", vars[1].v);
            }
            assert_eq!(2, vars.len());
        } else {
            panic!("expected DefUnion, found {:?}", actual[0]);
        }
        assert_eq!(1, actual.len());
    }

    #[test]
    fn def_func_arrowblock()
    {
        let input = "func foo ->
            bar(x, y)
            baz()
        --
        ";
        let actual = parse_file(input).unwrap();
        println!("{:#?}", actual);
        parses_to!(
            parser: LeemaParser,
            input: input,
            rule: Rule::file,
            tokens: [file(0, 71, [
                def_func(0, 62, [
                    func_mode(0, 4),
                    id(5, 8),
                    def_func_result(8, 8),
                    def_func_args(8, 8),
                    stmt_block(24, 60, [
                        expr(24, 33, [
                            id(24, 27),
                            tuple(27, 33, [
                                x_maybe_k(28, 29, [expr(28, 29, [id(28, 29)])]),
                                x_maybe_k(31, 32, [expr(31, 32, [id(31, 32)])]),
                            ])
                        ]),
                        expr(46, 51, [
                            id(46, 49),
                            tuple(49, 51),
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
        let actual = parse_file(input).unwrap();
        println!("{:#?}", actual);
        parses_to!(
            parser: LeemaParser,
            input: input,
            rule: Rule::file,
            tokens: [file(0, 15, [
                def_func(0, 15, [
                    func_mode(0, 4),
                    id(5, 8),
                    def_func_result(8, 8),
                    def_func_args(8, 8),
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
                tx_maybe_k(3, 8, [
                    id(3, 4),
                    typex(5, 8, [id(5, 8)]),
                ]),
                tx_maybe_k(9, 14, [
                    id(9, 10),
                    typex(11, 14, [id(11, 14)]),
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
        if let Ast::DefFunc(name, args, result, _body) = &*actual[0].node {
            assert_eq!(Ast::Id1("format"), *name.node);
            assert_eq!(*"x", *args[0].k.unwrap());
            assert_eq!(Ast::Id1("Int"), *args[0].v.node);
            assert_eq!(Ast::Id1("Str"), *result.node);
        } else {
            panic!("expected DefFunc, found {:?}", actual[0]);
        }
        assert_eq!(1, actual.len());
    }

    #[test]
    fn def_func_generic_line()
    {
        let input = "func <first A B>:A :: a:A b:B ->
                a
            --";
        let actual = parse(Rule::def_func, input).unwrap();
        println!("{:#?}", actual);
        if let Ast::DefFunc(name, args, result, _body) = &*actual[0].node {
            if let Ast::Generic(first, type_args) = &*name.node {
                assert_eq!(Ast::Id1("first"), *first.node);
                assert_eq!(None, type_args[0].k);
                assert_eq!(None, type_args[1].k);
                assert_eq!(Ast::Id1("A"), *type_args[0].v.node);
                assert_eq!(Ast::Id1("B"), *type_args[1].v.node);
                assert_eq!(2, type_args.len());
            } else {
                panic!("expected generic 'first', found {:?}", name.node);
            }
            assert_eq!(*"a", *args[0].k.unwrap());
            assert_eq!(*"b", *args[1].k.unwrap());
            assert_eq!(Ast::Id1("A"), *args[0].v.node);
            assert_eq!(Ast::Id1("B"), *args[1].v.node);
            assert_eq!(Ast::Id1("A"), *result.node);
        } else {
            panic!("expected DefFunc, found {:?}", actual[0]);
        }
        assert_eq!(1, actual.len());
    }

    #[test]
    fn def_func_generic_file()
    {
        let input = r#"
        func new_pair'A'B:(A B) :: a:A b:B ->
            (a, b)
        --
        "#;
        let actual = parse_file(input).unwrap();
        assert_eq!(1, actual.len());
    }

    #[test]
    fn def_func_match_block()
    {
        let input = r#"
        func factorial:Int :: i:Int
        |0 -> 0
        |1 -> 1
        |n -> n * factorial(n - 1)
        --
        "#;
        let actual = parse_file(input).unwrap();
        assert_eq!(1, actual.len());
    }

    #[test]
    fn def_macro()
    {
        let input = "macro macro_first :: a b ->
            a
        --";
        let actual = parse(Rule::def_func, input).unwrap();
        println!("{:#?}", actual);
        if let Ast::DefMacro(name, args, _body) = &*actual[0].node {
            assert_eq!("macro_first", *name);
            assert_eq!("a", args[0]);
            assert_eq!("b", args[1]);
        } else {
            panic!("expected DefMacro, found {:?}", actual[0]);
        }
        assert_eq!(1, actual.len());
    }

    #[test]
    fn def_struct_token()
    {
        let input = "datatype Taco --";
        let actual = parse(Rule::def_struct, input).unwrap();
        println!("{:#?}", actual);
        if let Ast::DefType(DataType::Struct, name, args) = &*actual[0].node {
            assert_eq!(Ast::Id1("Taco"), *name.node);
            assert_eq!(0, args.len());
        } else {
            panic!("expected DefStruct, found {:?}", actual[0]);
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

    /*
    #[test]
    fn generic_expr()
    {
        parses_to!(
            parser: LeemaParser,
            input: "<foo A>",
            rule: Rule::expr,
            tokens: [
                expr(0, 5, [
                    <
                    id(0, 3),
                    tick(3, 4),
                    id(4, 5),
                ])
            ]
        )
    }
    */

    #[test]
    fn generic_typex()
    {
        parses_to!(
            parser: LeemaParser,
            input: "<foo A>",
            rule: Rule::typex,
            tokens: [
                typex(0, 7, [
                    gen_type(0, 7, [
                        typex(1, 4, [id(1, 4)]),
                        tx_maybe_k(5, 6, [typex(5, 6, [id(5, 6)])]),
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
                mxmod(0, 14),
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
        let imps = parse_file(input).unwrap();
        println!("{:#?}", imps);
        // /root/path
        if let Ast::ModAction(ModAction::Import, root) = &*imps[0].node {
            assert_matches!(root, ModTree::Leaf("/root/path", _));
        } else {
            panic!("expected import, found {:?}", imps[0]);
        }
        // ../sibling/path
        if let Ast::ModAction(ModAction::Import, sib) = &*imps[1].node {
            assert_matches!(sib, ModTree::Leaf("../sibling/path", _));
        } else {
            panic!("expected import, found {:?}", imps[0]);
        }
        // child/path
        if let Ast::ModAction(ModAction::Import, ch) = &*imps[2].node {
            assert_matches!(ch, ModTree::Leaf("child/path", _));
        } else {
            panic!("expected import, found {:?}", imps[0]);
        }
        // child.funky
        if let Ast::ModAction(ModAction::Import, ch) = &*imps[3].node {
            assert_matches!(ch, ModTree::Leaf("child.funky", _));
        } else {
            panic!("expected import, found {:?}", imps[0]);
        }
        assert_eq!(4, imps.len());
    }

    #[test]
    fn mxblock()
    {
        let input = "import /foo ->
            bar
            baz/tacos
            tortas.food
        --
        ";
        let imps = parse_file(input).unwrap();
        println!("{:#?}", imps);
        if let Ast::ModAction(ModAction::Import, root) = &*imps[0].node {
            if let ModTree::Branch("/foo", block) = root {
                assert_matches!(block[0], ModTree::Leaf("bar", _));
            } else {
                panic!("expected sub /foo, found {:?}", root);
            }
        } else {
            panic!("expected import, found {:?}", imps);
        }
        assert_eq!(1, imps.len());
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
    fn not_paren_infix()
    {
        let input = "not (a and b)";
        let actual = parse(Rule::expr, input).unwrap();
        println!("{:#?}", actual);

        parses_to!(
            parser: LeemaParser,
            input: input,
            rule: Rule::expr,
            tokens: [
                expr(0, 13, [
                    not(0, 3),
                    tuple(4, 13, [
                        x_maybe_k(5, 12, [
                            expr(5, 12, [
                                id(5, 6),
                                and(7, 10),
                                id(11, 12)
                            ])
                        ])
                    ])
                ])
            ]
        );

        let t = &actual[0];
        if let Ast::Op1("not", x) = &*t.node {
            if let Ast::Op2("and", a, b) = &*x.node {
                assert_eq!(Ast::Id1("a"), *a.node);
                assert_eq!(Ast::Id1("b"), *b.node);
            } else {
                panic!("expected and op2, found {:?}", x);
            }
        } else {
            panic!("expected not op1, found {:?}", t);
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
            tokens: [expr(0, 3, [negative(0, 1), int(1, 3)])]
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
                    negative(0, 1),
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
                    negative(4, 5),
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
        let input = r#""taco""#;
        parses_to!(
            parser: LeemaParser,
            input: input,
            rule: Rule::expr,
            tokens: [
                expr(0, 6, [
                    str(0, 6, [strlit(1, 5)])
                ])
            ]
        );
        let ast = parse(Rule::expr, input).unwrap();
        assert_eq!(Ast::ConstVal(Val::Str(Lstr::Sref("taco"))), *ast[0].node);
    }

    #[test]
    fn str_escapes()
    {
        let input = r#""ta\co\"burr\nito\n""#;
        parses_to!(
            parser: LeemaParser,
            input: input,
            rule: Rule::expr,
            tokens: [
                expr(0, 20, [
                    str(0, 20, [
                        strlit(1, 3),
                        stresc(3, 4),
                        strlit(4, 6),
                        stresc(6, 8),
                        strlit(8, 12),
                        stresc(12, 14),
                        strlit(14, 17),
                        stresc(17, 19),
                    ])
                ])
            ]
        );
        let sstr = |s: &'static str| Ast::ConstVal(Val::Str(Lstr::Sref(s)));
        let actual = parse(Rule::expr, input).unwrap();
        if let Ast::StrExpr(s) = &*actual[0].node {
            let mut it = s.iter();
            assert_eq!(sstr("ta"), *it.next().unwrap().node);
            assert_eq!(sstr("\\"), *it.next().unwrap().node);
            assert_eq!(sstr("co"), *it.next().unwrap().node);
            assert_eq!(sstr("\""), *it.next().unwrap().node);
            assert_eq!(sstr("burr"), *it.next().unwrap().node);
            assert_eq!(sstr("\n"), *it.next().unwrap().node);
            assert_eq!(sstr("ito"), *it.next().unwrap().node);
            assert_eq!(sstr("\n"), *it.next().unwrap().node);
            assert_eq!(None, it.next());
        } else {
            panic!("expected StrExpr, found {:?}", actual[0]);
        }
        assert_eq!(1, actual.len());
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

    #[test]
    fn type_tuple()
    {
        let input = r#"(A B)"#;
        let ast = parse(Rule::typex, input).unwrap();
        println!("tuple type: {:#?}", ast);
        parses_to!(
            parser: LeemaParser,
            input: input,
            rule: Rule::typex,
            tokens: [
                typex(0, 5, [
                    tuple_type(0, 5, [
                        tx_maybe_k(1, 2, [
                            typex(1, 2, [id(1, 2)]),
                        ]),
                        tx_maybe_k(3, 4, [
                            typex(3, 4, [id(3, 4)]),
                        ]),
                    ])
                ])
            ]
        );
        if let Ast::Tuple(items) = &*ast[0].node {
            assert_eq!(Ast::Id1("A"), *items[0].v.node);
            assert_eq!(Ast::Id1("B"), *items[1].v.node);
        } else {
            panic!("expected tuple, found {:?}", *ast[0].node);
        }
        assert_eq!(1, ast.len());
    }

    #[test]
    fn type_suffixes()
    {
        let input = r#"(A? B*)"#;
        let ast = parse(Rule::typex, input).unwrap();
        println!("suffix type: {:#?}", ast);
        parses_to!(
            parser: LeemaParser,
            input: input,
            rule: Rule::typex,
            tokens: [
                typex(0, 7, [
                    tuple_type(0, 7, [
                        tx_maybe_k(1, 3, [
                            typex(1, 3, [
                                id(1, 2),
                                question(2, 3),
                            ]),
                        ]),
                        tx_maybe_k(4, 6, [
                            typex(4, 6, [
                                id(4, 5),
                                star(5, 6),
                            ]),
                        ]),
                    ])
                ])
            ]
        );
        if let Ast::Tuple(items) = &*ast[0].node {
            assert_eq!(Ast::Id1("A"), *items[0].v.node);
            assert_eq!(Ast::Id1("B"), *items[1].v.node);
        } else {
            panic!("expected tuple, found {:?}", *ast[0].node);
        }
        assert_eq!(1, ast.len());
    }
}
