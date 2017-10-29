use leema::val::{Val, Type};
use leema::log;
use leema::parse::{Parser, Token};
use std::collections::HashMap;
use std::io::prelude::*;
use std::io::{stderr};

/*
#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub enum FuncType {
    Pure,
    //Query,
    //Cmd,
    //IO,
    //Sys,
}
*/

#[derive(Copy)]
#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub struct TokenLoc
{
    lineno: i32,
    column: i16,
}

impl TokenLoc
{
    pub fn new(l: i32, c: i16) -> TokenLoc
    {
        TokenLoc{lineno: l, column: c}
    }
}

#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub struct TokenData<T>
{
    pub data: T,
    loc: TokenLoc,
}

impl<T> TokenData<T>
{
    pub fn new(d: T, tl: TokenLoc) -> TokenData<T>
    {
        TokenData{
            data: d,
            loc: tl,
        }
    }
}


pub fn parse(toks: Vec<Token>) -> Val
{
    let e = Err(0);
    let mut p = Parser::new(e);
    for t in toks {
        p.parse(t);
    }
    p.parse(Token::EOI);
    p.into_extra().unwrap()
}


#[cfg(test)]
mod tests {
    use leema::val::{Val, SxprType, Type};
    use leema::ast;
    use leema::sxpr;
    use leema::list;
    use leema::lex::{lex};
    use std::collections::HashMap;
    use std::rc::Rc;

#[test]
fn test_ast_parse_plus() {
    let input = "5 + 3\n";
    let lexed = lex(input);
    let root = ast::parse(lexed);
    let xargs = list::from2(Val::Int(5), Val::Int(3));
    let expected = sxpr::new(SxprType::BlockExpr,
        list::singleton(sxpr::call(Val::id("int_add".to_string()), xargs))
    );
    assert_eq!(expected, root);
}

#[test]
fn test_ast_parse_strlit() {
    let input = "\"taco\"\n";
    let root = ast::parse(lex(input));

    let expected = sxpr::new(SxprType::BlockExpr,
        list::singleton(Val::new_str("taco".to_string()))
    );
    assert_eq!(expected, root);
}

#[test]
fn test_ast_parse_string_id() {
    let input = "\"$var\"\n";
    let root = ast::parse(lex(input));

    let expected = sxpr::new(SxprType::BlockExpr,
        list::singleton(Val::id("var".to_string()))
    );
    assert_eq!(expected, root);
}

#[test]
fn test_ast_parse_string_list() {
    let input = "\"hello $name\n\"\n";
    let root = ast::parse(lex(input));

    let part1 = Val::new_str("hello ".to_string());
    let part2 = Val::id("name".to_string());
    let part3 = Val::new_str("\n".to_string());
    let expected = sxpr::new(SxprType::BlockExpr, list::singleton(
        sxpr::new(SxprType::StrExpr,
            list::cons(part1,
            list::cons(part2,
            list::cons(part3,
            Val::Nil,
            ))))
    ));
    assert_eq!(expected, root);
}

#[test]
fn test_ast_parse_plus_twice() {
    let input = "5 + 3 + 2\n";
    let root = ast::parse(lex(input));

    let first_add = sxpr::call(Val::id("int_add".to_string()),
        list::from2(Val::Int(5), Val::Int(3)),
    );
    let second_add = sxpr::call(Val::id("int_add".to_string()),
        list::from2(first_add, Val::Int(2))
    );

    let expected = sxpr::new(SxprType::BlockExpr,
        list::singleton(second_add)
    );
    assert_eq!(expected, root);
}

#[test]
fn test_ast_parse_call_one_param()
{
    let input = "inc(~4)\n";
    let root = ast::parse(lex(input));

    let neg4 = sxpr::call(
        Val::id("int_negate".to_string()),
        list::singleton(Val::Int(4)),
    );
    let expected = sxpr::new(SxprType::BlockExpr,
        list::singleton(
            sxpr::call(Val::id("inc".to_string()), list::singleton(neg4))
        )
    );
    assert_eq!(expected, root);
}

#[test]
fn test_ast_parse_function_call() {
    let input = "foo(7, 2)\n";
    let root = ast::parse(lex(input));

    let xargs = list::from2(Val::Int(7), Val::Int(2));
    let expected = sxpr::new(SxprType::BlockExpr,
        list::singleton(sxpr::call(Val::id("foo".to_string()), xargs))
    );
    assert_eq!(expected, root);
}

#[test]
fn test_ast_parse_tuple() {
    let input = "(3, \"taco\", true)\n";
    let root = ast::parse(lex(input));

    let xtup = list::singleton(Val::Tuple(vec![
        Val::Int(3),
        Val::new_str("taco".to_string()),
        Val::Bool(true),
    ]));
    let expected = sxpr::new(SxprType::BlockExpr, xtup);
    assert_eq!(expected, root);
}

#[test]
fn test_ast_parse_list_empty() {
    let input = "[]\n";
    let root = ast::parse(lex(input));

    let expected = sxpr::new(
        SxprType::BlockExpr, list::singleton(list::empty())
    );
    assert_eq!(expected, root);
}

#[test]
fn test_ast_parse_list() {
    let input = "[1, 2, x]\n";
    let root = ast::parse(lex(input));

    let xlist = list::cons(Val::Int(1),
        list::cons(Val::Int(2),
        list::cons(Val::id("x".to_string()),
        Val::Nil,
    )));
    let expected = sxpr::new(
        SxprType::BlockExpr, list::singleton(xlist)
    );
    assert_eq!(expected, root);
}

#[test]
fn test_ast_parse_list_cons() {
    let input = "1;2;x\n";
    let root = ast::parse(lex(input));

    let lst =
        list::cons(Val::Int(1),
            list::cons(Val::Int(2),
            Val::id("x".to_string())
        ));

    let expected = sxpr::new(SxprType::BlockExpr, list::singleton(lst));
    assert_eq!(expected, root);
}

#[test]
fn test_call_function_plus_comma()
{
    let input = "
    func main() ->
        foo(x+1, 40)
    --
    ";
    ast::parse(lex(input));
}

#[test]
fn test_parse_empty_tuple() {
    let input = "()";
    let root = ast::parse(lex(input));

    let xtuple = Val::Tuple(vec![]);
    let expected = sxpr::new(
        SxprType::BlockExpr, list::singleton(xtuple)
    );
    assert_eq!(expected, root);
}

#[test]
fn test_parse_one_tuple() {
    let input = "(5)";
    let root = ast::parse(lex(input));

    let xtuple = Val::Int(5);
    let expected = sxpr::new(
        SxprType::BlockExpr, list::singleton(xtuple)
    );
    assert_eq!(expected, root);
}

#[test]
fn test_parse_match_empty_list() {
    let input = "
    func is_empty(l)
    |([]) -> true
    |(_) -> false
    --
    ";
    let root = ast::parse(lex(input));

    let cases =
        list::cons(Val::Tuple(vec![list::empty()]),
        list::cons(sxpr::new_block(list::singleton(Val::Bool(true))),
        list::cons(
            list::cons(Val::Tuple(vec![Val::Wildcard]),
            list::cons(sxpr::new_block(list::singleton(Val::Bool(false))),
            Val::Nil)),
        Val::Nil)));
    let matchblk =
        sxpr::defunc(Val::id("is_empty".to_string()),
            list::singleton(Val::typed_id("l", Type::AnonVar)),
            Val::Type(Type::AnonVar),
            sxpr::match_expr(Val::CallParams, cases),
            Val::Void,
        );
    let expected = sxpr::new(SxprType::BlockExpr, list::singleton(matchblk));
    assert_eq!(expected, root);
}

#[test]
fn test_call_function_comma_plus()
{
    let input = "
    func main() ->
        foo(40, x+1)
    --
    ";
    ast::parse(lex(input));
}

#[test]
fn test_parse_multiple_param_func()
{
    let input = "
    func doubles(x, x2) ->
        x + x == x2
    --

    func main() ->
        doubles(5, 10)
    --
    ";
    ast::parse(lex(input));
}

#[test]
fn test_ast_parse_if()
{
    let input = "
    if x ->
        y
    else ->
        z
    --
    ";
    let root = ast::parse(lex(input));

    let blocka = sxpr::new(SxprType::BlockExpr, list::singleton(
        Val::id("y".to_string()),
    ));
    let blockb = sxpr::new(SxprType::BlockExpr, list::singleton(
        Val::id("z".to_string()),
    ));
    let expected = sxpr::new(SxprType::BlockExpr, list::cons(
        sxpr::new(SxprType::IfExpr,
        list::cons(Val::id("x".to_string()),
        list::cons(blocka,
        list::cons(blockb,
        Val::Nil,
        )))),
        Val::Nil,
    ));
    assert_eq!(expected, root);
}

#[test]
fn test_ast_parse_if_no_else()
{
    let input = "if x -> y --";
    let root = ast::parse(lex(input));

    let blocka = sxpr::new(SxprType::BlockExpr, list::singleton(
        Val::id("y".to_string()),
    ));
    let expected = sxpr::new(SxprType::BlockExpr, list::cons(
        sxpr::new(SxprType::IfExpr,
        list::cons(Val::id("x".to_string()),
        list::cons(blocka,
        list::cons(Val::Void,
        Val::Nil,
        )))),
        Val::Nil,
    ));
    assert_eq!(expected, root);
}

#[test]
fn test_ast_parse_macro()
{
    let input = "
    macro mand(a, b) ->
        if
        |a -> b
        |else -> false
        --
    --
    ";
    let root = ast::parse(lex(input));

    let blocka = sxpr::new(SxprType::BlockExpr, list::singleton(
        Val::id("b".to_string()),
    ));
    let blockb = sxpr::new(SxprType::BlockExpr, list::singleton(
        Val::Bool(false)
    ));
    let ifx = sxpr::new(SxprType::BlockExpr, list::cons(
        sxpr::new(SxprType::IfExpr,
            list::cons(Val::id("a".to_string()),
            list::cons(blocka,
            list::cons(blockb,
            Val::Nil,
        )))),
        Val::Nil,
    ));
    let args =
        list::cons(Val::id("a".to_string()),
        list::cons(Val::id("b".to_string()),
        Val::Nil,
    ));
    let expected = sxpr::new_block(list::singleton(
        sxpr::new(SxprType::DefMacro,
            list::cons(Val::id("mand".to_string()),
            list::cons(args,
            list::cons(ifx,
            Val::Nil,
        ))))
    ));
    assert_eq!(expected, root);
}

#[test]
fn test_parse_call_function_call_result()
{
    let input = "(foo(5))(6)";
    let root = ast::parse(lex(input));

    let foo_call = sxpr::new(SxprType::Call,
        list::from2(Val::id("foo".to_string()), Val::Int(5))
        );
    let expected = sxpr::new_block(list::singleton(
        sxpr::new(SxprType::Call,
            list::from2(foo_call, Val::Int(6)),
        )
    ));
    assert_eq!(expected, root);
}

#[test]
fn test_parse_defstruct()
{
    let input = "
    struct Taco
    .id: Int
    .name: Str
    --
    ";
    let root = ast::parse(lex(input));

    let expected = sxpr::new_block(list::singleton(
        sxpr::new(SxprType::DefStruct,
        list::cons(Val::Type(Type::Id(Rc::new("Taco".to_string()))),
        list::cons(Val::TypedId(Rc::new("id".to_string()), Type::Int),
        list::cons(Val::TypedId(Rc::new("name".to_string()), Type::Str),
        Val::Nil,
        )))),
    ));
    assert_eq!(expected, root);
}

#[test]
fn test_parse_match_list()
{
    let input = "
    match x
    |(h;t) -> h
    |(_) -> false
    --
    ";
    let root = ast::parse(lex(input));

    let expected = sxpr::new_block(list::singleton(
        sxpr::match_expr(
            Val::id("x".to_string()),
            list::from3(
                Val::Tuple(vec![list::cons(
                    Val::id("h".to_string()), Val::id("t".to_string())
                )]),
                sxpr::new_block(list::singleton(Val::id("h".to_string()))),
            list::from2(
                Val::Tuple(vec![Val::Wildcard]),
                sxpr::new_block(list::singleton(Val::Bool(false))),
            )),
        ),
    ));
    assert_eq!(expected, root);
}

#[test]
fn test_parse_constructor_call()
{
    let input = "Taco(1, 2)";
    let root = ast::parse(lex(input));

    let expected = sxpr::new_block(list::singleton(
        sxpr::new(SxprType::Call,
        list::cons(Val::Id(Rc::new("Taco".to_string())),
        list::cons(Val::Int(1),
        list::cons(Val::Int(2),
        Val::Nil,
        )))),
    ));
    assert_eq!(expected, root);
}

#[test]
fn test_parse_strlit_field_access()
{
    let input = "\"hello ${dog.name}\"";
    let root = ast::parse(lex(input));

    let expected = sxpr::new_block(list::singleton(
        sxpr::new(SxprType::StrExpr,
        list::cons(Val::new_str("hello ".to_string()),
        list::cons(Val::dot_access(
            Val::id("dog".to_string()),
            "name".to_string(),
            ),
        Val::Nil,
        ))),
    ));
    assert_eq!(expected, root);
}

#[test]
fn test_parse_let_plus_negation()
{
    let input = "
    let x := 4 + 8
    ~x
    ";
    let root = ast::parse(lex(input));

    let expected = sxpr::new_block(
        list::cons(sxpr::new(SxprType::Let,
            list::cons(Val::id("x".to_string()),
            list::cons(sxpr::call(Val::id("int_add".to_string()),
                list::from2(Val::Int(4), Val::Int(8)),
                ),
            Val::Nil,
            )),
        ),
        list::cons(sxpr::call(Val::id("int_negate".to_string()),
            list::singleton(Val::id("x".to_string())),
            ),
        Val::Nil,
        ))
    );

    assert_eq!(expected, root);
}

#[test]
fn test_parse_let_plus_tuple()
{
    let input = "
    let x := 4 + y
    (x, z)
    ";
    let root = ast::parse(lex(input));

    let expected = sxpr::new_block(
        list::cons(sxpr::new(SxprType::Let,
            list::cons(Val::id("x".to_string()),
            list::cons(sxpr::call(Val::id("int_add".to_string()),
                list::from2(Val::Int(4), Val::id("y".to_string())),
                ),
            Val::Nil,
            ))
        ),
        list::cons(Val::Tuple(vec![
            Val::id("x".to_string()),
            Val::id("z".to_string()),
            ]),
        Val::Nil,
        ))
    );

    assert_eq!(expected, root);
}

#[test]
fn test_parse_function_hashtag_tuple()
{
    let input = "
    func foo(input: [(Int, #)]): [#] ->
        [#tacos, #burritos]
    --
    ";
    let root = ast::parse(lex(input));

    assert!(true); // didn't panic!
}

}
