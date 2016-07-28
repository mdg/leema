use leema::reg::{Reg};
use leema::val::{Val};
use leema::log;
use leema::compile::{Iexpr,Source};
use leema::frame;
use std::fmt;
use std::collections::{HashMap};
use std::io::{stderr, Write};
use std::sync::Arc;


#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Eq)]
#[derive(PartialOrd)]
#[derive(Ord)]
#[derive(Clone)]
pub struct ModSym (pub String, pub String);

impl fmt::Display for ModSym {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ModSym(ref module, ref symbol) => {
                write!(f, "{}::{}", module, symbol)
            }
        }
    }
}

#[derive(Debug)]
#[derive(Clone)]
pub enum Op {
    LoadFunc(Reg, ModSym),
    ApplyFunc(Reg, Reg, Reg),
    Return,
    ConstVal(Reg, Val),
    Copy(Reg, Reg),
    Fork(Reg, Reg, Reg),
    //IfFail(Reg, i16),
    Jump(i16),
    JumpIfNot(i16, Reg),
    ListCons(Reg, Reg),
    ListCreate(Reg),
    StrCat(Reg, Reg),
    TupleCreate(Reg, i8),
}

pub type OpVec = Vec<Op>;

impl Op {
    pub fn print_list(ops: &OpVec)
    {
        for op in ops {
            println!("{:?}", op);
        }
    }
}


pub type RustFunc = fn(&mut frame::Frame) -> ();

trait RustFunc2 {
    fn call(&mut self, env: &mut frame::Frame) -> ();
}

pub enum Code
{
    Leema(Arc<OpVec>),
    Rust(RustFunc),
    Inter(Arc<Iexpr>),
}

impl fmt::Debug for Code
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Code::Leema(ref ops) => {
                let mut result;
                result = write!(f, "Code::Leema");
                for op in &**ops {
                    result = write!(f, "  {:?}\n", op);
                }
                result
            }
            &Code::Rust(_) => {
                write!(f, "Code::Rust")
            }
            &Code::Inter(ref ix) => {
                write!(f, "Code::Inter({:?})", ix)
            }
        }
    }
}

impl Clone for Code
{
    fn clone(&self) -> Code
    {
        match self {
            &Code::Leema(ref ops) => Code::Leema(ops.clone()),
            &Code::Rust(rf) => Code::Rust(rf),
            &Code::Inter(ref ix) => Code::Inter(ix.clone()),
        }
    }
}

#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
#[derive(Hash)]
#[derive(Eq)]
#[derive(Ord)]
pub enum CodeKey
{
    Main,
    Script,
    Name(Arc<String>),
    Repl(isize),
}

pub type CodeMap = HashMap<CodeKey, Code>;

/*
#[derive(Clone)]
#[derive(Debug)]
pub struct Lib
{
    book: HashMap<String, Code>,
    main: Option<Code>,
    ext: Option<Code>,
}

impl Lib
{
    fn new() -> Lib
    {
        Lib{
            book: HashMap::new(),
            main: None,
            ext: None,
        }
    }

    pub fn add(&self, name: String, c: Code)
    {
        match name {
            "main" => {
                self.main = Some(c);
            }
            "" => 
        }
    }
}
*/

pub fn make_ops(input: &Iexpr) -> OpVec
{
    let dst = input.dst;
    let mut ops = make_sub_ops(input);
    if dst != Reg::Result {
        ops.push(Op::Copy(Reg::Result, dst));
    }
    ops.push(Op::Return);
    ops
}

pub fn make_sub_ops(input: &Iexpr) -> OpVec
{
    match input.src {
        Source::Block(ref lines) => {
            let mut ops = vec![];
            for i in lines {
                ops.append(&mut make_sub_ops(i));
            }
            ops
        }
        Source::ConstVal(ref v) => {
            vec![Op::ConstVal(input.dst, v.clone())]
        }
        Source::BoundVal(src) => {
            // panic!("{:?} shouldn't be here", input);
            // shouldn't have to do anything here, should
            // just use the dst reg
            vec![Op::Copy(input.dst, src)]
        }
        Source::Call(ref f, ref args) => {
            make_call_ops(input.dst, f, args)
        }
        Source::DefineFunc(ref _name, ref code) => {
            make_ops(code)
        }
        Source::Fork(ref f, ref args) => {
            make_fork_ops(input.dst, f, args)
        }
        Source::CaseExpr(ref test, ref truth, ref lies) => {
            make_case_ops(&*test, &*truth, &*lies)
        }
        Source::IfStmt(ref test, ref truth, ref lies) => {
            make_if_ops(&*test, &*truth, &*lies)
        }
        Source::Str(ref items) => {
            make_str_ops(input.dst, items)
        }
        Source::Tuple(ref items) => {
            let newtup = Op::TupleCreate(
                input.dst,
                items.len() as i8,
                );
            let mut ops = vec![newtup];
            for i in items {
                ops.append(&mut make_sub_ops(i));
            }
            ops
        }
        Source::List(ref items) => {
            make_list_ops(input.dst, items)
        }
        Source::BooleanAnd(ref a, ref b) => {
            panic!("maybe AND should just be a macro");
        }
        Source::BooleanOr(ref a, ref b) => {
            panic!("maybe OR should just be a macro");
        }
        Source::Void => {
            // blank, skip it
            vec![]
        }
    }
}

pub fn make_call_ops(dst: Reg, f: &Iexpr, args: &Iexpr) -> OpVec
{
    //println!("make_call_ops {:?}({:?})", f, args);
    let freg = f.dst.clone();
    let argsreg = args.dst.clone();
    let mut ops = make_sub_ops(f);
    ops.append(&mut make_sub_ops(args));
    ops.push(Op::ApplyFunc(dst, freg, argsreg));
    ops
}

pub fn make_case_ops(test: &Iexpr, truth: &Iexpr, lies: &Iexpr) -> OpVec
{
verbose_out!("make_case_ops({:?},{:?},{:?})", test, truth, lies);
    let mut case_ops = make_sub_ops(&test);
    let mut truth_ops = make_sub_ops(&truth);
    let mut lies_ops = make_sub_ops(&lies);

    truth_ops.push(Op::Jump((lies_ops.len() + 1) as i16));
    case_ops.push(Op::JumpIfNot((truth_ops.len() + 1) as i16, test.dst));

    case_ops.append(&mut truth_ops);
    case_ops.append(&mut lies_ops);
    case_ops
}

pub fn make_if_ops(test: &Iexpr, truth: &Iexpr, lies: &Iexpr) -> OpVec
{
verbose_out!("make_if_ops({:?},{:?},{:?})", test, truth, lies);
    let mut if_ops = make_sub_ops(&test);
    let mut truth_ops = make_sub_ops(&truth);
    let mut lies_ops = make_sub_ops(&lies);

    truth_ops.push(Op::Jump((lies_ops.len() + 1) as i16));
    if_ops.push(Op::JumpIfNot((truth_ops.len() + 1) as i16, test.dst));

    if_ops.append(&mut truth_ops);
    if_ops.append(&mut lies_ops);
    // leave a void in the result to be sure it's not returning something else
    if_ops.push(Op::ConstVal(truth.dst, Val::Void));
    if_ops
}


pub fn make_fork_ops(dst: Reg, f: &Iexpr, args: &Iexpr) -> OpVec
{
    println!("make_fork_ops({:?}, {:?}, {:?})", dst, f, args);
    let freg = f.dst;
    let argsreg = args.dst;
    let mut ops = make_sub_ops(f);
    ops.append(&mut make_sub_ops(args));
    ops.push(Op::Fork(dst, freg, argsreg));
    ops
}

pub fn make_list_ops(dst: Reg, items: &Vec<Iexpr>) -> OpVec
{
    let mut ops = vec![Op::ListCreate(dst)];
    for i in items.iter().rev() {
        ops.push(Op::ListCons(dst, dst));
        ops.append(&mut make_sub_ops(i));
    }
    ops
}

pub fn make_str_ops(dst: Reg, items: &Vec<Iexpr>) -> OpVec
{
    let mut ops = vec![
        Op::ConstVal(
            dst,
            Val::Str(Arc::new("".to_string())),
        ),
    ];
    for i in items {
        let idst = i.dst;
        ops.append(&mut make_sub_ops(i));
        ops.push(Op::StrCat(dst, idst));
    }
    ops
}
