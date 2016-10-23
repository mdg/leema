use leema::reg::{Reg};
use leema::val::{Val, Type};
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

impl fmt::Display for ModSym
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match *self {
            ModSym(ref module, ref symbol) => {
                write!(f, "{}::{}", module, symbol)
            }
        }
    }
}

#[derive(Debug)]
#[derive(Clone)]
pub enum Op
{
    LoadFunc(Reg, ModSym),
    ApplyFunc(Reg, Reg, Reg),
    Return,
    SetResult(Reg),
    ConstVal(Reg, Val),
    Constructor(Reg, Type),
    Copy(Reg, Reg),
    Failure(Reg, Reg, Reg),
    Fork(Reg, Reg, Reg),
    //IfFail(Reg, i16),
    Jump(i16),
    JumpIfNot(i16, Reg),
    // jump if no match, pattern reg, input reg
    MatchPattern(i16, Reg, Reg),
    ListCons(Reg, Reg),
    ListCreate(Reg),
    StrCat(Reg, Reg),
    TupleCreate(Reg, i8),
}

pub type OpVec = Vec<Op>;

impl Op
{
    pub fn print_list(ops: &OpVec)
    {
        for op in ops {
            println!("{:?}", op);
        }
    }
}


pub type RustFunc = fn(&mut frame::Frame) -> ();

trait RustFunc2
{
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
    let mut ops = make_sub_ops(input);
    if input.typ != Type::Void {
        ops.push(Op::SetResult(input.dst.clone()));
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
        Source::ConstVal(Val::CallParams) => {
            // call params are already there, noop this
            vec![]
        }
        Source::ConstVal(ref v) => {
            vec![Op::ConstVal(input.dst.clone(), v.clone())]
        }
        Source::BoundVal(ref src) => {
            // panic!("{:?} shouldn't be here", input);
            // shouldn't have to do anything here, should
            // just use the dst reg
            vec![Op::Copy(input.dst.clone(), src.clone())]
        }
        Source::Fail(ref tag, ref msg) => {
            let mut ops = vec![];
            ops.append(&mut make_sub_ops(tag));
            ops.append(&mut make_sub_ops(msg));
            let failop = Op::Failure(
                input.dst.clone(),
                tag.dst.clone(),
                msg.dst.clone(),
            );
            ops.push(failop);
            ops.push(Op::SetResult(input.dst.clone()));
            ops.push(Op::Return);
            ops
        }
        Source::FieldAccess(ref base, subreg) => {
            let mut base_ops = make_sub_ops(base);
            base_ops.push(Op::Copy(input.dst.clone(), base.dst.sub(subreg)));
            base_ops
        }
        Source::Call(ref f, ref args) => {
            make_call_ops(&input.dst, f, args)
        }
        Source::Constructor(ref typ) => {
            vout!("make_constructor_ops({:?})\n", input);
            make_constructor_ops(&input.dst, typ)
        }
        Source::Fork(ref f, ref args) => {
            make_fork_ops(&input.dst, f, args)
        }
        Source::MatchExpr(ref x, ref cases) => {
            make_matchexpr_ops(&*x, &*cases)
        }
        Source::MatchCase(ref patt, ref code, ref next) => {
            panic!("matchcase ops not generated directly");
        }
        Source::PatternVar(_) => {
            panic!("PatternVar ops not generated directly");
        }
        Source::CaseExpr(ref test, ref truth, ref lies) => {
            make_case_ops(&*test, &*truth, &*lies)
        }
        Source::IfStmt(ref test, ref truth, ref lies) => {
            make_if_ops(&*test, &*truth, &*lies)
        }
        Source::Str(ref items) => {
            make_str_ops(&input.dst, items)
        }
        Source::Tuple(ref items) => {
            let newtup = Op::TupleCreate(
                input.dst.clone(),
                items.len() as i8,
                );
            let mut ops = vec![newtup];
            for i in items {
                ops.append(&mut make_sub_ops(i));
            }
            ops
        }
        Source::List(ref items) => {
            make_list_ops(&input.dst, items)
        }
        Source::BooleanAnd(ref a, ref b) => {
            panic!("maybe AND should just be a macro");
        }
        Source::BooleanOr(ref a, ref b) => {
            panic!("maybe OR should just be a macro");
        }
        Source::Return(ref result) => {
            let mut rops = make_sub_ops(result);
            rops.push(Op::SetResult(result.dst.clone()));
            rops.push(Op::Return);
            rops
        }
        Source::Void => {
            // blank, skip it
            vec![]
        }
    }
}

pub fn make_call_ops(dst: &Reg, f: &Iexpr, args: &Iexpr) -> OpVec
{
    //println!("make_call_ops {:?}({:?})", f, args);
    let freg = f.dst.clone();
    let argsreg = args.dst.clone();
    let mut ops = make_sub_ops(f);
    ops.append(&mut make_sub_ops(args));
    ops.push(Op::ApplyFunc(dst.clone(), freg, argsreg));
    ops
}

pub fn make_constructor_ops(dst: &Reg, typ: &Type) -> OpVec
{
    let mut ops = vec![];
    if let &Type::Struct(_, nfields) = typ {
        ops.push(Op::Constructor(dst.clone(), typ.clone()));
        let mut i = 0;
        while i < nfields {
            ops.push(Op::Copy(dst.sub(i), Reg::new_param(i)));
            i += 1;
        }
    } else {
        panic!("Cannot construct a not type");
    }
    ops
}

pub fn make_matchexpr_ops(x: &Iexpr, cases: &Iexpr) -> OpVec
{
vout!("make_matchexpr_ops({:?},{:?})", x, cases);
    let mut x_ops = match x.dst {
        Reg::Params => {
            // this means its a match function
            // nothing to be done here
            vec![]
        }
        _ => {
            make_sub_ops(&x)
        }
    };
    vout!("call make_matchcase_ops()\n");
    let mut case_ops = make_matchcase_ops(cases, &x.dst);
    vout!("made matchcase_ops =\n{:?}\n", case_ops);

    x_ops.append(&mut case_ops);
    x_ops
}

pub fn make_matchcase_ops(matchcase: &Iexpr, xreg: &Reg) -> OpVec
{
    let (patt, code, next) = match matchcase.src {
        Source::MatchCase(ref patt, ref code, ref next) => (patt, code, next),
        Source::ConstVal(Val::Void) => {
            // this is here when there's no else case
            vout!("empty_matchcase_ops\n");
            return vec![];
        }
        Source::Void => {
            // this is here when there's no else case
            vout!("empty_matchcase_ops\n");
            return vec![];
        }
        _ => {
            panic!("Cannot make ops for a not MatchCase {:?}", matchcase);
        }
    };
vout!("make_matchcase_ops({:?},{:?},{:?})\n", patt, code, next);
    let mut patt_ops = make_pattern_ops(patt);
    let mut code_ops = make_sub_ops(code);
    let mut next_ops = make_matchcase_ops(next, &xreg);

    code_ops.push(Op::Jump((next_ops.len() + 1) as i16));
    patt_ops.push(Op::MatchPattern(
        (code_ops.len() + 1) as i16,
        patt.dst.clone(),
        xreg.clone(),
    ));

    patt_ops.append(&mut code_ops);
    patt_ops.append(&mut next_ops);
    patt_ops
}

pub fn make_pattern_ops(pattern: &Iexpr) -> OpVec
{
    let mut ops = vec![];
    let pdst = pattern.dst.clone();
    match &pattern.src {
        &Source::ConstVal(ref v) => {
            ops.push(Op::ConstVal(pdst, v.clone()));
        }
        &Source::PatternVar(ref dst) => {
            ops.push(Op::ConstVal(pdst, Val::PatternVar(dst.clone())));
        }
        &Source::Tuple(ref items) => {
            ops.push(Op::TupleCreate(pdst, items.len() as i8));
            for i in items {
                let mut item_ops = make_pattern_ops(i);
                ops.append(&mut item_ops);
            }
        }
        _ => {
            panic!("That's not a pattern! {:?}", pattern);
        }
    }
    ops
}

pub fn make_case_ops(test: &Iexpr, truth: &Iexpr, lies: &Iexpr) -> OpVec
{
vout!("make_case_ops({:?},{:?},{:?})\n", test, truth, lies);
    let mut case_ops = make_sub_ops(&test);
    let mut truth_ops = make_sub_ops(&truth);
    let mut lies_ops = make_sub_ops(&lies);

    truth_ops.push(Op::Jump((lies_ops.len() + 1) as i16));
    case_ops.push(
        Op::JumpIfNot((truth_ops.len() + 1) as i16,
        test.dst.clone())
    );

    case_ops.append(&mut truth_ops);
    case_ops.append(&mut lies_ops);
    case_ops
}

pub fn make_if_ops(test: &Iexpr, truth: &Iexpr, lies: &Iexpr) -> OpVec
{
vout!("make_if_ops({:?},{:?},{:?})\n", test, truth, lies);
    let mut if_ops = make_sub_ops(&test);
    let mut truth_ops = make_sub_ops(&truth);
    let mut lies_ops = make_sub_ops(&lies);

    truth_ops.push(Op::Jump((lies_ops.len() + 1) as i16));
    if_ops.push(Op::JumpIfNot((truth_ops.len() + 1) as i16, test.dst.clone()));

    if_ops.append(&mut truth_ops);
    if_ops.append(&mut lies_ops);
    if_ops
}


pub fn make_fork_ops(dst: &Reg, f: &Iexpr, args: &Iexpr) -> OpVec
{
    println!("make_fork_ops({:?}, {:?}, {:?})", dst, f, args);
    let mut ops = make_sub_ops(f);
    ops.append(&mut make_sub_ops(args));
    ops.push(Op::Fork(dst.clone(), f.dst.clone(), args.dst.clone()));
    ops
}

pub fn make_list_ops(dst: &Reg, items: &Vec<Iexpr>) -> OpVec
{
    let mut ops = vec![Op::ListCreate(dst.clone())];
    for i in items.iter().rev() {
        ops.push(Op::ListCons(dst.clone(), dst.clone()));
        ops.append(&mut make_sub_ops(i));
    }
    ops
}

pub fn make_str_ops(dst: &Reg, items: &Vec<Iexpr>) -> OpVec
{
    let mut ops = vec![
        Op::ConstVal(
            dst.clone(),
            Val::Str(Arc::new("".to_string())),
        ),
    ];
    for i in items {
        ops.append(&mut make_sub_ops(i));
        ops.push(Op::StrCat(dst.clone(), i.dst.clone()));
    }
    ops
}
