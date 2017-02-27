use leema::reg::{Reg, RegTable};
use leema::val::{Val, Type};
use leema::log;
use leema::iexpr::{Iexpr, Source};
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
#[derive(PartialEq)]
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
    ListCons(Reg, Reg, Reg),
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

#[derive(Debug)]
struct Oxpr
{
    ops: Vec<Op>,
    dst: Reg,
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

pub fn make_ops(input: &Iexpr) -> OpVec
{
    let mut regtbl = RegTable::new();
    let mut ops = make_sub_ops(&mut regtbl, input);
    if input.typ != Type::Void {
        ops.push(Op::SetResult(Reg::local(0)));
    }
    ops.push(Op::Return);
    ops
}

pub fn make_sub_ops(rt: &mut RegTable, input: &Iexpr) -> Oxpr
{
    match input.src {
        Source::Block(ref lines) => {
            let mut ops = vec![];
            for i in lines.iter().rev() {
                ops.append(&mut make_sub_ops(rt, i));
            }
            ops.reverse();
            ops
        }
        Source::ConstVal(Val::CallParams) => {
            // call params are already there, noop this
            vec![]
        }
        Source::ConstVal(ref v) => {
            vec![Op::ConstVal(rt.dst(), v.clone())]
        }
        Source::Fail(ref tag, ref msg) => {
            let mut ops = vec![];
            let result_reg = rt.dst();
            ops.append(&mut make_sub_ops(rt, tag));
            ops.append(&mut make_sub_ops(rt, msg));
            let failop = Op::Failure(
                result_reg.clone(),
                Reg::Undecided,
                Reg::Undecided,
            );
            ops.push(failop);
            ops.push(Op::SetResult(result_reg));
            ops.push(Op::Return);
            ops
        }
        Source::FieldAccess(ref base, ref sub) => {
            let mut base_ops = make_sub_ops(rt, base);
            base_ops.push(Op::Copy(Reg::Undecided, Reg::Undecided));
            base_ops
        }
        Source::Func(ref body) => {
            make_sub_ops(rt, &body)
        }
        Source::Call(ref f, ref args) => {
            make_call_ops(rt, f, args)
        }
        Source::Constructor(ref typ) => {
            vout!("make_constructor_ops({:?})\n", input);
            make_constructor_ops(rt, &Reg::Undecided, typ)
        }
        Source::Fork(ref dst, ref f, ref args) => {
            // make_fork_ops(rt, f, args)
            Oxpr{ ops: vec![], dst: Reg::Undecided }
        }
        Source::Let(ref patt, ref x) => {
            let pval = assign_pattern_registers(rt, patt);
            make_sub_ops(rt, x)
        }
        Source::MatchExpr(ref x, ref cases) => {
            make_matchexpr_ops(rt, &*x, &*cases)
        }
        Source::MatchCase(ref patt, ref code, ref next) => {
            panic!("matchcase ops not generated directly");
        }
        Source::Id(ref id) => {
            let src = rt.id(id);
            let mut idops = vec![];
            if src != rt.dst() {
                idops.push(Op::Copy(rt.dst(), src));
            }
            Oxpr{ ops: idops, dst: src }
        }
        Source::IfExpr(ref test, ref truth, ref lies) => {
            make_if_ops(rt, &*test, &*truth, &*lies)
        }
        Source::StrMash(ref items) => {
            make_str_ops(rt, items)
        }
        Source::Tuple(ref items) => {
            let newtup = Op::TupleCreate(
                rt.dst(),
                items.len() as i8,
                );
            let mut ops = vec![newtup];
            for i in items {
                ops.append(&mut make_sub_ops(rt, i));
            }
            ops
        }
        Source::List(ref items) => {
            make_list_ops(rt, items)
        }
        Source::BooleanAnd(ref a, ref b) => {
            panic!("maybe AND should just be a macro");
        }
        Source::BooleanOr(ref a, ref b) => {
            panic!("maybe OR should just be a macro");
        }
        Source::ModuleAccess(ref module, ref name) => {
            panic!("Module access not supported yet: {:?}.{:?}", module, name);
        }
        Source::Return(ref result) => {
            let mut rops = make_sub_ops(rt, result);
            rops.push(Op::SetResult(Reg::Undecided));
            rops.push(Op::Return);
            rops
        }
        Source::RustBlock => {
            panic!("Whoa how to make a RustBlock ops?");
        }
    }
}

pub fn make_call_ops(rt: &mut RegTable, f: &Iexpr, args: &Iexpr) -> Oxpr
{
    let dst = rt.dst();

    let freg = rt.push_dst();
    let mut ops = make_sub_ops(rt, f);

    let argsreg = rt.push_dst();
    ops.append(&mut make_sub_ops(rt, args));
    ops.push(Op::ApplyFunc(dst, freg, argsreg));

    rt.pop_dst();
    rt.pop_dst();
    ops
}

pub fn make_constructor_ops(rt: &mut RegTable, dst: &Reg, typ: &Type) -> OpVec
{
    let mut ops = vec![];
    if let &Type::Struct(_, nfields) = typ {
        ops.push(Op::Constructor(dst.clone(), typ.clone()));
        let mut i = 0;
        while i < nfields {
            ops.push(Op::Copy(dst.sub(i), Reg::param(i)));
            i += 1;
        }
    } else {
        panic!("Cannot construct a not type");
    }
    ops
}

pub fn make_matchexpr_ops(rt: &mut RegTable, x: &Iexpr, cases: &Iexpr) -> OpVec
{
vout!("make_matchexpr_ops({:?},{:?})", x, cases);
    let mut x_ops = match Reg::Undecided {
        Reg::Params => {
            // this means its a match function
            // nothing to be done here
            vec![]
        }
        _ => {
            make_sub_ops(rt, &x)
        }
    };
    vout!("call make_matchcase_ops()\n");
    let mut case_ops = make_matchcase_ops(rt, cases, &Reg::Undecided);
    vout!("made matchcase_ops =\n{:?}\n", case_ops);

    x_ops.append(&mut case_ops);
    x_ops
}

pub fn make_matchcase_ops(rt: &mut RegTable, matchcase: &Iexpr, xreg: &Reg
            ) -> OpVec
{
    let (patt, code, next) = match matchcase.src {
        Source::MatchCase(ref patt, ref code, ref next) => (patt, code, next),
        Source::ConstVal(Val::Void) => {
            // this is here when there's no else case
            vout!("empty_matchcase_ops\n");
            return vec![];
        }
        _ => {
            panic!("Cannot make ops for a not MatchCase {:?}", matchcase);
        }
    };
vout!("make_matchcase_ops({:?},{:?},{:?})\n", patt, code, next);
    let mut patt_ops = make_pattern_ops(rt, patt);
    let mut code_ops = make_sub_ops(rt, code);
    let mut next_ops = make_matchcase_ops(rt, next, &xreg);

    code_ops.push(Op::Jump((next_ops.len() + 1) as i16));
    patt_ops.push(Op::MatchPattern(
        (code_ops.len() + 1) as i16,
        Reg::Undecided,
        xreg.clone(),
    ));

    patt_ops.append(&mut code_ops);
    patt_ops.append(&mut next_ops);
    patt_ops
}

pub fn assign_pattern_registers(rt: &mut RegTable, pattern: &Val) -> Val
{
    match pattern {
        &Val::Id(ref id) => {
            Val::PatternVar(rt.id(id))
        }
        _ => {
            panic!("pattern type unsupported: {:?}", pattern);
        }
    }
}

pub fn make_pattern_ops(rt: &mut RegTable, pattern: &Iexpr) -> OpVec
{
    let mut ops = vec![];
    let pdst = Reg::Undecided;
    match &pattern.src {
        &Source::ConstVal(ref v) => {
            ops.push(Op::ConstVal(pdst, v.clone()));
        }
        &Source::Id(ref dst) => {
            ops.push(Op::ConstVal(pdst, Val::PatternVar(Reg::Undecided)));
        }
        &Source::Tuple(ref items) => {
            ops.push(Op::TupleCreate(pdst, items.len() as i8));
            for i in items {
                let mut item_ops = make_pattern_ops(rt, i);
                ops.append(&mut item_ops);
            }
        }
        _ => {
            panic!("That's not a pattern! {:?}", pattern);
        }
    }
    ops
}

pub fn make_case_ops(rt: &mut RegTable, test: &Iexpr, truth: &Iexpr, lies: &Iexpr) -> OpVec
{
vout!("make_case_ops({:?},{:?},{:?})\n", test, truth, lies);
    let mut case_ops = make_sub_ops(rt, &test);
    let mut truth_ops = make_sub_ops(rt, &truth);
    let mut lies_ops = make_sub_ops(rt, &lies);

    truth_ops.push(Op::Jump((lies_ops.len() + 1) as i16));
    case_ops.push(
        Op::JumpIfNot(
            (truth_ops.len() + 1) as i16,
            Reg::Undecided,
        )
    );

    case_ops.append(&mut truth_ops);
    case_ops.append(&mut lies_ops);
    case_ops
}

pub fn make_if_ops(rt: &mut RegTable, test: &Iexpr, truth: &Iexpr, lies: &Iexpr) -> Oxpr
{
vout!("make_if_ops({:?},{:?},{:?})\n", test, truth, lies);
    rt.push_dst();
    let mut if_ops = make_sub_ops(rt, &test);
    rt.pop_dst();
    let mut truth_ops = make_sub_ops(rt, &truth);
    let mut lies_ops = make_sub_ops(rt, &lies);

    truth_ops.ops.push(Op::Jump((lies_ops.ops.len() + 1) as i16));
    if_ops.ops.push(
        Op::JumpIfNot((truth_ops.ops.len() + 1) as i16, if_ops.dst)
    );

    if_ops.ops.append(&mut truth_ops.ops);
    if_ops.ops.append(&mut lies_ops.ops);
    Oxpr{
        ops: if_ops.ops,
        dst: truth_ops.dst,
    }
}

/*
pub fn make_fork_ops(rt: &mut RegTable, dst: &Reg, f: &Iexpr, args: &Iexpr) -> Oxpr
{
    println!("make_fork_ops({:?}, {:?}, {:?})", dst, f, args);
    let mut ops = make_sub_ops(rt, f);
    ops.append(&mut make_sub_ops(rt, args));
    ops.push(Op::Fork(dst.clone(), Reg::Undecided, Reg::Undecided));
    ops
}
*/

pub fn make_list_ops(rt: &mut RegTable, items: &Vec<Iexpr>) -> Oxpr
{
    let dst = rt.dst();
    let mut ops = vec![Op::ListCreate(dst.clone())];
    rt.push_dst();
    for i in items.iter().rev() {
        let mut listops = make_sub_ops(rt, i);
        ops.append(&mut listops.ops);
        ops.push(Op::ListCons(dst.clone(), listops.dst, dst.clone()));
    }
    rt.pop_dst();
    Oxpr{ ops: ops, dst: dst }
}

pub fn make_str_ops(rt: &mut RegTable, items: &Vec<Iexpr>) -> Oxpr
{
    let dst = rt.dst();
    let mut ops = vec![
        Op::ConstVal(
            dst.clone(),
            Val::Str(Arc::new("".to_string())),
        ),
    ];
    let tmp = rt.push_dst();
    for i in items {
        let strops = &mut make_sub_ops(rt, i);
        ops.append(&mut strops.ops);
        ops.push(Op::StrCat(dst.clone(), tmp.clone()));
    }
    rt.pop_dst();
    Oxpr{ ops: ops, dst: dst }
}


#[cfg(test)]
mod tests {
    use leema::code::{self, Op};
    use leema::iexpr::{Iexpr};
    use leema::reg::{Reg};
    use leema::val::{Val};

#[test]
fn test_code_constval()
{
    let ic = Iexpr::const_val(Val::Int(9));
    let code = code::make_ops(&ic);

    assert_eq!(3, code.len());
    let x = vec![
        Op::ConstVal(Reg::local(0), Val::Int(9)),
        Op::SetResult(Reg::local(0)),
        Op::Return,
    ];
    assert_eq!(x, code);
}

}
