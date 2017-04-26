use leema::reg::{Reg, RegTable};
use leema::val::{Val, Type};
use leema::log;
use leema::ixpr::{Ixpr, Source};
use leema::frame;

use std::fmt;
use std::collections::{HashMap};
use std::io::{stderr, Write};
use std::sync::Arc;
use std::marker;

use tokio_core::reactor;


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
    MatchPattern(Reg, Val, Reg),
    ListCons(Reg, Reg, Reg),
    ListCreate(Reg),
    StrCat(Reg, Reg),
    TupleCreate(Reg, i8),
}

unsafe impl marker::Send for Op {}
unsafe impl marker::Sync for Op {}

impl Op
{
    pub fn print_list(ops: &OpVec)
    {
        for op in ops {
            println!("{:?}", op);
        }
    }
}

impl Clone for Op
{
    fn clone(&self) -> Op
    {
        match self {
            &Op::LoadFunc(ref r, ref ms) => Op::LoadFunc(r.clone(), ms.clone()),
            &Op::ApplyFunc(ref dst, ref f, ref args) => {
                Op::ApplyFunc(dst.clone(), f.clone(), args.clone())
            }
            &Op::Return => Op::Return,
            &Op::SetResult(ref src) => Op::SetResult(src.clone()),
            &Op::ConstVal(ref dst, ref src) => {
                Op::ConstVal(dst.clone(), src.deep_clone())
            }
            &Op::Constructor(ref dst, ref src) => {
                Op::Constructor(dst.clone(), src.deep_clone())
            }
            &Op::Copy(ref dst, ref src) => {
                Op::Copy(dst.clone(), src.clone())
            }
            &Op::Failure(ref dst, ref typ, ref msg) => {
                Op::Failure(dst.clone(), typ.clone(), msg.clone())
            }
            &Op::Jump(j) => Op::Jump(j),
            // &Op::Fork(r) => {}
            &Op::JumpIfNot(j, ref tst) => Op::JumpIfNot(j, tst.clone()),
            &Op::MatchPattern(ref dst, ref patt, ref input) => {
                Op::MatchPattern(dst.clone(), patt.deep_clone(), input.clone())
            }
            &Op::ListCons(ref dst, ref head, ref tail) => {
                Op::ListCons(dst.clone(), head.clone(), tail.clone())
            }
            &Op::ListCreate(ref dst) => Op::ListCreate(dst.clone()),
            &Op::StrCat(ref dst, ref src) => {
                Op::StrCat(dst.clone(), src.clone())
            }
            &Op::TupleCreate(ref dst, sz) => Op::TupleCreate(dst.clone(), sz),
            _ => {
                panic!("cannot clone op: {:?}", self);
            }
        }
    }
}

pub type OpVec = Vec<Op>;

#[derive(Debug)]
pub struct Oxpr
{
    ops: Vec<Op>,
    dst: Reg,
}


pub type RustFunc = fn(&mut frame::Frame) -> ();
pub type RustIoFunc = fn(&mut frame::Frame, &reactor::Handle) -> ();

trait RustFunc2
{
    fn call(&mut self, env: &mut frame::Frame) -> ();
}

pub enum Code
{
    Leema(OpVec),
    Rust(RustFunc),
    RustIo(RustIoFunc),
}

impl Code
{
    pub fn type_name(&self) -> &'static str
    {
        match self {
            &Code::Leema(_) => "LeemaCode",
            &Code::Rust(_) => "RustCode",
            &Code::RustIo(_) => "RustIo",
        }
    }
}

impl fmt::Display for Code
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self {
            &Code::Leema(_) => write!(f, "LeemaCode"),
            &Code::Rust(_) => write!(f, "RustCode"),
            &Code::RustIo(_) => write!(f, "RustIo"),
        }
    }
}

impl fmt::Debug for Code
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Code::Leema(ref ops) => {
                let mut result;
                result = write!(f, "Code::Leema\n");
                let mut i = 0;
                for op in &**ops {
                    result = write!(f, "{:3} {:?}\n", i, op);
                    i += 1;
                }
                result
            }
            &Code::Rust(_) => {
                write!(f, "Code::Rust")
            }
            &Code::RustIo(_) => {
                write!(f, "Code::RustIo")
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
            &Code::RustIo(rf) => Code::RustIo(rf),
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

pub fn make_ops(input: &Ixpr) -> OpVec
{
    vout!("make_ops({:?})\n", input);
    let mut regtbl = RegTable::new();
    let mut ops = make_sub_ops(&mut regtbl, input);
    if input.typ != Type::Void {
        ops.ops.push(Op::SetResult(Reg::local(0)));
    }
    ops.ops.push(Op::Return);
    ops.ops
}

pub fn make_sub_ops(rt: &mut RegTable, input: &Ixpr) -> Oxpr
{
    match input.src {
        Source::Block(ref lines) => {
            let mut oxprs = vec![];
            for i in lines.iter().rev() {
                oxprs.push(make_sub_ops(rt, i));
            }
            let mut ops = vec![];
            let mut last_dst = rt.dst().clone();
            for mut i in oxprs.iter_mut().rev() {
                ops.append(&mut i.ops);
                last_dst = i.dst.clone();
            }
            if *rt.dst() != last_dst {
                ops.push(Op::Copy(rt.dst().clone(), last_dst));
            }
            Oxpr{
                ops: ops,
                dst: rt.dst().clone(),
            }
        }
        Source::ConstVal(Val::CallParams) => {
            // call params are already there, noop this
            Oxpr{
                ops: vec![],
                dst: Reg::Params,
            }
        }
        Source::ConstVal(ref v) => {
            let dst = rt.dst();
            Oxpr{
                ops: vec![Op::ConstVal(dst.clone(), v.clone())],
                dst: dst.clone(),
            }
        }
        Source::Fail(ref tag, ref msg) => {
            let result_reg = rt.dst().clone();
            let mut ops = vec![];
            let mut tag_ops = make_sub_ops(rt, tag);
            let mut msg_ops = make_sub_ops(rt, msg);
            ops.append(&mut tag_ops.ops);
            ops.append(&mut msg_ops.ops);
            let failop = Op::Failure(
                result_reg.clone(),
                tag_ops.dst,
                msg_ops.dst,
            );
            ops.push(failop);
            ops.push(Op::SetResult(result_reg.clone()));
            ops.push(Op::Return);
            Oxpr{
                ops: ops,
                dst: result_reg,
            }
        }
        Source::FieldAccess(ref base, ref sub) => {
            let mut base_ops = make_sub_ops(rt, base);
            // base_ops.push(Op::Copy(Reg::Undecided, Reg::Undecided));
            // TODO: find the subreg index for the field name
            base_ops
        }
        Source::Func(ref argnames, ref body) => {
            rt.def_args(argnames);
            make_sub_ops(rt, &body)
        }
        Source::Call(ref f, ref args) => {
            make_call_ops(rt, f, args)
        }
        Source::Constructor(ref typ) => {
            vout!("make_constructor_ops({:?})\n", input);
            make_constructor_ops(rt, typ)
        }
        Source::Fork(ref dst, ref f, ref args) => {
            // make_fork_ops(rt, f, args)
            Oxpr{ ops: vec![], dst: Reg::Undecided }
        }
        Source::Let(ref patt, ref x) => {
            let pval = assign_pattern_registers(rt, patt);
            let mut xops = make_sub_ops(rt, x);
            xops.ops.push(Op::MatchPattern(rt.dst().clone(), pval, xops.dst));
            Oxpr{ ops: xops.ops, dst: rt.dst().clone() }
        }
        Source::MatchExpr(ref x, ref cases) => {
            make_matchexpr_ops(rt, &*x, &*cases)
        }
        Source::MatchCase(ref patt, ref code, ref next) => {
            panic!("matchcase ops not generated directly");
        }
        Source::Id(ref id) => {
            let src = rt.id(id);
vout!("id({}).reg = {:?}\n", id, src);
            Oxpr{ ops: vec![], dst: src }
        }
        Source::IfExpr(ref test, ref truth, ref lies) => {
            make_if_ops(rt, &*test, &*truth, &*lies)
        }
        Source::StrMash(ref items) => {
            make_str_ops(rt, items)
        }
        Source::Tuple(ref items) => {
            let dst = rt.dst().clone();
            let newtup = Op::TupleCreate(
                dst.clone(),
                items.len() as i8,
                );
            let mut ops = vec![newtup];
            rt.push_sub();
            for i in items {
                // set dst to dst.child
                let mut iops = make_sub_ops(rt, i);
                ops.append(&mut iops.ops);
                if *rt.dst() != iops.dst {
                    ops.push(Op::Copy(rt.dst().clone(), iops.dst));
                }
                rt.next_sub();
            }
            rt.pop_dst();
            Oxpr{
                ops: ops,
                dst: dst,
            }
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
            let modval = Val::Str(module.clone());
            let idval = Val::Str(name.clone());
            let modname = Val::Tuple(vec![modval, idval]);
            let dst = rt.dst();
            Oxpr{
                ops: vec![Op::ConstVal(dst.clone(), modname)],
                dst: dst.clone(),
            }
        }
        Source::Return(ref result) => {
            let mut rops = make_sub_ops(rt, result);
            rops.ops.push(Op::SetResult(Reg::Undecided));
            rops.ops.push(Op::Return);
            rops
        }
        Source::RustBlock => {
            Oxpr{
                ops: vec![],
                dst: rt.dst().clone(),
            }
        }
    }
}

pub fn make_call_ops(rt: &mut RegTable, f: &Ixpr, args: &Ixpr) -> Oxpr
{
    let dst = rt.dst().clone();
    vout!("make_call_ops: {:?} = {:?}\n", dst, f);

    rt.push_dst();
    let mut fops = make_sub_ops(rt, f);

    rt.push_dst();
    let mut argops = make_sub_ops(rt, args);
    fops.ops.append(&mut argops.ops);
    fops.ops.push(Op::ApplyFunc(dst.clone(), fops.dst.clone(), argops.dst));

    rt.pop_dst();
    rt.pop_dst();
    fops.dst = dst;
    fops
}

pub fn make_constructor_ops(rt: &mut RegTable, typ: &Type) -> Oxpr
{
    let dst = rt.dst();
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
    Oxpr{
        ops: ops,
        dst: dst.clone(),
    }
}

pub fn make_matchexpr_ops(rt: &mut RegTable, x: &Ixpr, cases: &Ixpr) -> Oxpr
{
vout!("make_matchexpr_ops({:?},{:?})", x, cases);
    rt.push_dst();
    let mut xops = make_sub_ops(rt, &x);
    rt.pop_dst();

    let mut case_ops = make_matchcase_ops(rt, cases, &xops.dst);
    vout!("made matchcase_ops =\n{:?}\n", case_ops);

    xops.ops.append(&mut case_ops.ops);
    Oxpr{
        ops: xops.ops,
        dst: rt.dst().clone(),
    }
}

pub fn make_matchcase_ops(rt: &mut RegTable, matchcase: &Ixpr, xreg: &Reg
            ) -> Oxpr
{
    let (patt, code, next) = match matchcase.src {
        Source::MatchCase(ref patt, ref code, ref next) => (patt, code, next),
        Source::ConstVal(Val::Void) => {
            // this is here when there's no else case
            vout!("empty_matchcase_ops\n");
            return Oxpr{
                ops: vec![],
                dst: Reg::Void,
            };
        }
        _ => {
            panic!("Cannot make ops for a not MatchCase {:?}", matchcase);
        }
    };
    // push reg scope
    let patt_val = assign_pattern_registers(rt, patt);
    let mut code_ops = make_sub_ops(rt, code);
    // pop reg scope
    let mut next_ops = make_matchcase_ops(rt, next, &xreg);

    let next_len = next_ops.ops.len();
    if next_len > 0 {
        code_ops.ops.push(Op::Jump((next_len + 1) as i16));
    }
    rt.push_dst();
    let mut patt_ops = vec![Op::MatchPattern(
        rt.dst().clone(),
        patt_val,
        xreg.clone(),
    )];
    patt_ops.push(Op::JumpIfNot(
        code_ops.ops.len() as i16 + 1,
        rt.dst().clone(),
    ));
    rt.pop_dst();

    patt_ops.append(&mut code_ops.ops);
    patt_ops.append(&mut next_ops.ops);
    Oxpr{
        ops: patt_ops,
        dst: Reg::Undecided,
    }
}

pub fn assign_pattern_registers(rt: &mut RegTable, pattern: &Val) -> Val
{
    match pattern {
        &Val::Id(ref id) => {
            let id_reg = rt.id(id);
            vout!("pattern var:reg is {}.{:?}\n", id, id_reg);
            Val::PatternVar(id_reg)
        }
        &Val::Int(i) => Val::Int(i),
        &Val::Bool(b) => Val::Bool(b),
        &Val::Str(ref s) => Val::Str(s.clone()),
        &Val::Hashtag(ref h) => Val::Hashtag(h.clone()),
        &Val::Wildcard => Val::Wildcard,
        &Val::Nil => Val::Nil,
        &Val::Cons(ref head, ref tail) => {
            let pr_head = assign_pattern_registers(rt, head);
            let pr_tail = assign_pattern_registers(rt, tail);
            Val::Cons(Box::new(pr_head), Box::new(pr_tail))
        }
        &Val::Tuple(ref items) => {
            let reg_items = items.iter().map(|p| {
                assign_pattern_registers(rt, p)
            }).collect();
            Val::Tuple(reg_items)
        }
        _ => {
            panic!("pattern type unsupported: {:?}", pattern);
        }
    }
}

pub fn make_if_ops(rt: &mut RegTable, test: &Ixpr, truth: &Ixpr, lies: &Ixpr) -> Oxpr
{
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
pub fn make_fork_ops(rt: &mut RegTable, dst: &Reg, f: &Ixpr, args: &Ixpr) -> Oxpr
{
    println!("make_fork_ops({:?}, {:?}, {:?})", dst, f, args);
    let mut ops = make_sub_ops(rt, f);
    ops.append(&mut make_sub_ops(rt, args));
    ops.push(Op::Fork(dst.clone(), Reg::Undecided, Reg::Undecided));
    ops
}
*/

pub fn make_list_ops(rt: &mut RegTable, items: &Vec<Ixpr>) -> Oxpr
{
    let dst = rt.dst().clone();
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

pub fn make_str_ops(rt: &mut RegTable, items: &Vec<Ixpr>) -> Oxpr
{
    let dst = rt.dst().clone();
    let mut ops = vec![
        Op::ConstVal(
            dst.clone(),
            Val::empty_str(),
        ),
    ];
    rt.push_dst();
    for i in items {
        let mut strops = make_sub_ops(rt, i);
        ops.append(&mut strops.ops);
        ops.push(Op::StrCat(dst.clone(), strops.dst));
    }
    rt.pop_dst();
    Oxpr{ ops: ops, dst: dst }
}


#[cfg(test)]
mod tests {
    use leema::code::{self, Op};
    use leema::ixpr::{Ixpr};
    use leema::reg::{Reg};
    use leema::val::{Val};

#[test]
fn test_code_constval()
{
    let ic = Ixpr::const_val(Val::Int(9));
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
