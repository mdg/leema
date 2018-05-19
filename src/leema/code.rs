use leema::fiber;
use leema::reg::{Reg, RegTable};
use leema::val::{Val, Type};
use leema::log;
use leema::ixpr::{Ixpr, Source};
use leema::frame;
use leema::rsrc;

use std::fmt;
use std::io::{Write};
use std::rc::{Rc};
use std::sync::Arc;
use std::marker;


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
    PropagateFailure(Reg, i16),
    ConstVal(Reg, Val),
    Constructor(Reg, Type, i8),
    Copy(Reg, Reg),
    Fork(Reg, Reg, Reg),
    //IfFail(Reg, i16),
    Jump(i16),
    JumpIfNot(i16, Reg),
    // if failure, copy hashtag, else jump
    IfFailure(Reg, Reg, i16),
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
            &Op::PropagateFailure(ref src, line) => {
                Op::PropagateFailure(src.clone(), line)
            }
            &Op::ConstVal(ref dst, ref src) => {
                Op::ConstVal(dst.clone(), src.deep_clone())
            }
            &Op::Constructor(ref dst, ref src, nflds) => {
                Op::Constructor(dst.clone(), src.deep_clone(), nflds)
            }
            &Op::Copy(ref dst, ref src) => {
                Op::Copy(dst.clone(), src.clone())
            }
            &Op::Jump(j) => Op::Jump(j),
            &Op::Fork(_, _, _) => {
                panic!("you can't fork yet, relax");
            }
            &Op::JumpIfNot(j, ref tst) => Op::JumpIfNot(j, tst.clone()),
            &Op::IfFailure(ref dst, ref src, j) => {
                Op::IfFailure(dst.clone(), src.clone(), j)
            }
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
        }
    }
}

pub type OpVec = Vec<(Op, i16)>;

#[derive(Debug)]
pub struct Oxpr
{
    ops: OpVec,
    dst: Reg,
}


pub type RustFunc = fn(&mut fiber::Fiber) -> frame::Event;

trait RustFunc2
{
    fn call(&mut self, env: &mut frame::Frame) -> frame::Event;
}

pub enum Code
{
    Leema(OpVec),
    Rust(RustFunc),
    Iop(rsrc::IopAction, Option<i8>),
}

impl Code
{
    pub fn type_name(&self) -> &'static str
    {
        match self {
            &Code::Leema(_) => "LeemaCode",
            &Code::Rust(_) => "RustCode",
            &Code::Iop(_, _) => "IopCode",
        }
    }

    pub fn is_leema(&self) -> bool
    {
        if let &Code::Leema(_) = self {
            true
        } else {
            false
        }
    }

    pub fn is_rust(&self) -> bool
    {
        if let &Code::Rust(_) = self {
            true
        } else {
            false
        }
    }

    pub fn get_iop(&self) -> Option<(rsrc::IopAction, Option<i8>)>
    {
        if let &Code::Iop(iopf, rsrc_idx) = self {
            Some((iopf, rsrc_idx))
        } else {
            None
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
            &Code::Iop(_, _) => write!(f, "IopCode"),
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
            &Code::Iop(_, _) => write!(f, "Code::Iop"),
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
            &Code::Iop(ref iopf, ref rsrc_idx) => {
                Code::Iop(*iopf, rsrc_idx.clone())
            }
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
        ops.ops.push((Op::SetResult(Reg::local(0)), input.line));
    }
    ops.ops.push((Op::Return, input.line));
    ops.ops
}

pub fn make_sub_ops(rt: &mut RegTable, input: &Ixpr) -> Oxpr
{
    match input.src {
        Source::Block(ref lines, ref fails, ref is_root) => {
            let mut oxprs = Vec::with_capacity(lines.len());
            for i in lines.iter().rev() {
                oxprs.push(make_sub_ops(rt, i));
            }
            let mut ops: Vec<(Op, i16)> = Vec::with_capacity(oxprs.len());
            let mut last_dst = rt.dst().clone();
            for mut i in oxprs.iter_mut().rev() {
                ops.append(&mut i.ops);
                last_dst = i.dst.clone();
            }
            if *rt.dst() != last_dst {
                ops.push((Op::Copy(rt.dst().clone(), last_dst), input.line));
            }
            Oxpr{
                ops: ops,
                dst: rt.dst().clone(),
            }
        }
        Source::ConstVal(ref v) => {
            let dst = rt.dst();
            Oxpr{
                ops: vec![(Op::ConstVal(dst.clone(), v.clone()), input.line)],
                dst: dst.clone(),
            }
        }
        Source::FieldAccess(ref base, fld_idx) => {
            let mut base_ops = make_sub_ops(rt, base);
            Oxpr{
                ops: base_ops.ops,
                dst: base_ops.dst.sub(fld_idx),
            }
        }
        Source::Func(ref argnames, ref body) => {
            rt.def_args(argnames);
            make_sub_ops(rt, &body)
        }
        Source::Call(ref f, ref args) => {
            make_call_ops(rt, f, args, &input.typ)
        }
        Source::Cons(ref h, ref t) => {
            let dst = rt.dst().clone();
            rt.push_dst();
            let mut hops = make_sub_ops(rt, h);
            rt.push_dst();
            let mut tops = make_sub_ops(rt, t);
            rt.pop_dst();
            rt.pop_dst();
            hops.ops.append(&mut tops.ops);
            hops.ops.push((
                Op::ListCons(dst.clone(), hops.dst, tops.dst),
                input.line,
            ));
            Oxpr{
                ops: hops.ops,
                dst: dst,
            }
        }
        Source::Constructor(ref typ, nflds) => {
            vout!("make_constructor_ops({:?})\n", input);
            make_constructor_ops(rt, typ, nflds, input.line)
        }
        Source::EnumConstructor(ref typ, idx, ref val) => {
            vout!("make_enum_constructor_ops({:?})\n", input);
            make_enum_constructor_ops(rt, typ, idx, &**val, input.line)
        }
        Source::Let(ref patt, ref x, ref fails) => {
            let pval = assign_pattern_registers(rt, patt);
            let mut xops = make_sub_ops(rt, x);
            let mut failops: Vec<(Op, i16)> =
                fails.iter().flat_map(|&(ref fail_name, ref f1)| {
                    rt.push_id(fail_name);
                    let ox = make_sub_ops(rt, f1);
                    rt.pop_id();
                    ox.ops.into_iter()
                }).collect();
            xops.ops.push((
                Op::MatchPattern(rt.dst().clone(), pval, xops.dst),
                input.line,
                ));
            xops.ops.append(&mut failops);
            Oxpr{ ops: xops.ops, dst: rt.dst().clone() }
        }
        Source::MatchFailure(ref x, ref cases) => {
            make_matchfailure_ops(rt, &*x, &*cases)
        }
        Source::MatchExpr(ref x, ref cases) => {
            make_matchexpr_ops(rt, &*x, &*cases)
        }
        Source::MatchCase(ref patt, ref code, ref next) => {
            panic!("matchcase ops not generated directly");
        }
        Source::PropagateFailure(ref id, usage_line) => {
            let src = rt.id(id);
            let propagate = Op::PropagateFailure(src.clone(), usage_line);
            Oxpr{ ops: vec![(propagate, input.line)], dst: src }
        }
        Source::Id(ref id, _) => {
            let src = rt.id(id);
            Oxpr{ ops: vec![], dst: src }
        }
        Source::IfExpr(ref test, ref truth, None) => {
            make_if_ops(rt, &*test, &*truth)
        }
        Source::IfExpr(ref test, ref truth, ref lies) => {
            make_if_else_ops(rt, &*test, &*truth, lies.as_ref().unwrap())
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
            let mut ops: Vec<(Op, i16)> = vec![(newtup, input.line)];
            rt.push_sub();
            for i in items {
                // set dst to dst.child
                let mut iops = make_sub_ops(rt, i);
                ops.append(&mut iops.ops);
                if *rt.dst() != iops.dst {
                    ops.push((
                        Op::Copy(rt.dst().clone(), iops.dst),
                        i.line,
                        ));
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
            make_list_ops(rt, items, input.line)
        }
        Source::ModuleAccess(ref module, ref name) => {
            let modval = Val::Str(module.clone());
            let idval = Val::Str(name.clone());
            let modname = Val::Tuple(vec![modval, idval]);
            let dst = rt.dst();
            Oxpr{
                ops: vec![(Op::ConstVal(dst.clone(), modname), input.line)],
                dst: dst.clone(),
            }
        }
        Source::Return(ref result) => {
            let mut rops = make_sub_ops(rt, result);
            rops.ops.push((Op::SetResult(rops.dst.clone()), input.line));
            rops.ops.push((Op::Return, input.line));
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

pub fn make_call_ops(rt: &mut RegTable, f: &Ixpr
    , args: &Ixpr, ftyp: &Type) -> Oxpr
{
    let dst = rt.dst().clone();
    vout!("make_call_ops: {:?} = {:?} : {:?}\n", dst, f, ftyp);

    rt.push_dst();
    let mut fops = make_sub_ops(rt, f);

    rt.push_dst();
    let mut argops = make_sub_ops(rt, args);
    fops.ops.append(&mut argops.ops);
    fops.ops.push((
        Op::ApplyFunc(dst.clone(), fops.dst.clone(), argops.dst),
        f.line,
        ));

    rt.pop_dst();
    rt.pop_dst();
    fops.dst = dst;
    fops
}

pub fn make_constructor_ops(rt: &mut RegTable, typ: &Type, nflds: i8
    , line: i16
    ) -> Oxpr
{
    let dst = rt.dst();

    let mut ops: Vec<(Op, i16)> = Vec::with_capacity((nflds + 1) as usize);
    ops.push((
        Op::Constructor(dst.clone(), typ.clone(), nflds),
        line,
        ));
    let mut i = 0;
    while i < nflds {
        ops.push((
            Op::Copy(dst.sub(i), Reg::param(i)),
            line,
            ));
        i += 1;
    }

    Oxpr{
        ops: ops,
        dst: dst.clone(),
    }
}

pub fn make_enum_constructor_ops(rt: &mut RegTable, typ: &Type, index: i16
    , data: &Ixpr, line: i16
    ) -> Oxpr
{
    let dst = rt.dst();
    let etype = typ.to_enum();

    let mut ops: Vec<(Op, i16)> = Vec::with_capacity(3);
    /*
    ops.push((
        Op::Constructor(dst.clone(), etype.clone(), nflds),
        line,
        ));
    let mut i = 0;
    while i < nflds {
        ops.push((
            Op::Copy(dst.sub(i), Reg::param(i)),
            line,
            ));
        i += 1;
    }
    */

    Oxpr{
        ops: ops,
        dst: dst.clone(),
    }
}

pub fn make_matchfailure_ops(rt: &mut RegTable, x: &Ixpr, cases: &Ixpr) -> Oxpr
{
    rt.push_dst();
    let xops = make_sub_ops(rt, x);
    rt.pop_dst();
    let failtag = rt.id("leema#failure");
    let mut case_ops =
        make_matchcase_ops(rt, xops.dst.clone(), cases, &failtag);

    let faillen = case_ops.ops.len() + 1;
    let mut failops = Vec::with_capacity(faillen);
    failops.push(
        (Op::IfFailure(failtag, xops.dst.clone(), faillen as i16), x.line));
    failops.append(&mut case_ops.ops);

    Oxpr{
        ops: failops,
        dst: xops.dst,
    }
}

pub fn make_matchexpr_ops(rt: &mut RegTable, x: &Ixpr, cases: &Ixpr) -> Oxpr
{
    let dst = rt.dst().clone();
    vout!("make_matchexpr_ops({:?},{:?}) -> {:?}\n", x, cases, dst);
    rt.push_dst();
    let mut xops = make_sub_ops(rt, &x);

    let mut case_ops = make_matchcase_ops(rt, dst.clone(), cases, &xops.dst);
    vout!("made matchcase_ops =\n{:?}\n", case_ops);
    rt.pop_dst();

    xops.ops.append(&mut case_ops.ops);
    Oxpr{
        ops: xops.ops,
        dst: dst,
    }
}

pub fn make_matchcase_ops(rt: &mut RegTable, dstreg: Reg
    , matchcase: &Ixpr, xreg: &Reg
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
    rt.push_dst_reg(dstreg.clone());
    let mut code_ops = make_sub_ops(rt, code);
    rt.pop_dst_reg();
    // pop reg scope
    let mut next_ops = make_matchcase_ops(rt, dstreg.clone(), next, &xreg);

    let next_len = next_ops.ops.len();
    if next_len > 0 {
        code_ops.ops.push((
            Op::Jump((next_len + 1) as i16),
            matchcase.line,
            ));
    }
    rt.push_dst();
    let mut patt_ops: Vec<(Op, i16)> = vec![(
        Op::MatchPattern(
            rt.dst().clone(),
            patt_val,
            xreg.clone(),
            ),
        matchcase.line,
    )];
    patt_ops.push((
        Op::JumpIfNot(
            code_ops.ops.len() as i16 + 1,
            rt.dst().clone(),
            ),
        matchcase.line,
    ));
    rt.pop_dst();

    patt_ops.append(&mut code_ops.ops);
    patt_ops.append(&mut next_ops.ops);
    Oxpr{
        ops: patt_ops,
        dst: dstreg,
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
            Val::Cons(Box::new(pr_head), Rc::new(pr_tail))
        }
        &Val::Tuple(ref items) => {
            let reg_items = items.iter().map(|p| {
                assign_pattern_registers(rt, p)
            }).collect();
            Val::Tuple(reg_items)
        }
        &Val::Struct(ref typ, ref vars) => {
            let reg_items = vars.iter().map(|v| {
                assign_pattern_registers(rt, v)
            }).collect();
            Val::Struct(typ.clone(), reg_items)
        }
        _ => {
            panic!("pattern type unsupported: {:?}", pattern);
        }
    }
}

pub fn make_if_ops(rt: &mut RegTable, test: &Ixpr, truth: &Ixpr) -> Oxpr
{
    rt.push_dst();
    let mut if_ops = make_sub_ops(rt, &test);
    rt.pop_dst();
    let mut truth_ops = make_sub_ops(rt, &truth);

    if_ops.ops.push((
        Op::JumpIfNot((truth_ops.ops.len() + 1) as i16, if_ops.dst),
        test.line,
    ));

    if_ops.ops.append(&mut truth_ops.ops);
    Oxpr{
        ops: if_ops.ops,
        dst: truth_ops.dst,
    }
}

pub fn make_if_else_ops(rt: &mut RegTable, test: &Ixpr, truth: &Ixpr
    , lies: &Ixpr
    ) -> Oxpr
{
    rt.push_dst();
    let mut if_ops = make_sub_ops(rt, &test);
    rt.pop_dst();
    let mut truth_ops = make_sub_ops(rt, &truth);
    let mut lies_ops = make_sub_ops(rt, &lies);

    truth_ops.ops.push((Op::Jump((lies_ops.ops.len() + 1) as i16), truth.line));
    if_ops.ops.push((
        Op::JumpIfNot((truth_ops.ops.len() + 1) as i16, if_ops.dst),
        lies.line,
    ));

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

pub fn make_list_ops(rt: &mut RegTable, items: &Vec<Ixpr>, line: i16) -> Oxpr
{
    let dst = rt.dst().clone();
    let mut ops = vec![(Op::ListCreate(dst.clone()), line)];
    rt.push_dst();
    for i in items.iter().rev() {
        let mut listops = make_sub_ops(rt, i);
        ops.append(&mut listops.ops);
        ops.push((Op::ListCons(dst.clone(), listops.dst, dst.clone()), i.line));
    }
    rt.pop_dst();
    Oxpr{ ops: ops, dst: dst }
}

pub fn make_str_ops(rt: &mut RegTable, items: &Vec<Ixpr>) -> Oxpr
{
    let dst = rt.dst().clone();
    let mut ops: Vec<(Op, i16)> = Vec::with_capacity(items.len());
    ops.push((
        Op::ConstVal(
            dst.clone(),
            Val::empty_str(),
        ),
        items.first().unwrap().line,
    ));
    rt.push_dst();
    for i in items {
        let mut strops = make_sub_ops(rt, i);
        ops.append(&mut strops.ops);
        ops.push((Op::StrCat(dst.clone(), strops.dst), i.line));
    }
    rt.pop_dst();
    Oxpr{ ops: ops, dst: dst }
}


#[cfg(test)]
mod tests {
    use leema::code::{self, Op};
    use leema::ixpr::{Ixpr};
    use leema::loader::{Interloader};
    use leema::program;
    use leema::reg::{Reg};
    use leema::val::{Val, SrcLoc};

#[test]
fn test_code_constval()
{
    let ic = Ixpr::const_val(Val::Int(9), 8);
    let code = code::make_ops(&ic);

    assert_eq!(3, code.len());
    let x = vec![
        (Op::ConstVal(Reg::local(0), Val::Int(9)), 8),
        (Op::SetResult(Reg::local(0)), 8),
        (Op::Return, 8),
    ];
    assert_eq!(x, code);
}

#[test]
#[should_panic]
fn test_load_code_fails_for_func_type_infer_mismatch()
{
    let input = String::from("

    ## foo should take [#] and return a #
    func foo(inputs)
    |([]) -> #empty
    |(#whatever;more) -> #whatever
    |(_;more) -> foo(more)
    --

    func main() ->
        foo([5, 3, 4])
    --
    ");

    let mut loader = Interloader::new("tacos.lma");
    loader.set_mod_txt("tacos", input);
    let mut prog = program::Lib::new(loader);
    prog.load_code("tacos", "main");
}

}
