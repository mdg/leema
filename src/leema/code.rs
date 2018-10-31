use leema::fiber;
use leema::frame;
use leema::ixpr::{Ixpr, Source};
use leema::lstr::Lstr;
use leema::reg::{Reg, RegTable};
use leema::rsrc;
use leema::sendclone::SendClone;
use leema::struple::Struple;
use leema::val::{Type, Val};
use leema::worker::RustFuncContext;

use std::fmt;
use std::marker;
use std::sync::Arc;


#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Eq)]
#[derive(PartialOrd)]
#[derive(Ord)]
#[derive(Clone)]
pub struct ModSym(pub String, pub String);

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
    ApplyFunc(Reg, Reg),
    Return,
    SetResult(Reg),
    PropagateFailure(Reg, i16),
    ConstVal(Reg, Val),
    ConstructEnum(Reg, Type, Lstr, Struple<Type>),
    Construple(Reg, Type, Struple<Type>),
    Copy(Reg, Reg),
    // Fork(Reg, Reg, Reg),
    Jump(i16),
    JumpIfNot(i16, Reg),
    IfFailure(Reg, i16),
    // jump if no match, pattern reg, input reg
    MatchPattern(Reg, Val, Reg),
    ListCons(Reg, Reg, Reg),
    ListCreate(Reg),
    MapCreate(Reg),
    StrCat(Reg, Reg),
    TupleCreate(Reg, i8),
}

unsafe impl marker::Send for Op {}
unsafe impl marker::Sync for Op {}

impl Clone for Op
{
    fn clone(&self) -> Op
    {
        match self {
            &Op::ApplyFunc(ref dst, ref f) => {
                Op::ApplyFunc(dst.clone(), f.clone())
            }
            &Op::Return => Op::Return,
            &Op::SetResult(ref src) => Op::SetResult(src.clone()),
            &Op::PropagateFailure(ref src, line) => {
                Op::PropagateFailure(src.clone(), line)
            }
            &Op::ConstVal(ref dst, ref src) => {
                Op::ConstVal(dst.clone(), src.deep_clone())
            }
            &Op::ConstructEnum(ref dst, ref typ, ref var, ref flds) => {
                Op::ConstructEnum(
                    dst.clone(),
                    typ.deep_clone(),
                    var.clone(),
                    flds.clone_for_send(),
                )
            }
            &Op::Construple(ref dst, ref typ, ref flds) => {
                Op::Construple(
                    dst.clone(),
                    typ.deep_clone(),
                    flds.clone_for_send(),
                )
            }
            &Op::Copy(ref dst, ref src) => Op::Copy(dst.clone(), src.clone()),
            &Op::Jump(j) => Op::Jump(j),
            &Op::JumpIfNot(j, ref tst) => Op::JumpIfNot(j, tst.clone()),
            &Op::IfFailure(ref src, j) => Op::IfFailure(src.clone(), j),
            &Op::MatchPattern(ref dst, ref patt, ref input) => {
                Op::MatchPattern(dst.clone(), patt.deep_clone(), input.clone())
            }
            &Op::ListCons(ref dst, ref head, ref tail) => {
                Op::ListCons(dst.clone(), head.clone(), tail.clone())
            }
            &Op::ListCreate(ref dst) => Op::ListCreate(dst.clone()),
            &Op::MapCreate(ref dst) => Op::MapCreate(dst.clone()),
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

pub type RustFunc2 = fn(RustFuncContext) -> frame::Event;

pub enum Code
{
    Leema(OpVec),
    Rust(RustFunc),
    Rust2(RustFunc2),
    Iop(rsrc::IopAction, Option<i8>),
}

impl Code
{
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
        match self {
            &Code::Rust(_) | &Code::Rust2(_) => true,
            _ => false,
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
            &Code::Rust2(_) => write!(f, "RustCode2"),
            &Code::Iop(_, _) => write!(f, "IopCode"),
        }
    }
}

impl fmt::Debug for Code
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self {
            &Code::Leema(ref ops) => {
                let mut result;
                result = writeln!(f, "Code::Leema");
                let mut i = 0;
                for op in &**ops {
                    result = writeln!(f, "{:3} {:?}", i, op);
                    i += 1;
                }
                result
            }
            &Code::Rust(_) => write!(f, "Code::Rust"),
            &Code::Rust2(_) => write!(f, "Code::Rust2"),
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
            &Code::Rust2(rf) => Code::Rust2(rf),
            &Code::Iop(ref iopf, ref rsrc_idx) => {
                Code::Iop(*iopf, rsrc_idx.clone())
            }
        }
    }
}

pub fn make_ops(input: &Ixpr) -> OpVec
{
    vout!("make_ops({:?})\n", input);
    let mut regtbl = RegTable::new();
    let mut ops = make_sub_ops(&mut regtbl, input);
    ops.ops.push((Op::SetResult(ops.dst), input.line));
    ops.ops.push((Op::Return, input.line));
    ops.ops
}

pub fn make_sub_ops(rt: &mut RegTable, input: &Ixpr) -> Oxpr
{
    match input.src {
        Source::Block(ref lines) => {
            vout!("block dst: {}", rt.dst());
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
            Oxpr {
                ops,
                dst: rt.dst().clone(),
            }
        }
        Source::ConstVal(Val::FuncRef(ref cri, ref cvs, ref typ)) => {
            let mut ops = Vec::with_capacity(cvs.0.len() + 1);
            let dst = rt.dst().clone();
            let blanks = Struple(
                cvs.0.iter().map(|cv| (cv.0.clone(), Val::Void)).collect(),
            );
            ops.push((
                Op::ConstVal(
                    dst.clone(),
                    Val::FuncRef(cri.clone(), blanks, typ.clone()),
                ),
                input.line,
            ));
            let skipped_args = match typ {
                &Type::Func(ref ftype) => ftype.args.len(),
                &Type::SpecialFunc(_, ref ftype) => ftype.args.len(),
                &Type::GenericFunc(_, _) => {
                    panic!("unexpected generic func {}: {}", cri, typ);
                }
                _ => {
                    panic!("func type is not a func: {}", typ);
                }
            };
            let mut i = skipped_args as i8;
            for cv in cvs.0.iter().skip(skipped_args) {
                if cv.0.is_none() {
                    panic!("closed variable has no name for {}", cri);
                }
                let var_name = cv.0.as_ref().unwrap();
                let var_src = rt.id(var_name);
                ops.push((Op::Copy(dst.sub(i), var_src), input.line));
                i += 1;
            }
            Oxpr { ops, dst }
        }
        Source::ConstVal(ref v) => {
            let dst = rt.dst();
            Oxpr {
                ops: vec![(Op::ConstVal(dst.clone(), v.clone()), input.line)],
                dst: dst.clone(),
            }
        }
        Source::FieldAccess(_, ref fldname, None) => {
            panic!("cannot access a field with no index: {}", fldname);
        }
        Source::FieldAccess(ref base, _, Some(fld_idx)) => {
            let mut base_ops = make_sub_ops(rt, base);
            Oxpr {
                ops: base_ops.ops,
                dst: base_ops.dst.sub(fld_idx),
            }
        }
        Source::Fork(ref _fx) => {
            panic!("cannot generate code for forks yet");
            // maybe a fork is not really a thing at this level
            // a fork is really
            // create a closure
            // call it and connect it to a future
            // return the future
            // maybe start w/ making closures and forks will be easier
        }
        Source::Func(ref argnames, ref closed, _, _, ref body) => {
            rt.def_args(argnames, closed);
            make_sub_ops(rt, &body)
        }
        Source::Call(ref f, ref args) => make_call_ops(rt, f, args),
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
            Oxpr { ops: hops.ops, dst }
        }
        Source::Construple(ref typ, ref variant, ref flds) => {
            vout!("make_construple_ops({:?})\n", typ);
            make_construple_ops(rt, typ, variant, flds, input.line)
        }
        Source::Let(ref patt, ref x, ref fails) => {
            let pval = assign_pattern_registers(rt, patt);
            rt.push_dst();
            let mut xops = make_sub_ops(rt, x);
            let mut failops: Vec<(Op, i16)> = fails
                .iter()
                .flat_map(|mf| {
                    let ox =
                        make_matchfailure_ops(rt, &mf.var, &mf.case, mf.line);
                    ox.ops.into_iter()
                })
                .collect();
            rt.pop_dst();
            xops.ops.push((
                Op::MatchPattern(rt.dst().clone(), pval, xops.dst),
                input.line,
            ));
            xops.ops.append(&mut failops);
            Oxpr {
                ops: xops.ops,
                dst: rt.dst().clone(),
            }
        }
        Source::MatchExpr(ref x, ref cases) => {
            make_matchexpr_ops(rt, &*x, &*cases)
        }
        Source::MatchCase(_, _, _) => {
            panic!("matchcase ops not generated directly");
        }
        Source::Id(ref id, _line) => {
            let src = rt.id(id);
            Oxpr {
                ops: vec![],
                dst: src,
            }
        }
        Source::IfExpr(ref test, ref truth, None) => {
            make_if_ops(rt, &*test, &*truth)
        }
        Source::IfExpr(ref test, ref truth, ref lies) => {
            make_if_else_ops(rt, &*test, &*truth, lies.as_ref().unwrap())
        }
        Source::StrMash(ref items) => make_str_ops(rt, items),
        Source::Tuple(ref items) => {
            let dst = rt.dst().clone();
            let newtup = Op::TupleCreate(dst.clone(), items.0.len() as i8);
            let mut ops: Vec<(Op, i16)> = vec![(newtup, input.line)];
            rt.push_sub();
            for i in items.0.iter() {
                // set dst to dst.child
                let mut iops = make_sub_ops(rt, &i.1);
                ops.append(&mut iops.ops);
                if *rt.dst() != iops.dst {
                    ops.push((Op::Copy(rt.dst().clone(), iops.dst), i.1.line));
                }
                rt.next_sub();
            }
            rt.pop_dst();
            Oxpr { ops, dst }
        }
        Source::List(ref items) => make_list_ops(rt, items, input.line),
        Source::Map(ref items) => make_map_ops(rt, items, input.line),
        Source::Return(ref result) => {
            let mut rops = make_sub_ops(rt, result);
            rops.ops.push((Op::SetResult(rops.dst.clone()), input.line));
            rops.ops.push((Op::Return, input.line));
            rops
        }
        Source::RustBlock(_, _) => {
            Oxpr {
                ops: vec![],
                dst: rt.dst().clone(),
            }
        }
    }
}

pub fn make_call_ops(rt: &mut RegTable, f: &Ixpr, args: &Struple<Ixpr>)
    -> Oxpr
{
    let dst = rt.dst().clone();
    vout!("make_call_ops: {:?} = {:?}\n", dst, f);

    let fref_dst = rt.push_dst().clone();
    let mut fops = make_sub_ops(rt, f);
    if fops.dst != fref_dst {
        fops.ops
            .push((Op::Copy(fref_dst.clone(), fops.dst), f.line));
        fops.dst = fref_dst.clone();
    }

    let mut argops: OpVec = args
        .0
        .iter()
        .enumerate()
        .flat_map(|(i, a)| {
            let argdst = fref_dst.sub(i as i8);
            rt.push_dst_reg(argdst.clone());
            let mut arg_ops: Oxpr = make_sub_ops(rt, &a.1);
            if arg_ops.dst != argdst {
                arg_ops
                    .ops
                    .push((Op::Copy(argdst.clone(), arg_ops.dst), a.1.line));
                arg_ops.dst = argdst.clone();
            }
            rt.pop_dst_reg();
            arg_ops.ops
        })
        .collect();
    fops.ops.append(&mut argops);
    fops.ops
        .push((Op::ApplyFunc(dst.clone(), fops.dst.clone()), f.line));

    rt.pop_dst();
    fops.dst = dst;
    fops
}

pub fn make_construple_ops(
    rt: &mut RegTable,
    typ: &Type,
    variant: &Option<Lstr>,
    flds: &Struple<Type>,
    line: i16,
) -> Oxpr
{
    let dst = rt.dst().clone();
    let construct = match variant {
        Some(ref ivar) => {
            let var = ivar.clone();
            Op::ConstructEnum(dst.clone(), typ.clone(), var, flds.clone())
        }
        None => Op::Construple(dst.clone(), typ.clone(), flds.clone()),
    };
    let ops: Vec<(Op, i16)> = vec![(construct, line)];

    Oxpr { ops, dst }
}

pub fn make_matchfailure_ops(
    rt: &mut RegTable,
    var: &Lstr,
    opt_cases: &Option<Ixpr>,
    line: i16,
) -> Oxpr
{
    let dst = rt.push_dst().clone();
    let vreg = rt.id(var);
    // if no case, generate default propagation
    if opt_cases.is_none() {
        let propagate = Op::PropagateFailure(vreg.clone(), line);
        return Oxpr {
            ops: vec![(propagate, line)],
            dst: vreg,
        };
    }

    let cases = opt_cases.as_ref().unwrap();
    let failtag = vreg.sub(0);
    let mut case_ops = make_matchcase_ops(rt, dst.clone(), cases, &failtag);

    let faillen = case_ops.ops.len() + 2;
    let mut failops = Vec::with_capacity(faillen);
    failops.push((Op::IfFailure(vreg.clone(), faillen as i16), cases.line));
    failops.append(&mut case_ops.ops);
    failops.push((Op::Copy(vreg.clone(), dst), cases.line));

    rt.pop_dst();
    Oxpr {
        ops: failops,
        dst: vreg,
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
    Oxpr { ops: xops.ops, dst }
}

pub fn make_matchcase_ops(
    rt: &mut RegTable,
    dstreg: Reg,
    matchcase: &Ixpr,
    xreg: &Reg,
) -> Oxpr
{
    let (patt, code, next) = match matchcase.src {
        Source::MatchCase(ref patt, ref code, ref next) => (patt, code, next),
        Source::ConstVal(Val::Void) => {
            // this is here when there's no else case
            vout!("empty_matchcase_ops\n");
            return Oxpr {
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
        code_ops
            .ops
            .push((Op::Jump((next_len + 1) as i16), matchcase.line));
    }
    rt.push_dst();
    let mut patt_ops: Vec<(Op, i16)> = vec![(
        Op::MatchPattern(rt.dst().clone(), patt_val, xreg.clone()),
        matchcase.line,
    )];
    patt_ops.push((
        Op::JumpIfNot(code_ops.ops.len() as i16 + 1, rt.dst().clone()),
        matchcase.line,
    ));
    rt.pop_dst();

    patt_ops.append(&mut code_ops.ops);
    patt_ops.append(&mut next_ops.ops);
    Oxpr {
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
            Val::Cons(Box::new(pr_head), Arc::new(pr_tail))
        }
        &Val::Tuple(ref vars) => {
            let reg_items = vars
                .0
                .iter()
                .map(|v| (v.0.clone(), assign_pattern_registers(rt, &v.1)))
                .collect();
            Val::Tuple(Struple(reg_items))
        }
        &Val::Struct(ref styp, ref vars) => {
            let reg_items = vars
                .0
                .iter()
                .map(|v| (v.0.clone(), assign_pattern_registers(rt, &v.1)))
                .collect();
            Val::Struct(styp.clone(), Struple(reg_items))
        }
        &Val::EnumStruct(ref styp, ref variant, ref vars) => {
            let reg_items = vars
                .0
                .iter()
                .map(|v| (v.0.clone(), assign_pattern_registers(rt, &v.1)))
                .collect();
            Val::EnumStruct(styp.clone(), variant.clone(), Struple(reg_items))
        }
        &Val::EnumToken(ref styp, ref variant) => {
            Val::EnumToken(styp.clone(), variant.clone())
        }
        _ => {
            panic!("pattern type unsupported: {:?}", pattern);
        }
    }
}

pub fn make_if_ops(rt: &mut RegTable, test: &Ixpr, truth: &Ixpr) -> Oxpr
{
    let mut truth_ops = make_sub_ops(rt, &truth);
    rt.push_dst();
    let mut if_ops = make_sub_ops(rt, &test);
    rt.pop_dst();

    if_ops.ops.push((
        Op::JumpIfNot((truth_ops.ops.len() + 1) as i16, if_ops.dst),
        test.line,
    ));

    if_ops.ops.append(&mut truth_ops.ops);
    Oxpr {
        ops: if_ops.ops,
        dst: truth_ops.dst,
    }
}

pub fn make_if_else_ops(
    rt: &mut RegTable,
    test: &Ixpr,
    truth: &Ixpr,
    lies: &Ixpr,
) -> Oxpr
{
    let mut truth_ops = make_sub_ops(rt, &truth);
    let mut lies_ops = make_sub_ops(rt, &lies);
    rt.push_dst();
    let mut if_ops = make_sub_ops(rt, &test);
    rt.pop_dst();

    truth_ops
        .ops
        .push((Op::Jump((lies_ops.ops.len() + 1) as i16), truth.line));
    if_ops.ops.push((
        Op::JumpIfNot((truth_ops.ops.len() + 1) as i16, if_ops.dst),
        lies.line,
    ));

    if_ops.ops.append(&mut truth_ops.ops);
    if_ops.ops.append(&mut lies_ops.ops);
    Oxpr {
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
    Oxpr { ops, dst }
}

pub fn make_map_ops(
    rt: &mut RegTable,
    _items: &Struple<Ixpr>,
    line: i16,
) -> Oxpr
{
    let dst = rt.dst().clone();
    let ops = vec![(Op::MapCreate(dst.clone()), line)];
    Oxpr { ops, dst }
}

pub fn make_str_ops(rt: &mut RegTable, items: &Vec<Ixpr>) -> Oxpr
{
    let dst = rt.dst().clone();
    let mut ops: Vec<(Op, i16)> = Vec::with_capacity(items.len());
    ops.push((
        Op::ConstVal(dst.clone(), Val::empty_str()),
        items.first().unwrap().line,
    ));
    rt.push_dst();
    for i in items {
        let mut strops = make_sub_ops(rt, i);
        ops.append(&mut strops.ops);
        ops.push((Op::StrCat(dst.clone(), strops.dst), i.line));
    }
    rt.pop_dst();
    Oxpr { ops, dst }
}


#[cfg(test)]
mod tests
{
    use leema::code::{self, Op};
    use leema::ixpr::Ixpr;
    use leema::loader::Interloader;
    use leema::lstr::Lstr;
    use leema::program;
    use leema::reg::Reg;
    use leema::val::Val;

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
    fn test_too_many_args()
    {
        let input = String::from(
            "
            func sum(a, b) -> a + b --
            func main() -> sum(3, 4, 5) --
            ",
        );

        let mut loader = Interloader::new(Lstr::Sref("tacos.lma"));
        loader.set_mod_txt(Lstr::Sref("tacos"), input);
        let mut prog = program::Lib::new(loader);
        prog.load_code(&Lstr::Sref("tacos"), &Lstr::Sref("main"));
    }

    #[test]
    #[ignore] // ignored until generics are fixed
    #[should_panic]
    fn test_load_code_fails_for_func_type_infer_mismatch()
    {
        let input = "
            ## foo should take [#] and return a #
            func foo(inputs)
            |([]) -> #empty
            |(#whatever;more) -> #whatever
            |(_;more) -> foo(more)
            --

            func main() ->
                foo([5, 3, 4])
            --
            "
        .to_string();

        let mut loader = Interloader::new(Lstr::Sref("tacos.lma"));
        loader.set_mod_txt(Lstr::Sref("tacos"), input);
        let mut prog = program::Lib::new(loader);
        prog.load_code(&Lstr::Sref("tacos"), &Lstr::Sref("main"));
    }

}
