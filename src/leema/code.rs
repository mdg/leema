use crate::leema::ast2::{Ast, AstNode, Xlist};
use crate::leema::failure::Lresult;
use crate::leema::fiber;
use crate::leema::frame;
use crate::leema::ixpr::{Ixpr, Source};
use crate::leema::lstr::Lstr;
use crate::leema::reg::{Reg, RegTable};
use crate::leema::rsrc;
use crate::leema::sendclone::SendClone;
use crate::leema::struple::Struple;
use crate::leema::val::{Type, Val};
use crate::leema::worker::RustFuncContext;

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


pub type RustFunc = fn(&mut fiber::Fiber) -> Lresult<frame::Event>;

pub type RustFunc2 = fn(RustFuncContext) -> Lresult<frame::Event>;

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

pub fn make_ops2(input: AstNode) -> OpVec
{
    vout!("make_ops({:?})\n", input);
    let lineno = input.loc.lineno as i16;
    let dst = input.dst.clone();
    let mut ops = make_sub_ops2(input);
    ops.ops.push((Op::SetResult(dst), lineno));
    ops.ops.push((Op::Return, lineno));
    ops.ops
}

pub fn make_sub_ops2(input: AstNode) -> Oxpr
{
    let input_line = input.loc.lineno as i16;
    let subops = match *input.node {
        Ast::Block(lines) => {
            let mut oxprs = Vec::with_capacity(lines.len());
            for i in lines {
                oxprs.push(make_sub_ops2(i));
            }
            let mut ops = Vec::with_capacity(oxprs.len());
            for mut i in oxprs {
                ops.append(&mut i.ops);
            }
            ops
        }
        Ast::ConstVal(v) => {
            vec![(Op::ConstVal(input.dst.clone(), v.clone()), input_line)]
        }
        Ast::Call(f, args) => make_call_ops(input.dst.clone(), f, args),
        Ast::Let(patt, _, x) => {
            let pval = make_pattern_val(patt);
            let mut xops = make_sub_ops2(x);
            /*
            let mut failops: Vec<(Op, i16)> = fails
                .into_iter()
                .flat_map(|mf| {
                    let ox =
                        make_matchfailure_ops(rt, &mf.var, &mf.case, mf.line);
                    ox.ops.into_iter()
                })
                .collect();
                */
            xops.ops.push((
                Op::MatchPattern(input.dst.clone(), pval, xops.dst),
                input_line,
            ));
            // xops.ops.append(&mut failops);
            xops.ops
        }
        Ast::List(items) => {
            make_list_ops(input.dst.clone(), items, input.loc.lineno as i16)
        }
        Ast::Tuple(items) => {
            let newtup = Op::TupleCreate(input.dst.clone(), items.0.len() as i8);
            let mut ops: Vec<(Op, i16)> = vec![(newtup, input_line)];
            for i in items.0 {
                let mut iops = make_sub_ops2(i.v);
                ops.append(&mut iops.ops);
                // ops.push((Op::Copy(input.dst.clone(), iops.dst), i.1.line));
            }
            ops
        }
        Ast::StrExpr(items) => make_str_ops(items),
        Ast::Id1(_) => vec![],
        Ast::RustBlock => vec![],
        _ => vec![],
    };
    Oxpr {
        ops: subops,
        dst: input.dst,
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
        Source::FieldAccess(_, ref fldname, None) => {
            panic!("cannot access a field with no index: {}", fldname);
        }
        Source::FieldAccess(ref base, _, Some(fld_idx)) => {
            let base_ops = make_sub_ops(rt, base);
            Oxpr {
                ops: base_ops.ops,
                dst: base_ops.dst.sub(fld_idx),
            }
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
            Oxpr { ops: hops.ops, dst }
        }
        Source::Construple(ref typ, ref variant, ref flds) => {
            vout!("make_construple_ops({:?})\n", typ);
            make_construple_ops(rt, typ, variant, flds, input.line)
        }
        Source::MatchExpr(ref x, ref cases) => {
            make_matchexpr_ops(rt, &*x, &*cases)
        }
        Source::MatchCase(_, _, _) => {
            panic!("matchcase ops not generated directly");
        }
        Source::IfExpr(ref test, ref truth, None) => {
            make_if_ops(rt, &*test, &*truth)
        }
        Source::IfExpr(ref test, ref truth, ref lies) => {
            make_if_else_ops(rt, &*test, &*truth, lies.as_ref().unwrap())
        }
        Source::Map(ref items) => make_map_ops(rt, items, input.line),
        Source::Return(ref result) => {
            let mut rops = make_sub_ops(rt, result);
            rops.ops.push((Op::SetResult(rops.dst.clone()), input.line));
            rops.ops.push((Op::Return, input.line));
            rops
        }
        _ => {
            unimplemented!();
        }
    }
}

pub fn make_call_ops(dst: Reg, f: AstNode, args: Xlist) -> OpVec
{
    vout!("make_call_ops: {:?}\n", f);

    let flineno = f.loc.lineno as i16;
    let mut fops = make_sub_ops2(f);

    let mut argops: OpVec = args
        .0
        .into_iter()
        .rev()
        .flat_map(|a| {
            let iargops: Oxpr = make_sub_ops2(a.v);
            iargops.ops
        })
        .collect();
    fops.ops.append(&mut argops);
    fops.ops.push((Op::ApplyFunc(dst, fops.dst), flineno));
    fops.ops
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

pub fn make_list_ops(dst: Reg, items: Xlist, lineno: i16) -> OpVec
{
    let mut ops = vec![(Op::ListCreate(dst.clone()), lineno)];
    for i in items.0.into_iter().rev() {
        let ilineno = i.v.loc.lineno as i16;
        let mut listops = make_sub_ops2(i.v);
        ops.append(&mut listops.ops);
        ops.push((Op::ListCons(dst.clone(), listops.dst, dst.clone()), ilineno));
    }
    ops
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

pub fn make_str_ops(items: Vec<AstNode>) -> OpVec
{
    let (dst, lineno) = {
        let first = items.first().unwrap();
        (first.dst.clone(), first.loc.lineno as i16)
    };
    let mut ops: Vec<(Op, i16)> = Vec::with_capacity(items.len());
    ops.push((
        Op::ConstVal(dst.clone(), Val::empty_str()),
        lineno,
    ));
    for i in items {
        let iline = i.loc.lineno as i16;
        let mut strops = make_sub_ops2(i);
        ops.append(&mut strops.ops);
        ops.push((Op::StrCat(dst.clone(), strops.dst), iline));
    }
    ops
}

pub fn make_pattern_val(pattern: AstNode) -> Val
{
    match *pattern.node {
        Ast::Id1(id) => {
            vout!("pattern var:reg is {}.{:?}\n", id, pattern.dst);
            Val::PatternVar(pattern.dst)
        }
        Ast::ConstVal(v) => v,
        Ast::Wildcard => Val::Wildcard,
        Ast::Tuple(vars) => {
            let reg_items = vars
                .0
                .into_iter()
                .map(|v| (v.k.map(|k| Lstr::Sref(k)), make_pattern_val(v.v)))
                .collect();
            Val::Tuple(Struple(reg_items))
        }
        /*
        &Val::Cons(ref head, ref tail) => {
            let pr_head = assign_pattern_registers(rt, head);
            let pr_tail = assign_pattern_registers(rt, tail);
            Val::Cons(Box::new(pr_head), Arc::new(pr_tail))
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
        */
        _ => {
            panic!("pattern type unsupported: {:?}", pattern);
        }
    }
}


#[cfg(test)]
mod tests
{
    use crate::leema::ast2::{Ast, AstNode, Loc};
    use crate::leema::code::{self, Op};
    use crate::leema::loader::Interloader;
    use crate::leema::lstr::Lstr;
    use crate::leema::program;
    use crate::leema::reg::Reg;
    use crate::leema::val::Val;

    #[test]
    fn test_code_constval()
    {
        let loc = Loc { lineno: 8, column: 7 };
        let mut node = AstNode::new(Ast::ConstVal(Val::Int(9)), loc);
        node.dst = Reg::local(3);
        let code = code::make_ops2(node);

        assert_eq!(3, code.len());
        let x = vec![
            (Op::ConstVal(Reg::local(3), Val::Int(9)), 8),
            (Op::SetResult(Reg::local(3)), 8),
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

        let mut loader = Interloader::new(Lstr::Sref("tacos.lma"), "lib");
        loader.set_mod_txt(Lstr::Sref("tacos"), input);
        let mut prog = program::Lib::new(loader);
        prog.load_code(&Lstr::Sref("tacos"), &Lstr::Sref("main")).unwrap();
    }

    #[test]
    #[should_panic]
    fn test_load_code_fails_for_func_type_infer_mismatch()
    {
        let input = "
            ## foo should take [#] and return a #
            func foo(inputs) >>
            |([]) -> #empty
            |(#whatever;more) -> #whatever
            |(_;more) -> foo(more)
            --

            func main() >>
                foo([5, 3, 4])
            --
            "
        .to_string();

        let mut loader = Interloader::new(Lstr::Sref("tacos.lma"), "lib");
        loader.set_mod_txt(Lstr::Sref("tacos"), input);
        let mut prog = program::Lib::new(loader);
        prog.load_code(&Lstr::Sref("tacos"), &Lstr::Sref("main")).unwrap();
    }

}
