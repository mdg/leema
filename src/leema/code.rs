use crate::leema::ast2::{Ast, AstNode, Case, Xlist};
use crate::leema::failure::Lresult;
use crate::leema::fiber;
use crate::leema::frame;
use crate::leema::list;
use crate::leema::lstr::Lstr;
use crate::leema::reg::{Reg, RegStack};
use crate::leema::rsrc;
use crate::leema::sendclone::SendClone;
use crate::leema::struple::{Struple2, StrupleItem};
use crate::leema::val::{Type, Val};
use crate::leema::worker::RustFuncContext;

use std::fmt;
use std::marker;
use std::mem;


const CODEFAIL: &'static str = "codegen_failure";

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
    ApplyFunc(Reg, Reg, u16),
    Return,
    SetResult(Reg),
    PropagateFailure(Reg, u16),
    ConstVal(Reg, Val),
    // ConstructEnum(Reg, Type, Lstr, Struple2<Type>),
    // Construple(Reg, Type, Struple2<Type>),
    Copy(Reg, Reg),
    // Fork(Reg, Reg, Reg),
    Jump(i16),
    JumpIfNot(i16, Reg),
    IfFailure(Reg, i16),
    // jump if no match, pattern reg, input reg
    MatchPattern(Reg, Val, Reg),
    ListCons(Reg, Reg, Reg),
    StrCat(Reg, Reg),
}

unsafe impl marker::Send for Op {}
unsafe impl marker::Sync for Op {}

impl Clone for Op
{
    fn clone(&self) -> Op
    {
        match self {
            &Op::ApplyFunc(ref dst, ref f, line) => {
                Op::ApplyFunc(dst.clone(), f.clone(), line)
            }
            &Op::Return => Op::Return,
            &Op::SetResult(ref src) => Op::SetResult(src.clone()),
            &Op::PropagateFailure(ref src, lineno) => {
                Op::PropagateFailure(src.clone(), lineno)
            }
            &Op::ConstVal(ref dst, ref src) => {
                Op::ConstVal(dst.clone(), src.clone_for_send())
            }
            &Op::Copy(ref dst, ref src) => Op::Copy(dst.clone(), src.clone()),
            &Op::Jump(j) => Op::Jump(j),
            &Op::JumpIfNot(j, ref tst) => Op::JumpIfNot(j, tst.clone()),
            &Op::IfFailure(ref src, j) => Op::IfFailure(src.clone(), j),
            &Op::MatchPattern(ref dst, ref patt, ref input) => {
                Op::MatchPattern(
                    dst.clone(),
                    patt.clone_for_send(),
                    input.clone(),
                )
            }
            &Op::ListCons(ref dst, ref head, ref tail) => {
                Op::ListCons(dst.clone(), head.clone(), tail.clone())
            }
            &Op::StrCat(ref dst, ref src) => {
                Op::StrCat(dst.clone(), src.clone())
            }
        }
    }
}

pub type OpVec = Vec<Op>;

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
    vout!("make_ops2({:?})\n", input);
    let ops_dst = input.dst.clone();
    let mut ops = make_sub_ops2(input);
    ops.ops.push(Op::SetResult(ops_dst));
    ops.ops.push(Op::Return);
    ops.ops
}

pub fn make_sub_ops2(input: AstNode) -> Oxpr
{
    let input_dst = input.dst;
    let ops = match *input.node {
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
        Ast::ConstVal(v) => vec![Op::ConstVal(input_dst, v.clone())],
        Ast::Call(f, args) => make_call_ops(input_dst, f, args),
        Ast::Copy(src) => {
            let mut src_ops = make_sub_ops2(src);
            src_ops.ops.push(Op::Copy(input_dst.clone(), src_ops.dst));
            src_ops.ops
        }
        Ast::Let(patt, _, x) => {
            let pval = if let Ast::ConstVal(pv) = *patt.node {
                pv
            } else {
                panic!("let patterns should be ConstVal! {:?}", patt);
            };
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
            xops.ops.push(Op::MatchPattern(input_dst, pval, xops.dst));
            // xops.ops.append(&mut failops);
            xops.ops
        }
        Ast::CopyAndSet(src, flds) => {
            let src_dst = src.dst.clone();
            let mut cs_ops = make_sub_ops2(src);
            cs_ops.ops.push(Op::Copy(input.dst, src_dst));
            for f in flds.into_iter() {
                let mut f_ops = make_sub_ops2(f.v);
                cs_ops.ops.append(&mut f_ops.ops);
            }
            cs_ops.ops
        }
        Ast::List(items) => make_list_ops(input_dst, items),
        Ast::Tuple(items) => {
            let newtup = Op::ConstVal(input_dst, Val::new_tuple(items.len()));
            let mut ops: Vec<Op> = vec![newtup];
            for (i, item) in items.into_iter().enumerate() {
                let subdst = input_dst.sub(i as i8);
                let mut iops = make_sub_ops2(item.v);
                // should be able to generalize this
                if iops.dst != subdst {
                    iops.ops.push(Op::Copy(subdst, iops.dst));
                }
                ops.append(&mut iops.ops);
                // ops.push((Op::Copy(input_dst.clone(), iops.dst), i.1.line));
            }
            ops
        }
        Ast::StrExpr(items) => make_str_ops(input_dst.clone(), items),
        Ast::Ifx(cases) => make_if_ops(cases),
        Ast::Matchx(Some(x), cases) => make_matchexpr_ops(x, cases),
        Ast::Return(result) => {
            let mut rops = make_sub_ops2(result);
            rops.ops.push(Op::SetResult(rops.dst.clone()));
            rops.ops.push(Op::Return);
            rops.ops
        }
        Ast::Op2(".", base, _field) => {
            let base_ops = make_sub_ops2(base);
            base_ops.ops
        }
        Ast::Id1(ref _id) => vec![],
        Ast::RustBlock => vec![],

        // invalid patterns
        Ast::Matchx(None, _) => {
            // None should have been replaced by the args
            // in an earlier phase?
            panic!("match expression must have an expression");
        }
        _ => {
            panic!("node should not exist in function: {:?}", input);
        }
    };
    Oxpr {
        ops,
        dst: input_dst,
    }
}

/*
pub fn make_sub_ops(input: &Ixpr) -> Oxpr
{
    match input.src {
        Source::ConstVal(Val::FuncRef(ref cri, ref cvs, ref typ)) => {
            let mut ops = Vec::with_capacity(cvs.0.len() + 1);
            let dst = rt.dst().clone();
            let blanks = Struple2(
                cvs.0.iter().map(|cv| (cv.0.clone(), Val::VOID)).collect(),
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
            hops.ops.push(
                Op::ListCons(dst.clone(), hops.dst, tops.dst),
            );
            Oxpr { ops: hops.ops, dst }
        }
    }
}
*/

pub fn make_call_ops(dst: Reg, f: AstNode, args: Xlist) -> OpVec
{
    vout!("make_call_ops: {} = {:?}\n", dst, f);

    let lineno = f.loc.lineno;
    let mut fops = make_sub_ops2(f);

    let mut argops: OpVec = args
        .into_iter()
        .rev()
        .flat_map(|a| {
            let iargops: Oxpr = make_sub_ops2(a.v);
            iargops.ops
        })
        .collect();
    fops.ops.append(&mut argops);
    fops.ops.push(Op::ApplyFunc(dst, fops.dst, lineno));
    fops.ops
}

pub fn make_construple_ops(
    dst: Reg,
    _typ: &Type,
    variant: &Option<Lstr>,
    _flds: &Struple2<Type>,
) -> Oxpr
{
    let construct = match variant {
        Some(ref _ivar) => {
            // let var = ivar.clone();
            // Op::ConstructEnum(dst.clone(), typ.clone(), var, flds.clone())
            Op::ConstVal(dst.clone(), Val::VOID)
        }
        None => {
            // Op::Construple(dst.clone(), typ.clone(), flds.clone()),
            Op::ConstVal(dst.clone(), Val::VOID)
        }
    };
    let ops: Vec<Op> = vec![construct];

    Oxpr { ops, dst }
}

/*
pub fn make_matchfailure_ops(
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
// */

pub fn make_matchexpr_ops(x: AstNode, cases: Vec<Case>) -> OpVec
{
    vout!("make_matchexpr_ops({:?},{:?})\n", x, cases);

    let mut xops = make_sub_ops2(x);
    let x_dst = xops.dst.clone();
    // assume # cases is already verified > 0
    let last_case = cases.len() - 1;
    let mut case_ops: Vec<OpVec> = cases
        .into_iter()
        .enumerate()
        .map(|(i, case)| {
            make_matchcase_ops(case, &x_dst, &x_dst, i == last_case)
        })
        .collect();
    vout!("made matchcase_ops =\n{:?}\n", case_ops);

    // add jumps after
    case_ops.iter_mut().rev().fold(0 as i16, |after, case| {
        if after > 0 {
            case.push(Op::Jump(after + 1));
        }
        let case_ops_len = case.len() as i16;
        after + case_ops_len
    });

    for mut case in case_ops {
        xops.ops.append(&mut case);
    }
    xops.ops
}

pub fn make_matchcase_ops(
    matchcase: Case,
    xreg: &Reg,
    _matchreg: &Reg,
    last_case: bool,
) -> OpVec
{
    let ifmatch_dst = matchcase.cond.dst;
    let patt_val = if let Ast::ConstVal(pv) = *matchcase.cond.node {
        pv
    } else {
        panic!("match patterns should be const vals! {:?}", matchcase.cond);
    };
    let mut code_ops = make_sub_ops2(matchcase.body);

    if patt_val == Val::Wildcard {
        // wildcard in the not-last case would be a semantic failure
        // and should have already been verified
        return code_ops.ops;
    }

    let mut patt_ops: Vec<Op> =
        vec![Op::MatchPattern(ifmatch_dst, patt_val, xreg.clone())];
    let mut jump_len = code_ops.ops.len() as i16 + 1;
    if !last_case {
        jump_len += 1;
    }
    patt_ops.push(Op::JumpIfNot(jump_len, ifmatch_dst));

    patt_ops.append(&mut code_ops.ops);
    patt_ops
}

/// Interleave the condition checks w/ the bodies. Something like:
///   eval cond a
///   jump_if_not(cond a, end of body a)
///   eval cond b
///   jump_if_not(cond b, end of body b)
///   body c
pub fn make_if_ops(cases: Vec<Case>) -> OpVec
{
    let mut case_ops: Vec<(Oxpr, Oxpr)> = cases
        .into_iter()
        .map(|case| {
            let cond_ops = make_sub_ops2(case.cond);
            let body_ops = make_sub_ops2(case.body);
            (cond_ops, body_ops)
        })
        .collect();

    case_ops.iter_mut().rev().fold(0 as i16, |after, case| {
        if after > 0 {
            case.1.ops.push(Op::Jump(after + 1));
        }
        let body_ops_len = case.1.ops.len() as i16;
        if case.0.dst != Reg::Void {
            case.0
                .ops
                .push(Op::JumpIfNot(body_ops_len + 1, case.0.dst.clone()));
        }
        let cond_ops_len = case.0.ops.len() as i16;
        after + cond_ops_len + body_ops_len
    });

    case_ops
        .into_iter()
        .flat_map(|mut case| {
            case.0.ops.append(&mut case.1.ops);
            case.0.ops
        })
        .collect()
}

/*
pub fn make_fork_ops(dst: &Reg, f: &Ixpr, args: &Ixpr) -> Oxpr
{
    println!("make_fork_ops({:?}, {:?}, {:?})", dst, f, args);
    let mut ops = make_sub_ops(rt, f);
    ops.append(&mut make_sub_ops(rt, args));
    ops.push(Op::Fork(dst.clone(), Reg::Undecided, Reg::Undecided));
    ops
}
*/

pub fn make_list_ops(dst: Reg, items: Xlist) -> OpVec
{
    let mut ops = vec![Op::ConstVal(dst, Val::Nil)];
    for i in items.into_iter().rev() {
        let idst = i.v.dst;
        let mut listops = make_sub_ops2(i.v);
        ops.append(&mut listops.ops);
        ops.push(Op::ListCons(dst, idst, dst));
    }
    ops
}

pub fn make_str_ops(dst: Reg, items: Vec<AstNode>) -> OpVec
{
    let mut ops: Vec<Op> = Vec::with_capacity(items.len());
    ops.push(Op::ConstVal(dst, Val::empty_str()));
    for i in items {
        let idst = i.dst;
        let mut strops = make_sub_ops2(i);
        ops.append(&mut strops.ops);
        ops.push(Op::StrCat(dst.clone(), idst));
    }
    ops
}

pub fn assign_registers(input: &mut AstNode) -> Lresult<()>
{
    vout!("assign_registers({:?})\n", input);
    Registration::assign_registers(input, RegStack::new())?;
    Ok(())
}

pub struct Registration {}

impl Registration
{
    pub fn new() -> Registration
    {
        Registration {}
    }

    fn extract_pattern_val(node: &mut AstNode) -> Lresult<Val>
    {
        if let Ast::ConstVal(ref mut pval) = &mut *node.node {
            Ok(mem::take(pval))
        } else {
            Err(rustfail!(
                "compile_failure",
                "expected ConstVal, found {:?}",
                node,
            ))
        }
    }

    fn set_dst_or_copy(node: &mut AstNode, dst: Reg)
    {
        if node.dst == dst {
            // already set, do nothing
            return;
        }
        if node.dst != Reg::Undecided {
            // make the copy
            *node = AstNode::new(Ast::Copy(mem::take(node)), node.loc);
        }
        node.dst = dst;
    }

    fn assign_registers(node: &mut AstNode, mut stack: RegStack)
        -> Lresult<()>
    {
        stack.push_if_undecided(&mut node.dst);
        let first_dst = node.dst.clone();

        match &mut *node.node {
            Ast::Block(ref mut items) => {
                // copy the block's dst to the last item in the block
                if let Some(item) = items.last_mut() {
                    Self::set_dst_or_copy(item, node.dst);
                }
                for i in items.iter_mut() {
                    Self::assign_registers(i, stack)?;
                }
            }
            Ast::Call(ref mut f, ref mut args) => {
                stack.push_if_undecided(&mut f.dst);
                for (i, a) in args.iter_mut().enumerate() {
                    a.v.dst = f.dst.sub(i as i8);
                    Self::assign_registers(&mut a.v, stack)?;
                }
            }
            Ast::Let(ref mut lhs, _, ref mut rhs) => {
                Self::make_pattern_val(lhs)?;
                Self::assign_registers(rhs, stack)?;
            }
            Ast::Matchx(ref mut match_input, ref mut cases) => {
                if let Some(ref mut mi) = match_input {
                    Self::assign_registers(mi, stack)?;
                }
                let cond_dst = stack.push();
                for case in cases.iter_mut() {
                    case.cond.dst = cond_dst;
                    Self::make_pattern_val(&mut case.cond)?;
                    Self::set_dst_or_copy(&mut case.body, node.dst);
                    Self::assign_registers(&mut case.body, stack)?;
                }
            }
            Ast::Ifx(ref mut cases) => {
                let cond_dst = stack.push();
                for case in cases.iter_mut() {
                    if *case.cond.node == Ast::VOID {
                        case.cond.dst = Reg::Void;
                    } else if case.cond.dst == Reg::Undecided {
                        case.cond.dst = cond_dst;
                    }
                    Self::set_dst_or_copy(&mut case.body, node.dst);
                }
            }
            Ast::StrExpr(ref mut items) => {
                for i in items.iter_mut() {
                    Self::assign_registers(i, stack)?;
                }
            }
            Ast::Tuple(ref mut items) => {
                // subs can't go past 2 levels, so if the tuple itself
                // is a sub, move it to a temp stacked reg, set the fields
                // and copy it back to its original sub reg
                let sub_dst = if node.dst.is_sub() {
                    let tmp = node.dst;
                    node.dst = stack.push();
                    Some(tmp)
                } else {
                    None
                };
                for (i, item) in items.iter_mut().enumerate() {
                    item.v.dst = node.dst.sub(i as i8);
                    Self::assign_registers(&mut item.v, stack)?;
                }
                if let Some(old_dst) = sub_dst {
                    *node = AstNode::new(Ast::Copy(mem::take(node)), node.loc);
                    node.dst = old_dst;
                }
            }
            Ast::List(ref mut items) => {
                // not really sure what's going on here or why
                for i in items.iter_mut() {
                    Self::assign_registers(&mut i.v, stack)?;
                }
            }
            Ast::Op2(".", ref mut base, ref field) => {
                Self::assign_registers(base, stack)?;
                let field_idx = match &*field.node {
                    Ast::Id1(idx_str) => idx_str.parse().unwrap(),
                    Ast::ConstVal(Val::Int(i)) => *i as i8,
                    other => {
                        panic!("field name is not an identifier: {:?}", other)
                    }
                };
                node.dst = base.dst.sub(field_idx);
            }
            Ast::Op2(op2, ref mut a, ref mut b) => {
                panic!("assign registers to op2? {:?} {} {:?}", a, op2, b);
            }
            Ast::CopyAndSet(ref mut src, ref mut fields) => {
                Self::assign_registers(src, stack)?;
                for (i, f) in fields.iter_mut().enumerate() {
                    Self::set_dst_or_copy(&mut f.v, node.dst.sub(i as i8));
                    // skip assigning the type,
                    // probably don' care about it anymore
                    // f.v.typ = copied.typ.clone();
                }
            }
            Ast::Copy(ref mut src) => {
                // flatten the copy if unnecessary
                if node.dst == src.dst {
                    *node = mem::take(src);
                }
            }
            Ast::Return(ref mut result) => {
                Self::assign_registers(result, stack)?;
            }
            // nothing else to do
            Ast::Id1(_) | Ast::ConstVal(_) => {}
            // these shouldn't be here
            Ast::DefConst(_, _)
            | Ast::DefFunc(_, _, _, _)
            | Ast::DefMacro(_, _, _)
            | Ast::DefType(_, _, _)
            | Ast::FuncType(_, _)
            | Ast::Generic(_, _)
            | Ast::ModAction(_, _)
            | Ast::Module(_)
            | Ast::RustBlock
            | Ast::Wildcard => {} // do nothing
            Ast::Op1(op1, x) => {
                panic!("unexpected Op1 {} {:?}", op1, x);
            }
            Ast::Type(t) => {
                panic!("assigning a register to a type? {:?}", t);
            }
        }

        // if the dst reg has changed, insert a copy node
        if node.dst != first_dst && first_dst != Reg::Void {
            let mut copy_node = AstNode::void();
            copy_node.dst = first_dst;
            mem::swap(node, &mut copy_node);
            node.node = Box::new(Ast::Copy(copy_node));
        }
        Ok(())
    }

    fn make_pattern_val(node: &mut AstNode) -> Lresult<()>
    {
        /*
        if !mode.is_pattern() {
        */

        let pval = match &mut *node.node {
            Ast::Id1(id) => {
                if node.dst == Reg::Undecided {
                    panic!("unexpected undecided pattern reg: {}", id);
                }
                Val::PatternVar(node.dst)
            }
            Ast::Tuple(ref mut items) => {
                let tval: Lresult<Struple2<Val>> = items
                    .iter_mut()
                    .map(|item| {
                        let pk = item.k.map(|k| Lstr::Sref(k));
                        let pv = ltry!(Self::extract_pattern_val(&mut item.v));
                        Ok(StrupleItem::new(pk, pv))
                    })
                    .collect();
                Val::Tuple(tval?)
            }
            Ast::List(ref mut items) => {
                let mut result = Val::Nil;
                for i in items.iter_mut().rev() {
                    let next = ltry!(Self::extract_pattern_val(&mut i.v));
                    result = list::cons(next, result);
                }
                result
            }
            Ast::ConstVal(ref val) => val.clone(),
            Ast::Wildcard => Val::Wildcard,
            Ast::Op2(";", ref mut a, ref mut b) => {
                let pa = ltry!(Self::extract_pattern_val(a));
                let pb = ltry!(Self::extract_pattern_val(b));
                list::cons(pa, pb)
            }
            pnode => {
                // do nothing with other pattern values
                return Err(rustfail!(
                    CODEFAIL,
                    "cannot make a pattern val: {:?}",
                    pnode,
                ));
            }
        };
        *node.node = Ast::ConstVal(pval);
        Ok(())
    }
}

impl fmt::Debug for Registration
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "Registration")
    }
}


#[cfg(test)]
mod tests
{
    use crate::leema::ast2::{Ast, AstNode, Loc};
    use crate::leema::code::{self, Op};
    use crate::leema::loader::Interloader;
    use crate::leema::module::ModKey;
    use crate::leema::parser::{self, Rule};
    use crate::leema::program;
    use crate::leema::reg::Reg;
    use crate::leema::val::{Fref, Val};

    use std::path::PathBuf;

    fn core_program(mods: &[(&'static str, String)]) -> program::Lib
    {
        let mut loader = Interloader::default();
        for (name, src) in mods.iter() {
            loader.set_mod_txt(ModKey::from(*name), src.clone());
        }
        program::Lib::new(loader)
    }

    #[test]
    fn test_code_constval()
    {
        let loc = Loc {
            lineno: 8,
            column: 7,
        };
        let mut node = AstNode::new(Ast::ConstVal(Val::Int(9)), loc);
        node.dst = Reg::local(3);
        let code = code::make_ops2(node);

        assert_eq!(3, code.len());
        let x = vec![
            Op::ConstVal(Reg::local(3), Val::Int(9)),
            Op::SetResult(Reg::local(3)),
            Op::Return,
        ];
        assert_eq!(x, code);
    }

    #[test]
    fn test_code_lists()
    {
        let input = r#"
        import /io.print

        func is_empty:Bool :: l:[Int] ->
            match
            |[] -> True
            |_ -> False
            --
        --

        func main ->
            let e := is_empty([4, 8, 3])
            print("is empty? $e\n")
        --
        "#
        .to_string();

        let mut prog = core_program(&[("/foo", input)]);
        let main_ref = Fref::from(("/foo", "main"));
        let is_empty_ref = Fref::from(("/foo", "is_empty"));
        // make sure it didn't panic or fail
        prog.read_code(&main_ref).unwrap();
        prog.read_code(&is_empty_ref).unwrap();
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

        let path = vec![PathBuf::from("lib")];
        let mut loader = Interloader::new("tacos.lma", path);
        loader.set_mod_txt(From::from("tacos"), input);
        let mut prog = program::Lib::new(loader);
        let fref = Fref::with_modules(From::from("tacos"), "main");
        prog.load_code(&fref).unwrap();
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

        let path = vec![PathBuf::from("lib")];
        let mut loader = Interloader::new("tacos.lma", path);
        loader.set_mod_txt(ModKey::from("tacos"), input);
        let mut prog = program::Lib::new(loader);
        let fref = Fref::with_modules(From::from("tacos"), "main");
        prog.load_code(&fref).unwrap();
    }
}
