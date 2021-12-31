use crate::leema::ast2::{self, Ast, AstNode, Case, StepResult, Xlist};
use crate::leema::failure::{self, Lresult};
use crate::leema::fiber;
use crate::leema::frame;
use crate::leema::list;
use crate::leema::lstr::Lstr;
use crate::leema::reg::Reg;
use crate::leema::rsrc;
use crate::leema::sendclone::SendClone;
use crate::leema::struple::{Struple2, StrupleItem};
use crate::leema::val::{Type, Val};
use crate::leema::worker::RustFuncContext;

use std::collections::HashMap;
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
    /// Call the function at .0 items up the stack
    /// Args are between .0 and HEAD
    ///     still not clear where self/closed goes
    /// Source line at .1
    /// Leave the result on the stack
    PushCall
    {
        argc: i16,
        line: i16,
    },

    /// Push a constant value onto the stack
    PushConst(Val),

    /// Push a register value onto the stack
    PushReg(Reg),

    /// Pop a register off the stack and move it to a local
    PopReg(Reg),

    /// Pop a register off the stack and match it to the given pattern
    PopMatch(Val),

    /// Pop a register off the stack and branch on a pattern match
    /// match x
    /// |1 -> "a"
    /// |y -> "b$y"
    /// --
    /// PushReg(x)
    /// BranchMatch(1, 1) (pops if match)
    /// PushConst("a")
    /// Jump(0)
    /// Label(1)
    /// PopMatch(y)
    ///    BranchMatch(2, y) (pops if match)
    ///    Push("b$y")
    ///    Jump(0)
    ///    Label(2)
    ///    Pop(Null)
    ///    PushFailure "no match"
    /// Label(0)
    BranchMatch(i16, Val),

    /// Pop a register value off the stack
    /// Jump if it's not true
    /// if
    /// |a -> b
    /// |-> false
    /// --
    /// PushReg(a)
    /// BranchIf(0) (pops)
    /// PushReg(b)
    /// Jump(1)
    /// Label(0)
    /// PushConst(false)
    /// Label(1)
    BranchIf(i16),

    /// Push a register value onto the stack
    Label(i16),

    /// Pop two vals off the stack, append and push
    PopListCons,

    /// Pop two vals off the stack, append and push
    PopStrCat,

    /// Pop a value off the stack and set it as the result
    PushResult,

    /// Return to the calling function
    Return,

    /// Reserve .0 registers for locals and .1 registers for stack
    /// Eventually get ride of .1
    ReserveLocal(i16, i16),
    PropagateFailure(Reg, u16),
    /// Is this necessary? Just PushConst(VOID) isn't it?
    StackPush,

    // ConstructEnum(Reg, Type, Lstr, Struple2<Type>),
    // Construple(Reg, Type, Struple2<Type>),
    Copy(Reg, Reg),
    // Fork(Reg, Reg, Reg),
    /// Jump up or down in the function
    Jump(i16),
    IfFailure(Reg, i16),
}

unsafe impl marker::Send for Op {}
unsafe impl marker::Sync for Op {}

impl Clone for Op
{
    fn clone(&self) -> Op
    {
        match self {
            &Op::PushCall { argc, line } => Op::PushCall { argc, line },
            &Op::Return => Op::Return,
            &Op::PushResult => Op::PushResult,
            &Op::ReserveLocal(n, s) => Op::ReserveLocal(n, s),
            &Op::PropagateFailure(ref src, lineno) => {
                Op::PropagateFailure(src.clone(), lineno)
            }
            &Op::StackPush => Op::StackPush,
            &Op::PushConst(ref src) => Op::PushConst(src.clone_for_send()),
            &Op::Copy(ref dst, ref src) => Op::Copy(dst.clone(), src.clone()),
            &Op::BranchMatch(j, ref patt) => {
                Op::BranchMatch(j, patt.clone_for_send())
            }
            &Op::BranchIf(j) => Op::BranchIf(j),
            &Op::Jump(j) => Op::Jump(j),
            &Op::Label(j) => Op::Label(j),
            &Op::IfFailure(ref src, j) => Op::IfFailure(src.clone(), j),
            &Op::PushReg(src) => Op::PushReg(src),
            &Op::PopReg(dst) => Op::PopReg(dst),
            &Op::PopMatch(ref patt) => Op::PopMatch(patt.clone_for_send()),
            &Op::PopListCons => Op::PopListCons,
            &Op::PopStrCat => Op::PopStrCat,
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

struct LocalMax
{
    local_max: i8,
    stack_max: i8,
}

impl LocalMax
{
    pub fn num_locals(node: &mut AstNode) -> Lresult<(i8, i8)>
    {
        let mut max_local = LocalMax::default();
        ast2::walk_ref_mut(node, &mut max_local)?;
        Ok((max_local.local_max + 1, max_local.stack_max + 1))
    }
}

impl Default for LocalMax
{
    fn default() -> LocalMax
    {
        LocalMax {
            local_max: -1,
            stack_max: -1,
        }
    }
}

impl ast2::Op for LocalMax
{
    fn pre(&mut self, node: &mut AstNode, _mode: ast2::AstMode) -> StepResult
    {
        match node.dst {
            Reg::Local(ireg) => {
                let p = ireg.get_primary();
                if p > self.local_max {
                    self.local_max = p;
                }
            }
            _ => {}
        }
        Ok(ast2::AstStep::Ok)
    }
}

pub fn make_ops2(mut input: AstNode) -> OpVec
{
    vout!("make_ops2({:?})\n", input);
    let (num_locals, num_stack) = LocalMax::num_locals(&mut input).unwrap();
    let mut opm = OpMaker::new();
    let mut ops = make_sub_ops2(input, &mut opm);
    if num_locals > 0 || num_stack > 0 {
        ops.ops
            .insert(0, Op::ReserveLocal(num_locals.into(), num_stack.into()));
    }
    ops.ops.push(Op::PushResult);
    ops.ops.push(Op::Return);
    set_jumps(ops.ops)
}

struct OpMaker
{
    label: i16,
}

impl OpMaker
{
    pub fn new() -> OpMaker
    {
        OpMaker { label: -1 }
    }

    pub fn next_label(&mut self) -> i16
    {
        self.label += 1;
        self.label
    }
}

fn make_sub_ops2(input: AstNode, opm: &mut OpMaker) -> Oxpr
{
    let input_dst = input.dst;
    let ops = match *input.node {
        Ast::Block(lines) => {
            let mut oxprs = Vec::with_capacity(lines.len());
            for i in lines {
                oxprs.push(make_sub_ops2(i, opm));
            }
            let mut ops = Vec::with_capacity(oxprs.len());
            for mut i in oxprs {
                ops.append(&mut i.ops);
            }
            ops
        }
        Ast::ConstVal(v) => vec![Op::PushConst(v.clone())],
        Ast::Call(f, args) => make_call_ops(f, args, opm),
        Ast::Copy(src) => {
            let mut src_ops = make_sub_ops2(src, opm);
            src_ops.ops.push(Op::Copy(input_dst.clone(), src_ops.dst));
            src_ops.ops
        }
        Ast::Let(patt, _, x) => {
            let pval = if let Ast::ConstVal(pv) = *patt.node {
                pv
            } else {
                panic!("let patterns should be ConstVal! {:?}", patt);
            };
            let mut xops = make_sub_ops2(x, opm);
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
            xops.ops.push(Op::PopMatch(pval));
            // xops.ops.append(&mut failops);
            xops.ops
        }
        Ast::CopyAndSet(src, flds) => {
            let src_dst = src.dst.clone();
            let mut cs_ops = make_sub_ops2(src, opm);
            cs_ops.ops.push(Op::Copy(input.dst, src_dst));
            for f in flds.into_iter() {
                let mut f_ops = make_sub_ops2(f.v, opm);
                cs_ops.ops.append(&mut f_ops.ops);
            }
            cs_ops.ops
        }
        Ast::List(items) => make_list_ops(items, opm),
        Ast::Tuple(items) => {
            let newtup = Op::PushConst(Val::new_tuple(items.len()));
            let mut ops: Vec<Op> = vec![newtup];
            for (i, item) in items.into_iter().enumerate() {
                let subdst = input_dst.sub(i as i8);
                let mut iops = make_sub_ops2(item.v, opm);
                // should be able to generalize this
                if iops.dst != subdst {
                    iops.ops.push(Op::Copy(subdst, iops.dst));
                }
                ops.append(&mut iops.ops);
                // ops.push((Op::Copy(input_dst.clone(), iops.dst), i.1.line));
            }
            ops
        }
        Ast::StrExpr(items) => make_str_ops(items, opm),
        Ast::Ifx(cases) => make_if_ops(cases, opm),
        Ast::Matchx(Some(x), cases) => make_matchexpr_ops(x, cases, opm),
        Ast::Wildcard => vec![Op::PushConst(Val::Bool(true))],
        Ast::Return(result) => {
            let mut rops = make_sub_ops2(result, opm);
            rops.ops.push(Op::PushResult);
            rops.ops.push(Op::Return);
            rops.ops
        }
        Ast::Op2(".", base, _field) => {
            let base_ops = make_sub_ops2(base, opm);
            base_ops.ops
        }
        Ast::Id(ref _id) => {
            vec![Op::PushReg(input_dst)]
        }

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

/// 23: Void (result)
/// 24: f
/// 25: arg0
/// 26: arg1
fn make_call_ops(f: AstNode, args: Xlist, opm: &mut OpMaker) -> OpVec
{
    vout!("make_call_ops: {:?}\n", f);

    let lineno = f.loc.lineno;
    let mut call_ops = OpVec::new();

    // push the result
    call_ops.push(Op::PushConst(Val::VOID));

    // push the call
    let mut fops = make_sub_ops2(f, opm);
    call_ops.append(&mut fops.ops);

    // push the subject
    // void for now, should this go in the call later?
    call_ops.push(Op::PushConst(Val::VOID));

    // push the args
    let argc = args.len() as i16;
    let mut argops: OpVec = args
        .into_iter()
        .flat_map(|a| {
            let iargops: Oxpr = make_sub_ops2(a.v, opm);
            iargops.ops
        })
        .collect();
    call_ops.append(&mut argops);
    call_ops.push(Op::PushCall {
        argc,
        line: lineno as i16,
    });
    call_ops
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
            Op::PushConst(Val::VOID)
        }
        None => {
            // Op::Construple(dst.clone(), typ.clone(), flds.clone()),
            Op::PushConst(Val::VOID)
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

fn make_matchexpr_ops(x: AstNode, cases: Vec<Case>, opm: &mut OpMaker)
    -> OpVec
{
    vout!("make_matchexpr_ops({:?},{:?})\n", x, cases);

    let mut xops = make_sub_ops2(x, opm);
    // assume # cases is already verified > 0
    let end_label = opm.next_label();
    let case_ops: Vec<OpVec> = cases
        .into_iter()
        .map(|case| make_matchcase_ops(case, end_label, opm))
        .collect();
    vout!("made matchcase_ops =\n{:?}\n", case_ops);

    for mut case in case_ops {
        xops.ops.append(&mut case);
    }
    xops.ops.push(Op::Label(end_label));
    xops.ops
}

fn make_matchcase_ops(
    matchcase: Case,
    end_label: i16,
    opm: &mut OpMaker,
) -> OpVec
{
    let patt_val = if let Ast::ConstVal(pv) = *matchcase.cond.node {
        pv
    } else {
        panic!("match patterns should be const vals! {:?}", matchcase.cond);
    };
    let mut code_ops = make_sub_ops2(matchcase.body, opm);

    if patt_val == Val::Wildcard {
        // wildcard in the not-last case would be a semantic failure
        // and should have already been verified
        return code_ops.ops;
    }

    let case_label = opm.next_label();
    let mut patt_ops: Vec<Op> = vec![Op::BranchMatch(case_label, patt_val)];

    patt_ops.append(&mut code_ops.ops);
    patt_ops.push(Op::Jump(end_label));
    patt_ops.push(Op::Label(case_label));
    patt_ops
}

/// Interleave the condition checks w/ the bodies. Something like:
///   eval cond a
///   jump_if_not(cond a, end of body a)
///   eval cond b
///   jump_if_not(cond b, end of body b)
///   body c
fn make_if_ops(cases: Vec<Case>, opm: &mut OpMaker) -> OpVec
{
    let end_label = opm.next_label();
    let case_ops: Vec<(Oxpr, Oxpr)> = cases
        .into_iter()
        .map(|case| {
            let case_label = opm.next_label();
            let mut cond_ops = make_sub_ops2(case.cond, opm);
            cond_ops.ops.push(Op::BranchIf(case_label));
            let mut body_ops = make_sub_ops2(case.body, opm);
            body_ops.ops.push(Op::Jump(end_label));
            body_ops.ops.push(Op::Label(case_label));
            (cond_ops, body_ops)
        })
        .collect();

    let mut ops: OpVec = case_ops
        .into_iter()
        .flat_map(|mut case| {
            case.0.ops.append(&mut case.1.ops);
            case.0.ops
        })
        .collect();
    ops.push(Op::Label(end_label));
    ops
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

fn make_list_ops(items: Xlist, opm: &mut OpMaker) -> OpVec
{
    let mut ops = vec![Op::PushConst(Val::Nil)];
    for i in items.into_iter().rev() {
        let mut listops = make_sub_ops2(i.v, opm);
        ops.append(&mut listops.ops);
        ops.push(Op::PopListCons);
    }
    ops
}

fn make_str_ops(items: Vec<AstNode>, opm: &mut OpMaker) -> OpVec
{
    let mut ops: Vec<Op> = Vec::with_capacity(items.len());
    ops.push(Op::PushConst(Val::empty_str()));
    for i in items {
        let mut strops = make_sub_ops2(i, opm);
        ops.append(&mut strops.ops);
        ops.push(Op::PopStrCat);
    }
    ops
}

fn set_jumps(ops: OpVec) -> OpVec
{
    let mut jumps = Vec::with_capacity(ops.len());
    let mut label_loc = HashMap::with_capacity(ops.len());
    for op in ops.into_iter() {
        if let Op::Label(lbl) = op {
            label_loc.insert(lbl, jumps.len() as i16);
        } else {
            jumps.push(op);
        }
    }

    for (i, op) in jumps.iter_mut().enumerate() {
        match op {
            Op::Jump(ref mut lbl)
            | Op::BranchIf(ref mut lbl)
            | Op::BranchMatch(ref mut lbl, _) => {
                let loc = label_loc.get(lbl).unwrap();
                *lbl = *loc - (i as i16);
            }
            // ignore non-jumpy ops
            _ => {}
        }
    }

    jumps
}

pub fn assign_registers(input: &mut AstNode) -> Lresult<()>
{
    vout!("assign_registers({:?})\n", input);
    Registration::assign_registers(input)
}

pub struct Registration {}

impl Registration
{
    pub fn new() -> Registration
    {
        Registration {}
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

    fn assign_registers(node: &mut AstNode) -> Lresult<()>
    {
        let first_dst = node.dst.clone();

        match &mut *node.node {
            Ast::Block(ref mut items) => {
                // copy the block's dst to the last item in the block
                if let Some(item) = items.last_mut() {
                    Self::set_dst_or_copy(item, node.dst);
                }
                for i in items.iter_mut() {
                    Self::assign_registers(i)?;
                }
            }
            Ast::Call(_, ref mut args) => {
                for a in args.iter_mut() {
                    Self::assign_registers(&mut a.v)?;
                }
            }
            Ast::Let(ref mut lhs, _, ref mut rhs) => {
                let pval = Self::make_pattern_val(lhs)?;
                *lhs.node = Ast::ConstVal(pval);
                Self::assign_registers(rhs)?;
            }
            Ast::Matchx(ref mut match_input, ref mut cases) => {
                if let Some(ref mut mi) = match_input {
                    Self::assign_registers(mi)?;
                }
                for case in cases.iter_mut() {
                    let pval = Self::make_pattern_val(&case.cond)?;
                    *case.cond.node = Ast::ConstVal(pval);
                    Self::set_dst_or_copy(&mut case.body, node.dst);
                    Self::assign_registers(&mut case.body)?;
                }
            }
            Ast::Ifx(ref mut cases) => {
                for case in cases.iter_mut() {
                    if *case.cond.node == Ast::VOID {
                        case.cond.dst = Reg::Void;
                    }
                    Self::assign_registers(&mut case.cond)?;
                    Self::set_dst_or_copy(&mut case.body, node.dst);
                    Self::assign_registers(&mut case.body)?;
                }
            }
            Ast::StrExpr(ref mut items) => {
                for i in items.iter_mut() {
                    Self::assign_registers(i)?;
                }
            }
            Ast::Tuple(ref mut items) => {
                for item in items.iter_mut() {
                    Self::assign_registers(&mut item.v)?;
                }
            }
            Ast::List(ref mut items) => {
                // not really sure what's going on here or why
                for i in items.iter_mut() {
                    Self::assign_registers(&mut i.v)?;
                }
            }
            Ast::Op2(".", ref mut base, ref field) => {
                Self::assign_registers(base)?;
                let field_idx = match &*field.node {
                    Ast::Id(idx_str) => {
                        idx_str.parse().map_err(|e| {
                            rustfail!(
                                "leema_failure",
                                "{} for {:?}",
                                e,
                                idx_str,
                            )
                        })?
                    }
                    Ast::ConstVal(Val::Int(i)) => *i as i8,
                    Ast::DataMember(_, i) => *i as i8,
                    other => {
                        panic!(
                            "field name is not an identifier: {:?} @ {:?}",
                            other, field.loc
                        )
                    }
                };
                node.dst = base.dst.sub(field_idx);
            }
            Ast::Op2(op2, ref mut a, ref mut b) => {
                panic!("assign registers to op2? {:?} {} {:?}", a, op2, b);
            }
            Ast::CopyAndSet(ref mut src, ref mut fields) => {
                Self::assign_registers(src)?;
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
                Self::assign_registers(result)?;
            }
            // nothing else to do
            Ast::Id(_) | Ast::ConstVal(_) => {}
            // these shouldn't be here
            Ast::Canonical(_)
            | Ast::DataMember(_, _)
            | Ast::DefConst(_, _)
            | Ast::DefFunc(_, _, _, _)
            | Ast::DefImpl(_, _, _)
            | Ast::DefTrait(_, _)
            | Ast::DefMacro(_, _, _)
            | Ast::DefType(_, _, _)
            | Ast::FuncType(_, _)
            | Ast::Generic(_, _)
            | Ast::ModAction(_, _)
            | Ast::Wildcard => {} // do nothing
            Ast::Alias(_, _) => {
                panic!("unexpected alias {:?}, at {:?}", node.node, node.loc);
            }
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

    fn make_pattern_val(node: &AstNode) -> Lresult<Val>
    {
        let pval = match &*node.node {
            Ast::Id(id) => {
                if node.dst == Reg::Undecided {
                    panic!("unexpected undecided pattern reg: {}", id);
                }
                Val::PatternVar(node.dst)
            }
            Ast::Tuple(ref items) => {
                let tval: Lresult<Struple2<Val>> = items
                    .iter()
                    .map(|item| {
                        let pk = item.k.map(|k| Lstr::Sref(k));
                        let pv = ltry!(Self::make_pattern_val(&item.v));
                        Ok(StrupleItem::new(pk, pv))
                    })
                    .collect();
                Val::Tuple(tval?)
            }
            Ast::List(ref items) => {
                let mut result = Val::Nil;
                for i in items.iter().rev() {
                    let next = ltry!(Self::make_pattern_val(&i.v));
                    result = list::cons(next, result);
                }
                result
            }
            Ast::ConstVal(ref val) => val.clone(),
            Ast::Wildcard => Val::Wildcard,
            Ast::Op2(";", ref a, ref b) => {
                let pa = ltry!(Self::make_pattern_val(a));
                let pb = ltry!(Self::make_pattern_val(b));
                list::cons(pa, pb)
            }
            pnode => {
                // do nothing with other pattern values
                return Err(lfail!(
                    failure::Mode::CodeFailure,
                    "cannot make a pattern val",
                    "pattern": ldebug!(pnode),
                    "line": ldisplay!(node.loc.lineno),
                ));
            }
        };
        Ok(pval)
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

        assert_eq!(4, code.len());
        let x = vec![
            Op::ReserveLocal(4, 0),
            Op::PushConst(Val::Int(9)),
            Op::PushResult,
            Op::Return,
        ];
        assert_eq!(x, code);
    }

    #[test]
    fn test_code_lists()
    {
        let input = r#"
        import /io

        func is_empty:Bool :: l:[Int] ->
            match
            |[] -> True
            |_ -> False
            --
        --

        func main ->
            let e := is_empty([4, 8, 3])
            io.print("is empty? $e\n")
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
