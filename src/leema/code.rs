use crate::leema::ast2::{self, Ast, AstNode, Case, StepResult, Xlist};
use crate::leema::failure::{self, Lresult};
use crate::leema::fiber;
use crate::leema::frame;
use crate::leema::list;
use crate::leema::lstr::Lstr;
use crate::leema::reg::{Ireg, Reg};
use crate::leema::rsrc;
use crate::leema::sendclone::SendClone;
use crate::leema::struple::{Struple2, StrupleItem};
use crate::leema::val::{self, Type, Val};
use crate::leema::worker::RustFuncContext;

use std::collections::HashMap;
use std::fmt;
use std::marker;

const CODEFAIL: &str = "codegen_failure";

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

    /// Pop N elements off the stack and put them in a new tuple
    PushTuple(i8),

    /// Pop a struct off the stack and push one of its fields onto stack
    PushField(i8),

    /// Pop stack top into Nth field of object at top of stack
    PopIntoField(i8),

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

    /// Reserve .0 registers for locals
    ReserveLocal(i16),
    PropagateFailure(u16),
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
            Op::PushCall { argc, line } => {
                Op::PushCall {
                    argc: *argc,
                    line: *line,
                }
            }
            Op::Return => Op::Return,
            Op::PushResult => Op::PushResult,
            Op::ReserveLocal(n) => Op::ReserveLocal(*n),
            Op::PropagateFailure(lineno) => Op::PropagateFailure(*lineno),
            Op::StackPush => Op::StackPush,
            Op::PushConst(src) => Op::PushConst(src.clone_for_send()),
            Op::Copy(dst, src) => Op::Copy(*dst, *src),
            Op::BranchMatch(j, patt) => {
                Op::BranchMatch(*j, patt.clone_for_send())
            }
            Op::BranchIf(j) => Op::BranchIf(*j),
            Op::Jump(j) => Op::Jump(*j),
            Op::Label(j) => Op::Label(*j),
            Op::IfFailure(src, j) => Op::IfFailure(*src, *j),
            Op::PushReg(src) => Op::PushReg(*src),
            Op::PopReg(dst) => Op::PopReg(*dst),
            Op::PopIntoField(fld) => Op::PopIntoField(*fld),
            Op::PushField(fld) => Op::PushField(*fld),
            Op::PopMatch(patt) => Op::PopMatch(patt.clone_for_send()),
            Op::PopListCons => Op::PopListCons,
            Op::PopStrCat => Op::PopStrCat,
            Op::PushTuple(n) => Op::PushTuple(*n),
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
    /// TODO Remove rsrc_idx field from 2nd param
    Iop(rsrc::IopAction, Option<i8>),
}

impl Code
{
    pub fn is_leema(&self) -> bool
    {
        matches!(self, Code::Leema(_))
    }

    pub fn is_rust(&self) -> bool
    {
        matches!(self, Code::Rust(_) | Code::Rust2(_))
    }

    /// TODO remove the rsrc_idx field from this
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
            Code::Leema(_) => write!(f, "LeemaCode"),
            Code::Rust(_) => write!(f, "RustCode"),
            Code::Rust2(_) => write!(f, "RustCode2"),
            Code::Iop(_, _) => write!(f, "IopCode"),
        }
    }
}

impl fmt::Debug for Code
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self {
            Code::Leema(ref ops) => {
                writeln!(f, "Code::Leema")?;
                for (i, op) in ops.iter().enumerate() {
                    writeln!(f, "{:3} {:?}", i, op)?;
                }
                Ok(())
            }
            Code::Rust(_) => write!(f, "Code::Rust"),
            Code::Rust2(_) => write!(f, "Code::Rust2"),
            Code::Iop(_, _) => write!(f, "Code::Iop"),
        }
    }
}

impl Clone for Code
{
    fn clone(&self) -> Code
    {
        match self {
            Code::Leema(ref ops) => Code::Leema(ops.clone()),
            Code::Rust(rf) => Code::Rust(*rf),
            Code::Rust2(rf) => Code::Rust2(*rf),
            Code::Iop(ref iopf, ref rsrc_idx) => Code::Iop(*iopf, *rsrc_idx),
        }
    }
}

struct LocalMax
{
    local_max: i8,
}

impl LocalMax
{
    pub fn num_locals(node: &mut AstNode) -> Lresult<i8>
    {
        let mut max_local = LocalMax::default();
        ast2::walk_ref_mut(node, &mut max_local)?;
        Ok(max_local.local_max + 1)
    }

    pub fn check_max(&mut self, i: Ireg)
    {
        let p = i.get_primary();
        if p > self.local_max {
            self.local_max = p;
        }
    }
}

impl Default for LocalMax
{
    fn default() -> LocalMax
    {
        LocalMax { local_max: -1 }
    }
}

impl ast2::Op for LocalMax
{
    fn pre(&mut self, node: &mut AstNode, _mode: ast2::AstMode) -> StepResult
    {
        if let Reg::Local(ireg) = node.dst {
            self.check_max(ireg);
        }
        if let Ast::ConstVal(v) = &*node.node {
            ltry!(v.walk_ref(self));
        }
        Ok(ast2::AstStep::Ok)
    }
}

impl val::Op for LocalMax
{
    fn pre_ref(&mut self, v: &Val) -> Lresult<()>
    {
        if let Val::Reg(Reg::Local(ireg)) = v {
            self.check_max(*ireg);
        }
        Ok(())
    }
}

pub fn make_ops2(mut input: AstNode) -> OpVec
{
    vout!("make_ops2({:?})\n", input);
    let num_locals = LocalMax::num_locals(&mut input).unwrap();
    let mut opm = OpMaker::new();
    let mut ops = make_sub_ops2(input, &mut opm);
    if num_locals > 0 {
        ops.ops.insert(0, Op::ReserveLocal(num_locals.into()));
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
        Ast::ConstVal(v) => vec![Op::PushConst(v)],
        Ast::Call(f, args) => make_call_ops(f, args, opm),
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
            if let Val::Reg(dst) = pval {
                xops.ops.push(Op::PopReg(dst));
            } else {
                xops.ops.push(Op::PopMatch(pval));
            }
            // xops.ops.append(&mut failops);
            xops.ops
        }
        Ast::CopyAndSet(src, flds) => {
            // should leave src on top of the stack
            let mut cs_ops = make_sub_ops2(src, opm);
            // push each field to the stack and pop it into the field of src
            for f in flds.into_iter() {
                let fld_idx = if let Reg::Param(reg) = f.v.dst {
                    reg.get_primary()
                } else {
                    // this function should return an Lresult
                    Lresult::<()>::Err(lfail!(
                        failure::Mode::StaticLeemaFailure,
                        "expected param reg",
                        "reg": ldebug!(f.v.dst),
                        "line": ldebug!(input.loc),
                    ))
                    .unwrap();
                    panic!("failure");
                };
                let mut f_ops = make_sub_ops2(f.v, opm);
                f_ops.ops.push(Op::PopIntoField(fld_idx));
                cs_ops.ops.append(&mut f_ops.ops);
            }
            cs_ops.ops
        }
        Ast::List(items) => make_list_ops(items, opm),
        Ast::StrExpr(items) => make_str_ops(items, opm),
        Ast::Tuple(items) => make_tuple_ops(items, opm),
        Ast::Ifx(cases) => make_if_ops(cases, opm),
        Ast::Matchx(Some(x), cases) => make_matchexpr_ops(x, cases, opm),
        Ast::Wildcard => vec![Op::PushConst(Val::Bool(true))],
        Ast::Return(result) => {
            let mut rops = make_sub_ops2(result, opm);
            rops.ops.push(Op::PushResult);
            rops.ops.push(Op::Return);
            rops.ops
        }
        Ast::Op2(".", base, field) => {
            let mut base_ops = make_sub_ops2(base, opm);
            match *field.node {
                Ast::DataMember(f) => {
                    base_ops.ops.push(Op::PushField(f as i8));
                }
                other => {
                    panic!("dunno what this is: {:?}", other);
                }
            }
            base_ops.ops
        }
        Ast::Id(_id) => {
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
    let mut call_ops = OpVec::with_capacity(args.len() + 6);

    // push the result
    call_ops.push(Op::PushConst(Val::VOID));

    // push the call
    let mut fops = make_sub_ops2(f, opm);
    call_ops.append(&mut fops.ops);

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
    call_ops.push(Op::PropagateFailure(lineno));
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
    let case_ops = cases.into_iter().map(|case| {
        let case_label = opm.next_label();
        let mut cond_ops = make_sub_ops2(case.cond, opm);
        cond_ops.ops.push(Op::BranchIf(case_label));
        let mut body_ops = make_sub_ops2(case.body, opm);
        body_ops.ops.push(Op::Jump(end_label));
        body_ops.ops.push(Op::Label(case_label));
        (cond_ops, body_ops)
    });

    let mut ops: OpVec = case_ops
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

/// Make the ops to construct a new tuple
/// Later optimize this for a constant tuple to just be a PushConst
fn make_tuple_ops(items: Xlist, opm: &mut OpMaker) -> OpVec
{
    let mut ops: Vec<Op> = vec![];
    let num_items = items.len();
    for item in items.into_iter() {
        let mut iops = make_sub_ops2(item.v, opm);
        ops.append(&mut iops.ops);
    }
    ops.push(Op::PushTuple(num_items as i8));
    ops
}

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
        }
        node.dst = dst;
    }

    fn assign_registers(node: &mut AstNode) -> Lresult<()>
    {
        match &mut *node.node {
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
                    Self::assign_registers(&mut case.body)?;
                }
            }
            Ast::Ifx(ref mut cases) => {
                for case in cases.iter_mut() {
                    if *case.cond.node == Ast::VOID {
                        case.cond.dst = Reg::Void;
                    }
                    Self::assign_registers(&mut case.cond)?;
                    Self::assign_registers(&mut case.body)?;
                }
            }
            Ast::Op2(".", ref mut base, ref _field) => {
                Self::assign_registers(base)?;
            }
            Ast::Op2(op2, ref mut a, ref mut b) => {
                panic!("assign registers to op2? {:?} {} {:?}", a, op2, b);
            }
            Ast::Alias(_, _) => {
                panic!("unexpected alias {:?}, at {:?}", node.node, node.loc);
            }
            Ast::Op1(op1, x) => {
                panic!("unexpected Op1 {} {:?}", op1, x);
            }
            Ast::Type(t) => {
                panic!("assigning a register to a type? {:?}", t);
            }
            _ => {}
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
                Val::Reg(node.dst)
            }
            Ast::Tuple(ref items) => {
                let tval: Lresult<Struple2<Val>> = items
                    .iter()
                    .map(|item| {
                        let pk = item.k.map(Lstr::Sref);
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
        let loc = Loc::new(8, 7);
        let mut node = AstNode::new(Ast::ConstVal(Val::Int(9)), loc);
        node.dst = Reg::local(3);
        let code = code::make_ops2(node);

        assert_eq!(4, code.len());
        let x = vec![
            Op::ReserveLocal(4),
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
