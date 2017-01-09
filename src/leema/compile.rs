use leema::val::{Val,SexprType,Type};
use leema::list;
use leema::log;
use leema::module::{ModuleInterface, ModKey};
use leema::sexpr;
use leema::scope::Scope;
use leema::reg::{Reg, Ireg};
use leema::ast;
use leema::loader::{Interloader};
use leema::code::{self, CodeKey, CodeMap, Code, OpVec};

use std::collections::{HashMap};
use std::sync::Arc;
use std::rc::Rc;
use std::mem;
use std::io::{stderr, Write};


#[derive(Debug)]
#[derive(PartialEq)]
pub enum Source
{
    Block(Vec<Iexpr>),
    BooleanAnd(Box<Iexpr>, Box<Iexpr>),
    BooleanOr(Box<Iexpr>, Box<Iexpr>),
    ConstVal(Val),
    Call(Box<Iexpr>, Box<Iexpr>),
    Constructor(Type),
    BoundVal(Reg),
    Fail(Box<Iexpr>, Box<Iexpr>),
    FieldAccess(Box<Iexpr>, i8),
    Fork(Box<Iexpr>, Box<Iexpr>),
    MatchExpr(Box<Iexpr>, Box<Iexpr>),
    MatchCase(Box<Iexpr>, Box<Iexpr>, Box<Iexpr>),
    CaseExpr(Box<Iexpr>, Box<Iexpr>, Box<Iexpr>),
    IfStmt(Box<Iexpr>, Box<Iexpr>, Box<Iexpr>),
    List(Vec<Iexpr>),
    PatternVar(Reg),
    Str(Vec<Iexpr>),
    Tuple(Vec<Iexpr>),
    Return(Box<Iexpr>),
    Void,
}

impl Source
{
    pub fn type_of(src: &Source) -> Type
    {
        match src {
            &Source::Void => Type::Void,
            _ => Type::Unknown,
        }
    }
}

#[derive(Debug)]
#[derive(PartialEq)]
pub struct Iexpr
{
    pub dst: Reg,
    pub typ: Type,
    pub src: Source,
}

impl Iexpr
{
    pub fn new(src: Source) -> Iexpr
    {
        Iexpr{
            dst: Reg::Undecided,
            typ: Source::type_of(&src),
            src: src,
        }
    }

    fn new_block(code: Vec<Iexpr>) -> Iexpr
    {
vout!("new_block> {:?}\n", code);
        let block_type = match code.last() {
            None => {
                Type::Void
            }
            Some(ix) => {
                ix.typ.clone()
            }
        };
        Iexpr{
            dst: Reg::Undecided,
            typ: block_type,
            src: Source::Block(code),
        }
    }

    fn noop() -> Iexpr
    {
        Iexpr{
            dst: Reg::Void,
            typ: Type::Void,
            src: Source::Void,
        }
    }

    fn const_val(src: Val) -> Iexpr
    {
        Iexpr{
            dst: Reg::Undecided,
            typ: src.get_type(),
            src: Source::ConstVal(src),
        }
    }

    fn bound_val(src: Reg, t: Type) -> Iexpr
    {
        Iexpr{
            // TODO:
            // set to void to avoid reassignment
            // the src is the one that we care about
            // right?
            dst: Reg::Undecided,
            typ: t,
            src: Source::BoundVal(src),
        }
    }

    fn pattern_var(dst: Reg) -> Iexpr
    {
        Iexpr{
            dst: Reg::Undecided,
            typ: Type::Unknown,
            src: Source::PatternVar(dst),
        }
    }

    fn call(t: Type, f: Iexpr, args: Iexpr) -> Iexpr
    {
        Iexpr{
            dst: Reg::Undecided,
            typ: t,
            src: Source::Call(Box::new(f), Box::new(args)),
        }
    }

    fn constructor(t: Type) -> Iexpr
    {
        Iexpr{
            dst: Reg::new_reg(0),
            typ: t.clone(),
            src: Source::Constructor(t),
        }
    }

    fn fork(dst: Reg, t: Type, f: Iexpr, args: Iexpr) -> Iexpr
    {
        Iexpr{
            dst: dst,
            typ: t,
            src: Source::Fork(Box::new(f), Box::new(args)),
        }
    }

    fn match_expr(x: Iexpr, cases: Iexpr) -> Iexpr
    {
        Iexpr{
            dst: Reg::Undecided,
            typ: cases.typ.clone(),
            src: Source::MatchExpr(
                Box::new(x),
                Box::new(cases),
            ),
        }
    }

    fn match_case(pattern: Iexpr, code: Iexpr, next: Iexpr) -> Iexpr
    {
        Iexpr{
            dst: Reg::Undecided,
            typ: code.typ.clone(),
            src: Source::MatchCase(
                Box::new(pattern),
                Box::new(code),
                Box::new(next),
            ),
        }
    }

    fn case_expr(test: Iexpr, truth: Iexpr, lies: Iexpr) -> Iexpr
    {
        if truth.typ != lies.typ {
            panic!("Mismatched case types: {:?}!={:?}", truth.typ, lies.typ);
        }
        Iexpr{
            dst: Reg::Undecided,
            typ: truth.typ.clone(),
            src: Source::CaseExpr(
                Box::new(test),
                Box::new(truth),
                Box::new(lies),
            ),
        }
    }

    fn ifstmt(test: Iexpr, truth: Iexpr, lies: Iexpr) -> Iexpr
    {
        Iexpr{
            dst: Reg::Void,
            typ: Type::Void, // if statements are untyped
            src: Source::IfStmt(
                Box::new(test),
                Box::new(truth),
                Box::new(lies)
            ),
        }
    }

    fn str(items: Vec<Iexpr>) -> Iexpr
    {
        Iexpr{
            dst: Reg::Undecided,
            typ: Type::Str,
            src: Source::Str(items),
        }
    }

    fn tuple(items: Vec<Iexpr>) -> Iexpr
    {
        let mut types = vec![];
        for i in &items {
            types.push(i.typ.clone());
        }
        Iexpr{
            dst: Reg::Undecided, 
            typ: Type::Tuple(types),
            src: Source::Tuple(items),
        }
    }

    fn list(items: Vec<Iexpr>) -> Iexpr
    {
        let item_type = items.iter().fold(Type::Unknown, |old_t, new_x| {
            if old_t == Type::Unknown {
                new_x.typ.clone()
            } else if old_t == new_x.typ {
                old_t
            } else {
                panic!("Mixed list types: {:?} != {:?}", old_t, new_x);
            }
        });
        Iexpr{
            dst: Reg::Undecided,
            typ: Type::StrictList(Box::new(item_type)),
            src: Source::List(items),
        }
    }
}


#[derive(Clone)]
#[derive(Debug)]
pub struct Binding(Reg, Type);

#[derive(Debug)]
pub struct StaticSpace
{
    scope: Scope,
    typefields: HashMap<Type, Vec<(Arc<String>, Type)>>,
    pub interlib: HashMap<CodeKey, Arc<Iexpr>>,
    pub lib: CodeMap,
    last_reg: Reg,
}

impl StaticSpace
{
    // pub fn new(key: &ModKey) -> StaticSpace
    pub fn new(mi: Rc<ModuleInterface>) -> StaticSpace
    {
        StaticSpace{
            scope: Scope::new(),
            typefields: HashMap::new(),
            interlib: HashMap::new(),
            lib: HashMap::new(),
            last_reg: Reg::Undecided,
        }
    }

    pub fn define_func(&mut self, name: Arc<String>, typ: Type, code: Code)
    {
        vout!("define {}:={:?}\n", name, typ);
        let key = if *name == "main" {
            CodeKey::Main
        } else if *name == "*script*" {
            CodeKey::Script
        } else {
            self.scope.assign_label(Reg::Lib, &*name, typ);
            CodeKey::Name(name)
        };
        match code {
            Code::Rust(r) => {
                self.lib.insert(key, Code::Rust(r));
            }
            Code::Leema(l) => {
                self.lib.insert(key, Code::Leema(l));
            }
            Code::Inter(i) => {
                self.interlib.insert(key, i.clone());
            }
        };
    }

    pub fn has_main(&self) -> bool
    {
        self.lib.contains_key(&CodeKey::Main)
            || self.interlib.contains_key(&CodeKey::Main)
    }

    pub fn has_script(&self) -> bool
    {
        self.lib.contains_key(&CodeKey::Script)
            || self.interlib.contains_key(&CodeKey::Script)
    }

    pub fn take_lib(&mut self) -> CodeMap
    {
        let mut tmp = HashMap::new();
        mem::swap(&mut self.lib, &mut tmp);
        tmp
    }

    pub fn compile(&mut self, expr: Val) -> Iexpr
    {
        let mut i = self.precompile(expr);
        self.assign_registers(&mut i);
        i
    }

    pub fn precompile(&mut self, val: Val) -> Iexpr
    {
        match val {
            Val::Cons(_, _) => {
                self.precompile_list(val)
            }
            Val::Nil => {
                Iexpr::const_val(Val::Nil)
            }
            Val::Tuple(tup) => {
                self.precompile_tuple(tup)
            }
            Val::Sexpr(st, x) => {
                self.precompile_sexpr(st, *x)
            }
            Val::Id(id) => {
                self.precompile_id(&*id)
            }
            Val::Type(Type::Id(id)) => {
                self.precompile_id(&*id)
            }
            Val::CallParams => {
                Iexpr{
                    dst: Reg::Params,
                    typ: Type::Tuple(self.scope.function_param_types().clone()),
                    src: Source::ConstVal(Val::CallParams),
                }
            }
            Val::Failure(_, _, _) => {
                panic!("Should not compile failures directly");
            }
            _ => {
                Iexpr::const_val(val)
            }
        }
    }

    pub fn precompile_sexpr(&mut self, st: SexprType, expr: Val) -> Iexpr
    {
        match st {
            SexprType::Let => {
                self.precompile_let(expr)
            }
            SexprType::Fork => {
                //self.precompile_fork(expr);
                Iexpr::new(Source::Void)
            }
            SexprType::BlockExpr => {
                self.precompile_block(expr)
            }
            SexprType::Call => {
                self.precompile_call(expr)
            }
            SexprType::DefFunc => {
                self.precompile_defunc(expr);
                Iexpr::new(Source::Void)
            }
            SexprType::Fail => {
                self.precompile_fail(expr)
            }
            SexprType::StrExpr => {
                self.precompile_str(expr)
            }
            SexprType::FieldAccess => {
                self.precompile_fieldaccess(expr)
            }
            /*
            SexprType::BooleanAnd(a, b) => {
                let ia = Box::new(self.precompile(*a));
                let ib = Box::new(self.precompile(*b));
                Iexpr::new(Source::BooleanAnd(ia, ib))
            }
            SexprType::BooleanOr(a, b) => {
                let ia = Box::new(self.precompile(*a));
                let ib = Box::new(self.precompile(*b));
                Iexpr::new(Source::BooleanOr(ia, ib))
            }
            */
            SexprType::MatchExpr => {
                self.precompile_matchx(expr)
            }
            SexprType::CaseExpr => {
                self.precompile_casex(expr)
            }
            SexprType::IfStmt => {
                self.precompile_ifstmt(expr)
            }
            SexprType::MatchFailed => {
                self.precompile_matchfailed(expr)
            }
            SexprType::Return => {
                let r = self.precompile(expr);
                Iexpr::new(Source::Return(Box::new(r)))
            }
            /*
            TODO: make this work at some point
            SexprType::LessThan3(oreq1, oreq2) => {
                let lt1func = if oreq1 {
                    "less_than_equal".to_string()
                } else {
                    "less_than".to_string()
                };
                let lt2func = if oreq2 {
                    "less_than_equal".to_string()
                } else {
                    "less_than".to_string()
                };

                let args1 = Val::Tuple(vec![*a, *b.clone()]);
                let args2 = Val::Tuple(vec![*b, *c]);
                let lt1 = sexpr::call(lt1func, args1);
                let lt2 = sexpr::call(lt2func, args2);

                self.precompile(sexpr::binaryop(
                    "and".to_string(), lt1, lt2
                ))
            }
            */
            SexprType::Comparison => {
                panic!("Can't compile Comparison yet");
            }
            SexprType::Import => {
                panic!("Can't compile Import");
            }
            SexprType::DefMacro => {
                // ignore macros, they're handled elsewhere
                // panic!("Macros should have been handled elsewhere");
                Iexpr::noop()
            }
            SexprType::DefStruct => {
                // ignore defstruct here. will deal with them earlier
                panic!("structs should have been handled elsewhere");
            }
        }
    }

    pub fn precompile_let(&mut self, expr: Val) -> Iexpr
    {
        let (lhs, e2) = list::take(expr);
        let (rhs, _ ) = list::take(e2);
vout!("compile let {} := {}\n", lhs, rhs);
        let name = lhs.to_str();

        let let_ps = self.scope.take_failure_check(&name);

        let mut irhs = self.precompile(rhs);
        let mut is_new = false;
        match self.scope.lookup_label(&name) {
            None => {
                // label not already defined. assign it.
                // using this flag to escape immutable borrow in match
                is_new = true;
            }
            Some((dst, typ)) if self.scope.is_failed(&name) => {
                irhs.dst = dst.clone();
                if irhs.typ != *typ {
                    panic!("Recovered type does not match original type: {}",
                        name);
                }
            }
            Some((_, _)) => {
                // already defined, but not a failure so blow up
                panic!("{} was already defined", name);
            }
        }
        if is_new {
            // TODO: run this through the same logic as pattern matching
            irhs.dst = Reg::new_reg(self.scope.nextreg());
            self.last_reg = irhs.dst.clone();

            self.scope.assign_label(
                irhs.dst.clone(), &*name, irhs.typ.clone()
            );
        }
        match let_ps {
            Some(l) => {
                Iexpr::new_block(vec![
                    irhs,
                    self.precompile(l),
                ])
            }
            None => irhs,
        }
    }

    pub fn precompile_fork(&mut self, name: Arc<String>, val: Val) -> Iexpr
    {
vout!("compile fork {} := {}\n", name, val);
        if self.scope.is_label(&name) {
            panic!("{} was already defined", name);
        }

        let mut valexpr = self.compile(val);
        let dst = Reg::new_reg(self.scope.nextreg());
        //valexpr.dst = Reg::R1(self.scope.nextreg());
        self.last_reg = dst.clone();
        self.scope.assign_label(dst.clone(), &*name, valexpr.typ.clone());

        let fork_name = Arc::new(format!("fork_{}", name));

        let valexpr_type = valexpr.typ.clone();
        self.define_func(
            fork_name.clone(),
            Type::f(vec![], valexpr_type.clone()),
            Code::Inter(Arc::new(valexpr)),
        );
        // shouldn't need to type define a function that will
        // only be called here
        // self.define(fexpr.dst, name, fexpr.typ.clone());

        let fork_name_ix = self.precompile(Val::Id(fork_name));

        // now make an expression to call the new function
        let cargs = Iexpr::tuple(vec![]);
        let forkx = Iexpr::fork(dst, valexpr_type, fork_name_ix, cargs);
        forkx
    }

    /*
    pub fn precompile_defstruct(&mut self, expr: Val)
    {
        let (nameval, mut fields) = list::take(expr);
vout!("precompile_defstruct({:?},{:?})\n", nameval, fields);

        let nametype = nameval.to_type();
        let name = match nametype {
            Type::Id(namestr) => namestr,
            _ => {
                panic!("Not a type name: {:?}", nametype);
            }
        };

        if self.scope.is_type(&name) {
            panic!("Type already defined: {}", name);
        }

        let mut constructor_types = vec![];
        let mut typefields = vec![];
        while let Val::Cons(head, tail) = fields {
            let (fldname, fldtype) = Val::split_typed_id(&*head);
            constructor_types.push(fldtype.clone());
            typefields.push((fldname.to_str(), fldtype));
            fields = *tail;
        }
        let nfields = constructor_types.len() as i8;
        let struct_type = Type::Struct(name.clone(), nfields);
        self.scope.define_type(&name, &struct_type);
        self.typefields.insert(struct_type.clone(), typefields);
        let fixpr = Iexpr::constructor(struct_type.clone());
        vout!("typefields now: {:?}\n", self.typefields);

        self.define_func(
            name.clone(),
            Type::f(constructor_types, struct_type),
            Code::Inter(Arc::new(fixpr)),
        );
    }
    */

    pub fn precompile_defunc(&mut self, func: Val)
    {
        let (nameval, f1) = list::take(func);
        let (params, f2) = list::take(f1);
        let (result, f3) = list::take(f2);
        let (code, f4) = list::take(f3);
        let ps = list::head(f4);
        let name = nameval.to_str();

        let (inf_arg_types, funcx) = {
            let mut argtypes = vec![];
            let mut i: i8 = 0;
            let full_types = false;

            self.scope.push_function_scope(&*name, ps);
            let mut next_param = params;
            while next_param != Val::Nil {
                let (head, tail) = list::take(next_param);
                if !head.is_id() {
                    panic!("func param not an id with type {:?}", head);
                }
                let (pid, raw_ptype) = Val::split_typed_id(&head);
                let var_name = pid.to_str();
                let ptype = self.precompile_type(raw_ptype);
vout!("precompiled {} as {:?}\n", var_name, ptype);
                argtypes.push(ptype.clone());
                self.scope.assign_label(
                    Reg::new_param(i),
                    &*var_name,
                    ptype,
                );
                next_param = tail;
                i += 1;
            }
            if !result.is_type() {
                panic!("Result is not a type? {:?}", result);
            }
            let rt = self.precompile_type(result.to_type());
vout!("{} type: {:?} -> {:?}\n", name, argtypes, rt);

            // necessary for recursion
            // if the type isn't predefined, can't recurse
            let ftype = Type::Func(argtypes.clone(), Box::new(rt));
            self.scope.assign_label(Reg::Lib, &*name, ftype);
            self.scope.set_function_param_types(&argtypes);

            let mut fexpr = self.compile(code);
print!("fexpr> {:?} : {:?}\n", fexpr, fexpr.typ);
            let final_arg_types = {
                let input_arg_tuple = Type::Tuple(argtypes);
                let final_arg_tuple =
                    self.scope.find_inferred_type(&input_arg_tuple);
                Type::tuple_items(final_arg_tuple.clone())
            };
            self.scope.pop_function_scope();
            (final_arg_types, fexpr)
        };

        let final_ftype = Type::f(inf_arg_types, funcx.typ.clone());
print!("final_ftype> {:?}\n", final_ftype);
        self.define_func(
            name.clone(),
            final_ftype,
            Code::Inter(Arc::new(funcx)),
        );
    }

    pub fn precompile_block(&mut self, items: Val) -> Iexpr
    {
vout!("pc block> {:?}\n", items);
        let mut bvec = self.precompile_list_to_vec(items);
        bvec.retain(|i| {
            i.src != Source::Void
        });
        if bvec.is_empty() {
            return Iexpr{
                dst: Reg::Void,
                typ: Type::Void,
                src: Source::Void,
            };
        }
        Iexpr::new_block(bvec)
    }

    pub fn precompile_matchx(&mut self, expr: Val) -> Iexpr
    {
        let (raw_x, e2) = list::take(expr);
        let (raw_cases, _) = list::take(e2);
vout!("precompile matchx\n\t{:?}\n\t{:?}\n", raw_x, raw_cases);
        let match_func = raw_x == Val::CallParams;

        let x = self.precompile(raw_x);
        let cases = self.precompile_matchcase(raw_cases, match_func, &x.typ);
vout!("ixmatch:\n\t{:?}\n\t{:?}\n", x, cases);
        Iexpr::match_expr(x, cases)
    }

    pub fn precompile_matchcase(&mut self, expr: Val, match_func: bool,
        match_type: &Type
    ) -> Iexpr {
        if expr == Val::Void {
            return Iexpr::noop();
        }
        let (raw_patt, e2) = list::take(expr);
        let (raw_code, e3) = list::take(e2);
        let raw_next = list::head_or(e3, Val::Void);
vout!("precompile matchcase\n\t{:?}\n\t{:?}\n\t{:?}\n", raw_patt, raw_code, raw_next);
        self.scope.push_block_scope();

        let patt_reg = if match_func {
            Reg::Params
        } else {
            Reg::Undecided
        };
        let patt_val = self.scope.assign_pattern(raw_patt, &match_type, &patt_reg);
        let patt = Iexpr::const_val(patt_val);
        let code = self.precompile(raw_code);
        let next = self.precompile_matchcase(raw_next, match_func, match_type);
vout!("ixmatchcase:\n\t{:?}\n\t{:?}\n\t{:?}\n", patt, code, next);
        self.scope.pop_block_scope();
        Iexpr::match_case(patt, code, next)
    }

    pub fn precompile_casex(&mut self, expr: Val) -> Iexpr
    {
        let (raw_test, e2) = list::take(expr);
        let (raw_truth, e3) = list::take(e2);
        let (raw_lies, _) = list::take(e3);
vout!("precompile casex\n\t{:?}\n\t{:?}\n\t{:?}\n", raw_test, raw_truth, raw_lies);

        let test = self.precompile(raw_test);
        let truth = self.precompile(raw_truth);
        let lies = self.precompile(raw_lies);
vout!("ixcase:\n\t{:?}\n\t{:?}\n\t{:?}\n", test, truth, lies);
        Iexpr::case_expr(test, truth, lies)
    }

    pub fn precompile_ifstmt(&mut self, expr: Val) -> Iexpr
    {
        let (raw_test, e2) = list::take(expr);
        let (raw_truth, e3) = list::take(e2);
        let (raw_lies, _) = list::take(e3);
vout!("precompile ifx\n\t{:?}\n\t{:?}\n\t{:?}\n", raw_test, raw_truth, raw_lies);

        let test = self.precompile(raw_test);
        let truth = self.precompile(raw_truth);
        let lies = self.precompile(raw_lies);
vout!("ixif:\n\t{:?}\n\t{:?}\n\t{:?}\n", test, truth, lies);
        Iexpr::ifstmt(test, truth, lies)
    }

    pub fn precompile_fail(&mut self, failstmt: Val) -> Iexpr
    {
        let (raw_tag, s2) = list::take(failstmt);
        let (raw_msg, _) = list::take(s2);

        let mut tag = self.precompile(raw_tag);
        let mut msg = self.precompile(raw_msg);
        if tag.typ != Type::Hashtag {
            panic!("Failure tag is not a hashtag: {:?}", tag.typ);
        }
        if msg.typ != Type::Str {
            panic!("Failure msg is not a string: {:?}", msg.typ);
        }
        Iexpr{
            dst: Reg::Undecided,
            typ: Type::Failure,
            src: Source::Fail(
                Box::new(tag),
                Box::new(msg),
            ),
        }
    }

    pub fn precompile_matchfailed(&mut self, expr: Val) -> Iexpr
    {
        let (raw_x, e2) = list::take(expr);
        let (raw_casex, _) = list::take(e2);
vout!("precompile matchfailed\n\t{:?}\n\t{:?}\n", raw_x, raw_casex);

        let failed_id = match &raw_x {
            &Val::Id(ref failed_id_ref) => {
                self.scope.push_failed_scope(failed_id_ref);
                failed_id_ref.clone()
            }
            _ => {
                panic!("failure match expressions must be an ID");
            }
        };

        let mut x = self.precompile(raw_x);
        match x.src {
            Source::BoundVal(ref mut reg) => {
                *reg = reg.sub(0);
            }
            _ => {
                panic!("Can only fail match bound labels");
            }
        }

        let cases = self.precompile_matchfailedcase(raw_casex);
vout!("ixmatchfailed:\n\t{:?}\n\t{:?}\n", x, cases);
        let mut ix = Iexpr::match_expr(x, cases);
        ix.dst = Reg::Void;
        self.scope.pop_failed_scope();
        ix
    }

    pub fn precompile_matchfailedcase(&mut self, expr: Val) -> Iexpr
    {
        let (raw_patt, e2) = list::take(expr);
        let (raw_code, e3) = list::take(e2);
        let (raw_next, _) = list::take(e3);
vout!("precompile failedmatchcase\n\t{:?}\n\t{:?}\n\t{:?}\n", raw_patt, raw_code, raw_next);

        match &raw_patt {
            &Val::Hashtag(_) => {}
            &Val::Id(_) => {}
            &Val::Wildcard => {}
            _ => {
                panic!("Failed patterns must be hashtags or _");
            }
        }

        let patt_val = self.scope.assign_pattern(raw_patt, &Type::Hashtag, &Reg::Undecided);
        let patt = Iexpr::const_val(patt_val);
        let code = self.precompile(raw_code);
        let next = self.precompile_matchfailedcase(raw_next);
vout!("ixfailedmatchcase:\n\t{:?}\n\t{:?}\n\t{:?}\n", patt, code, next);
        let mut ix = Iexpr::match_case(patt, code, next);
        ix.dst = Reg::Void;
        ix
    }

    pub fn precompile_id(&mut self, name: &String) -> Iexpr
    {
        // look up stuff in static space
        match self.scope.lookup_label(name) {
            Some((reg, typ)) => {
                match reg {
                    &Reg::Reg(_) => {
                        Iexpr::bound_val(reg.clone(), typ.clone())
                    }
                    &Reg::Param(_) => {
                        Iexpr::bound_val(reg.clone(), typ.clone())
                    }
                    &Reg::Lib => {
                        Iexpr::const_val(Val::new_str(name.clone()))
                    }
                    _ => {
                        panic!("unexpected reg: {:?}", reg);
                    }
                }
            }
            None => {
                panic!("undefined variable: {}", name);
            }
        }
    }

    pub fn precompile_type(&mut self, ptype: Type) -> Type
    {
        match ptype {
            Type::AnonVar => {
                self.scope.new_typevar()
            }
            Type::Id(type_id) => {
                let found_type = self.scope.get_type(&*type_id);
                if found_type.is_none() {
                    panic!("Undefined type: {}", type_id);
                }
                found_type.unwrap().clone()
            }
            Type::Var(_) => {
                ptype
            }
            Type::Int => {
                ptype
            }
            Type::Bool => {
                ptype
            }
            Type::Str => {
                ptype
            }
            Type::Void => {
                ptype
            }
            Type::Hashtag => {
                ptype
            }
            Type::Tuple(_) => {
                ptype
            }
            Type::Failure => {
                ptype
            }
            Type::Struct(s, _) => {
                panic!("How'd we get a struct type already? {}", s);
            }
            _ => {
                panic!("What kind of type is that? {:?}", ptype);
            }
        }
    }

    pub fn precompile_call(&mut self, call: Val) -> Iexpr
    {
        let (f, sx) = list::take(call);
        let args = list::head(sx);
vout!("args = {:?}\n", args);
        let fname = match &f {
            &Val::Id(ref name) => name,
            &Val::Type(Type::Id(ref name)) => name,
            _ => {
                panic!("not a valid function name: {:?}", f);
            }
        }.clone();
        let fexpr = self.precompile(f);
        let cargs = self.precompile(args);
vout!("cargs = {:?}\n", cargs);
vout!("cargs.type = {:?}\n", cargs.typ);
        let call_result = self.scope.apply_call_types(&fname, &cargs.typ);
        Iexpr::call(call_result, fexpr, cargs)
    }

    pub fn precompile_str(&mut self, expr: Val) -> Iexpr
    {
        let mut next_s = expr;
        let mut last_s = None;
        let mut new_strs = Vec::new();
        loop {
            match (last_s, next_s) {
                (Some(ls), Val::Nil) => {
                    new_strs.push(self.precompile(ls));
                    break;
                }
                (None, Val::Nil) => {
                    // stick an empty string in there. we really shouldn't
                    // be here. this should be handled in the parser.
                    break;
                }
                (Some(Val::Str(ls)), Val::Cons(next, tail)) => {
                    match *next {
                        Val::Str(ns) => {
                            let next_str = format!("{}{}", ls, ns);
                            last_s = Some(Val::new_str(next_str));
                        }
                        _ => {
                            let next_str = Val::Str(ls.clone());
                            new_strs.push(Iexpr::const_val(next_str));
                            last_s = Some(*next);
                        }
                    }
                    next_s = *tail;
                }
                (Some(innerx), Val::Cons(next, tail)) => {
                    new_strs.push(self.precompile(innerx));
                    last_s = Some(*next);
                    next_s = *tail;
                }
                (None, Val::Cons(ns, tail)) => {
                    last_s = Some(*ns);
                    next_s = *tail;
                }
                a => panic!("How'd we get that? {:?}", a),
            }
        }

        let result: Iexpr = match new_strs.len() {
            0 => Iexpr::const_val(Val::empty_str()),
            1 => new_strs.pop().unwrap(),
            _ => Iexpr::str(new_strs),
        };
        result
    }

    pub fn precompile_fieldaccess(&mut self, expr: Val) -> Iexpr
    {
vout!("typefields at fieldaccess: {:?}\n", self.typefields);
        let (raw_base, x2) = list::take(expr);
        let (raw_field, _) = list::take(x2);

        let base = self.precompile(raw_base);

        let field_name = match raw_field {
            Val::Id(fld_name) => fld_name,
            _ => {
                panic!("Not a field name! {:?}", raw_field);
            }
        };

        if base.typ == Type::Unknown {
            panic!("type of {:?} is unknown. Cannot access {} field",
                base, field_name);
        }
        // lookup fields in 
        let found_field = self.lookup_field_by_name(&base.typ, &*field_name);
        if found_field.is_none() {
            panic!("cannot find field: {:?}.{} in {:?}",
                base.typ, field_name, self.typefields);
        }
        let (fldtyp, fldidx) = found_field.unwrap();
        Iexpr{
            dst: Reg::Undecided,
            typ: fldtyp.clone(),
            src: Source::FieldAccess(Box::new(base), fldidx),
        }
    }

    pub fn precompile_list(&mut self, items: Val) -> Iexpr
    {
        Iexpr::list(self.precompile_list_to_vec(items))
    }

    fn precompile_list_to_vec(&mut self, items: Val) -> Vec<Iexpr>
    {
        let mut it = items;
        let mut result = vec![];
        while it != Val::Nil {
            let (head, tail) = list::take(it);
            result.push(self.precompile(head));
            it = tail;
        }
        result
    }

    pub fn precompile_tuple(&mut self, items: Vec<Val>) -> Iexpr
    {
        let mut results = vec![];
        for i in items {
            let ci = self.precompile(i);
            results.push(ci);
        }
        Iexpr::tuple(results)
    }

    pub fn lookup_field_by_name(&self, stype: &Type, fname: &String) ->
        Option<(&Type, i8)>
    {
        let sfields_opt = self.typefields.get(stype);
        if sfields_opt.is_none() {
            return None;
        }
        let sfields = sfields_opt.unwrap();
        let mut i = 0;
        for sf in sfields {
            let &(ref sfname, ref sftype): &(Arc<String>, Type) = sf;
            if (**sfname) == *fname {
                return Some((sftype, i));
            }
            i += 1;
        }
        return None;
    }

    pub fn assign_registers(&mut self, i: &mut Iexpr)
    {
        i.typ = self.scope.find_inferred_type(&i.typ).clone();
        // now actually assign registers
        if i.dst == Reg::Undecided {
            i.dst = Reg::new_reg(self.scope.nextreg());
        }
        match i.src {
            Source::Block(ref mut items) => {
                self.assign_block_registers(&mut i.dst, items);
            }
            Source::Call(ref mut f, ref mut args) => {
                self.assign_registers(&mut *f);
                self.assign_registers(&mut *args);
            }
            Source::FieldAccess(ref mut base, _) => {
                self.assign_registers(base);
            }
            Source::Fork(ref mut f, ref mut args) => {
                self.assign_registers(&mut *f);
                self.assign_registers(&mut *args);
            }
            Source::Str(ref mut items) => {
                for it in items {
                    self.assign_registers(&mut *it);
                }
            }
            Source::Tuple(ref mut tup) => {
                self.assign_tuple_registers(&i.dst, &mut *tup);
            }
            Source::MatchExpr(ref mut x, ref mut cases) => {
                self.assign_registers(x);
                cases.dst = i.dst.clone();
                self.assign_registers(cases);
            }
            Source::MatchCase(ref mut patt, ref mut code, ref mut next) => {
                self.assign_registers(patt);
                // all of these cases should use the same reg
                code.dst = i.dst.clone();
                next.dst = i.dst.clone();
                self.assign_registers(code);
                self.assign_registers(next);
            }
            Source::CaseExpr(ref mut test, ref mut truth, ref mut lies) => {
                self.assign_registers(test);
                // use the same register for truth and lies
                truth.dst = i.dst.clone();
                lies.dst = i.dst.clone();
                self.assign_registers(truth);
                self.assign_registers(lies);
            }
            Source::IfStmt(ref mut test, ref mut truth, ref mut lies) => {
                self.assign_registers(test);
                self.assign_registers(truth);
                // use the same register for truth and lies
                lies.dst = truth.dst.clone();
                self.assign_registers(lies);
            }
            Source::List(ref mut l) => {
                self.assign_list_registers(&i.dst, &mut *l);
            }
            Source::BooleanAnd(ref mut a, ref mut b) => {
                self.assign_registers(&mut *a);
                self.assign_registers(&mut *b);
            }
            Source::BooleanOr(ref mut a, ref mut b) => {
                self.assign_registers(&mut *a);
                self.assign_registers(&mut *b);
            }
            Source::Fail(ref mut tag, ref mut msg) => {
                self.assign_registers(tag);
                self.assign_registers(msg);
            }
            Source::Return(ref mut res) => {
                if res.dst == Reg::Undecided {
                    res.dst = i.dst.clone();
                } else {
                    i.dst = res.dst.clone();
                }
                self.assign_registers(res);
            }
            // nothing to recurse into for these
            Source::BoundVal(_) => {}
            Source::ConstVal(_) => {}
            Source::Constructor(_) => {}
            Source::PatternVar(_) => {}
            Source::Void => {}
        }
    }

    pub fn assign_block_registers(&mut self, dst: &mut Reg, items: &mut Vec<Iexpr>)
    {
        match items.last_mut() {
            Some(ref mut i) if i.dst == Reg::Undecided => {
                i.dst = dst.clone();
            }
            Some(ref i) => {
                *dst = i.dst.clone();
            }
            _ => {},
        }
        for ref mut i in items.iter_mut() {
            self.assign_registers(i);
        }
    }

    pub fn assign_list_registers(&mut self, dst: &Reg, items: &mut Vec<Iexpr>)
    {
        let head_dst = dst.sub(0);
        for mut i in items {
            i.dst = head_dst.clone();
            self.assign_registers(&mut i);
        }
    }

    pub fn assign_tuple_registers(&mut self, dst: &Reg, tup: &mut Vec<Iexpr>)
    {
        let mut i = 0;
        for mut x in tup {
            if x.dst == Reg::Undecided {
                x.dst = dst.sub(i);
            }
            i += 1;
            self.assign_registers(&mut x);
        }
    }
}

#[cfg(test)]
mod tests {
    use leema::ast;
    use leema::val::{Val, SexprType, Type};
    use leema::sexpr;
    use leema::compile::{Iexpr, Source};
    use leema::reg::{Reg};
    use leema::code::{Code, CodeKey};
    use leema::loader::{Interloader};
    use leema::module::{ModKey, ModuleInterface};
    use leema::list;
    use leema::frame::{self, Frame};
    use leema::lex::{lex};
    use leema::prefab;

    use std::collections::HashMap;
    use std::rc::Rc;
    use std::sync::Arc;
    use std::io::{stderr, Write};


fn call_with_no_params(fs: &mut Frame)
{
    // blah
    let x = 5;
}

#[test]
fn test_compile_call_no_params()
{
    let input = "no_params()\n";
    let root = ast::parse(lex(input));
    let mut inter = Interloader::new("test.lma");
    let mk = ModKey::name_only("tacos");
    let mi = Rc::new(ModuleInterface::new(&mk));
    let mut ss = prefab::new_staticspace(mi, &mut inter);
    ss.define_func(Arc::new("no_params".to_string()),
        Type::f(vec![], Type::Void),
        Code::Rust(call_with_no_params),
    );
    let iprog = ss.compile(root);

    let fname = Iexpr{
        dst: Reg::new_reg(1),
        typ: Type::Str,
        src: Source::ConstVal(Val::new_str("no_params".to_string())),
    };
    let argt = Iexpr{
        dst: Reg::new_reg(2),
        typ: Type::Tuple(vec![]),
        src: Source::Tuple(vec![]),
    };
    let expected_call = Iexpr{
        dst: Reg::new_reg(0),
        typ: Type::Void,
        src: Source::Call(Box::new(fname), Box::new(argt)),
    };
    let expected_block = Iexpr{
        dst: Reg::new_reg(0),
        typ: Type::Void,
        src: Source::Block(vec![expected_call]),
    };
    assert_eq!(expected_block, iprog);
}

/*
#[test]
fn test_compile_macro()
{
    let input = "
    macro mand(a, b) ->
        case
        |a -> b
        |else -> false
        --
    --
    ";
    let root = ast::parse(lex(input));
    let mut inter = Interloader::new("test.lma");
    let mk = ModKey::name_only("tacos");
    let mi = Rc::new(ModuleInterface::new(&mk));
    let mut ss = prefab::new_staticspace(mi, &mut inter);

    let iprog = ss.compile(root);
    let expected = Iexpr{
        dst: Reg::Void,
        typ: Type::Void,
        src: Source::Void,
    };
    assert_eq!(expected, iprog);
    let macro_name = "mand".to_string();
    let found_macro = ss.scope.get_macro(&macro_name);
    assert!(found_macro.is_some());

    let &(ref args, ref body) = found_macro.unwrap();
    assert_eq!(vec![
        Arc::new("a".to_string()),
        Arc::new("b".to_string()),
    ], *args);

    let block_t = sexpr::new_block(list::singleton(Val::id("b".to_string())));
    let block_f = sexpr::new_block(list::singleton(Val::Bool(false)));
    let expected_body = sexpr::new_block(
        list::cons(
            sexpr::new(SexprType::CaseExpr,
                list::cons(Val::id("a".to_string()),
                list::cons(block_t,
                list::cons(block_f,
                Val::Nil,
            )))),
            Val::Nil,
        ),
    );
    assert_eq!(expected_body, *body);
}

#[test]
fn test_use_macro()
{
    let input = "macro mand(a, b) {
        if a {
            b
        } else {
            false
        }
    }
    mand(true, false)
    ".to_string();
    let root = ast::parse(lex(input));
    let mut ss = prefab::new_staticspace();
    let iprog = ss.compile(root);

    let expected = Iexpr{
        dst: Reg::Result,
        typ: Type::Bool,
        src: Source::Block(vec![
            Iexpr{
                dst: Reg::Result,
                typ: Type::Void,
                src: Source::Void,
            },
        ]),
    };
    assert_eq!(expected, iprog);
}
*/

#[test]
fn test_precompile_if_block()
{
    let input = "
    if true ->
        1
    else ->
        2
    --
    ";
    let root = ast::parse(lex(input));
    let mut inter = Interloader::new("test.lma");
    let mk = ModKey::name_only("tacos");
    let mi = Rc::new(ModuleInterface::new(&mk));
    let mut ss = prefab::new_staticspace(mi, &mut inter);
    let ifprog = ss.compile(root);

    let test = Iexpr{
        dst: Reg::new_reg(1),
        typ: Type::Bool,
        src: Source::ConstVal(Val::Bool(true)),
    };
    let block_t = Iexpr{
        dst: Reg::new_reg(2),
        typ: Type::Int,
        src: Source::Block(vec![
            Iexpr{
                dst: Reg::new_reg(2),
                typ: Type::Int,
                src: Source::ConstVal(Val::Int(1)),
            },
        ]),
    };
    let block_f = Iexpr{
        dst: Reg::new_reg(2),
        typ: Type::Int,
        src: Source::Block(vec![
            Iexpr{
                dst: Reg::new_reg(2),
                typ: Type::Int,
                src: Source::ConstVal(Val::Int(2)),
            },
        ]),
    };
    let expected = Iexpr{
        dst: Reg::Void,
        typ: Type::Void,
        src: Source::Block(vec![
            Iexpr{
                dst: Reg::Void,
                typ: Type::Void,
                src: Source::IfStmt(
                    Box::new(test),
                    Box::new(block_t),
                    Box::new(block_f)
                ),
            },
        ]),
    };
    assert_eq!(expected, ifprog);
}

#[test]
fn test_compile_func_oneline_untyped()
{
    let input = "func inc(x) -> x + 1 --";
    let root = ast::parse(lex(input));
    let mut inter = Interloader::new("test.lma");
    let mk = ModKey::name_only("tacos");
    let mi = Rc::new(ModuleInterface::new(&mk));
    let mut ss = prefab::new_staticspace(mi, &mut inter);

    let iprog = ss.compile(root);

    let expected = Iexpr{
        dst: Reg::Void,
        typ: Type::Void,
        src: Source::Void,
    };
    assert_eq!(expected, iprog);
    // but also assert that the function was defined!
    assert!(ss.scope.is_label(&"inc".to_string()));
}

#[test]
fn test_compile_and_call_func()
{
    let input = "
    func make_hash() -> #foo --
    func main() ->
        let h := make_hash()
        cout(\"hello $h\n\")
    --
    ";
    let root = ast::parse(lex(input));
    let mut inter = Interloader::new("test.lma");
    let mk = ModKey::name_only("tacos");
    let mi = Rc::new(ModuleInterface::new(&mk));
    let mut ss = prefab::new_staticspace(mi, &mut inter);

    let iprog = ss.compile(root);
}

/*
#[test]
fn test_compile_strx_field_access()
{
    let input = "
    struct Foo
        .fld: Int
    --
    func foo_fld(s: Foo): Str -> \"hello ${s.fld}\" --
    ";
    let root = ast::parse(lex(input));
    let mut inter = Interloader::new("test.lma");
    let mk = ModKey::name_only("tacos");
    let mi = Rc::new(ModuleInterface::new(&mk));
    let mut ss = prefab::new_staticspace(mi, &mut inter);

    let iroot = ss.compile(root);

    let expected = Iexpr{
        dst: Reg::Void,
        typ: Type::Void,
        src: Source::Void,
    };
    assert_eq!(expected, iroot);
    // but also assert that the function was defined!
    // for now, as long as compile didn't fail, this is enough
    assert!(ss.scope.is_label("foo_fld"));
}
*/

#[test]
fn test_compile_main_func()
{
    let input = "func main() -> 1 --";
    let root = ast::parse(lex(input));
    let mut inter = Interloader::new("test.lma");
    let mk = ModKey::name_only("tacos");
    let mi = Rc::new(ModuleInterface::new(&mk));
    let mut ss = prefab::new_staticspace(mi, &mut inter);

    let iprog = ss.compile(root);

    // make sure that ss thinks it has a main function
    assert!(ss.has_main());
}

}
