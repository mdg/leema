use leema::val::{Val,SexprType,Type};
use leema::list;
use leema::log;
use leema::sexpr;
use leema::scope::Scope;
use leema::reg::{Reg, Ireg};
use leema::ast;
use leema::code::{self, CodeKey, CodeMap, Code, OpVec};
use std::collections::{HashMap};
use std::sync::Arc;
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
    fn new(src: Source) -> Iexpr
    {
        Iexpr{
            dst: Reg::Undecided,
            typ: Source::type_of(&src),
            src: src,
        }
    }

    fn new_block(code: Vec<Iexpr>) -> Iexpr
    {
verbose_out!("new_block> {:?}\n", code);
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
            dst: Reg::Result,
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
        Iexpr{
            dst: Reg::Undecided, 
            typ: Type::RelaxedList,
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
    _anon_type_idx: i16,
}

impl StaticSpace
{
    pub fn new() -> StaticSpace
    {
        StaticSpace{
            scope: Scope::new("__script".to_string()),
            typefields: HashMap::new(),
            interlib: HashMap::new(),
            lib: HashMap::new(),
            last_reg: Reg::Undecided,
            _anon_type_idx: 0,
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
        self.set_registers(&mut i);
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
                    typ: Type::Unknown,
                    src: Source::ConstVal(Val::CallParams)
                }
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
            SexprType::DefMacro => {
                self.precompile_macro(expr);
                Iexpr::new(Source::Void)
            }
            SexprType::DefStruct => {
                self.precompile_defstruct(expr);
                Iexpr::new(Source::Void)
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
            SexprType::MatchCase => {
                self.precompile_matchcase(expr)
            }
            SexprType::CaseExpr => {
                self.precompile_casex(expr)
            }
            SexprType::IfStmt => {
                self.precompile_ifstmt(expr)
            }
            /*
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
            _ => {
                panic!("Can't compile {:?}/{:?}", st, expr);
            }
        }
    }

    pub fn precompile_let(&mut self, expr: Val) -> Iexpr
    {
        let (lhs, e2) = list::take(expr);
        let (rhs, _ ) = list::take(e2);
verbose_out!("compile let {} := {}\n", lhs, rhs);
        let name = lhs.to_str();
        if self.scope.is_label(&name) {
            panic!("{} was already defined", name);
        }
        let mut irhs = self.precompile(rhs);
        irhs.dst = Reg::new_reg(self.scope.nextreg());
        self.last_reg = irhs.dst.clone();

        self.scope.assign_label(irhs.dst.clone(), &*name, irhs.typ.clone());
        irhs
    }

    pub fn precompile_fork(&mut self, name: Arc<String>, val: Val) -> Iexpr
    {
verbose_out!("compile fork {} := {}\n", name, val);
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

    pub fn precompile_defstruct(&mut self, expr: Val)
    {
        let (nameval, mut fields) = list::take(expr);
verbose_out!("precompile_defstruct({:?},{:?})\n", nameval, fields);

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
            let (fldname, fldtype) = sexpr::split_id_with_type(*head);
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

    pub fn precompile_macro(&mut self, expr: Val)
    {
        let (nameval, f1) = list::take(expr);
        let (mut params, f2) = list::take(f1);
        let (code, _) = list::take(f2);
verbose_out!("precompile_macro({:?},{:?},{:?})\n", nameval, params, code);

        let name = nameval.to_str();
        let mut args = vec![];
        while params != Val::Nil {
            let (head, tail) = list::take(params);
            if !head.is_id() {
                panic!("macro param not an id {:?}", head);
            }
            let var_name = Val::id_name(&head);
            args.push(var_name);
            params = tail;
        }

verbose_out!("macro_defined({:?},{:?},{:?})\n", name, args, code);
        self.scope.define_macro(&*name, args, code);
    }

    pub fn precompile_defunc(&mut self, func: Val)
    {
        let (nameval, f1) = list::take(func);
        let (params, f2) = list::take(f1);
        let (result, f3) = list::take(f2);
        let (code, _) = list::take(f3);

        let name = nameval.to_str();
        Scope::push_scope(&mut self.scope, (*name).clone());

        let (inf_arg_types, funcx) = {
            let mut argtypes = vec![];
            let mut i: i8 = 0;
            let full_types = false;

            let mut next_param = params;
            while next_param != Val::Nil {
                let (head, tail) = list::take(next_param);
                if !sexpr::is_type(&head, SexprType::IdWithType) {
                    panic!("func param not an id with type {:?}", head);
                }
                let (pid, raw_ptype) = sexpr::split_id_with_type(head);
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
            let unwrapped_type = result.to_type();
            let rt = match unwrapped_type {
                Type::AnonVar => {
                    Type::Var(Arc::new(format!("{}_ResultType", name)))
                }
                _ => {
                    unwrapped_type
                }
            };
vout!("{} type: {:?} -> {:?}\n", name, argtypes, rt);

            // necessary for recursion
            // if the type isn't predefined, can't recurse
            let ftype = Type::Func(argtypes.clone(), Box::new(rt));
            self.scope.assign_label(Reg::Lib, &*name, ftype);

            let mut fexpr = self.compile(code);
vout!("fexpr> {:?} : {:?}\n", fexpr, fexpr.typ);
            let final_arg_tuple =
                self.scope.inferred_type(Type::Tuple(argtypes));
            let final_arg_types = Type::tuple_items(final_arg_tuple);
            Scope::pop_scope(&mut self.scope);
            (final_arg_types, fexpr)
        };

        let final_ftype = Type::f(inf_arg_types, funcx.typ.clone());
        self.define_func(
            name.clone(),
            final_ftype,
            Code::Inter(Arc::new(funcx)),
        );
    }

    pub fn precompile_block(&mut self, items: Val) -> Iexpr
    {
verbose_out!("pc block> {:?}\n", items);
        let mut bvec = self.precompile_list_to_vec(items);
        bvec.retain(|i| {
            i.src != Source::Void
        });
        Iexpr::new_block(bvec)
    }

    pub fn precompile_matchx(&mut self, expr: Val) -> Iexpr
    {
        let (raw_x, e2) = list::take(expr);
        let (raw_cases, _) = list::take(e2);
verbose_out!("precompile matchx\n\t{:?}\n\t{:?}\n", raw_x, raw_cases);

        let x = self.precompile(raw_x);
        let cases = self.precompile(raw_cases);
verbose_out!("ixmatch:\n\t{:?}\n\t{:?}\n", x, cases);
        Iexpr::match_expr(x, cases)
    }

    pub fn precompile_matchcase(&mut self, expr: Val) -> Iexpr
    {
        let (raw_patt, e2) = list::take(expr);
        let (raw_code, e3) = list::take(e2);
        let (raw_next, _) = list::take(e3);
verbose_out!("precompile matchcase\n\t{:?}\n\t{:?}\n\t{:?}\n", raw_patt, raw_code, raw_next);

        let patt = self.precompile_pattern(raw_patt);
        let code = self.precompile(raw_code);
        let next = self.precompile(raw_next);
verbose_out!("ixmatchcase:\n\t{:?}\n\t{:?}\n\t{:?}\n", patt, code, next);
        Iexpr::match_case(patt, code, next)
    }

    pub fn precompile_pattern(&mut self, pexpr: Val) -> Iexpr
    {
        match pexpr {
            Val::Id(name) => {
                let dstreg = Reg::new_reg(self.scope.nextreg());
                self.scope.assign_label(dstreg.clone(), &*name, Type::Unknown);
                Iexpr::new(Source::PatternVar(dstreg))
            }
            Val::Int(_) => {
                Iexpr::const_val(pexpr)
            }
            Val::Str(_) => {
                Iexpr::const_val(pexpr)
            }
            Val::Bool(_) => {
                Iexpr::const_val(pexpr)
            }
            Val::Hashtag(_) => {
                Iexpr::const_val(pexpr)
            }
            Val::Wildcard => {
                Iexpr::const_val(pexpr)
            }
            Val::Tuple(items) => {
                let mut ptup = vec![];
                for i in items {
                    ptup.push(self.precompile_pattern(i));
                }
                Iexpr::tuple(ptup)
            }
            _ => {
                panic!("That's not a pattern! {:?}", pexpr);
            }
        }
    }

    pub fn precompile_casex(&mut self, expr: Val) -> Iexpr
    {
        let (raw_test, e2) = list::take(expr);
        let (raw_truth, e3) = list::take(e2);
        let (raw_lies, _) = list::take(e3);
verbose_out!("precompile casex\n\t{:?}\n\t{:?}\n\t{:?}\n", raw_test, raw_truth, raw_lies);

        let test = self.precompile(raw_test);
        let truth = self.precompile(raw_truth);
        let lies = self.precompile(raw_lies);
verbose_out!("ixcase:\n\t{:?}\n\t{:?}\n\t{:?}\n", test, truth, lies);
        Iexpr::case_expr(test, truth, lies)
    }

    pub fn precompile_ifstmt(&mut self, expr: Val) -> Iexpr
    {
        let (raw_test, e2) = list::take(expr);
        let (raw_truth, e3) = list::take(e2);
        let (raw_lies, _) = list::take(e3);
verbose_out!("precompile ifx\n\t{:?}\n\t{:?}\n\t{:?}\n", raw_test, raw_truth, raw_lies);

        let test = self.precompile(raw_test);
        let truth = self.precompile(raw_truth);
        let lies = self.precompile(raw_lies);
verbose_out!("ixif:\n\t{:?}\n\t{:?}\n\t{:?}\n", test, truth, lies);
        Iexpr::ifstmt(test, truth, lies)
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
                let idx = self._anon_type_idx;
                self._anon_type_idx += 1;
                Type::Var(Arc::new(format!("TypeVar_{}", idx)))
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
            _ => {
                panic!("What kind of type is that? {:?}", ptype);
            }
        }
    }

    pub fn precompile_call(&mut self, call: Val) -> Iexpr
    {
        let (f, sx) = list::take(call);
        let args = list::take_head(sx);
verbose_out!("args = {:?}\n", args);
        let fname = match &f {
            &Val::Id(ref name) => {
                if self.scope.is_macro(name) {
                    return self.precompile_macro_call(name, args);
                }
                name
            }
            &Val::Type(Type::Id(ref name)) => name,
            _ => {
                panic!("not a valid function name: {:?}", f);
            }
        }.clone();
        let fexpr = self.precompile(f);
        let cargs = self.precompile(args);
verbose_out!("cargs = {:?}\n", cargs);
verbose_out!("cargs.type = {:?}\n", cargs.typ);
        let call_result = self.scope.apply_call_types(&fname, &cargs.typ);
        Iexpr::call(call_result, fexpr, cargs)
    }

    pub fn precompile_macro_call(&mut self, name: &String, mut argx: Val) -> Iexpr
    {
verbose_out!("precompile_macro_call({}, {:?})\n", name, argx);
        let mappl = {
            let &(ref ids, ref body) = self.scope.get_macro(name).unwrap();
verbose_out!("ids = {:?}\n", ids);
verbose_out!("body = {:?}\n", body);
            let argv = Val::tuple_items(argx);
            let expected_argc = ids.len();
            let passed_argc = argv.len();
            if expected_argc != passed_argc {
                panic!("wrong number of arguments for macro: {} expected {}, got {}",
                    name, expected_argc, passed_argc
                    );
            }

            let mut i = 0;
            let mut margs = HashMap::new();
            while i < passed_argc {
                margs.insert(
                    ids.get(i).unwrap().clone(),
                    argv.get(i).unwrap().clone(),
                );
                i += 1;
            }

verbose_out!("macro replace ids\n\t{:?}\n\t{:?}\n", body, margs);
            Val::replace_ids(body.clone(), &margs)
        };
verbose_out!("result = {:?}\n", mappl);
        self.precompile(mappl)
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

    pub fn replace_inferred_types(&mut self, i: &mut Iexpr)
    {
        // replaces type vars w/ inferred types
        i.typ = self.scope.inferred_type(i.typ.clone());
    }

    pub fn assign_registers(&mut self, i: &mut Iexpr)
    {
        self.replace_inferred_types(i);
        // now actually assign registers
        if i.dst == Reg::Undecided {
            i.dst = Reg::new_reg(self.scope.nextreg());
        }
        match i.src {
            Source::Block(ref mut items) => {
                self.assign_block_registers(&i.dst, items);
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
                self.assign_registers(&mut *x);
                cases.dst = i.dst.clone();
                self.assign_registers(&mut *cases);
            }
            Source::MatchCase(ref mut patt, ref mut code, ref mut next) => {
                self.assign_registers(&mut *patt);
                code.dst = i.dst.clone();
                next.dst = i.dst.clone();
                self.assign_registers(&mut *code);
                self.assign_registers(&mut *next);
            }
            Source::CaseExpr(ref mut test, ref mut truth, ref mut lies) => {
                self.assign_registers(&mut *test);
                truth.dst = i.dst.clone();
                lies.dst = i.dst.clone();
                self.assign_registers(&mut *truth);
                self.assign_registers(&mut *lies);
            }
            Source::IfStmt(ref mut test, ref mut truth, ref mut lies) => {
                self.assign_registers(&mut *test);
                truth.dst = Reg::Void;
                lies.dst = Reg::Void;
                self.assign_registers(&mut *truth);
                self.assign_registers(&mut *lies);
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
            // nothing to recurse into for these
            Source::BoundVal(_) => {}
            Source::ConstVal(_) => {}
            Source::Constructor(_) => {}
            Source::PatternVar(_) => {}
            Source::Void => {}
        }
    }

    pub fn assign_block_registers(&mut self, dst: &Reg, items: &mut Vec<Iexpr>)
    {
        let mut first = true;
        match items.last_mut() {
            Some(ref mut i) => i.dst = dst.clone(),
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

    pub fn set_registers(&mut self, i: &mut Iexpr)
    {
        if i.dst == Reg::Undecided {
            i.dst = Reg::Result;
        }
        self.assign_registers(i);

    }
}

pub struct Compiler<'a>
{
    pub ss: &'a mut StaticSpace,
    loader: ast::Loader,
}

impl<'a> Compiler<'a>
{
    pub fn new(ss: &'a mut StaticSpace, l: ast::Loader) -> Compiler<'a>
    {
        Compiler {
            ss: ss,
            loader: l,
        }
    }

    pub fn compile_file(&mut self, filename: String)
    {
verbose_out!("parse output: {:?}\n", filename);
        let ast = self.loader.parse(filename);
verbose_out!("ast: {:?}\n", ast);
        let script = self.ss.compile(ast.root());
verbose_out!("script output: {:?}\n", script);
        if script.src != Source::Void {
            let stype = script.typ.clone();
            self.ss.define_func(
                Arc::new("*script*".to_string()),
                Type::f(vec![], stype),
                Code::Inter(Arc::new(script)),
            );
        }
    }
}

#[cfg(test)]
mod tests {
    use leema::ast::{Ast};
    use leema::val::{Val, SexprType, Type};
    use leema::sexpr;
    use leema::compile::{Iexpr, Source};
    use leema::reg::{Reg};
    use leema::code::{Code, CodeKey};
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
    let input = "no_params()\n".to_string();
    let root = Ast::parse(lex(input));
    let mut ss = prefab::new_staticspace();
    ss.define_func(Arc::new("no_params".to_string()),
        Type::f(vec![], Type::Void),
        Code::Rust(call_with_no_params),
    );
    let iprog = ss.compile(root.root());

    let fname = Iexpr{
        dst: Reg::new_reg(0),
        typ: Type::Str,
        src: Source::ConstVal(Val::new_str("no_params".to_string())),
    };
    let argt = Iexpr{
        dst: Reg::new_reg(1),
        typ: Type::Tuple(vec![]),
        src: Source::Tuple(vec![]),
    };
    let expected_call = Iexpr{
        dst: Reg::Result,
        typ: Type::Void,
        src: Source::Call(Box::new(fname), Box::new(argt)),
    };
    let expected_block = Iexpr{
        dst: Reg::Result,
        typ: Type::Void,
        src: Source::Block(vec![expected_call]),
    };
    assert_eq!(expected_block, iprog);
}

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
    ".to_string();
    let root = Ast::parse(lex(input));
    let mut ss = prefab::new_staticspace();

    let iprog = ss.compile(root.root());
    let expected = Iexpr{
        dst: Reg::Result,
        typ: Type::Void,
        src: Source::Block(vec![]),
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

/*
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
    let root = Ast::parse(lex(input));
    let mut ss = prefab::new_staticspace();
    let iprog = ss.compile(root.root());

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
    ".to_string();
    let root = Ast::parse(lex(input));
    let mut ss = prefab::new_staticspace();
    let ifprog = ss.compile(root.root());

    let test = Iexpr{
        dst: Reg::new_reg(0),
        typ: Type::Bool,
        src: Source::ConstVal(Val::Bool(true)),
    };
    let block_t = Iexpr{
        dst: Reg::Void,
        typ: Type::Int,
        src: Source::Block(vec![
            Iexpr{
                dst: Reg::Void,
                typ: Type::Int,
                src: Source::ConstVal(Val::Int(1)),
            },
        ]),
    };
    let block_f = Iexpr{
        dst: Reg::Void,
        typ: Type::Int,
        src: Source::Block(vec![
            Iexpr{
                dst: Reg::Void,
                typ: Type::Int,
                src: Source::ConstVal(Val::Int(2)),
            },
        ]),
    };
    let expected = Iexpr{
        dst: Reg::Result,
        typ: Type::Void,
        src: Source::Block(vec![
            Iexpr{
                dst: Reg::Result,
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
    let input = "func inc(x) -> x + 1 --".to_string();
    let root = Ast::parse(lex(input));
    let mut ss = prefab::new_staticspace();

    let iprog = ss.compile(root.root());

    let expected = Iexpr{
        dst: Reg::Result,
        typ: Type::Void,
        src: Source::Block(vec![]),
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
    ".to_string();
    let root = Ast::parse(lex(input));
    let mut ss = prefab::new_staticspace();

    let iprog = ss.compile(root.root());
}

#[test]
fn test_compile_strx_field_access()
{
    let input = "
    struct Foo
        .fld: Int
    --
    func foo_fld(s: Foo): Str -> \"hello ${s.fld}\" --
    ".to_string();
    let root = Ast::parse(lex(input));
    let mut ss = prefab::new_staticspace();

    let root = ss.compile(root.root());

    let expected = Iexpr{
        dst: Reg::Result,
        typ: Type::Void,
        src: Source::Block(vec![]),
    };
    assert_eq!(expected, root);
    // but also assert that the function was defined!
    // for now, as long as compile didn't fail, this is enough
    assert!(ss.scope.is_label(&"foo_fld".to_string()));
}

#[test]
fn test_compile_main_func()
{
    let input = "func main() -> 1 --".to_string();
    let root = Ast::parse(lex(input));
    let mut ss = prefab::new_staticspace();

    let iprog = ss.compile(root.root());

    // make sure that ss thinks it has a main function
    assert!(ss.has_main());
}

}
