use leema::val::{Val,SexprType,Type};
use leema::list;
use leema::sexpr;
use leema::reg::{Reg};
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
	BoundVal(Reg),
	DefineFunc(String, Box<Iexpr>),
	Fork(Box<Iexpr>, Box<Iexpr>),
	IfExpr(Box<Iexpr>, Box<Iexpr>, Box<Iexpr>),
	List(Vec<Iexpr>),
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
		let block_type = match code.last() {
			None => {
				Type::Unknown
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
			dst: Reg::Undecided,
			typ: t,
			src: Source::BoundVal(src),
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

	fn fork(dst: Reg, t: Type, f: Iexpr, args: Iexpr) -> Iexpr
	{
		Iexpr{
			dst: dst,
			typ: t,
			src: Source::Fork(Box::new(f), Box::new(args)),
		}
	}

	fn if_expr(test: Iexpr, truth: Iexpr, lies: Iexpr) -> Iexpr
	{
        if truth.typ != lies.typ {
            panic!("Mismatched if types: {:?}!={:?}", truth.typ, lies.typ);
        }
		Iexpr{
			dst: Reg::Undecided,
			typ: truth.typ.clone(),
			src: Source::IfExpr(
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
	scope_name: String,
	func_name: String,
	m: HashMap<Arc<String>, (Vec<Arc<String>>, Val)>,
	E: HashMap<Arc<String>, Reg>,
	T: HashMap<Arc<String>, Type>,
	inferred: HashMap<Arc<String>, Type>,
	pub interlib: HashMap<CodeKey, Arc<Iexpr>>,
	pub lib: CodeMap,
	last_reg: Reg,
	_nextreg: i8,
}

impl StaticSpace
{
	pub fn new() -> StaticSpace
	{
		StaticSpace{
			scope_name: "".to_string(),
			func_name: "".to_string(),
			m: HashMap::new(),
			E: HashMap::new(),
			T: HashMap::new(),
			inferred: HashMap::new(),
			interlib: HashMap::new(),
			lib: HashMap::new(),
			last_reg: Reg::Undecided,
			_nextreg: 0,
		}
	}

	pub fn child(&self, name: &String) -> StaticSpace
	{
		StaticSpace{
			scope_name: format!("{}.{}", self.scope_name, name),
			func_name: name.clone(),
			m: self.m.clone(),
			E: self.E.clone(),
			T: self.T.clone(),
			inferred: HashMap::new(),
			interlib: HashMap::new(),
			lib: HashMap::new(),
			last_reg: Reg::Undecided,
			_nextreg: 0,
		}
	}

	pub fn define(&mut self, r: Reg, name: Arc<String>, typ: Type)
	{
		self.E.insert(name.clone(), r);
		self.T.insert(name, typ);
	}

	pub fn defined(&self, name: &String) -> bool
	{
		self.E.get(name).is_some()
	}

	pub fn is_macro(&self, name: &String) -> bool
	{
		self.m.get(name).is_some()
	}

	pub fn define_type(&mut self, name: Arc<String>, typ: Type)
	{
		self.T.insert(name, typ);
	}

	pub fn define_func(&mut self, name: Arc<String>
		, result: Type, args: Vec<Type>, code: Code)
	{
		let typ = Type::Func(args, Box::new(result));
        let key = if *name == "main" {
            CodeKey::Main
        } else {
            self.E.insert(name.clone(), Reg::Lib);
            self.T.insert(name.clone(), typ);
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
				self.precompile_id(id)
			}
			_ => {
				Iexpr::const_val(val)
			}
		}
	}

	pub fn precompile_sexpr(&mut self, st: SexprType, expr: Val) -> Iexpr
	{
		match st {
			SexprType::Bind => {
				//self.precompile_bind(name, *val)
				Iexpr::new(Source::Void)
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
			SexprType::StrExpr => {
				//self.precompile_str(strings)
				Iexpr::new(Source::Void)
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
			SexprType::IfCase(test, ifcode) => {
				let it = self.precompile(*test);
				let icode = self.precompile(*ifcode);
				Iexpr::if_true(it, icode)
			}
			SexprType::ElseCase(code) => {
				Iexpr::else_case(self.precompile(*code))
			}
			*/
			SexprType::IfExpr => {
				self.precompile_ifexpr(expr)
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

	pub fn precompile_bind(&mut self, name: Arc<String>, val: Val) -> Iexpr
	{
		println!("compile let {} := {}", name, val);
		if self.defined(&name) {
			panic!("{} was already defined", name);
		}
		let mut valexpr = self.precompile(val);
		valexpr.dst = Reg::R1(self.nextreg());
		self.last_reg = valexpr.dst;

		self.define(valexpr.dst, name, valexpr.typ.clone());
		valexpr
	}

	pub fn precompile_fork(&mut self, name: Arc<String>, val: Val) -> Iexpr
	{
		println!("compile fork {} := {}", name, val);
		if self.defined(&name) {
			panic!("{} was already defined", name);
		}

		let mut valexpr = self.compile(val);
		let dst = Reg::R1(self.nextreg());
		//valexpr.dst = Reg::R1(self.nextreg());
		self.last_reg = dst;
		self.define(dst, name.clone(), valexpr.typ.clone());

		let fork_name = Arc::new(format!("fork_{}", name));

        let valexpr_type = valexpr.typ.clone();
		self.define_func(
			fork_name.clone(),
			valexpr_type.clone(),
			vec![],
			Code::Inter(Arc::new(valexpr)),
		);
		// shouldn't need to type define a function that will
		// only be called here
		// self.define(fexpr.dst, name, fexpr.typ.clone());

		let fork_name_ix = self.precompile(Val::Id(fork_name));

println!("What's in precompile_fork({:?})", self.E);
		// now make an expression to call the new function
		let cargs = Iexpr::tuple(vec![]);
		let forkx = Iexpr::fork(dst, valexpr_type, fork_name_ix, cargs);
		forkx
	}

	pub fn precompile_macro(&mut self, expr: Val)
	{
		let (nameval, f1) = list::take(expr);
		let (mut params, f2) = list::take(f1);
		let (code, _) = list::take(f2);
print!("precompile_macro({:?},{:?},{:?})\n", nameval, params, code);

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

print!("macro_defined({:?},{:?},{:?})\n", name, args, code);
		self.m.insert(name.clone(), (args, code));
	}

	pub fn precompile_defunc(&mut self, func: Val)
	{
		let (nameval, f1) = list::take(func);
		let (params, f2) = list::take(f1);
		let (result, f3) = list::take(f2);
		let (code, _) = list::take(f3);

		let name = nameval.to_str();

		let mut ss = self.child(&name);
		let mut argtypes = vec![];
		let mut i: i8 = 0;
		let full_types = false;

		let mut next_param = params;
		while next_param != Val::Nil {
			let (head, tail) = list::take(next_param);
			if !head.is_id() {
				panic!("func param not an id {:?}", head);
			}
			let (st, sx) = sexpr::split(head);
			let (pid, ptype_val) = list::take(sx);
			let var_name = pid.to_str();
			let ptype = Type::var(var_name.clone());
			//let ptype = Type(ptype_val)
			argtypes.push(ptype.clone());
			ss.define(
				Reg::P1(i),
				var_name,
				ptype
			);
			/*
			match head {
				Sexpr::IdType(var_name, param_type) => {
					let ptype = if
						param_type == Type::AnonVar
					{
						Type::Var(
							var_name.clone(),
							"TypeVar_{}".to_string()
						)
					} else {
						param_type
					};
					ss.define(
						Reg::P1(i),
						var_name,
						ptype.clone(),
					);
					argtypes.push(ptype);
				}
				_ => {
					panic!("not an id {:?}"
						, head);
				}
			};
			*/
			next_param = tail;
			i += 1;
		}

		/*
		// only necessary for recursion
		// if the type isn't predefined, can't recurse
		if false { // function type declared
			// ss.define(name.clone(), Type::Func);
		} else { // define it as unknown
			// really this should be declaring type variables
			// and setting the result a type variables
			let rt = if result_type == Type::AnonVar {
				Type::Var(
					name.clone(),
					format!("{}ResultType", name),
				)
			} else {
				result_type
			};
			ss.E.insert(name.clone(), Reg::Lib);
			ss.T.insert(name.clone(),
				Type::Func(Box::new(rt),
					argtypes.clone()
				),
			);
		}
		*/
		let fexpr = ss.compile(code);
println!("fexpr> {:?} : {:?}", fexpr, fexpr.typ);
		let inftypes = ss.apply_inferences(argtypes);
println!("inftypes> {:?}", inftypes);
		self.define_func(
			name.clone(),
			fexpr.typ.clone(),
			inftypes,
			Code::Inter(Arc::new(fexpr)),
		);
	}

	pub fn apply_inferences(&self, types: Vec<Type>) -> Vec<Type>
	{
		let mut output = vec![];
		for t in types {
			let newt = match &t {
				&Type::Var(_, ref tv) => {
					match self.inferred.get(tv) {
						None => None,
						Some(newtype) => {
							Some(newtype.clone())
						}
					}
				}
				_ => None,
			};
			if newt.is_some() {
println!("replace {:?} with {:?}", t, newt);
				output.push(newt.unwrap());
			} else {
println!("don't replace {:?} with {:?}", t, self.inferred);
				output.push(t);
			}
		}
		output
	}

	pub fn precompile_block(&mut self, items: Val) -> Iexpr
	{
		Iexpr::new_block(self.precompile_list_to_vec(items))
	}

	pub fn precompile_ifexpr(&mut self, expr: Val) -> Iexpr
	{
        let (raw_test, e2) = list::take(expr);
        let (raw_truth, e3) = list::take(e2);
        let (raw_lies, _) = list::take(e3);
        let test = self.precompile(raw_test);
        let truth = self.precompile(raw_truth);
        let lies = self.precompile(raw_lies);
        Iexpr::if_expr(test, truth, lies)
	}

	pub fn precompile_id(&mut self, name: Arc<String>) -> Iexpr
	{
		// look up stuff in static space
		let reg = self.E.get(&*name);
		let typ = self.T.get(&*name);
		match reg {
			None => {
				println!("what's in E? {:?}", self.E);
				panic!("undefined variable: {}", name);
			}
			Some(&Reg::R1(reg)) => {
				println!("precompile bound var {} = {:?}", name, reg);
				Iexpr::bound_val(Reg::R1(reg), typ.unwrap().clone())
			}
			Some(&Reg::P1(p)) => {
				println!("precompile param {} = {:?}", name, reg);
				Iexpr::bound_val(Reg::P1(p), typ.unwrap().clone())
			}
			Some(&Reg::Undecided) => {
				panic!("precompile bound undecided {:?} for {}", reg, name);
			}
			Some(&Reg::Lib) => {
				println!("found function {}", name);
				Iexpr::const_val(Val::Str(name))
			}
			Some(_) => {
				panic!("unexpected reg: {:?}", reg);
			}
		}
	}

	pub fn precompile_call(&mut self, call: Val) -> Iexpr
	{
		let (f, sx) = list::take(call);
		let args = list::take_head(sx);
println!("Compile {}({:?})", f, args);
		if !f.is_id() {
			panic!("not an identifier: {:?}", f);
		}
		let fname = f.to_str();
		if self.is_macro(&fname) {
			return self.precompile_macro_call(&fname, args);
		}
		let fexpr = self.precompile(f);
println!("precompile_call {}({:?})", fname, args);
		let cargs = self.precompile(args);
		let deftype = self.function_type(&fname);
println!("call {:?}({:?}) -> {:?}", fexpr, cargs, deftype);
		let call_argtypes = cargs.typ.clone();
		/*
		let call_argtypes = cargs.iter().map(|a| {
			a.typ.clone()
		}).collect();
		*/
		let (def_argtypes, def_result) = Type::func_types(deftype);
		if def_result == Type::Unknown {
			panic!("function result type is unknown {}", fname);
		}
		self.match_types(&def_argtypes, &call_argtypes);
		Iexpr::call(def_result, fexpr, cargs)
	}

	pub fn match_types(&mut self, def_args: &Vec<Type>
		, call_arg_type: &Type)
	{
println!("match types? {:?} == {:?}", def_args, call_arg_type);
		let call_args = match call_arg_type {
			&Type::Tuple(ref tuple_arg_vec) => tuple_arg_vec,
			_ => {
				panic!("call args not a tuple: {:?}",
					call_arg_type
				);
			}
		};

		let arg_len = def_args.len();
		let call_arg_len = call_args.len();
		if call_arg_len < arg_len {
			panic!("too few args: f{:?} called with {:?}",
				def_args, call_args);
		}
		if call_arg_len > arg_len {
			panic!("too many args: f{:?} called with {:?}",
				def_args, call_args);
		}
		let mut i = 0;
		let mut bad_types = false;
		while i < arg_len {
			let def_arg = def_args.get(i).unwrap();
			let call_arg = call_args.get(i).unwrap();

			if *call_arg == Type::Unknown {
				panic!("call arg is unknown!");
			} else if def_arg.is_var() && call_arg.is_var() {
				panic!("can't infer types");
			} else if call_arg.is_var() {
				let name = call_arg.var_name();
println!("found arg var named {}", name);
				self.infer_type(
					call_arg.var_name(),
					call_arg.tmp_type(),
					def_arg
				);
			} else if def_arg.is_var() {
				println!("the function arg is a type var?");
				//bad_types = true;
			} else if def_arg != call_arg {
println!("wrong arg {:?} != {:?}", def_arg, call_arg);
				bad_types = true;
				break;
			} else {
				// arg types match, we're good
			}
			i += 1;
		}
		if bad_types {
			panic!("wrong types: f{:?} called with {:?}",
				def_args, call_args);
		}
	}

	pub fn infer_type(&mut self, varname: Arc<String>
		, tmptype: Arc<String>
		, newtype: &Type
	) {
println!("infer_type({}, {}, {:?}) for {}", varname, tmptype, newtype, self.func_name);
		self.T.remove(&varname);
		self.T.insert(
			varname,
			newtype.clone(),
			);
		self.inferred.insert(tmptype, newtype.clone());

		//let f = self.T.get(&self.func_name).unwrap();
//println!("ftype to fix {:?} in {:?}", f, self.scope_name);
	}

	pub fn precompile_macro_call(&mut self, name: &String, mut argx: Val) -> Iexpr
	{
write!(stderr(), "precompile_macro_call({}, {:?})\n", name, argx);
		let mappl = {
			let &(ref ids, ref body) = self.m.get(name).unwrap();
write!(stderr(), "ids = {:?}\n", ids);
write!(stderr(), "body = {:?}\n", body);
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

			Val::replace_ids(body.clone(), &margs)
		};
write!(stderr(), "result = {:?}\n", mappl);
		self.precompile(mappl)
	}

	/*
	pub fn apply_macro(&mut self, name: &String, args: Sexpr) -> Sexpr
	{
	}
	*/

	pub fn precompile_str(&mut self, strings: Val) -> Iexpr
	{
		Iexpr::str(self.precompile_list_to_vec(strings))
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

	pub fn function_type(&mut self, fname: &String) -> Type
	{
		let fbind = self.T.get(fname);
		match fbind {
			Some(&ref typ) => typ.clone(),
			None => panic!("what is this? {:?}", fbind),
		}
	}

	pub fn assign_registers(&mut self, i: &mut Iexpr)
	{
		if i.dst == Reg::Undecided {
			i.dst = Reg::R1(self.nextreg());
		}
		match i.src {
			Source::Block(ref mut items) => {
				self.assign_block_registers(i.dst, items);
			}
			Source::Call(ref mut f, ref mut args) => {
				self.assign_registers(&mut *f);
				self.assign_registers(&mut *args);
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
				self.assign_tuple_registers(i.dst, &mut *tup);
			}
			Source::IfExpr(ref mut test, ref mut truth, ref mut lies) => {
				self.assign_registers(&mut *test);
                truth.dst = i.dst;
                lies.dst = i.dst;
                self.assign_registers(&mut *truth);
                self.assign_registers(&mut *lies);
			}
			Source::List(ref mut l) => {
				self.assign_list_registers(i.dst, &mut *l);
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
			Source::DefineFunc(_, _) => {}
			Source::Void => {}
		}
	}

	pub fn assign_block_registers(&mut self, dst: Reg, items: &mut Vec<Iexpr>)
	{
		let mut first = true;
		for ref mut i in items.iter_mut().rev() {
			if first {
				i.dst = dst;
				first = false;
			}
			self.assign_registers(i);
		}
	}

	pub fn assign_list_registers(&mut self, dst: Reg, items: &mut Vec<Iexpr>)
	{
		if dst.is_primary() {
			let dst2 = dst.to_secondary(0);
			for mut i in items {
				i.dst = dst2;
				self.assign_registers(&mut i);
			}
		} else {
			panic!("can't put list in secondary {:?}", dst);
		}
	}

	pub fn assign_tuple_registers(&mut self, dst: Reg, tup: &mut Vec<Iexpr>)
	{
		match dst {
			Reg::R1(r) => {
				let mut i = 0;
				for mut x in tup {
					if x.dst == Reg::Undecided {
						x.dst = Reg::R2(r,i);
					}
					i += 1;
					self.assign_registers(&mut x);
				}
			}
			_ => {
				for mut i in tup {
					self.assign_registers(&mut i);
				}
			}
		}
	}

	pub fn set_registers(&mut self, i: &mut Iexpr)
	{
		if i.dst == Reg::Undecided {
			i.dst = Reg::Result;
		}
		self.assign_registers(i);

	}

	pub fn nextreg(&mut self) -> i8
	{
		let r = self._nextreg;
		self._nextreg += 1;
		r
	}
}

pub struct Compiler
{
	loader: Box<ast::Loader>,
	pub ss: StaticSpace,
}

impl Compiler
{
	pub fn new(ss: StaticSpace, l: Box<ast::Loader>) -> Compiler
	{
		Compiler {
			loader: l,
			ss: ss,
		}
	}

	pub fn compile_file(&mut self, filename: String)
	{
		let ast = self.loader.parse(filename);
		self.ss.compile(ast.root());
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
	ss.define_func(Arc::new("no_params".to_string()), Type::Void,
		vec![], Code::Rust(call_with_no_params),
	);
	let iprog = ss.compile(root.root());

	let fname = Iexpr{
		dst: Reg::R1(0),
		typ: Type::Str,
		src: Source::ConstVal(Val::new_str("no_params".to_string())),
	};
	let argt = Iexpr{
		dst: Reg::R1(1),
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
	let input = "macro mand(a, b) {
        if a {
            b
        } else {
            false
        }
    }
    ".to_string();
    let root = Ast::parse(lex(input));
    let mut ss = prefab::new_staticspace();

    let iprog = ss.compile(root.root());
    let expected = Iexpr{
        dst: Reg::Result,
        typ: Type::Void,
        src: Source::Block(vec![
            Iexpr{
                dst: Reg::Result,
                typ: Type::Void,
                src: Source::Void,
            },
        ]),
    };
    assert_eq!(expected, iprog);
    let macro_name = "mand".to_string();
    assert!(ss.m.contains_key(&macro_name));

    let &(ref names, ref body) = ss.m.get(&macro_name).unwrap();
    assert_eq!(vec![
        Arc::new("a".to_string()),
        Arc::new("b".to_string()),
    ], *names);

    let block_t = sexpr::new_block(list::singleton(Val::id("b".to_string())));
    let block_f = sexpr::new_block(list::singleton(Val::Bool(false)));
    let expected_body = sexpr::new_block(
        list::cons(
            sexpr::new(SexprType::IfExpr,
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
	let input = "if true {
        1
    } else {
        2
    }
    ".to_string();
	let root = Ast::parse(lex(input));
	let mut ss = prefab::new_staticspace();
	let ifprog = ss.compile(root.root());

    let test = Iexpr{
        dst: Reg::R1(0),
        typ: Type::Bool,
        src: Source::ConstVal(Val::Bool(true)),
    };
    let block_t = Iexpr{
        dst: Reg::Result,
        typ: Type::Int,
        src: Source::Block(vec![
            Iexpr{
                dst: Reg::Result,
                typ: Type::Int,
                src: Source::ConstVal(Val::Int(1)),
            },
        ]),
    };
    let block_f = Iexpr{
        dst: Reg::Result,
        typ: Type::Int,
        src: Source::Block(vec![
            Iexpr{
                dst: Reg::Result,
                typ: Type::Int,
                src: Source::ConstVal(Val::Int(2)),
            },
        ]),
    };
	let expected = Iexpr{
        dst: Reg::Result,
        typ: Type::Int,
        src: Source::Block(vec![
            Iexpr{
                dst: Reg::Result,
                typ: Type::Int,
                src: Source::IfExpr(
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
	let input = "func inc(x) => x + 1\n".to_string();
	let root = Ast::parse(lex(input));
	let mut ss = prefab::new_staticspace();

	let iprog = ss.compile(root.root());

	let expected = Iexpr{
		dst: Reg::Result,
		typ: Type::Void,
		src: Source::Block(vec![
			Iexpr{
				dst: Reg::Result,
				typ: Type::Void,
				src: Source::Void,
			},
		]),
	};
	assert_eq!(expected, iprog);
	// but also assert that the function was defined!
	assert!(ss.defined(&"inc".to_string()));
}

#[test]
fn test_compile_main_func()
{
	let input = "func main() => 1\n".to_string();
	let root = Ast::parse(lex(input));
	let mut ss = prefab::new_staticspace();

	let iprog = ss.compile(root.root());

    // make sure that ss thinks it has a main function
	assert!(ss.has_main());
}

}
