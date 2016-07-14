use leema::val::{Env};
use leema::frame::{Frame, Application, Parent};
use leema::lex::{lex};
use leema::ast::{Ast};
use leema::compile::{StaticSpace};
use leema::code::{CodeKey, Code, Op, make_ops};

use std::sync::{Arc, Mutex};
use std::thread;
use std::io::{stdin, stdout, Write};


fn prompt() {
	write!(stdout(), "> ").ok();
	stdout().flush().ok();
}

fn read_cmd() -> String {
	let mut buffer = String::new();
	stdin().read_line(&mut buffer).ok();
	buffer
}

/*
fn apply_macro(mdef: Sexpr, input: List) -> Sexpr {
	match mdef {
		Sexpr::MacroDef(_, ref args, ref code) => {
			if input_items.len() != args.len() {
				panic!("Wrong number of args");
			}
			let mut i = 0;
			let mut argval: HashMap<&String, Rc<Ast>> = HashMap::new();
			for arg in args {
				argval.insert(arg, input_items[i].clone());
				i = i + 1;
			}
			println!("argval: {:?}", argval);
			// do nothing
			// Ast::Bool(true)
			let mut new_code = Vec::with_capacity(code.len());
			for c in code {
				new_code.push(Ast::replace_id(c, &argval));
			}
			Sexpr::Block(List::Nil)
		}
		_ => {
			panic!("That was not a macro");
		}
	}
}
*/


pub fn push_eval(app: Arc<Mutex<Application>>, i: isize, function: Code, e: Env)
{
	println!("exec {:?}", function);

	let frm = Frame::new(Parent::Repl, e);
//println!("repl.push_eval: app.lock().unwrap()");
	let mut _app = app.lock().unwrap();
	let ckey = CodeKey::Repl(i);
	_app.push_new_frame(&ckey, frm);
	_app.add_code(ckey, function);
}

pub fn wait_eval(app: Arc<Mutex<Application>>) -> Frame
{
//println!("repl.wait_eval: app.lock().unwrap()");
	let mut result = None;
	while result.is_none() {
		thread::yield_now();
		let mut _app = app.lock().unwrap();
//println!("repl.wait_eval: app.pop_old_frame().unwrap()");
		result = _app.take_main_frame()
	}
	result.unwrap()
}

pub fn reploop(app: Arc<Mutex<Application>>, mut e: Env, mut ss: StaticSpace)
{
	let mut i = 1;
	loop {
		prompt();
		let input = read_cmd();

		let tokens = lex(input);
		println!("tokens = {:?}\n", tokens);

		let ast_root = Ast::parse(tokens);
		println!("ast = {:?}", ast_root);
		let rootx = Ast::root(ast_root);

		// static compilation
		let inter = ss.compile(rootx);
		println!("inter> {:?}", inter);

		let ops = make_ops(&inter);
		println!("ops>");
		Op::print_list(&ops);
		println!("---\n");

		let code = Code::Leema(Arc::new(ops));
		push_eval(app.clone(), i, code, e);
		//let result = Worker::eval(cmd_code);
		// println!("= {:?}", result);

		let mut result_frame = wait_eval(app.clone());
		println!("= {:?}", result_frame.e.takeResult());
		println!("w/ {:?}", result_frame);
		e = result_frame.take_env();
		i += 1;
	}
}
