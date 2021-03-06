#![deny(warnings)]
#![allow(dead_code)]

#[macro_use]
mod leema;

#[macro_use]
extern crate pest_derive;

use crate::leema::application::Application;
use crate::leema::canonical::Canonical;
use crate::leema::code;
use crate::leema::failure::Lresult;
use crate::leema::list;
use crate::leema::loader::Interloader;
use crate::leema::lstr::Lstr;
use crate::leema::parser;
use crate::leema::program;
use crate::leema::val::{Fref, Type, Val};

use docopt::Docopt;
use std::env;

#[derive(Debug)]
#[derive(RustcDecodable)]
struct Args
{
    arg_script: String,
    arg_args: Vec<String>,
    flag_verbose: bool,
    flag_func: Option<String>,
    flag_mod: Option<String>,
    flag_tokens: bool,
    flag_ast: bool,
    flag_preface: bool,
    flag_proto: bool,
    flag_semantics: bool,
    flag_registers: bool,
    flag_typecheck: bool,
    flag_code: bool,
    flag_repl: bool,
}

static USAGE: &'static str = "
leema interpreter

Usage:
  leema [options] <script> [<args>...]
  leema (-v | --verbose)
  leema (-h | --help)

Options:
     --typecheck   Typecheck the script
     --tokens      Show the tokens in this module for debugging
     --ast         Show the ast for the module
     --preface     Show the preface for the module
     --proto       Show the proto mod for the module
     --semantics   Semantically analyze the module
     --registers   Assign registers to the AST
     --code        Show code for the function
     --repl        Launch the REPL
     --func=<func>
     --mod=<module>
  -v --verbose     Output debug messages
  -h --help        Show this message
";

const ENV_VERBOSE: &str = "LEEMA_VERBOSE";

fn main()
{
    // got this pattern from stack overflow. kind of in disbelief that
    // there isn't a better way to do it.
    let exit_code = match real_main() {
        Ok(()) => 0,
        Err(e) => {
            eprintln!("{:#?}", e);
            match (e.code, e.status.cli_code()) {
                (0, cli_code) => cli_code,
                (cli_code, _) => cli_code.into(),
            }
        }
    };
    std::process::exit(exit_code);
}

fn real_main() -> Lresult<()>
{
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.decode())
        .unwrap_or_else(|e| e.exit());

    let verbosenv = env::var_os(ENV_VERBOSE);
    if args.flag_verbose || verbosenv.is_some() && "1" == &verbosenv.unwrap() {
        crate::leema::log::set_verbose();
        vout!("verbose mode\n");
    }
    vout!("cmd: {:?}\n", env::current_exe().unwrap());
    vout!("args:{:?}\n", args);

    let leema_path = match env::var("LEEMA_PATH") {
        Ok(path_val) => Interloader::split_paths(&path_val),
        Err(env::VarError::NotPresent) => Interloader::default_path(),
        Err(env::VarError::NotUnicode(os_path)) => {
            eprintln!("LEEMA_PATH is not unicode: {:?}", os_path);
            Interloader::default_path()
        }
    };

    let main_file = Interloader::static_str(args.arg_script);
    let mut inter = Interloader::new(main_file, leema_path);
    let main_key = inter.new_key(inter.main_mod.as_path())?;
    let main_mod = inter.main_mod.clone();
    let fref = match args.flag_func {
        Some(func) => {
            let sfunc = Interloader::static_str(func);
            let fref_node = parser::parse_fref(sfunc)?;
            fref_node.node.to_fref(main_key.clone())?
        }
        None => {
            let main_type = Type::f(Type::VOID, vec![]);
            Fref::new(main_key.clone(), "main", main_type)
        }
    };
    vout!("run {}\n", main_mod);

    let main_result = if args.flag_tokens {
        let modtxt = inter.read_mod(&main_key)?;
        let tokens = parser::parse_tokens(parser::Rule::file, modtxt)?;
        println!("tokens:");
        println!("{:#?}", tokens);
        None
    } else if args.flag_ast {
        let modtxt = inter.read_mod(&main_key)?;
        let ast = parser::parse(parser::Rule::file, modtxt)?;
        println!("{:#?}", ast);
        None
    } else if args.flag_proto {
        let mut prog = program::Lib::new(inter);
        let module = match args.flag_mod {
            Some(user_mod) => Canonical::new(Lstr::from(user_mod)),
            None => main_mod.clone(),
        };
        prog.load_proto_and_imports(&module)?;
        let proto = prog.find_proto(&module)?;
        println!("\n{:#?}\n", proto);
        None
    } else if args.flag_semantics {
        let mut prog = program::Lib::new(inter);
        let semantics = prog.read_semantics(&fref)?;
        println!("\n{:#?}\n", semantics);
        None
    } else if args.flag_registers {
        let mut prog = program::Lib::new(inter);
        let start = start_timer!();
        let semantics = prog.read_semantics(&fref)?;
        let mut semantics_ast = semantics.src;
        code::assign_registers(&mut semantics_ast)?;
        println!("\n{:#?}\n", semantics_ast);
        println!("registers {:?}", start.elapsed());
        None
    } else if args.flag_code {
        let mut prog = program::Lib::new(inter);
        let code = prog.load_code(&fref);
        println!("code: {:?}", code);
        None
    } else if args.flag_repl {
        return Err(rustfail!(
            "invalid_options",
            "wouldn't it be cool if there were a repl?",
        ));
    } else {
        let lma_args_rev: Val =
            args.arg_args.iter().fold(Val::Nil, |aacc, a| {
                list::cons(Val::Str(Lstr::from(a.to_string())), aacc)
            });
        let leema_args = list::reverse(&lma_args_rev);

        let (mut app, result_recv) =
            Application::run_main(inter, fref, leema_args);
        app.wait_for_result(result_recv)
    };

    match main_result {
        Some(Val::Token(t)) if t == Type::VOID => Ok(()),
        Some(Val::Int(_)) => Ok(()),
        Some(Val::Failure2(failure)) => Err(*failure),
        Some(mainr) => {
            println!("{}", mainr);
            Ok(())
        }
        None => Ok(()),
    }
}
