#![deny(warnings)]
#![allow(dead_code)]

extern crate docopt;
extern crate libc;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate mopa;
extern crate bytes;
extern crate futures;
extern crate hyper;
extern crate hyper_rustls;
#[cfg(test)]
#[macro_use]
extern crate matches;
extern crate rand;
extern crate rustc_serialize;
extern crate serde;
extern crate serde_json;
extern crate tokio;
extern crate tokio_current_thread;

#[macro_use]
mod leema;

use leema::application::Application;
use leema::list;
use leema::loader::Interloader;
use leema::lri::Lri;
use leema::lstr::Lstr;
use leema::module::ModuleSource;
use leema::program;
use leema::struple::Struple;
use leema::token::{Tokenz, TokenResult};
use leema::typecheck;
use leema::val::Val;

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
    flag_tok: bool,
    flag_tokens: bool,
    flag_ast: bool,
    flag_modsrc: bool,
    flag_preface: bool,
    flag_proto: bool,
    flag_inter: bool,
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
     --tok         Show the tokens in this module
     --tokens      Show the tokens in this module for debugging
     --ast         Show the ast for the module
     --modsrc      Show the modsrc for the module
     --preface     Show the preface for the module
     --proto       Show the proto mod for the module
     --inter       Show the inter mod for the module
     --code        Show code for the function
     --repl        Launch the REPL
     --func=<func>
  -v --verbose     Output debug messages
  -h --help        Show this message
";

const ENV_VERBOSE: &str = "LEEMA_VERBOSE";


fn main()
{
    // got this pattern from stack overflow. kind of in disbelief that
    // there isn't a better way to do it.
    let exit_code = real_main();
    std::process::exit(exit_code);
}

fn real_main() -> i32
{
    vout!("cmd: {:?}", env::current_exe());
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.decode())
        .unwrap_or_else(|e| e.exit());

    let verbosenv = env::var_os(ENV_VERBOSE);
    if args.flag_verbose || verbosenv.is_some() && "1" == &verbosenv.unwrap() {
        ::leema::log::set_verbose();
        vout!("verbose mode\n");
    }
    vout!("args:{:?}\n", args);

    let leema_path = match env::var("LEEMA_PATH") {
        Ok(path_val) => path_val,
        Err(env::VarError::NotPresent) => String::new(),
        Err(env::VarError::NotUnicode(os_path)) => {
            eprintln!("LEEMA_PATH is not unicode: {:?}", os_path);
            String::new()
        }
    };

    let file = Lstr::from(args.arg_script);
    let leema_args_rev: Val = args.arg_args.iter().fold(Val::Nil, |aacc, a| {
        list::cons(Val::Str(Lstr::from(a.to_string())), aacc)
    });
    let leema_args = list::reverse(&leema_args_rev);
    let inter = Interloader::new(file, &leema_path);
    let mod_name = inter.main_mod.clone();
    vout!("run {}\n", inter.main_mod);

    let main_result = if args.flag_tokens {
        let modtxt = inter.read_module(&mod_name).unwrap();
        let toks = ModuleSource::read_tokens(&modtxt);
        println!("tokens:");
        for t in &toks {
            println!("\t{:?}", t);
        }
        Val::Int(0)
    } else if args.flag_tok {
        let modtxt = inter.read_module(&mod_name).unwrap();
        let toks: Vec<TokenResult> = Tokenz::lex(&modtxt).collect();
        println!("tokens:");
        for t in &toks {
            println!("\t{:?}", t);
        }
        Val::Int(0)
    } else if args.flag_ast {
        let modtxt = inter.read_module(&mod_name).unwrap();
        let ast = ModuleSource::read_ast(&modtxt);
        println!("{}\n", ast);
        Val::Int(0)
    } else if args.flag_modsrc {
        let prog = program::Lib::new(inter);
        let src = prog.read_modsrc(&mod_name);
        println!("{:?}\n", src);
        Val::Int(0)
    } else if args.flag_preface {
        let prog = program::Lib::new(inter);
        let (_, pref) = prog.read_preface(&mod_name);
        println!("{:?}\n", pref);
        Val::Int(0)
    } else if args.flag_proto {
        let mut prog = program::Lib::new(inter);
        let proto = prog.read_proto(&mod_name);
        println!("\n{}\n", proto);
        Val::Int(0)
    } else if args.flag_inter {
        let mut prog = program::Lib::new(inter);
        let imod = prog.read_inter(&mod_name);
        let fix = match args.flag_func {
            Some(func) => {
                let lfunc = Lstr::from(func);
                imod.interfunc.get(&lfunc)
            }
            None => imod.interfunc.get("main"),
        };
        println!("\n{:?}\n", fix);
        Val::Int(0)
    } else if args.flag_typecheck {
        let mut prog = program::Lib::new(inter);
        let func_name = Lstr::Sref("main");
        let funcri = Lri::with_modules(mod_name, func_name);
        let ftype = prog.typecheck(&funcri, typecheck::Depth::Full);
        println!("type: {}", ftype);
        Val::Int(0)
    } else if args.flag_code {
        let mut prog = program::Lib::new(inter);
        let code = match args.flag_func {
            Some(func) => {
                let func_name = Lstr::from(func);
                prog.load_code(&mod_name, &func_name)
            }
            None => prog.load_code(&mod_name, &Lstr::Sref("main")),
        };
        println!("code: {:?}", code);
        Val::Int(0)
    } else if args.flag_repl {
        println!("wouldn't it be cool if there was a repl?");
        Val::Int(2)
    } else {
        let prog = program::Lib::new(inter);
        let mut app = Application::new(prog);
        let caller = app.caller();
        let main_lri = Lri::with_modules(mod_name, Lstr::Sref("main"));
        app.run();
        let main_arg = Struple(vec![(None, leema_args)]);
        let result_recv = caller.push_call(main_lri, main_arg);
        app.wait_for_result(result_recv).unwrap()
    };

    i32::from(Application::handle_result(main_result))
}
