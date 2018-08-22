#![deny(warnings)]
#![allow(dead_code)]

extern crate docopt;
extern crate libc;
#[macro_use]
extern crate mopa;
extern crate bytes;
extern crate futures;
extern crate rand;
extern crate rustc_serialize;
extern crate tokio_core;
extern crate tokio_io;

#[macro_use]
mod leema;

use leema::log;

use leema::application::Application;
use leema::list;
use leema::loader::Interloader;
use leema::lri::Lri;
use leema::lstr::Lstr;
use leema::module::ModuleSource;
use leema::program;
use leema::typecheck;
use leema::val::Val;

use docopt::Docopt;
use std::env;
use std::io::Write;


#[derive(Debug)]
#[derive(RustcDecodable)]
struct Args
{
    arg_cmd: String,
    arg_script: Vec<String>,
    flag_verbose: bool,
    flag_func: Option<String>,
}

const USAGE: &'static str = "
leema interpreter

Usage:
  leema [options] <cmd> <script>...
  leema (-v | --verbose)
  leema (-h | --help)

Options:
     --func=<func>
  -v --verbose     Output debug messages
  -h --help        Show this message
  --repl           Launch the REPL
";


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

    if args.flag_verbose {
        log::set_verbose();
    }
    vout!("verbose mode\nargs:{:?}\n", args);

    let file = args.arg_script.first().unwrap();
    let leema_args: Val =
        args.arg_script.iter().skip(1).fold(Val::Nil, |acc, a| {
            let strval = Val::Str(Lstr::from(a.to_string()));
            list::cons(strval, acc)
        });
    let inter = Interloader::new(file);
    let modkey = inter.mod_name_to_key(&inter.main_mod);
    vout!("{} {}\n", args.arg_cmd, inter.main_mod);

    if args.arg_cmd == "tokens" {
        let modtxt = inter.read_module(&modkey);
        let toks = ModuleSource::read_tokens(&modtxt);
        println!("{:?}\n", toks);
    } else if args.arg_cmd == "ast" {
        let modtxt = inter.read_module(&modkey);
        let ast = ModuleSource::read_ast(&modtxt);
        println!("{:?}\n", ast);
    } else if args.arg_cmd == "modsrc" {
        let prog = program::Lib::new(inter);
        let src = prog.read_modsrc(&modkey.name);
        println!("{:?}\n", src);
    } else if args.arg_cmd == "preface" {
        let prog = program::Lib::new(inter);
        let (_, pref) = prog.read_preface(&modkey.name);
        println!("{:?}\n", pref);
    } else if args.arg_cmd == "proto" {
        let mut prog = program::Lib::new(inter);
        let proto = prog.read_proto(&modkey.name);
        println!("\n{:?}\n", proto);
    } else if args.arg_cmd == "inter" {
        let mut prog = program::Lib::new(inter);
        let imod = prog.read_inter(&modkey.name);
        let fix = match args.flag_func {
            Some(func) => imod.interfunc.get(&func),
            None => imod.interfunc.get("main"),
        };
        println!("\n{:?}\n", fix);
    } else if args.arg_cmd == "typecheck" {
        let mut prog = program::Lib::new(inter);
        let mod_name = Lstr::Rc(modkey.name.clone());
        let func_name = Lstr::Sref("main");
        let funcri = Lri::with_modules(mod_name, func_name);
        prog.typecheck(&funcri, typecheck::Depth::Full);
    } else if args.arg_cmd == "code" {
        let mut prog = program::Lib::new(inter);
        let code = match args.flag_func {
            Some(func) => prog.load_code(&modkey.name, &func),
            None => prog.load_code(&modkey.name, "main"),
        };
        println!("code: {:?}", code);
    } else if args.arg_cmd == "run" {
        let prog = program::Lib::new(inter);
        let mut app = Application::new(prog);
        app.set_args(leema_args);
        app.push_call(Lstr::Rc(modkey.name.clone()), Lstr::Sref("main"));
        app.run();
        let result = app.wait_for_result();
        return Application::handle_result(result) as i32;
    } else {
        println!("invalid command: {:?}", args.arg_cmd);
        return 1;
    }

    /*
    let app = Application::new(program);
    app.push_call(&modname, "main");
    let result = app.run();
    match result {
        Val::Int(resulti) => {
            return resulti as i32;
        }
        Val::Failure(tag, msg, stack) => {
            println!("Uncaught Failure: {} \"{}\"\n{}", tag, msg, stack);
            return leema::CLI_UNCAUGHT_FAILURE;
        }
        _ => {}
    }
    */
    return 0;
}
