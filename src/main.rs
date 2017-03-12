#[macro_use]
use leema::log;

#[macro_use]
mod leema;

use leema::loader::{Interloader};
use leema::module::{ModuleSource};
use leema::program;
use leema::application::{Application};
use std::io::{stderr, Write};
use docopt::{Docopt};

extern crate libc;
extern crate docopt;
extern crate rustc_serialize;

#[derive(Debug)]
#[derive(RustcDecodable)]
struct Args {
    arg_cmd: String,
    arg_file: String,
    flag_verbose: bool,
}

const USAGE: &'static str = "
leema interpreter

Usage:
  leema [options] <cmd> <file>
  leema (-v | --verbose)
  leema (-h | --help)

Options:
  -v --verbose    Output debug messages
  -h --help       Show this message
  --repl          Launch the REPL
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
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.decode())
        .unwrap_or_else(|e| e.exit());

    if args.flag_verbose {
        log::set_verbose();
    }
    vout!("verbose mode\nargs:{:?}\n", args);

    let inter = Interloader::new(&args.arg_file);
    let modkey = inter.mod_name_to_key(&inter.main_mod);
    println!("{} {}", args.arg_cmd, inter.main_mod);

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
        println!("\n{:?}\n", imod);
    } else if args.arg_cmd == "typecheck" {
        let mut prog = program::Lib::new(inter);
        prog.deep_typecheck(&modkey.name, "main");
    } else if args.arg_cmd == "code" {
        let mut prog = program::Lib::new(inter);
        let code = prog.load_code(&modkey.name, "main");
        println!("code: {:?}", code);
    } else if args.arg_cmd == "run" {
        let prog = program::Lib::new(inter);
        let mut app = Application::new(prog);
        app.push_call(&modkey.name, "main");
        app.run();
        let result = app.wait_for_result();
        println!("Result = {:?}", result);
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
