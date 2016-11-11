#[macro_use]
use leema::log;

#[macro_use]
mod leema;

use leema::frame::{self, Frame};
use leema::prefab;
use leema::val::{Env, Val};
use leema::code::{CodeKey};
use leema::inter::{Interloader, Version};
use leema::compile::{self, StaticSpace};
use leema::program;
use leema::application::{Application};
use leema::typecheck;
use leema::ast;
use std::io::{stderr, Write};
use std::fs;
use std::path::Path;
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

    let mut inter = Interloader::new(&args.arg_file);

    let modkey = inter.mod_name_to_key(&inter.main_mod);
    let mut rootmod = inter.init_module(modkey);

    if args.arg_cmd == "tokens" {
        let tokens = Interloader::read_tokens(&mut rootmod);
        println!("{:?}\n", tokens);
    } else if args.arg_cmd == "ast" {
        let tokens = Interloader::read_tokens(&mut rootmod);
        let smod = Interloader::read_ast(&mut rootmod);
        println!("{:?}\n", smod);
    } else if args.arg_cmd == "inter" {
        let tokens = Interloader::read_tokens(&mut rootmod);
        let smod = Interloader::read_ast(&mut rootmod);
        let src = Interloader::read_inter(&mut rootmod);
        println!("{:?}\n", src);
    } else if args.arg_cmd == "typecheck" {
        println!("typecheck {}", args.arg_file);
        let initial_version = Version::Sin;
        let mut prog = program::Lib::new(initial_version);
        // typecheck::program(&mut prog, &interload, path);
    } else {
        println!("invalid command");
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
