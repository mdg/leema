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
use std::sync::{Arc, Mutex};
use std::thread;
use docopt::{Docopt};

extern crate libc;
extern crate docopt;
extern crate rustc_serialize;

#[derive(Debug)]
#[derive(RustcDecodable)]
struct Args {
    arg_file: String,
    flag_verbose: bool,
}

const USAGE: &'static str = "
leema interpreter

Usage:
  leema [options] <file>
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

    let interload = Interloader::new();
    let mut prog = program::Lib::new(Version::Sin);
    typecheck::module(&mut prog, &args.arg_file);

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
