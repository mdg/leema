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

    let path = Path::new(&args.arg_file);
    if path.parent().is_none() {
        panic!("Cannot execute root directory");
    }
    if !path.exists() {
        panic!("Path does not exist: {}", args.arg_file);
    }
    if !path.is_file() {
        panic!("Path is not a file: {}", args.arg_file);
    }
    let root_path = path.parent().unwrap();

    let initial_version = Version::Sin;
    let mut interload = Interloader::new();
    interload.add_path(root_path);
    let mut prog = program::Lib::new(initial_version);
    typecheck::program(&mut prog, &interload, path);

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
