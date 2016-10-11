#[macro_use]
use leema::log;

#[macro_use]
mod leema;

use leema::frame::{self, Frame};
use leema::prefab;
use leema::val::{Env, Val};
use leema::code::{CodeKey};
use leema::interloader::{Interloader};
use leema::compile::{self, StaticSpace};
use leema::application::{Application};
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

    /*
    let interspace = {
        let interloader = InterLoader::new();
        interloader.load_module("prefab");
        interloader.load_root_file(args.arg_file);
        interloader.take_space()
    };
    */

    let modname = Interloader::module_name(&args.arg_file);
    let inter = Interloader::new();
    inter.load_module("prefab");
    inter.load_module(&modname);

    let app = Application::new(inter);
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
    return 0;
}
