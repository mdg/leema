#[macro_use]
use leema::log;

#[macro_use]
mod leema;

use leema::frame::{self, Frame};
use leema::prefab;
use leema::inter::{Version};
use leema::loader::{Interloader};
use leema::compile::{self, StaticSpace};
use leema::module::{ModuleSource};
use leema::program;
use leema::scope::{Scope};
use leema::application::{Application};
use leema::typecheck;
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

    let mut inter = Interloader::new(&args.arg_file);

    let modkey = inter.mod_name_to_key(&inter.main_mod);

    if args.arg_cmd == "tokens" {
        let mut modtxt = inter.read_module(&modkey);
        let toks = ModuleSource::read_tokens(&modtxt);
        println!("{:?}\n", toks);
    } else if args.arg_cmd == "ast" {
        let mut modtxt = inter.read_module(&modkey);
        let ast = ModuleSource::read_ast(&modtxt);
        println!("{:?}\n", ast);
    } else if args.arg_cmd == "modsrc" {
        let mut modtxt = inter.read_module(&modkey);
        let src = ModuleSource::new(modkey, modtxt);
        println!("{:?}\n", src);
    } else if args.arg_cmd == "typecheck" {
        /*
        println!("typecheck {}", inter.main_mod);
        let main_mod = inter.main_mod.clone();
        let mut prog = program::Lib::new(inter);
        rootmod.load();
        prog.add_mod(rootmod);
        let mut scope = Scope::init();
        typecheck::program(scope, &mut prog, &main_mod, "main");
        */
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
