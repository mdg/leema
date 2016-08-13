#[macro_use]
mod leema;
mod repl;
mod parse;

use leema::frame::{self, Frame};
use leema::prefab;
use leema::log;
use leema::val::{Env, Val};
use leema::code::{CodeKey};
use leema::compile::{Compiler};
use leema::frame::{Application, Parent};
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
    arg_file: Option<String>,
    flag_export_ops: bool,
    flag_repl: bool,
    flag_verbose: bool,
}

const USAGE: &'static str = "
leema interpreter

Usage:
  leema [options]
  leema [options] <file>
  leema (-v | --verbose)
  leema (-h | --help)

Options:
  --export-ops    Export program ops to a file
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

    let loader = ast::Loader::new();
    let mut ss = prefab::new_staticspace();
    if args.arg_file.is_some() {
        {
            let mut c = Compiler::new(&mut ss, loader);
            c.compile_file(args.arg_file.unwrap());
        }
        vout!("lib code> {:?}\n", ss.lib);
        vout!("\nss> {:?}\n", ss);
    } else if !args.flag_repl {
        panic!("do you want a file or the repl?");
    }


    let mut app = Application::new();
    app.add_app_code(&ss);

    if args.flag_export_ops {
        let mut opsf = fs::File::create("leema-ops").ok().unwrap();
        for keycode in app.code_iter() {
            write!(opsf, "{:?}\n", keycode).ok();
        }
        return 0;
    }

    let e = Env::new();
    if ss.has_main() {
        if ss.has_script() {
            // might just get rid of scripts altogether
            // panic!("Cannot have both script code and a main function");
        }
        let frm = Frame::new_root(e);
verbose_out!("We have main!\n{:?}", frm);
        app.push_new_frame(&CodeKey::Main, frm);
    } else if ss.has_script() {
        let frm = Frame::new_root(e);
verbose_out!("We have a script!\n{:?}", frm);
        app.push_new_frame(&CodeKey::Script, frm);
    }

    let rappl = Arc::new(Mutex::new(app));
    let app0 = rappl.clone();

    thread::spawn(move || {
        let mut w0 = frame::Worker::new(app0);
        verbose_out!("w0.gotowork");
        w0.gotowork();
    });

    if args.flag_repl {
        repl::reploop(rappl.clone(), ss);
    } else if ! (ss.has_main() || ss.has_script()) {
        write!(stderr(), "no main function or script code\n").ok();
        return leema::CLI_NOMAIN;
    } else {
        let result = Application::wait_until_done(&rappl);
        vout!("result: {:?}\n", result);
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
    }
    return 0;
}
