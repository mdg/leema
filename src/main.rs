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
use leema::reg::Reg;
use std::io::{stderr, Write};
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

    println!("{:?}", args);
    let loader = ast::new_file_loader();
    let mut ss = prefab::new_staticspace();
    if args.arg_file.is_some() {
        let mut c = Compiler::new(ss, loader);
        c.compile_file(args.arg_file.unwrap());
        ss = c.ss;
        //println!("file inter> {:?}", ss.inter);
        println!("lib code> {:?}", ss.lib);
        println!("\nss> {:?}\n", ss);
    } else if !args.flag_repl {
        panic!("do you want a file or the repl?");
    }

    if args.flag_verbose {
        log::set_verbose();
    }
    verbose_out!("verbose mode\n");

    let mut app = Application::new();
    app.add_app_code(&ss);

    let e = Env::new();
    if ss.has_main() {
        let frm = Frame::new(Parent::Main, e);
verbose_out!("We have main!\n{:?}", frm);
        app.push_new_frame(&CodeKey::Main, frm);
    }

    let rappl = Arc::new(Mutex::new(app));
    let app0 = rappl.clone();

    thread::spawn(move || {
        let mut w0 = frame::Worker::new(app0);
        verbose_out!("w0.gotowork");
        w0.gotowork();
    });

    if args.flag_repl {
        repl::reploop(rappl.clone(), Env::new(), ss);
    } else if ! (ss.has_main() || ss.has_script()) {
        write!(stderr(), "no main function or script code\n").ok();
        return leema::CLI_NOMAIN;
    } else {
        let result = Application::wait_until_done(&rappl);
        println!("result: {:?}", result);
        let rframe = result.unwrap();
        let rframe_res = rframe.e.get_reg(&Reg::Result);
        if let &Val::Int(resulti) = rframe_res {
            return resulti as i32;
        }
    }
    return 0;
}
