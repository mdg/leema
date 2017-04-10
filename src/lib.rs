#[macro_use]
use leema::log;

#[macro_use]
mod leema;

use leema::loader::{Interloader};
use leema::module::{ModuleSource};
use leema::program;
use leema::application::{Application};
use leema::val::Val;
use std::io::{stderr, Write};
use docopt::{Docopt};

extern crate libc;
extern crate docopt;
extern crate rand;
extern crate rustc_serialize;

fn start_app(module: &str) -> Application
{
    let inter = Interloader::new(module);
    let modkey = inter.mod_name_to_key(&inter.main_mod);
    vout!("start_app({})\n", inter.main_mod);

    let prog = program::Lib::new(inter);
    let mut app = Application::new(prog);
    app.run();
    app
}

fn call_sync(app: &mut Application, module: &str, func: &str) -> Option<Val>
{
    app.push_call(&module, func);
    let result = app.wait_for_result();
    vout!("Result = {:?}\n", result);
    result
}
