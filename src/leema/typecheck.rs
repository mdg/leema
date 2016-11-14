
use leema::loader::{Interloader};
use leema::program;
use leema::log;

use std::path::Path;
use std::io::{stderr, Write};


pub fn program(prog: &mut program::Lib, inter: &Interloader)
{
    module(prog, inter, &inter.main_mod);
}

pub fn module(prog: &mut program::Lib, inter: &Interloader, modname: &str)
{
    vout!("typecheck.module({})\n", modname);
    // let modname = module::name(module_or_file);

    let module = inter.load_module(modname);
    println!("loaded module:\n{:?}\n", module);
    //if prog.module
}
