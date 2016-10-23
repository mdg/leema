
use leema::inter::{Interloader};
use leema::program;
use leema::log;

use std::path::Path;
use std::io::{stderr, Write};


pub fn program(prog: &mut program::Lib, inter: &Interloader, modfile: &Path)
{
    let modname = modfile.file_stem().unwrap().to_str().unwrap();
    module(prog, inter, modname);
}

pub fn module(prog: &mut program::Lib, inter: &Interloader, modname: &str)
{
    vout!("typecheck.module({})\n", modname);
    // let modname = module::name(module_or_file);

    let module = inter.load_module(modname);
    println!("loaded module: {:?}", module);
    //if prog.module
}
