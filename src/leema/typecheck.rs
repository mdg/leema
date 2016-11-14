
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

    let mut m = if !prog.has_mod(modname) {
        let mkey = inter.mod_name_to_key(&modname);
        prog.add_mod(inter.init_module(mkey))
    } else {
        prog.get_mod_mut(modname)
    };
    m.load();
    println!("loaded module:\n{:?}\n", m);
    //if prog.module
}
