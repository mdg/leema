
use leema::loader::{Interloader};
use leema::program;
use leema::log;

use std::path::Path;
use std::io::{stderr, Write};


pub fn program(prog: &mut program::Lib, inter: &Interloader)
{
    {
        let mut m = if !prog.has_mod(&inter.main_mod) {
            let mkey = inter.mod_name_to_key(&inter.main_mod);
            prog.add_mod(inter.init_module(mkey))
        } else {
            prog.get_mod_mut(&inter.main_mod)
        };
        m.load();
    }
    module(prog, inter, &inter.main_mod);
}

pub fn module(prog: &mut program::Lib, inter: &Interloader, modname: &str)
{
    vout!("typecheck.module({})\n", modname);
    let imported_mods = {
        let m = prog.get_mod(modname);
        println!("loaded module:\n{:?}\n", m);

        let mut imods = Vec::with_capacity(m.src.imports.len());
        for i in &m.src.imports {
            println!("import({})", i);
            let ikey = inter.mod_name_to_key(&i);
            let mut im = inter.init_module(ikey);
            im.load();
            imods.push(im);
        }
        imods
    };

    for i in imported_mods {
        println!("imported({:?})", i);
        prog.add_mod(i);
    }
}
