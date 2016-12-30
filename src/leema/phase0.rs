use leema::program::{Lib};
use leema::val::{SexprType, Val};
use leema::module::{ModulePreface};


pub fn preproc(prog: &mut Lib, mp: &ModulePreface, ast: &Val)
{
    let whatever = match ast {
        &Val::Sexpr(SexprType::BlockExpr, ref exprs) => {
            println!("preproc(block, {:?})", exprs);
        }
        _ => {
            println!("preproc(something_else, {:?})", ast);
        }
    };
}
