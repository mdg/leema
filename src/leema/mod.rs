#[macro_use]
pub mod log;
pub mod ast;
pub mod code;
pub mod compile;
pub mod frame;
pub mod lex;
pub mod val;
pub mod list;
pub mod prefab;
pub mod reg;
pub mod sexpr;

pub const CLI_SUCCESS: i32 =  0;
pub const CLI_NOMAIN : i32 = -1;

