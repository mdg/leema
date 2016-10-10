#[macro_use]
pub mod log;
pub mod ast;
pub mod code;
pub mod compile;
pub mod frame;
pub mod lex;
pub mod val;
pub mod scope;
pub mod list;
pub mod prefab;
pub mod reg;
pub mod sexpr;
pub mod parse;

pub const CLI_SUCCESS: i32 =  0;
pub const CLI_NOMAIN : i32 = -1;
pub const CLI_UNCAUGHT_FAILURE : i32 = -2;

