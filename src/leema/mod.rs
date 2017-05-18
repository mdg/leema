#[macro_use]
pub mod log;
pub mod ast;
pub mod code;
pub mod fiber;
pub mod frame;
pub mod lex;
pub mod val;
pub mod list;
pub mod loader;
pub mod module;
pub mod phase0;
pub mod prefab;
pub mod program;
pub mod reg;
pub mod application;
pub mod ixpr;
pub mod infer;
pub mod inter;
pub mod sxpr;
pub mod parse;
pub mod typecheck;
pub mod udp;
pub mod worker;

pub const CLI_SUCCESS: i32 =  0;
pub const CLI_NOMAIN : i32 = -1;
pub const CLI_UNCAUGHT_FAILURE : i32 = -2;

