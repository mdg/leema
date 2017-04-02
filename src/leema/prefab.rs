use leema::val::{Val, Type};
use leema::code::{RustFunc};
use leema::frame::{Frame};
use leema::list;
use leema::log;

use std::fs::File;
use std::io::{stderr, Read, Write};
use std::any::{Any};
use std::fmt::{self, Display, Debug};
use std::sync::{Mutex};
use std::rc::{Rc};

use rand;


pub fn int_add(fs: &mut Frame)
{
    let ic;
    {
        let a = fs.get_param(0);
        let b = fs.get_param(1);
        match (a,b) {
            (&Val::Int(ia), &Val::Int(ib)) => {
                ic = ia + ib;
            }
            _ => {
                panic!("wtf is all that? {:?}", (a,b));
            }
        }
    }
    fs.parent.set_result(Val::Int(ic));
}

pub fn int_sub(fs: &mut Frame)
{
    let ic;
    {
        let a = fs.get_param(0);
        let b = fs.get_param(1);
        match (a,b) {
            (&Val::Int(ia), &Val::Int(ib)) => {
                ic = ia - ib;
            }
            _ => {
                panic!("wtf is all that? {:?}", (a,b));
            }
        }
    }
    fs.parent.set_result(Val::Int(ic));
}

pub fn int_mult(fs: &mut Frame)
{
    let ic;
    {
        let a = fs.get_param(0);
        let b = fs.get_param(1);
        match (a,b) {
            (&Val::Int(ia), &Val::Int(ib)) => {
                ic = ia * ib;
            }
            _ => {
                panic!("can't multiply that! {:?}", (a,b));
            }
        }
    }
    fs.parent.set_result(Val::Int(ic));
}

pub fn int_mod(fs: &mut Frame)
{
    let ic;
    {
        let a = fs.get_param(0);
        let b = fs.get_param(1);
        match (a,b) {
            (&Val::Int(ia), &Val::Int(ib)) => {
                ic = ia % ib;
            }
            _ => {
                panic!("can't mod that! {:?}", (a,b));
            }
        }
    }
    fs.parent.set_result(Val::Int(ic));
}

pub fn int_negate(fs: &mut Frame)
{
    let result;
    {
        let a = fs.get_param(0);
        match a {
            &Val::Int(a) => {
                result = -a;
            }
            _ => {
                panic!("can't negate a not int? {:?}", a);
            }
        }
    }
    fs.parent.set_result(Val::Int(result));
}

pub fn int_random(fs: &mut Frame)
{
    let result = rand::random::<i64>(); // as i64;
    fs.parent.set_result(Val::Int(result))
}

pub fn bool_not(fs: &mut Frame)
{
    println!("run bool_not!");
    let bnot;
    {
        let bval = fs.get_param(0);
        bnot = match bval {
            &Val::Bool(b) => !b,
            _ => {
                panic!("wtf is all that? {:?}", bval);
            }
        }
    }
    fs.parent.set_result(Val::Bool(bnot));
}

pub fn bool_xor(fs: &mut Frame)
{
    let result;
    {
        let va = fs.get_param(0);
        let vb = fs.get_param(1);
        match (va,vb) {
            (&Val::Bool(a), &Val::Bool(b)) => {
                result = a && !b || b && !a;
            }
            _ => {
                panic!("wtf is all that? {:?}", (va,vb));
            }
        }
    }
    fs.parent.set_result(Val::Bool(result));
}

pub fn list_cons(fs: &mut Frame)
{
    let result = {
        let head = fs.e.get_param(0);
        let tail = fs.e.get_param(1);
        list::cons(head.clone(), tail.clone())
    };
    fs.parent.set_result(result);
}

pub fn less_than(fs: &mut Frame)
{
    let va = fs.e.get_param(0);
    let vb = fs.e.get_param(1);
    fs.parent.set_result(Val::Bool(va < vb));
}

pub fn less_than_equal(fs: &mut Frame)
{
    let va = fs.e.get_param(0);
    let vb = fs.e.get_param(1);
    fs.parent.set_result(Val::Bool(va <= vb));
}

pub fn equal(fs: &mut Frame)
{
    let va = fs.e.get_param(0);
    let vb = fs.e.get_param(1);
    fs.parent.set_result(Val::Bool(va == vb));
}

pub fn greater_than(fs: &mut Frame)
{
    let result;
    {
        let va = fs.get_param(0);
        let vb = fs.get_param(1);
        result = va > vb;
    }
    fs.parent.set_result(Val::Bool(result));
}

pub fn greater_than_equal(fs: &mut Frame)
{
    let result;
    {
        let va = fs.get_param(0);
        let vb = fs.get_param(1);
        result = va >= vb;
    }
    fs.parent.set_result(Val::Bool(result));
}

pub fn get_type(fs: &mut Frame)
{
    let result;
    {
        let v = fs.get_param(0);
        result = v.get_type();
    }
    fs.parent.set_result(Val::Type(result));
}

pub fn cout(fs: &mut Frame)
{
    let v = fs.e.get_param(0);
    print!("{}", v);
    fs.parent.set_result(Val::Void);
}

pub fn cerr(fs: &mut Frame)
{
    {
        let va = fs.get_param(0);
        write!(stderr(), "{}", va);
    }
    fs.parent.set_result(Val::Void);
}


struct LeemaFile {
    f: Mutex<File>,
}

impl LeemaFile
{
    pub fn new(f: File) -> LeemaFile
    {
        LeemaFile{f: Mutex::new(f)}
    }
}

impl Display for LeemaFile
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "LeemaFile")
    }
}

impl Debug for LeemaFile
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "LeemaFile")
    }
}


pub fn file_read(fs: &mut Frame)
{
    let open_result = {
        let fnval = fs.get_param(0);
        match fnval {
            &Val::Str(ref fnstr) => {
                File::open(&**fnstr)
            }
            _ => {
                panic!("Can't open file with not string {:?}"
                    , fnval);
            }
        }
    };
    let openf = match open_result {
        Ok(f) => {
            Val::new_lib(
                LeemaFile::new(f),
                Type::Lib("File".to_string()),
            )
        }
        Err(_) => Val::failure(
            Val::hashtag("file_open_fail".to_string()),
            Val::new_str("Failed to open file".to_string()),
            fs.trace.failure_here(),
            )
    };
    fs.parent.set_result(openf);
}

pub fn file_stream_read(fs: &mut Frame)
{
    let mut input = "".to_string();
    {
        let mut streamval = fs.get_param_mut(0);
        let mut anyf = match streamval {
            &mut Val::Lib(ref lv) =>
            {
                &*lv.v as &Any
            }
            _ => {
                panic!("Can't read from not a File {:?}"
                    , streamval);
            }
        };
        let mut optf = Any::downcast_ref::<LeemaFile>(anyf);
        let mut myfref = optf.unwrap();
        let mut lockf = myfref.f.lock();
        let mut rawf = lockf.unwrap();
        let mut result = rawf.read_to_string(&mut input);
        //let result = myf.f.lock().unwrap().read_to_string(&mut input);
    }
println!("read from file: '{}'", input);
    fs.parent.set_result(Val::new_str(input));
}

pub fn source_code() -> &'static str
{
    "macro boolean_and(a, b) ->
        if
        |a -> b
        |else -> false
        --
    --

    macro boolean_or(a, b) ->
        if
        |a -> true
        |else -> b
        --
    --

    func int_add(a: Int, b: Int): Int -RUST-
    func int_sub(a: Int, b: Int): Int -RUST-
    func int_mult(a: Int, b: Int): Int -RUST-
    func int_mod(a: Int, b: Int): Int -RUST-
    func int_negate(a: Int): Int -RUST-
    func int_random(): Int -RUST-
    func equal(a, b): Bool -RUST-
    func less_than(a, b): Bool -RUST-
    func cout(txt: Str): Void -RUST-
    func list_cons(head, tail): List -RUST-
    "
}

macro_rules! load_rust_funcs {
    ( $fname:ident, $( $f:ident ),* ) => {
        match $fname {
            $(
            stringify!($f) => Some($f),
            )*
            _ => None,
        }
    }
}

pub fn load_rust_func(func_name: &str) -> Option<RustFunc>
{
    load_rust_funcs!(
        func_name,
        int_add,
        int_sub,
        int_mult,
        int_mod,
        int_negate,
        int_random,
        bool_not,
        bool_xor,
        list_cons,
        less_than,
        less_than_equal,
        equal,
        greater_than,
        greater_than_equal,
        get_type,
        cout,
        file_read,
        file_stream_read
    )
}
