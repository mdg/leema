use leema::val::{Val, Type, LibVal};
use leema::code::{Code, RustFunc};
use leema::fiber::{Fiber};
use leema::frame::{Frame, Event};
use leema::list;
use leema::log;

use std::fs::File;
use std::io::{stderr, Read, Write};
use std::any::{Any};
use std::fmt::{self, Display, Debug};
use std::sync::{Mutex};
use std::rc::{Rc};

use rand;


pub fn int_add(mut f: Fiber) -> Event
{
    let ic;
    {
        let a = f.head.get_param(0);
        let b = f.head.get_param(1);
        match (a,b) {
            (&Val::Int(ia), &Val::Int(ib)) => {
                ic = ia + ib;
            }
            _ => {
                panic!("wtf is all that? {:?}", (a,b));
            }
        }
    }
    f.head.parent.set_result(Val::Int(ic));
    Event::success(f)
}

pub fn int_sub(mut f: Fiber) -> Event
{
    let ic;
    {
        let a = f.head.get_param(0);
        let b = f.head.get_param(1);
        match (a,b) {
            (&Val::Int(ia), &Val::Int(ib)) => {
                ic = ia - ib;
            }
            _ => {
                panic!("wtf is all that? {:?}", (a,b));
            }
        }
    }
    f.head.parent.set_result(Val::Int(ic));
    Event::success(f)
}

pub fn int_mult(mut f: Fiber) -> Event
{
    let ic;
    {
        let a = f.head.get_param(0);
        let b = f.head.get_param(1);
        match (a,b) {
            (&Val::Int(ia), &Val::Int(ib)) => {
                ic = ia * ib;
            }
            _ => {
                panic!("can't multiply that! {:?}", (a,b));
            }
        }
    }
    f.head.parent.set_result(Val::Int(ic));
    Event::success(f)
}

pub fn int_mod(mut f: Fiber) -> Event
{
    let ic;
    {
        let a = f.head.get_param(0);
        let b = f.head.get_param(1);
        match (a,b) {
            (&Val::Int(ia), &Val::Int(ib)) => {
                ic = ia % ib;
            }
            _ => {
                panic!("can't mod that! {:?}", (a,b));
            }
        }
    }
    f.head.parent.set_result(Val::Int(ic));
    Event::success(f)
}

pub fn int_negate(mut f: Fiber) -> Event
{
    let result;
    {
        let a = f.head.get_param(0);
        match a {
            &Val::Int(a) => {
                result = -a;
            }
            _ => {
                panic!("can't negate a not int? {:?}", a);
            }
        }
    }
    f.head.parent.set_result(Val::Int(result));
    Event::success(f)
}

pub fn int_random(mut f: Fiber) -> Event
{
    let result = rand::random::<i64>(); // as i64;
    f.head.parent.set_result(Val::Int(result));
    Event::success(f)
}

pub fn bool_not(mut f: Fiber) -> Event
{
    println!("run bool_not!");
    let bnot;
    {
        let bval = f.head.get_param(0);
        bnot = match bval {
            &Val::Bool(b) => !b,
            _ => {
                panic!("wtf is all that? {:?}", bval);
            }
        }
    }
    f.head.parent.set_result(Val::Bool(bnot));
    Event::success(f)
}

pub fn bool_xor(mut f: Fiber) -> Event
{
    let result;
    {
        let va = f.head.get_param(0);
        let vb = f.head.get_param(1);
        match (va,vb) {
            (&Val::Bool(a), &Val::Bool(b)) => {
                result = a && !b || b && !a;
            }
            _ => {
                panic!("wtf is all that? {:?}", (va,vb));
            }
        }
    }
    f.head.parent.set_result(Val::Bool(result));
    Event::success(f)
}

pub fn list_cons(mut f: Fiber) -> Event
{
    let result = {
        let head = f.head.e.get_param(0);
        let tail = f.head.e.get_param(1);
        list::cons(head.clone(), tail.clone())
    };
    f.head.parent.set_result(result);
    Event::success(f)
}

pub fn less_than(mut f: Fiber) -> Event
{
    let result = {
        let va = f.head.e.get_param(0);
        let vb = f.head.e.get_param(1);
        Val::Bool(va < vb)
    };
    f.head.parent.set_result(result);
    Event::success(f)
}

pub fn less_than_equal(mut f: Fiber) -> Event
{
    let result = {
        let va = f.head.e.get_param(0);
        let vb = f.head.e.get_param(1);
        Val::Bool(va <= vb)
    };
    f.head.parent.set_result(result);
    Event::success(f)
}

pub fn equal(mut f: Fiber) -> Event
{
    let result = {
        let va = f.head.e.get_param(0);
        let vb = f.head.e.get_param(1);
        Val::Bool(va == vb)
    };
    f.head.parent.set_result(result);
    Event::success(f)
}

pub fn greater_than(mut f: Fiber) -> Event
{
    let result = {
        let va = f.head.get_param(0);
        let vb = f.head.get_param(1);
        va > vb
    };
    f.head.parent.set_result(Val::Bool(result));
    Event::success(f)
}

pub fn greater_than_equal(mut f: Fiber) -> Event
{
    let result = {
        let va = f.head.get_param(0);
        let vb = f.head.get_param(1);
        va >= vb
    };
    f.head.parent.set_result(Val::Bool(result));
    Event::success(f)
}

pub fn get_type(mut f: Fiber) -> Event
{
    let result: Type;
    {
        let v = f.head.get_param(0);
        result = v.get_type();
    }
    f.head.parent.set_result(Val::Type(result));
    Event::success(f)
}

pub fn cout(mut f: Fiber) -> Event
{
    {
        let v = f.head.e.get_param(0);
        print!("{}", v);
    }
    f.head.parent.set_result(Val::Void);
    Event::success(f)
}

pub fn cerr(mut f: Fiber) -> Event
{
    {
        let va = f.head.get_param(0);
        write!(stderr(), "{}", va);
    }
    f.head.parent.set_result(Val::Void);
    Event::success(f)
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

impl LibVal for LeemaFile
{
    fn get_type(&self) -> Type
    {
        Type::Lib("File".to_string())
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


pub fn file_read(mut f: Fiber) -> Event
{
    let open_result = {
        let fnval = f.head.get_param(0);
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
        Ok(file) => {
            Val::libval(LeemaFile::new(file))
        }
        Err(_) => Val::failure(
            Val::hashtag("file_open_fail".to_string()),
            Val::new_str("Failed to open file".to_string()),
            f.head.trace.failure_here(),
            )
    };
    f.head.parent.set_result(openf);
    Event::success(f)
}

pub fn file_stream_read(mut f: Fiber) -> Event
{
    let mut input = "".to_string();
    {
        let mut streamval = f.head.e.get_param_mut(0);
        let mut optf = streamval.libval_as();
        let mut myfref: &LeemaFile = optf.unwrap();
        let mut lockf = myfref.f.lock();
        let mut rawf = lockf.unwrap();
        let mut result = rawf.read_to_string(&mut input);
        //let result = myf.f.lock().unwrap().read_to_string(&mut input);
    }
println!("read from file: '{}'", input);
    f.head.parent.set_result(Val::new_str(input));
    Event::success(f)
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
    func list_cons(head: $A, tail: [$A]): [$A] -RUST-

    func int_abs(a: Int): Int ->
        if
        |a < 0 -> ~a
        |else -> a
        --
    --
    "
}

macro_rules! load_rust_funcs {
    ( $fname:ident, $( $f:ident ),* ) => {
        match $fname {
            $(
            stringify!($f) => Some(Code::Rust($f)),
            )*
            _ => None,
        }
    }
}

pub fn load_rust_func(func_name: &str) -> Option<Code>
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
