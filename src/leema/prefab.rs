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


pub fn int_add(f: &mut Fiber) -> Event
{
    let fs = &mut f.head;
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
    Event::success()
}

pub fn int_sub(f: &mut Fiber) -> Event
{
    let fs = &mut f.head;
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
    Event::success()
}

pub fn int_mult(f: &mut Fiber) -> Event
{
    let fs = &mut f.head;
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
    Event::success()
}

pub fn int_mod(f: &mut Fiber) -> Event
{
    let fs = &mut f.head;
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
    Event::success()
}

pub fn int_negate(f: &mut Fiber) -> Event
{
    let fs = &mut f.head;
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
    Event::success()
}

pub fn int_random(f: &mut Fiber) -> Event
{
    let fs = &mut f.head;
    let result = rand::random::<i64>(); // as i64;
    fs.parent.set_result(Val::Int(result));
    Event::success()
}

pub fn bool_not(f: &mut Fiber) -> Event
{
    let fs = &mut f.head;
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
    Event::success()
}

pub fn bool_xor(f: &mut Fiber) -> Event
{
    let fs = &mut f.head;
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
    Event::success()
}

pub fn list_cons(f: &mut Fiber) -> Event
{
    let fs = &mut f.head;
    let result = {
        let head = fs.e.get_param(0);
        let tail = fs.e.get_param(1);
        list::cons(head.clone(), tail.clone())
    };
    fs.parent.set_result(result);
    Event::success()
}

pub fn less_than(f: &mut Fiber) -> Event
{
    let fs = &mut f.head;
    let va = fs.e.get_param(0);
    let vb = fs.e.get_param(1);
    fs.parent.set_result(Val::Bool(va < vb));
    Event::success()
}

pub fn less_than_equal(f: &mut Fiber) -> Event
{
    let fs = &mut f.head;
    let va = fs.e.get_param(0);
    let vb = fs.e.get_param(1);
    fs.parent.set_result(Val::Bool(va <= vb));
    Event::success()
}

pub fn equal(f: &mut Fiber) -> Event
{
    let fs = &mut f.head;
    let va = fs.e.get_param(0);
    let vb = fs.e.get_param(1);
    fs.parent.set_result(Val::Bool(va == vb));
    Event::success()
}

pub fn greater_than(f: &mut Fiber) -> Event
{
    let fs = &mut f.head;
    let result;
    {
        let va = fs.get_param(0);
        let vb = fs.get_param(1);
        result = va > vb;
    }
    fs.parent.set_result(Val::Bool(result));
    Event::success()
}

pub fn greater_than_equal(f: &mut Fiber) -> Event
{
    let fs = &mut f.head;
    let result;
    {
        let va = fs.get_param(0);
        let vb = fs.get_param(1);
        result = va >= vb;
    }
    fs.parent.set_result(Val::Bool(result));
    Event::success()
}

pub fn get_type(f: &mut Fiber) -> Event
{
    let fs = &mut f.head;
    let result: Type;
    {
        let v = fs.get_param(0);
        result = v.get_type();
    }
    fs.parent.set_result(Val::Type(result));
    Event::success()
}

pub fn cout(f: &mut Fiber) -> Event
{
    let fs = &mut f.head;
    let v = fs.e.get_param(0);
    print!("{}", v);
    fs.parent.set_result(Val::Void);
    Event::success()
}

pub fn cerr(f: &mut Fiber) -> Event
{
    let fs = &mut f.head;
    {
        let va = fs.get_param(0);
        write!(stderr(), "{}", va);
    }
    fs.parent.set_result(Val::Void);
    Event::success()
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


pub fn file_read(f: &mut Fiber) -> Event
{
    let fs = &mut f.head;
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
            Val::libval(LeemaFile::new(f))
        }
        Err(_) => Val::failure(
            Val::hashtag("file_open_fail".to_string()),
            Val::new_str("Failed to open file".to_string()),
            fs.trace.failure_here(),
            )
    };
    fs.parent.set_result(openf);
    Event::success()
}

pub fn file_stream_read(f: &mut Fiber) -> Event
{
    let fs = &mut f.head;
    let mut input = "".to_string();
    {
        let mut streamval = fs.get_param_mut(0);
        let mut optf = streamval.libval_as();
        let mut myfref: &LeemaFile = optf.unwrap();
        let mut lockf = myfref.f.lock();
        let mut rawf = lockf.unwrap();
        let mut result = rawf.read_to_string(&mut input);
        //let result = myf.f.lock().unwrap().read_to_string(&mut input);
    }
println!("read from file: '{}'", input);
    fs.parent.set_result(Val::new_str(input));
    Event::success()
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
