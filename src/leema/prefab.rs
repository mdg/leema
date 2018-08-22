use leema::code::Code;
use leema::fiber::Fiber;
use leema::frame::Event;
use leema::list;
use leema::log;
use leema::lstr::Lstr;
use leema::val::{self, LibVal, Type, Val};

use std::fmt::{self, Debug, Display};
use std::fs::File;
use std::io::{stderr, stdin, Read, Write};
use std::sync::Mutex;
use std::time::Duration;

use rand;


pub fn int_add(f: &mut Fiber) -> Event
{
    let ic;
    {
        let a = f.head.get_param(0);
        let b = f.head.get_param(1);
        match (a, b) {
            (&Val::Int(ia), &Val::Int(ib)) => {
                ic = ia + ib;
            }
            _ => {
                panic!("wtf is all that? {:?}", (a, b));
            }
        }
    }
    f.head.parent.set_result(Val::Int(ic));
    Event::success()
}

pub fn int_sub(f: &mut Fiber) -> Event
{
    let ic;
    {
        let a = f.head.get_param(0);
        let b = f.head.get_param(1);
        match (a, b) {
            (&Val::Int(ia), &Val::Int(ib)) => {
                ic = ia - ib;
            }
            _ => {
                panic!("wtf is all that? {:?}", (a, b));
            }
        }
    }
    f.head.parent.set_result(Val::Int(ic));
    Event::success()
}

pub fn int_mult(f: &mut Fiber) -> Event
{
    let ic;
    {
        let a = f.head.get_param(0);
        let b = f.head.get_param(1);
        match (a, b) {
            (&Val::Int(ia), &Val::Int(ib)) => {
                ic = ia * ib;
            }
            _ => {
                panic!("can't multiply that! {:?}", (a, b));
            }
        }
    }
    f.head.parent.set_result(Val::Int(ic));
    Event::success()
}

pub fn int_mod(f: &mut Fiber) -> Event
{
    let ic;
    {
        let a = f.head.get_param(0);
        let b = f.head.get_param(1);
        match (a, b) {
            (&Val::Int(ia), &Val::Int(ib)) => {
                ic = ia % ib;
            }
            _ => {
                panic!("can't mod that! {:?}", (a, b));
            }
        }
    }
    f.head.parent.set_result(Val::Int(ic));
    Event::success()
}

pub fn int_negate(f: &mut Fiber) -> Event
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
    Event::success()
}

pub fn int_random(f: &mut Fiber) -> Event
{
    let result = rand::random::<i64>(); // as i64;
    f.head.parent.set_result(Val::Int(result));
    Event::success()
}

pub fn bool_not(f: &mut Fiber) -> Event
{
    let i = f.head.e.get_param(0);
    if let &Val::Bool(b) = i {
        f.head.parent.set_result(Val::Bool(!b));
        Event::success()
    } else {
        let tag = Val::Hashtag(Lstr::Sref("invalid_type"));
        let msg = Val::Str(Lstr::from(format!(
            "input to not must be a boolean: {:?}",
            i
        )));

        let fail =
            Val::failure(tag, msg, f.head.trace.clone(), val::FAILURE_TYPE);
        f.head.parent.set_result(fail);
        Event::failure()
    }
}

pub fn bool_xor(f: &mut Fiber) -> Event
{
    let result;
    {
        let va = f.head.get_param(0);
        let vb = f.head.get_param(1);
        match (va, vb) {
            (&Val::Bool(a), &Val::Bool(b)) => {
                result = a && !b || b && !a;
            }
            _ => {
                panic!("wtf is all that? {:?}", (va, vb));
            }
        }
    }
    f.head.parent.set_result(Val::Bool(result));
    Event::success()
}

pub fn list_cons(f: &mut Fiber) -> Event
{
    let result = {
        let head = f.head.e.get_param(0);
        let tail = f.head.e.get_param(1);
        list::cons(head.clone(), tail.clone())
    };
    f.head.parent.set_result(result);
    Event::success()
}

pub fn less_than(f: &mut Fiber) -> Event
{
    let result = {
        let va = f.head.e.get_param(0);
        let vb = f.head.e.get_param(1);
        Val::Bool(va < vb)
    };
    f.head.parent.set_result(result);
    Event::success()
}

pub fn less_than_equal(f: &mut Fiber) -> Event
{
    let result = {
        let va = f.head.e.get_param(0);
        let vb = f.head.e.get_param(1);
        Val::Bool(va <= vb)
    };
    f.head.parent.set_result(result);
    Event::success()
}

pub fn equal(f: &mut Fiber) -> Event
{
    let result = {
        let va = f.head.e.get_param(0);
        let vb = f.head.e.get_param(1);
        Val::Bool(va == vb)
    };
    f.head.parent.set_result(result);
    Event::success()
}

pub fn greater_than(f: &mut Fiber) -> Event
{
    let result = {
        let va = f.head.get_param(0);
        let vb = f.head.get_param(1);
        va > vb
    };
    f.head.parent.set_result(Val::Bool(result));
    Event::success()
}

pub fn greater_than_equal(f: &mut Fiber) -> Event
{
    let result = {
        let va = f.head.get_param(0);
        let vb = f.head.get_param(1);
        va >= vb
    };
    f.head.parent.set_result(Val::Bool(result));
    Event::success()
}

pub fn get_type(f: &mut Fiber) -> Event
{
    let result: Type;
    {
        let v = f.head.get_param(0);
        result = v.get_type();
    }
    f.head.parent.set_result(Val::Type(result));
    Event::success()
}


pub fn leema_sleep(f: &mut Fiber) -> Event
{
    use std::thread::sleep;

    let tval = f.head.get_param(0);
    let tint = tval.to_int();
    sleep(Duration::from_millis(tint as u64));
    Event::success()
}

/**
 * cin
 */
pub fn cin(f: &mut Fiber) -> Event
{
    vout!("cin()\n");
    let mut input = String::new();
    match stdin().read_line(&mut input) {
        Ok(_) => {
            f.head.parent.set_result(Val::Str(Lstr::from(input)));
            Event::success()
        }
        Err(_) => {
            f.head.parent.set_result(Val::failure(
                Val::Hashtag(Lstr::Sref("console_read_fail")),
                Val::Str(Lstr::Sref("")),
                f.head.trace.fail_here(),
                val::FAILURE_INTERNAL,
            ));

            Event::success()
        }
    }
}

pub fn cout(f: &mut Fiber) -> Event
{
    {
        let v = f.head.e.get_param(0);
        print!("{}", v);
    }
    f.head.parent.set_result(Val::Void);
    Event::success()
}

pub fn cerr(f: &mut Fiber) -> Event
{
    {
        let va = f.head.get_param(0);
        write!(stderr(), "{}", va).expect("fail writing to stderr");
    }
    f.head.parent.set_result(Val::Void);
    Event::success()
}

pub fn create_failure(f: &mut Fiber) -> Event
{
    let failtag = f.head.e.get_param(0);
    let failmsg = f.head.e.get_param(1);
    let failure = Val::Failure(
        Box::new(failtag.clone()),
        Box::new(failmsg.clone()),
        f.head.trace.fail_here(),
        val::FAILURE_INTERNAL,
    );
    f.head.parent.set_result(failure);
    Event::failure()
}


struct LeemaFile
{
    f: Mutex<File>,
}

impl LeemaFile
{
    pub fn new(f: File) -> LeemaFile
    {
        LeemaFile { f: Mutex::new(f) }
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
    let open_result = {
        let fnval = f.head.get_param(0);
        match fnval {
            &Val::Str(ref fnstr) => File::open(&**fnstr),
            _ => {
                panic!("Can't open file with not string {:?}", fnval);
            }
        }
    };
    let openf = match open_result {
        Ok(file) => Val::libval(LeemaFile::new(file)),
        Err(_) => {
            Val::failure(
                Val::Hashtag(Lstr::Sref("file_open_fail")),
                Val::Str(Lstr::Sref("Failed to open file")),
                f.head.trace.fail_here(),
                val::FAILURE_INTERNAL,
            )
        }
    };
    f.head.parent.set_result(openf);
    Event::success()
}

pub fn file_stream_read(f: &mut Fiber) -> Event
{
    let mut input = "".to_string();
    {
        let streamval = f.head.e.get_param_mut(0);
        let optf = streamval.libval_as();
        let myfref: &LeemaFile = optf.unwrap();
        let lockf = myfref.f.lock();
        let mut rawf = lockf.unwrap();
        rawf.read_to_string(&mut input)
            .expect("failed to read from file to string");
        //let result = myf.f.lock().unwrap().read_to_string(&mut input);
    }
    f.head.parent.set_result(Val::Str(Lstr::from(input)));
    Event::success()
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
        leema_sleep,
        cin,
        cout,
        create_failure,
        file_read,
        file_stream_read
    )
}
