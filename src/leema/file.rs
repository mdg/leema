use crate::leema::code::Code;
use crate::leema::failure::Lresult;
use crate::leema::fiber::Fiber;
use crate::leema::frame::Event;
use crate::leema::lstr::Lstr;
use crate::leema::rsrc;
use crate::leema::val::{self, LibVal, Type, Val};

use std::fmt::{self, Debug, Display};
use std::fs::{File, OpenOptions};
use std::io::{Read, Write};
use std::path::Path;
use std::sync::Mutex;


const TYPEPATH_FILE: &'static str = "/file/File";
const TYPE_FILE: Type = Type::named(TYPEPATH_FILE);

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
        TYPE_FILE
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

pub fn file_open(_ctx: rsrc::IopCtx) -> rsrc::Event
{
    vout!("file_open()\n");
    rsrc::Event::Result(Val::VOID)
}

pub fn file_read_file(mut ctx: rsrc::IopCtx) -> Lresult<rsrc::Event>
{
    vout!("file_read_file()\n");
    let pathval = ctx.take_param(0).unwrap();
    let path = Path::new(pathval.str());
    let mut f = File::open(path).unwrap();
    let mut s = String::new();
    f.read_to_string(&mut s).expect("read_to_string failure");
    ctx.set_result(Val::Str(Lstr::from(s)));
    Ok(rsrc::Event::Complete)
}

pub fn file_write(_ctx: rsrc::IopCtx) -> rsrc::Event
{
    vout!("file_write()\n");
    rsrc::Event::Result(Val::VOID)
}

pub fn file_write_file(mut ctx: rsrc::IopCtx) -> Lresult<rsrc::Event>
{
    vout!("file_write_file()\n");
    let pathval = ctx.take_param(0).unwrap();
    let output = ctx.take_param(1).unwrap();
    let path = Path::new(pathval.str());
    let mut f = OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open(path)
        .unwrap();
    f.write_all(output.str().as_bytes())
        .expect("write_all failure");
    Ok(rsrc::Event::Complete)
}

pub fn file_exists(mut ctx: rsrc::IopCtx) -> Lresult<rsrc::Event>
{
    let pathval = ctx.take_param(0).unwrap();
    let exists = Path::new(pathval.str()).exists();
    ctx.set_result(Val::Bool(exists));
    Ok(rsrc::Event::Complete)
}

pub fn file_read(f: &mut Fiber) -> Lresult<Event>
{
    let open_result = {
        let fnval = f.head.e.get_param(0)?;
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
    f.head.e.set_result(openf);
    Event::success()
}

pub fn file_stream_read(f: &mut Fiber) -> Lresult<Event>
{
    /*
     * reimplement this on the io worker
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
    f.head.e.set_result(Val::Str(Lstr::from(input)));
    */
    f.head.e.set_result(Val::Str(Lstr::Sref("")));
    Event::success()
}

pub fn load_rust_func(func_name: &str) -> Option<Code>
{
    match func_name {
        "file_read" => Some(Code::Rust(file_read)),
        "file_stream_read" => Some(Code::Rust(file_stream_read)),
        "read_file" => Some(Code::Iop(file_read_file, None)),
        "write_file" => Some(Code::Iop(file_write_file, None)),
        "exists" => Some(Code::Iop(file_exists, None)),
        _ => None,
    }
}
