
static mut VERBOSE: bool = false;

pub fn set_verbose()
{
    unsafe {
        VERBOSE = true;
    }
}

pub fn is_verbose() -> bool
{
    unsafe {
        VERBOSE
    }
}

#[macro_export]
macro_rules! vout
{
    ($fmt:expr) => {
        if log::is_verbose() {
            write!(stderr(), $fmt).ok();
        }
    };
    ($fmt:expr, $($arg:tt)*) => {
        if log::is_verbose() {
            (write!(stderr(), $fmt, $($arg)*)).ok();
        }
    };
}
