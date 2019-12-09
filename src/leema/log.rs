static mut VERBOSE: bool = false;


pub fn set_verbose()
{
    unsafe {
        VERBOSE = true;
    }
}

pub fn is_verbose() -> bool
{
    unsafe { VERBOSE }
}

#[macro_export]
macro_rules! vout
{
    ($fmt:expr) => {
        if crate::leema::log::is_verbose() {
            eprint!($fmt);
        }
    };
    ($fmt:expr, $($arg:tt)*) => {
        if crate::leema::log::is_verbose() {
            (eprint!($fmt, $($arg)*));
        }
    };
}

#[macro_export]
macro_rules! start_timer
{
    () => {
        std::time::Instant::now()
    };
}

#[macro_export]
macro_rules! log_timer
{
    ($start:expr, $msg:expr) => {
        let dur = $start.elapsed();
        vout!("{:?} {}\n", dur, $msg);
    };
    ($start:expr, $fmt:expr, $($arg:tt)*) => {
        let dur = $start.elapsed();
        vout!("{:?} ", dur);
        vout!($fmt, $($arg)*);
        vout!("\n");
    };
}
