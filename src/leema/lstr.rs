
use std::fmt;
use std::rc::Rc;
use std::sync::Arc;


#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
#[derive(Eq)]
#[derive(Hash)]
pub enum Lstr
{
    Rc(Rc<String>),
    Arc(Arc<String>),
    Sref(&'static str),
    Cat(Box<Lstr>, Box<Lstr>),
}

impl Lstr
{
    pub fn empty() -> Lstr
    {
        Lstr::Sref("")
    }

    pub fn from_string(s: String) -> Lstr
    {
        Lstr::Rc(Rc::new(s))
    }

    pub fn from_sref(sref: &'static str) -> Lstr
    {
        Lstr::Sref(sref)
    }

    pub fn cat(a: Lstr, b: Lstr) -> Lstr
    {
        Lstr::Cat(Box::new(a), Box::new(b))
    }
}

impl fmt::Display for Lstr
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self {
            &Lstr::Rc(ref s) => {
                write!(f, "{}", s)
            }
            &Lstr::Arc(ref s) => {
                write!(f, "{}", s)
            }
            &Lstr::Sref(ref s) => {
                write!(f, "{}", s)
            }
            &Lstr::Cat(ref a, ref b) => {
                write!(f, "{}{}", a, b)
            }
        }
    }
}
