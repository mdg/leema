
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

    pub fn str(&self) -> &str
    {
        match self {
            &Lstr::Rc(ref s) => &(**s),
            &Lstr::Arc(ref s) => &(**s),
            &Lstr::Sref(ref s) => s,
            _ => {
                panic!("not a str: {:?}", self);
            }
        }
    }
}

impl<'a> From<&'a Lstr> for String
{
    fn from(ls: &'a Lstr) -> String
    {
        match ls {
            &Lstr::Rc(ref s) => (**s).clone(),
            &Lstr::Arc(ref s) => (**s).clone(),
            &Lstr::Sref(ref s) => s.to_string(),
            &Lstr::Cat(ref a, ref b) => {
                format!("{}{}", a, b)
            }
        }
    }
}

impl<'a> From<&'a Lstr> for Rc<String>
{
    fn from(ls: &'a Lstr) -> Rc<String>
    {
        match ls {
            &Lstr::Rc(ref s) => s.clone(),
            &Lstr::Arc(ref s) => Rc::new((**s).clone()),
            &Lstr::Sref(ref s) => Rc::new(s.to_string()),
            &Lstr::Cat(ref a, ref b) => {
                Rc::new(format!("{}{}", a, b))
            }
        }
    }
}

impl From<String> for Lstr
{
    fn from(s: String) -> Lstr
    {
        Lstr::Rc(Rc::new(s))
    }
}

impl<'a> From<&'a String> for Lstr
{
    fn from(s: &'a String) -> Lstr
    {
        Lstr::Rc(Rc::new(s.clone()))
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


#[cfg(test)]
mod tests {
    use leema::lstr::{Lstr};
    use std::collections::{HashSet, HashMap};

#[test]
fn test_hashset_contains_sref() {
    let mut s = HashSet::new();
    s.insert(Lstr::from_sref("tacos"));
    assert!(s.contains(Lstr::from_sref("tacos")));
}

}
