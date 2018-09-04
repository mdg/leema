use leema::sendclone::SendClone;

use std::borrow::Borrow;
use std::cmp::{Ordering, PartialEq, PartialOrd};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops::Deref;
use std::sync::Arc;


#[derive(Debug)]
#[derive(Clone)]
#[derive(Eq)]
pub enum Lstr
{
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

    pub fn cat(a: Lstr, b: Lstr) -> Lstr
    {
        Lstr::Cat(Box::new(a), Box::new(b))
    }

    pub fn str(&self) -> &str
    {
        match self {
            &Lstr::Arc(ref s) => &(**s),
            &Lstr::Sref(ref s) => s,
            _ => {
                panic!("not a str: {:?}", self);
            }
        }
    }
}

impl SendClone for Lstr
{
    type Item = Lstr;

    fn clone_for_send(&self) -> Lstr
    {
        match self {
            &Lstr::Arc(ref s) => Lstr::Arc(s.clone()),
            &Lstr::Sref(ref s) => Lstr::Sref(s),
            _ => {
                panic!("cannot Lstr::clone_for_send: {:?}", self);
            }
        }
    }
}

impl<'a> From<&'a Lstr> for String
{
    fn from(ls: &'a Lstr) -> String
    {
        match ls {
            &Lstr::Arc(ref s) => (**s).clone(),
            &Lstr::Sref(ref s) => s.to_string(),
            &Lstr::Cat(ref a, ref b) => format!("{}{}", a, b),
        }
    }
}

impl From<String> for Lstr
{
    fn from(s: String) -> Lstr
    {
        Lstr::Arc(Arc::new(s))
    }
}

impl<'a> From<&'a String> for Lstr
{
    fn from(s: &'a String) -> Lstr
    {
        Lstr::Arc(Arc::new(s.clone()))
    }
}

impl<'a> From<&'static str> for Lstr
{
    fn from(s: &'static str) -> Lstr
    {
        Lstr::Sref(s)
    }
}

impl AsRef<str> for Lstr
{
    fn as_ref(&self) -> &str
    {
        self.str()
    }
}

impl Deref for Lstr
{
    type Target = str;

    fn deref(&self) -> &str
    {
        self.str()
    }
}

impl Borrow<str> for Lstr
{
    fn borrow(&self) -> &str
    {
        self.str()
    }
}

impl PartialEq for Lstr
{
    fn eq(&self, b: &Lstr) -> bool
    {
        PartialEq::eq(self.str(), b.str())
    }
}

impl<'a, 'b> PartialEq<&'b Lstr> for &'a str
{
    fn eq(&self, other: &&Lstr) -> bool
    {
        *self == (*other).str()
    }
}

impl<'a, 'b> PartialEq<str> for Lstr
{
    fn eq(&self, other: &str) -> bool
    {
        self.str() == other
    }
}

impl PartialOrd for Lstr
{
    fn partial_cmp(&self, other: &Lstr) -> Option<Ordering>
    {
        PartialOrd::partial_cmp(self.str(), other.str())
    }
}

impl fmt::Display for Lstr
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self {
            &Lstr::Arc(ref s) => write!(f, "{}", s),
            &Lstr::Sref(ref s) => write!(f, "{}", s),
            &Lstr::Cat(ref a, ref b) => write!(f, "{}{}", a, b),
        }
    }
}

/**
 * delegate hash to the str version of the Lstr
 */
impl Hash for Lstr
{
    fn hash<H: Hasher>(&self, state: &mut H)
    {
        self.str().hash(state)
    }
}


#[cfg(test)]
mod tests
{
    use leema::lstr::Lstr;

    use std::collections::HashSet;
    use std::sync::Arc;

    #[test]
    fn test_eq_arc_arc()
    {
        let a = Lstr::Arc(Arc::new(String::from("aaa")));
        let b = Lstr::Arc(Arc::new(String::from("aaa")));
        assert_eq!(a, b);
        assert_eq!(b, a);
    }

    #[test]
    fn test_eq_arc_sref()
    {
        let a = Lstr::Arc(Arc::new(Rc::new(String::from("aaa"))));
        let b = Lstr::from("aaa");
        assert_eq!(a, b);
        assert_eq!(b, a);
    }

    #[test]
    fn test_eq_str_lstr()
    {
        let a = Lstr::from("abc");
        assert_eq!("abc", &a);
    }

    #[test]
    fn test_eq_lstr_str()
    {
        let a = Lstr::from("abc");
        assert_eq!(&a, "abc");
    }

    #[test]
    fn test_ne_arc_arc()
    {
        let a = Lstr::Arc(Arc::new(String::from("aaa")));
        let b = Lstr::Arc(Arc::new(String::from("bbb")));
        assert_ne!(a, b);
    }

    #[test]
    fn test_ne_arc_sref()
    {
        let a = Lstr::Arc(Arc::new(String::from("aaa")));
        let b = Lstr::from("bbb");
        assert_ne!(a, b);
    }

    #[test]
    fn test_hashset_contains_sref()
    {
        let mut s = HashSet::new();
        s.insert(Lstr::from("tacos"));
        assert!(s.contains(&Lstr::from("tacos")));
    }

    #[test]
    fn test_hashset_with_sref_contains_arc()
    {
        let mut s = HashSet::new();
        s.insert(Lstr::from("tacos"));
        assert!(s.contains(&Lstr::from(String::from("tacos"))));
    }

    #[test]
    fn test_hashset_contains_str()
    {
        let mut s = HashSet::new();
        s.insert(Lstr::from("tacos"));
        assert!(s.contains("tacos"));
    }

    fn str_identity(ls: &str) -> &str
    {
        ls
    }

    #[test]
    fn test_borrow_lstr_as_str()
    {
        let s1 = str_identity("tacos");
        let s2 = str_identity(&Lstr::Sref("tacos"));

        assert_eq!(s1, s2);
    }

}
