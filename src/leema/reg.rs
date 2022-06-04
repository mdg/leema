use crate::leema::failure::Lresult;
use crate::leema::val::Val;

use std::collections::HashMap;
use std::fmt;

#[derive(PartialEq)]
#[derive(Eq)]
#[derive(PartialOrd)]
#[derive(Ord)]
#[derive(Clone)]
#[derive(Copy)]
pub enum Ireg
{
    Reg(i8),
    Sub(i8, i8),
}

impl Ireg
{
    pub fn sub(&self, newsub: i8) -> Ireg
    {
        match self {
            Ireg::Reg(r) => Ireg::Sub(*r, newsub),
            Ireg::Sub(_, _) => {
                panic!("cannot take sub reg of sub reg: {}", self);
            }
        }
    }

    pub fn is_primary(&self) -> bool
    {
        match self {
            Ireg::Reg(_) => true,
            Ireg::Sub(_, _) => false,
        }
    }

    pub fn get_primary(&self) -> i8
    {
        match self {
            Ireg::Reg(r) => *r,
            Ireg::Sub(r, _) => *r,
        }
    }

    pub fn next_sibling(&self) -> Ireg
    {
        match self {
            Ireg::Sub(r, s) => Ireg::Sub(*r, s + 1),
            Ireg::Reg(r) => Ireg::Reg(r + 1),
        }
    }
}

impl fmt::Display for Ireg
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self {
            Ireg::Reg(r) => write!(f, ".{}", r),
            Ireg::Sub(r, ref s) => write!(f, ".{}.{}", r, s),
        }
    }
}

impl fmt::Debug for Ireg
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "{}", self)
    }
}

pub trait Iregistry
{
    fn ireg_get(&self, r: Ireg) -> Lresult<&Val>;
    fn ireg_set(&mut self, r: Ireg, v: Val) -> Lresult<()>;
}

#[derive(PartialEq)]
#[derive(Eq)]
#[derive(PartialOrd)]
#[derive(Ord)]
#[derive(Clone)]
#[derive(Copy)]
pub enum Reg
{
    Param(Ireg),
    Local(Ireg),
    /// Top of the stack
    Top,
    Lib,
    /// Maybe call it Reg::Null, like /dev/null?
    Void,
    Undecided,
}

impl Reg
{
    pub fn sub(&self, sub: i8) -> Reg
    {
        match self {
            Reg::Param(ref r) => Reg::Param(r.sub(sub)),
            Reg::Local(ref r) => Reg::Local(r.sub(sub)),
            Reg::Void => Reg::Void,
            _ => {
                panic!("Can't make a sub reg for {:?}", self);
            }
        }
    }

    pub fn param(p: i8) -> Reg
    {
        Reg::Param(Ireg::Reg(p))
    }

    pub fn local(p: i8) -> Reg
    {
        Reg::Local(Ireg::Reg(p))
    }

    pub fn is_primary(&self) -> bool
    {
        matches!(
            self,
            Reg::Param(Ireg::Reg(_)) | Reg::Local(Ireg::Reg(_)) | Reg::Top
        )
    }

    pub fn is_sub(&self) -> bool
    {
        matches!(
            self,
            Reg::Param(Ireg::Sub(_, _)) | Reg::Local(Ireg::Sub(_, _))
        )
    }

    pub fn next_sibling(&self) -> Reg
    {
        match self {
            Reg::Param(ref r) => Reg::Param(r.next_sibling()),
            Reg::Local(ref r) => Reg::Local(r.next_sibling()),
            _ => {
                panic!("register has no sibling: {:?}", self);
            }
        }
    }
}

impl fmt::Display for Reg
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self {
            Reg::Param(ref r) => write!(f, "Param{}", r),
            Reg::Local(ref r) => write!(f, "Local{}", r),
            Reg::Top => write!(f, "Reg::Top"),
            Reg::Lib => write!(f, "Reg::Lib"),
            Reg::Void => write!(f, "Reg::Void"),
            Reg::Undecided => write!(f, "Reg::Undecided"),
        }
    }
}

impl fmt::Debug for Reg
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "{}", self)
    }
}

/// Table for allocating local registers for a function
///
/// Local registers are allocated for the duration of a function
///
/// Down:
/// - ids assign new or existing
/// - calls assign new, push params as current
#[derive(Debug)]
pub struct RegTab
{
    ids: HashMap<&'static str, Reg>,
    next_local: i8,
}

impl RegTab
{
    pub fn with_args(args: Vec<&'static str>) -> RegTab
    {
        let mut ids = HashMap::new();
        for (i, name) in args.iter().enumerate() {
            ids.insert(*name, Reg::param(i as i8));
        }
        RegTab { ids, next_local: 0 }
    }

    pub fn push(&self) -> RegTab
    {
        RegTab {
            ids: HashMap::new(),
            next_local: self.next_local,
        }
    }

    pub fn new_name(&mut self, id: &'static str) -> Reg
    {
        let r = self.next_local;
        self.next_local += 1;
        self.ids.insert(id, Reg::local(r));
        Reg::local(r)
    }

    pub fn with_name(&self, id: &'static str) -> Option<Reg>
    {
        self.ids.get(id).copied()
    }

    pub fn unnamed(&mut self) -> Reg
    {
        let r = self.next_local;
        self.next_local += 1;
        Reg::local(r)
    }

    /// find how many local registers have been allocated to this table
    pub fn size(&self) -> i8
    {
        self.next_local
    }
}

#[cfg(test)]
mod tests
{
    use crate::leema::reg::Reg;

    /// just a placeholder test
    #[test]
    fn test_reg_void()
    {
        let r = Reg::Void;
        assert_eq!(false, r.is_primary());
    }
}
