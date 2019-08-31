use crate::leema::failure::Lresult;
use crate::leema::val::Val;

use std::collections::HashMap;
use std::fmt;


#[derive(PartialEq)]
#[derive(Eq)]
#[derive(PartialOrd)]
#[derive(Ord)]
#[derive(Clone)]
pub enum Ireg
{
    Reg(i8),
    Sub(i8, Box<Ireg>),
}

impl Ireg
{
    pub fn sub(&self, newsub: i8) -> Ireg
    {
        match self {
            &Ireg::Reg(r) => Ireg::Sub(r, Box::new(Ireg::Reg(newsub))),
            &Ireg::Sub(r, ref s) => Ireg::Sub(r, Box::new(s.sub(newsub))),
        }
    }

    pub fn is_primary(&self) -> bool
    {
        match self {
            &Ireg::Reg(_) => true,
            &Ireg::Sub(_, _) => false,
        }
    }

    pub fn get_primary(&self) -> i8
    {
        match self {
            &Ireg::Reg(r) => r,
            &Ireg::Sub(r, _) => r,
        }
    }

    pub fn next_sibling(&self) -> Ireg
    {
        match self {
            &Ireg::Sub(r, ref s) => Ireg::Sub(r, Box::new(s.next_sibling())),
            &Ireg::Reg(r) => Ireg::Reg(r + 1),
        }
    }

    pub fn get_sub(&self) -> &Ireg
    {
        match self {
            &Ireg::Sub(_, ref s) => &*s,
            &Ireg::Reg(_) => panic!("Cannot get sub from Ireg::Reg"),
        }
    }
}

impl fmt::Display for Ireg
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self {
            &Ireg::Reg(r) => write!(f, ".{}", r),
            &Ireg::Sub(r, ref s) => write!(f, ".{}{}", r, s),
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
    fn ireg_get(&self, r: &Ireg) -> Lresult<&Val>;
    fn ireg_set(&mut self, r: &Ireg, v: Val);
}

#[derive(PartialEq)]
#[derive(Eq)]
#[derive(PartialOrd)]
#[derive(Ord)]
#[derive(Clone)]
pub enum Reg
{
    Param(Ireg),
    Local(Ireg),
    Stack(Ireg),
    Lib,
    Void,
    Undecided,
}

impl Reg
{
    pub fn sub(&self, sub: i8) -> Reg
    {
        match self {
            &Reg::Param(ref r)|&Reg::Local(ref r) => Reg::Param(r.sub(sub)),
            // &Reg::Local(ref r) => Reg::Local(r.sub(sub)),
            &Reg::Void => Reg::Void,
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

    pub fn stack(p: i8) -> Reg
    {
        Reg::Stack(Ireg::Reg(p))
    }

    pub fn is_primary(&self) -> bool
    {
        match self {
            &Reg::Param(Ireg::Reg(_)) => true,
            &Reg::Local(Ireg::Reg(_)) => true,
            _ => false,
        }
    }

    pub fn is_sub(&self) -> bool
    {
        match self {
            &Reg::Param(Ireg::Sub(_, _)) => true,
            &Reg::Local(Ireg::Sub(_, _)) => true,
            _ => false,
        }
    }

    pub fn next_sibling(&self) -> Reg
    {
        match self {
            &Reg::Param(ref sub) => Reg::Param(sub.next_sibling()),
            &Reg::Local(ref sub) => Reg::Local(sub.next_sibling()),
            _ => {
                panic!("register has no sibling: {:?}", self);
            }
        }
    }

    pub fn get_sub(&self) -> &Ireg
    {
        match self {
            &Reg::Param(ref s) => s,
            &Reg::Local(Ireg::Sub(_, ref s)) => s,
            _ => panic!("cannot get sub from other register: {:?}", self),
        }
    }
}

impl fmt::Display for Reg
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self {
            &Reg::Param(ref r) => write!(f, "Param{}", r),
            &Reg::Local(ref r) => write!(f, "Local{}", r),
            &Reg::Stack(ref r) => write!(f, "Stack{}", r),
            &Reg::Lib => write!(f, "Reg::Lib"),
            &Reg::Void => write!(f, "Reg::Void"),
            &Reg::Undecided => write!(f, "Reg::Undecided"),
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
pub struct RegTab
{
    ids: HashMap<&'static str, i8>,
    next_local: i8,
}

impl RegTab
{
    pub fn new() -> RegTab
    {
        RegTab {
            ids: HashMap::new(),
            next_local: 0,
        }
    }

    pub fn named(&mut self, id: &'static str) -> Reg
    {
        if let Some(r) = self.ids.get(id) {
            return Reg::local(*r);
        }
        let r = self.next_local;
        self.next_local += 1;
        self.ids.insert(id, r);
        Reg::local(r)
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


/// RegStack pulls temporary registers from the table
/// and puts them back when they're no longer used
///
/// root = inner_table
/// (b, bnode) = root.push()
/// (c, cnode) = bnode.push()
/// use c.reg
/// d = c.push()
/// d.drop(|| {
///     d.restore(c)
/// })
/// c.drop(|| {
///     c.restore(b)
/// })
///
/// pusher -> tab
/// pushed(r) -> popper -> (pusher|tab)
///           -> prev pushed
pub struct RegStack<'t>
{
    pub dst: Reg,
    pub local: &'t mut RegTab,
    stack: i8,
}

impl<'t> RegStack<'t>
{
    pub fn new(local: &'t mut RegTab) -> RegStack<'t>
    {
        RegStack{
            dst: Reg::stack(0),
            local,
            stack: 0,
        }
    }

    pub fn push(self) -> (Reg, RegStack<'t>)
    {
        let next_stack = self.stack + 1;
        let r = Reg::stack(next_stack);
        let new_stack = RegStack{
            dst: r.clone(),
            local: self.local,
            stack: next_stack,
        };
        (r, new_stack)
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
