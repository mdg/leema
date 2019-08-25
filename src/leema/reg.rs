use crate::leema::failure::Lresult;
use crate::leema::lstr::Lstr;
use crate::leema::val::Val;

use std::cell::RefCell;
use std::cmp;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;


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
    fn ireg_get_mut(&mut self, r: &Ireg) -> &mut Val;
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
    // Stack(Ireg),
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
            &Reg::Local(ref r) => write!(f, "Reg{}", r),
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


#[derive(Clone)]
#[derive(Debug)]
pub struct Tree
{
    zero: Option<Box<Tree>>,
    one: Option<Box<Tree>>,
    reg: Option<i8>,
    max: i8,
}

impl Tree
{
    pub fn new() -> Tree
    {
        Tree {
            zero: None,
            one: None,
            reg: None,
            max: 0,
        }
    }

    fn with_reg(reg: i8) -> Tree
    {
        Tree {
            zero: None,
            one: None,
            reg: Some(reg),
            max: reg,
        }
    }

    pub fn push(&mut self) -> i8
    {
        self._push(1)
    }

    pub fn pop(&mut self, r: &Reg)
    {
        let ir = match r {
            &Reg::Local(Ireg::Reg(localreg)) => localreg,
            _ => {
                panic!("cannot pop a not local reg: {:?}", r);
            }
        };
        if ir == 1 {
            self.reg = None;
            self.max = self.get_max();
            return;
        }
        self._pop(ir >> 1);
    }

    fn _push(&mut self, r: i8) -> i8
    {
        if self.reg.is_none() {
            self.reg = Some(r);
            self.max = self.get_max();
            return r;
        }
        let new0x = (r << 1) & 0x7e;
        let new1x = new0x | 0x01;
        let new_reg = match (&mut self.zero, &mut self.one) {
            (&mut Some(ref mut old0), &mut Some(ref old1))
                if old0.max < old1.max =>
            {
                old0._push(new0x)
            }
            (&mut Some(_), &mut Some(ref mut old1)) => old1._push(new0x),
            (old0 @ &mut None, _) => {
                *old0 = Some(Box::new(Tree::with_reg(new0x)));
                new0x
            }
            (_, old1 @ &mut None) => {
                *old1 = Some(Box::new(Tree::with_reg(new1x)));
                new1x
            }
        };
        self.max = self.get_max();
        new_reg
    }

    fn _pop(&mut self, r: i8)
    {
        if r == 1 {
            self.reg = None;
            self.max = self.get_max();
            return;
        } else {
            let nextr = r >> 1;
            if r & 0x01 == 0x01 {
                self.zero.as_mut().unwrap()._pop(nextr);
            } else {
                self.one.as_mut().unwrap()._pop(nextr);
            }
        }
    }

    pub fn get_max(&mut self) -> i8
    {
        match (&self.zero, &self.one) {
            (&Some(ref old0), &Some(ref old1)) => cmp::max(old0.max, old1.max),
            (&None, &Some(ref old1)) => old1.max,
            (&Some(ref old0), &None) => old0.max,
            _ => self.max,
        }
    }
}

#[derive(Clone)]
#[derive(Debug)]
pub struct RegState
{
    in_use: bool,
    prev: Option<i8>,
    name: Option<Lstr>,
}

impl Default for RegState
{
    fn default() -> RegState
    {
        RegState {
            in_use: false,
            prev: None,
            name: None,
        }
    }
}

pub struct RegTab
{
    ids: HashMap<Lstr, Reg>,
    reg: Rc<RefCell<Tree>>,
    state: Rc<RefCell<Vec<RegState>>>,
    pub current: Reg,
}

impl RegTab
{
    pub fn new() -> RegTab
    {
        RegTab {
            ids: HashMap::new(),
            reg: Rc::new(RefCell::new(Tree::new())),
            state: Rc::new(RefCell::new(vec![RegState::default(); 8])),
            current: Reg::local(0),
        }
    }

    pub fn push_dst(&mut self) -> ScopedReg
    {
        let icurrent = self.reg.borrow_mut().push();
        self.current = Reg::local(icurrent);
        ScopedReg {
            r: self.current.clone(),
            tree: self.reg.clone(),
            free_on_drop: true,
        }
    }

    pub fn push_dst_reg(&mut self, r: Reg) -> ScopedReg
    {
        ScopedReg {
            r,
            tree: self.reg.clone(),
            free_on_drop: false,
        }
    }

    pub fn id(&mut self, name: &Lstr) -> Reg
    {
        {
            let first_get = self.ids.get(name);
            if first_get.is_some() {
                return first_get.unwrap().clone();
            }
        }
        let ireg = self.reg.borrow_mut().push();
        let reg = Reg::local(ireg);
        self.ids.insert(name.clone(), reg.clone());
        reg
    }
}


pub struct ScopedReg
{
    pub r: Reg,
    tree: Rc<RefCell<Tree>>,
    free_on_drop: bool,
}

impl ScopedReg
{
    pub fn pop(mut self)
    {
        self._pop();
    }

    fn _pop(&mut self)
    {
        if self.r == Reg::Void {
            return;
        }
        self.tree.borrow_mut().pop(&self.r);
        self.r = Reg::Void;
    }
}

impl Drop for ScopedReg
{
    fn drop(&mut self)
    {
        self._pop();
    }
}


pub struct StackedReg<'a>
{
    reg: Reg,
    stack: RegStack<'a>,
}


pub enum RegStack<'a>
{
    Node(Reg, &'a RegStack<'a>),
    Base(Reg, RegTab),
}

impl<'a> RegStack<'a>
{
    pub fn reg(&self) -> Reg
    {
        Reg::Void
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
