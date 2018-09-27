use leema::log;
use leema::lstr::Lstr;
use leema::val::Val;

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
    fn ireg_get(&self, r: &Ireg) -> &Val;
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
    Lib,
    Void,
    Undecided,
}

impl Reg
{
    pub fn sub(&self, sub: i8) -> Reg
    {
        match self {
            &Reg::Param(ref ir) => Reg::Param(ir.sub(sub)),
            &Reg::Local(ref r) => Reg::Local(r.sub(sub)),
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


pub struct ScopedReg<'a, 'b>
    where 'a: 'b
{
    pub r: Reg,
    pub t: RegStack<'a, 'b>,
}

pub enum RegStack<'a, 'b>
    where 'a: 'b
{
    Top(&'a mut RegTable, Option<&'b mut RegStack<'a, 'a>>),
    Node(Option<&'b mut RegStack<'a, 'a>>),
}

impl<'a, 'b> RegStack<'a, 'b>
{
    pub fn push_dst(&'a mut self) -> ScopedReg<'a, 'b>
    {
        match self {
            &mut RegStack::Top(ref mut rt, ref mut next) => {
                let dst = rt.push_dst().clone();
                let new_next = next.take();
                *self = RegStack::Node(new_next);
                let new_top = RegStack::Top(rt, Some(self));
                ScopedReg{r: dst, t: new_top}
            }
            &mut RegStack::Node(_) => {
                panic!("cannot push from a lower node in reg stack");
            }
        }
    }
}

pub struct ScopedReg1<'a>
{
    pub rt: &'a mut RegTable,
    pushes: Vec<Option<Reg>>,
}

impl<'a> ScopedReg1<'a>
{
    pub fn init(rt: &'a mut RegTable) -> ScopedReg1<'a>
    {
        ScopedReg1{
            rt,
            pushes: vec![],
        }
    }

    pub fn dst(&self) -> &Reg
    {
        self.rt.dst()
    }

    pub fn push_dst(&mut self) -> &Reg
    {
        let dst = self.rt.next();
        self.rt.dstack.push(dst);
        self.pushes.push(None);
        self.rt.dst()
    }

    pub fn pop_dst(&mut self)
    {
        let popped = self.rt.dstack.pop().unwrap();
        if let Reg::Local(Ireg::Reg(i)) = popped {
eprintln!("recycle reg: {}", i);
            self.rt.free.push(i);
        }
    }
}

impl<'a> Drop for ScopedReg1<'a>
{
    fn drop(&mut self)
    {
        for opt_r in self.pushes.drain(..).rev() {
            match opt_r {
                Some(_) => {
                    self.rt.pop_dst_reg();
                }
                None => {
                    self.rt.pop_dst();
                }
            }
        }
    }
}

pub struct RegTable
{
    dstack: Vec<Reg>,
    labels: HashMap<Lstr, Reg>,
    free: Vec<i8>,
    _lastreg: i8,
}

impl RegTable
{
    pub fn new() -> RegTable
    {
        RegTable {
            dstack: vec![Reg::local(0)],
            labels: HashMap::new(),
            free: Vec::new(),
            _lastreg: 0,
        }
    }

    pub fn dst(&self) -> &Reg
    {
        self.dstack.last().unwrap()
    }

    pub fn def_args(&mut self, args: &Vec<Lstr>, closed: &Vec<Lstr>)
    {
        let mut r: i8 = 0;
        for a in args.iter().chain(closed.iter()) {
            self.labels.insert(a.clone(), Reg::param(r));
            r += 1;
        }
    }

    pub fn push_dst(&mut self) -> &Reg
    {
        let dst = self.next();
        self.dstack.push(dst.clone());
        self.dst()
    }

    pub fn pop_dst(&mut self)
    {
        let popped = self.dstack.pop().unwrap();
        if let Reg::Local(Ireg::Reg(i)) = popped {
            self.free.push(i);
        }
    }

    pub fn push_dst_reg(&mut self, dst: Reg) -> &Reg
    {
        self.dstack.push(dst);
        self.dst()
    }

    pub fn pop_dst_reg(&mut self)
    {
        self.dstack.pop();
    }

    pub fn push_sub(&mut self) -> &Reg
    {
        let dst = self.dst().sub(0);
        self.dstack.push(dst.clone());
        self.dst()
    }

    pub fn next_sub(&mut self) -> Reg
    {
        let dst = self.dst().next_sibling();
        let last = self.dstack.last_mut().unwrap();
        *last = dst.clone();
        dst
    }

    pub fn id(&mut self, name: &Lstr) -> Reg
    {
        if !self.labels.contains_key(name) {
            let dst = self.next();
            vout!("assign {} to {}\n", dst, name);
            self.labels.insert(name.clone(), dst);
        }
        self.labels.get(name).unwrap().clone()
    }

    fn next(&mut self) -> Reg
    {
        match self.free.pop() {
            None => {
                self._lastreg += 1;
                Reg::local(self._lastreg)
            }
            Some(r) => Reg::local(r),
        }
    }
}


#[cfg(test)]
mod tests
{
    use leema::reg::{Reg, RegTable};

    #[test]
    fn test_init()
    {
        let rt = RegTable::new();
        assert_eq!(Reg::local(0), *rt.dst());
    }

    #[test]
    fn test_next()
    {
        let mut rt = RegTable::new();

        let r1 = rt.next();
        let r2 = rt.next();

        assert_eq!(Reg::local(1), r1);
        assert_eq!(Reg::local(2), r2);
    }

    #[test]
    fn test_push_dst()
    {
        let mut rt = RegTable::new();

        assert_eq!(Reg::local(1), *rt.push_dst());
        assert_eq!(Reg::local(2), *rt.push_dst());
    }

    #[test]
    fn test_push_then_dst()
    {
        let mut rt = RegTable::new();

        rt.push_dst();
        assert_eq!(Reg::local(1), *rt.dst());

        rt.push_dst();
        assert_eq!(Reg::local(2), *rt.dst());
    }

    #[test]
    fn test_pop_back()
    {
        let mut rt = RegTable::new();

        rt.push_dst();
        rt.push_dst();

        assert_eq!(Reg::local(2), *rt.dst());

        rt.pop_dst();
        assert_eq!(Reg::local(1), *rt.dst());

        rt.pop_dst();
        assert_eq!(Reg::local(0), *rt.dst());
    }

    #[test]
    fn test_pop_push_free()
    {
        let mut rt = RegTable::new();

        rt.push_dst();
        assert_eq!(Reg::local(1), *rt.dst());

        rt.push_dst();
        assert_eq!(Reg::local(2), *rt.dst());

        rt.pop_dst();
        rt.pop_dst();

        assert_eq!(Reg::local(1), *rt.push_dst());
        assert_eq!(Reg::local(2), *rt.push_dst());
        assert_eq!(Reg::local(3), *rt.push_dst());
        assert_eq!(Reg::local(3), *rt.dst());
    }

}
