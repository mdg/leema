use leema::val::{Val};

#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Eq)]
#[derive(PartialOrd)]
#[derive(Ord)]
#[derive(Clone)]
pub enum Ireg {
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

    pub fn get_sub(&self) -> &Ireg
    {
        match self {
            &Ireg::Sub(_, ref s) => &*s,
            &Ireg::Reg(_) => panic!("Cannot get sub from Ireg::Reg"),
        }
    }
}

pub trait Iregistry {
    fn ireg_get(&self, r: &Ireg) -> &Val;
    fn ireg_get_mut(&mut self, r: &Ireg) -> &mut Val;
    fn ireg_set(&mut self, r: &Ireg, v: Val);
}

#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Eq)]
#[derive(PartialOrd)]
#[derive(Ord)]
#[derive(Clone)]
pub enum Reg {
    Params,
    Param(Ireg),
    Result,
    Subresult(Ireg),
    Reg(Ireg),
    Lib,
    Void,
    Undecided,
}

impl Reg
{
    pub fn sub(&self, sub: i8) -> Reg
    {
        match self {
            &Reg::Params => {
                Reg::Param(Ireg::Reg(sub))
            }
            &Reg::Param(ref ir) => {
                Reg::Param(ir.sub(sub))
            }
            &Reg::Result => {
                Reg::Subresult(Ireg::Reg(sub))
            }
            &Reg::Subresult(ref ir) => {
                Reg::Subresult(ir.sub(sub))
            }
            &Reg::Reg(ref r) => {
                Reg::Reg(r.sub(sub))
            }
            &Reg::Void => Reg::Void, 
            _ => {
                panic!("Can't make a subreg for {:?}", self);
            }
        }
    }

    pub fn new_reg(p: i8) -> Reg
    {
        Reg::Reg(Ireg::Reg(p))
    }

    pub fn is_primary(&self) -> bool
    {
        match self {
            &Reg::Params => true,
            &Reg::Result => true,
            &Reg::Reg(Ireg::Reg(_)) => true,
            _ => false,
        }
    }

    pub fn is_sub(&self) -> bool
    {
        match self {
            &Reg::Param(_) => true,
            &Reg::Subresult(_) => true,
            &Reg::Reg(Ireg::Sub(_, _)) => true,
            _ => false,
        }
    }

    pub fn get_sub(&self) -> &Ireg
    {
        match self {
            &Reg::Param(ref s) => s,
            &Reg::Subresult(ref s) => s,
            &Reg::Reg(Ireg::Sub(_, ref s)) => s,
            _ => panic!("cannot get sub from other register: {:?}", self),
        }
    }
}
