use leema::val::{Val};

#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Eq)]
#[derive(PartialOrd)]
#[derive(Ord)]
#[derive(Copy)]
#[derive(Clone)]
pub enum Reg {
    Param(i8, Option<i8>),
    Reg(i8, Option<i8>),
    P1(i8),
    P2(i8, i8),
    R1(i8),
    R2(i8, i8),
    //Rn(i8, String),
    //N1(String),
    //N2(String, String),
    //Nr(String, i8),
    Result,
    Result2(i8),
    Lib,
    Void,
    Undecided,
}

impl Reg {
    pub fn to_secondary(&self, i2: i8) -> Reg
    {
        match self {
            &Reg::P1(i1) => Reg::P2(i1, i2),
            &Reg::R1(i1) => Reg::R2(i1, i2), 
            &Reg::Result => Reg::Result2(i2), 
            &Reg::Void => Reg::Void, 
            _ => {
                panic!("Can't make a 2 version for {:?}", self);
            }
        }
    }

    pub fn is_primary(&self) -> bool
    {
        match self {
            &Reg::P1(_) => true,
            &Reg::R1(_) => true,
            &Reg::Result => true,
            _ => false,
        }
    }

    pub fn is_secondary(&self) -> bool
    {
        match self {
            &Reg::P2(_,_) => true,
            &Reg::R2(_,_) => true,
            &Reg::Result2(_) => true,
            _ => false,
        }
    }
}

pub trait NumericRegistry {
    fn getRegR1(&self, r: i8) -> &Val;
    fn setRegR1(&mut self, r: i8, v: Val);
}
