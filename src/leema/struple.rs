
use leema::lstr::{Lstr};
use leema::reg::{self, Ireg};
use leema::val::{Val};

#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
pub struct Struple(pub Vec<(Option<Lstr>, Val)>);

impl Struple
{
    pub fn new_indexed(items: Vec<Val>) -> Struple
    {
        let new_items: Vec<(Option<Lstr>, Val)> = items.into_iter().map(|i| {
            (None, i)
        }).collect();
        Struple(new_items)
    }

    pub fn new_tuple2(a: Val, b: Val) -> Struple
    {
        Struple(vec![
            (None, a),
            (None, b),
        ])
    }
}

impl reg::Iregistry for Struple
{
    fn ireg_get(&self, i: &Ireg) -> &Val
    {
        match i {
            // get reg on struple
            &Ireg::Reg(p) => {
                if p as usize >= self.0.len() {
                    panic!("{:?} too big for {:?}", i, self.0);
                }
                &self.0[p as usize].1
            }
            &Ireg::Sub(p, ref s) => {
                if p as usize >= self.0.len() {
                    panic!("{:?} too big for {:?}", i, self.0);
                }
                self.0[p as usize].1.ireg_get(&*s)
            }
        }
    }

    fn ireg_get_mut(&mut self, i: &Ireg) -> &mut Val
    {
        match i {
            // set reg on struple
            &Ireg::Reg(p) => {
                if p as usize >= tup.len() {
                    panic!("{:?} too big for {:?}", i, tup);
                }
                &mut self.0[p as usize].1
            }
            &Ireg::Sub(p, ref s) => {
                if p as usize >= tup.len() {
                    panic!("{:?} too big for {:?}", i, tup);
                }
                self.0[p as usize].1.ireg_get_mut(&*s)
            }
        }
    }

    fn ireg_set(&mut self, i: &Ireg, v: Val)
    {
        match i {
            // get reg on struple
            &Ireg::Reg(p) => {
                if p as usize >= fields.len() {
                    panic!("{:?} too big for struple {:?}"
                        , i, self);
                }
                self.0[p as usize].1 = v;
            }
            &Ireg::Sub(p, ref s) => {
                if p as usize >= fld.0.len() {
                    panic!("{:?} too big for strtuple {:?}", i, self);
                }
                self.0[p as usize].1.ireg_set(&*s, v);
            }
        }
    }
}
