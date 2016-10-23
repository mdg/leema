use leema::inter::{Version, Intermod};
use std::collections::{HashMap};


pub struct Lib
{
    intermod: HashMap<String, Intermod>,
    version: Version,
}

impl Lib
{
    pub fn new(v: Version) -> Lib
    {
        Lib{
            intermod: HashMap::new(),
            version: v,
        }
    }
}

pub struct Set
{
    pub sin: Lib,
    pub cos: Lib,
}

impl Set
{
    pub fn new() -> Set
    {
        Set{
            sin: Lib::new(Version::Sin),
            cos: Lib::new(Version::Cos),
        }
    }

    pub fn lib(&mut self, version: Version) -> &mut Lib
    {
        match version {
            Version::Sin => &mut self.sin,
            Version::Cos => &mut self.cos,
        }
    }
}
