
use leema::lstr::Lstr;

use std::fmt;


#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
#[derive(Eq)]
#[derive(Hash)]
pub struct Lri
{
    modules: Option<Lstr>,
    localid: Lstr,
    params: Option<Vec<Lri>>,
}

impl Lri
{
    pub fn new(local: Lstr) -> Lri
    {
        Lri{
            modules: None,
            localid: local,
            params: None,
        }
    }
    pub fn with_modules(mods: Lstr, local: Lstr) -> Lri
    {
        Lri{
            modules: Some(mods),
            localid: local,
            params: None,
        }
    }
}

impl fmt::Display for Lri
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self {
            &Lri{modules: None, localid: ref lid, params: None} => {
                write!(f, "{}", lid)
            }
            &Lri{modules: _, localid: ref lid, params: _} => {
                write!(f, "{}", lid)
            }
        }
    }
}
