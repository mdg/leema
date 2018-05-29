
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

    pub fn full(mods: Option<Lstr>, id: Lstr, params: Option<Vec<Lri>>) -> Lri
    {
        Lri{
            modules: mods,
            localid: id,
            params: params,
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

    pub fn local(&self) -> &Lstr
    {
        &self.localid
    }

    pub fn deep_clone(&self) -> Lri
    {
        let new_mods = self.modules.as_ref().map(|m| {
            m.deep_clone()
        });
        let new_id = self.localid.deep_clone();
        let new_params = self.params.as_ref().map(|params| {
            params.iter().map(|p| {
                p.deep_clone()
            }).collect()
        });
        Lri{
            modules: new_mods,
            localid: new_id,
            params: new_params,
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
            &Lri{modules: Some(ref mods), localid: ref lid, params: None} => {
                write!(f, "{}::{}", mods, lid)
            }
            &Lri{modules: _, localid: ref lid, params: Some(ref typs)} => {
                panic!("cannot display Lri with types: {:?}", self);
            }
        }
    }
}
