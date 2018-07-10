
use leema::lstr::Lstr;
use leema::val::{Type};

use std::fmt;
use std::rc::{Rc};


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
    params: Option<Vec<Type>>,
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

    pub fn full(mods: Option<Lstr>, id: Lstr, params: Option<Vec<Type>>) -> Lri
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

    pub fn add_modules(&self, mods: Lstr) -> Lri
    {
        Lri{
            modules: Some(mods),
            localid: self.localid.clone(),
            params: self.params.clone(),
        }
    }

    pub fn local_only(&self) -> bool
    {
        self.modules.is_none() && self.params.is_none()
    }

    pub fn has_modules(&self) -> bool
    {
        self.modules.is_some()
    }

    pub fn mod_ref(&self) -> Option<&Lstr>
    {
        self.modules.as_ref()
    }

    /**
     * deprecated
     */
    pub fn local(&self) -> &Lstr
    {
        &self.localid
    }

    pub fn local_ref(&self) -> &Lstr
    {
        &self.localid
    }

    pub fn param_ref(&self) -> Option<&Vec<Type>>
    {
        self.params.as_ref()
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

impl<'a> From<&'a Lri> for Lstr
{
    fn from(i: &'a Lri) -> Lstr
    {
        if i.local_only() {
            return i.local_ref().clone();
        }
        let str = format!("{}", i);
        Lstr::Rc(Rc::new(str))
    }
}

impl fmt::Display for Lri
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        if self.modules.is_some() {
            write!(f, "{}::", self.modules.as_ref().unwrap())?;
        }
        if let Some(ref params) = self.params.as_ref() {
            write!(f, "{}[", self.localid)?;
            for p in params.iter() {
                write!(f, "{},", p)?;
            }
            write!(f, "]")
        } else {
            write!(f, "{}", self.localid)
        }
    }
}
