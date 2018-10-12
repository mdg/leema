use leema::lstr::Lstr;
use leema::sendclone::SendClone;
use leema::val::{Type, Val};

use std::fmt;


#[derive(Clone)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
#[derive(Eq)]
#[derive(Ord)]
#[derive(Hash)]
pub struct Lri
{
    pub modules: Option<Lstr>,
    pub localid: Lstr,
    pub params: Option<Vec<Type>>,
}

impl Lri
{
    pub fn new(local: Lstr) -> Lri
    {
        Lri {
            modules: None,
            localid: local,
            params: None,
        }
    }

    pub fn full(mods: Option<Lstr>, id: Lstr, params: Option<Vec<Type>>)
        -> Lri
    {
        Lri {
            modules: mods,
            localid: id,
            params,
        }
    }

    pub fn with_modules(mods: Lstr, local: Lstr) -> Lri
    {
        Lri {
            modules: Some(mods),
            localid: local,
            params: None,
        }
    }

    pub fn add_modules(&self, mods: Lstr) -> Lri
    {
        Lri {
            modules: Some(mods),
            localid: self.localid.clone(),
            params: self.params.clone(),
        }
    }

    pub fn replace_params(&self, params: Vec<Type>) -> Lri
    {
        Lri {
            modules: self.modules.clone(),
            localid: self.localid.clone(),
            params: Some(params),
        }
    }

    /// specialize the parameters for this Lri w/ the given ones
    pub fn specialize_params(&self, other: &Vec<Type>) -> Result<Lri, Val>
    {
        if self.params.is_none() {
            return Err(Val::Str(Lstr::Sref(
                "cannot specialize Lri w/ no params"
            )));
        }
        let self_p = self.params.as_ref().unwrap();
        if self_p.len() != other.len() {
            return Err(Val::Str(Lstr::Sref(
                "cannot specialize wrong number of Lri params"
            )));
        }
        Ok(self.replace_params(other.clone()))
    }

    pub fn make_params_typevars(&mut self)
    {
        self.params.as_mut().map(|params| {
            for p in params {
                *p = Lri::make_param_typevar(p);
            }
        });
    }

    pub fn make_param_typevar(p: &Type) -> Type
    {
        match p {
            Type::UserDef(ref tri) if tri.modules.is_some() => {
                panic!("type parameters cannot have module prefix: {}", tri);
            }
            Type::UserDef(ref tri) if tri.params.is_some() => {
                panic!("type parameters cannot have type parameters: {}", tri);
            }
            Type::UserDef(ref tri) => Type::Var(tri.localid.clone()),
            Type::Var(ref v) => Type::Var(v.clone()),
            _ => {
                panic!("cannot make typevar from: {}", p);
            }
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

    pub fn matches_modules(&self, mods: &str) -> bool
    {
        match self.modules {
            None => false,
            Some(ref imod) => imod == mods,
        }
    }

    pub fn mod_ref(&self) -> Option<&Lstr>
    {
        self.modules.as_ref()
    }

    pub fn safe_mod(&self) -> Lstr
    {
        match &self.modules {
            &Some(ref mods) => mods.clone(),
            &None => Lstr::Sref(""),
        }
    }

    pub fn has_params(&self) -> bool
    {
        self.params.is_some()
    }

    pub fn nominal_eq(a: &Lri, b: &Lri) -> bool
    {
        a.modules == b.modules && a.localid == b.localid
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
        let new_mods = self.modules.as_ref().map(|m| m.clone_for_send());
        let new_id = self.localid.clone_for_send();
        let new_params = self
            .params
            .as_ref()
            .map(|params| params.iter().map(|p| p.deep_clone()).collect());
        Lri {
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
        Lstr::from(format!("{}", i))
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

impl fmt::Debug for Lri
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        fmt::Display::fmt(self, f)
    }
}


#[cfg(test)]
mod tests
{
    use leema::lri::Lri;
    use leema::lstr::Lstr;
    use leema::val::Type;

    #[test]
    fn test_lri_equality()
    {
        let sref = Lri::full(
            Some(Lstr::Sref("A")),
            Lstr::Sref("B"),
            Some(vec![Type::UserDef(Lri::new(Lstr::Sref("C")))]),
        );
        let smem = Lri::full(
            Some(Lstr::from("A".to_string())),
            Lstr::from("B".to_string()),
            Some(vec![Type::UserDef(Lri::new(Lstr::from("C".to_string())))]),
        );

        assert!(PartialEq::eq(&sref, &smem));
    }
}
