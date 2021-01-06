/// Canonical module and id type
///
/// /core/Str
/// /core/<Option T>
use crate::leema::failure::Lresult;
use crate::leema::lstr::Lstr;
use crate::leema::sendclone;

use std::ffi::OsStr;
use std::fmt;
use std::path::{Path, PathBuf};


#[macro_export]
macro_rules! canonical {
    ($c:expr) => {
        crate::leema::canonical::Canonical::new(crate::leema::lstr::Lstr::Sref(
            $c,
        ))
    };
}

// imported path
pub enum Impath
{
    Absolute,
    Sibling,
    Child,
    Local(&'static Impath, &'static str),
    Exported(&'static Impath, &'static str),
}

#[derive(Clone)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
#[derive(Eq)]
#[derive(Ord)]
#[derive(Hash)]
pub struct Canonical(Lstr);

impl Canonical
{
    pub const DEFAULT: Canonical = canonical!("_");

    pub const fn new(c: Lstr) -> Canonical
    {
        Canonical(c)
    }

    /// Check if this module is a core module
    pub fn is_core(&self) -> bool
    {
        // convert to a path which knows not to match /core to /corebad
        self.as_path().starts_with("/core")
    }

    pub fn as_path(&self) -> &Path
    {
        Path::new(self.0.as_str())
    }

    /// convert this canonical to an Lstr
    pub fn to_lstr(&self) -> Lstr
    {
        self.0.clone()
    }

    /// add an identifier to an existing canonical
    /// fails if sub is Absolute
    pub fn join<S>(&self, sub: S) -> Lresult<Canonical>
    where
        S: AsRef<OsStr> + std::fmt::Debug,
    {
        let subpath = Path::new(sub.as_ref());
        if subpath.is_absolute() {
            return Err(rustfail!(
                "leema_failure",
                "cannot join an absolute path: {:?}",
                sub,
            ));
        } else if subpath.starts_with("../") {
            let sib_base = self.as_path().parent().unwrap();
            let sib_path = subpath.strip_prefix("../").unwrap();
            // recursing in here would be good to allow multiple ../
            let sib = sib_base.join(sib_path);
            Ok(Canonical::new(Lstr::from(format!("{}", sib.display()))))
        } else {
            let ch_path = self.as_path().join(subpath);
            Ok(Canonical::new(Lstr::from(format!("{}", ch_path.display()))))
        }
    }

    /// split the final .extension element off the end of Canonical
    pub fn split_function(&self) -> Option<(Canonical, Lstr)>
    {
        match self.0 {
            Lstr::Sref(ss) => {
                let mut ssplit = ss.rsplitn(2, ".");
                let sfunc = ssplit.next().unwrap();
                let smodule = ssplit.next()?;
                Some((Canonical::new(Lstr::Sref(smodule)), Lstr::Sref(sfunc)))
            }
            Lstr::Arc(ref s) => {
                let mut split = s.rsplitn(2, ".");
                let func = split.next().unwrap();
                let module = split.next()?;
                Some((
                    Canonical::new(Lstr::from(module.to_string())),
                    Lstr::from(func.to_string()),
                ))
            }
            ref other => {
                panic!("not a normal Lstr: {:?}", other);
            }
        }
    }

    /// get the Lstr out of the Canonical
    pub fn as_lstr(&self) -> &Lstr
    {
        &self.0
    }

    /// get the str out of the Canonical
    pub fn as_str(&self) -> &str
    {
        self.0.as_str()
    }

    pub fn ancestors(path: &Path) -> Vec<&Path>
    {
        let av: Vec<&Path> = path.ancestors().collect();
        av.into_iter()
            .rev()
            .skip(1)
            .map(|p| {
                // extensions should be trimmed
                // is there a way to only do this for the final path
                // since it's the only one that will ever have an extension?
                let xlen = match p.extension() {
                    Some(x) => x.len() + 1,
                    None => {
                        return p;
                    }
                };
                let pstr = p.as_os_str().to_str().unwrap();
                let (base, _) = pstr.split_at(pstr.len() - xlen);
                Path::new(base)
            })
            .collect()
    }

    pub fn file_path_buf(cpath: &Path) -> Lresult<PathBuf>
    {
        cpath
            .strip_prefix("/")
            .map(|relative_path| relative_path.with_extension("lma"))
            .map_err(|path_err| {
                rustfail!(
                    "load_fail",
                    "unexpected lack of root prefix in {} - error {}",
                    cpath.display(),
                    path_err,
                )
            })
    }
}

impl From<&Path> for Canonical
{
    fn from(cp: &Path) -> Canonical
    {
        if !cp.is_absolute() {
            panic!("canonical must be absolute: {:?}", cp);
        }
        let cstr: String =
            cp.to_str().expect("expected unicode path").to_string();
        Canonical(Lstr::from(cstr))
    }
}

impl From<PathBuf> for Canonical
{
    fn from(cp: PathBuf) -> Canonical
    {
        if !cp.is_absolute() {
            panic!("canonical must be absolute: {:?}", cp);
        }
        let cstr: String =
            cp.to_str().expect("expected unicode path").to_string();
        Canonical(Lstr::from(cstr))
    }
}

impl PartialEq<Canonical> for str
{
    fn eq(&self, other: &Canonical) -> bool
    {
        self == other.0.as_str()
    }
}

impl Default for Canonical
{
    fn default() -> Canonical
    {
        Canonical::DEFAULT
    }
}

impl fmt::Display for Canonical
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "{}", self.0)
    }
}

impl fmt::Debug for Canonical
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "{}", self.0)
    }
}

impl sendclone::SendClone for Canonical
{
    type Item = Canonical;

    fn clone_for_send(&self) -> Canonical
    {
        Canonical(self.0.clone_for_send())
    }
}


#[cfg(test)]
mod tests
{
    use super::Canonical;
    use crate::leema::lstr::Lstr;

    use std::path::Path;

    #[test]
    fn test_canonical_ancestors()
    {
        let a = Canonical::ancestors(Path::new("/foo/bar/baz"));
        let mut it = a.iter();
        assert_eq!("/foo", it.next().unwrap().as_os_str());
        assert_eq!("/foo/bar", it.next().unwrap().as_os_str());
        assert_eq!("/foo/bar/baz", it.next().unwrap().as_os_str());
        assert_eq!(3, a.len());
    }

    #[test]
    fn test_canonical_push_sibling()
    {
        let cm = Canonical(Lstr::from("/foo/bar"));
        let result = cm.join(&Path::new("../taco")).unwrap();
        assert_eq!("/foo/taco", result.0.str());
    }

    #[test]
    fn test_canonical_split_id_arc()
    {
        let ct = Canonical(Lstr::from("/taco/Burrito".to_string()));
        let (m, t) = ct.split_id().unwrap();
        assert_eq!("/taco", m.0.str());
        assert_eq!("Burrito", t.str());
    }

    #[test]
    fn test_canonical_split_id_sref()
    {
        let ct = Canonical(Lstr::from("/taco/Burrito"));
        let (m, t) = ct.split_id().unwrap();
        assert_eq!("/taco", m.0.str());
        assert_eq!("Burrito", t.str());
    }
}
