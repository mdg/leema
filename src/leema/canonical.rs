/// Canonical module and id type
///
/// /core/Str
/// /core/<Option T>
/// /core\LocalType
use crate::leema::failure::Lresult;
use crate::leema::lstr::Lstr;
use crate::leema::module::{ImportedMod, ModRelativity};

use std::borrow::Borrow;
use std::ffi::OsStr;
use std::fmt;
use std::path::{Path, PathBuf};


#[macro_export]
macro_rules! canonical {
    ($c:expr) => {
        crate::leema::canonical::Canonical(crate::leema::lstr::Lstr::Sref($c))
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
enum Lpath
{
    Root(&'static str),
    Path2([&'static str; 2]),
    Path3([&'static str; 3]),
    Path(Rc<Lpath>, &'static str),
}

static ROOT: Lpath = Lpath::Root;
static DEFPATH: Lpath = Lpath::Exported(&ROOT, "_");

#[derive(Clone)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
#[derive(Eq)]
#[derive(Ord)]
#[derive(Hash)]
pub struct Canonical(pub Lstr);

impl Canonical
{
    pub const DEFAULT: Canonical = canonical!("_");

    pub fn new(c: Lstr) -> Canonical
    {
        Canonical(c)
    }

    /// Check if this module is a core module
    pub fn is_core(&self) -> bool
    {
        // maybe memoize this as a struct var at some point
        self.0.starts_with("/core")
    }

    pub fn as_path(&self) -> &Path
    {
        Path::new(self.0.str())
    }

    /// add an identifier to an existing canonical
    /// fails if sub is Absolute
    pub fn join<S>(&self, sub: S) -> Lresult<Canonical>
    where
        S: AsRef<OsStr> + std::fmt::Debug,
    {
        let subpath = Path::new(sub.as_ref());
        match ImportedMod::path_relativity(subpath) {
            ModRelativity::Absolute => {
                return Err(rustfail!(
                    "leema_failure",
                    "cannot join an absolute path: {:?}",
                    sub,
                ));
            }
            ModRelativity::Sibling => {
                let sib_base = self.as_path().parent().unwrap();
                let sib_path = subpath.strip_prefix("../").unwrap();
                // recursing in here would be good to allow multiple ../
                let sib = sib_base.join(sib_path);
                Ok(Canonical(Lstr::from(format!("{}", sib.display()))))
            }
            ModRelativity::Child | ModRelativity::Local => {
                let ch_path = self.as_path().join(subpath);
                Ok(Canonical(Lstr::from(format!("{}", ch_path.display()))))
            }
        }
    }

    /// if imp is absolute, replace self with imp
    pub fn push<S: AsRef<OsStr>>(&self, imp: &S) -> Canonical
    {
        let import = Path::new(imp);
        match ImportedMod::path_relativity(import) {
            ModRelativity::Absolute => {
                Canonical(Lstr::from(format!("{}", import.display())))
            }
            ModRelativity::Sibling => {
                let sib_base = self.as_path().parent().unwrap();
                let sib_path = import.strip_prefix("../").unwrap();
                let sib = sib_base.join(sib_path);
                Canonical(Lstr::from(format!("{}", sib.display())))
            }
            ModRelativity::Child | ModRelativity::Local => {
                let ch_path = self.as_path().join(import);
                Canonical(Lstr::from(format!("{}", ch_path.display())))
            }
        }
    }

    /// split the final element off the end of Canonical
    pub fn split_id(&self) -> Lresult<(Canonical, Lstr)>
    {
        match self.0 {
            Lstr::Sref(s) => {
                let p = Path::new(s);
                match (p.parent(), p.file_name()) {
                    (Some(parent), Some(id)) => {
                        Ok((
                            Canonical(Lstr::Sref(parent.to_str().unwrap())),
                            Lstr::Sref(id.to_str().unwrap()),
                        ))
                    }
                    _ => {
                        return Err(rustfail!(
                            "failure",
                            "split cannot be split: {}",
                            self.0,
                        ));
                    }
                }
            }
            Lstr::Arc(ref s) => {
                let p = Path::new(&**s);
                let ext = p.extension().unwrap();
                let stem = p.file_stem().unwrap();
                let parent = Canonical(Lstr::from(
                    p.with_file_name(stem).to_str().unwrap().to_string(),
                ));
                Ok((parent, Lstr::from(ext.to_str().unwrap().to_string())))
            }
            ref other => {
                panic!("not a normal Lstr: {:?}", other);
            }
        }
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
        Canonical::new(Lstr::from(cstr))
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
        Canonical::new(Lstr::from(cstr))
    }
}

impl Borrow<str> for Canonical
{
    fn borrow(&self) -> &str
    {
        self.0.str()
    }
}

impl PartialEq<Canonical> for str
{
    fn eq(&self, other: &Canonical) -> bool
    {
        *self == other.0
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
        f.write_str(self.0.str())
    }
}

impl fmt::Debug for Canonical
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "{:?}", self.0.str())
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
        let a = Canonical::ancestors(Path::new("/foo/bar.baz"));
        let mut it = a.iter();
        assert_eq!("/foo", it.next().unwrap().as_os_str());
        assert_eq!("/foo/bar", it.next().unwrap().as_os_str());
        assert_eq!(2, a.len());
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
        let ct = Canonical(Lstr::from("/taco.Burrito".to_string()));
        let (m, t) = ct.split_id().unwrap();
        assert_eq!("/taco", m.0.str());
        assert_eq!("Burrito", t.str());
    }

    #[test]
    fn test_canonical_split_id_sref()
    {
        let ct = Canonical(Lstr::from("/taco.Burrito"));
        let (m, t) = ct.split_id().unwrap();
        assert_eq!("/taco", m.0.str());
        assert_eq!("Burrito", t.str());
    }
}
