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
        crate::leema::canonical::Canonical::Path(
            crate::leema::lstr::Lstr::Sref($c),
        )
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
pub enum Canonical
{
    Path(Lstr),
    Open(Lstr, Lstr),
    Closed(Lstr, Box<Canonical>),
}

impl Canonical
{
    pub const DEFAULT: Canonical = canonical!("_");

    pub const fn new(c: Lstr) -> Canonical
    {
        Canonical::Path(c)
    }

    /// Check if this module is a core module
    pub fn is_core(&self) -> bool
    {
        // maybe memoize this as a struct var at some point
        self.as_path().starts_with("/core")
    }

    pub fn as_path(&self) -> &Path
    {
        match self {
            Canonical::Path(p) => Path::new(p.as_str()),
            Canonical::Open(p, _) => Path::new(p.as_str()),
            Canonical::Closed(p, _) => Path::new(p.as_str()),
        }
    }

    /// convert this canonical to an Lstr
    pub fn to_lstr(&self) -> Lstr
    {
        if let Canonical::Path(p) = self {
            p.clone()
        } else {
            lstrf!("{}", self)
        }
    }

    /// add an identifier to an existing canonical
    /// fails if sub is Absolute
    pub fn join<S>(&self, sub: S) -> Lresult<Canonical>
    where
        S: AsRef<OsStr> + std::fmt::Debug,
    {
        match self {
            Canonical::Path(p) => Ok(Canonical::Path(Self::join_path(p, sub)?)),
            Canonical::Open(_p, _v) => {
                panic!("no open");
            }
            Canonical::Closed(_p, _t) => {
                panic!("no closed");
            }
        }
    }

    /// add an identifier to an existing canonical
    /// fails if sub is Absolute
    fn join_path<S>(p: &Lstr, sub: S) -> Lresult<Lstr>
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
            let sib_base = Path::new(p.as_str()).parent().unwrap();
            let sib_path = subpath.strip_prefix("../").unwrap();
            // recursing in here would be good to allow multiple ../
            let sib = sib_base.join(sib_path);
            Ok(Lstr::from(format!("{}", sib.display())))
        } else {
            let ch_path = Path::new(p.as_str()).join(subpath);
            Ok(Lstr::from(format!("{}", ch_path.display())))
        }
    }

    /// split the final element off the end of Canonical
    pub fn split_id(&self) -> Lresult<(Canonical, Lstr)>
    {
        match self {
            Canonical::Path(p) => {
                let (pp, id) = Self::split_id_str(p)?;
                Ok((Canonical::Path(pp), id))
            }
            Canonical::Open(p, _) => {
                let (pp, id) = Self::split_id_str(p)?;
                Ok((Canonical::Path(pp), id))
            }
            Canonical::Closed(p, _) => {
                let (pp, id) = Self::split_id_str(p)?;
                Ok((Canonical::Path(pp), id))
            }
        }
    }

    /// split the final element off the end of Canonical
    fn split_id_str(path_str: &Lstr) -> Lresult<(Lstr, Lstr)>
    {
        match path_str {
            Lstr::Sref(s) => {
                let ss: &'static str = s;
                let p: &'static Path = Path::new(ss);
                match (p.parent(), p.file_name()) {
                    (Some(parent), Some(id)) => {
                        Ok((
                            Lstr::Sref(parent.to_str().unwrap()),
                            Lstr::Sref(id.to_str().unwrap()),
                        ))
                    }
                    _ => {
                        return Err(rustfail!(
                            "failure",
                            "split cannot be split: {}",
                            s,
                        ));
                    }
                }
            }
            Lstr::Arc(ref s) => {
                let p = Path::new(&**s);
                let ext = p.extension().unwrap();
                let stem = p.file_stem().unwrap();
                let parent = Lstr::from(
                    p.with_file_name(stem).to_str().unwrap().to_string(),
                );
                Ok((parent, Lstr::from(ext.to_str().unwrap().to_string())))
            }
            ref other => {
                panic!("not a normal Lstr: {:?}", other);
            }
        }
    }

    /// get the string portion of the Canonical
    fn path_str(&self) -> &Lstr
    {
        match self {
            Canonical::Path(p) => p,
            Canonical::Open(p, _) => p,
            Canonical::Closed(p, _) => p,
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

impl PartialEq<Canonical> for str
{
    fn eq(&self, other: &Canonical) -> bool
    {
        match (self, other) {
            (p0, Canonical::Path(p1)) => p0 == p1,
            _ => false,
        }
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
        match self {
            Canonical::Path(p) => write!(f, "{}", p),
            Canonical::Open(p, s) => {
                write!(f, "<{} {}>", p, s)
            }
            Canonical::Closed(p, s) => {
                write!(f, "<")?;
                write!(f, "{} {}", p, s)?;
                write!(f, ">")
            }
        }
    }
}

impl sendclone::SendClone for Canonical
{
    type Item = Canonical;

    fn clone_for_send(&self) -> Canonical
    {
        match self {
            Canonical::Path(p) => Canonical::Path(p.clone_for_send()),
            Canonical::Open(p, s) => {
                Canonical::Open(p.clone_for_send(), s.clone_for_send())
            }
            Canonical::Closed(p, s) => {
                Canonical::Closed(
                    p.clone_for_send(),
                    Box::new(s.clone_for_send()),
                )
            }
        }
    }
}

impl fmt::Debug for Canonical
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "{}", self)
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
