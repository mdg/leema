use crate::leema::failure::Lresult;
use crate::leema::lstr::Lstr;

use std::borrow::Borrow;
use std::fmt;
use std::path::{Path, PathBuf};


#[macro_export]
macro_rules! canonical {
    ($cm:expr) => {
        crate::leema::canonical::Canonical(crate::leema::lstr::Lstr::Sref($cm))
    };
}

#[derive(Clone)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
#[derive(Eq)]
#[derive(Ord)]
#[derive(Hash)]
pub struct Canonical(Lstr);

const DEFAULT_CANONICAL: Canonical = canonical!("_");

impl Canonical
{
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

    // add an identifier to an existing canonical
    // if the canonical is a module, add the id as an extension
    // if the canonical is an ID already, upgrade the ID to a module
    // and add the new sub id as an extension
    pub fn sub_id(&self, sub: &str) -> Canonical
    {
        let p = Path::new(self.0.str());
        let new_p = match p.extension() {
            Some(ext) => {
                let stem = p.file_stem().unwrap().to_str().unwrap();
                let mut new_path = p.parent().unwrap().join(stem);
                new_path.push(ext);
                new_path.set_extension(sub);
                new_path
            }
            None => p.with_extension(sub),
        };
        Canonical(Lstr::from(new_p.to_str().unwrap().to_string()))
    }

    pub fn split_id(&self) -> Lresult<(Canonical, Lstr)>
    {
        match self.0 {
            Lstr::Sref(s) => {
                let p = Path::new(s);
                let ext = p.extension().unwrap();
                let stem = p.file_stem().unwrap();
                let parent = Canonical(Lstr::from(
                    p.with_file_name(stem).to_str().unwrap().to_string(),
                ));
                Ok((parent, Lstr::Sref(ext.to_str().unwrap())))
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

impl From<&'static Path> for Canonical
{
    fn from(cp: &'static Path) -> Canonical
    {
        if !cp.is_absolute() {
            panic!("canonical must be absolute: {:?}", cp);
        }
        Canonical::new(Lstr::Sref(cp.to_str().expect("expected unicode path")))
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
        DEFAULT_CANONICAL
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
