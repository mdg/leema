use crate::leema::canonical::Canonical;
use crate::leema::failure::Lresult;
use crate::leema::lstr::Lstr;
use crate::leema::sendclone;

use std::borrow::Borrow;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::path::{Component, Path, PathBuf};

#[derive(Clone)]
#[derive(Copy)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Eq)]
#[derive(Hash)]
#[derive(PartialOrd)]
#[derive(Ord)]
pub enum ModTyp
{
    File,
    Data,
    TraitData,
    Trait,
    Impl,
    Alias,
}

impl ModTyp
{
    pub fn is_data(&self) -> bool
    {
        matches!(self, ModTyp::Data | ModTyp::TraitData)
    }
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq)]
#[derive(Eq)]
#[derive(Hash)]
#[derive(PartialOrd)]
#[derive(Ord)]
pub struct ModKey
{
    pub name: Canonical,
    pub file: Option<Box<Path>>,
    pub mtyp: ModTyp,
}

impl ModKey
{
    pub fn new(name: Canonical, path: PathBuf) -> ModKey
    {
        ModKey {
            name,
            file: Some(From::from(path)),
            mtyp: ModTyp::File,
        }
    }

    /// If the path exists, get it. Otherwise return the module name
    pub fn best_path(&self) -> Lstr
    {
        self.file
            .as_ref()
            .map(|f| {
                // convert the path to a string and then Lstr
                Lstr::from(format!("{}", f.display()))
            })
            .unwrap_or_else(|| self.name.to_lstr())
    }

    pub fn submod(&self, mt: ModTyp, name: &'static str) -> Lresult<ModKey>
    {
        Ok(ModKey {
            name: self.name.join(name)?,
            file: self.file.clone(),
            mtyp: mt,
        })
    }

    /// create a new ModKey with the same path and n arbitrary module name
    /// the ModTyp for this key will always be ModTyp::Impl
    /// kind of weird that it can't encode both the trait type and
    /// the data type, but the ProtoModule can so hopefully that's good enough
    pub fn subimpl(&self, name: Canonical) -> Lresult<ModKey>
    {
        Ok(ModKey {
            name,
            file: self.file.clone(),
            mtyp: ModTyp::Impl,
        })
    }
}

impl From<Canonical> for ModKey
{
    fn from(name: Canonical) -> ModKey
    {
        ModKey {
            name,
            file: None,
            mtyp: ModTyp::File,
        }
    }
}

impl From<&'static str> for ModKey
{
    fn from(mod_str: &'static str) -> ModKey
    {
        ModKey {
            name: Canonical::new(Lstr::Sref(mod_str)),
            file: None,
            mtyp: ModTyp::File,
        }
    }
}

impl Default for ModKey
{
    fn default() -> ModKey
    {
        ModKey {
            name: Canonical::DEFAULT,
            file: None,
            mtyp: ModTyp::File,
        }
    }
}

impl fmt::Display for ModKey
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "{}", self.name)?;
        if self.file.is_some() {
            write!(f, ":{}", self.file.as_ref().unwrap().to_str().unwrap())?;
        }
        Ok(())
    }
}

impl sendclone::SendClone for ModKey
{
    type Item = ModKey;

    fn clone_for_send(&self) -> ModKey
    {
        ModKey {
            name: self.name.clone_for_send(),
            file: self.file.clone(),
            mtyp: self.mtyp,
        }
    }
}

#[derive(Clone)]
#[derive(Copy)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Eq)]
#[derive(Hash)]
pub enum ModRelativity
{
    Absolute,
    Child,
    Sibling,
    Local,
}

/// mod alias .str, no /, for ast
///
/// local mod, import mod, no leading /
/// |child .str
/// |sibling .str
/// |absolute .str
///
/// relative mod .str
/// |a/b ->   child
/// |./a/b -> sibling
/// |/a/b ->  absolute
///
/// imported mod .str, leading / or ./ or no
///
/// canonical mod .str, no leading /
///
/// type mod?
/// .local_mod
/// .canonical
///
/// ModKey
/// .canonical
/// .path: str

/// ModAlias is a string that references an imported module
#[derive(Copy)]
#[derive(Clone)]
#[derive(Debug)]
pub struct ModAlias(pub &'static str);

impl ModAlias
{
    pub fn new(m: &'static str) -> ModAlias
    {
        ModAlias(m)
    }

    pub fn str(&self) -> &'static str
    {
        self.0
    }
}

impl fmt::Display for ModAlias
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        f.write_str(self.0)
    }
}

impl Borrow<str> for ModAlias
{
    fn borrow(&self) -> &str
    {
        self.0
    }
}

impl AsRef<str> for ModAlias
{
    fn as_ref(&self) -> &str
    {
        self.0
    }
}

impl PartialEq for ModAlias
{
    fn eq(&self, other: &ModAlias) -> bool
    {
        self.0 == other.0
    }
}

impl Eq for ModAlias {}

impl Hash for ModAlias
{
    fn hash<H: Hasher>(&self, state: &mut H)
    {
        self.0.hash(state);
    }
}

/// Not sure what this is for. Might delete it later.
#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
pub struct ImportedMod(pub PathBuf);

impl ImportedMod
{
    /// Check the relativity for this module's path
    pub fn relativity(&self) -> ModRelativity
    {
        Self::path_relativity(&self.0)
    }

    /// check the relativity for any path
    pub fn path_relativity(p: &Path) -> ModRelativity
    {
        if p.extension().map(|x| x == "function").unwrap_or(false) {
            return ModRelativity::Local;
        }
        match p.components().next().unwrap() {
            Component::Normal(_) => ModRelativity::Child,
            Component::ParentDir => ModRelativity::Sibling,
            Component::RootDir => ModRelativity::Absolute,
            Component::CurDir => {
                panic!("no relativity for current directory components");
            }
            Component::Prefix(pre) => {
                panic!("no relativity for prefix: {:?}", pre);
            }
        }
    }

    /// check if this imported module has an absolute path
    pub fn is_absolute(&self) -> bool
    {
        self.0.components().next().unwrap() == Component::RootDir
    }

    pub fn is_sibling(&self) -> bool
    {
        self.0.starts_with("../")
    }

    pub fn is_child(&self) -> bool
    {
        self.0.starts_with("./")
    }

    pub fn has_extension(&self) -> bool
    {
        self.0.extension().is_some()
    }

    // trim the extension if there is one
    pub fn trim_extension(&self) -> (&Path, bool)
    {
        // is there a way to only do this for the final path
        // since it's the only one that will ever have an extension?
        let xlen = match self.0.extension() {
            Some(x) => x.len() + 1,
            None => {
                return (&self.0, false);
            }
        };
        let pstr = self.0.as_os_str().to_str().unwrap();
        let (base, _) = pstr.split_at(pstr.len() - xlen);
        (Path::new(base), true)
    }

    pub fn is_empty(path: &Path) -> bool
    {
        path.as_os_str().is_empty()
    }

    pub fn root_head(path: &Path) -> Lresult<(&Path, &Path)>
    {
        let mut it = path.components();
        match it.next() {
            Some(Component::RootDir) => {
                // ok, that's fine
            }
            _ => {
                return Err(rustfail!(
                    "compile_failure",
                    "not a root path: {:?}",
                    path,
                ));
            }
        }
        it.next();
        let tail = it.as_path();

        let path_str = path.to_str().unwrap();
        let mut tail_len = tail.to_str().unwrap().len();
        if tail_len > 0 {
            // for the directory separator that disappeared
            tail_len += 1;
        }
        let head_len = path_str.len() - tail_len;
        let head_str = &path_str[0..head_len];
        let head = Path::new(head_str);
        Ok((head, tail))
    }

    pub fn child_head(path: &Path) -> Lresult<(&Path, &Path)>
    {
        let mut it = path.components();
        match it.next() {
            Some(Component::Normal(h)) => Ok((Path::new(h), it.as_path())),
            Some(_) => {
                return Err(rustfail!(
                    "compile_failure",
                    "cannot take head from not-child path {:?}",
                    path,
                ));
            }
            None => {
                return Err(rustfail!(
                    "compile_failure",
                    "cannot take head from empty path",
                ));
            }
        }
    }
}

impl From<&str> for ImportedMod
{
    fn from(imod: &str) -> ImportedMod
    {
        ImportedMod(PathBuf::from(String::from(imod)))
    }
}

impl AsRef<std::ffi::OsStr> for ImportedMod
{
    fn as_ref(&self) -> &std::ffi::OsStr
    {
        self.0.as_os_str()
    }
}

impl PartialEq<ImportedMod> for &str
{
    fn eq(&self, other: &ImportedMod) -> bool
    {
        **self == *other.0.as_os_str()
    }
}

impl fmt::Display for ImportedMod
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "{}", self.0.display())
    }
}

#[cfg(test)]
mod tests
{
    use super::{ImportedMod, ModAlias, ModRelativity};

    use std::collections::HashMap;
    use std::path::Path;

    #[test]
    fn test_mod_alias_map()
    {
        let mut ma_map = HashMap::new();
        let wp = Path::new("whatever");
        ma_map.insert(ModAlias("whatever"), wp);
        let wp_str = wp.to_str().unwrap();
        ma_map.get(wp_str).unwrap();
    }

    #[test]
    fn test_imported_mod_not_absolute()
    {
        let rel = ImportedMod::from("tortas/tacos");
        assert!(!rel.is_absolute());
    }

    #[test]
    fn test_imported_mod_relativity_absolute()
    {
        let rel = ImportedMod::from("/a").relativity();
        assert_eq!(ModRelativity::Absolute, rel);
    }

    #[test]
    fn test_imported_mod_relativity_child()
    {
        let rel = ImportedMod::from("a").relativity();
        assert_eq!(ModRelativity::Child, rel);
    }

    #[test]
    fn test_imported_mod_relativity_sibling()
    {
        let rel = ImportedMod::from("../a").relativity();
        assert_eq!(ModRelativity::Sibling, rel);
    }

    #[test]
    fn test_child_head_one()
    {
        let (h, t) = ImportedMod::child_head(Path::new("foo")).unwrap();
        assert_eq!("foo", h.as_os_str());
        assert_eq!("", t.as_os_str());
    }

    #[test]
    fn test_child_head_three()
    {
        let (h, t) = ImportedMod::child_head(Path::new("foo/bar/baz")).unwrap();
        assert_eq!("foo", h.as_os_str());
        assert_eq!("bar/baz", t.as_os_str());
    }

    #[test]
    fn test_child_head_absolute()
    {
        let f = ImportedMod::child_head(Path::new("/foo/bar")).unwrap_err();
        assert_eq!("compile_failure", f.tag.str());
    }

    #[test]
    fn test_child_head_sibling()
    {
        let f = ImportedMod::child_head(Path::new("../foo/bar")).unwrap_err();
        assert_eq!("compile_failure", f.tag.str());
    }
}
