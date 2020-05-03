use crate::leema::failure::Lresult;
use crate::leema::lstr::Lstr;
use crate::leema::sendclone;

use std::borrow::Borrow;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::path::{Component, Path, PathBuf};


const DEFAULT_MODNAME: &'static str = "$";
pub const DEFAULT_MOD: CanonicalMod = CanonicalMod(Lstr::Sref("$"));

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq)]
#[derive(Eq)]
#[derive(Hash)]
#[derive(PartialOrd)]
#[derive(Ord)]
pub struct ModKey
{
    pub name: CanonicalMod,
    pub file: Option<Box<Path>>,
}

impl ModKey
{
    pub fn new(name: CanonicalMod, path: PathBuf) -> ModKey
    {
        ModKey {
            name,
            file: Some(From::from(path)),
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
            .unwrap_or_else(|| {
                // clone the name Lstr
                self.name.0.clone()
            })
    }
}

impl From<CanonicalMod> for ModKey
{
    fn from(name: CanonicalMod) -> ModKey
    {
        ModKey {
            name,
            file: None,
        }
    }
}

impl From<&'static str> for ModKey
{
    fn from(mod_str: &'static str) -> ModKey
    {
        ModKey {
            name: CanonicalMod(Lstr::Sref(mod_str)),
            file: None,
        }
    }
}

impl Default for ModKey
{
    fn default() -> ModKey
    {
        ModKey {
            name: DEFAULT_MOD,
            file: None,
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
            name: self.name.clone(),
            file: self.file.clone(),
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
/// TypeMod
/// |Alias .str
/// |Import
///    .alias
///    .canonical
/// |Canonical .str
///
/// ModKey
/// .canonical
/// .path: str

/// ModAlias is a string that references an imported module
#[derive(Copy)]
#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
#[derive(Eq)]
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
    pub fn relativity(&self) -> ModRelativity
    {
        Self::path_relativity(&self.0)
    }

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

    pub fn is_absolute(&self) -> bool
    {
        !(self.is_sibling() || self.is_child())
    }

    pub fn is_sibling(&self) -> bool
    {
        self.0.starts_with("../")
    }

    pub fn is_child(&self) -> bool
    {
        self.0.starts_with("./")
    }

    pub fn is_empty(path: &Path) -> bool
    {
        path.as_os_str().is_empty()
    }

    pub fn ancestors(path: &Path) -> Vec<&Path>
    {
        let av: Vec<&Path> = path.ancestors().collect();
        av.into_iter().rev().skip(1).collect()
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
            Some(Component::Normal(h)) => {
                Ok((Path::new(h), it.as_path()))
            }
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

#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
#[derive(Eq)]
#[derive(Ord)]
#[derive(Hash)]
pub struct CanonicalMod(pub Lstr);

#[macro_export]
macro_rules! canonical_mod
{
    ($cm:expr) => {
        crate::leema::module::CanonicalMod(crate::leema::lstr::Lstr::Sref($cm))
    };
}

impl CanonicalMod
{
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

    pub fn mod_path(&self) -> &Path
    {
        self.as_path()
    }

    pub fn push(&self, imp: ImportedMod) -> CanonicalMod
    {
        match imp.relativity() {
            ModRelativity::Absolute => {
                CanonicalMod(Lstr::from(format!("{}", imp)))
            }
            ModRelativity::Sibling => {
                let sib_base = self.as_path().parent().unwrap();
                let sib_path = imp.0.strip_prefix("../").unwrap();
                let sib = sib_base.join(sib_path);
                CanonicalMod(Lstr::from(format!("{}", sib.display())))
            }
            ModRelativity::Child|ModRelativity::Local => {
                let ch_path = self.as_path().join(imp.0);
                CanonicalMod(Lstr::from(format!("{}", ch_path.display())))
            }
        }
    }

    pub fn file_path_buf(cpath: &Path) -> Lresult<PathBuf>
    {
        cpath
            .strip_prefix("/")
            .map(|relative_path| {
                relative_path.with_extension("lma")
            })
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

impl From<&Path> for CanonicalMod
{
    fn from(mp: &Path) -> CanonicalMod
    {
        if !mp.is_absolute() {
            panic!("canonical mod must be absolute: {:?}", mp);
        }
        CanonicalMod(lstrf!("{}", mp.display()))
    }
}

impl Borrow<str> for CanonicalMod
{
    fn borrow(&self) -> &str
    {
        self.0.str()
    }
}

impl PartialEq<CanonicalMod> for str
{
    fn eq(&self, other: &CanonicalMod) -> bool
    {
        *self == other.0
    }
}

impl Default for CanonicalMod
{
    fn default() -> CanonicalMod
    {
        DEFAULT_MOD
    }
}

impl fmt::Display for CanonicalMod
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        f.write_str(self.0.str())
    }
}

#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialOrd)]
#[derive(Eq)]
#[derive(Ord)]
#[derive(Hash)]
pub struct TypeMod
{
    pub import: Lstr,
    pub canonical: Lstr,
}

#[macro_export]
macro_rules! canonical_typemod
{
    ($tm:expr) => {
        crate::leema::module::TypeMod {
            import: crate::leema::lstr::Lstr::Sref($tm),
            canonical: crate::leema::lstr::Lstr::Sref($tm),
        }
    };
}

#[macro_export]
macro_rules! user_type
{
    ($m:expr, $t:expr) => {
        crate::leema::val::Type::User(canonical_typemod!($m), $t)
    };
}

impl From<&CanonicalMod> for TypeMod
{
    fn from(cmod: &CanonicalMod) -> TypeMod
    {
        TypeMod {
            import: cmod.0.clone(),
            canonical: cmod.0.clone(),
        }
    }
}

/*
impl From<ModAlias> for TypeMod
{
    fn from(alias: ModAlias) -> TypeMod
    {
        TypeMod::Alias(alias.0)
    }
}

impl From<(ModAlias, &ImportedMod)> for TypeMod
{
    fn from(mods: (ModAlias, &ImportedMod)) -> TypeMod
    {
        TypeMod {
            alias: mods.0,
            imported: mods.1.clone(),
            canonical: None,
        }
    }
}
*/

impl PartialEq for TypeMod
{
    fn eq(&self, other: &TypeMod) -> bool
    {
        self.canonical == other.canonical
    }
}

impl fmt::Display for TypeMod
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        f.write_str(self.import.str())
    }
}


#[cfg(test)]
mod tests
{
    use super::{CanonicalMod, ImportedMod, ModAlias, ModRelativity};
    use crate::leema::lstr::Lstr;

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
    fn test_canonical_push_sibling()
    {
        let cm = CanonicalMod(Lstr::from("/foo/bar"));
        let im = ImportedMod::from("../taco");
        let result = cm.push(im);
        assert_eq!("/foo/taco", result.0.str());
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
