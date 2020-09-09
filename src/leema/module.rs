use crate::leema::failure::Lresult;
use crate::leema::lstr::Lstr;
use crate::leema::sendclone;

use std::borrow::Borrow;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::path::{Component, Path, PathBuf};


const DEFAULT_MODNAME: &'static str = "$";
pub const DEFAULT_MOD: CanonicalMod = CanonicalMod(Lstr::Sref("$"));

#[derive(Clone)]
#[derive(Copy)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Eq)]
#[derive(Hash)]
#[derive(PartialOrd)]
#[derive(Ord)]
pub enum SubModTyp
{
    Type,
    Interface,
    InterfaceImpl,
    Protocol,
    ProtocolImpl,
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
    pub name: CanonicalMod,
    pub file: Option<Box<Path>>,
    pub submod: Option<SubModTyp>,
}

impl ModKey
{
    pub fn new(name: CanonicalMod, path: PathBuf) -> ModKey
    {
        ModKey {
            name,
            file: Some(From::from(path)),
            submod: None,
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

    pub fn submod(&self, smt: SubModTyp, name: &'static str) -> ModKey
    {
        ModKey {
            name: CanonicalMod(lstrf!("{}.{}", self.name, name)),
            file: self.file.clone(),
            submod: Some(smt),
        }
    }
}

impl From<CanonicalMod> for ModKey
{
    fn from(name: CanonicalMod) -> ModKey
    {
        ModKey {
            name,
            file: None,
            submod: None,
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
            submod: None,
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
            submod: None,
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
            name: CanonicalMod(self.name.0.clone_for_send()),
            file: self.file.clone(),
            submod: self.submod.clone(),
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

impl AsRef<str> for ModAlias
{
    fn as_ref(&self) -> &str
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
macro_rules! canonical_mod {
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

    pub fn push(&self, import: &Path) -> CanonicalMod
    {
        match ImportedMod::path_relativity(import) {
            ModRelativity::Absolute => {
                CanonicalMod(Lstr::from(format!("{}", import.display())))
            }
            ModRelativity::Sibling => {
                let sib_base = self.as_path().parent().unwrap();
                let sib_path = import.strip_prefix("../").unwrap();
                let sib = sib_base.join(sib_path);
                CanonicalMod(Lstr::from(format!("{}", sib.display())))
            }
            ModRelativity::Child | ModRelativity::Local => {
                let ch_path = self.as_path().join(import);
                CanonicalMod(Lstr::from(format!("{}", ch_path.display())))
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
macro_rules! canonical_typemod {
    ($tm:expr) => {
        crate::leema::module::TypeMod {
            import: crate::leema::lstr::Lstr::Sref($tm),
            canonical: crate::leema::lstr::Lstr::Sref($tm),
        }
    };
}

#[macro_export]
macro_rules! user_type {
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

impl sendclone::SendClone for TypeMod
{
    type Item = TypeMod;

    fn clone_for_send(&self) -> TypeMod
    {
        TypeMod {
            import: self.import.clone_for_send(),
            canonical: self.canonical.clone_for_send(),
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
    fn test_canonical_ancestors()
    {
        let a = CanonicalMod::ancestors(Path::new("/foo/bar.baz"));
        let mut it = a.iter();
        assert_eq!("/foo", it.next().unwrap().as_os_str());
        assert_eq!("/foo/bar", it.next().unwrap().as_os_str());
        assert_eq!(2, a.len());
    }

    #[test]
    fn test_canonical_push_sibling()
    {
        let cm = CanonicalMod(Lstr::from("/foo/bar"));
        let result = cm.push(Path::new("../taco"));
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
