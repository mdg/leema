use crate::leema::lstr::Lstr;

use std::fmt;
use std::path::PathBuf;


// Textmod -> Preface -> Protomod -> Intermod -> Code

#[derive(Debug)]
#[derive(Clone)]
pub struct ModKey
{
    pub name: Lstr,
    pub file: Option<PathBuf>,
    // version: Option<Version>,
}

impl ModKey
{
    pub fn new(name: Lstr, path: PathBuf) -> ModKey
    {
        ModKey {
            name,
            file: Some(path),
        }
    }

    pub fn name_only(name: Lstr) -> ModKey
    {
        ModKey { name, file: None }
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
