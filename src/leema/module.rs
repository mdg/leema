use leema::val::{Val, Type, SexprType};
use leema::iexpr::{Iexpr, Source};
use leema::parse::{Token};

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};


pub struct ModKey
{
    pub name: String,
    pub file: Option<PathBuf>,
    // version: Option<Version>,
}

impl ModKey
{
    pub fn new(name: &str, path: PathBuf) -> ModKey
    {
        ModKey{
            name: String::from(name),
            file: Some(path),
        }
    }

    pub fn name_only(name: &str) -> ModKey
    {
        ModKey{
            name: String::from(name),
            file: None,
        }
    }
}

pub struct Module
{
    pub key: ModKey,
    pub txt: String,
    pub tok: Vec<Token>,
    pub ast: Val,
    pub imports: HashSet<String>,
    pub macros: HashMap<String, Val>,
    pub srcfunx: HashMap<String, Val>,
}

impl Module
{
    pub fn new(key: ModKey, txt: String) -> Module
    {
        let modul = Module{
            key: key,
            // version: Option<Version>,
            txt: txt,
            tok: Vec::new(),
            ast: Val::Void,
            imports: HashSet::new(),
            macros: HashMap::new(),
            srcfunx: HashMap::new(),
        };
        modul
    }
}
