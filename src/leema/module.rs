use crate::leema::lstr::Lstr;
use crate::leema::sendclone;

use std::fmt;
use std::path::{Path, PathBuf};


const DEFAULT_MODNAME: &'static str = "$";

#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Eq)]
#[derive(Hash)]
#[derive(PartialOrd)]
#[derive(Ord)]
pub struct Chain(Vec<&'static str>);

impl Chain
{
    pub fn new(chain: Vec<&'static str>) -> Chain
    {
        Chain(chain)
    }

    pub fn chain(&self) -> &Vec<&'static str>
    {
        &self.0
    }

    pub fn is_empty(&self) -> bool
    {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize
    {
        self.0.len()
    }

    pub fn last(&self) -> &'static str
    {
        self.0.last().expect("module chain underflow")
    }

    pub fn push(&mut self, sub: &'static str)
    {
        self.0.push(sub);
    }

    pub fn pop(&mut self) -> &'static str
    {
        self.0.pop().expect("module chain underflow")
    }

    pub fn join(&mut self, mut sub: Chain)
    {
        self.0.append(&mut sub.0);
    }

    /// Get the parent of this Chain
    pub fn parent(&self) -> Chain
    {
        let mut tmp = self.0.clone();
        tmp.pop();
        Chain(tmp)
    }

    pub fn head(mut self) -> (&'static str, Option<Chain>)
    {
        let head = self.0.remove(0);
        let tail = if self.0.is_empty() {
            None
        } else {
            Some(self)
        };
        (head, tail)
    }

    pub fn split(mut self) -> (Chain, Option<Chain>)
    {
        let tail = self.0.split_off(1);
        let opt_tail = if tail.is_empty() {
            None
        } else {
            Some(Chain(tail))
        };
        (self, opt_tail)
    }
}

impl From<&'static str> for Chain
{
    fn from(ch: &'static str) -> Chain
    {
        let paths = ch.split("/").collect();
        Chain(paths)
    }
}

impl From<&Chain> for String
{
    fn from(ch: &Chain) -> String
    {
        ch.0.join("/")
    }
}

impl Default for Chain
{
    fn default() -> Chain
    {
        Chain(vec![DEFAULT_MODNAME])
    }
}

impl fmt::Display for Chain
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        f.write_str(&self.0.join("/"))
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
    pub name: Lstr,
    pub chain: Chain,
    pub file: Option<Box<Path>>,
}

impl ModKey
{
    pub fn new(name: Chain, path: PathBuf) -> ModKey
    {
        ModKey {
            name: lstrf!("{:?}", name),
            chain: name,
            file: Some(From::from(path)),
        }
    }

    /// If the path exists, get it. Otherwise return the module name
    pub fn best_path(&self) -> Lstr
    {
        self.file.as_ref()
            .and_then(|ref p| {
                p.to_str()
            })
            .map(|ps| {
                Lstr::from(String::from(ps))
            })
            .unwrap_or_else(|| {
                self.name.clone()
            })
    }
}

impl From<Chain> for ModKey
{
    fn from(chain: Chain) -> ModKey
    {
        ModKey {
            name: lstrf!("{}", chain),
            chain,
            file: None,
        }
    }
}

impl From<&'static str> for ModKey
{
    fn from(chain_str: &'static str) -> ModKey
    {
        ModKey::from(Chain::from(chain_str))
    }
}

impl Default for ModKey
{
    fn default() -> ModKey
    {
        ModKey {
            name: Lstr::Sref(DEFAULT_MODNAME),
            chain: Chain::default(),
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
            name: self.name.clone_for_send(),
            chain: self.chain.clone(),
            file: self.file.clone(),
        }
    }
}

/// Collect a module chain together with a module relationship enum
#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Eq)]
#[derive(Hash)]
pub struct ModPath
{
    pub relativity: ModRelativity,
    pub path: Chain, // or Lstr
}

impl ModPath
{
    pub fn new(relativity: ModRelativity, path: Chain) -> ModPath
    {
        ModPath {
            relativity,
            path,
        }
    }

    pub fn abs(path: Chain) -> ModPath
    {
        ModPath {
            relativity: ModRelativity::Absolute,
            path,
        }
    }

    pub fn child(path: Chain) -> ModPath
    {
        ModPath {
            relativity: ModRelativity::Child,
            path,
        }
    }

    pub fn local(path: Chain) -> ModPath
    {
        ModPath {
            relativity: ModRelativity::Local,
            path,
        }
    }

    pub fn push(&mut self, mut sub: &'static str)
    {
        self.path.push(&mut sub);
    }

    pub fn join(&mut self, sub: Chain)
    {
        self.path.join(sub);
    }

    pub fn head(mut self) -> (&'static str, Option<ModPath>)
    {
        let head = self.path.0.remove(0);
        self.relativity = ModRelativity::Child;
        let tail = if self.path.0.is_empty() {
            None
        } else {
            Some(self)
        };
        (head, tail)
    }

    pub fn split(mut self) -> (ModPath, Option<ModPath>)
    {
        let tail = self.path.0.split_off(1);
        let opt_tail = if tail.is_empty() {
            None
        } else {
            Some(ModPath::child(Chain(tail)))
        };
        (self, opt_tail)
    }
}

impl fmt::Display for ModPath
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self.relativity {
            ModRelativity::Absolute => {
                f.write_str("/")?;
            }
            ModRelativity::Sibling => {
                f.write_str("../")?;
            }
            ModRelativity::Child|ModRelativity::Local => {
                // nothing
            }
        }
        write!(f, "{}", self.path)
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
