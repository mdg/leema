use crate::leema::failure::Lresult;
use crate::leema::module::{self, ModKey};

use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::sync::Mutex;

use lazy_static::lazy_static;


static mut TEXTS: Option<Vec<String>> = None;

unsafe fn put_modtxt(val: String) -> &'static str
{
    lazy_static! {
        static ref LOCK: Mutex<()> = Mutex::new(());
    }
    let mut lock = LOCK.lock().expect("failed locking TEXTS");
    if TEXTS.is_none() {
        TEXTS = Some(Vec::with_capacity(12));
    }
    let texts = TEXTS.as_mut().unwrap();
    texts.push(val);
    let stext: &'static str = texts.last().as_ref().unwrap();
    *lock = ();
    stext
}

#[derive(Debug)]
pub struct Interloader
{
    pub main_mod: module::Chain,
    paths: Vec<PathBuf>,
    keys: HashMap<module::Chain, ModKey>,
    texts: HashMap<module::Chain, &'static str>,
}

impl Interloader
{
    pub fn new(mainfile: &'static str, path_str: &str) -> Interloader
    {
        let path = Path::new(mainfile);
        let ext = path.extension();
        if ext.is_none() {
            panic!("Main file has no extension: {}", mainfile);
        }
        if ext.unwrap() != "lma" {
            panic!("Main file extension is not lma: {}", mainfile);
        }
        let modname = path.file_stem();
        if modname.is_none() {
            panic!("Is that not a real file? {}", mainfile);
        }

        let root_path = Some(path.parent().unwrap().to_path_buf());
        let splits = env::split_paths(path_str);
        let paths: Vec<PathBuf> = root_path.into_iter().chain(splits).collect();

        let mod_str =
            Self::static_str(modname.unwrap().to_str().unwrap().to_string());
        let main_mod = module::Chain::from(mod_str);
        Interloader {
            main_mod,
            paths,
            keys: HashMap::new(),
            texts: HashMap::new(),
        }
    }

    pub fn set_mod_txt(
        &mut self,
        key: ModKey,
        content: String,
    ) -> &'static str
    {
        let stext = Self::static_str(content);
        self.texts.insert(key.chain.clone(), stext);
        self.keys.insert(key.chain.clone(), key);
        stext
    }

    pub fn new_key(&self, chain: &module::Chain) -> Lresult<ModKey>
    {
        if let Some(found) = self.keys.get(chain) {
            return Ok(found.clone());
        }

        let path = ltry!(self.find_file_path(chain));
        Ok(ModKey::new(chain.clone(), path))
    }

    pub fn read_mod(&mut self, key: &ModKey) -> Lresult<&'static str>
    {
        if let Some(txt) = self.texts.get(&key.chain) {
            return Ok(txt);
        }

        let filepath = key.file.as_ref().ok_or_else(|| {
            rustfail!(
                "file_not_found",
                "could not find text for module with no file: {}",
                key,
            )
        })?;
        let text = self.read_file_text(filepath)?;
        let stext = self.set_mod_txt(key.clone(), text);
        Ok(stext)
    }

    /// turn any regular String into a &'static str that will live forever
    pub fn static_str(s: String) -> &'static str
    {
        unsafe { put_modtxt(s) }
    }

    /// this all seems suboptimal, but it can probably be fixed later
    fn find_file_path(&self, name: &module::Chain) -> Lresult<PathBuf>
    {
        let mut file_path = PathBuf::new();
        file_path.push(String::from(name));
        let mod_path = file_path.join("_.lma");
        file_path.set_extension("lma");

        for p in self.paths.iter() {
            let mut check_path = p.clone();
            check_path.push(file_path.clone());
            if check_path.exists() && check_path.is_file() {
                return Ok(check_path);
            }

            check_path = p.join(mod_path.clone());
            if check_path.exists() && check_path.is_file() {
                return Ok(check_path);
            }
        }

        Err(rustfail!(
            "file_not_found",
            "Module file path cannot be found: {:?}",
            name,
        ))
    }

    fn read_file_text(&self, path: &Path) -> Lresult<String>
    {
        if !path.exists() {
            return Err(rustfail!(
                "file_not_found",
                "Module file does not exist: {:?}",
                path,
            ));
        }
        if !path.is_file() {
            return Err(rustfail!(
                "file_not_found",
                "Module is not a file: {:?}",
                path,
            ));
        }
        let mut f = File::open(path).ok().unwrap();
        let mut result = String::new();
        f.read_to_string(&mut result)
            .expect("failed reading file to text");
        Ok(result)
    }
}

/// Default Interloader primarily for using in tests and dev
impl Default for Interloader
{
    fn default() -> Interloader
    {
        let root_path = Path::new(file!())
            .parent()
            .unwrap() // pop loader.rs
            .parent()
            .unwrap() // pop leema/
            .parent()
            .unwrap(); // pop src/
        let leema_path = root_path.join(Path::new("lib"));

        Interloader {
            main_mod: module::Chain::default(),
            paths: vec![root_path.to_path_buf(), leema_path],
            keys: HashMap::new(),
            texts: HashMap::new(),
        }
    }
}


#[cfg(test)]
mod tests
{
    use crate::leema::loader::Interloader;

    use std::path::Path;

    #[test]
    fn test_root_path()
    {
        let i = Interloader::new("hello/world.lma", "lib");

        let expected = vec![Path::new("hello"), Path::new("lib")];
        assert_eq!(expected, i.paths);
    }

    #[test]
    fn test_main_mod()
    {
        let i = Interloader::new("hello/world.lma", "lib");

        assert_eq!("world", &String::from(&i.main_mod));
    }

}
