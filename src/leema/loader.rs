use crate::leema::failure::Lresult;
use crate::leema::lstr::Lstr;
use crate::leema::module::ModKey;

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
    pub main_mod: Lstr,
    paths: Vec<PathBuf>,
    texts: HashMap<Lstr, &'static str>,
}

impl Interloader
{
    pub fn new(mainfile: Lstr, path_str: &str) -> Interloader
    {
        let path = Path::new(mainfile.str());
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
            Lstr::from(modname.unwrap().to_str().unwrap().to_string());
        Interloader {
            main_mod: mod_str,
            paths,
            texts: HashMap::new(),
        }
    }

    pub fn set_mod_txt(
        &mut self,
        modname: Lstr,
        content: String,
    ) -> &'static str
    {
        let stext = unsafe { put_modtxt(content) };
        self.texts.insert(modname, stext);
        stext
    }

    fn mod_name_to_key(&self, mod_name: &Lstr) -> Lresult<ModKey>
    {
        let contained = self.texts.contains_key(mod_name);
        if contained {
            Ok(ModKey::name_only(mod_name.clone()))
        } else {
            let path = ltry!(self.find_file_path(mod_name));
            Ok(ModKey::new(mod_name.clone(), path))
        }
    }

    pub fn read_module(&mut self, mod_name: &Lstr) -> Lresult<String>
    {
        self.read_mod(mod_name).map(|text| text.to_string())
    }

    pub fn read_mod(&mut self, mod_name: &Lstr) -> Lresult<&'static str>
    {
        if let Some(txt) = self.texts.get(mod_name) {
            return Ok(txt);
        }

        let mod_key = ltry!(self.mod_name_to_key(mod_name));
        let filepath = mod_key.file.as_ref().ok_or_else(|| {
            rustfail!(
                "file_not_found",
                "could not find text for module with no file: {}",
                mod_name,
            )
        })?;
        let text = self.read_file_text(filepath)?;
        let stext = self.set_mod_txt(mod_name.clone(), text);
        self.texts.insert(mod_name.clone(), stext);
        Ok(stext)
    }

    fn find_file_path(&self, name: &Lstr) -> Lresult<PathBuf>
    {
        let mut file_path = PathBuf::new();
        file_path.push(name.str());
        file_path.set_extension("lma");

        for p in self.paths.iter() {
            let mut check_path = p.clone();
            check_path.push(file_path.clone());
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
        println!("file: {:?}", file!());
        let root_path = Path::new(file!())
            .parent().unwrap()   // pop loader.rs
            .parent().unwrap()   // pop leema/
            .parent().unwrap();  // pop src/
        let leema_path = root_path.join(Path::new("lib"));

        Interloader {
            main_mod: Lstr::Sref("__default__"),
            paths: vec![leema_path],
            texts: HashMap::new(),
        }
    }
}


#[cfg(test)]
mod tests
{
    use crate::leema::loader::Interloader;
    use crate::leema::lstr::Lstr;

    use std::path::Path;

    #[test]
    fn test_root_path()
    {
        let i = Interloader::new(Lstr::Sref("hello/world.lma"), "lib");

        let expected = vec![Path::new("hello"), Path::new("lib")];
        assert_eq!(expected, i.paths);
    }

    #[test]
    fn test_main_mod()
    {
        let i = Interloader::new(Lstr::Sref("hello/world.lma"), "lib");

        assert_eq!("world", i.main_mod.str());
    }

}
