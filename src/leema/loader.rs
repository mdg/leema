use crate::leema::failure::Lresult;
use crate::leema::lstr::Lstr;
use crate::leema::module::ModKey;

use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};


#[derive(Debug)]
pub struct Interloader
{
    pub main_mod: Lstr,
    paths: Vec<PathBuf>,
    modtxt: HashMap<Lstr, String>,
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
            modtxt: HashMap::new(),
        }
    }

    pub fn set_mod_txt(&mut self, modname: Lstr, content: String)
    {
        self.modtxt.insert(modname, content);
    }

    fn mod_name_to_key(&self, mod_name: &Lstr) -> Lresult<ModKey>
    {
        if self.modtxt.contains_key(mod_name) {
            Ok(ModKey::name_only(mod_name.clone()))
        } else {
            let path = self.find_file_path(mod_name)?;
            Ok(ModKey::new(mod_name.clone(), path))
        }
    }

    pub fn read_module(&self, mod_name: &Lstr) -> Lresult<String>
    {
        let mod_key = self.mod_name_to_key(mod_name)?;
        if mod_key.file.is_none() {
            self.modtxt
                .get(mod_name)
                .map(|txt| txt.clone())
                .ok_or_else(|| {
                    rustfail!(
                        "file_not_found",
                        "could not find text for module with no file: {}",
                        mod_name,
                    )
                })
        } else {
            self.read_file_text(mod_key.file.as_ref().unwrap())
        }
    }

    pub fn read_mod<'a, 'b>(&'a mut self, mod_name: &'b Lstr) -> Lresult<&'a str>
    {
        let mod_key = self.mod_name_to_key(mod_name)?;
        if mod_key.file.is_some() {
            let txt = self.read_file_text(mod_key.file.as_ref().unwrap())?;
            self.modtxt.insert(mod_name.clone(), txt);
        }
        self.modtxt
            .get(mod_name)
            .map(|txt| txt.as_str())
            .ok_or_else(|| {
                rustfail!(
                    "file_not_found",
                    "could not find text for module with no file: {}",
                    mod_name,
                )
            })
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
            "Module file does not exist: {:?}",
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
