use leema::ast::{self, Ast};
use leema::lex::lex;
use leema::lstr::Lstr;
use leema::parse::Token;
use leema::val::{Type, Val};

use std::collections::{HashMap, HashSet};
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

type MacroMap = HashMap<Lstr, Ast>;

#[derive(Debug)]
pub struct ModuleSource
{
    pub key: ModKey,
    pub txt: String,
    pub ast: Ast,
}

impl ModuleSource
{
    pub fn new(mk: ModKey, txt: String) -> ModuleSource
    {
        let ast = ModuleSource::read_ast(&txt);
        ModuleSource { key: mk, txt, ast }
    }

    pub fn init() -> ModuleSource
    {
        let init_key = ModKey::name_only(Lstr::Sref("__init__"));
        ModuleSource::new(init_key, String::from(""))
    }

    pub fn read_tokens(txt: &str) -> Vec<Token>
    {
        lex(txt)
    }

    pub fn read_ast(txt: &str) -> ast::Ast
    {
        let toks = ModuleSource::read_tokens(txt);
        ast::parse(toks)
    }
}

#[derive(Debug)]
pub struct ModulePreface
{
    pub key: ModKey,
    pub imports: HashSet<Lstr>,
    pub macros: MacroMap,
}

impl ModulePreface
{
    pub fn new(ms: &ModuleSource) -> ModulePreface
    {
        let mut mp = ModulePreface {
            key: ms.key.clone(),
            imports: HashSet::new(),
            macros: HashMap::new(),
        };
        // everything imports prefab by default
        // should probably get rid of this eventually tho
        if &*ms.key.name != "prefab" {
            mp.imports.insert(Lstr::Sref("prefab"));
        }
        mp.split_ast(&ms.ast);
        mp
    }

    pub fn split_ast(&mut self, ast: &Ast)
    {
        match ast {
            &Ast::Block(ref lines) => {
                for l in lines.iter() {
                    ModulePreface::split_ast_block_item(self, l);
                }
            }
            _ => {
                panic!("what's that doing in the ast? {:?}", ast);
            }
        }
    }

    pub fn split_ast_block_item(mp: &mut ModulePreface, item: &Ast)
    {
        match item {
            &Ast::Import(ref i, _) => {
                let imp_string = (**i).localid_str();
                mp.imports.insert(imp_string.clone());
            }
            &Ast::DefFunc(
                ast::FuncClass::Macro,
                ref decl,
                ref _body,
            ) => {
                let name_string = Lstr::from(&decl.name);
                mp.macros.insert(name_string, item.clone());
            }
            _ => {
                // ignore everything else, it will be handled in a later phase
            }
        }
    }
}

#[derive(Debug)]
pub struct ModuleInterface
{
    pub key: ModKey,
    pub funcs: HashMap<Lstr, Option<Val>>,
    pub valtypes: HashMap<Lstr, Type>,
}

impl ModuleInterface
{
    pub fn new(ms: &ModuleSource) -> ModuleInterface
    {
        ModuleInterface {
            key: ms.key.clone(),
            funcs: HashMap::new(),
            valtypes: HashMap::new(),
        }
    }
}
