use leema::ast::{self, Ast};
use leema::val::{Val, Type};
use leema::lex::{lex};
use leema::parse::{Token};

use std::collections::{HashMap, HashSet};
use std::path::{PathBuf};
use std::rc::{Rc};


// Textmod -> Preface -> Protomod -> Intermod -> Code

#[derive(Debug)]
#[derive(Clone)]
pub struct ModKey
{
    pub name: Rc<String>,
    pub file: Option<PathBuf>,
    // version: Option<Version>,
}

impl ModKey
{
    pub fn new(name: &str, path: PathBuf) -> ModKey
    {
        ModKey{
            name: Rc::new(String::from(name)),
            file: Some(path),
        }
    }

    pub fn name_only(name: &str) -> ModKey
    {
        ModKey{
            name: Rc::new(String::from(name)),
            file: None,
        }
    }
}

type MacroMap = HashMap<String, Ast>;

#[derive(Debug)]
pub struct ModuleSource
{
    pub key: Rc<ModKey>,
    pub txt: String,
    pub ast: Ast,
}

impl ModuleSource
{
    pub fn new(mk: ModKey, txt: String) -> ModuleSource
    {
        let ast = ModuleSource::read_ast(&txt);
        ModuleSource{
            key: Rc::new(mk),
            txt: txt,
            ast: ast,
        }
    }

    pub fn init() -> ModuleSource
    {
        let init_key = ModKey::name_only("__init__");
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
    pub key: Rc<ModKey>,
    pub imports: HashSet<String>,
    pub macros: MacroMap,
}

impl ModulePreface
{
    pub fn new(ms: &ModuleSource) -> ModulePreface
    {
        let mut mp = ModulePreface{
            key: ms.key.clone(),
            imports: HashSet::new(),
            macros: HashMap::new(),
        };
        // everything imports prefab by default
        // should probably get rid of this eventually tho
        if &*ms.key.name != "prefab" {
            mp.imports.insert(String::from("prefab"));
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
                let imp_string = (**i).localid_str().to_string();
                mp.imports.insert(imp_string);
            }
            &Ast::DefFunc(ast::FuncClass::Macro, ref name, ref _args
                    , _, ref _body, ref _loc
            ) => {
                let name_string = String::from(&**name);
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
    pub key: Rc<ModKey>,
    pub funcs: HashMap<String, Option<Val>>,
    pub valtypes: HashMap<String, Type>,
}

impl ModuleInterface
{
    pub fn new(ms: &ModuleSource) -> ModuleInterface
    {
        ModuleInterface{
            key: ms.key.clone(),
            funcs: HashMap::new(),
            valtypes: HashMap::new(),
        }
    }
}
