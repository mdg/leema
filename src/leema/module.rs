use leema::ast;
use leema::val::{Val, Type, SexprType};
use leema::iexpr::{Iexpr, Source};
use leema::list;
use leema::lex::{lex};
use leema::parse::{Token};

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::rc::{Rc};
use std::sync::{Arc};
use std::mem;


#[derive(Debug)]
#[derive(Clone)]
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

type MacroDef = (Vec<Arc<String>>, Val);
type MacroMap = HashMap<String, MacroDef>;

#[derive(Debug)]
pub struct ModuleSource
{
    pub key: Rc<ModKey>,
    pub txt: String,
    pub ast: Val,
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

    pub fn read_ast(txt: &str) -> Val
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
        mp.split_ast(&ms.ast);
        mp
    }

    pub fn split_ast(&mut self, ast: &Val)
    {
        match ast {
            &Val::Sexpr(SexprType::BlockExpr, ref sx) => {
                list::fold_mut_ref(self, sx
                    , ModulePreface::split_ast_block_item);
            }
            _ => {
                panic!("what's that doing in the ast? {:?}", ast);
            }
        }
    }

    pub fn split_ast_block_item(mp: &mut ModulePreface, item: &Val)
    {
        match item {
            &Val::Sexpr(SexprType::Import, ref imp) => {
                let iname = list::head_ref(imp);
                mp.imports.insert(String::from(iname.str()));
            }
            &Val::Sexpr(SexprType::DefMacro, ref dm) => {
                let (mname_val, args_val, body) = list::to_ref_tuple3(dm);
                let mname = mname_val.str();
                let mut args = vec![];
                list::fold_mut_ref(&mut args, args_val, |acc, a| {
                    acc.push(a.to_str().clone());
                });
                mp.macros.insert(String::from(mname), (args, body.clone()));
            }
            _ => {
                println!("Unexpected top-level ast item: {:?}", item);
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
    pub newtypes: HashMap<Type, Val>,
}

impl ModuleInterface
{
    pub fn new(ms: &ModuleSource) -> ModuleInterface
    {
        ModuleInterface{
            key: ms.key.clone(),
            funcs: HashMap::new(),
            valtypes: HashMap::new(),
            newtypes: HashMap::new(),
        }
    }
}
