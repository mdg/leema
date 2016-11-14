use leema::ast;
use leema::val::{Val, Type, SexprType};
use leema::iexpr::{Iexpr, Source};
use leema::list;
use leema::lex::{lex};
use leema::parse::{Token};

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::mem;


#[derive(Debug)]
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

#[derive(Debug)]
pub struct ModSrc
{
    pub imports: HashSet<String>,
    pub macros: HashMap<String, Val>,
    pub funcs: HashMap<String, Val>,
}

impl ModSrc
{
    pub fn new() -> ModSrc
    {
        ModSrc{
            imports: HashSet::new(),
            macros: HashMap::new(),
            funcs: HashMap::new(),
        }
    }

    pub fn split_ast(&mut self, ast: Val)
    {
        match ast {
            Val::Sexpr(SexprType::BlockExpr, sx) => {
                list::fold_mut(self, *sx
                    , ModSrc::split_ast_block_item);
            }
            _ => {
                panic!("what's that doing in the ast? {:?}", ast);
            }
        }
    }

    pub fn split_ast_block_item(ms: &mut ModSrc, item: Val)
    {
        match item {
            Val::Sexpr(SexprType::Import, imp) => {
                let iname = (*Val::to_str(list::head_ref(&imp))).clone();
                ms.imports.insert(iname);
            }
            Val::Sexpr(SexprType::DefMacro, dm) => {
                let mname = (*Val::to_str(list::head_ref(&dm))).clone();
                ms.macros.insert(mname, *dm);
            }
            Val::Sexpr(SexprType::DefFunc, df) => {
                let fname = (*Val::to_str(list::head_ref(&df))).clone();
                ms.funcs.insert(fname, *df);
            }
            _ => {
                panic!("Unexpected top-level ast item: {:?}", item);
            }
        }
    }
}

#[derive(Debug)]
pub struct Module
{
    pub key: ModKey,
    pub txt: String,
    pub tok: Vec<Token>,
    pub ast: Val,
    pub src: ModSrc,
    loaded: bool,
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
            src: ModSrc::new(),
            loaded: false,
        };
        modul
    }

    pub fn load(&mut self)
    {
        if self.loaded {
            return;
        }
        self.read_tokens();
        self.read_ast();
        self.split_ast();
        self.loaded = true;
    }

    pub fn read_tokens(&mut self)
    {
        self.tok = lex(&self.txt);
    }

    pub fn read_ast(&mut self)
    {
        self.ast = ast::parse(self.tok.clone());
    }

    pub fn split_ast(&mut self)
    {
        let mut ast = Val::Void;
        mem::swap(&mut ast, &mut self.ast);
        ModSrc::split_ast(&mut self.src, ast);
    }
}
