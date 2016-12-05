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

#[derive(Debug)]
pub struct ModSrc
{
    pub key: ModKey,
    pub imports: HashSet<String>,
    pub macros: HashMap<String, (Vec<Arc<String>>, Val)>,
    pub funcs: HashMap<String, Val>,
    pub func_types: HashMap<String, Type>,
}

impl ModSrc
{
    pub fn new(mk: &ModKey) -> ModSrc
    {
        ModSrc{
            key: mk.clone(),
            imports: HashSet::new(),
            macros: HashMap::new(),
            funcs: HashMap::new(),
            func_types: HashMap::new(),
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

    pub fn func_type(df: &Val) -> Type
    {
        let (name, args, result_type) = list::to_ref_tuple3(df);
        result_type.to_type().clone()
    }

    pub fn split_ast_block_item(ms: &mut ModSrc, item: Val)
    {
        match item {
            Val::Sexpr(SexprType::Import, imp) => {
                let iname = (*Val::to_str(list::head_ref(&imp))).clone();
                ms.imports.insert(iname);
            }
            Val::Sexpr(SexprType::DefMacro, dm) => {
                let (mname_val, args_val, body) = list::to_tuple3(*dm);
                let mname = (*mname_val.to_str()).clone();
                let args = list::map_to_vec(args_val, |a| {
                    a.to_str()
                });
                ms.macros.insert(mname, (args, body));
            }
            Val::Sexpr(SexprType::DefFunc, df) => {
                let ftype = ModSrc::func_type(&*df);
                let fname = (*Val::to_str(list::head_ref(&df))).clone();
                ms.funcs.insert(fname.clone(), *df);
                ms.func_types.insert(fname, ftype);
            }
            _ => {
                panic!("Unexpected top-level ast item: {:?}", item);
            }
        }
    }
}

#[derive(Debug)]
pub struct ModuleInterface
{
    pub key: ModKey,
    pub macros: HashMap<String, (Vec<Arc<String>>, Val)>,
    pub func_types: HashMap<String, Type>,
    pub type_defs: HashMap<String, Type>,
}

impl ModuleInterface
{
    pub fn new(key: &ModKey) -> ModuleInterface
    {
        ModuleInterface{
            key: key.clone(),
            macros: HashMap::new(),
            func_types: HashMap::new(),
            type_defs: HashMap::new(),
        }
    }

    pub fn load(ms: &ModSrc) -> Rc<ModuleInterface>
    {
        Rc::new(ModuleInterface{
            key: ms.key.clone(),
            macros: ms.macros.clone(),
            func_types: ms.func_types.clone(),
            type_defs: HashMap::new(),
        })
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
    pub ifc: Rc<ModuleInterface>,
    loaded: bool,
    pub imports_loaded: bool,
}

impl Module
{
    pub fn new(key: ModKey, txt: String) -> Module
    {
        let modsrc = ModSrc::new(&key);
        let modifc = Rc::new(ModuleInterface::new(&key));
        Module{
            key: key,
            // version: Option<Version>,
            txt: txt,
            tok: Vec::new(),
            ast: Val::Void,
            src: modsrc,
            ifc: modifc,
            loaded: false,
            imports_loaded: false,
        }
    }

    pub fn load(&mut self)
    {
        if self.loaded {
            return;
        }
        self.read_tokens();
        self.read_ast();
        self.split_ast();
        self.ifc = ModuleInterface::load(&self.src);
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

    pub fn init_scope(&mut self)
    {
    }
}
