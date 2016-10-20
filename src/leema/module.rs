use std::collections::{HashMap, HashSet};


/*
struct module
- name
- list imports
- map str:val makros
- // map str:Val raw_types
- map str:Val raw_func
- map str:Type type0_types
- map str:Iexpr type0_func
- map str:Type typen_types
- map str:Iexpr typen_func
*/
pub struct Module
{
    name: String,
    file: String,
    sexpr: Val,
    imports: HashSet<String>,
    macros: HashMap<String, Val>,
    raw_func: HashMap<String, Val>,
    type0_func: HashMap<String, Iexpr>,
    typed_func: HashMap<String, Iexpr>,
}

impl Module
{
    pub fn new(&mut self, name: &str) -> Module
    {
        Module{
            name: name.clone(),
            file: name.clone(),
            sexpr: Val::Void,
            imports: HashSet::new(),
            macros: HashMap::new(),
            raw_func: HashMap::new(),
            type0_func: HashMap::new(),
            typed_func: HashMap::new(),
        }
    }
}


pub struct Program
{
    modules: HashMap<u64, HashMap<String, Module>>,
    current_version: u64,
}

impl ProgramLoad
{
    pub fn new() -> ProgramLoad
    {
        ProgramLoad{
            module: HashMap::new(),
            current_version: 0,
        }
    }

    pub fn init_module(&mut self, name: &str) -> &mut Module
    {
        self.modules.insert(module, Module::new(name));
        self.modules.get_mut(module).unwrap()
    }
}
