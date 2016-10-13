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
    imports: HashSet<String>,
    macros: HashMap<String, Val>,
    raw_func: HashMap<String, Val>,
    type0_func: HashMap<String, Iexpr>,
    typed_func: HashMap<String, Iexpr>,
}
