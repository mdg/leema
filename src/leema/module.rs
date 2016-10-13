use std::collections::{HashMap};


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
    imports: HashMap<String, bool>,
    macros: HashMap<String, Val>,
}
