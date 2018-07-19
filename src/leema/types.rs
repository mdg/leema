
use leema::list;
use leema::lri::{Lri};
use leema::lstr::{Lstr};
use leema::struple::{Struple};
use leema::val::{Type, Val};


pub const STRUCT_FIELD_LRI: Lri = Lri{
    modules: Some(Lstr::Sref("types")),
    localid: Lstr::Sref("StructFieldVal"),
    params: None,
};


pub fn option_type(t: Type) -> Lri
{
    Lri::full(
        Some(Lstr::Sref("option")),
        Lstr::Sref("T"),
        Some(vec![t]),
    )
}

pub fn new_some(v: Val) -> Val
{
    let some_type = v.get_type();
    let optype = option_type(some_type);
    let fields = Struple(vec![(None, v)]);
    Val::EnumStruct(optype, Lstr::Sref("Some"), fields)
}

pub fn new_none(t: Type) -> Val
{
    let optype = option_type(t);
    Val::EnumToken(optype, Lstr::Sref("None"))
}

/**
 * Get the value of a struct field if given a name
 */
pub fn get_field_val(sv: &Val, name: &Lstr) -> Option<Val>
{
    if let &Val::EnumStruct(_, _, ref fields) = sv {
        for f in fields.0.iter() {
            match &f.0 {
                &Some(ref it_name) if it_name == name => {
                    return Some(f.1.clone());
                }
                _ => {
                    continue;
                }
            }
        }
        None
    } else {
        None
    }
}

pub fn new_struct_field(name: Option<Lstr>, typ: &Type) -> Val
{
    let name_val = match name {
        Some(inner_name) => {
            new_some(Val::Str(inner_name.rc()))
        }
        None => {
            new_none(Type::Str)
        }
    };
    let fields = Struple(vec![
        (Some(Lstr::Sref("name")), name_val),
        (Some(Lstr::Sref("type")), Val::Type(typ.clone())),
    ]);
    Val::Struct(STRUCT_FIELD_LRI.clone(), fields)
}

pub fn new_type_val(name: Lri, fields: &Vec<(Option<Lstr>, Type)>) -> Val
{
    let mut struct_fields_acc = Val::Nil;
    for f in fields.iter() {
        struct_fields_acc =
            list::cons(new_struct_field(f.0.clone(), &f.1), struct_fields_acc);
    }
    let struct_field_vals = list::reverse(&struct_fields_acc);

    let struct_type_lri =
        Lri::with_modules(Lstr::Sref("types"), Lstr::Sref("TypeVal"));
    let struct_fields_struple = Struple(vec![
        (Some(Lstr::Sref("fields")), struct_field_vals),
    ]);

    Val::Struct(struct_type_lri.clone(), struct_fields_struple)
}
