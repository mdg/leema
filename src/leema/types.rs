
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

pub fn get_named_struct_field<'a, 'b>(sv: &'a Val, name: &'b Lstr
    ) -> Option<(i16, &'a Val)>
{
    let fields = match sv {
        &Val::Struct(_, ref fld_struple) => fld_struple,
        &Val::EnumStruct(_, _, ref fld_struple) => fld_struple,
        _ => {
            return None;
        }
    };
    for (i, f) in fields.0.iter().enumerate() {
        match &f.0 {
            &Some(ref it_name) if it_name == name => {
                return Some((i as i16, &f.1));
            }
            _ => {
                continue;
            }
        }
    }
    None
}

pub fn get_indexed_struct_field(sv: &Val, idx: i16) -> Option<&(Option<Lstr>, Val)>
{
    let fields = match sv {
        &Val::Struct(_, ref fld_struple) => fld_struple,
        &Val::EnumStruct(_, _, ref fld_struple) => fld_struple,
        _ => {
            return None;
        }
    };
    fields.0.get(0)
}

/**
 * Check if this value is a variant with the given name
 */
pub fn is_enum_variant(v: &Val, test_variant: &Lstr) -> bool
{
    match v {
        Val::EnumToken(_, ref val_variant) => {
            test_variant == val_variant
        }
        Val::EnumStruct(_, ref val_variant, _) => {
            test_variant == val_variant
        }
        _ => false,
    }
}

/**
 * Get the value of a struct field if given a name
 */
pub fn get_field_type<'a, 'b>(sv: &'a Val, name: &'b Lstr) -> Option<(i16, &'a Type)>
{
    let fields = get_named_struct_field(sv, &Lstr::from("fields"))
        .expect("cannot find 'fields' field in structure");
    for f in list::iter(&fields.1) {
        let opt_fld_name = get_named_struct_field(f, &Lstr::from("name"));
        if opt_fld_name.is_none() {
            // this struct has no name field? wtf!
            panic!("field type val has no name field");
        }
        let fld_name_val = &opt_fld_name.unwrap().1;
        if !is_enum_variant(fld_name_val, &Lstr::Sref("Some")) {
            // this is an indexed field, not a named field. skip it.
            continue;
        }
        let some_fld_name = get_indexed_struct_field(fld_name_val, 0)
            .expect("some value is not found");
    }
    None
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


#[cfg(test)]
mod tests {
    use leema::list;
    use leema::lri::{Lri};
    use leema::lstr::{Lstr};
    use leema::types;
    use leema::val::{Val, Type};


#[test]
fn test_type_val()
{
    let tv_lri = Lri::with_modules(Lstr::Sref("tacos"), Lstr::Sref("Burrito"));
    let tv = types::new_type_val(tv_lri, &vec![
        (Some(Lstr::Sref("filling")), Type::Str),
        (Some(Lstr::Sref("rice")), Type::Bool),
    ]);

    let filling = types::get_field_type(&tv, &Lstr::Sref("filling"))
        .expect("cannot find the burrito filling field");
}

}
