use crate::leema::ast2::{self, Ast, AstNode, DataType, Xlist};
use crate::leema::failure::Lresult;
use crate::leema::grammar2::Grammar;
use crate::leema::loader::Interloader;
use crate::leema::lri::Lri;
use crate::leema::lstr::Lstr;
use crate::leema::module::ModKey;
use crate::leema::struple::{Struple2, StrupleItem, StrupleKV};
use crate::leema::token::Tokenz;
use crate::leema::val::{FuncType, Type, Val};

use std::collections::{HashMap, HashSet};


const PROTOFAIL: &'static str = "prototype_failure";


/// Asts separated into their types of components
#[derive(Debug)]
pub struct ProtoModule
{
    pub key: ModKey,
    pub imports: HashSet<&'static str>,
    pub macros: HashMap<&'static str, Ast>,
    pub constants: HashMap<&'static str, AstNode>,
    types: HashMap<&'static str, Type>,
    pub funcseq: Vec<&'static str>,
    pub funcsrc: HashMap<&'static str, (Xlist, AstNode)>,
    pub token: HashSet<&'static str>,
    pub struct_fields: HashMap<&'static str, AstNode>,
    pub genfunc: HashMap<&'static str, (Xlist, Xlist, AstNode)>,
    pub gentype: HashMap<&'static str, Xlist>,
}

impl ProtoModule
{
    pub fn new(key: ModKey, src: &'static str) -> Lresult<ProtoModule>
    {
        let items = Grammar::new(Tokenz::lexp(src)?).parse_module()?;

        let mut proto = ProtoModule {
            key,
            imports: HashSet::new(),
            macros: HashMap::new(),
            constants: HashMap::new(),
            types: HashMap::new(),
            funcseq: Vec::new(),
            funcsrc: HashMap::new(),
            token: HashSet::new(),
            struct_fields: HashMap::new(),
            genfunc: HashMap::new(),
            gentype: HashMap::new(),
        };

        for i in items {
            match *i.node {
                Ast::DefConst(name, val) => {
                    proto.constants.insert(name, val);
                }
                Ast::DefMacro(macro_name, _, _) => {
                    proto.macros.insert(macro_name, *i.node);
                }
                Ast::DefFunc(name, args, body) => {
                    proto.add_func(name, args, body)?;
                }
                Ast::DefType(DataType::Struct, name, fields) => {
                    if fields.is_empty() {
                        proto.add_token(name)?;
                    } else {
                        proto.add_struct(name, fields)?;
                    }
                }
                Ast::DefType(DataType::Union, name, variants) => {
                    proto.add_union(name, variants)?;
                }
                Ast::Import(imp) => {
                    proto.imports.insert(imp);
                }
                _ => {
                    return Err(rustfail!(
                        PROTOFAIL,
                        "expected module statement, found {:?}",
                        i,
                    ));
                }
            }
        }
        Ok(proto)
    }

    fn add_func(
        &mut self,
        name: AstNode,
        args: Xlist,
        body: AstNode,
    ) -> Lresult<()>
    {
        match *name.node {
            Ast::Id1(name_id) => {
                self.add_func_not_generic(name_id, args, body, name.loc)?;
            }
            Ast::Generic(name, gen_args) => {
                if let Ast::Id1(name_id) = *name.node {
                    self.add_generic_func(name_id, gen_args, args, body, name.loc)?;
                } else {
                    return Err(rustfail!(
                        PROTOFAIL,
                        "unsupported generic func name: {:?}",
                        name,
                    ));
                }
            }
            invalid_name => {
                return Err(rustfail!(
                    PROTOFAIL,
                    "unsupported func name: {:?}",
                    invalid_name,
                ));
            }
        }
        Ok(())
    }

    fn add_func_not_generic(
        &mut self,
        name: &'static str,
        args: Xlist,
        body: AstNode,
        loc: ast2::Loc,
    ) -> Lresult<()>
    {
        let args2 = args.clone();
        // not generic so everything is a closed type
        let ftyp = if !args.is_empty() {
            let empty_type_args: &[&'static str] = &[];
            let arg_types_r: Lresult<Vec<StrupleItem<Option<Lstr>, Type>>>;
            arg_types_r = args
                .0
                .into_iter()
                .map(|i| {
                    Ok(StrupleItem::new(
                        i.k.map(|k| Lstr::Sref(k)),
                        ast_to_type(&self.key.name, &i.v, empty_type_args)?,
                    ))
                })
                .collect();
            let mut arg_types_vec = arg_types_r?;
            let mut result_type_vec =
                arg_types_vec.split_off(arg_types_vec.len() - 1);
            let result_type = result_type_vec.pop().unwrap().v;
            let arg_types = StrupleKV::from(arg_types_vec);
            FuncType::new(arg_types, result_type)
        } else {
            FuncType::new(StrupleKV::new(), Type::Void)
        };
        let fref_args = ftyp.args.map_v(|_| Ok(Val::Void))?;
        self.funcseq.push(name);
        // is this args2 param necessary anymore? the type should be enough
        self.funcsrc.insert(name, (args2, body));
        let fref = Val::Fref(
            self.key.name.clone(),
            name,
            fref_args,
            Type::Func(ftyp.clone()),
        );
        let fref_ast = AstNode::new_constval(fref, loc);
        self.constants.insert(name, fref_ast);
        self.types.insert(name, Type::Func(ftyp));
        Ok(())
    }

    fn add_generic_func(
        &mut self,
        name: &'static str,
        typeargs: Xlist,
        args: Xlist,
        body: AstNode,
        loc: ast2::Loc,
    ) -> Lresult<()>
    {
        if typeargs.is_empty() {
            return Err(rustfail!(
                PROTOFAIL,
                "generic type args cannot be empty for func: {} at line {}",
                name,
                loc.lineno,
            ));
        }
        self.genfunc.insert(name, (typeargs.clone(), args.clone(), body));

        let open_result: Lresult<Vec<&'static str>> = typeargs.0.into_iter().map(|ta| {
            if let Ast::Id1(typeargname) = *ta.v.node {
                Ok(typeargname)
            } else {
                Err(rustfail!(
                    PROTOFAIL,
                    "type arg for {} is not an id: {:?}",
                    name,
                    ta,
                ))
            }
        }).collect();
        let opens: Vec<&'static str> = open_result?;

        let ftyp = if !args.is_empty() {
            let arg_types_r: Lresult<Vec<StrupleItem<Option<Lstr>, Type>>>;
            arg_types_r = args
                .0
                .into_iter()
                .map(|i| {
                    Ok(StrupleItem::new(
                        i.k.map(|k| Lstr::Sref(k)),
                        ast_to_type(&self.key.name, &i.v, &opens)?,
                    ))
                })
                .collect();
            let mut arg_types_vec = arg_types_r?;
            let mut result_type_vec =
                arg_types_vec.split_off(arg_types_vec.len() - 1);
            let result_type = result_type_vec.pop().unwrap().v;
            let arg_types = StrupleKV::from(arg_types_vec);
            FuncType::new(arg_types, result_type)
        } else {
            FuncType::new(StrupleKV::new(), Type::Void)
        };

        let fref_args = ftyp.args.map_v(|_| Ok(Val::Void))?;
        let genfunctype = Type::Open(opens, Box::new(Type::Func(ftyp)));

        let fref = Val::Fref(
            self.key.name.clone(),
            name,
            fref_args,
            genfunctype.clone(),
        );
        let fref_ast = AstNode::new_constval(fref, loc);
        self.constants.insert(name, fref_ast);

        self.types.insert(name, genfunctype);

        Ok(())
    }

    fn add_struct(&mut self, name: AstNode, _fields: Xlist) -> Lresult<()>
    {
        match *name.node {
            Ast::Id1(name_id) => {
                let typ = Type::User(self.key.name.clone(), name_id);
                self.types.insert(name_id, typ.clone());
                let constructor_ref = Val::Fref(
                    self.key.name.clone(),
                    name_id,
                    Struple2::new(),
                    typ,
                );
                self.constants.insert(
                    name_id,
                    AstNode::new_constval(constructor_ref, name.loc),
                );
                // do something with fields too!
            }
            Ast::Generic(gen, gen_args) => {
                if let Ast::Id1(name_id) = *gen.node {
                    let inner = Type::User(self.key.name.clone(), name_id);
                    let mut gen_vars = vec![];
                    for a in gen_args.into_iter() {
                        if let Ast::Id1(var) = *a.v.node {
                            gen_vars.push(var);
                        } else {
                            return Err(rustfail!(
                                PROTOFAIL,
                                "generic arguments must be IDs: {:?}",
                                a,
                            ));
                        }
                    }
                    let open = Type::Open(gen_vars, Box::new(inner));
                    self.types.insert(name_id, open);
                } else {
                    return Err(rustfail!(
                        PROTOFAIL,
                        "invalid generic struct name: {:?}",
                        gen,
                    ));
                }
            }
            invalid_name => {
                return Err(rustfail!(
                    PROTOFAIL,
                    "invalid struct name: {:?}",
                    invalid_name,
                ));
            }
        }
        Ok(())
    }

    fn add_token(&mut self, name: AstNode) -> Lresult<()>
    {
        match *name.node {
            Ast::Id1(name_id) => {
                let lri = Lri::new(Lstr::from(name_id));
                self.types.insert(name_id, Type::UserDef(lri));
            }
            Ast::Generic(iname, _) => {
                return Err(rustfail!(
                    PROTOFAIL,
                    "token cannot be generic: {:?}",
                    iname,
                ));
            }
            invalid_name => {
                return Err(rustfail!(
                    PROTOFAIL,
                    "invalid token name: {:?}",
                    invalid_name,
                ));
            }
        }
        Ok(())
    }

    fn add_union(&mut self, name: AstNode, variants: Xlist) -> Lresult<()>
    {
        if variants.is_empty() {
            return Err(rustfail!(
                PROTOFAIL,
                "union must have at least variant variant: {:?}",
                name,
            ));
        }
        // proto.types.push(i);
        Ok(())
    }

    pub fn pop_func(&mut self, func: &str)
        -> Lresult<Option<(Xlist, AstNode)>>
    {
        Ok(self.funcsrc.remove(func))
    }

    pub fn find_macro(&self, macroname: &str) -> Option<&Ast>
    {
        self.macros.get(macroname)
    }

    pub fn find_const(&self, name: &str) -> Option<&AstNode>
    {
        self.constants.get(name)
    }

    pub fn get_type(&self, name: &str) -> Lresult<&Type>
    {
        self.types
            .get(name)
            .ok_or_else(|| rustfail!(PROTOFAIL, "type not found: {}", name))
    }
}

fn ast_to_type(
    local_mod: &Lstr,
    node: &AstNode,
    opens: &[&'static str],
) -> Lresult<Type>
{
    Ok(match &*node.node {
        Ast::Id1("Bool") => Type::Bool,
        Ast::Id1("Int") => Type::Int,
        Ast::Id1("Str") => Type::Str,
        Ast::Id1("#") => Type::Hashtag,
        Ast::Id1(id) if opens.contains(&id) => Type::Var(Lstr::Sref(id)),
        Ast::Id1(id) => Type::User(local_mod.clone(), id),
        Ast::Id2(module, id) => Type::User(module.clone(), id),
        Ast::List(inner_items) if inner_items.len() == 1 => {
            let inner = &inner_items.0.first().unwrap().v;
            let inner_t = ast_to_type(local_mod, inner, opens)?;
            Type::StrictList(Box::new(inner_t))
        }
        Ast::Tuple(inner_items) => {
            let inner_t: Lresult<Vec<StrupleItem<Option<Lstr>, Type>>> = inner_items.0
                .iter()
                .map(|item| {
                    let k = item.k.map(|ik| Lstr::Sref(ik));
                    let v = ast_to_type(local_mod, &item.v, opens)?;
                    Ok(StrupleItem::new(k, v))
                })
                .collect();
            Type::Tuple(StrupleKV(inner_t?))
        }
        Ast::Generic(_, typeargs) => {
            let _gen = typeargs.map_v(|v| ast_to_type(local_mod, v, opens));
            unimplemented!()
        }
        invalid => {
            return Err(rustfail!(
                PROTOFAIL,
                "cannot derive type from: {:?}",
                invalid,
            ));
        }
    })
}

pub struct ProtoLib
{
    protos: HashMap<Lstr, ProtoModule>,
}

impl ProtoLib
{
    pub fn new() -> ProtoLib
    {
        ProtoLib {
            protos: HashMap::new(),
        }
    }

    pub fn add_module(
        &mut self,
        modname: &Lstr,
        src: &'static str,
    ) -> Lresult<()>
    {
        vout!("ProtoLib::add_module({})\n", modname);
        if self.protos.contains_key(modname) {
            return Err(rustfail!(
                PROTOFAIL,
                "cannot load a module twice: {}",
                modname,
            ));
        }
        let modkey = ModKey::name_only(modname.clone());
        let proto = ProtoModule::new(modkey, src)?;
        self.protos.insert(modname.clone(), proto);
        Ok(())
    }

    pub fn load(
        &mut self,
        loader: &mut Interloader,
        modname: &Lstr,
    ) -> Lresult<()>
    {
        vout!("ProtoLib::load({})\n", modname);
        if self.protos.contains_key(modname) {
            return Ok(());
        }
        let modtxt = ltry!(loader.read_mod(modname));
        let modkey = ModKey::name_only(modname.clone());
        let proto = ProtoModule::new(modkey, modtxt)?;
        self.protos.insert(modname.clone(), proto);
        Ok(())
    }

    pub fn load_imports(
        &mut self,
        loader: &mut Interloader,
        modname: &Lstr,
    ) -> Lresult<()>
    {
        vout!("ProtoLib::load_imports({})\n", modname);
        let mut imported: Vec<Lstr> = vec![];
        {
            let proto = self.protos.get(modname).ok_or_else(|| {
                rustfail!(
                    PROTOFAIL,
                    "an import module does not exist: {}",
                    modname,
                )
            })?;
            for i in proto.imports.iter() {
                if i == &modname {
                    return Err(rustfail!(
                        PROTOFAIL,
                        "a module cannot import itself: {}",
                        i,
                    ));
                }
                if self.protos.contains_key(*i) {
                    continue;
                }
                imported.push(Lstr::from(String::from(*i)));
            }
        }
        for i in imported.iter() {
            lfailoc!(self.load(loader, i))?;
        }
        Ok(())
    }

    pub fn pop_func(
        &mut self,
        module: &str,
        func: &str,
    ) -> Lresult<Option<(Xlist, AstNode)>>
    {
        self.protos
            .get_mut(module)
            .ok_or_else(|| {
                rustfail!(PROTOFAIL, "could not find module: {}", module,)
            })
            .and_then(|protomod| protomod.pop_func(func))
    }

    pub fn get(&self, modname: &str) -> Lresult<&ProtoModule>
    {
        self.protos.get(modname).ok_or_else(|| {
            rustfail!(PROTOFAIL, "module not loaded: {}", modname,)
        })
    }
}


#[cfg(test)]
mod tests
{
    use super::ProtoModule;
    use crate::leema::lri::Lri;
    use crate::leema::lstr::Lstr;
    use crate::leema::module::ModKey;
    use crate::leema::struple::{Struple2, StrupleItem, StrupleKV};
    use crate::leema::val::{FuncType, Type};

    fn new_proto(input: &'static str) -> ProtoModule
    {
        let key = ModKey::name_only(Lstr::Sref("foo"));
        ProtoModule::new(key, input).expect("ProtoModule load failure")
    }

    #[test]
    fn test_proto_func_noargs()
    {
        let proto = new_proto(r#"func hello >> "world" --"#);

        let funcseq = proto.funcseq.get(0).expect("no funcseq type");
        assert_eq!("hello", *funcseq);
        assert_eq!(1, proto.funcseq.len());
        assert!(proto.funcsrc.contains_key("hello"));
        assert_eq!(1, proto.funcsrc.len());
    }

    #[test]
    fn test_proto_genericfunc()
    {
        let input = r#"
        func swap[:T] a:T b:T :(:T :T)
        >>
            (b, a)
        --
        "#;
        let proto = new_proto(input);
        let tvt = Type::Var(Lstr::Sref("T"));

        assert_eq!(1, proto.genfunc.len());
        assert!(proto.genfunc.contains_key("swap"));

        assert_eq!(1, proto.constants.len());
        assert!(proto.constants.contains_key("swap"));

        assert_eq!(1, proto.types.len());
        assert!(proto.types.contains_key("swap"));
        assert_eq!(
            Type::Open(
                vec!["T"],
                Box::new(Type::Func(FuncType::new(
                    StrupleKV::from_vec(vec![
                        StrupleItem::new(Some(Lstr::Sref("a")), tvt.clone()),
                        StrupleItem::new(Some(Lstr::Sref("b")), tvt.clone()),
                    ]),
                    Type::Tuple(Struple2::new_tuple2(tvt.clone(), tvt.clone())),
                ))),
            ),
            *proto.types.get("swap").unwrap(),
        );
    }

    #[test]
    fn test_proto_generic_struct()
    {
        let proto = new_proto("type Point[:T] x:T y:T --");

        let point_type = proto.types.get("Point").expect("no Point type");
        let expected = Type::Open(
            vec!["T"],
            Box::new(Type::User(Lstr::Sref("foo"), "Point")),
        );
        assert_eq!(expected, *point_type);
    }

    #[test]
    fn test_proto_token()
    {
        let proto = new_proto("type Burrito --");

        let burrito_type = proto.types.get("Burrito").expect("no Burrito type");
        assert_eq!(
            Type::UserDef(Lri::new(Lstr::from("Burrito"))),
            *burrito_type
        );
    }

    #[test]
    fn test_proto_struct()
    {
        let proto = new_proto("type Point x:Int y:Int --");

        let point_type = proto.types.get("Point").expect("no Point type");
        assert_eq!(Type::User(Lstr::Sref("foo"), "Point"), *point_type);
    }
}
