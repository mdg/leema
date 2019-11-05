use crate::leema::ast2::{Ast, AstNode, DataType, ModAction, Xlist};
use crate::leema::failure::Lresult;
use crate::leema::grammar2::Grammar;
use crate::leema::loader::Interloader;
use crate::leema::lstr::Lstr;
use crate::leema::module::ModKey;
use crate::leema::struple::{self, Struple2, StrupleItem, StrupleKV};
use crate::leema::token::Tokenz;
use crate::leema::val::{Fref, FuncType, GenericTypes, Type, Val};

use std::collections::{HashMap, HashSet};


const PROTOFAIL: &'static str = "prototype_failure";


/// Asts separated into their types of components
#[derive(Debug)]
pub struct ProtoModule
{
    pub key: ModKey,
    pub imports: HashMap<&'static str, Lstr>,
    pub exports: HashMap<&'static str, Lstr>,
    pub macros: HashMap<&'static str, Ast>,
    pub constants: HashMap<&'static str, AstNode>,
    types: HashMap<&'static str, Type>,
    pub funcseq: Vec<&'static str>,
    pub funcsrc: HashMap<&'static str, (Xlist, AstNode)>,
    pub token: HashSet<&'static str>,
    pub struct_fields: HashMap<&'static str, AstNode>,
}

impl ProtoModule
{
    pub fn new(key: ModKey, src: &'static str) -> Lresult<ProtoModule>
    {
        let items = Grammar::new(Tokenz::lexp(src)?).parse_module()?;

        let mut proto = ProtoModule {
            key,
            imports: HashMap::new(),
            exports: HashMap::new(),
            macros: HashMap::new(),
            constants: HashMap::new(),
            types: HashMap::new(),
            funcseq: Vec::new(),
            funcsrc: HashMap::new(),
            token: HashSet::new(),
            struct_fields: HashMap::new(),
        };

        for i in items {
            match *i.node {
                Ast::DefConst(name, val) => {
                    proto.constants.insert(name, val);
                }
                Ast::DefMacro(macro_name, _, _) => {
                    proto.macros.insert(macro_name, *i.node);
                }
                Ast::DefFunc(name, args, result, body) => {
                    proto.add_func(name, args, result, body)?;
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
                Ast::ModAction(ModAction::Import, tree) => {
                    tree.collect(&mut proto.imports);
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
        result: AstNode,
        body: AstNode,
    ) -> Lresult<()>
    {
        let m = &self.key.name;
        let opens: GenericTypes;
        let type_maker: Box<Fn(FuncType) -> Type>;

        let name_id = match *name.node {
            Ast::Id1(name_id) => {
                opens = vec![];
                type_maker = Box::new(|ft| Type::Func(ft));
                name_id
            }
            Ast::Generic(gen, gen_args) => {
                let opens1: Lresult<GenericTypes> = gen_args.iter()
                    .map(|a| {
                        if let Ast::Id1(var) = *a.v.node {
                            Ok(StrupleItem::new(var, Type::Unknown))
                        } else {
                            Err(rustfail!(
                                PROTOFAIL,
                                "generic arguments must be IDs: {:?}",
                                a,
                            ))
                        }
                    })
                    .collect();
                opens = opens1?;

                type_maker = Box::new(|ft| {
                    Type::Generic(true, Box::new(Type::Func(ft)), opens.clone())
                });

                if let Ast::Id1(name_id) = *gen.node {
                    name_id
                } else {
                    return Err(rustfail!(
                        PROTOFAIL,
                        "invalid generic func name: {:?}",
                        gen,
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
        };

        let ftyp = ast_to_ftype(m, &args, &result, &opens)?;
        let call_args = ftyp.call_args();
        let typ = type_maker(ftyp);
        let fref = Fref::new(m.clone(), name_id, typ.clone());
        let call = Val::Call(fref, call_args);
        let mut fref_ast = AstNode::new_constval(call, name.loc);
        fref_ast.typ = typ;

        self.constants.insert(name_id, fref_ast);
        self.funcseq.push(name_id);
        self.funcsrc.insert(name_id, (args, body));

        Ok(())
    }

    fn add_struct(&mut self, name: AstNode, fields: Xlist) -> Lresult<()>
    {
        let m = &self.key.name;
        let struct_typ: Type;
        let opens: GenericTypes;

        let sname_id = match *name.node {
            Ast::Id1(name_id) => {
                struct_typ = Type::User(m.clone(), name_id);
                opens = vec![];
                name_id
            }
            Ast::Generic(gen, gen_args) => {
                let opens1: Lresult<GenericTypes> = gen_args.iter()
                    .map(|a| {
                        if let Ast::Id1(var) = *a.v.node {
                            Ok(StrupleItem::new(var, Type::Unknown))
                        } else {
                            Err(rustfail!(
                                PROTOFAIL,
                                "generic arguments must be IDs: {:?}",
                                a,
                            ))
                        }
                    })
                    .collect();
                opens = opens1?;

                if let Ast::Id1(name_id) = *gen.node {
                    let inner = Type::User(m.clone(), name_id);
                    struct_typ = Type::Generic(true, Box::new(inner), opens.clone());
                    name_id
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
        };

        let args = xlist_to_types(&m, &fields, &opens)?;
        let ftyp = FuncType::new(args, struct_typ.clone());
        let call_args = ftyp.call_args();
        let constructor_type = Type::Func(ftyp);
        let fref = Fref::new(
            m.clone(),
            sname_id,
            constructor_type,
        );
        let constructor_call = Val::Call(
            fref,
            call_args,
        );
        self.constants.insert(
            sname_id,
            AstNode::new_constval(constructor_call, name.loc),
        );
        self.types.insert(sname_id, struct_typ);
        self.funcseq.push(sname_id);
        // also add to funcsrc and struct_fields
        Ok(())
    }

    fn add_token(&mut self, name: AstNode) -> Lresult<()>
    {
        match *name.node {
            Ast::Id1(name_id) => {
                let t = Type::User(self.key.name.clone(), name_id);
                self.types.insert(name_id, t);
            }
            Ast::Generic(iname, _) => {
                return Err(rustfail!(
                    PROTOFAIL,
                    "tokens cannot be generic: {:?}",
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

    /// TODO rename this. Maybe take_func?
    pub fn pop_func(&mut self, func: &str)
        -> Option<(Xlist, AstNode)>
    {
        let generic = match self.constants.get(func) {
            Some(fconst) => fconst.typ.is_open(),
            None => {
                return None;
            }
        };
        if generic {
            // make a copy for generic functions
            self.funcsrc.get(func).map(|fsrc| fsrc.clone())
        } else {
            // just take the original if not generic
            self.funcsrc.remove(func)
        }
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

pub fn ast_to_type(
    local_mod: &Lstr,
    node: &AstNode,
    opens: &[StrupleItem<&'static str, Type>],
) -> Lresult<Type>
{
    Ok(match &*node.node {
        Ast::Id1("Bool") => Type::BOOL,
        Ast::Id1("Int") => Type::INT,
        Ast::Id1("Str") => Type::STR,
        Ast::Id1("#") => Type::HASHTAG,
        Ast::Id1(id) if struple::contains_key(opens, id) => {
            Type::OpenVar(id)
        }
        Ast::Id1(id) => Type::User(local_mod.clone(), id),
        Ast::Id2(module, id) => Type::User(module.clone(), id),
        Ast::List(inner_items) if inner_items.len() == 1 => {
            let inner = &inner_items.first().unwrap().v;
            let inner_t = ast_to_type(local_mod, inner, opens)?;
            Type::StrictList(Box::new(inner_t))
        }
        Ast::Tuple(inner_items) => {
            let inner_t: Lresult<Vec<StrupleItem<Option<Lstr>, Type>>> = inner_items
                .iter()
                .map(|item| {
                    let k = item.k.map(|ik| Lstr::Sref(ik));
                    let v = ast_to_type(local_mod, &item.v, opens)?;
                    Ok(StrupleItem::new(k, v))
                })
                .collect();
            Type::Tuple(inner_t?)
        }
        Ast::Generic(_, typeargs) => {
            let _gen = struple::map_v(typeargs, |v| ast_to_type(local_mod, v, opens));
            unimplemented!()
        }
        Ast::Void => Type::Void,
        invalid => {
            return Err(rustfail!(
                PROTOFAIL,
                "cannot derive type from: {:?}",
                invalid,
            ));
        }
    })
}

fn ast_to_ftype(
    local_mod: &Lstr,
    args: &Xlist,
    result: &AstNode,
    opens: &[StrupleItem<&'static str, Type>],
) -> Lresult<FuncType>
{
    let arg_types = xlist_to_types(local_mod, args, &opens)?;
    let result_type = ast_to_type(local_mod, &result, opens)?;
    Ok(FuncType::new(arg_types, result_type))
}

fn xlist_to_types(
    local_mod: &Lstr,
    args: &Xlist,
    opens: &[StrupleItem<&'static str, Type>],
) -> Lresult<Struple2<Type>>
{
    let arg_types_r: Lresult<StrupleKV<Option<Lstr>, Type>>;
    arg_types_r = args.into_iter()
        .map(|i| {
            Ok(StrupleItem::new(
                i.k.map(|k| Lstr::Sref(k)),
                ast_to_type(local_mod, &i.v, opens)?,
            ))
        })
        .collect();
    Ok(arg_types_r?)
}

#[derive(Debug)]
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


    /// (head, tail) = path.split()
    /// proto_head = load_canonical(head)
    /// (mid, tail2) = tail.split()
    /// new_path in proto_head.exports[mid]
    /// if new_path.startswith(head+mid) >>
    ///   load(head+mid, tail2)
    /// else if new_path.startswith(head) >>
    ///   load(head, tail
    ///
    /// a/b/d
    /// a exports b: b
    /// b exports d: c/d
    /// c exports d: d
    ///
    /// b cannot export a sibling's locals w/ relative path
    /// a/b/f
    /// a exports b: b
    /// b exports f: /a/e/f
    /// a exports e: e
    /// e exports f: f
    ///
    /// a/b/y/z
    /// a exports b: b
    /// b exports y: /x/y
    /// x exports y: y
    /// y exports z: z
    ///
    /// load_absolute(mod_path)
    /// base_path, postfix = mod_path.split()
    /// base_proto = find_proto(base_path)
    /// load_relative(base_proto, postfix)
    ///
    /// load_sibling(base_proto, mod_path)
    /// sibling_base, postfix = mod_path.split()
    /// base_proto = find_sibling_proto(base_proto, sibling_base)
    /// load_relative(base_proto, postfix)
    ///
    /// load_child(base_proto, child_path)
    /// child_head, child_tail = child_path.split()
    /// child_proto = find_child_proto(base_proto, child_head)
    /// load_relative(child_proto, child_tail)
    ///
    /// load_relative(base_proto, rel_path)
    /// next_prefix, postfix = relative_postfix.split()
    /// next_path = base_proto.exports[next_prefix]
    /// if next_path is relative:
    ///   load_child(base_proto, next_path + postfix)
    /// if next_path is absolute:
    ///   load_absolute(next_path + next_tail)
    pub fn load(
        &mut self,
        loader: &mut Interloader,
        modbase: &Lstr,
        modname: &Lstr,
    ) -> Lresult<()>
    {
        vout!("ProtoLib::load({}, {})\n", modbase, modname);
        let (canonical_name, rem) = self.canonical_name(modbase, modname)?;
        self.load_canonical(loader, &canonical_name)?;
        if let Some(next) = rem {
            self.load(loader, &canonical_name, &next)?;
        }
        Ok(())
    }

    /// in x: import /a/b/d
    /// in a: export b
    /// in b: export c/d
    /// in c: export d
    /// in d: blah
    pub fn canonical_name(
        &self,
        modbase: &Lstr,
        modname: &Lstr,
    ) -> Lresult<(Lstr, Option<Lstr>)>
    {
        vout!("ProtoLib::canonical_name({}, {})\n", modbase, modname);
        let mut modit = modname.splitn(2, "/");
        let mod1 = modit.next().expect("must have at least one module name");
        let rem = modit.next().map(|s| Lstr::from(s.to_string()));

        if modbase == "" {
            return Ok((Lstr::from(mod1.to_string()), rem));
        }
        let canonical = self.get(modbase)?;

        match canonical.exports.get(mod1) {
            Some(export) => {
                Ok((export.clone(), rem))
            }
            None => {
                Err(rustfail!(
                    PROTOFAIL,
                    "module {} is not exported by module {}",
                    mod1,
                    modbase,
                ))
            }
        }
    }

    /// load module by canonical name, w/o checking permissions etc
    fn load_canonical(
        &mut self,
        loader: &mut Interloader,
        modname: &Lstr,
    ) -> Lresult<()>
    {
        vout!("ProtoLib::load_direct({})\n", modname);
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
            for (_, i) in proto.imports.iter() {
                if i == modname {
                    return Err(rustfail!(
                        PROTOFAIL,
                        "a module cannot import itself: {}",
                        i,
                    ));
                }
                if self.protos.contains_key(i) {
                    continue;
                }
                imported.push(i.clone());
            }
        }
        for i in imported.iter() {
            lfailoc!(self.load(loader, &Lstr::EMPTY, i))?;
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
            .map(|protomod| protomod.pop_func(func))
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
    use crate::leema::lstr::Lstr;
    use crate::leema::module::ModKey;
    use crate::leema::struple::{self, StrupleItem};
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
        func swap[:T] a:T b:T / (:T :T)
        >>
            (b, a)
        --
        "#;
        let proto = new_proto(input);
        let tvt = Type::OpenVar("T");

        assert_eq!(1, proto.constants.len());
        assert!(proto.constants.contains_key("swap"));
        assert_eq!(
            Type::Generic(
                true,
                Box::new(Type::Func(FuncType::new(
                    vec![
                        StrupleItem::new(Some(Lstr::Sref("a")), tvt.clone()),
                        StrupleItem::new(Some(Lstr::Sref("b")), tvt.clone()),
                    ],
                    Type::Tuple(struple::new_tuple2(tvt.clone(), tvt.clone())),
                ))),
                vec![StrupleItem::new("T", Type::Unknown)],
            ),
            proto.constants.get("swap").unwrap().typ,
        );

        // function definitions do not create new types
        assert_eq!(0, proto.types.len());
    }

    #[test]
    fn test_proto_generic_struct()
    {
        let proto = new_proto("type Point[:T] x:T y:T --");

        let point_type = proto.types.get("Point").expect("no Point type");
        let expected = Type::Generic(
            true,
            Box::new(Type::User(Lstr::Sref("foo"), "Point")),
            vec![StrupleItem::new("T", Type::Unknown)],
        );
        assert_eq!(expected, *point_type);
    }

    #[test]
    fn test_proto_token()
    {
        let proto = new_proto("type Burrito --");

        let burrito_type = proto.types.get("Burrito").expect("no Burrito type");
        assert_eq!(
            Type::User(Lstr::from("foo"), "Burrito"),
            *burrito_type,
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
