/// First pass at a module
/// Extract any macros, types and function declarations
/// All processing is local to the module
///
/// A ProtoMod is a thing that can hold functions.
/// It can be a:
///   - file module
///   - trait
///   - struct
///   - enum
///   - protocol
///   - implemented trait or protocol
///     maybe implementations get put back w/ their
///     original traits or protocols?
///
/// data or trait functions get put in their own module, not the parent
///
/// trait
///   struct
///
/// trait
///   enum
///     struct
///     struct
///
/// enum
///   struct
///   token
///
/// token
///
/// trait
///
use crate::leema::ast2::{
    Ast, AstNode, DataType, Loc, ModAction, ModTree, Xlist,
};
use crate::leema::canonical::Canonical;
use crate::leema::failure::{self, Failure, Lresult};
use crate::leema::loader::Interloader;
use crate::leema::lstr::Lstr;
use crate::leema::module::{
    ImportedMod, ModAlias, ModKey, ModRelativity, ModTyp,
};
use crate::leema::parser::parse_file;
use crate::leema::struple::{self, Struple2, StrupleItem, StrupleKV};
use crate::leema::val::{Fref, FuncType, GenericTypes, Type, TypeSrc, Val};

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use lazy_static::lazy_static;


// special module field names
// Func
pub const MODNAME_CONSTRUCT: &'static str = "__construct";
// Type
pub const MODNAME_DATATYPE: &'static str = "__datatype";
// Bool
pub const MODNAME_EXPORTALL: &'static str = "__exportall";
// Struple2<Type>
pub const MODNAME_FIELDS: &'static str = "__fields";
// Str
pub const MODNAME_FILE: &'static str = "__file";
// ModTyp Enum
pub const MODNAME_MODTYP: &'static str = "__modtyp";
// Str
pub const MODNAME_NAME: &'static str = "__name";
// Struple2<Type>
pub const MODNAME_VARIANTS: &'static str = "__variants";

const NUM_MODNAMES: usize = 7;

const PROTOFAIL: &'static str = "prototype_failure";

lazy_static! {
    static ref DEFAULT_IDS: HashMap<&'static str, Canonical> = {
        let core_mod = canonical!("/core");
        let mut ids = HashMap::new();
        ids.insert("Bool", core_mod.clone());
        ids.insert("boolean_and", core_mod.clone());
        ids.insert("boolean_not", core_mod.clone());
        ids.insert("boolean_or", core_mod.clone());
        ids.insert("cons", core_mod.clone());
        ids.insert("create_failure", core_mod.clone());
        ids.insert("fail", core_mod.clone());
        ids.insert("False", core_mod.clone());
        ids.insert("Hashtag", core_mod.clone());
        ids.insert("Int", core_mod.clone());
        ids.insert("int_add", core_mod.clone());
        ids.insert("int_sub", core_mod.clone());
        ids.insert("int_mult", core_mod.clone());
        ids.insert("int_div", core_mod.clone());
        ids.insert("int_mod", core_mod.clone());
        ids.insert("int_negate", core_mod.clone());
        ids.insert("int_equal", core_mod.clone());
        ids.insert("int_less_than", core_mod.clone());
        ids.insert("int_lteq", core_mod.clone());
        ids.insert("int_gt", core_mod.clone());
        ids.insert("int_gteq", core_mod.clone());
        ids.insert("new_struct_val", core_mod.clone());
        ids.insert("not_equal", core_mod.clone());
        ids.insert("None", core_mod.clone());
        ids.insert("Option", core_mod.clone());
        ids.insert("Some", core_mod.clone());
        ids.insert("Str", core_mod.clone());
        ids.insert("True", core_mod.clone());
        ids.insert("void", core_mod.clone());
        ids.insert("Void", core_mod.clone());
        ids.insert("#", core_mod);
        ids
    };

    static ref DEFAULT_TYPES: HashMap<&'static str, Type> = {
        let mut ids = HashMap::new();
        ids.insert("Bool", Type::BOOL);
        ids.insert("Int", Type::INT);
        ids.insert("Option", Type::option(None));
        ids.insert("Str", Type::STR);
        ids.insert("Void", Type::VOID);
        ids.insert("#", Type::HASHTAG);
        ids
    };
}

const STATIC_INDEX_NAMES: [&'static str; 17] = [
    "statixName0",
    "statixName1",
    "statixName2",
    "statixName3",
    "statixName4",
    "statixName5",
    "statixName6",
    "statixName7",
    "statixName8",
    "statixName9",
    "statixName10",
    "statixName11",
    "statixName12",
    "statixName13",
    "statixName14",
    "statixName15",
    "statixName16",
];

#[derive(Clone)]
#[derive(Debug)]
struct ProtoType
{
    n: &'static str,
    t: Type,
    g: GenericTypes,
}

pub type CanonicalTypeSrc = HashMap<Canonical, TypeSrc>;

/// Asts separated into their types of components
#[derive(Debug)]
pub struct ProtoModule
{
    pub key: ModKey,
    typ: Option<ProtoType>,
    pub imports: HashMap<&'static str, Canonical>,
    pub exports_all: Option<bool>,
    pub funcseq: Vec<&'static str>,
    pub funcsrc: HashMap<&'static str, (Xlist, AstNode)>,
    submods: HashMap<&'static str, ProtoModule>,
    modscope: HashMap<&'static str, AstNode>,
    localdef: HashSet<&'static str>,
}

impl ProtoModule
{
    pub fn new(key: ModKey, src: &'static str) -> Lresult<ProtoModule>
    {
        let items = parse_file(src)?;
        Self::with_ast(key, None, items)
    }

    fn new_submod(key: ModKey, parent: ProtoType, items: Vec<AstNode>) -> Lresult<ProtoModule>
    {
        Self::with_ast(key, Some(parent), items)
    }

    fn with_ast(key: ModKey, parent: Option<ProtoType>, items: Vec<AstNode>) -> Lresult<ProtoModule>
    {
        let modname = key.name.clone();

        let mut proto = ProtoModule {
            key,
            typ: parent,
            imports: HashMap::new(),
            exports_all: None,
            funcseq: Vec::new(),
            funcsrc: HashMap::new(),
            submods: HashMap::new(),
            modscope: HashMap::new(),
            localdef: HashSet::new(),
        };

        let mut exports = HashMap::new();
        let mut imports = HashMap::new();
        let mut it = items.into_iter().peekable();
        while it.peek().is_some() {
            match &*it.peek().unwrap().node {
                Ast::ModAction(_, _) => {
                    // an import, take the next
                }
                _ => {
                    // no more imports or exports. move on to the regular stuff
                    break;
                }
            }

            let i = it.next().unwrap();
            match *i.node {
                Ast::ModAction(ModAction::Import, tree) => {
                    tree.collect(&mut imports)?;
                }
                Ast::ModAction(ModAction::Export, ModTree::All(loc)) => {
                    if !proto.exports_all.is_none() {
                        return Err(Failure::static_leema(
                            failure::Mode::CompileFailure,
                            Lstr::Sref("cannot mix exporting * and specific modules"),
                            modname.0.clone(),
                            loc.lineno,
                        ));
                    }
                    proto.exports_all = Some(true);
                }
                Ast::ModAction(ModAction::Export, tree) => {
                    if proto.exports_all == Some(true) {
                        return Err(Failure::static_leema(
                            failure::Mode::CompileFailure,
                            Lstr::Sref("cannot mix export * and specific"),
                            modname.0.clone(),
                            i.loc.lineno,
                        ));
                    }
                    tree.collect(&mut exports)?;
                }
                what => {
                    return Err(rustfail!(
                        PROTOFAIL,
                        "not an import or export: {:?}",
                        what,
                    ));
                }
            }
        }

        // make sure imports don't overlap w/ anything defined
        for (k, (v, loc)) in imports.into_iter() {
            proto.refute_redefines_default(k, loc)?;

            if proto.imports.contains_key(k) {
                return Err(Failure::static_leema(
                    failure::Mode::CompileFailure,
                    lstrf!("duplicate import: {}", v),
                    proto.key.best_path(),
                    loc.lineno,
                ));
            }

            proto.add_import(k, v)?;
            // note this import as local and not accessible from parents
            proto.localdef.insert(k);
        }

        for (k, (v, loc)) in exports.into_iter() {
            // make sure that only child modules are exported
            // siblings and absolute modules cannot be exported (for now)
            if v.relativity() != ModRelativity::Child {
                return Err(Failure::static_leema(
                    failure::Mode::CompileFailure,
                    lstrf!("only child modules can be exported: {}", v),
                    proto.key.best_path(),
                    loc.lineno,
                ));
            }

            proto.add_import(k, v.clone())?;
            // let this imported item be accessible from parents
        }

        for i in it {
            ltry!(proto.add_definition(i));
        }
        Ok(proto)
    }

    fn append_ast(&mut self, items: Vec<AstNode>) -> Lresult<()>
    {
        for i in items.into_iter() {
            ltry!(self.add_definition(i));
        }
        Ok(())
    }

    fn add_import(
        &mut self,
        name: &'static str,
        imp: ImportedMod,
    ) -> Lresult<()>
    {
        // if imp is absolute, canonical will just be imp
        let canonical = self.key.name.push(&imp);
        self.imports.insert(name, canonical);
        Ok(())
    }

    pub fn add_definition(&mut self, node: AstNode) -> Lresult<()>
    {
        let loc = node.loc;
        let result = match *node.node {
            Ast::DefConst(name, val) => {
                self.modscope.insert(name, val);
                lfailoc!(self.refute_redefines_default(name, loc))
            }
            Ast::DefMacro(macro_name, _, _) => {
                self.modscope.insert(macro_name, node);
                lfailoc!(self.refute_redefines_default(macro_name, loc))
            }
            Ast::DefFunc(name, args, result, body) => {
                lfailoc!(self.add_func(name, args, result, body))
            }
            Ast::DefTrait(name, funcs) => {
                lfailoc!(self.add_trait(name, funcs))
            }
            Ast::DefImpl(iface, typ, funcs) => {
                lfailoc!(self.impl_trait(typ, iface, funcs))
            }
            Ast::DefType(DataType::Union, name, variants) => {
                lfailoc!(self.add_union(name, variants))
            }
            Ast::DefType(DataType::Struct, name, fields) => {
                if fields.is_empty() {
                    lfailoc!(self.add_token(name))
                } else {
                    lfailoc!(self.add_struct(name, fields))
                }
            }
            Ast::DefType(DataType::Alias, name, mut src_vec) => {
                if src_vec.is_empty() {
                    Err(rustfail!(
                        "parse_failure",
                        "expected source type, found none",
                    ))
                } else {
                    let src = src_vec.drain(0..1).next().unwrap().v;
                    lfailoc!(self.add_alias_type(name, src))
                }
            }
            Ast::DefType(DataType::Rust, name, _) => {
                lfailoc!(self.add_rust_type(name))
            }
            Ast::ModAction(_, _) => {
                Err(Failure::static_leema(
                    failure::Mode::CompileFailure,
                    Lstr::Sref("imports must come before definitions"),
                    self.key.best_path(),
                    node.loc.lineno,
                ))
            }
            _ => {
                Err(Failure::static_leema(
                    failure::Mode::CompileFailure,
                    lstrf!("expected module statement, found {:?}", node),
                    self.key.best_path(),
                    node.loc.lineno,
                ))
            }
        };

        result.map_err(|e| {
            e.lstr_loc(
                Lstr::from(self.key.best_path_ref().to_string()),
                loc.lineno as u32,
            )
        })
    }

    fn refute_redefines_default(
        &self,
        id: &str,
        loc: Loc,
    ) -> Lresult<()>
    {
        if !self.key.name.is_core() && DEFAULT_IDS.contains_key(id) {
            Err(Failure::static_leema(
                failure::Mode::CompileFailure,
                lstrf!("cannot redefine core {}", id),
                self.key.best_path(),
                loc.lineno,
            ))
        } else {
            Ok(())
        }
    }

    fn add_func(
        &mut self,
        name: AstNode,
        mut args: Xlist,
        result: AstNode,
        body: AstNode,
    ) -> Lresult<()>
    {
        let loc = name.loc;
        if args.len() > 0 {
            let first = args.get_mut(0).unwrap();
            if first.k.is_none() && *first.v.node == Ast::Id("self") {
                first.k = Some("self");
                *first.v.node = Ast::Id("Self");
            }
        }

        let opens = self.typ.as_ref().map(|o| o.g.clone()).unwrap_or(vec![]);
        let (name_id, ft, ftyp) =
            ltry!(self.make_func_type(name, &args, result, opens));

        let call_args = ft.call_args();
        let fref = Fref::new(self.key.clone(), name_id, ftyp.clone());
        let call = Val::Call(fref, call_args);
        let mut fref_ast = AstNode::new_constval(call, loc);
        fref_ast.typ = ftyp;

        self.funcseq.push(name_id);
        self.funcsrc.insert(name_id, (args, body));
        self.modscope.insert(name_id, fref_ast);

        Ok(())
    }

    fn add_struct(&mut self, name: AstNode, fields: Xlist) -> Lresult<()>
    {
        let loc = name.loc;
        if self.typ.is_some() {
            if !name.node.is_void() {
                return Err(rustfail!(
                    "semantic_error",
                    "cannot define new struct within trait: {:?} at {:?}",
                    name,
                    loc,
                ));
            }
            let parent = self.typ.as_ref().unwrap().clone();
            self.add_typed_struct(parent, fields, loc)
        } else {
            if name.node.is_void() {
                return Err(rustfail!(
                    "semantic_error",
                    "cannot define unnamed struct outside of trait: {:?}",
                    loc,
                ));
            }
            let proto_t = self.make_proto_type(name)?;
            self.add_typed_struct(proto_t, fields, loc)
        }
    }

    fn add_typed_struct(
        &mut self,
        proto: ProtoType,
        fields: Xlist,
        loc: Loc,
    ) -> Lresult<()>
    {
        let typ = proto.t.clone();

        let macro_call = AstNode::new(Ast::Id("new_struct_val"), loc);
        let fields_arg: Xlist = fields
            .iter()
            .enumerate()
            .map(|(i, f)| {
                let k = f.k.unwrap_or(STATIC_INDEX_NAMES[i]);
                StrupleItem::new(f.k, AstNode::new(Ast::Id(k), loc))
            })
            .collect();
        let macro_args = vec![
            StrupleItem::new_v(AstNode::new(
                Ast::ConstVal(Val::Type(typ.clone())),
                loc,
            )),
            StrupleItem::new_v(AstNode::new(
                Ast::ConstVal(Val::Int(fields_arg.len() as i64)),
                loc,
            )),
            StrupleItem::new_v(AstNode::new(Ast::Tuple(fields_arg), loc)),
        ];

        let construction = AstNode::new(Ast::Call(macro_call, macro_args), loc);
        let mut type_node = AstNode::new(Ast::Type(typ.clone()), loc);
        type_node.typ = Type::Kind;
        let constructor_ast = AstNode::new(
            Ast::DefFunc(
                AstNode::new(Ast::Id(MODNAME_CONSTRUCT), loc),
                fields.clone(),
                AstNode::new(Ast::Type(typ.clone()), loc),
                construction.clone(),
            ),
            loc,
        );

        if self.typ.is_some() {
            self.append_ast(vec![constructor_ast])?;
        } else {
            self.add_submod(ModTyp::Data, proto, vec![constructor_ast])?;
            self.modscope.insert(MODNAME_DATATYPE, type_node);
            // TODO add this definition to the struct module instead
            // TODO the fields too
        }
        Ok(())
    }

    fn new_struct_src(
        name: &'static str,
        _struct_typ: Type,
        fld_names: &[&'static str],
    ) -> Lresult<AstNode>
    {
        let args = fld_names.join(", ");
        let _constructor = format!(
            r#"
        let _new_struct_ = {}(Void)
        _new_struct_({})
        "#,
            name, args
        );
        // parse_file(
        /*
        let void_args = Vec::with_capacity(args.len());
        void_args.resize(args.len(), Val::Void);
        let mut block = vec![
            AstNode::new(Ast::ConstVal(Val::Struct(
                struct_typ.clone(),
                Vec::with_capacity(args.len()),
            )), loc),
        ];
        block.push(AstNode::new(Ast::Call(new_id, fld_names), loc));
        let block_node = AstNode::new(Ast::Block(block), loc);
        let constructor_src = new_struct_src(sname_id, args);
        */
        Ok(AstNode::void())
    }

    fn add_token(&mut self, name: AstNode) -> Lresult<()>
    {
        match *name.node {
            Ast::Id(name_id) => {
                ltry!(self.refute_redefines_default(name_id, name.loc));
                let tok_mod = self.key.name.clone();
                let t = Type::User(tok_mod.push(&name_id));
                let token_val = Val::Token(t.clone());
                let const_node =
                    AstNode::new_constval(token_val.clone(), name.loc);
                self.modscope.insert(name_id, const_node);
            }
            Ast::Generic(iname, _) => {
                return Err(Failure::static_leema(
                    failure::Mode::CompileFailure,
                    lstrf!("tokens cannot be generic: {:?}", iname),
                    self.key.best_path(),
                    name.loc.lineno,
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

    fn add_alias_type(&mut self, name: AstNode, src: AstNode) -> Lresult<()>
    {
        let loc = name.loc;
        let ProtoType{n: name_id, ..} = self.make_proto_type(name)?;
        let srct = ltry!(self.ast_to_type(&self.key.name, &src, &[]));
        let typeval = Val::Type(srct.clone());
        let mut node = AstNode::new_constval(typeval, loc);
        node.typ = Type::Kind;
        let alias_generics = vec![]; // add generics later
        let alias_node = AstNode::new(Ast::Alias(alias_generics, Box::new(node)), loc);
        self.modscope.insert(name_id, alias_node);
        Ok(())
    }

    fn add_rust_type(&mut self, name: AstNode) -> Lresult<()>
    {
        let loc = name.loc;
        let proto_t = self.make_proto_type(name)?;
        let ProtoType{n: name_id, t, ..} = &proto_t;
        let typeval = Val::Type(t.clone());
        let mut node = AstNode::new_constval(typeval, loc);
        node.typ = Type::Kind;
        let subkey = self.key.submod(ModTyp::Data, name_id);
        let mut sub = ltry!(Self::new_submod(subkey, proto_t.clone(), vec![]));
        // TODO: add __datatype or whatever
        sub.modscope.insert(MODNAME_DATATYPE, node);
        self.submods.insert(name_id, sub);
        Ok(())
    }

    fn add_union(&mut self, name: AstNode, variants: Xlist) -> Lresult<()>
    {
        let loc = name.loc;
        if variants.len() < 2 {
            return Err(rustfail!(
                PROTOFAIL,
                "unions must have at least two variants: {:?}",
                name,
            ));
        }
        let proto = self.make_proto_type(name)?;
        let sname_id = proto.n;
        let union_typ = proto.t.clone();

        for var in variants.into_iter() {
            let var_name = var.k.unwrap();
            let var_typ = Type::variant(union_typ.clone(), var_name);
            self.funcseq.push(var_name);
            if let Ast::DefType(DataType::Struct, _, flds) = *var.v.node {
                if flds.is_empty() {
                    let vval = Val::EnumToken(var_typ, Lstr::Sref(var_name));
                    let vast = AstNode::new_constval(vval, var.v.loc);
                    self.modscope.insert(sname_id, vast);
                } else {
                    self.add_typed_struct(proto.clone(), flds, loc)?;
                }
            } else {
                return Err(rustfail!(
                    "compile_failure",
                    "expected variant struct found: {:?}",
                    var,
                ));
            }
        }
        // TODO: add the __datatype and/or __modshape fields to
        // the union's module scope
        // self.types.insert(sname_id, union_typ);

        Ok(())
    }

    fn add_trait(
        &mut self,
        name: AstNode,
        funcs: Vec<AstNode>,
    ) -> Lresult<()>
    {
        let proto_t = self.make_proto_type(name)?;
        // look in funcs for a struct def and use the ModTyp::Data?
        // or does that get set in add_typed_struct
        self.add_submod(ModTyp::Trait, proto_t, funcs)
    }

    fn add_submod(
        &mut self,
        subtype: ModTyp,
        proto_t: ProtoType,
        mut funcs: Vec<AstNode>,
    ) -> Lresult<()>
    {
        let id = proto_t.n;
        let subkey = self.key.submod(subtype, id);
        let utyp = Type::User(subkey.name.clone());
        let alias = AstNode::new(
            Ast::DefType(
                DataType::Alias,
                AstNode::new(Ast::Id("Self"), Loc::default()),
                vec![StrupleItem::new_v(
                    AstNode::new(Ast::Type(utyp), Loc::default()),
                )],
            ),
            Loc::default(),
        );
        funcs.push(alias);
        self.imports.insert(id, subkey.name.clone());
        let proto = ltry!(ProtoModule::new_submod(subkey, proto_t, funcs));
        self.submods.insert(id, proto);
        Ok(())
    }

    fn impl_datatype(
        &mut self,
        _datatype: AstNode,
        _funcs: Vec<AstNode>,
    ) -> Lresult<()>
    {
        Ok(())
    }

    fn impl_trait(
        &mut self,
        _typ: AstNode,
        _iface: AstNode,
        _funcs: Vec<AstNode>,
    ) -> Lresult<()>
    {
        Ok(())
    }

    fn make_func_type(
        &mut self,
        name: AstNode,
        args: &Xlist,
        result: AstNode,
        mut opens: GenericTypes,
    ) -> Lresult<(&'static str, FuncType, Type)>
    {
        let loc = name.loc;
        let type_maker: Box<dyn Fn(FuncType) -> Type>;

        let id = match *name.node {
            Ast::Id(name_id) => {
                type_maker = Box::new(|ft| Type::Func(ft));
                name_id
            }
            Ast::Generic(gen, gen_args) => {
                let open_result: Lresult<GenericTypes> = gen_args
                    .iter()
                    .map(|a| {
                        let var = if let Some(v) = a.k {
                            v
                        } else if let Ast::Id(v) = *a.v.node {
                            v
                        } else {
                            return Err(rustfail!(
                                "compile_failure",
                                "unexpected generic argument: {:?}",
                                a,
                            ));
                        };
                        Ok(StrupleItem::new(var, Type::OpenVar(var)))
                    })
                    .collect();
                opens.append(&mut open_result?);

                type_maker = Box::new(|ft| {
                    Type::Generic(true, Box::new(Type::Func(ft)), opens.clone())
                });

                if let Ast::Id(name_id) = *gen.node {
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

        ltry!(self.refute_redefines_default(id, loc));
        let ft =
            ltry!(self.ast_to_ftype(&self.key.name, &args, &result, &opens));
        let ftyp = type_maker(ft.clone());
        Ok((id, ft, ftyp))
    }

    fn make_proto_type(
        &mut self,
        name: AstNode,
    ) -> Lresult<ProtoType>
    {
        let m = &self.key.name;
        let utyp: Type;
        let opens: GenericTypes;

        let id: &'static str = match *name.node {
            Ast::Id(name_id) => {
                utyp = Type::User(self.key.name.join(name_id)?);
                opens = vec![];
                name_id
            }
            Ast::Generic(gen_id, gen_args) => {
                let mut opens1 = Vec::with_capacity(gen_args.len());
                let mut gen_arg_vars = Vec::with_capacity(gen_args.len());
                for a in gen_args.iter() {
                    let var = if let Some(var) = a.k {
                        var
                    } else if let Ast::Id(var) = *a.v.node {
                        var
                    } else {
                        return Err(rustfail!(
                            PROTOFAIL,
                            "generic arguments must have string key: {:?}",
                            a,
                        ));
                    };
                    opens1.push(StrupleItem::new(var, Type::Unknown));
                    gen_arg_vars
                        .push(StrupleItem::new(var, Type::OpenVar(var)));
                }
                opens = opens1;

                if let Ast::Id(name_id) = *gen_id.node {
                    if opens.is_empty() {
                        return Err(Failure::static_leema(
                            failure::Mode::TypeFailure,
                            lstrf!(
                                "generic must have at least one argument: {}",
                                name_id,
                            ),
                            m.0.clone(),
                            name.loc.lineno,
                        ));
                    }

                    let inner = Type::User(m.join(name_id)?);
                    utyp = Type::Generic(true, Box::new(inner), gen_arg_vars);
                    name_id
                } else {
                    return Err(rustfail!(
                        PROTOFAIL,
                        "invalid generic type name: {:?}",
                        gen_id,
                    ));
                }
            }
            Ast::ConstVal(cv) if cv == Val::VOID => {
                if self.typ.is_none() {
                    return Err(rustfail!(
                        "syntax_error",
                        "datatypes must have a typename: {:?}",
                        self.key.best_path(),
                    ));
                }
                let typ = self.typ.as_ref().unwrap();
                utyp = typ.t.clone();
                opens = typ.g.clone();
                typ.n
            }
            _ => {
                return Err(rustfail!(
                    "compile_failure",
                    "unsupported datatype id: {:?}",
                    name,
                ));
            }
        };
        ltry!(self.refute_redefines_default(id, name.loc));
        Ok(ProtoType{n: id, t: utyp, g: opens})
    }

    /*
        let id = match *name.node {
            Ast::Id(name_id) => {
                utyp = Type::User(TypeMod::from(m), name_id);
                opens = vec![];
                name_id
            }
            Ast::Generic(gen_id, gen_args) => {
                let mut opens1 = Vec::with_capacity(gen_args.len());
                let mut gen_arg_vars = Vec::with_capacity(gen_args.len());
                for a in gen_args.iter() {
                    let var = if let Some(var) = a.k {
                        var
                    } else if let Ast::Id(var) = *a.v.node {
                        var
                    } else {
                        return Err(rustfail!(
                            PROTOFAIL,
                            "generic arguments must have string key: {:?}",
                            a,
                        ));
                    };
                    opens1.push(StrupleItem::new(var, Type::Unknown));
                    gen_arg_vars
                        .push(StrupleItem::new(var, Type::OpenVar(var)));
                }
                opens = opens1;

                if let Ast::Id(name_id) = *gen_id.node {
                    if opens.is_empty() {
                        return Err(Failure::static_leema(
                            failure::Mode::TypeFailure,
                            lstrf!(
                                "generic must have at least one argument: {}",
                                name_id,
                            ),
                            m.0.clone(),
                            name.loc.lineno,
                        ));
                    }

                    let itmod = TypeMod::from(m);
                    let inner = Type::User(itmod, name_id);
                    utyp = Type::Generic(true, Box::new(inner), gen_arg_vars);
                    name_id
                } else {
                    return Err(rustfail!(
                        PROTOFAIL,
                        "invalid generic type name: {:?}",
                        gen_id,
                    ));
                }
            }
            _ => {
                return Err(rustfail!(
                    "compile_failure",
                    "unsupported datatype id: {:?}",
                    name,
                ));
            }
        };
        ltry!(self.refute_redefines_default(id, name.loc));
        Ok((id, utyp, opens))
    }
    */

    pub fn type_modules(&self) -> Lresult<Vec<ProtoModule>>
    {
        Ok(vec![])
    }

    /// TODO rename this. Maybe take_func?
    pub fn pop_func(&mut self, func: &str) -> Option<(Xlist, AstNode)>
    {
        let generic = match self.find_modelem(func) {
            Some(fconst) => {
                // check if this is a locally defined function
                fconst.typ.is_open()
            }
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

    pub fn find_macro<'a, 'b>(&'a self, macroname: &'b str) -> Option<&'a Ast>
    {
        self.find_modelem(macroname).and_then(|node| {
            if let Ast::DefMacro(_, _, _) = &*node.node {
                Some(&*node.node)
            } else {
                None
            }
        })
    }

    /// find a module element, regardless of visibility
    pub fn find_modelem<'a, 'b>(&'a self, name: &'b str)
        -> Option<&'a AstNode>
    {
        self.modscope.get(name)
    }

    pub fn find_type(&self, name: &str) -> Option<&Type>
    {
        self.find_modelem(name).and_then(|node| {
            match &*node.node {
                Ast::ConstVal(Val::Type(typeval)) => Some(typeval),
                _ => None,
            }
        })
    }

    pub fn imported_module(&self, alias: &ModAlias) -> Lresult<&Canonical>
    {
        self.imports.get(alias.as_ref()).ok_or_else(|| {
            rustfail!(
                PROTOFAIL,
                "no module imported as {} from {}",
                alias,
                self.key.name,
            )
        })
    }

    pub fn ast_to_type(
        &self,
        local_mod: &Canonical,
        node: &AstNode,
        opens: &[StrupleItem<&'static str, Type>],
    ) -> Lresult<Type>
    {
        Ok(match &*node.node {
            Ast::Id(id) if struple::contains_key(opens, id) => {
                Type::OpenVar(id)
            }
            Ast::Id(id) => {
                match self.find_modelem(id) {
                    Some(modelem) => {
                        ltry!(self.ast_to_type(local_mod, modelem, opens))
                    }
                    None => {
                        match DEFAULT_TYPES.get(id) {
                            Some(dt) => dt.clone(),
                            None => {
                                return Err(rustfail!(
                                    "internal_failure",
                                    "should this have been found? {}",
                                    id,
                                ));
                            }
                        }
                    }
                }
            }
            Ast::List(inner_items) if inner_items.len() == 1 => {
                let inner = &inner_items.first().unwrap().v;
                let inner_t = ltry!(self.ast_to_type(local_mod, inner, opens));
                Type::list(inner_t)
            }
            Ast::Tuple(inner_items) => {
                let inner_t: Lresult<Vec<StrupleItem<Option<Lstr>, Type>>> =
                    inner_items
                        .iter()
                        .map(|item| {
                            let k = item.k.map(|ik| Lstr::Sref(ik));
                            let v = ltry!(
                                self.ast_to_type(local_mod, &item.v, opens)
                            );
                            Ok(StrupleItem::new(k, v))
                        })
                        .collect();
                Type::Tuple(inner_t?)
            }
            Ast::FuncType(args, result) => {
                let ftype =
                    ltry!(self.ast_to_ftype(local_mod, args, result, opens));
                Type::Func(ftype)
            }
            Ast::Generic(base, typeargs) => {
                let genbase = ltry!(self.ast_to_type(local_mod, base, opens));
                let genargsr: Lresult<GenericTypes> = typeargs
                    .iter()
                    .map(|t| {
                        let k = t.k.unwrap_or("");
                        Ok(StrupleItem::new(
                            k,
                            ltry!(self.ast_to_type(local_mod, &t.v, opens)),
                        ))
                    })
                    .collect();
                let genargs = genargsr?;
                Type::generic(genbase, genargs)
            }
            Ast::ConstVal(cv) => cv.get_type(),
            Ast::Type(typ) => typ.clone(),
            Ast::Op2(".", module_node, id_node) => {
                match (&*module_node.node, &*id_node.node) {
                    (Ast::Id(m), Ast::Id(id)) => {
                        match self.imports.get(m) {
                            Some(canonical) => {
                                Type::User(canonical.join(id)?)
                            }
                            None => {
                                return Err(rustfail!(
                                    PROTOFAIL,
                                    "unknown module {}",
                                    m,
                                ));
                            }
                        }
                    }
                    what => {
                        return Err(rustfail!(
                            PROTOFAIL,
                            "unexpected type {:?}",
                            what,
                        ));
                    }
                }
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

    /// for finding the field index in a defined struct type
    /// ok great, but can you find a method in a trait implementation?
    pub fn type_field_idx<S>(type_src: &CanonicalTypeSrc, c: &Canonical, fld: &S) -> Option<i64>
        where S: AsRef<str>
    {
        match type_src.get(c)? {
            TypeSrc::Struct(_t, flds) => {
                match struple::find_str(&flds[..], fld) {
                    Some((fld_idx, _)) => Some(fld_idx as i64),
                    None => None,
                }
            }
            TypeSrc::Alias(_t0, Type::User(c1)) => {
                Self::type_field_idx(type_src, &c1, fld)
            }
            _ => None,
        }
    }

    fn ast_to_ftype(
        &self,
        local_mod: &Canonical,
        args: &Xlist,
        result: &AstNode,
        opens: &[StrupleItem<&'static str, Type>],
    ) -> Lresult<FuncType>
    {
        let arg_types = self.xlist_to_types(local_mod, args, &opens)?;
        let result_type = ltry!(self.ast_to_type(local_mod, &result, opens));
        Ok(FuncType::new(arg_types, result_type))
    }

    fn xlist_to_types(
        &self,
        local_mod: &Canonical,
        args: &Xlist,
        opens: &[StrupleItem<&'static str, Type>],
    ) -> Lresult<Struple2<Type>>
    {
        let arg_types_r: Lresult<StrupleKV<Option<Lstr>, Type>>;
        arg_types_r = args
            .into_iter()
            .map(|i| {
                Ok(StrupleItem::new(
                    i.k.map(|k| Lstr::Sref(k)),
                    ltry!(self.ast_to_type(local_mod, &i.v, opens)),
                ))
            })
            .collect();
        Ok(arg_types_r?)
    }
}

#[derive(Debug)]
pub struct ProtoLib
{
    protos: HashMap<PathBuf, ProtoModule>,
    type_src: CanonicalTypeSrc,
}

impl ProtoLib
{
    pub fn new() -> ProtoLib
    {
        ProtoLib {
            protos: HashMap::new(),
            type_src: HashMap::new(),
        }
    }

    /// mostly used for testing
    pub fn add_module(
        &mut self,
        modname: &Path,
        src: &'static str,
    ) -> Lresult<()>
    {
        vout!("ProtoLib::add_module({})\n", modname.display());
        if self.protos.contains_key(modname) {
            return Err(rustfail!(
                PROTOFAIL,
                "cannot load a module twice: {}",
                modname.display(),
            ));
        }
        let modkey = ModKey::from(Canonical::from(modname));
        let proto = ProtoModule::new(modkey, src)?;
        self.put_module(modname, proto)?;
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
    ///   for p in mod_path:
    ///     load_canonical(p)
    ///
    pub fn load_absolute(
        &mut self,
        loader: &mut Interloader,
        mod_path: &Path,
    ) -> Lresult<()>
    {
        vout!("ProtoLib::load_absolute({})\n", mod_path.display());
        if !mod_path.is_absolute() {
            return Err(rustfail!(
                PROTOFAIL,
                "module is not absolute {:?}",
                mod_path,
            ));
        }
        for p in Canonical::ancestors(mod_path) {
            ltry!(self.load_canonical(loader, p));
        }
        Ok(())
    }

    /*
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
    */

    /// load module by canonical name, w/o checking permissions etc
    fn load_canonical(
        &mut self,
        loader: &mut Interloader,
        modpath: &Path,
    ) -> Lresult<()>
    {
        vout!("ProtoLib::load_canonical({:?})\n", modpath);
        if self.protos.contains_key(modpath) {
            // already loaded
            return Ok(());
        }

        let modkey = ltry!(loader.new_key(modpath));
        let modtxt = ltry!(loader.read_mod(&modkey));
        let mut proto = ltry!(ProtoModule::new(modkey.clone(), modtxt)
            .map_err(|e| { e.lstr_loc(modkey.best_path(), 0) }));
        for sub in proto.submods.drain() {
            let submodname = sub.1.key.name.clone();
            self.put_module(submodname.as_path(), sub.1)?;
        }

        self.put_module(modpath, proto)?;
        Ok(())
    }

    fn put_module(
        &mut self,
        modpath: &Path,
        proto: ProtoModule,
    ) -> Lresult<()>
    {
        let type_mods = proto.type_modules()?;
        self.protos.insert(modpath.to_path_buf(), proto);
        for tm in type_mods.into_iter() {
            self.protos.insert(tm.key.name.as_path().to_path_buf(), tm);
        }
        Ok(())
    }

    pub fn load_imports(
        &mut self,
        loader: &mut Interloader,
        modname: &Path,
    ) -> Lresult<()>
    {
        vout!("ProtoLib::load_imports({})\n", modname.display());
        let mut imported: Vec<(ModAlias, Canonical)> = vec![];
        {
            let proto = self.protos.get(modname).ok_or_else(|| {
                rustfail!(
                    PROTOFAIL,
                    "an import module does not exist: {}",
                    modname.display(),
                )
            })?;
            for (k, i) in proto.imports.iter() {
                if i.as_path() == modname {
                    return Err(rustfail!(
                        PROTOFAIL,
                        "a module cannot import itself: {}",
                        i,
                    ));
                }
                imported.push((ModAlias(*k), i.clone()));
            }
        }

        for (_k, i) in imported.iter() {
            ltry!(self.load_absolute(loader, &i.as_path()))
        }
        Ok(())
    }

    pub fn exported_elem(
        &self,
        modname: &Canonical,
        elem: &'static str,
        loc: Loc,
    ) -> Lresult<&AstNode>
    {
        let proto = self.path_proto(modname).unwrap();
        if proto.localdef.contains(elem) {
            return Err(Failure::static_leema(
                failure::Mode::CompileFailure,
                lstrf!("module element is not exported {} {}", modname, elem),
                modname.0.clone(),
                loc.lineno,
            ));
        }
        proto.modscope.get(elem)
            .ok_or_else(|| {
                Failure::static_leema(
                    failure::Mode::CompileFailure,
                    lstrf!("module element not found {} {}", modname, elem),
                    modname.0.clone(),
                    loc.lineno,
                )
            })
    }

    pub fn imported_proto(
        &self,
        proto: &Canonical,
        alias: &ModAlias,
    ) -> Lresult<&ProtoModule>
    {
        let modpath = proto.as_path();
        let protomod = self.protos.get(modpath).ok_or_else(|| {
            rustfail!(PROTOFAIL, "module not loaded: {:?}", proto)
        })?;
        let new_path = ltry!(protomod.imported_module(alias));
        Ok(ltry!(self.path_proto(&new_path)))
    }

    pub fn path_proto(&self, path: &Canonical) -> Lresult<&ProtoModule>
    {
        self.protos.get(path.as_path()).ok_or_else(|| {
            rustfail!(PROTOFAIL, "module not loaded: {:?}", path)
        })
    }

    pub fn path_proto_mut(
        &mut self,
        path: &Canonical,
    ) -> Lresult<(&mut ProtoModule, &CanonicalTypeSrc)>
    {
        let result = self.protos.get_mut(path.as_path()).ok_or_else(|| {
            rustfail!(PROTOFAIL, "module not loaded: {:?}", path)
        });
        match result {
            Err(e) => Err(e),
            Ok(p) => Ok((p, &self.type_src)),
        }
    }
}


#[cfg(test)]
mod tests
{
    use super::{ProtoLib, ProtoModule};
    use crate::leema::ast2::Ast;
    use crate::leema::loader::Interloader;
    use crate::leema::lstr::Lstr;
    use crate::leema::module::ModKey;
    use crate::leema::struple::{self, StrupleItem};
    use crate::leema::val::{FuncType, Type, Val};

    use std::path::Path;

    use matches::assert_matches;


    fn new_proto(input: &'static str) -> ProtoModule
    {
        let key = ModKey::from("/foo");
        ProtoModule::new(key, input).expect("ProtoModule load failure")
    }

    #[test]
    fn test_proto_func_noargs()
    {
        let proto = new_proto(r#"func hello -> "world" --"#);

        let funcseq = proto.funcseq.get(0).expect("no funcseq type");
        assert_eq!("hello", *funcseq);
        assert_eq!(1, proto.funcseq.len());
        assert_eq!(1, proto.funcsrc.len());
    }

    #[test]
    fn test_proto_genericfunc()
    {
        let input = r#"
        func <first T>:T :: a:T b:T ->
            a
        --
        "#;
        let proto = new_proto(input);
        let tvt = Type::OpenVar("T");

        assert_eq!(1, proto.modscope.len());
        assert!(proto.modscope.contains_key("first"));
        assert_eq!(
            Type::Generic(
                true,
                Box::new(Type::Func(FuncType::new(
                    vec![
                        StrupleItem::new(Some(Lstr::Sref("a")), tvt.clone()),
                        StrupleItem::new(Some(Lstr::Sref("b")), tvt.clone()),
                    ],
                    tvt.clone(),
                ))),
                vec![StrupleItem::new("T", Type::OpenVar("T"))],
            ),
            proto.modscope.get("first")
                .unwrap()
                .typ,
        );

        // first is exported by default
        assert_eq!(0, proto.localdef.len());
        // function definitions do not create new modules
        assert_eq!(0, proto.submods.len());
    }

    #[test]
    fn test_proto_generic_struct()
    {
        let proto = new_proto("datatype <Point T> :: x:T y:T --");

        let point_type = proto.types.get("Point").expect("no Point type");
        let expected = Type::Generic(
            true,
            Box::new(user_type!("/foo/Point")),
            vec![StrupleItem::new("T", Type::OpenVar("T"))],
        );
        assert_eq!(expected, *point_type);
    }

    #[test]
    fn test_proto_imports()
    {
        let proto = new_proto(
            "
        import /tacos
        import burritos
        import tortas ->
            huevos
            huevos ->
                rancheros
                enchiladas
            --
        --
        import ../nachos
        ",
        );

        assert_eq!(6, proto.imports.len());
        assert_eq!(0, proto.exports.len());

        assert_eq!(*"/tacos", proto.imports["tacos"]);
        assert_eq!(*"/foo/burritos", proto.imports["burritos"]);
        assert_eq!(*"/foo/tortas/huevos", proto.imports["huevos"]);
        assert_eq!(*"/foo/tortas/huevos/rancheros", proto.imports["rancheros"]);
        assert_eq!(
            *"/foo/tortas/huevos/enchiladas",
            proto.imports["enchiladas"]
        );
        assert_eq!(*"/nachos", proto.imports["nachos"]);
    }

    #[test]
    fn test_proto_export_star()
    {
        let proto = new_proto("export *");

        assert_eq!(0, proto.imports.len());
        assert_eq!(0, proto.exports.len());
        assert_eq!(Some(true), proto.exports_all);
    }

    #[test]
    fn test_proto_export_module()
    {
        let proto = new_proto("export tacos");

        assert_eq!(1, proto.imports.len());
        assert_eq!(0, proto.localdef.len());
        assert_eq!(*"/foo/tacos", proto.imports["tacos"]);
        assert_eq!(Some(false), proto.exports_all);
    }

    #[test]
    fn test_proto_export_nothing()
    {
        let proto = new_proto("import tacos");

        assert_eq!(1, proto.imports.len());
        assert_eq!(1, proto.exports.len());
        assert_eq!(*"/foo/tacos", proto.imports["tacos"]);
        assert_eq!("tacos", proto.exports["tacos"]);
        assert_eq!(None, proto.exports_all);
    }

    #[test]
    fn test_import_basic_exports()
    {
        let a = "
        import b
        import c
        import c/d
        "
        .to_string();
        let b = "
        func foo >> 3 --
        "
        .to_string();
        let c = "
        export d
        "
        .to_string();
        let d = "
        func bar >> 5 --
        "
        .to_string();

        let mut loader = Interloader::default();
        loader.set_mod_txt(ModKey::from("/a"), a);
        loader.set_mod_txt(ModKey::from("/a/b"), b);
        loader.set_mod_txt(ModKey::from("/a/c"), c);
        loader.set_mod_txt(ModKey::from("/a/c/d"), d);

        let mut protos = ProtoLib::new();
        protos.load_absolute(&mut loader, Path::new("/a")).unwrap();
        protos.load_imports(&mut loader, Path::new("/a")).unwrap();

        assert_eq!(4, protos.protos.len());
    }

    #[test]
    fn test_import_skipped_export()
    {
        let a = "import b/c/d".to_string();
        let b = "export c/d".to_string();
        let c = "export d".to_string();
        let d = "
        func bar >> 5 --
        "
        .to_string();

        let mut loader = Interloader::default();
        loader.set_mod_txt(ModKey::from("/a"), a);
        loader.set_mod_txt(ModKey::from("/a/b"), b);
        loader.set_mod_txt(ModKey::from("/a/b/c"), c);
        loader.set_mod_txt(ModKey::from("/a/b/c/d"), d);

        let mut protos = ProtoLib::new();
        protos.load_absolute(&mut loader, Path::new("/a")).unwrap();
        protos.load_imports(&mut loader, Path::new("/a")).unwrap();

        assert_eq!(4, protos.protos.len());
    }

    #[test]
    fn test_import_absolute_and_sibling()
    {
        let a = "
        export b
        export c
        "
        .to_string();
        let b = "
        import ../c
        import /d
        "
        .to_string();
        let c = "
        func foo >> 3 --
        "
        .to_string();
        let d = "
        func bar >> 5 --
        "
        .to_string();

        let mut loader = Interloader::default();
        loader.set_mod_txt(ModKey::from("/a"), a);
        loader.set_mod_txt(ModKey::from("/a/b"), b);
        loader.set_mod_txt(ModKey::from("/a/c"), c);
        loader.set_mod_txt(ModKey::from("/d"), d);
        let mut protos = ProtoLib::new();

        protos.load_absolute(&mut loader, Path::new("/a")).unwrap();
        protos.load_imports(&mut loader, Path::new("/a")).unwrap();
        assert_eq!(3, protos.protos.len());

        protos
            .load_absolute(&mut loader, Path::new("/a/b"))
            .unwrap();
        protos.load_imports(&mut loader, Path::new("/a/b")).unwrap();
        assert_eq!(4, protos.protos.len());
    }

    #[test]
    #[should_panic]
    fn test_not_exported()
    {
        let a = "
        import /b
        "
        .to_string();
        let b = "
        func foo >> 3 --
        "
        .to_string();

        let mut loader = Interloader::default();
        loader.set_mod_txt(ModKey::from("/a"), a);
        loader.set_mod_txt(ModKey::from("/a/b"), b);
        let mut protos = ProtoLib::new();

        protos.load_absolute(&mut loader, Path::new("a")).unwrap();
        protos.load_imports(&mut loader, Path::new("a")).unwrap();
        assert_eq!(2, protos.protos.len());

        protos
            .load_absolute(&mut loader, Path::new("a/b"))
            .expect("want this panic");
    }

    #[test]
    fn test_proto_import_exported_func()
    {
        let a = "
        func foo >> 4 --
        "
        .to_string();
        let b = "import /a.foo
        func bar >> foo() + 3 --
        "
        .to_string();

        let mut loader = Interloader::default();
        loader.set_mod_txt(ModKey::from("/a"), a);
        loader.set_mod_txt(ModKey::from("/b"), b);
        let mut protos = ProtoLib::new();

        protos.load_absolute(&mut loader, Path::new("/b")).unwrap();
        protos.load_imports(&mut loader, Path::new("/b")).unwrap();
        assert_eq!(2, protos.protos.len());

        let a = protos.path_proto(&canonical!("/a")).unwrap();
        assert_eq!(0, a.imported_vals.len());
        assert_eq!(1, a.exported_vals.len());
        assert_eq!(0, a.local_vals.len());
        assert!(struple::contains_key(&a.exported_vals, &Some("foo")));

        let b = protos.path_proto(&canonical!("/b")).unwrap();
        assert_eq!(0, b.imported_vals.len());
        assert_eq!(1, b.exported_vals.len());
        assert_eq!(0, b.local_vals.len());
        assert_eq!(1, b.id_canonicals.len());
    }

    #[test]
    fn test_proto_token()
    {
        let proto = new_proto("datatype Burrito --");

        let burrito_type = proto.types.get("Burrito").expect("no Burrito type");
        assert_eq!(user_type!("/foo/Burrito"), *burrito_type,);

        assert!(proto.token.contains("Burrito"));

        let burrito_val =
            proto.find_modelem("Burrito").expect("no Burrito const");
        assert_matches!(*burrito_val.node, Ast::ConstVal(_));
        if let Ast::ConstVal(ref val) = &*burrito_val.node {
            assert_eq!(Val::Token(burrito_type.clone()), *val);
        }
    }

    #[test]
    fn test_proto_struct()
    {
        let proto = new_proto("datatype Point :: x:Int y:Int --");

        let point_type = proto.types.get("Point").expect("no Point type");
        assert_eq!(user_type!("/foo/Point"), *point_type);
    }

    #[test]
    fn test_proto_alias_type()
    {
        let proto = new_proto("datatype Burrito := Int");

        let burrito_type = proto.types.get("Burrito").expect("no Burrito type");
        assert_eq!(user_type!("/foo/Burrito"), *burrito_type,);

        let burrito_val =
            proto.find_modelem("Burrito").expect("no Burrito const");
        assert_matches!(*burrito_val.node, Ast::ConstVal(_));
        if let Ast::ConstVal(ref val) = &*burrito_val.node {
            if let Val::Type(t1) = &*val {
                assert_eq!(user_type!("/foo/Burrito"), *t1);
            } else {
                panic!("expected user type, found: {:?}", val);
            }
        }
    }
}
