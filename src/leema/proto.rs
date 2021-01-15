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
use crate::leema::struple::{self, StrupleItem, StrupleKV};
use crate::leema::val::{Fref, Type, TypeArgSlice, TypeArgs, Val};

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
    static ref BUILTIN_SCOPE: HashMap<&'static str, Ast> = {
        let mut ids = HashMap::new();
        // types
        ids.insert("Bool", Ast::canonical("/core/Bool"));
        ids.insert("Int", Ast::canonical("/core/Int"));
        ids.insert("Str", Ast::canonical("/core/Str"));
        ids.insert("Option", Ast::canonical("/core/Option"));
        ids.insert("#", Ast::canonical("/core/#"));
        // constants
        ids.insert("False", Ast::ConstVal(Val::FALSE));
        ids.insert("True", Ast::ConstVal(Val::TRUE));
        ids.insert("Void", Ast::ConstVal(Val::VOID));
        // functions
        ids.insert("boolean_and", Ast::canonical("/core.boolean_and"));
        ids.insert("boolean_not", Ast::canonical("/core.boolean_not"));
        ids.insert("boolean_or", Ast::canonical("/core.boolean_or"));
        ids.insert("create_failure", Ast::canonical("/core.create_failure"));
        ids.insert("fail", Ast::canonical("/core.fail"));
        ids.insert("int_add", Ast::canonical("/core.int_add"));
        ids.insert("int_div", Ast::canonical("/core.int_div"));
        ids.insert("int_equal", Ast::canonical("/core.int_equal"));
        ids.insert("int_gt", Ast::canonical("/core.int_gt"));
        ids.insert("int_gteq", Ast::canonical("/core.int_gteq"));
        ids.insert("int_less_than", Ast::canonical("/core.int_less_than"));
        ids.insert("int_mod", Ast::canonical("/core.int_mod"));
        ids.insert("int_mult", Ast::canonical("/core.int_mult"));
        ids.insert("int_negate", Ast::canonical("/core.int_negate"));
        ids.insert("int_sub", Ast::canonical("/core.int_sub"));
        ids.insert("int_lteq", Ast::canonical("/core.int_lteq"));
        ids.insert("new_struct_val", Ast::canonical("/core.new_struct_val"));
        ids.insert("not_equal", Ast::canonical("/core.not_equal"));
        ids.insert("void", Ast::canonical("/core.void"));
        ids
    };

    static ref BUILTIN_TYPES: HashMap<&'static str, Type> = {
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
}

/// Asts separated into their types of components
/// trait_t will be Some for a trait or data type
/// data_t will be Some for a data type or impl
#[derive(Debug)]
pub struct ProtoModule
{
    pub key: ModKey,
    data_t: Option<ProtoType>,
    trait_t: Option<ProtoType>,
    pub imports: HashMap<&'static str, Canonical>,
    pub exports_all: Option<bool>,
    pub funcseq: Vec<&'static str>,
    pub funcsrc: HashMap<&'static str, (Xlist, AstNode)>,
    submods: StrupleKV<&'static str, ProtoModule>,
    modscope: HashMap<&'static str, AstNode>,
    localdef: HashSet<&'static str>,
    /// types that implement this trait (if it's a trait)
    implementors: HashSet<Canonical>,
    /// implementations by this type for other types
    implementations: StrupleKV<Canonical, ProtoModule>,
}

impl ProtoModule
{
    pub fn new(key: ModKey, src: &'static str) -> Lresult<ProtoModule>
    {
        let items = parse_file(src)?;
        Self::with_ast(key, None, None, items)
    }

    fn with_ast(
        key: ModKey,
        data_t: Option<ProtoType>,
        trait_t: Option<ProtoType>,
        items: Vec<AstNode>,
    ) -> Lresult<ProtoModule>
    {
        let modname = key.name.clone();
        let mut proto = ProtoModule {
            key,
            data_t: data_t,
            trait_t: trait_t,
            imports: HashMap::new(),
            exports_all: None,
            funcseq: Vec::new(),
            funcsrc: HashMap::new(),
            submods: Vec::new(),
            modscope: HashMap::new(),
            localdef: HashSet::new(),
            implementors: HashSet::new(),
            implementations: Vec::new(),
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
                            Lstr::Sref(
                                "cannot mix exporting * and specific modules",
                            ),
                            modname.to_lstr(),
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
                            modname.to_lstr(),
                            i.loc.lineno,
                        ));
                    }
                    proto.exports_all = Some(false);
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

            proto.add_import(k, v, loc)?;
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

            proto.add_import(k, v.clone(), loc)?;
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

    fn type_args(&self) -> TypeArgs
    {
        self.trait_t
            .as_ref()
            .or(self.data_t.as_ref())
            .map(|p| p.t.args.clone())
            .unwrap_or(vec![])
    }

    /// return the src type if this module defines an alias
    pub fn alias_type(&self) -> Option<&Type>
    {
        if self.key.mtyp == ModTyp::Alias {
            self.data_t.as_ref().map(|t| &t.t)
        } else {
            None
        }
    }

    /// trait with implementation
    pub fn trait_with_impl(&self, t: &Type) -> Option<&Type>
    {
        if self.key.mtyp == ModTyp::Trait {
            if self.implementors.contains(&t.path) {
                Some(&self.trait_t.as_ref().unwrap().t)
            } else {
                None
            }
        } else {
            None
        }
    }

    fn add_import(
        &mut self,
        name: &'static str,
        imp: ImportedMod,
        loc: Loc,
    ) -> Lresult<()>
    {
        // if imp is absolute, canonical will just be imp
        let canon = if imp.is_absolute() {
            Canonical::from(imp.0)
        } else {
            self.key.name.join(&imp)?
        };
        self.imports.insert(name, canon.clone());
        self.modscope
            .insert(name, AstNode::new(Ast::Canonical(canon), loc));
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
            Ast::DefTrait(name, funcs) => lfailoc!(self.add_trait(name, funcs)),
            Ast::DefImpl(iface, typ, funcs) => {
                lfailoc!(self.impl_trait(iface, typ, funcs))
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
            e.lstr_loc(Lstr::from(self.key.best_path()), loc.lineno as u32)
        })
    }

    fn refute_redefines_default(&self, id: &str, loc: Loc) -> Lresult<()>
    {
        if !self.key.name.is_core() && BUILTIN_SCOPE.contains_key(id) {
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

        let opens = self.type_args();
        let (name_id, ftyp) =
            ltry!(self.make_func_type(name, &args, result, opens.clone()));
        let ft = ftyp.try_func_ref()?;

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
        if self.trait_t.is_some() {
            // a struct type defined within a trait
            if !name.node.is_void() {
                return Err(rustfail!(
                    "semantic_error",
                    "cannot define new struct within trait: {:?} at {:?}",
                    name,
                    loc,
                ));
            }
            // this is a struct for a trait, set its data_t now
            self.data_t = self.trait_t.clone();
            self.add_typed_struct(fields, loc)
        } else {
            // a struct type defined directly within a file module
            if name.node.is_void() {
                return Err(rustfail!(
                    "semantic_error",
                    "cannot define unnamed struct outside of trait: {:?}",
                    loc,
                ));
            }
            let data_t = self.make_proto_type(name)?;
            // create a module for holding the constructor
            let subp = ltry!(self.add_selfmod(
                ModTyp::Data,
                Some(data_t),
                None,
                vec![],
                loc,
            ));
            subp.add_typed_struct(fields, loc)
        }
    }

    fn add_typed_struct(&mut self, fields: Xlist, loc: Loc) -> Lresult<()>
    {
        let typ = self.data_t.as_ref().unwrap().t.clone();

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
        type_node.typ = Type::KIND;
        let constructor_ast = AstNode::new(
            Ast::DefFunc(
                AstNode::new(Ast::Id(MODNAME_CONSTRUCT), loc),
                fields.clone(),
                AstNode::new(Ast::Type(typ.clone()), loc),
                construction.clone(),
            ),
            loc,
        );

        self.append_ast(vec![constructor_ast])?;
        self.modscope.insert(MODNAME_DATATYPE, type_node);
        ltry!(self.add_data_fields(fields));
        Ok(())
    }

    /// add the struct fields to the module definition
    fn add_data_fields(&mut self, fields: Xlist) -> Lresult<()>
    {
        for (i, f) in fields.iter().enumerate() {
            // safe to unwrap b/c this will only be called for structs
            // where self.data_t has already been set to Some
            let data_t = &self.data_t.as_ref().unwrap().t;
            let t = self.ast_to_type(&f.v, data_t.type_args())?;
            match f.k {
                Some(k) => {
                    let scope_type =
                        AstNode::new(Ast::DataMember(t, i as u8), f.v.loc);
                    self.modscope.insert(k, scope_type);
                }
                None => {
                    eprintln!("unnamed field: {} {:?}", i, f)
                    /*
                    return Err(Failure::static_leema(
                        failure::Mode::LeemaTodoFailure,
                        Lstr::Sref("unnamed fields unimplemented"),
                        self.key.name.0.clone(),
                        f.v.loc.lineno,
                    ))
                    */
                }
            }
        }
        Ok(())
    }

    fn add_token(&mut self, name: AstNode) -> Lresult<()>
    {
        match *name.node {
            Ast::Id(name_id) => {
                ltry!(self.refute_redefines_default(name_id, name.loc));
                let tok_mod = self.key.name.clone();
                let t = Type::from(tok_mod.join(&name_id)?);
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
        let alias_t = ltry!(self.make_proto_type(name));
        let src_typ = ltry!(self.ast_to_type(&src, &alias_t.t.args));
        let src_node = AstNode::new(Ast::Type(src_typ.clone()), src.loc);
        let src_t = ProtoType {
            n: "alias",
            t: src_typ,
        };

        let id = alias_t.n;
        let subkey = self.key.submod(ModTyp::Alias, id)?;
        self.imports.insert(id, subkey.name.clone());
        self.modscope
            .insert(id, AstNode::new(Ast::Canonical(subkey.name.clone()), loc));
        // can't use add-selfmod b/c it makes an alias which recurses infinitely
        let mut sub = ltry!(ProtoModule::with_ast(
            subkey,
            Some(src_t),
            Some(alias_t),
            vec![]
        ));
        sub.modscope.insert(MODNAME_DATATYPE, src_node);
        struple::push_unique(&mut self.submods, id, sub)?;
        Ok(())
    }

    fn add_rust_type(&mut self, name: AstNode) -> Lresult<()>
    {
        let loc = name.loc;
        let proto_t = self.make_proto_type(name)?;
        let name = proto_t.n;
        let typenode = Ast::Type(proto_t.t.clone());
        let mut node = AstNode::new(typenode, loc);
        node.typ = Type::KIND;
        let subcon = {
            let sub = ltry!(self.add_selfmod(
                ModTyp::Data,
                Some(proto_t),
                None,
                vec![],
                loc
            ));
            // TODO: add __datatype or whatever
            sub.modscope.insert(MODNAME_DATATYPE, node);
            sub.key.name.clone()
        };
        self.modscope
            .insert(name, AstNode::new(Ast::Canonical(subcon), loc));
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
        let data_t = self.make_proto_type(name)?;
        let sname_id = data_t.n;
        let union_typ = data_t.t.clone();
        self.imports.insert(sname_id, union_typ.path.clone());

        let m = ltry!(self.add_selfmod(
            ModTyp::Data,
            Some(data_t),
            None,
            vec![],
            loc
        ));
        for var in variants.into_iter() {
            let var_name = var.k.unwrap();
            if let Ast::DefType(DataType::Struct, _, flds) = *var.v.node {
                if flds.is_empty() {
                    let vval =
                        Val::EnumToken(union_typ.clone(), Lstr::Sref(var_name));
                    let vast = AstNode::new_constval(vval, var.v.loc);
                    m.modscope.insert(sname_id, vast);
                } else {
                    m.add_typed_struct(flds, loc)?;
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
        m.modscope
            .insert(MODNAME_DATATYPE, AstNode::new(Ast::Type(union_typ), loc));

        Ok(())
    }

    fn add_trait(&mut self, name: AstNode, funcs: Vec<AstNode>) -> Lresult<()>
    {
        let loc = name.loc;
        let proto_t = self.make_proto_type(name)?;
        let trait_type = AstNode::new(Ast::Type(proto_t.t.clone()), loc);
        // look in funcs for a struct def and use the ModTyp::Data?
        // or does that get set in add_typed_struct
        let m = ltry!(self.add_selfmod(
            ModTyp::Trait,
            None,
            Some(proto_t),
            funcs,
            loc
        ));
        m.modscope.insert(MODNAME_DATATYPE, trait_type);
        Ok(())
    }

    fn add_selfmod(
        &mut self,
        subtype: ModTyp,
        data_t: Option<ProtoType>,
        trait_t: Option<ProtoType>,
        mut funcs: Vec<AstNode>,
        loc: Loc,
    ) -> Lresult<&mut ProtoModule>
    {
        let id = data_t.as_ref().or(trait_t.as_ref()).unwrap().n;
        let subkey = self.key.submod(subtype, id)?;
        let utyp = data_t.as_ref().or(trait_t.as_ref()).unwrap().t.clone();
        let alias = AstNode::new(
            Ast::DefType(
                DataType::Alias,
                AstNode::new(Ast::Id("Self"), loc),
                vec![StrupleItem::new_v(AstNode::new(Ast::Type(utyp), loc))],
            ),
            loc,
        );
        funcs.insert(0, alias);
        self.imports.insert(id, subkey.name.clone());
        self.modscope
            .insert(id, AstNode::new(Ast::Canonical(subkey.name.clone()), loc));
        let sub = ltry!(ProtoModule::with_ast(subkey, data_t, trait_t, funcs));
        struple::push_unique(&mut self.submods, id, sub)?;
        Ok(struple::find_mut(&mut self.submods, id).unwrap())
    }

    /// mark a trait as implemented for a given type
    /// put to a list
    fn impl_trait(
        &mut self,
        trait_node: AstNode,
        data_node: AstNode,
        mut funcs: Vec<AstNode>,
    ) -> Lresult<()>
    {
        let id = "impl";
        let loc = trait_node.loc;
        let trait_t = ltry!(self.make_proto_type(trait_node));
        let data_typ = ltry!(self.ast_to_type(&data_node, &trait_t.t.args));
        let data_t = ProtoType {
            n: id,
            t: data_typ.clone(),
        };

        let subkey = self.key.subimpl(trait_t.t.path.clone())?;
        let alias = AstNode::new(
            Ast::DefType(
                DataType::Alias,
                AstNode::new(Ast::Id("Self"), loc),
                vec![StrupleItem::new_v(AstNode::new(
                    Ast::Type(data_typ),
                    loc,
                ))],
            ),
            loc,
        );
        funcs.insert(0, alias);

        let sub = ltry!(ProtoModule::with_ast(
            subkey,
            Some(data_t),
            Some(trait_t),
            funcs
        ));
        self.submods.push(StrupleItem::new(id, sub));
        Ok(())
    }

    fn make_func_type(
        &mut self,
        name: AstNode,
        args: &Xlist,
        result: AstNode,
        mut opens: TypeArgs,
    ) -> Lresult<(&'static str, Type)>
    {
        let loc = name.loc;

        let id = match *name.node {
            Ast::Id(name_id) => name_id,
            Ast::Generic(gen, gen_args) => {
                let open_result: Lresult<TypeArgs> = gen_args
                    .iter()
                    .enumerate()
                    .map(|(i, a)| {
                        let var = if let Some(v) = a.k {
                            Lstr::Sref(v)
                        } else if let Ast::Id(v) = *a.v.node {
                            Lstr::Sref(v)
                        } else {
                            Type::unwrap_name(&None, i)
                        };
                        let key = var.clone();
                        Ok(StrupleItem::new(key, Type::open(var)))
                    })
                    .collect();
                opens.append(&mut open_result?);

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
        let ftyp = lfctx!(
            self.ast_to_ftype(&result, &args, &opens),
            "file": self.key.best_path(),
            "line": lstrf!("{}", name.loc.lineno),
            "func": Lstr::Sref(id)
        );
        Ok((id, ftyp))
    }

    fn make_proto_type(&mut self, name: AstNode) -> Lresult<ProtoType>
    {
        let m = &self.key.name;
        let utyp: Type;

        let id: &'static str = match *name.node {
            Ast::Id(name_id) => {
                utyp = Type::from(self.key.name.join(name_id)?);
                name_id
            }
            Ast::Generic(gen_id, gen_args) => {
                let mut gen_arg_vars: TypeArgs =
                    Vec::with_capacity(gen_args.len());
                for (i, a) in gen_args.iter().enumerate() {
                    let var = if let Some(var) = a.k {
                        Lstr::Sref(var)
                    } else if let Ast::Id(var) = *a.v.node {
                        Lstr::Sref(var)
                    } else {
                        Type::unwrap_name(&None, i)
                    };
                    gen_arg_vars
                        .push(StrupleItem::new(var.clone(), Type::open(var)));
                }

                if let Ast::Id(name_id) = *gen_id.node {
                    if gen_arg_vars.is_empty() {
                        return Err(Failure::static_leema(
                            failure::Mode::TypeFailure,
                            lstrf!(
                                "generic must have at least one argument: {}",
                                name_id,
                            ),
                            m.to_lstr(),
                            name.loc.lineno,
                        ));
                    }

                    let inner = m.join(name_id)?;
                    utyp = Type::new(inner, gen_arg_vars);
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
                if self.trait_t.is_none() {
                    return Err(rustfail!(
                        "syntax_error",
                        "datatypes must have a typename: {:?}",
                        self.key.best_path(),
                    ));
                }
                let typ = self.trait_t.as_ref().unwrap();
                utyp = typ.t.clone();
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
        Ok(ProtoType { n: id, t: utyp })
    }

    pub fn take_func(&mut self, func: &Fref) -> Lresult<AstNode>
    {
        let generic = match self.find_modelem(func.f) {
            Some(fconst) => {
                // check if this is a locally defined function
                fconst.typ.is_open()
            }
            None => {
                return Err(rustfail!(
                    "semantic_failure",
                    "no function found for {}",
                    func
                ));
            }
        };
        let src = if generic {
            // make a copy for generic functions
            self.funcsrc.get(func.f).map(|fsrc| fsrc.1.clone())
        } else {
            // just take the original if not generic
            self.funcsrc.remove(func.f).map(|fsrc| fsrc.1)
        };
        src.ok_or_else(|| {
            rustfail!("semantic_failure", "no function source for {}", func)
        })
    }

    pub fn take_method(&mut self, func: &Fref) -> Lresult<AstNode>
    {
        let opt_im = self
            .implementations
            .iter_mut()
            .find(|i| i.v.key.name == func.m.name);
        match opt_im {
            Some(im) => im.v.take_func(func),
            None => {
                Err(rustfail!(
                    "leema_failure",
                    "no implementation of trait {} with {}",
                    func.m.name,
                    self.key.name,
                ))
            }
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

    /// find a method call
    /// this checks modelems and then implementations
    /// sequentially finding implementation methods for now
    /// maybe would want to do something more intelligent later
    pub fn find_method<'a, 'b>(&'a self, name: &'b str) -> Option<&'a AstNode>
    {
        self.find_modelem(name).or_else(|| {
            self.implementations
                .iter()
                .find_map(|im| im.v.find_modelem(name))
        })
    }

    pub fn find_type(&self, name: &str) -> Option<&Type>
    {
        self.find_modelem(name)
            .and_then(|node| {
                match &*node.node {
                    Ast::ConstVal(Val::Type(typeval)) => Some(typeval),
                    _ => None,
                }
            })
            .or_else(|| BUILTIN_TYPES.get(name))
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
        node: &AstNode,
        opens: &TypeArgSlice,
    ) -> Lresult<Type>
    {
        Ok(match &*node.node {
            Ast::Id(id) if struple::contains_key(opens, id) => {
                Type::open(Lstr::Sref(id))
            }
            Ast::Id(id) => {
                if let Some(sub) = struple::find(&self.submods, id) {
                    if let Some(modelem) = sub.find_modelem(MODNAME_DATATYPE) {
                        ltry!(self.ast_to_type(modelem, opens))
                    } else {
                        return Err(rustfail!(
                            "compile_error",
                            "{} is not a type at {:?}",
                            id,
                            node.loc
                        ));
                    }
                } else if let Some(modelem) = self.find_modelem(id) {
                    ltry!(self.ast_to_type(modelem, opens))
                } else if let Some(dt) = BUILTIN_TYPES.get(id) {
                    dt.clone()
                } else {
                    return Err(rustfail!(
                        "internal_failure",
                        "should this have been found? {}",
                        id,
                    ));
                }
            }
            Ast::List(inner_items) if inner_items.len() == 1 => {
                let inner = &inner_items.first().unwrap().v;
                let inner_t = ltry!(self.ast_to_type(inner, opens));
                Type::list(inner_t)
            }
            Ast::Tuple(inner_items) => {
                let inner_t: Lresult<TypeArgs> = inner_items
                    .iter()
                    .enumerate()
                    .map(|(i, item)| {
                        let klstr = item.k.map(|s| Lstr::Sref(s));
                        let k = Type::unwrap_name(&klstr, i);
                        let v = ltry!(self.ast_to_type(&item.v, opens));
                        Ok(StrupleItem::new(k, v))
                    })
                    .collect();
                Type::tuple(inner_t?)
            }
            Ast::FuncType(args, result) => {
                ltry!(self.ast_to_ftype(args, result, opens))
            }
            Ast::Generic(base, typeargs) => {
                let genbase = ltry!(self.ast_to_type(base, opens));
                if genbase.argc() > 0 {
                    panic!("generic inner generic: {}", genbase);
                }
                let genargsr: Lresult<TypeArgs> = typeargs
                    .iter()
                    .enumerate()
                    .map(|(i, t)| {
                        let k = t.k.map(|s| Lstr::Sref(s));
                        Ok(StrupleItem::new(
                            Type::unwrap_name(&k, i),
                            ltry!(self.ast_to_type(&t.v, opens)),
                        ))
                    })
                    .collect();
                let genargs = genargsr?;
                Type::new(genbase.path.clone(), genargs)
            }
            Ast::ConstVal(cv) => cv.get_type(),
            Ast::Type(typ) => typ.clone(),
            Ast::Op2(".", module_node, id_node) => {
                match (&*module_node.node, &*id_node.node) {
                    (Ast::Id(m), Ast::Id(id)) => {
                        match self.imports.get(m) {
                            Some(canonical) => Type::from(canonical.join(id)?),
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
            Ast::Alias(_, src) => self.ast_to_type(src, opens)?,
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
        &self,
        result: &AstNode,
        args: &Xlist,
        opens: &TypeArgSlice,
    ) -> Lresult<Type>
    {
        let arg_types = self.xlist_to_types(args, &opens)?;
        let result_type = ltry!(self.ast_to_type(&result, opens));
        let gens = Vec::from(opens);
        Ok(Type::generic_f(gens, result_type, arg_types))
    }

    fn xlist_to_types(
        &self,
        args: &Xlist,
        opens: &TypeArgSlice,
    ) -> Lresult<TypeArgs>
    {
        let arg_types_r: Lresult<TypeArgs>;
        arg_types_r = args
            .into_iter()
            .enumerate()
            .map(|(idx, i)| {
                let klstr = i.k.map(|k| Lstr::Sref(k));
                Ok(StrupleItem::new(
                    Type::unwrap_name(&klstr, idx),
                    ltry!(self.ast_to_type(&i.v, opens)),
                ))
            })
            .collect();
        Ok(arg_types_r?)
    }

    /// add an implemented trait to this data module
    fn put_impl(&mut self, sub: ProtoModule) -> Lresult<()>
    {
        // is it worth it to check that this is in fact a data module?
        let trait_path = sub.trait_t.as_ref().unwrap().t.path.clone();
        struple::push_unique(&mut self.implementations, trait_path, sub)
    }
}

pub fn find_builtin(id: &str) -> Option<&'static Ast>
{
    BUILTIN_SCOPE.get(id)
}

#[derive(Debug)]
pub struct ProtoLib
{
    protos: HashMap<PathBuf, ProtoModule>,
}

impl ProtoLib
{
    pub fn new() -> ProtoLib
    {
        ProtoLib {
            protos: HashMap::new(),
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
        let proto = ltry!(ProtoModule::new(modkey.clone(), modtxt)
            .map_err(|e| { e.lstr_loc(modkey.best_path(), 0) }));
        self.put_module(modpath, proto)?;
        Ok(())
    }

    fn put_module(
        &mut self,
        modpath: &Path,
        mut proto: ProtoModule,
    ) -> Lresult<()>
    {
        for sub in proto.submods.drain(..).map(|s| s.v) {
            let submodname = sub.key.name.clone();
            if sub.key.mtyp == ModTyp::Impl {
                {
                    let trait_c = &sub.trait_t.as_ref().unwrap().t.path;
                    let trait_path = Path::new(trait_c.as_str());
                    let trait_proto =
                        self.protos.get_mut(trait_path).ok_or_else(|| {
                            rustfail!(
                                "leema_failure",
                                "cannot find trait: {}",
                                trait_c,
                            )
                        })?;
                    let data_path = &sub.data_t.as_ref().unwrap().t.path;
                    trait_proto.implementors.insert(data_path.clone());
                }

                {
                    let data_c = &sub.data_t.as_ref().unwrap().t.path;
                    let data_path = Path::new(data_c.as_str());
                    let data_proto =
                        self.protos.get_mut(data_path).ok_or_else(|| {
                            rustfail!(
                                "leema_failure",
                                "cannot find datatype: {}",
                                data_c,
                            )
                        })?;
                    data_proto.put_impl(sub)?;
                }
            } else {
                self.put_module(submodname.as_path(), sub)?;
            }
        }

        self.protos.insert(modpath.to_path_buf(), proto);
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
        elem: &str,
        loc: Loc,
    ) -> Lresult<&AstNode>
    {
        let proto = lfctx!(self.path_proto(modname), "elem": Lstr::from(elem.to_string()), "line": lstrf!("{}", loc.lineno));
        if proto.localdef.contains(elem) {
            return Err(Failure::static_leema(
                failure::Mode::CompileFailure,
                lstrf!("module element is not exported {} {}", modname, elem),
                modname.to_lstr(),
                loc.lineno,
            ));
        }
        proto.modscope.get(elem).ok_or_else(|| {
            Failure::static_leema(
                failure::Mode::CompileFailure,
                lstrf!("module element not found {} {}", modname, elem),
                modname.to_lstr(),
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
    ) -> Lresult<&mut ProtoModule>
    {
        self.protos.get_mut(path.as_path()).ok_or_else(|| {
            rustfail!(PROTOFAIL, "module not loaded: {:?}", path)
        })
    }

    /// take the type and source for the given func
    /// seems like first parameter isn't used, could optimize by removing it
    pub fn take_func(&mut self, f: &Fref) -> Lresult<(Canonical, AstNode)>
    {
        if f.m.mtyp == ModTyp::Impl {
            let ftyp = f.t.func_ref().unwrap();
            let self_arg = ftyp.args.first().unwrap();
            if self_arg.k.as_str() != "self" {
                return Err(rustfail!(
                    "leema_failure",
                    "first method argument isn't self: {}:{}",
                    self_arg.k,
                    self_arg.v,
                ));
            }
            let proto = self.path_proto_mut(&self_arg.v.path)?;
            let name = proto.key.name.clone();
            Ok((name, ltry!(proto.take_method(&f))))
        } else {
            let proto = self.path_proto_mut(&f.m.name)?;
            let name = proto.key.name.clone();
            Ok((name, ltry!(proto.take_func(&f))))
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
    use crate::leema::val::{Type, Val};

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
        let tvt = Type::open(Lstr::Sref("T"));

        assert_eq!(1, proto.modscope.len());
        assert!(proto.modscope.contains_key("first"));
        assert_eq!(
            Type::generic_f(
                vec![StrupleItem::new(
                    Lstr::Sref("T"),
                    Type::open(Lstr::Sref("T"))
                )],
                tvt.clone(),
                vec![
                    StrupleItem::new(Lstr::Sref("a"), tvt.clone()),
                    StrupleItem::new(Lstr::Sref("b"), tvt.clone()),
                ],
            ),
            proto.modscope.get("first").unwrap().typ,
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

        let point_type = proto.modscope.get("Point").expect("no Point type");
        /*
        let expected = Type::Generic(
            Box::new(user_type!("/foo/Point")),
            vec![StrupleItem::new("T", Type::open("T"))],
        );
        */
        assert_eq!(Ast::Canonical(canonical!("/foo/Point")), *point_type.node);
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
        assert_eq!(*"/foo/tacos", proto.imports["tacos"]);
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
        assert_eq!(0, a.imports.len());
        assert_eq!(1, a.modscope.len());
        assert_eq!(0, a.localdef.len());
        assert!(a.modscope.contains_key("foo"));
        assert!(!a.modscope.contains_key("bar"));

        let b = protos.path_proto(&canonical!("/b")).unwrap();
        assert_eq!(1, b.imports.len());
        assert_eq!(2, b.modscope.len());
        assert_eq!(1, b.localdef.len());
        assert!(b.modscope.contains_key("foo"));
        assert!(b.modscope.contains_key("bar"));
    }

    #[test]
    fn test_proto_token()
    {
        let proto = new_proto("datatype Burrito --");
        let expected_type = user_type!("/foo/Burrito");
        let burrito_val =
            proto.find_modelem("Burrito").expect("no Burrito const");
        assert_matches!(*burrito_val.node, Ast::ConstVal(_));
        if let Ast::ConstVal(ref val) = &*burrito_val.node {
            assert_eq!(Val::Token(expected_type), *val);
        }
    }

    #[test]
    fn test_proto_struct()
    {
        let proto = new_proto("datatype Point :: x:Int y:Int --");
        let point_type = proto.modscope.get("Point").expect("no Point type");
        assert_eq!(Ast::Canonical(canonical!("/foo/Point")), *point_type.node);
    }

    #[test]
    fn test_proto_alias_type()
    {
        let proto = new_proto("datatype Burrito := Int");
        let burrito =
            struple::find(&proto.submods, "Burrito").expect("no Burrito alias");

        assert_eq!(
            "/core/Int",
            burrito.data_t.as_ref().unwrap().t.path.as_str()
        );
    }
}
