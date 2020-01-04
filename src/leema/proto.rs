use crate::leema::ast2::{Ast, AstNode, DataType, Loc, ModAction, ModTree, Xlist};
use crate::leema::failure::{self, Failure, Lresult};
use crate::leema::grammar2::Grammar;
use crate::leema::loader::Interloader;
use crate::leema::lstr::Lstr;
use crate::leema::module::{CanonicalMod, ImportedMod, ModAlias, ModKey, ModRelativity, TypeMod};
use crate::leema::struple::{self, Struple2, StrupleItem, StrupleKV};
use crate::leema::token::Tokenz;
use crate::leema::val::{Fref, FuncType, GenericTypes, Type, Val};

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use lazy_static::lazy_static;


const PROTOFAIL: &'static str = "prototype_failure";

lazy_static! {
    static ref DEFAULT_IDS: HashMap<&'static str, CanonicalMod> = {
        let core_mod = canonical_mod!("/core");
        let mut ids = HashMap::new();
        ids.insert("Bool", core_mod.clone());
        ids.insert("cons", core_mod.clone());
        ids.insert("create_failure", core_mod.clone());
        ids.insert("fail", core_mod.clone());
        ids.insert("Hashtag", core_mod.clone());
        ids.insert("Int", core_mod.clone());
        ids.insert("Str", core_mod.clone());
        ids.insert("#", core_mod);
        ids
    };
}


/// Asts separated into their types of components
#[derive(Debug)]
pub struct ProtoModule
{
    pub key: ModKey,
    pub imports: HashMap<ModAlias, ImportedMod>,
    pub exports: HashMap<ModAlias, ImportedMod>,
    pub id_canonicals: HashMap<&'static str, CanonicalMod>,
    pub mod_canonicals: HashMap<ModAlias, CanonicalMod>,
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
        let mut grammar = Grammar::new(Tokenz::lexp(src)?);
        grammar.set_path(key.best_path());
        let items = grammar.parse_module()?;

        let mut proto = ProtoModule {
            key,
            imports: HashMap::new(),
            exports: HashMap::new(),
            id_canonicals: HashMap::new(),
            mod_canonicals: HashMap::new(),
            macros: HashMap::new(),
            constants: HashMap::new(),
            types: HashMap::new(),
            funcseq: Vec::new(),
            funcsrc: HashMap::new(),
            token: HashSet::new(),
            struct_fields: HashMap::new(),
        };

        let mut exports = HashMap::new();
        let mut imports = HashMap::new();
        let mut it = items.into_iter().peekable();
        while it.peek().is_some() {
            if let Ast::ModAction(_, _) = &*it.peek().unwrap().node {
                // another import, take the next
            } else {
                // no more imports, exports. move on to the regular stuff
                break;
            }

            let i = it.next().unwrap();
            match *i.node {
                Ast::ModAction(ModAction::Import, tree) => {
                    tree.collect(&mut imports)?;
                }
                Ast::ModAction(ModAction::Export, tree) => {
                    tree.collect(&mut exports)?;
                }
                _ => {
                    panic!("not an import or export");
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

            proto.imports.insert(ModAlias::new(k), v);
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

            proto.imports.insert(ModAlias::new(k), v.clone());
            proto.exports.insert(ModAlias::new(k), v);
        }

        for i in it {
            match *i.node {
                Ast::DefConst(name, val) => {
                    ltry!(proto.refute_redefines_default(name, i.loc));
                    proto.constants.insert(name, val);
                }
                Ast::DefMacro(macro_name, _, _) => {
                    ltry!(proto.refute_redefines_default(macro_name, i.loc));
                    proto.macros.insert(macro_name, *i.node);
                }
                Ast::DefFunc(name, args, result, body) => {
                    proto.add_func(name, args, result, body)?;
                }
                Ast::DefType(DataType::Struct, name, fields) => {
                    if fields.is_empty() {
                        ltry!(proto.add_token(name));
                    } else {
                        ltry!(proto.add_struct(name, fields));
                    }
                }
                Ast::DefType(DataType::Union, name, variants) => {
                    proto.add_union(name, variants)?;
                }
                Ast::ModAction(_, _) => {
                    return Err(Failure::static_leema(
                        failure::Mode::CompileFailure,
                        Lstr::Sref("imports must come before definitions"),
                        proto.key.best_path(),
                        i.loc.lineno,
                    ));
                }
                _ => {
                    return Err(Failure::static_leema(
                        failure::Mode::CompileFailure,
                        lstrf!("expected module statement, found {:?}", i),
                        proto.key.best_path(),
                        i.loc.lineno,
                    ));
                }
            }
        }

        for (k, v) in proto.exports.iter() {
            // if it's not defined locally, must be a child module
            // so import it
            if proto.defines(k.str()) {
                // importing a child module over a local definition
                return Err(Failure::static_leema(
                    failure::Mode::CompileFailure,
                    lstrf!("local definition cannot shadow export: {}", v),
                    proto.key.best_path(),
                    0,
                ));
            }
        }

        // export any local definitions
        for name in proto.macros.keys().chain(proto.funcseq.iter()) {
            let export_path = PathBuf::from(*name);
            proto.exports.insert(ModAlias(name), ImportedMod(export_path));
        }

        Ok(proto)
    }

    fn refute_redefines_default(
        &self,
        id: &'static str,
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

    pub fn default_imports() -> &'static HashMap<&'static str, CanonicalMod>
    {
        &DEFAULT_IDS
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
        let type_maker: Box<dyn Fn(FuncType) -> Type>;

        let name_id = match *name.node {
            Ast::Id1(name_id) => {
                ltry!(self.refute_redefines_default(name_id, name.loc));
                opens = vec![];
                type_maker = Box::new(|ft| Type::Func(ft));
                name_id
            }
            Ast::Generic(gen, gen_args) => {
                let opens1: Lresult<GenericTypes> = gen_args
                    .iter()
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
                    ltry!(self.refute_redefines_default(name_id, name.loc));
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

        let ftyp = self.ast_to_ftype(m, &args, &result, &opens)?;
        let call_args = ftyp.call_args();
        let typ = type_maker(ftyp);
        let fref = Fref::new(self.key.clone(), name_id, typ.clone());
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
                ltry!(self.refute_redefines_default(name_id, name.loc));
                struct_typ = Type::User(TypeMod::from(m), name_id);
                opens = vec![];
                name_id
            }
            Ast::Generic(gen, gen_args) => {
                let opens1: Lresult<GenericTypes> = gen_args
                    .iter()
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
                    ltry!(self.refute_redefines_default(name_id, name.loc));
                    let itmod = TypeMod::from(m);
                    let inner = Type::User(itmod, name_id);
                    struct_typ =
                        Type::Generic(true, Box::new(inner), opens.clone());
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

        let args = self.xlist_to_types(&m, &fields, &opens)?;
        let ftyp = FuncType::new(args, struct_typ.clone());
        let call_args = ftyp.call_args();
        let constructor_type = Type::Func(ftyp);
        let fref = Fref::new(self.key.clone(), sname_id, constructor_type);
        let constructor_call = Val::Call(fref, call_args);
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
                ltry!(self.refute_redefines_default(name_id, name.loc));
                let tok_mod = TypeMod::from(&self.key.name);
                let t = Type::User(tok_mod, name_id);
                let token_val = Val::Token(t.clone());
                let const_node = AstNode::new_constval(token_val, name.loc);
                self.types.insert(name_id, t);
                self.token.insert(name_id);
                self.constants.insert(name_id, const_node);
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

    fn add_union(&mut self, name: AstNode, variants: Xlist) -> Lresult<()>
    {
        if variants.is_empty() {
            return Err(rustfail!(
                PROTOFAIL,
                "union must have at least variant variant: {:?}",
                name,
            ));
        }
        // later ltry!(self.refute_redefines_default(name_id, name.loc));
        // proto.types.push(i);
        Ok(())
    }

    fn add_import(&mut self, tree: ModTree) -> Lresult<()>
    {
        let mut imports = HashMap::new();
        tree.collect(&mut imports)?;
        for (k, (v, loc)) in imports.into_iter() {
            self.refute_redefines_default(k, loc)?;

            if self.exports.contains_key(k) && self.imports.contains_key(k) {
                return Err(Failure::static_leema(
                    failure::Mode::CompileFailure,
                    lstrf!("duplicate import: {}", v),
                    self.key.best_path(),
                    loc.lineno,
                ));
            }
            self.imports.insert(ModAlias(k), v);
        }
        Ok(())
    }

    fn add_export(&mut self, tree: ModTree) -> Lresult<()>
    {
        let mut exports = HashMap::new();
        tree.collect(&mut exports)?;
        for (k, (v, loc)) in exports.into_iter() {
            self.refute_redefines_default(k, loc)?;

            if self.exports.contains_key(k) && self.imports.contains_key(k) {
                return Err(Failure::static_leema(
                    failure::Mode::CompileFailure,
                    lstrf!("duplicate export: {}", v),
                    self.key.best_path(),
                    loc.lineno,
                ));
            }
            self.exports.insert(ModAlias(k), v);
        }
        Ok(())
    }

    pub fn type_modules(&self) -> Lresult<Vec<ProtoModule>>
    {
        Ok(vec![])
    }

    pub fn set_canonical(
        &mut self,
        key: ModAlias,
        canonical: CanonicalMod,
        is_id: bool,
    )
    {
        if is_id {
            self.id_canonicals.insert(key.0, canonical);
        } else {
            self.mod_canonicals.insert(key, canonical);
        }
    }

    /// TODO rename this. Maybe take_func?
    pub fn pop_func(&mut self, func: &str) -> Option<(Xlist, AstNode)>
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

    /// Check if this module defines a function, type or macro of this name
    pub fn defines(&self, id: &str) -> bool
    {
        self.constants.contains_key(id)
            || self.macros.contains_key(id)
            || self.types.contains_key(id)
    }

    /// Check if this module defines a type
    pub fn defines_type(&self, id: &str) -> bool
    {
        self.types.contains_key(id)
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

    pub fn imported_module(&self, alias: &ModAlias) -> Lresult<&CanonicalMod>
    {
        self.mod_canonicals
            .get(alias)
            .ok_or_else(|| {
                rustfail!(
                    PROTOFAIL,
                    "no module imported as {} from {}",
                    alias,
                    self.key.name,
                )
            })
    }

    /// get the canonical module for an id
    pub fn canonical_mod_for_id(&self, id: &str) -> Option<&CanonicalMod>
    {
        self.id_canonicals
            .get(id)
            .or_else(|| DEFAULT_IDS.get(id))
    }

    pub fn ast_to_type(
        &self,
        local_mod: &CanonicalMod,
        node: &AstNode,
        opens: &[StrupleItem<&'static str, Type>],
    ) -> Lresult<Type>
    {
        Ok(match &*node.node {
            // Ast::Id1("Bool") => Type::BOOL,
            // Ast::Id1("Int") => Type::INT,
            // Ast::Id1("Str") => Type::STR,
            // Ast::Id1("#") => Type::HASHTAG,
            Ast::Id1(id) if struple::contains_key(opens, id) => Type::OpenVar(id),
            Ast::Id1(id) => Type::User(Lstr::EMPTY, id),
            Ast::Id2(alias, id) => {
                Type::User(TypeMod::from(*alias), id)
            }
            Ast::List(inner_items) if inner_items.len() == 1 => {
                let inner = &inner_items.first().unwrap().v;
                let inner_t = self.ast_to_type(local_mod, inner, opens)?;
                Type::StrictList(Box::new(inner_t))
            }
            Ast::Tuple(inner_items) => {
                let inner_t: Lresult<Vec<StrupleItem<Option<Lstr>, Type>>> =
                    inner_items
                        .iter()
                        .map(|item| {
                            let k = item.k.map(|ik| Lstr::Sref(ik));
                            let v = self.ast_to_type(local_mod, &item.v, opens)?;
                            Ok(StrupleItem::new(k, v))
                        })
                        .collect();
                Type::Tuple(inner_t?)
            }
            Ast::FuncType(args, result) => {
                let ftype = self.ast_to_ftype(local_mod, args, result, opens)?;
                Type::Func(ftype)
            }
            Ast::Generic(_, typeargs) => {
                let _gen =
                    struple::map_v(typeargs, |v| {
                        self.ast_to_type(local_mod, v, opens)
                    });
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
        &self,
        local_mod: &CanonicalMod,
        args: &Xlist,
        result: &AstNode,
        opens: &[StrupleItem<&'static str, Type>],
    ) -> Lresult<FuncType>
    {
        let arg_types = self.xlist_to_types(local_mod, args, &opens)?;
        let result_type = self.ast_to_type(local_mod, &result, opens)?;
        Ok(FuncType::new(arg_types, result_type))
    }

    fn xlist_to_types(
        &self,
        local_mod: &CanonicalMod,
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
                    self.ast_to_type(local_mod, &i.v, opens)?,
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
        let modkey = ModKey::from(CanonicalMod::from(modname));
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
    /// base_path, postfix = mod_path.split()
    /// load_canonical(base_path)
    /// load_relative(base_path, postfix)
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
    /// load_relative(base_path, rel_path)
    /// next_prefix, postfix = relative_postfix.split()
    /// base_proto = find_proto(base_path)
    /// canonical_path = base_proto.exports[next_prefix]
    /// if next_path is relative:
    ///   load_child(canonical_path, next_path + postfix)
    /// if next_path is absolute:
    ///   load_absolute(canonical_path + next_tail)
    pub fn load_absolute(
        &mut self,
        loader: &mut Interloader,
        mod_path: &Path,
    ) -> Lresult<(CanonicalMod, bool)>
    {
        vout!("ProtoLib::load_absolute({})\n", mod_path.display());
        let (head, tail) = ImportedMod::head(mod_path);
        ltry!(self.load_canonical(loader, Path::new(&head)));
        self.load_relative(loader, Path::new(&head), tail)
    }

    pub fn load_child(
        &mut self,
        loader: &mut Interloader,
        base_path: &Path,
        child_path: &Path,
    ) -> Lresult<(CanonicalMod, bool)>
    {
        vout!("ProtoLib::load_child({:?}, {:?})\n", base_path.display(), child_path.display());
        let (head, tail) = ImportedMod::head(child_path);
        let next_base = base_path.join(head);
        ltry!(self.load_canonical(loader, &next_base));
        self.load_relative(loader, &next_base, tail)
    }

    pub fn load_relative(
        &mut self,
        loader: &mut Interloader,
        base_path: &Path,
        next_path: &Path,
    ) -> Lresult<(CanonicalMod, bool)>
    {
        if ImportedMod::is_empty(next_path) {
            let cmod = CanonicalMod::from(base_path);
            return Ok((cmod, false));
        }
        let (head, tail) = ImportedMod::head(next_path);
        let base_proto = ltry!(self._path_proto(base_path));

        // this isn't necessarily canonical, it might be shortcutted later on
        let head_export = match base_proto.exports.get(head) {
            Some(exp_head) => {
                if ImportedMod::is_empty(tail) {
                    exp_head.0.clone()
                } else {
                    exp_head.0.join(tail)
                }
            }
            None => {
                return Err(rustfail!(
                    PROTOFAIL,
                    "submodule {:?} not exported from {}",
                    head,
                    base_path.display(),
                ));
            }
        };

        match ImportedMod::path_relativity(&head_export) {
            ModRelativity::Absolute => {
                Err(rustfail!(
                    PROTOFAIL,
                    "cannot load absolute module relatively: {}",
                    head_export.display(),
                ))
            }
            ModRelativity::Child => {
                lfailoc!(self.load_child(
                    loader,
                    base_path,
                    &head_export,
                ))
            }
            ModRelativity::Sibling => {
                Err(rustfail!(
                    PROTOFAIL,
                    "cannot load sibling module as child: {} / {}",
                    base_path.display(),
                    head_export.display(),
                ))
            }
            ModRelativity::Local => {
                // already loaded, no need to load deeper
                let cmod = CanonicalMod::from(base_path);
                Ok((cmod, true))
            }
        }
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

    fn put_module(&mut self, modpath: &Path, proto: ProtoModule) -> Lresult<()>
    {
        let type_mods = proto.type_modules()?;
        self.protos.insert(modpath.to_path_buf(), proto);
        for tm in type_mods.into_iter() {
            self.protos.insert(tm.key.name.mod_path().to_path_buf(), tm);
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
        let mut imported: Vec<(ModAlias, ImportedMod)> = vec![];
        {
            let proto = self.protos.get(modname).ok_or_else(|| {
                rustfail!(
                    PROTOFAIL,
                    "an import module does not exist: {}",
                    modname.display(),
                )
            })?;
            for (k, i) in proto.imports.iter() {
                if i.0 == *modname {
                    return Err(rustfail!(
                        PROTOFAIL,
                        "a module cannot import itself: {}",
                        i,
                    ));
                }
                imported.push((*k, i.clone()));
            }
        }

        let mut canonicals = vec![];
        for (k, i) in imported.into_iter() {
            let result = match i.relativity() {
                ModRelativity::Absolute => {
                    ltry!(self.load_absolute(loader, &i.0))
                }
                ModRelativity::Child => {
                    ltry!(self.load_child(loader, modname.clone(), &i.0))
                }
                ModRelativity::Sibling => {
                    let parent = modname.parent().unwrap();
                    ltry!(self.load_child(loader, parent, &i.0))
                }
                ModRelativity::Local => {
                    panic!("cannot import locally defined identifiers: {}", i);
                }
            };
            canonicals.push((k, result.0, result.1));
        }

        // safe to unwrap b/c verified this module is present above
        let proto_mut = self.protos.get_mut(modname).unwrap();
        for (k, c, is_id) in canonicals.into_iter() {
            proto_mut.set_canonical(k, c, is_id);
        }
        Ok(())
    }

    pub fn pop_func(
        &mut self,
        module: &CanonicalMod,
        func: &str,
    ) -> Lresult<Option<(Xlist, AstNode)>>
    {
        self.protos
            .get_mut(module.mod_path())
            .ok_or_else(|| {
                rustfail!(PROTOFAIL, "could not find module: {}", module,)
            })
            .map(|protomod| protomod.pop_func(func))
    }

    pub fn imported_proto(
        &self,
        proto: &CanonicalMod,
        alias: &ModAlias,
    ) -> Lresult<&ProtoModule>
    {
        let modpath = proto.mod_path();
        let protomod = self.protos.get(modpath).ok_or_else(|| {
            rustfail!(PROTOFAIL, "module not loaded: {:?}", proto)
        })?;
        let new_path = ltry!(protomod.imported_module(alias));
        self.path_proto(&new_path)
    }

    pub fn path_proto(&self, path: &CanonicalMod) -> Lresult<&ProtoModule>
    {
        self._path_proto(path.mod_path())
    }

    fn _path_proto(&self, path: &Path) -> Lresult<&ProtoModule>
    {
        self.protos.get(path).ok_or_else(|| {
            rustfail!(PROTOFAIL, "module not loaded: {:?}", path)
        })
    }
}


#[cfg(test)]
mod tests
{
    use super::{ProtoLib, ProtoModule};
    use crate::leema::ast2::Ast;
    use crate::leema::loader::Interloader;
    use crate::leema::lstr::Lstr;
    use crate::leema::module::{
        Chain, ModKey, ModPath,
        ModRelativity::{Absolute, Child, Sibling},
    };
    use crate::leema::struple::{self, StrupleItem};
    use crate::leema::val::{FuncType, Type, Val};

    use matches::assert_matches;


    fn new_proto(input: &'static str) -> ProtoModule
    {
        let key = ModKey::from("foo");
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
        func swap[T] a:T b:T /(T T)
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
        let proto = new_proto("datatype Point[T] x:T y:T --");

        let point_type = proto.types.get("Point").expect("no Point type");
        let expected = Type::Generic(
            true,
            Box::new(Type::User(Lstr::Sref("foo"), "Point")),
            vec![StrupleItem::new("T", Type::Unknown)],
        );
        assert_eq!(expected, *point_type);
    }

    #[test]
    fn test_proto_imports()
    {
        let proto = new_proto(
            "
        import /tacos
        import >>
            burritos
            tortas >>
                huevos >>
                    .
                    rancheros
                --
                enchiladas
            --
            ../nachos
        --
        ",
        );

        assert_eq!(6, proto.imports.len());
        assert_eq!(0, proto.exports.len());

        assert_eq!(Absolute, proto.imports["tacos"].relativity);
        assert_eq!(Child, proto.imports["burritos"].relativity);
        assert_eq!(Child, proto.imports["huevos"].relativity);
        assert_eq!(Child, proto.imports["rancheros"].relativity);
        assert_eq!(Child, proto.imports["enchiladas"].relativity);
        assert_eq!(Sibling, proto.imports["nachos"].relativity);

        assert_eq!(Chain::from("tacos"), proto.imports["tacos"].path);
        assert_eq!(Chain::from("burritos"), proto.imports["burritos"].path);
        assert_eq!(Chain::from("tortas/huevos"), proto.imports["huevos"].path);
        assert_eq!(
            Chain::from("tortas/huevos/rancheros"),
            proto.imports["rancheros"].path
        );
        assert_eq!(
            Chain::from("tortas/enchiladas"),
            proto.imports["enchiladas"].path
        );
        assert_eq!(Chain::from("nachos"), proto.imports["nachos"].path);
    }

    #[test]
    fn test_proto_exports()
    {
        let proto = new_proto(
            "
        export tacos
        ",
        );

        assert_eq!(1, proto.imports.len());
        assert_eq!(1, proto.exports.len());
        assert_eq!(Child, proto.imports["tacos"].relativity);
        assert_eq!(Child, proto.exports["tacos"].relativity);
        assert_eq!(Chain::from("tacos"), proto.imports["tacos"].path);
        assert_eq!(Chain::from("tacos"), proto.exports["tacos"].path);
    }

    #[test]
    fn test_import_basic_exports()
    {
        let a = "
        import >>
            b
            c >>
                d
            --
        --
        "
        .to_string();
        let b = "
        func foo >> 3 --
        "
        .to_string();
        let c = "export >>
            d
        --
        "
        .to_string();
        let d = "
        func bar >> 5 --
        "
        .to_string();

        let mut loader = Interloader::default();
        loader.set_mod_txt(ModKey::from("a"), a);
        loader.set_mod_txt(ModKey::from("a/b"), b);
        loader.set_mod_txt(ModKey::from("a/c"), c);
        loader.set_mod_txt(ModKey::from("a/c/d"), d);

        let mut protos = ProtoLib::new();
        protos.load_absolute(&mut loader, Chain::from("a")).unwrap();
        protos.load_imports(&mut loader, &Chain::from("a")).unwrap();

        assert_eq!(4, protos.protos.len());
    }

    #[test]
    fn test_import_skipped_export()
    {
        let a = "import b/d".to_string();
        let b = "export c/d".to_string();
        let c = "export d".to_string();
        let d = "
        func bar >> 5 --
        "
        .to_string();

        let mut loader = Interloader::default();
        loader.set_mod_txt(ModKey::from("a"), a);
        loader.set_mod_txt(ModKey::from("a/b"), b);
        loader.set_mod_txt(ModKey::from("a/b/c"), c);
        loader.set_mod_txt(ModKey::from("a/b/c/d"), d);

        let mut protos = ProtoLib::new();
        protos.load_absolute(&mut loader, Chain::from("a")).unwrap();
        protos.load_imports(&mut loader, &Chain::from("a")).unwrap();

        assert_eq!(4, protos.protos.len());
    }

    #[test]
    fn test_import_absolute_and_sibling()
    {
        let a = "
        export b
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
        loader.set_mod_txt(ModKey::from("a"), a);
        loader.set_mod_txt(ModKey::from("a/b"), b);
        loader.set_mod_txt(ModKey::from("a/c"), c);
        loader.set_mod_txt(ModKey::from("d"), d);
        let mut protos = ProtoLib::new();

        protos.load_absolute(&mut loader, Chain::from("a")).unwrap();
        protos.load_imports(&mut loader, &Chain::from("a")).unwrap();
        assert_eq!(2, protos.protos.len());

        protos
            .load_absolute(&mut loader, Chain::from("a/b"))
            .unwrap();
        protos
            .load_imports(&mut loader, &Chain::from("a/b"))
            .unwrap();
        assert_eq!(4, protos.protos.len());
    }

    #[test]
    #[should_panic]
    fn test_not_exported()
    {
        let a = "
        import b
        "
        .to_string();
        let b = "
        func foo >> 3 --
        "
        .to_string();

        let mut loader = Interloader::default();
        loader.set_mod_txt(ModKey::from("a"), a);
        loader.set_mod_txt(ModKey::from("a/b"), b);
        let mut protos = ProtoLib::new();

        protos.load_absolute(&mut loader, Chain::from("a")).unwrap();
        protos.load_imports(&mut loader, &Chain::from("a")).unwrap();
        assert_eq!(2, protos.protos.len());

        protos
            .load_absolute(&mut loader, Chain::from("a/b"))
            .expect("want this panic");
    }

    #[test]
    fn test_proto_import_exported_func()
    {
        let a = "
        func foo >> 4 --
        "
        .to_string();
        let b = "import /a/foo
        func bar >> foo() + 3 --
        "
        .to_string();

        let mut loader = Interloader::default();
        loader.set_mod_txt(ModKey::from("a"), a);
        loader.set_mod_txt(ModKey::from("b"), b);
        let mut protos = ProtoLib::new();

        protos.load_absolute(&mut loader, Chain::from("b")).unwrap();
        protos.load_imports(&mut loader, &Chain::from("b")).unwrap();
        assert_eq!(2, protos.protos.len());

        let a = protos.path_proto(&Chain::from("a")).unwrap();
        assert_eq!(0, a.imports.len());
        assert_eq!(1, a.exports.len());
        assert_eq!(ModPath::local(Chain::from("foo")), a.exports["foo"]);

        let b = protos.path_proto(&Chain::from("b")).unwrap();
        assert_eq!(0, b.imports.len());
        assert_eq!(1, b.imported_ids.len());
        assert_eq!(1, b.exports.len());
        assert_eq!(Chain::from("a"), b.imported_ids["foo"]);
    }

    #[test]
    fn test_proto_token()
    {
        let proto = new_proto("datatype Burrito --");

        let burrito_type = proto.types.get("Burrito").expect("no Burrito type");
        assert_eq!(Type::User(Lstr::from("foo"), "Burrito"), *burrito_type,);

        assert!(proto.token.contains("Burrito"));

        let burrito_val =
            proto.constants.get("Burrito").expect("no Burrito const");
        assert_matches!(*burrito_val.node, Ast::ConstVal(_));
        if let Ast::ConstVal(ref val) = &*burrito_val.node {
            assert_eq!(Val::Token(burrito_type.clone()), *val);
        }
    }

    #[test]
    fn test_proto_struct()
    {
        let proto = new_proto("datatype Point x:Int y:Int --");

        let point_type = proto.types.get("Point").expect("no Point type");
        assert_eq!(Type::User(Lstr::Sref("foo"), "Point"), *point_type);
    }
}
