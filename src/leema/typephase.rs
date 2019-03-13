use leema::ast::{IfCase, IfType};
use leema::failure::{Failure, Lresult};
use leema::infer::Inferator;
use leema::inter::Blockstack;
use leema::lri::{Lri, ModLocalId, SpecialModId};
use leema::lstr::Lstr;
use leema::reg::{Reg, RegTable};
use leema::struple::{Struple, Struple2, StrupleItem, StrupleKV};
use leema::val::{SrcLoc, Type, Val};

use std::collections::{HashMap, HashSet};
use std::iter::FromIterator;




pub struct Semantics<'a>
{
    pub name: Lstr,
    imports: HashSet<Lstr>,
    pub types: HashMap<Lstr, HashMap<Lstr, Type>>,

    pub calls: HashSet<ModLocalId>,
    pub typecalls: HashSet<SpecialModId>,
    pub closed: Option<HashSet<Lstr>>,

    is_closure: bool,
    blocks: Blockstack,
    infer: Inferator<'a>,
    reg: RegTable,
}

impl<'a> Semantics<'a>
{
    pub fn new(name: &'a Lri) -> Semantics<'a>
    {
        Semantics {
            name: name.localid.clone(),
            imports: HashSet::new(),
            types: HashMap::new(),

            calls: HashSet::new(),
            typecalls: HashSet::new(),
            closed: None,

            is_closure: false,
            blocks: Blockstack::new(),
            infer: Inferator::new(&name),
            reg: RegTable::new(),
        }
    }

    pub fn map_node(&mut self, node: &AstNode) -> Lresult<AstNode>
    {
        match &*node.node {
            Ast::Block(ref items) => {
                if items.is_empty() {
                    let new_node = node
                        .replace(Ast::Block(Vec::with_capacity(0)), Type::Void);
                    return Ok(new_node);
                }

                let item_results: Vec<Lresult<AstNode>> =
                    items.iter().map(|i| self.map_node(i)).collect();
                let new_items: Vec<AstNode> = Lresult::from_iter(item_results)?;
                let last_type = {
                    let last_item = new_items.last().unwrap();
                    self.merge_types(&node.typ, &last_item.typ)?
                };
                self.replace(node, Ast::Block(new_items), last_type)
            }
            Ast::ModId(ref m, ref l) => {
                let (new_node, new_type) = self.map_modid(m, l)?;
                self.replace(node, new_node, new_type)
            }
            Ast::Type(ref t) => {
                self.replace(node, Ast::Type(t.clone()), Type::Kind)
            }
            Ast::TypeCall(ref id, ref args) => {
                let (new_node, new_type) = self.map_typecall(id, args)?;
                self.replace(node, new_node, new_type)
            }
            unknown => {
                return Err(rustfail!(
                    "incomplete_semantics",
                    "cannot analyze: {:?}",
                    unknown,
                ));
            }
        }
    }

    pub fn map_modid(
        &mut self,
        module: &Lstr,
        local: &Lstr,
    ) -> Lresult<(Ast, Type)>
    {
        let mod_types = self.types.get(module).ok_or_else(|| {
            Failure::new(
                "code_error",
                Lstr::from(format!("module not found: {}", module)),
            )
        })?;

        let found_type = mod_types.get(local).ok_or_else(|| {
            Failure::new(
                "code_error",
                Lstr::from(format!("{} not found in module {}", local, module)),
            )
        })?;

        match found_type {
            Type::Func(_ft) => {
                Err(Failure::new("debug", Lstr::Sref("is func")))
            }
            Type::GenericFunc(ref _tvars, ref _ft) => {
                let new_node = Ast::ModId(module.clone(), local.clone());
                Ok((new_node, found_type.clone()))
            }
            not_func => {
                Err(Failure::new(
                    "leema_incomplete",
                    Lstr::from(format!(
                        "cannot compile id yet {}::{}: {}",
                        module, local, not_func
                    )),
                ))
            }
        }
    }

    pub fn map_typecall(
        &mut self,
        id: &AstNode,
        args: &Struple2<AstNode>,
    ) -> Lresult<(Ast, Type)>
    {
        let new_id = self.map_node(id)?;
        let new_args: Struple2<Type> = args.map_v(|a| {
            let new_node = self.map_node(a)?;
            match *new_node.node {
                Ast::Type(t) => Ok(t),
                not_type => {
                    Err(rustfail!("leema_fail", "not a type: {:?}", not_type))
                }
            }
        })?;

        let (result, rtype) = match &new_id.typ {
            Type::GenericFunc(ref targs, ref ft) => {
                // ok, that's good
                let targs_len = targs.len();
                let new_args_len = new_args.len();
                if targs_len != new_args_len {
                    return Err(Failure::new(
                        "code_error",
                        Lstr::from(format!(
                            "expected {} arguments to {:?}, found {}",
                            targs_len, new_id, new_args_len
                        )),
                    ));
                }
                let special_types: StrupleKV<Lstr, Type> = targs
                    .iter()
                    .zip(new_args.into_iter())
                    .map(|arg| StrupleItem::new(arg.0.clone(), arg.1.v))
                    .collect();
                let arg_vals: Struple<Val> = special_types
                    .iter()
                    .map(|i| (Some(i.k.clone()), Val::Void))
                    .collect();
                let fri = match *new_id.node {
                    Ast::Id(ref localid) => Lri::new(localid.clone()),
                    Ast::ModId(ref modid, ref localid) => {
                        let mlid =
                            ModLocalId::new(modid.clone(), localid.clone());
                        let tctypes = special_types.clone();
                        let tcid = SpecialModId::new(mlid, tctypes);
                        self.typecalls.insert(tcid);
                        Lri::with_modules(modid.clone(), localid.clone())
                    }
                    not_id => {
                        return Err(Failure::new(
                            "code_failure",
                            Lstr::from(format!("not an id: {:?}", not_id)),
                        ));
                    }
                };
                // ft.replace_types(special_types);
                // *t = Type::SpecialFunc(special_types.clone(), ft);
                let spec_type = Type::SpecialFunc(special_types, ft.clone());
                let t2 = spec_type.clone();
                (Ast::ConstVal(Val::FuncRef(fri, arg_vals, spec_type)), t2)
            }
            Type::Func(_) => {
                return Err(Failure::new(
                    "code_error",
                    Lstr::from(format!(
                        "cannot pass type args to regular function: {:?}",
                        new_id
                    )),
                ));
            }
            not_func => {
                return Err(Failure::new(
                    "code_failure",
                    Lstr::from(format!(
                        "typecall id must be type func: {:?}",
                        not_func
                    )),
                ));
            }
        };
        Ok((result, rtype))
    }

    fn replace(
        &mut self,
        old: &AstNode,
        new_ast: Ast,
        new_typ: Type,
    ) -> Lresult<AstNode>
    {
        let m_type = self.merge_types(&old.typ, &new_typ)?;
        Ok(old.replace(new_ast, m_type))
    }

    pub fn merge_types(&mut self, a: &Type, b: &Type) -> Lresult<Type>
    {
        self.infer
            .merge_types(a, b)
            .map_err(|e| rustfail!("type_err", "type info: {}", e))
    }
}


#[cfg(test)]
mod tests
{
    use super::*;
    use leema::lri::Lri;
    use leema::lstr::Lstr;
    use leema::struple::StrupleKV;
    use leema::val::{FuncType, SrcLoc, Type};


    fn new_node(node: Ast) -> AstNode
    {
        AstNode::new(node, SrcLoc::default())
    }

    #[test]
    fn test_typecall()
    {
        let lri = Lri::with_modules(Lstr::Sref("a"), Lstr::Sref("b"));
        let t_str = Lstr::Sref("T");
        let u_str = Lstr::Sref("U");
        let x_str = Lstr::Sref("x");
        let y_str = Lstr::Sref("y");
        let dgentype = Type::GenericFunc(
            vec![t_str.clone(), u_str.clone()],
            FuncType::new(
                StrupleKV::from_vec(vec![
                    StrupleItem::new(Some(x_str), Type::Var(t_str)),
                    StrupleItem::new(Some(y_str), Type::Var(u_str.clone())),
                ]),
                Type::Var(u_str),
            ),
        );
        let mut mod_c = HashMap::new();
        mod_c.insert(Lstr::Sref("d"), dgentype);

        let mut sem = Semantics::new(&lri);
        sem.types.insert(Lstr::Sref("c"), mod_c);

        let typecall = new_node(Ast::TypeCall(
            new_node(Ast::ModId(Lstr::Sref("c"), Lstr::Sref("d"))),
            StrupleKV::from(vec![
                new_node(Ast::Type(Type::Int)),
                new_node(Ast::Type(Type::Str)),
            ]),
        ));
        let result = sem.map_node(&typecall).unwrap();

        assert_matches!(*result.node, Ast::ConstVal(_));
        if let Ast::ConstVal(val) = &*result.node {
            if let Val::FuncRef(fri, args, typ) = val {
                assert_eq!("c", fri.modules.as_ref().unwrap());
                assert_eq!("d", &fri.localid);
                assert_eq!(2, args.0.len());
                match typ {
                    Type::SpecialFunc(ref targs, ref _ft) => {
                        // field x specialized from T to Int
                        assert_eq!("T", &targs[0].k);
                        assert_eq!(Type::Int, targs[0].v);
                        // field y specialized from U to Str
                        assert_eq!("U", &targs[1].k);
                        assert_eq!(Type::Str, targs[1].v);
                    }
                    Type::GenericFunc(ref _targs, ref _ft) => {
                        panic!("found generic func, expected special func");
                    }
                    Type::Func(ref _ft) => {
                        panic!("found mono func, expected special func");
                    }
                    not_func => {
                        panic!("found a not func: {:?}", not_func);
                    }
                }
            } else {
                panic!("const val not a FuncRef")
            }
        } else {
            panic!("not a const val");
        }

        // even though it's not actually called at this point, it still needs
        // to be flagged for typechecking purposes. then the program can
        // decide whether to typecheck deeper now or later.
        let tcid = sem.typecalls.iter().next().unwrap();
        assert_eq!(1, sem.typecalls.len());
        assert_eq!("c", &tcid.id.module);
        assert_eq!("d", &tcid.id.local);
        let x = tcid.tparams.get(0).unwrap();
        let y = tcid.tparams.get(1).unwrap();
        assert_eq!(2, tcid.num_vars());
        assert_eq!("T", &x.k);
        assert_eq!("U", &y.k);
        assert_eq!(Type::Int, x.v);
        assert_eq!(Type::Str, y.v);

        assert!(sem.calls.is_empty());
    }
}
