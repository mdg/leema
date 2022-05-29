use crate::leema::ast2::{
    self, Ast, AstMode, AstNode, AstStep, Loc, StepResult,
};
use crate::leema::val::{Type, Val};
use crate::leema::Lstr;

pub struct LocalizeGenerics
{
    current_local: String,
    next_local_index: u32,
}

impl LocalizeGenerics
{
    pub fn new() -> LocalizeGenerics
    {
        LocalizeGenerics {
            current_local: String::new(),
            next_local_index: 0,
        }
    }

    fn localized_id(&mut self, loc: Loc) -> &str
    {
        let local_index = self.next_local_index;
        self.next_local_index += 1;
        self.current_local = format!("{}@{}", local_index, loc.lineno);
        &self.current_local
    }

    pub fn localize_type(&mut self, t: &mut Type, loc: Loc)
    {
        if !t.contains_open() {
            return;
        }
        t.localize_generics(self.localized_id(loc));
    }

    fn next_local_typevar(&mut self) -> Type
    {
        let local_index = self.next_local_index;
        self.next_local_index += 1;
        Type::local(lstrf!("local-{}", local_index))
    }
}

impl ast2::Op for LocalizeGenerics
{
    fn pre(&mut self, node: &mut AstNode, mode: AstMode) -> StepResult
    {
        match &mut *node.node {
            Ast::Type(t) => {
                self.localize_type(t, node.loc);
            }
            Ast::ConstVal(Val::Type(t)) => {
                self.localize_type(t, node.loc);
            }
            Ast::ConstVal(Val::Func(fref)) => {
                // for functions, if any types match function type args
                // replace them w/ the concrete types
                // if function defines any new type args, replace those
                // parameter types with local variabls
                if fref.contains_open() || node.typ.contains_open() {
                    let local_id = self.localized_id(node.loc);
                    fref.localize_generics(local_id);
                    node.typ.localize_generics(local_id);
                }
            }
            Ast::ConstVal(Val::Struct(t, _)) => {
                self.localize_type(t, node.loc);
            }
            Ast::ConstVal(Val::EnumStruct(t, _, _)) => {
                self.localize_type(t, node.loc);
            }
            Ast::ConstVal(Val::EnumToken(t, _)) => {
                self.localize_type(t, node.loc);
            }
            Ast::ConstVal(Val::Token(t)) => {
                self.localize_type(t, node.loc);
            }
            Ast::Generic(_base, _type_args) => {
                /*
                for ta in type_args.iter_mut() {
                    self.localize_node_with_id(&mut ta.v, local_id.clone());
                }
                self.localize_node_with_id(base, local_id);
                */
            }
            /*
            Ast::Call(callx, args) => {
                for ta in args.iter_mut() {
                    self.localize_node_with_id(&mut ta.v, local_id.clone());
                }
                self.localize_node_with_id(callx, local_id);
            }
            */
            Ast::Op2(_, ref mut _a, ref mut _b) => {
                // self.localize_node_with_id(a, local_id.clone());
                // self.localize_node_with_id(b, local_id.clone());
            }
            Ast::Wildcard if mode.is_pattern() => {
                let local_id = lstrf!("_{}", self.localized_id(node.loc));
                node.typ = Type::local(local_id);
            }
            Ast::Id(patt) if mode.is_pattern() => {
                if node.typ == Type::UNKNOWN {
                    let type_var = if *patt == "_" {
                        panic!("unexpected _");
                    } else {
                        Lstr::Sref(patt)
                    };
                    node.typ = Type::local(type_var);
                }
            }
            Ast::Id(_id) => {
                // shouldn't have to localize ids
            }
            _ => {}
        }
        Ok(AstStep::Ok)
    }

    fn post(&mut self, node: &mut AstNode, _mode: AstMode) -> StepResult
    {
        match &*node.node {
            Ast::Call(_callx, _args) => {}
            _ => {
                // clean up any leftover unknown types
                self.localize_type(&mut node.typ, node.loc);
                if node.typ == Type::UNKNOWN {
                    node.typ = self.next_local_typevar();
                }
            }
        }
        if node.typ.contains_open() {
            panic!("open node: {:#?}", *node);
        }
        Ok(AstStep::Ok)
    }
}
