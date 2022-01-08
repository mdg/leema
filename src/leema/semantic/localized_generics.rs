use crate::leema::ast2::{
    self, Ast, AstMode, AstNode, AstStep, Loc, StepResult,
};
use crate::leema::val::{Type, Val};
use crate::leema::Lstr;


pub struct LocalizeGenerics
{
    current_local: String,
    next_local_id: u32,
}

impl LocalizeGenerics
{
    pub fn new() -> LocalizeGenerics
    {
        LocalizeGenerics {
            current_local: String::new(),
            next_local_id: 0,
        }
    }

    fn localized_id(&mut self, loc: Loc) -> &str
    {
        let local_id = self.next_local_id;
        self.next_local_id += 1;
        self.current_local = format!("{}@{}", local_id, loc.lineno);
        &self.current_local
    }

    fn localize_type(&mut self, t: &mut Type, loc: Loc)
    {
        if !t.contains_open() && *t != Type::UNKNOWN {
            return;
        }
        t.localize_generics(self.localized_id(loc));
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
            Ast::Id(patt) if mode.is_pattern() => {
                if node.typ == Type::UNKNOWN {
                    let type_var = if *patt == "_" {
                        lstrf!("_{}", self.localized_id(node.loc))
                    } else {
                        Lstr::Sref(patt)
                    };
                    node.typ = Type::local(type_var);
                }
            }
            Ast::Id(id) => {
                // shouldn't have to localize ids
            }
            _ => {}
        }
        self.localize_type(&mut node.typ, node.loc);
        Ok(AstStep::Ok)
    }
}
