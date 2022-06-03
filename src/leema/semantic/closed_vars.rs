use crate::leema::ast2::{self, Ast, AstMode, AstNode, AstStep, StepResult};
use crate::leema::struple;
use crate::leema::val::{Type, TypeArgSlice};

pub struct ClosedVars<'p>
{
    type_args: &'p TypeArgSlice,
}

impl<'p> ClosedVars<'p>
{
    pub fn new(type_args: &'p TypeArgSlice) -> ClosedVars<'p>
    {
        ClosedVars { type_args }
    }
}

impl<'p> ast2::Op for ClosedVars<'p>
{
    fn pre(&mut self, node: &mut AstNode, mode: AstMode) -> StepResult
    {
        node.typ.close_generics(self.type_args);
        match &mut *node.node {
            Ast::Type(t) => {
                t.close_generics(self.type_args);
            }
            Ast::Id(id) if mode == AstMode::Type => {
                if let Some(new_type) = struple::find(self.type_args, id) {
                    *node.node = Ast::Type(new_type.clone());
                    node.typ = Type::KIND;
                }
            }
            _ => {}
        }
        Ok(AstStep::Ok)
    }
}
