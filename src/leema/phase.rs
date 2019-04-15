use crate::leema::ast2::AstNode;
use crate::leema::failure::Lresult;


trait Lphase
{
    fn map_node<'i>(&mut self, node: AstNode<'i>) -> Lresult<AstNode<'i>>;

    fn map_nodes<'i>(&mut self, nodes: Vec<AstNode<'i>>) -> Lresult<Vec<AstNode<'i>>>
    {
        nodes
            .into_iter()
            .map(|n| {
                self.map_node(n)
            })
            .collect()
    }
}
