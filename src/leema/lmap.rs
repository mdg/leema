use Val;

use std::sync::Arc;

pub type LmapNode = Option<Arc<Lmap>>;

#[derive(Clone)]
#[derive(Debug)]
pub struct Lmap
(
    Option<Arc<Lmap>>, (Val, Val), Option<Arc<Lmap>>
);

impl Lmap
{
    pub fn new() -> LmapNode
    {
        None
    }

    pub fn insert(tree: &LmapNode, k: Val, v: Val) -> LmapNode
    {
        if tree.is_none() {
            return Some(Arc::new(Lmap(Lmap::new(), (k, v), Lmap::new())));
        }

        match **tree.as_ref().unwrap() {
            Lmap(ref left, (ref nkey, ref nval), ref right) => {
                if k < *nkey {
                    let newleft = Lmap::insert(left, k, v);
                    Some(Arc::new(
                        Lmap(newleft,
                            (nkey.clone(), nval.clone()), right.clone())
                    ))
                } else if k > *nkey {
                    let newright = Lmap::insert(right, k, v);
                    Some(Arc::new(
                        Lmap(left.clone(),
                            (nkey.clone(), nval.clone()), newright)
                    ))
                } else {
                    tree.clone()
                }
            }
        }
    }

    pub fn get<'a>(tree: &'a LmapNode, k: &Val) -> Option<&'a Val>
    {
        if tree.is_none() {
            return None
        }
        match **tree.as_ref().unwrap() {
            Lmap(ref left, (ref nkey, ref nval), ref right) => {
                if *k == *nkey {
                    Some(nval)
                } else if *k < *nkey {
                    Lmap::get(left, k)
                } else {
                    Lmap::get(right, k)
                }
            }
        }
    }
}

