use crate::leema::lstr::Lstr;
use crate::leema::struple::StrupleItem;
use crate::leema::val::{Type, Val};

use std::cmp::Ordering;
use std::sync::Arc;

const TYPEPATH_MAP: &str = "/map/T";

pub fn map_type() -> Type
{
    Type::t(
        TYPEPATH_MAP,
        vec![
            StrupleItem {
                k: Lstr::Sref("K"),
                v: Type::UNKNOWN,
            },
            StrupleItem {
                k: Lstr::Sref("V"),
                v: Type::UNKNOWN,
            },
        ],
    )
}

pub type LmapNode = Option<Arc<Lmap>>;

#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
#[derive(Eq)]
#[derive(Ord)]
pub struct Lmap(pub Option<Arc<Lmap>>, pub (Val, Val), pub Option<Arc<Lmap>>);

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
                match k.cmp(nkey) {
                    Ordering::Less => {
                        let newleft = Lmap::insert(left, k, v);
                        Some(Arc::new(Lmap(
                            newleft,
                            (nkey.clone(), nval.clone()),
                            right.clone(),
                        )))
                    }
                    Ordering::Greater => {
                        let newright = Lmap::insert(right, k, v);
                        Some(Arc::new(Lmap(
                            left.clone(),
                            (nkey.clone(), nval.clone()),
                            newright,
                        )))
                    }
                    Ordering::Equal => {
                        Some(Arc::new(Lmap(
                            left.clone(),
                            (k, v),
                            right.clone(),
                        )))
                    }
                }
            }
        }
    }

    pub fn get<'a>(tree: &'a LmapNode, k: &Val) -> Option<&'a Val>
    {
        if tree.is_none() {
            return None;
        }
        match **tree.as_ref().unwrap() {
            Lmap(ref left, (ref nkey, ref nval), ref right) => {
                match k.cmp(nkey) {
                    Ordering::Equal => {
                        Some(nval)
                    }
                    Ordering::Less => {
                        Lmap::get(left, k)
                    }
                    Ordering::Greater => {
                        Lmap::get(right, k)
                    }
                }
            }
        }
    }

    pub fn len(tree: &LmapNode) -> usize
    {
        if tree.is_none() {
            return 0;
        }
        let itree = tree.as_ref().unwrap();
        Lmap::len(&itree.0) + Lmap::len(&itree.2) + 1
    }
}

#[cfg(test)]
mod tests
{
    use crate::leema::lmap::Lmap;
    use crate::leema::lstr::Lstr;
    use crate::leema::val::Val;

    #[test]
    pub fn test_lmap_constructor()
    {
        assert_eq!(None, Lmap::new());
    }

    #[test]
    pub fn test_lmap_insert()
    {
        let k = Val::Str(Lstr::Sref("tacos"));
        let m1 = Lmap::new();
        assert!(Lmap::get(&m1, &k).is_none());
        let m2 = Lmap::insert(&m1, k.clone(), Val::Int(4));
        assert_eq!(Val::Int(4), *Lmap::get(&m2, &k).unwrap());
    }
}
