use crate::leema::failure::Lresult;
use crate::leema::lstr::Lstr;
use crate::leema::reg::{self, Ireg};
use crate::leema::sendclone;
use crate::leema::val::Val;

use std::clone::Clone;
use std::fmt;
use std::iter::{FromIterator, Iterator};
use std::ops::Index;
use std::slice::SliceIndex;


#[derive(Clone)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
#[derive(Eq)]
#[derive(Ord)]
#[derive(Hash)]
pub struct StrupleItem<K, V>
{
    pub k: K,
    pub v: V,
}

impl<K, V> StrupleItem<K, V>
{
    pub fn new(k: K, v: V) -> StrupleItem<K, V>
    {
        StrupleItem { k, v }
    }

    pub fn new_v(v: V) -> StrupleItem<Option<K>, V>
    {
        StrupleItem { k: None, v }
    }
}

impl<V> fmt::Display for StrupleItem<Option<Lstr>, V>
where
    V: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self.k {
            Some(ref k) => write!(f, "{}:{}", k, self.v),
            None => write!(f, ":{}", self.v),
        }
    }
}

impl<V> fmt::Display for StrupleItem<Lstr, V>
where
    V: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "{}:{}", self.k, self.v)
    }
}

impl<K, V> fmt::Debug for StrupleItem<K, V>
where
    K: fmt::Debug,
    V: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "({:?}:{:?})", self.k, self.v)
    }
}

impl<K, V> Default for StrupleItem<K, V>
where
    K: Default,
    V: Default,
{
    fn default() -> StrupleItem<K, V>
    {
        StrupleItem {
            k: Default::default(),
            v: Default::default(),
        }
    }
}

#[derive(Clone)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
#[derive(Eq)]
#[derive(Ord)]
#[derive(Hash)]
pub struct StrupleKV<K, V>(pub Vec<StrupleItem<K, V>>);


pub type Struple2<T> = StrupleKV<Option<Lstr>, T>;

impl<K, V> StrupleKV<K, V>
{
    pub fn new() -> StrupleKV<K, V>
    {
        StrupleKV(vec![])
    }

    pub fn none() -> StrupleKV<K, V>
    {
        StrupleKV(vec![])
    }

    pub fn from_vec(items: Vec<StrupleItem<K, V>>) -> StrupleKV<K, V>
    {
        StrupleKV(items)
    }

    pub fn into_iter(self) -> impl IntoIterator<Item = StrupleItem<K, V>>
    {
        self.0.into_iter()
    }

    pub fn is_empty(&self) -> bool
    {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize
    {
        self.0.len()
    }

    pub fn get(&self, idx: usize) -> Option<&StrupleItem<K, V>>
    {
        self.0.get(idx)
    }

    pub fn iter<'a>(&'a self) -> impl Iterator<Item = &StrupleItem<K, V>>
    {
        self.0.iter()
    }

    pub fn iter_k(&self) -> impl Iterator<Item = &K>
    {
        self.iter().map(|kv| &kv.k)
    }

    pub fn iter_v(&self) -> impl Iterator<Item = &V>
    {
        self.iter().map(|kv| &kv.v)
    }

    pub fn map_v<F, U>(&self, mut f: F) -> Lresult<StrupleKV<K, U>>
    where
        F: FnMut(&V) -> Lresult<U>,
        K: Clone,
    {
        let m_result_items: Vec<Lresult<StrupleItem<K, U>>> = self
            .0
            .iter()
            .map(|kv| {
                let u = f(&kv.v)?;
                Ok(StrupleItem::new(kv.k.clone(), u))
            })
            .collect();
        let m_items = Lresult::from_iter(m_result_items)?;
        Ok(StrupleKV::from_vec(m_items))
    }

    pub fn map_v_into<F, U>(self, mut f: F) -> Lresult<StrupleKV<K, U>>
    where
        F: FnMut(V) -> Lresult<U>,
    {
        let m_result_items: Vec<Lresult<StrupleItem<K, U>>> = self
            .0
            .into_iter()
            .map(|kv| {
                let u = f(kv.v)?;
                Ok(StrupleItem::new(kv.k, u))
            })
            .collect();
        let m_items = Lresult::from_iter(m_result_items)?;
        Ok(StrupleKV::from_vec(m_items))
    }
}

impl<K, V> StrupleKV<K, V>
where
    K: PartialEq,
{
    pub fn find(&self, key: &K) -> Option<(usize, &V)>
    {
        self.0
            .iter()
            .enumerate()
            .find(|(_, i)| i.k == *key)
            .map(|(idx, item)| (idx, &item.v))
    }
}

impl<V> Struple2<V>
{
    pub fn new_tuple2(a: V, b: V) -> Struple2<V>
    {
        StrupleKV(vec![
            StrupleItem::new(None, a),
            StrupleItem::new(None, b),
        ])
    }
}

impl<K, V> FromIterator<StrupleItem<K, V>> for StrupleKV<K, V>
{
    fn from_iter<I: IntoIterator<Item = StrupleItem<K, V>>>(
        iter: I,
    ) -> StrupleKV<K, V>
    {
        let items = Vec::from_iter(iter);
        StrupleKV::from_vec(items)
    }
}

impl<K, V> From<Vec<StrupleItem<K, V>>> for StrupleKV<K, V>
{
    fn from(items: Vec<StrupleItem<K, V>>) -> StrupleKV<K, V>
    {
        StrupleKV::from_vec(items)
    }
}

impl<K, V> From<Vec<(K, V)>> for StrupleKV<K, V>
{
    fn from(items: Vec<(K, V)>) -> StrupleKV<K, V>
    {
        let new_items = items
            .into_iter()
            .map(|(k, v)| StrupleItem::new(k, v))
            .collect();
        StrupleKV::from_vec(new_items)
    }
}

impl<K, V> From<Vec<V>> for StrupleKV<Option<K>, V>
{
    fn from(items: Vec<V>) -> StrupleKV<Option<K>, V>
    {
        let full_items = items
            .into_iter()
            .map(|i| StrupleItem::new(None, i))
            .collect();
        StrupleKV::from_vec(full_items)
    }
}

impl<K, V, I> Index<I> for StrupleKV<K, V>
where
    I: SliceIndex<[StrupleItem<K, V>]>,
{
    type Output = I::Output;

    fn index(&self, idx: I) -> &Self::Output
    {
        &self.0[idx]
    }
}

impl<K, V> fmt::Display for StrupleKV<K, V>
where
    StrupleItem<K, V>: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "(")?;
        for i in &self.0 {
            write!(f, "{},", i)?;
        }
        write!(f, ")")
    }
}

impl<K, V> fmt::Debug for StrupleKV<K, V>
where
    K: fmt::Debug,
    V: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "{:?}", self.0)
    }
}

impl<K, V> sendclone::SendClone for StrupleKV<K, V>
where
    K: sendclone::SendClone<Item = K>,
    V: sendclone::SendClone<Item = V>,
{
    type Item = StrupleKV<K, V>;

    fn clone_for_send(&self) -> StrupleKV<K, V>
    {
        let safe_items = self
            .0
            .iter()
            .map(|i| {
                let new_key = i.k.clone_for_send();
                let new_val = i.v.clone_for_send();
                StrupleItem::new(new_key, new_val)
            })
            .collect();
        StrupleKV::from_vec(safe_items)
    }
}

impl<K> reg::Iregistry for StrupleKV<K, Val>
where
    K: fmt::Debug,
{
    fn ireg_get(&self, i: Ireg) -> Lresult<&Val>
    {
        match i {
            // get reg on struple
            Ireg::Reg(p) => {
                if p as usize >= self.0.len() {
                    Err(rustfail!(
                        "leema_failure",
                        "{:?} too big for {:?}",
                        i,
                        self.0,
                    ))
                } else {
                    Ok(&self.0[p as usize].v)
                }
            }
            Ireg::Sub(p, s) => {
                if p as usize >= self.0.len() {
                    Err(rustfail!(
                        "leema_failure",
                        "{:?} too big for {:?}",
                        i,
                        self.0,
                    ))
                } else {
                    self.0[p as usize].v.ireg_get(Ireg::Reg(s))
                }
            }
        }
    }

    fn ireg_set(&mut self, i: Ireg, v: Val)
    {
        match i {
            // set reg on struple
            Ireg::Reg(p) => {
                if p as usize >= self.0.len() {
                    panic!("{:?} too big for struple {:?}", i, self);
                }
                self.0[p as usize].v = v;
            }
            Ireg::Sub(p, s) => {
                if p as usize >= self.0.len() {
                    panic!("{:?} too big for strtuple {:?}", i, self);
                }
                self.0[p as usize].v.ireg_set(Ireg::Reg(s), v);
            }
        }
    }
}


#[cfg(test)]
mod tests
{
    use crate::leema::lstr::Lstr;
    use crate::leema::struple::{Struple2, StrupleItem, StrupleKV};
    use crate::leema::val::Val;


    #[test]
    fn test_struple_find()
    {
        let s: Struple2<Val> = StrupleKV(vec![
            StrupleItem::new(Some(Lstr::Sref("taco")), Val::Int(2)),
            StrupleItem::new(None, Val::Int(3)),
            StrupleItem::new(Some(Lstr::Sref("burrito")), Val::Int(4)),
        ]);

        let actual = s.find(&Some(Lstr::Sref("burrito"))).expect("burrito value");
        assert_eq!(2, actual.0);
        assert_eq!(Val::Int(4), *actual.1);
    }
}
