/// General structure / tuple storage mechanism
///
/// Eventually look at an optimization like:
/// enum Struple<K, V>
/// {
///     Two([StrupleItem<K, V>; 2]),
///     Three([StrupleItem<K, V>; 3]),
///     Four([StrupleItem<K, V>; 4]),
///     Some(Rc<Vec<StrupleItem<K, V>>>),
///     Many(TreeMap<K, V>),
///     Cow(TreeMap<K, V>, Rc<Vec<StrupleItem<K, V>>>),
/// }
use crate::leema::failure::Lresult;
use crate::leema::lstr::Lstr;
use crate::leema::reg::{self, Ireg};
use crate::leema::sendclone;
use crate::leema::val::Val;

use std::clone::Clone;
use std::fmt;
use std::iter::{FromIterator, Iterator};


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
            Some(ref k) => write!(f, "{}:{},", k, self.v),
            None => write!(f, "{},", self.v),
        }
    }
}

impl<V> fmt::Display for StrupleItem<Option<&'static str>, V>
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
        if f.alternate() {
            write!(f, "({:#?}:{:#?})", self.k, self.v)
        } else {
            write!(f, "({:?}:{:?})", self.k, self.v)
        }
    }
}

impl<K, V> From<(K, V)> for StrupleItem<K, V>
{
    fn from(item: (K, V)) -> StrupleItem<K, V>
    {
        StrupleItem {
            k: item.0,
            v: item.1,
        }
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


pub type StrupleKV<K, V> = Vec<StrupleItem<K, V>>;
pub type Struple2<T> = Vec<StrupleItem<Option<Lstr>, T>>;
pub type StrupleSlice<K, V> = [StrupleItem<K, V>];
pub type Struple2Slice<T> = [StrupleItem<Option<Lstr>, T>];

pub fn iter_k<K, V>(s: &StrupleKV<K, V>) -> impl Iterator<Item = &K>
{
    s.iter().map(|kv| &kv.k)
}

pub fn iter_v<K, V>(s: &StrupleKV<K, V>) -> impl Iterator<Item = &V>
{
    s.iter().map(|kv| &kv.v)
}

pub fn map_v<K, V, F, U>(
    s: &StrupleKV<K, V>,
    mut f: F,
) -> Lresult<StrupleKV<K, U>>
where
    F: FnMut(&V) -> Lresult<U>,
    K: Clone,
{
    // let m_result_items: Vec<Lresult<StrupleItem<K, U>>> = s
    let m_result_items = s.iter().map(|kv| {
        let u = f(&kv.v)?;
        Ok(StrupleItem::new(kv.k.clone(), u))
    });
    Lresult::from_iter(m_result_items)
}

pub fn map_v_into<K, V, F, U>(
    s: StrupleKV<K, V>,
    mut f: F,
) -> Lresult<StrupleKV<K, U>>
where
    F: FnMut(V) -> Lresult<U>,
{
    // let m_result_items: Vec<Lresult<StrupleItem<K, U>>> = s
    let m_result_items = s.into_iter().map(|kv| {
        let u = f(kv.v)?;
        Ok(StrupleItem::new(kv.k, u))
    });
    Lresult::from_iter(m_result_items)
}

pub fn find<'s, 'k, K, V>(
    s: &'s [StrupleItem<K, V>],
    key: &'k K,
) -> Option<(usize, &'s V)>
where
    K: PartialEq,
{
    s.iter()
        .enumerate()
        .find(|(_, i)| i.k == *key)
        .map(|(idx, item)| (idx, &item.v))
}

pub fn find_str<'s, K, S, V>(
    s: &'s [StrupleItem<Option<S>, V>],
    key: K,
) -> Option<(usize, &'s V)>
    where S: AsRef<str>
        , K: AsRef<str>
{
    s.iter()
        .enumerate()
        .find(|(_, i)| {
            match &i.k {
                Some(ref item_key) => *item_key.as_ref() == *key.as_ref(),
                None => false,
            }
        })
        .map(|(idx, item)| (idx, &item.v))
}

pub fn find_str_mut<'s, 'k, K, S, V>(
    s: &'s mut [StrupleItem<Option<S>, V>],
    key: K,
) -> Option<(usize, &'s mut V)>
    where S: AsRef<str>, K: AsRef<str>
{
    s.iter_mut()
        .enumerate()
        .find(|(_, i)| {
            match &i.k {
                Some(ref item_key) => *item_key.as_ref() == *key.as_ref(),
                None => false,
            }
        })
        .map(|(idx, item)| (idx, &mut item.v))
}

pub fn contains_key<K, V>(s: &[StrupleItem<K, V>], k: &K) -> bool
where
    K: PartialEq,
{
    find(s, k).is_some()
}

pub fn new_tuple2<K, V>(a: V, b: V) -> StrupleKV<Option<K>, V>
{
    vec![StrupleItem::new(None, a), StrupleItem::new(None, b)]
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
            .iter()
            .map(|i| {
                let new_key = i.k.clone_for_send();
                let new_val = i.v.clone_for_send();
                StrupleItem::new(new_key, new_val)
            })
            .collect();
        safe_items
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
                if p as usize >= self.len() {
                    Err(rustfail!(
                        "leema_failure",
                        "{:?} too big for {:?}",
                        i,
                        self,
                    ))
                } else {
                    Ok(&self[p as usize].v)
                }
            }
            Ireg::Sub(p, s) => {
                if p as usize >= self.len() {
                    Err(rustfail!(
                        "leema_failure",
                        "{:?} too big for {:?}",
                        i,
                        self,
                    ))
                } else {
                    self[p as usize].v.ireg_get(Ireg::Reg(s))
                }
            }
        }
    }

    fn ireg_set(&mut self, i: Ireg, v: Val)
    {
        match i {
            // set reg on struple
            Ireg::Reg(p) => {
                if p as usize >= self.len() {
                    panic!("{:?} too big for struple {:?}", i, self);
                }
                self[p as usize].v = v;
            }
            Ireg::Sub(p, s) => {
                if p as usize >= self.len() {
                    panic!("{:?} too big for strtuple {:?}", i, self);
                }
                self[p as usize].v.ireg_set(Ireg::Reg(s), v);
            }
        }
    }
}


#[cfg(test)]
mod tests
{
    use crate::leema::lstr::Lstr;
    use crate::leema::struple::{self, Struple2, StrupleItem};
    use crate::leema::val::Val;


    #[test]
    fn test_struple_find()
    {
        let s: Struple2<Val> = vec![
            StrupleItem::new(Some(Lstr::Sref("taco")), Val::Int(2)),
            StrupleItem::new(None, Val::Int(3)),
            StrupleItem::new(Some(Lstr::Sref("burrito")), Val::Int(4)),
        ];

        let actual = struple::find(&s, &Some(Lstr::Sref("burrito")))
            .expect("burrito value");
        assert_eq!(2, actual.0);
        assert_eq!(Val::Int(4), *actual.1);
    }
}
