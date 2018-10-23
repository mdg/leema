use leema::failure::Lresult;
use leema::lstr::Lstr;
use leema::reg::{self, Ireg};
use leema::sendclone;
use leema::val::Val;

use std::clone::Clone;
use std::fmt;
use std::iter::{FromIterator, Iterator};
use std::slice::Iter;


#[derive(Clone)]
#[derive(Debug)]
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
        StrupleItem{ k, v }
    }
}

#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
#[derive(Eq)]
#[derive(Ord)]
#[derive(Hash)]
pub struct StrupleKV<K, V>
{
    pub items: Option<Vec<StrupleItem<K, V>>>,
}

pub type Struple2<T> = StrupleKV<Option<Lstr>, T>;

impl<K, V> StrupleKV<K, V>
{
    pub fn new(items: Option<Vec<StrupleItem<K, V>>>) -> StrupleKV<K, V>
    {
        StrupleKV{ items }
    }

    pub fn none() -> StrupleKV<K, V>
    {
        StrupleKV{ items: None }
    }

    pub fn from_vec(items: Vec<StrupleItem<K, V>>) -> StrupleKV<K, V>
    {
        if items.is_empty() {
            return StrupleKV::none();
        }
        StrupleKV { items: Some(items) }
    }

    pub fn into_iter(&self) -> Option<impl IntoIterator<Item = StrupleItem<K, V>>>
    {
        self.items.map(|inner| inner.into_iter())
    }

    pub fn is_empty(&self) -> bool
    {
        match self.items {
            None => true,
            Some(ref items) => items.is_empty(),
        }
    }

    pub fn len(&self) -> usize
    {
        match self.items {
            None => 0,
            Some(ref items) => items.len(),
        }
    }

    pub fn iter<'a>(&'a self) -> StrupleIter<'a, K, V>
    {
        let it = self.items.map(|inner| inner.iter());
        StrupleIter{ it }
    }

    pub fn iter_k(&self) -> impl Iterator<Item = &K>
    {
        self.iter().map(|kv| &kv.k)
    }

    pub fn iter_v(&self) -> impl Iterator<Item = &V>
    {
        self.iter().map(|kv| &kv.v)
    }

    pub fn map_v<F, U>(&self, f: F) -> Lresult<StrupleKV<K, U>>
    where
        F: Fn(&V) -> Lresult<U>,
        K: Clone,
    {
        if self.items.is_none() {
            return Ok(StrupleKV::none());
        }
        let m_result_items: Vec<Lresult<StrupleItem<K, U>>> =
            self.items.as_ref().unwrap().iter().map(|kv| {
                let u = f(&kv.v)?;
                Ok(StrupleItem::new(kv.k.clone(), u))
            }).collect();
        let m_items = Lresult::from_iter(m_result_items)?;
        Ok(StrupleKV::from_vec(m_items))
    }

    pub fn map_v_into<F, U>(self, f: F) -> Lresult<StrupleKV<K, U>>
    where
        F: Fn(V) -> Lresult<U>,
    {
        if self.items.is_none() {
            return Ok(StrupleKV::none());
        }
        let m_result_items: Vec<Lresult<StrupleItem<K, U>>> = self.items
            .unwrap()
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

impl<K, V> FromIterator<StrupleItem<K, V>> for StrupleKV<K, V>
{
    fn from_iter<I: IntoIterator<Item = StrupleItem<K, V>>>(
        iter: I,
    ) -> StrupleKV<K, V>
    {
        let items = Vec::from_iter(iter);
        /*
        let mut items = Vec::new();
        for item in iter {
            items.push(item);
        }
        */
        StrupleKV::from(items)
    }
}

impl<K, V> From<Vec<StrupleItem<K, V>>> for StrupleKV<K, V>
{
    fn from(items: Vec<StrupleItem<K, V>>) -> StrupleKV<K, V>
    {
        StrupleKV::from_vec(items)
    }
}

pub struct StrupleIter<'a, K: 'a, V: 'a>
{
    it: Option<Iter<'a, StrupleItem<K, V>>>,
}

impl<'a, K, V> Iterator for StrupleIter<'a, K, V>
{
    type Item = &'a StrupleItem<K, V>;

    fn next(&mut self) -> Option<Self::Item>
    {
        self.it.and_then(|inner| {
            inner.next()
        })
    }
}



#[derive(Clone)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
#[derive(Eq)]
#[derive(Ord)]
#[derive(Hash)]
pub struct Struple<T>(pub Vec<(Option<Lstr>, T)>);

impl<T> Struple<T>
{
    pub fn new_indexed(items: Vec<T>) -> Struple<T>
    {
        let new_items: Vec<(Option<Lstr>, T)> =
            items.into_iter().map(|i| (None, i)).collect();
        Struple(new_items)
    }

    pub fn new_tuple2(a: T, b: T) -> Struple<T>
    {
        Struple(vec![(None, a), (None, b)])
    }

    pub fn map<C>(&self, f: C) -> Lresult<Struple<T>>
    where
        C: Fn(&T) -> Lresult<T>,
    {
        let inner_f = |i: &(Option<Lstr>, T)| {
            let i_result = f(&i.1)?;
            Ok((i.0.clone(), i_result))
        };
        let m_items = self.0.iter().map(inner_f);
        Ok(Struple(Lresult::from_iter(m_items)?))
    }

    /**
     * Find a value with the given key
     */
    pub fn find(&self, key: &str) -> Option<(usize, &T)>
    {
        self.0
            .iter()
            .enumerate()
            .find(|(_, i)| i.0.as_ref().map_or(false, |ik| ik.str() == key))
            .map(|(idx, item)| (idx, &item.1))
    }
}

impl<T> FromIterator<(Option<Lstr>, T)> for Struple<T>
{
    fn from_iter<I: IntoIterator<Item = (Option<Lstr>, T)>>(
        iter: I,
    ) -> Struple<T>
    {
        let mut items = Vec::new();
        for item in iter {
            items.push(item);
        }
        Struple(items)
    }
}

impl<T> sendclone::SendClone for Struple<T>
where
    T: sendclone::SendClone<Item = T>,
{
    type Item = Struple<T>;

    fn clone_for_send(&self) -> Struple<T>
    {
        let safe_items = self
            .0
            .iter()
            .map(|i| {
                let new_key = i.0.as_ref().map(|ik| ik.clone_for_send());
                (new_key, i.1.clone_for_send())
            })
            .collect();
        Struple(safe_items)
    }
}

impl<T> fmt::Display for Struple<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        f.write_str("(")?;
        for &(ref opt_name, ref x) in self.0.iter() {
            if let &Some(ref name) = opt_name {
                write!(f, "{}:", name)?;
            }
            write!(f, "{},", x)?;
        }
        f.write_str(")")
    }
}

impl<T> fmt::Debug for Struple<T>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        f.write_str("(")?;
        for &(ref opt_name, ref x) in self.0.iter() {
            if let &Some(ref name) = opt_name {
                write!(f, "{}:", name)?;
            }
            write!(f, "{:?},", x)?;
        }
        f.write_str(")")
    }
}

impl reg::Iregistry for Struple<Val>
{
    fn ireg_get(&self, i: &Ireg) -> &Val
    {
        match i {
            // get reg on struple
            &Ireg::Reg(p) => {
                if p as usize >= self.0.len() {
                    panic!("{:?} too big for {:?}", i, self.0);
                }
                &self.0[p as usize].1
            }
            &Ireg::Sub(p, ref s) => {
                if p as usize >= self.0.len() {
                    panic!("{:?} too big for {:?}", i, self.0);
                }
                self.0[p as usize].1.ireg_get(&*s)
            }
        }
    }

    fn ireg_get_mut(&mut self, i: &Ireg) -> &mut Val
    {
        match i {
            // set reg on struple
            &Ireg::Reg(p) => {
                if p as usize >= self.0.len() {
                    panic!("{:?} too big for {:?}", i, self.0);
                }
                &mut self.0[p as usize].1
            }
            &Ireg::Sub(p, ref s) => {
                if p as usize >= self.0.len() {
                    panic!("{:?} too big for {:?}", i, self.0);
                }
                self.0[p as usize].1.ireg_get_mut(&*s)
            }
        }
    }

    fn ireg_set(&mut self, i: &Ireg, v: Val)
    {
        match i {
            // get reg on struple
            &Ireg::Reg(p) => {
                if p as usize >= self.0.len() {
                    panic!("{:?} too big for struple {:?}", i, self);
                }
                self.0[p as usize].1 = v;
            }
            &Ireg::Sub(p, ref s) => {
                if p as usize >= self.0.len() {
                    panic!("{:?} too big for strtuple {:?}", i, self);
                }
                self.0[p as usize].1.ireg_set(&*s, v);
            }
        }
    }
}


#[cfg(test)]
mod tests
{
    use leema::lstr::Lstr;
    use leema::struple::Struple;
    use leema::val::Val;


    #[test]
    fn test_struple_find()
    {
        let s = Struple(vec![
            (Some(Lstr::from("taco")), Val::Int(2)),
            (None, Val::Int(3)),
            (Some(Lstr::from("burrito")), Val::Int(4)),
        ]);

        let actual = s.find("burrito").expect("burrito value");
        assert_eq!(2, actual.0);
        assert_eq!(Val::Int(4), *actual.1);
    }
}
