use leema::val::{Val};

use std::cmp::{Eq};
use std::collections::{HashMap};
use std::fmt::{Debug};
use std::hash::{Hash};
use std::iter::{Iterator};
use std::rc::{Rc};


#[derive(Debug)]
pub struct ListIterator<'a>
{
    cursor: &'a Val,
}

impl<'a> Iterator for ListIterator<'a>
{
    type Item = &'a Val;

    fn next(&mut self) -> Option<&'a Val>
    {
        let c = self.cursor;
        match c {
            &Val::Cons(ref head, ref tail) => {
                *self = ListIterator{cursor: &**tail};
                Some(head)
            }
            &Val::Nil => None,
            _ => {
                panic!("cannot iterate on a not list");
            }
        }
    }
}

pub fn iter<'a>(head: &'a Val) -> ListIterator<'a>
{
    ListIterator{cursor: head}
}

pub fn from_vec(items: &Vec<Val>) -> Val
{
    let new_items = items.iter().fold(Val::Nil, |acc, i| {
        cons(i.clone(), acc)
    });
    reverse(&new_items)
}

pub fn cons(head: Val, tail: Val) -> Val
{
    match tail {
        Val::Cons(_, _) => {
            Val::Cons(Box::new(head), Rc::new(tail))
        }
        Val::Nil => {
            Val::Cons(Box::new(head), Rc::new(Val::Nil))
        }
        Val::Id(_) => {
            // this is used when parsing list patterns
            Val::Cons(Box::new(head), Rc::new(tail))
        }
        Val::Wildcard => {
            Val::Cons(Box::new(head), Rc::new(tail))
        }
        Val::PatternVar(_) => {
            Val::Cons(Box::new(head), Rc::new(tail))
        }
        _ => {
            panic!("Can't cons to a not list {:?}", tail);
        }
    }
}

pub fn concat(l1: &Val, l2: &Val) -> Val
{
    if l1 == &Val::Nil {
        return l2.clone();
    }
    if l2 == &Val::Nil {
        return l1.clone();
    }

    match l1 {
        &Val::Nil => {
            l2.clone()
        }
        &Val::Cons(ref head, ref tail) => {
            let new_tail = concat(tail, l2);
            cons((**head).clone(), new_tail)
        }
        _ => {
            panic!("cannot concat to a not list: concat({:?}, {:?})", l1, l2);
        }
    }
}

pub fn singleton(head: Val) -> Val
{
    cons(head, Val::Nil)
}

pub fn from2(a: Val, b: Val) -> Val
{
    cons(a, cons(b, Val::Nil))
}

pub fn from3(a: Val, b: Val, c: Val) -> Val
{
    cons(a, cons(b, cons(c, Val::Nil)))
}

pub fn empty() -> Val
{
    Val::Nil
}

pub fn from_tuple(t: &Val) -> Val
{
    let mut result = empty();
    match t {
        &Val::Tuple(ref items) => {
            for i in items.iter().rev() {
                result = cons(i.clone(), result);
            }
        }
        _ => {
            panic!("cannot create list from not tuple: {:?}", t);
        }
    }
    result
}

pub fn ref_to_vec(it: &Val) -> Vec<Val>
{
    map_ref_to_vec(it, |i| {
        i.clone()
    })
}

pub fn is_empty(l: &Val) -> bool
{
    match l {
        &Val::Nil => true,
        &Val::Cons(_, _) => false,
        _ => {
            panic!("is_empty parameter is not list");
        }
    }
}

pub fn is_singleton(l: &Val) -> bool
{
    if is_empty(l) {
        return false;
    }
    let (head, tail) = take_ref(l);
    is_empty(&*tail)
}

pub fn len(l: &Val) -> usize
{
    fold_ref(0, l, |res, _| { res + 1 })
}

pub fn last<'a>(l: &'a Val) -> Option<&'a Val>
{
    match l {
        &Val::Nil => None,
        &Val::Cons(ref head, ref tail) if **tail == Val::Nil => Some(head),
        &Val::Cons(ref head, ref tail) => last(tail),
        _ => {
            panic!("cannot get last of a not list: {:?}", l);
        }
    }
}

pub fn map_ref<F>(mut l: &Val, mut op: F) -> Val
    where F: FnMut(&Val) -> Val
{
    let mut result = Val::Nil;
    while *l != Val::Nil {
        let (head, tail) = take_ref(l);
        let single = op(head);
        result = cons(single, result);
        l = &*tail;
    }
    reverse(&result)
}

pub fn map_ref_to_vec<F, T>(l: &Val, mut op: F) -> Vec<T>
    where F: FnMut(&Val) -> T
{
    let mut it = l;
    let list_len = len(l);
    let mut acc = Vec::with_capacity(list_len);
    while *it != Val::Nil {
        let (head, tail) = take_ref(it);
        let single = op(head);
        acc.push(single);
        it = &*tail;
    }
    acc
}

pub fn fold_ref<R, F>(init: R, l: &Val, op: F) -> R
    where F: Fn(R, &Val) -> R
{
    let mut result = init;
    let mut curr = l;
    while *curr != Val::Nil {
        let (head, tail) = take_ref(curr);
        result = op(result, head);
        curr = &*tail;
    }
    result
}

pub fn fold_mut_ref<R, F>(init: &mut R, l: &Val, op: F)
    where F: Fn(&mut R, &Val)
{
    let mut it = l;
    while *it != Val::Nil {
        if !it.is_list() {
            panic!("Cannot fold on not-list: {:?}", it);
        }
        let (head, tail) = take_ref(it);
        op(init, head);
        it = &*tail;
    }
}

pub fn merge_adjacent<F>(l: &Val, op: F) -> Val
    where F: Fn(&Val, &Val) -> Option<Val>
{
    if *l == Val::Nil {
        return Val::Nil;
    }
    let mut acc = Val::Nil;
    let (premerge, mut it) = take_ref(l);
    let mut merger = premerge.clone();
    while **it != Val::Nil {
        if !it.is_list() {
            panic!("Cannot merge not-list: {:?}", it);
        }
        let (head, tail) = take_ref(&*it);
        let opt_merged = op(&merger, head);
        match opt_merged {
            Some(merged) => {
                merger = merged;
            }
            None => {
                acc = cons(merger, acc);
                merger = head.clone();
            }
        }
        it = tail;
    }
    reverse(&cons(merger, acc))
}

pub fn partition<F>(l: &Val, pred: F) -> (Val, Val)
    where F: Fn(&Val) -> bool
{
    let mut it = l;
    let mut truth = Val::Nil;
    let mut lies = Val::Nil;
    while *it != Val::Nil {
        let (head, tail) = take_ref(it);
        if pred(head) {
            truth = cons(head.clone(), truth);
        } else {
            lies = cons(head.clone(), lies);
        }
        it = &**tail;
    }
    let true_result = reverse(&truth);
    let false_result = reverse(&lies);
    (true_result, false_result)
}

pub fn keyed_by<K, F>(l: &Val, keyf: F) -> HashMap<K, Val>
    where K: Eq + Hash + Debug
        , F: Fn(&Val) -> K
{
    let mut it = l;
    let mut result: HashMap<K, Val> = HashMap::with_capacity(len(l));
    while *it != Val::Nil {
        let (head, tail) = take_ref(it);
        let key = keyf(head);
        if result.contains_key(&key) {
            panic!("cannot duplicate key: {:?}", key);
        }
        result.insert(key, head.clone());
        it = &**tail;
    }

    result
}

pub fn reverse(l: &Val) -> Val
{
    let mut result = Val::Nil;
    let mut next = l;
    while *next != Val::Nil {
        let (ref head, ref tail) = take_ref(next);
        result = cons((*head).clone(), result);
        next = tail;
    }
    result
}

pub fn take(l: Val) -> (Val, Rc<Val>)
{
    match l {
        Val::Cons(head, tail) => (*head, tail),
        Val::Nil => {
            panic!("Cannot take from empty list");
        }
        _ => {
            panic!("Cannot take from not a list: {:?}", l);
        }
    }
}

pub fn take_ref(l: &Val) -> (&Val, &Rc<Val>)
{
    match l {
        &Val::Cons(ref head, ref tail) => (head, tail),
        &Val::Nil => {
            panic!("Cannot take from empty list");
        }
        _ => {
            panic!("Cannot take from not a list: {:?}", l);
        }
    }
}

pub fn to_ref_tuple2(l1: &Val) -> (&Val, &Val)
{
    let (i1, l2) = take_ref(l1);
    let (i2, _) = take_ref(&*l2);
    (i1, i2)
}

pub fn to_ref_tuple3(l1: &Val) -> (&Val, &Val, &Val)
{
    let (i1, l2) = take_ref(l1);
    let (i2, l3) = take_ref(&*l2);
    let (i3, _) = take_ref(&*l3);
    (i1, i2, i3)
}

pub fn to_ref_tuple4(l1: &Val) -> (&Val, &Val, &Val, &Val)
{
    let (i1, l2) = take_ref(l1);
    let (i2, l3) = take_ref(&*l2);
    let (i3, l4) = take_ref(&*l3);
    let (i4, _) = take_ref(&*l4);
    (i1, i2, i3, i4)
}

pub fn head(l: Val) -> Val
{
    match l {
        Val::Cons(head, tail) => *head,
        Val::Nil => {
            panic!("Cannot take_head from empty list");
        }
        _ => {
            panic!("Cannot take_head from not a list: {:?}", l);
        }
    }
}

pub fn head_or(l: Val, orval: Val) -> Val
{
    match l {
        Val::Cons(head, tail) => *head,
        Val::Nil => {
            orval
        }
        _ => {
            panic!("Cannot take head from not a list: {:?}", l);
        }
    }
}

pub fn head_ref(l: &Val) -> &Val
{
    match l {
        &Val::Cons(ref head, _) => head,
        &Val::Nil => {
            panic!("Cannot take_head from empty list");
        }
        _ => {
            panic!("Cannot take_head from not a list: {:?}", l);
        }
    }
}

pub fn set_head(l: &mut Val, v: Val)
{
    match l {
        &mut Val::Nil => {
            panic!("cannot set head for empty list");
        }
        &mut Val::Cons(ref mut head, _) => {
            *head = Box::new(v);
        }
        _ => {
            panic!("Cannot set head on a not list: {:?}", l);
        }
    }
}


#[cfg(test)]
mod tests {
    use leema::list;
    use leema::val::{Val};

#[test]
fn test_concat()
{
    let l1 = list::from2(Val::Int(1), Val::Int(2));
    let l2 = list::from2(Val::Int(3), Val::Int(4));

    let lc = list::concat(&l1, &l2);

    let exp =
        list::cons(Val::Int(1),
        list::cons(Val::Int(2),
        list::cons(Val::Int(3),
        list::cons(Val::Int(4),
        Val::Nil
        ))));
    assert_eq!(exp, lc);
}

#[test]
fn test_map_ref()
{
    let l =
        list::cons(Val::Bool(true),
        list::cons(Val::Bool(false),
        list::cons(Val::Bool(true),
        Val::Nil,
    )));

    let not_l = list::map_ref(&l, |v| {
        if let &Val::Bool(b) = v {
            Val::Bool(!b)
        } else {
            Val::Void
        }
    });

    let expected =
        list::cons(Val::Bool(false),
        list::cons(Val::Bool(true),
        list::cons(Val::Bool(false),
        Val::Nil,
    )));

    assert_eq!(expected, not_l);
}

#[test]
fn test_len()
{
    let l =
        list::cons(Val::Int(2),
        list::cons(Val::Int(3),
        list::cons(Val::Int(5),
        Val::Nil,
        )));
    assert_eq!(3, list::len(&l));
}

#[test]
fn test_merge_adjacent()
{
    let l =
        list::cons(Val::Int(2),
        list::cons(Val::Int(3),
        list::cons(Val::Int(2),
        list::cons(Val::Int(4),
        list::cons(Val::Int(4),
        Val::Nil,
        )))));

    let m = list::merge_adjacent(&l, |a, b| {
        if a == b {
            Some(a.clone())
        } else {
            None
        }
    });

    let expected =
        list::cons(Val::Int(2),
        list::cons(Val::Int(3),
        list::cons(Val::Int(2),
        list::cons(Val::Int(4),
        Val::Nil
        ))));

    assert_eq!(expected, m);
}

#[test]
fn test_iterator_sum()
{
    let l = list::from3(Val::Int(2), Val::Int(3), Val::Int(4));
    let actual = list::iter(&l).fold(Val::Int(0), |sumval, ival| {
        match (sumval, ival) {
            (Val::Int(sum), &Val::Int(i)) => {
                Val::Int(sum + i)
            }
            _ => {
                panic!("cannot add not-integers");
            }
        }
    });
    assert_eq!(Val::Int(9), actual);
}

}
