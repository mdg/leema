use leema::val::{Val};
use std::collections::{LinkedList};


pub fn cons(head: Val, tail: Val) -> Val
{
    match tail {
        Val::Sxpr(t, oldt) => {
            Val::Sxpr(t, Box::new(cons(head, *oldt)))
        }
        Val::Cons(_, _) => {
            Val::Cons(Box::new(head), Box::new(tail))
        }
        Val::Nil => {
            Val::Cons(Box::new(head), Box::new(Val::Nil))
        }
        Val::Id(_) => {
            // this is used when parsing list patterns
            Val::Cons(Box::new(head), Box::new(tail))
        }
        Val::Wildcard => {
            Val::Cons(Box::new(head), Box::new(tail))
        }
        Val::PatternVar(_) => {
            Val::Cons(Box::new(head), Box::new(tail))
        }
        _ => {
            panic!("Can't cons to a not list {:?}", tail);
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

pub fn to_vec(mut it: Val) -> Vec<Val>
{
    let mut acc = Vec::new();
    loop {
        match it {
            Val::Cons(h, mut t) => {
                acc.push(*h);
                it = *t;
            }
            Val::Nil => break,
            _ => {
                panic!("cannot convert not-List to vector: {:?}", it);
            }
        }
    }
    acc
}

pub fn is_empty(l: &Val) -> bool
{
    match l {
        &Val::Nil => true,
        &Val::Cons(_, _) => false,
        &Val::Sxpr(_, ref head) => {
            match **head {
                Val::Nil => true,
                _ => false,
            }
        }
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
    is_empty(tail)
}

pub fn len(l: &Val) -> usize
{
    fold_ref(0, l, |res, _| { res + 1 })
}

pub fn map<F>(mut l: Val, op: F) -> Val
    where F: Fn(Val) -> Val
{
    let mut result = Val::Nil;
    while l != Val::Nil {
        let (head, tail) = take(l);
        let single = op(head);
        result = cons(single, result);
        l = tail;
    }
    reverse(&result)
}

pub fn map_ref<F>(mut l: &Val, mut op: F) -> Val
    where F: FnMut(&Val) -> Val
{
    let mut result = Val::Nil;
    while *l != Val::Nil {
        let (head, tail) = take_ref(l);
        let single = op(head);
        result = cons(single, result);
        l = tail;
    }
    reverse(&result)
}

pub fn map_to_ll<F>(l: Val, op: F) -> LinkedList<Val>
    where F: Fn(Val) -> Val
{
    let mut it = l;
    let acc = LinkedList::new();
    while it != Val::Nil {
    }
    acc
}

pub fn map_to_vec<F, T>(l: Val, op: F) -> Vec<T>
    where F: Fn(Val) -> T
{
    let mut it = l;
    let mut acc = Vec::new();
    while it != Val::Nil {
        let (head, tail) = take(it);
        let single = op(head);
        acc.push(single);
        it = tail;
    }
    acc
}

pub fn map_ref_to_vec<F, T>(l: &Val, mut op: F) -> Vec<T>
    where F: FnMut(&Val) -> T
{
    let mut it = l;
    let mut acc = Vec::new();
    while *it != Val::Nil {
        let (head, tail) = take_ref(it);
        let single = op(head);
        acc.push(single);
        it = tail;
    }
    acc
}

pub fn fold<R, F>(init: R, l: Val, op: F) -> R
    where F: Fn(R, Val) -> R
{
    let mut acc = init;
    let mut it = l;
    while it != Val::Nil {
        if !it.is_list() {
            panic!("Cannot fold on not-list: {:?}", it);
        }
        let (head, tail) = take(it);
        acc = op(acc, head);
        it = tail;
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
        curr = tail;
    }
    result
}

pub fn fold_mut<R, F>(init: &mut R, l: Val, op: F)
    where F: Fn(&mut R, Val)
{
    let mut it = l;
    while it != Val::Nil {
        if !it.is_list() {
            panic!("Cannot fold on not-list: {:?}", it);
        }
        let (head, tail) = take(it);
        op(init, head);
        it = tail;
    }
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
        it = tail;
    }
}

pub fn merge_adjacent<F>(l: Val, op: F) -> Val
    where F: Fn(Val, Val) -> (Option<Val>, Val)
{
    if l == Val::Nil {
        return Val::Nil;
    }
    let mut acc = Val::Nil;
    let (mut merge, mut it) = take(l);
    while it != Val::Nil {
        if !it.is_list() {
            panic!("Cannot fold on not-list: {:?}", it);
        }
        let (head, tail) = take(it);
        let (appender, merger) = op(merge, head);
        merge = merger;
        if appender.is_some() {
            acc = cons(appender.unwrap(), acc);
        }
        it = tail;
    }
    reverse(&cons(merge, acc))
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

pub fn take(l: Val) -> (Val, Val)
{
    match l {
        Val::Cons(head, tail) => (*head, *tail),
        Val::Nil => {
            panic!("Cannot take from empty list");
        }
        _ => {
            panic!("Cannot take from not a list: {:?}", l);
        }
    }
}

pub fn take_ref(l: &Val) -> (&Val, &Val)
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

pub fn to_tuple2(l1: Val) -> (Val, Val)
{
    let (i1, l2) = take(l1);
    let (i2, _) = take(l2);
    (i1, i2)
}

pub fn to_tuple3(l1: Val) -> (Val, Val, Val)
{
    let (i1, l2) = take(l1);
    let (i2, l3) = take(l2);
    let (i3, _) = take(l3);
    (i1, i2, i3)
}

pub fn to_tuple4(l1: Val) -> (Val, Val, Val, Val)
{
    let (i1, l2) = take(l1);
    let (i2, l3) = take(l2);
    let (i3, l4) = take(l3);
    let (i4, _) = take(l4);
    (i1, i2, i3, i4)
}

pub fn to_ref_tuple2(l1: &Val) -> (&Val, &Val)
{
    let (i1, l2) = take_ref(l1);
    let (i2, _) = take_ref(l2);
    (i1, i2)
}

pub fn to_ref_tuple3(l1: &Val) -> (&Val, &Val, &Val)
{
    let (i1, l2) = take_ref(l1);
    let (i2, l3) = take_ref(l2);
    let (i3, _) = take_ref(l3);
    (i1, i2, i3)
}

pub fn to_ref_tuple4(l1: &Val) -> (&Val, &Val, &Val, &Val)
{
    let (i1, l2) = take_ref(l1);
    let (i2, l3) = take_ref(l2);
    let (i3, l4) = take_ref(l3);
    let (i4, _) = take_ref(l4);
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
fn test_map()
{
    let l =
        list::cons(Val::Bool(true),
        list::cons(Val::Bool(false),
        list::cons(Val::Bool(true),
        Val::Nil,
    )));

    let not_l = list::map(l, |v| {
        if let Val::Bool(b) = v {
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

}
