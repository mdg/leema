use leema::val::{Val};
use leema::reg;
use std::fmt::{self, Display, Debug, Formatter};
use std::collections::{LinkedList};


pub fn cons(head: Val, tail: Val) -> Val
{
	match tail {
		Val::Sexpr(t, oldt) => {
			Val::Sexpr(t, Box::new(cons(head, *oldt)))
		}
		Val::Cons(_, _) => {
			Val::Cons(Box::new(head), Box::new(tail))
		}
		Val::Nil => {
			Val::Cons(Box::new(head), Box::new(Val::Nil))
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

pub fn is_empty(l: &Val) -> bool
{
	match l {
		&Val::Nil => true,
		&Val::Cons(_, _) => false,
		&Val::Sexpr(_, ref head) => {
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

pub fn len(l: &Val) -> usize {
	fold_ref(l, 0, |res, _| { res + 1 })
}

pub fn map<F>(l: Val, op: F) -> Val
	where F: Fn(Val) -> Val
{
	//list::from_ll(map_to_ll(l, op))
		/*
		let mut l = input;
		let mut result = List::Nil;
		while l != List::Nil {
			let (head, tail) = List::take(l);
			let single = op(head);
			result = List::cons(single, result);
			l = tail;
		}
		List::reverse(result)
		*/
	Val::Nil
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

pub fn map_to_vec<R, F>(l: Val, op: F) -> Vec<R>
	where F: Fn(Val) -> R
{
	let mut it = l;
	let acc = Vec::new();
	while it != Val::Nil {
	}
	acc
}

pub fn fold<R, F>(l: Val, init: R, op: F) -> R
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

pub fn fold_ref<R, F>(l: &Val, init: R, op: F) -> R
	where F: Fn(R, &Val) -> R
{
	let mut result = init;
	let mut curr = l;
	while *curr != Val::Nil {
		let (head, tail) = take_ref(l);
		result = op(result, head);
		curr = tail;
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

pub fn take_head(l: Val) -> Val
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


#[derive(Clone)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
pub enum List {
	Cons {
		h: Box<Val>,
		t: Box<List>,
	},
	Nil,
}

impl List {

	pub fn head(&self) -> &Val
	{
		match self {
			&List::Nil => {
				panic!("no head for empty list");
			}
			&List::Cons{h:ref head, ..} => {
				&*head
			}
		}
	}

	pub fn mut_tail(&mut self) -> &mut List
	{
		match self {
			&mut List::Nil => {
				panic!("cannot get mut tail for empty list");
			}
			&mut List::Cons{t:ref mut tail, ..} => {
				&mut *tail
			}
		}
	}

	/*
	pub fn iter(&self) -> ListIter
	{
		ListIter{head: self}
	}
	*/
}

/*
pub struct ListIter<'a> {
	head: &'a List,
}

impl<'a> Iterator for ListIter<'a>
{
	type Item = &'a Val;

	fn next(&mut self) -> Option<&'a Val>
	{
		match self.head {
			&List::Nil => None,
			&List::Cons{h:ref head, t:ref tail} => {
				self.head = &*tail;
				Some(&*head)
			}
		}
	}
}
*/


#[cfg(test)]
mod tests {
	use leema::val::{Val};


}
