
use leema::frame;
use leema::val::{Val, Type};

use std::fmt;

use mopa;


pub trait Rsrc
    : mopa::Any
    + fmt::Debug
{
    fn get_type(&self) -> Type;
}

mopafy!(Rsrc);

pub type Result = Fn(Val, Box<Rsrc>);
pub type Action = fn(Box<Result>, Box<Rsrc>, Vec<Val>) -> frame::Event;
