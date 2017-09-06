
use leema::frame;
use leema::val::{Val, MsgVal};
use leema::worker;

use std;
use std::fmt;
// std::sync::mpsc::{channel, Sender, Receiver};
use std::collections::{HashMap, LinkedList};

use mopa;

/*
Rsrc
Resource

RsrcOp
RsrcAction
ResourceAction
IoAction

Iop
IoResult

Ioq
ResourceQueue
IoEvent
*/

pub trait Rsrc
    : mopa::Any
    + fmt::Debug
{
}

mopafy!(Rsrc);

pub type IoResult = Fn(Val, Box<Rsrc>);
pub type RsrcAction = fn(Box<IoResult>, Box<Rsrc>, Vec<Val>) -> frame::Event;
    // -> IoFuture(Box<future::Future<Item=(), Error=()>>);

#[derive(Debug)]
enum IoMsg
{
    Iop(i64, RsrcAction, Vec<MsgVal>),
}

#[derive(Debug)]
pub struct Iop
{
    pub action: Box<RsrcAction>,
    pub params: Vec<Val>,
    pub src_worker_id: i64,
}

pub struct Ioq
{
    rsrc: Option<Box<Rsrc>>,
    queue: LinkedList<Iop>,
}

struct Io
{
    resource: HashMap<i64, Ioq>,
    msg_rx: std::sync::mpsc::Receiver<IoMsg>,
    app_tx: std::sync::mpsc::Sender<worker::Msg>,
    worker_tx: HashMap<i64, std::sync::mpsc::Sender<worker::Msg>>,
}

