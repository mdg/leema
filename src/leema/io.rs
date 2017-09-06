
use leema::frame;
use leema::msg::{WorkerMsg, AppMsg, IoMsg};
use leema::rsrc::{self, Rsrc};
use leema::val::{Val, MsgVal};
use leema::worker;

use std;
use std::fmt;
// std::sync::mpsc::{channel, Sender, Receiver};
use std::collections::{HashMap, LinkedList};

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

#[derive(Debug)]
pub struct Iop
{
    pub action: Box<rsrc::Action>,
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
    app_tx: std::sync::mpsc::Sender<WorkerMsg>,
    worker_tx: HashMap<i64, std::sync::mpsc::Sender<WorkerMsg>>,
}

