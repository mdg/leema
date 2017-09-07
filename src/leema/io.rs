
use leema::frame::{self, Event};
use leema::msg::{WorkerMsg, AppMsg, IoMsg};
use leema::rsrc::{self, Rsrc};
use leema::val::{Val, MsgVal};
use leema::worker;

use std;
use std::fmt;
// std::sync::mpsc::{channel, Sender, Receiver};
use std::collections::{HashMap, LinkedList};

use futures::{Poll, Async};

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

impl Ioq
{
    /**
     * Checkout a resource object and return it w/ the Iop
     *
     * If the resource is already in used, then return None
     */
    pub fn checkout(&mut self, worker_id: i64, iopf: Box<rsrc::Action>
        , args: Vec<Val>) -> Option<(Box<Rsrc>, Iop)>
    {
        let iop = Iop{
            action: iopf,
            params: args,
            src_worker_id: worker_id,
        };

        match self.rsrc.take() {
            Some(r) => {
                Some((r, iop))
            }
            None => {
                self.queue.push_back(iop);
                None
            }
        }
    }

    /**
     * Add the resource back to the Ioq to be used later
     */
    pub fn checkin(&mut self, r: Box<Rsrc>) -> Option<(Box<Rsrc>, Iop)>
    {
        match self.queue.pop_front() {
            Some(iop) => {
                Some((r, iop))
            }
            None => {
                self.rsrc = Some(r);
                None
            }
        }
    }
}

struct Io
{
    resource: HashMap<i64, Ioq>,
    msg_rx: std::sync::mpsc::Receiver<IoMsg>,
    app_tx: std::sync::mpsc::Sender<WorkerMsg>,
    worker_tx: HashMap<i64, std::sync::mpsc::Sender<WorkerMsg>>,
}

impl Io
{
}
