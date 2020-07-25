use crate::leema::code::Code;
use crate::leema::lstr::Lstr;
use crate::leema::rsrc::IopAction;
use crate::leema::sendclone::SendClone;
use crate::leema::struple::Struple2;
use crate::leema::val::{Fref, MsgVal, Val};

use std::fmt;
use std::ops::Deref;
use std::sync::mpsc;


#[derive(Debug)]
pub struct MsgItem<T>(T);

impl<T> MsgItem<T>
where
    T: SendClone<Item = T>,
{
    pub fn new(i: &T) -> MsgItem<T>
    {
        MsgItem(i.clone_for_send() as T)
    }

    pub fn take(self) -> T
    {
        self.0
    }
}

impl<T> Deref for MsgItem<T>
{
    type Target = T;

    fn deref(&self) -> &T
    {
        &self.0
    }
}

unsafe impl<T> Send for MsgItem<T> {}

type MsgLstr = MsgItem<Lstr>;

#[derive(Debug)]
pub enum AppMsg
{
    // Spawn(module, function)
    Spawn(mpsc::Sender<Val>, Fref, Struple2<Val>),
    MainResult(MsgVal),
}

#[derive(Debug)]
pub enum WorkerMsg
{
    // Spawn(module, function)
    Spawn(mpsc::Sender<Val>, Fref, Struple2<Val>),
    // FoundCode(fiber_id, fref, code)
    FoundCode(i64, MsgItem<Fref>, Code),
    // IopResult(fiber_id, MsgVal)
    IopResult(i64, MsgVal),
    Done,
}

pub enum IoMsg
{
    Iop
    {
        worker_id: i64,
        fiber_id: i64,
        action: IopAction,
        rsrc_id: Option<i64>,
        params: MsgVal,
    },
    NewWorker(i64, mpsc::Sender<WorkerMsg>),
    Done,
}

impl fmt::Debug for IoMsg
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self {
            &IoMsg::Iop {
                worker_id,
                fiber_id,
                ref params,
                ..
            } => {
                write!(
                    f,
                    "IoMsg::Iop({}:{}, {:?})",
                    worker_id, fiber_id, params
                )
            }
            &IoMsg::NewWorker(worker_id, _) => {
                write!(f, "IoMsg::NewWorker({})", worker_id)
            }
            &IoMsg::Done => write!(f, "IoMsg::Done"),
        }
    }
}
