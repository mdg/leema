
use leema::code::{Code};
use leema::rsrc::{IopAction};
use leema::sendclone::{SendClone};
use leema::val::{MsgVal};

use std::fmt;
use std::sync::mpsc;


#[derive(Debug)]
pub struct MsgItem<T>(T);

impl<T> MsgItem<T>
    where T: SendClone<Item = T>
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

unsafe impl<T> Send for MsgItem<T> {}

#[derive(Debug)]
pub enum AppMsg
{
    // Spawn(module, function)
    Spawn(String, String),
    // RequestCode(worker_id, fiber_id, module, function)
    RequestCode(i64, i64, String, String),
    MainResult(MsgVal),
}

#[derive(Debug)]
pub enum WorkerMsg
{
    // Spawn(module, function)
    Spawn(String, String),
    // FoundCode(fiber_id, module, function, code)
    FoundCode(i64, String, String, Code),
    // IopResult(fiber_id, MsgVal)
    IopResult(i64, MsgVal),
    Done,
}

pub enum IoMsg
{
    Iop{
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
            &IoMsg::Iop{worker_id, fiber_id, ref params, ..} => {
                write!(f, "IoMsg::Iop({}:{}, {:?})"
                    , worker_id, fiber_id, params)
            }
            &IoMsg::NewWorker(worker_id, _) => {
                write!(f, "IoMsg::NewWorker({})", worker_id)
            }
            &IoMsg::Done => {
                write!(f, "IoMsg::Done")
            }
        }
    }
}
