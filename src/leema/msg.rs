
use leema::code::{Code};
use leema::rsrc::{self, Rsrc};
use leema::val::{MsgVal, Val};


#[derive(Debug)]
pub enum AppMsg
{
    // Spawn(module, function)
    Spawn(String, String),
    // RequestCode(worker_id, frame_id, module, function)
    RequestCode(i64, i64, String, String),
    MainResult(MsgVal),
}

#[derive(Debug)]
pub enum WorkerMsg
{
    // Spawn(module, function)
    Spawn(String, String),
    // FoundCode(frame_id, module, function, code)
    FoundCode(i64, String, String, Code),
    // IopResult(fiber_id, MsgVal)
    IopResult(i64, MsgVal),
    Done,
}

#[derive(Debug)]
pub enum IoMsg
{
    Iop{
        worker_id: i64,
        frame_id: i64,
        action: rsrc::Action,
        params: Vec<MsgVal>,
    },
}
