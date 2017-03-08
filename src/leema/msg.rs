
use leema::val::{MsgVal};
use leema::code::{Code};

#[derive(Debug)]
pub enum Msg
{
    // Call(module, function)
    Call(String, String),
    // RequestCode(worker_id, frame_id, module, function)
    RequestCode(i64, i64, String, String),
    // FoundCode(frame_id, module, function, code)
    FoundCode(i64, String, String, Code),
    MainResult(MsgVal),
}
