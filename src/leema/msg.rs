
use leema::val::{MsgVal};
use leema::code::{Code};

#[derive(Debug)]
pub enum Msg
{
    Call(String, String),
    RequestCode(i64, i64, String, String),
    MainResult(MsgVal),
    FoundCode(i64, String, String, Code),
}
