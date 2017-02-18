

#[derive(Debug)]
pub enum MsgVal
{
    Int(i64),
    Str(String),
    Bool(bool),
    Hashtag(String),
    Cons(Box<MsgVal>, Box<MsgVal>),
    Nil,
}

#[derive(Debug)]
pub enum Msg
{
    Call(String, String),
    RequestCode(String, String),
    MainResult(MsgVal),
}
