

pub enum Msg
{
    Int(i64),
    Str(Box<String>),
    Bool(bool),
    Hashtag(Box<String>),
    Cons(Box<Msg>, Box<Msg>),
    Nil,
}
