

pub trait SendClone
{
    type Item;

    fn clone_for_send(&self) -> Self::Item;
}

#[derive(Debug)]
pub struct SafeToSend<T>(T);

impl<T> SafeToSend<T>
    where T: SendClone<Item = T>
{
    pub fn new(i: &T) -> SafeToSend<T>
    {
        SafeToSend(i.clone_for_send() as T)
    }

    pub fn take(self) -> T
    {
        self.0
    }
}

unsafe impl<T> Send for SafeToSend<T> {}
