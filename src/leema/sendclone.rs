pub trait SendClone
{
    type Item;

    fn clone_for_send(&self) -> Self::Item;
}

impl<T> SendClone for Option<T>
where
    T: SendClone<Item = T>,
{
    type Item = Option<T>;

    fn clone_for_send(&self) -> Option<T>
    {
        self.as_ref().map(|v| v.clone_for_send())
    }
}

impl SendClone for &'static str
{
    type Item = &'static str;

    fn clone_for_send(&self) -> &'static str
    {
        self
    }
}
