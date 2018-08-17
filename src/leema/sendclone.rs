pub trait SendClone
{
    type Item;

    fn clone_for_send(&self) -> Self::Item;
}
