
use leema::lstr::{Lstr};
use leema::val::{Val};

pub struct Struple(Vec<(Option<Lstr>, Val)>);

pub struct Struple1
{
    indexed: Option<Vec<Val>>,
    keyed: Option<Vec<(Lstr, Val)>>,
}

impl Struple
{
    pub fn new_tuple2(a: Val, b: Val) -> Struple
    {
        Struple(vec![
            (None, a),
            (None, b),
        ])
    }
}
