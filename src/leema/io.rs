
/*
Rsrc
Resource
IoResource

RsrcOp
RsrcAction
ResourceAction
Iop
IoAction

IoResult

Ioq
ResourceQueue
IoEvent

Io
Event
*/

type ResourceAction

enum IoMsg
{
    Iop(i64, ResourceOp, Vec<MsgVal>),
}

#[derive(Debug)]
pub struct Iop
{
    pub action: Box<IopAction>,
    pub params: Vec<Val>,
    pub src_worker_id: i64,
}

pub struct Ioq
{
    rsrc: Option<Box<Resource>>,
    queue: LinkedList<Iop>,
}

struct Io
{
    resource: HashMap<i64, Ioq>,
    msg_rx: Receiver<IoMsg>,
    app_tx: Sender<Msg>,
    worker_tx: HashMap<i64, Sender<WorkerMsg>>,
}

