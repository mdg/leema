use crate::leema::failure::{self, Lresult};
// use crate::leema::io::{Io, IoLoop};
use crate::leema::loader::Interloader;
use crate::leema::msg::{AppMsg, IoMsg, WorkerMsg};
use crate::leema::program;
use crate::leema::struple::Struple2;
use crate::leema::val::{Fref, Val};
use crate::leema::worker::{Worker, WorkerSeed};

use std::collections::HashMap;

use tokio::sync::mpsc::error::TryRecvError;
use tokio::sync::mpsc::{channel, Receiver, Sender};
use tokio::task::{self, JoinHandle};


/// let caller = Application::start(loader);
/// let call = caller.call(fref, args);
/// let result = call.wait_for_result();
/// let async_result = caller.call_async(fref, args).await;
pub struct Application
{
    app_recv: Receiver<AppMsg>,
    app_send: Sender<AppMsg>,
    io_recv: Option<Receiver<IoMsg>>,
    io_send: Sender<IoMsg>,
    worker: HashMap<i64, Sender<WorkerMsg>>,
    calls: Vec<(Sender<Val>, Fref, Struple2<Val>)>,
    args: Val,
    result: Option<Val>,
    done: bool,
    last_worker_id: i64,
}

impl Application
{
    pub fn start(inter: Interloader) -> TaskQueue
    {
        let app = Application::new();
        let tasks = TaskQueue::new(app.app_send.clone());
        app.start_runtime(inter);
        tasks
    }

    fn new() -> Application
    {
        let (tx, rx) = channel(100);
        let (iotx, iorx) = channel(100);
        Application {
            app_recv: rx,
            app_send: tx,
            io_recv: Some(iorx),
            io_send: iotx,
            worker: HashMap::new(),
            calls: Vec::new(),
            args: Val::Nil,
            result: None,
            done: false,
            last_worker_id: 0,
        }
    }

    pub fn caller(&self) -> AppCaller
    {
        AppCaller {
            app_send: self.app_send.clone(),
        }
    }

    pub fn run_main(
        _inter: Interloader,
        mainf: Fref,
        _args: Val,
    ) -> (Application, Receiver<Val>)
    {
        let app = Application::new();
        let caller = app.caller();
        let result_recv = caller.push_call(mainf, vec![]);
        (app, result_recv)
    }

    pub fn run_lib(_inter: Interloader) -> Application
    {
        Application::new()
    }

    #[tokio::main]
    async fn start_runtime(self, inter: Interloader)
    {
        self.run(inter).await;
    }

    async fn run(mut self, inter: Interloader)
    {
        self.start_io(inter);
        let _wh1 = {
            let seed = self.new_worker().await;
            Self::start_worker(seed)
        };
        let _apph = self.spawn_app_loop();
    }

    fn start_io(&mut self, inter: Interloader) // -> thread::JoinHandle<()>
    {
        let _prog = program::Lib::new(inter);
        // let app_send = self.app_send.clone();
        /*
        thread::Builder::new()
            .name("leema-io".to_string())
            .spawn(move || {
                let rcio = Io::new(app_send, io_recv, prog);
                IoLoop::run(rcio);
            })
            .unwrap()
            */
    }

    async fn new_worker(&mut self) -> WorkerSeed
    {
        let worker_id = self.next_worker_id();
        let app_send = self.app_send.clone();
        let io_send = self.io_send.clone();
        let (worker_send, worker_recv) = channel(100);
        self.worker.insert(worker_id, worker_send.clone());
        self.io_send
            .send(IoMsg::NewWorker(worker_id, worker_send))
            .await.expect("fail to send worker to io thread");
        WorkerSeed {
            wid: worker_id,
            app_send,
            io_send,
            worker_recv,
        }
    }

    async fn start_worker(seed: WorkerSeed) -> JoinHandle<()>
    {
        let handle = tokio::spawn(async move {
            vout!("start worker {}\n", seed.wid);
            let w = Worker::init(seed);
            Worker::run(w);
        });
        handle
    }

    pub fn next_worker_id(&mut self) -> i64
    {
        self.last_worker_id += 1;
        self.last_worker_id
    }

    fn spawn_app_loop(mut self) -> JoinHandle<()>
    {
        task::spawn(async move {
            while !self.done {
                self.iterate().await;
            }
        })
    }

    /// DELETEME
    pub fn wait_for_result(
        &mut self,
        _result_recv: Receiver<Val>,
    ) -> Option<Val>
    {
        None
    }

    /// DELETEME
    pub fn try_recv_result(&mut self, result_recv: &mut Receiver<Val>)
    {
        match result_recv.try_recv() {
            Ok(result) => {
                self.result = Some(result);
                self.done = true;
            }
            Err(TryRecvError::Empty) => {
                // do nothing, not finished yet
            }
            Err(TryRecvError::Disconnected) => {
                println!("error receiving application result");
                self.result = Some(Val::Int(3));
                self.done = true;
            }
        }
    }

    async fn iterate(&mut self) -> bool
    {
        let mut did_something = false;
        for call in self.calls.drain(..) {
            let (dst, call, args) = call;
            vout!("application call {}({:?})\n", call, args);
            let w = self.worker.values().next().unwrap();
            let msg = WorkerMsg::Spawn(dst, call, args);
            w.send(msg).await
                .expect("fail sending spawn call to worker");
            did_something = true;
        }

        while let Result::Ok(msg) = self.app_recv.try_recv() {
            self.process_msg(msg);
            did_something = true;
        }
        did_something
    }

    pub fn process_msg(&mut self, msg: AppMsg)
    {
        vout!("Received a message! {:?}\n", msg);
        match msg {
            AppMsg::MainResult(mv) => {
                self.result = Some(mv.take());
                self.done = true;
            }
            AppMsg::Spawn(result_dst, func, args) => {
                self.calls.push((result_dst, func, args));
            }
        }
    }

    pub fn take_result(&mut self) -> Option<Val>
    {
        self.result.take()
    }
}

/*
#[derive(Debug)]
pub struct CallHandle
{
    result: FutureReceiver<Val>,
}

impl Future for CallHandle
{
    type Item = rsrc::Event;
    type Error = rsrc::Event;

    fn poll(&mut self) -> Poll<rsrc::Event, rsrc::Event>
    {
        Ok(Async::NotReady)
    }
}
*/

#[derive(Debug)]
pub struct TaskResult
{
    f: Fref,
    result: Receiver<Val>,
}

impl TaskResult
{
    pub fn new(f: Fref, result: Receiver<Val>) -> TaskResult
    {
        TaskResult {f, result}
    }

    pub fn wait(
        &mut self,
    ) -> Lresult<Val>
    {
        vout!("TaskResult.wait\n");
        let result = ltry!(self.result.blocking_recv().ok_or_else(|| {
            lfail!(
                failure::Mode::Underflow,
                "no result received",
                "func": ldisplay!(self.f),
            )
        }));
        Ok(result)
    }
}

#[derive(Clone)]
#[derive(Debug)]
pub struct TaskQueue
{
    app_send: Sender<AppMsg>,
}

impl TaskQueue
{
    pub fn new(app_send: Sender<AppMsg>) -> TaskQueue
    {
        TaskQueue {app_send}
    }

    pub fn spawn(&self, call: Fref, args: Struple2<Val>) -> TaskResult
    {
        let (result_send, result_recv) = channel(1);
        let result = self.app_send
            .blocking_send(AppMsg::Spawn(result_send, call.clone(), args));
        if let Err(e) = result {
            eprintln!("send error: {:?}", e);
        }
        TaskResult::new(call, result_recv)
    }
}

#[derive(Clone)]
#[derive(Debug)]
pub struct AppCaller
{
    app_send: Sender<AppMsg>,
}

impl AppCaller
{
    pub fn push_call(&self, call: Fref, args: Struple2<Val>) -> Receiver<Val>
    {
        let (result_send, result_recv) = channel(1);
        self.app_send
            .blocking_send(AppMsg::Spawn(result_send, call, args))
            .unwrap();
        result_recv
    }

    pub fn wait_for_result(&mut self, mut result_recv: Receiver<Val>) -> Val
    {
        vout!("wait_for_result\n");
        // this name is a little off. it's # of cycles when nothing was done
        let mut result = None;
        while result.is_none() {
            result = self.try_recv_result(&mut result_recv);
        }
        vout!("result received\n");
        result.unwrap()
    }

    pub fn try_recv_result(
        &mut self,
        result_recv: &mut Receiver<Val>,
    ) -> Option<Val>
    {
        match result_recv.try_recv() {
            Ok(result) => Some(result),
            Err(TryRecvError::Empty) => {
                // do nothing, not finished yet
                None
            }
            Err(TryRecvError::Disconnected) => {
                println!("error receiving application result");
                Some(Val::Failure2(Box::new(lfail!(
                    failure::Mode::RuntimeLeemaFailure,
                    "disconnected before receiving result"
                ))))
            }
        }
    }
}


/*
enum Stype
| Complete(Type)
| Var(String)
| Anon
--

enum Itype
| Complete(Type),
| Var(String),
| Infernode(Itype, Itype),
--
*/


#[cfg(test)]
mod tests
{
    use crate::leema::application::Application;
    use crate::leema::loader::Interloader;
    use crate::leema::module::ModKey;
    use crate::leema::struple::StrupleItem;
    use crate::leema::val::{Fref, Val};

    use libc::getpid;
    use std::io::{stderr, Write};
    use std::path::PathBuf;


    #[test]
    fn test_main_func_finishes()
    {
        let p = unsafe {
            getpid();
        };
        writeln!(stderr(), "test_main_func_finishes {:?}", p).unwrap();
        let input = "func main >> 3 --".to_string();
        let path = vec![PathBuf::from("lib")];
        let mut loader = Interloader::new("test.lma", path);
        let test_key = ModKey::from("/test");
        loader.set_mod_txt(test_key.clone(), input);

        let mainf = Fref::with_modules(test_key, "main");
        let tasks = Application::start(loader);
        let mut result = tasks.spawn(mainf, vec![StrupleItem::new_v(Val::Nil)]);

        writeln!(stderr(), "wait until done").unwrap();
        let result = result.wait().unwrap();
        assert_eq!(Val::Int(3), result);
    }
}
