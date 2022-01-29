use crate::leema::failure::{self, Lresult};
// use crate::leema::io::{Io, IoLoop};
use crate::leema::loader::Interloader;
use crate::leema::msg::{IoMsg, SpawnMsg, WorkerMsg};
use crate::leema::program;
use crate::leema::struple::Struple2;
use crate::leema::val::{Fref, Val};
use crate::leema::worker::{Worker, WorkerSeed};

use std::collections::HashMap;
use std::thread;

use tokio::runtime::Builder;
use tokio::sync::mpsc::error::TryRecvError;
use tokio::sync::mpsc::{channel, Receiver, Sender};
use tokio::task::{self, JoinHandle};


/// let caller = Application::start(loader);
/// let call = caller.call(fref, args);
/// let result = call.wait_for_result();
/// let async_result = caller.call_async(fref, args).await;
pub struct Application
{
    app_recv: Receiver<SpawnMsg>,
    app_send: Sender<SpawnMsg>,
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
        let (app, io_rx) = Application::new();
        let tasks = TaskQueue::new(app.app_send.clone());
        app.run(inter, io_rx);
        tasks
    }

    fn new() -> (Application, Receiver<IoMsg>)
    {
        let (spawn_tx, spawn_rx) = channel(100);
        let (iotx, iorx) = channel(100);
        let app = Application {
            app_recv: spawn_rx,
            app_send: spawn_tx,
            io_send: iotx,
            worker: HashMap::new(),
            calls: Vec::new(),
            args: Val::Nil,
            result: None,
            done: false,
            last_worker_id: 0,
        };
        (app, iorx)
    }

    /*
    fn start_runtime(self, inter: Interloader)
    {
        let rt = Builder::new_multi_thread()
            .thread_name("leema-worker")
            .enable_all()
            .on_thread_start(|| {
                eprintln!("thread start");
            })
            .on_thread_stop(|| {
                eprintln!("thread stop");
            })
            .on_thread_park(|| {
                eprintln!("thread park");
            })
            .on_thread_unpark(|| {
                eprintln!("thread unpark");
            })
            .build()
            .unwrap();

        rt.block_on(async {
            self.run(inter).await;
            task::yield_now().await;
        })
    }
    */

    fn run(mut self, inter: Interloader, io_rx: Receiver<IoMsg>)
    {
        self.start_io(inter, io_rx);
        let wh0 = self.start_worker();
        let a = wh0.join();
        self.spawn_app_loop();
    }

    fn start_io(
        &mut self,
        inter: Interloader,
        _io_rx: Receiver<IoMsg>,
    ) -> thread::JoinHandle<()>
    {
        let _prog = program::Lib::new(inter);
        let _app_send = self.app_send.clone();
        thread::Builder::new()
            .name("leema-io".to_string())
            .spawn(move || {
                // let rcio = Io::new(app_send, io_recv, prog);
                // IoLoop::run(rcio);
            })
            .unwrap()
    }

    fn new_worker(&mut self) -> WorkerSeed
    {
        let worker_id = self.next_worker_id();
        let io_send = self.io_send.clone();
        let (worker_send, worker_recv) = channel(100);
        self.worker.insert(worker_id, worker_send.clone());
        /*
        self.io_send
            .send(IoMsg::NewWorker(worker_id, worker_send))
            .await
            .expect("fail to send worker to io thread");
            */
        let io_msg = IoMsg::NewWorker(worker_id, worker_send);
        WorkerSeed {
            wid: worker_id,
            io_send,
            worker_recv,
            io_msg,
        }
    }

    fn start_worker(&mut self) -> std::thread::JoinHandle<()>
    {
        let seed = self.new_worker();
        std::thread::spawn(move || {
            let rt = Builder::new_current_thread()
                .thread_name("leema-worker")
                .enable_time()
                .on_thread_start(|| {
                    eprintln!("thread start");
                })
                .on_thread_stop(|| {
                    eprintln!("thread stop");
                })
                .on_thread_park(|| {
                    eprintln!("thread park");
                })
                .on_thread_unpark(|| {
                    eprintln!("thread unpark");
                })
                .build()
                .unwrap();

            rt.block_on(async {
                Self::async_start_worker(seed).await;
                task::yield_now().await;
            })
        })
    }

    async fn async_start_worker(seed: WorkerSeed) -> JoinHandle<()>
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

    fn spawn_app_loop(self) -> thread::JoinHandle<()>
    {
        thread::Builder::new()
            .name("leema-app".to_string())
            .spawn(move || {
                while !self.done {
                }
            })
            .unwrap()
    }
}

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
        TaskResult { f, result }
    }

    pub fn wait(&mut self) -> Lresult<Val>
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

    pub fn try_recv_result(&mut self) -> Option<Val>
    {
        match self.result.try_recv() {
            Ok(result) => Some(result),
            Err(TryRecvError::Empty) => {
                // do nothing, not finished yet
                None
            }
            Err(TryRecvError::Disconnected) => {
                println!("error receiving application result");
                Some(Val::Int(3))
            }
        }
    }
}

#[derive(Clone)]
#[derive(Debug)]
pub struct TaskQueue
{
    app_send: Sender<SpawnMsg>,
}

impl TaskQueue
{
    pub fn new(app_send: Sender<SpawnMsg>) -> TaskQueue
    {
        TaskQueue { app_send }
    }

    pub fn spawn(&self, call: Fref, args: Struple2<Val>) -> TaskResult
    {
        let (result_send, result_recv) = channel(1);
        let result = self.app_send.blocking_send(SpawnMsg::Spawn(
            result_send,
            call.clone(),
            args,
        ));
        if let Err(e) = result {
            eprintln!("send error: {:?}", e);
        }
        TaskResult::new(call, result_recv)
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
    use std::path::PathBuf;


    #[test]
    fn test_main_func_finishes()
    {
        let p = unsafe {
            getpid();
        };
        eprintln!("test_main_func_finishes {:?}", p);
        let input = "func main >> 3 --".to_string();
        let path = vec![PathBuf::from("lib")];
        let mut loader = Interloader::new("test.lma", path);
        let test_key = ModKey::from("/test");
        loader.set_mod_txt(test_key.clone(), input);

        let mainf = Fref::with_modules(test_key, "main");
        let tasks = Application::start(loader);
        let mut result = tasks.spawn(mainf, vec![StrupleItem::new_v(Val::Nil)]);

        eprintln!("wait until done");
        let result = result.wait().unwrap();
        assert_eq!(Val::Int(3), result);
    }
}
