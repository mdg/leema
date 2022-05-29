use crate::leema::failure::{self, Lresult};
use crate::leema::io::{Io, IoLoop};
use crate::leema::loader::Interloader;
use crate::leema::msg::{self, AppMsg, IoMsg, SpawnMsg, WorkerMsg};
use crate::leema::program;
use crate::leema::struple::Struple2;
use crate::leema::val::{Fref, Val};
use crate::leema::worker::{Worker, WorkerSeed};

use std::collections::HashMap;
use std::sync::mpsc::{sync_channel, Receiver, SyncSender, TryRecvError};
use std::thread;

/// let caller = Application::start(loader);
/// let call = caller.call(fref, args);
/// let result = call.wait_for_result();
/// let async_result = caller.call_async(fref, args).await;
pub struct Application
{
    app_recv: Receiver<AppMsg>,
    app_send: SyncSender<AppMsg>,
    io_send: SyncSender<IoMsg>,
    worker: HashMap<i64, SyncSender<WorkerMsg>>,
    args: Val,
    result: Option<Val>,
    done: bool,
    last_worker_id: i64,
}

impl Application
{
    pub fn start(inter: Interloader) -> TaskQueue
    {
        let (spawn_tx, spawn_rx) = sync_channel(100);
        let (app, io_rx) = Application::new();
        let tasks = TaskQueue::new(app.app_send.clone(), spawn_tx);
        app.run(inter, io_rx, spawn_rx);
        vout!("Application running...\n");
        tasks
    }

    fn new() -> (Application, Receiver<IoMsg>)
    {
        let (spawn_tx, spawn_rx) = sync_channel(100);
        let (iotx, iorx) = sync_channel(100);
        let app = Application {
            app_recv: spawn_rx,
            app_send: spawn_tx,
            io_send: iotx,
            worker: HashMap::new(),
            args: Val::Nil,
            result: None,
            done: false,
            last_worker_id: 0,
        };
        (app, iorx)
    }

    fn run(
        mut self,
        inter: Interloader,
        io_rx: Receiver<IoMsg>,
        spawn_rx: Receiver<SpawnMsg>,
    )
    {
        vout!("Application::run\n");
        let spawn = msg::SpawnReceiver::new(spawn_rx);
        self.start_io(inter, io_rx);
        let _wh0 = self.start_worker(spawn);
        // let _wh1 = self.start_worker(spawn);
        self.spawn_app_loop();
    }

    fn start_io(
        &mut self,
        inter: Interloader,
        io_rx: Receiver<IoMsg>,
    ) -> thread::JoinHandle<()>
    {
        let prog = program::Lib::new(inter);
        thread::Builder::new()
            .name("leema-io".to_string())
            .spawn(move || {
                let rcio = Io::new(io_rx, prog);
                IoLoop::run(rcio);
            })
            .unwrap()
    }

    fn new_worker(&mut self, spawn: msg::SpawnReceiver) -> WorkerSeed
    {
        let worker_id = self.next_worker_id();
        let io_send = self.io_send.clone();
        let (worker_send, worker_recv) = sync_channel(100);
        self.worker.insert(worker_id, worker_send.clone());
        io_send
            .send(IoMsg::NewWorker(worker_id, worker_send))
            .expect("fail to send worker to io thread");
        WorkerSeed {
            wid: worker_id,
            io_send,
            worker_recv,
            spawn,
        }
    }

    fn start_worker(
        &mut self,
        spawn: msg::SpawnReceiver,
    ) -> std::thread::JoinHandle<()>
    {
        let seed = self.new_worker(spawn);
        std::thread::spawn(move || {
            let w = Worker::init(seed);
            Worker::run(w);
            /*
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
            */
        })
    }

    pub fn next_worker_id(&mut self) -> i64
    {
        self.last_worker_id += 1;
        self.last_worker_id
    }

    fn handle_app_msg(&mut self, m: AppMsg)
    {
        match m {
            /*
            AppMsg::Spawn(result_tx, f, args) => {
                // send to a worker
                let wm = WorkerMsg::Spawn(result_tx, f, args);
                let w = self.worker.values_mut().next().unwrap();
                w.send(wm);
                /*
                let id = NEXT_FIBER_ID.fetch_add(1, Ordering::SeqCst);
                let id2 = NEXT_FIBER_ID.fetch_add(1, Ordering::SeqCst);
                task::spawn(move {
                    let fib = new_fiber(f, args, Some(result_tx));
                    run_fiber_loop(fib).await
                })
                */
            }
            */
            AppMsg::Stop => {
                panic!("stop");
            }
        }
    }

    fn iterate(&mut self)
    {
        match self.app_recv.try_recv() {
            Ok(m) => {
                self.handle_app_msg(m);
            }
            Err(TryRecvError::Empty) => {
                // no messages, do nothing
            }
            Err(TryRecvError::Disconnected) => {
                eprintln!("app queue disconnected");
                self.stop();
            }
        }
    }

    fn spawn_app_loop(mut self) -> thread::JoinHandle<()>
    {
        thread::Builder::new()
            .name("leema-app".to_string())
            .spawn(move || {
                while !self.done {
                    self.iterate();
                }
            })
            .unwrap()
    }

    fn stop(&mut self)
    {
        // send a stop msg to the workers and io threads/tasks
        self.done = true;
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
        let result = ltry!(self.result.recv().map_err(|e| {
            lfail!(
                failure::Mode::Underflow,
                "no result received",
                "error": ldisplay!(e),
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
    app_send: SyncSender<AppMsg>,
    spawn_send: SyncSender<SpawnMsg>,
}

impl TaskQueue
{
    pub fn new(
        app: SyncSender<AppMsg>,
        spawn: SyncSender<SpawnMsg>,
    ) -> TaskQueue
    {
        TaskQueue {
            app_send: app,
            spawn_send: spawn,
        }
    }

    pub fn spawn(&self, call: Fref, args: Struple2<Val>) -> TaskResult
    {
        vout!("spawn {} {:?}\n", call, args);
        let (result_send, result_recv) = sync_channel(1);
        let result = self.spawn_send.send(SpawnMsg::Spawn(
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
