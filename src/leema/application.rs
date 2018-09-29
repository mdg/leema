use leema::io::{Io, IoLoop};
use leema::log;
use leema::lri::Lri;
use leema::msg::{AppMsg, IoMsg, MsgItem, WorkerMsg};
use leema::program;
use leema::struple::Struple;
use leema::val::Val;
use leema::worker::Worker;

use std::cmp::min;
use std::collections::{HashMap, LinkedList};
use std::sync::mpsc::{channel, Receiver, Sender, TryRecvError};
use std::thread;
use std::time::Duration;


pub struct Application
{
    prog: program::Lib,
    app_recv: Receiver<AppMsg>,
    app_send: Sender<AppMsg>,
    io_recv: Option<Receiver<IoMsg>>,
    io_send: Sender<IoMsg>,
    worker: HashMap<i64, Sender<WorkerMsg>>,
    calls: LinkedList<(Sender<Val>, Lri, Struple<Val>)>,
    args: Val,
    result: Option<Val>,
    done: bool,
    last_worker_id: i64,
}

impl Application
{
    pub fn new(prog: program::Lib) -> Application
    {
        let (tx, rx) = channel();
        let (iotx, iorx) = channel();
        Application {
            prog,
            app_recv: rx,
            app_send: tx,
            io_recv: Some(iorx),
            io_send: iotx,
            worker: HashMap::new(),
            calls: LinkedList::new(),
            args: Val::Nil,
            result: None,
            done: false,
            last_worker_id: 0,
        }
    }

    pub fn set_args(&mut self, args: Val)
    {
        self.args = args;
    }

    pub fn caller(&self) -> AppCaller
    {
        AppCaller {
            app_send: self.app_send.clone(),
        }
    }

    pub fn push_call(&mut self, dst: Sender<Val>, call: Lri, args: Struple<Val>)
    {
        self.calls.push_back((dst, call, args));
    }

    pub fn run(&mut self)
    {
        self.start_io();
        self.start_worker();
        self.start_worker();
    }

    fn start_io(&mut self) -> thread::JoinHandle<()>
    {
        let app_send = self.app_send.clone();
        let io_recv = self.io_recv.take().unwrap();
        thread::Builder::new()
            .name("leema-io".to_string())
            .spawn(move || {
                let rcio = Io::new(app_send, io_recv);
                IoLoop::run(rcio);
            })
            .unwrap()
    }

    fn start_worker(&mut self) -> thread::JoinHandle<()>
    {
        let worker_id = self.next_worker_id();
        let app_send = self.app_send.clone();
        let io_send = self.io_send.clone();
        let (worker_send, worker_recv) = channel();
        vout!("start worker {}\n", worker_id);
        let handle = thread::spawn(move || {
            let w = Worker::init(worker_id, app_send, io_send, worker_recv);
            Worker::run(w);
        });
        self.worker.insert(worker_id, worker_send.clone());
        self.io_send
            .send(IoMsg::NewWorker(worker_id, worker_send))
            .expect("fail to send worker to io thread");
        handle
    }

    pub fn next_worker_id(&mut self) -> i64
    {
        self.last_worker_id += 1;
        self.last_worker_id
    }

    pub fn wait_for_result(
        &mut self,
        mut result_recv: Receiver<Val>,
    ) -> Option<Val>
    {
        vout!("wait_for_result\n");
        // this name is a little off. it's # of cycles when nothing was done
        let mut did_nothing = 0;
        while !self.done {
            if self.iterate() {
                did_nothing = 0;
            } else {
                did_nothing = min(did_nothing + 1, 100_000);
                if did_nothing > 1000 {
                    thread::sleep(Duration::from_micros(did_nothing));
                }
            }
            self.try_recv_result(&mut result_recv);
        }
        vout!("application done\n");
        self.result.take()
    }

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

    pub fn iterate(&mut self) -> bool
    {
        let mut did_something = false;
        while let Some((dst, call, args)) = self.calls.pop_front() {
            vout!("application call {}({:?})\n", call, args);
            let w = self.worker.values().next().unwrap();
            let msg = WorkerMsg::Spawn(dst, call, args);
            w.send(msg).expect("fail sending spawn call to worker");
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
            AppMsg::RequestCode(worker_id, frame, mmodule, mfunc) => {
                let module = mmodule.take();
                let func = mfunc.take();
                let code = self.prog.load_code(&module, &func);
                let worker = self.worker.get(&worker_id).unwrap();
                worker
                    .send(WorkerMsg::FoundCode(
                        frame,
                        MsgItem::new(&module),
                        MsgItem::new(&func),
                        code.clone(),
                    ))
                    .expect("fail to send found code to worker");
            }
            AppMsg::MainResult(mv) => {
                self.result = Some(mv.take());
                self.done = true;
            }
            AppMsg::Spawn(result_dst, func, args) => {
                self.calls.push_back((result_dst, func, args));
            }
        }
    }

    pub fn take_result(&mut self) -> Option<Val>
    {
        self.result.take()
    }

    pub fn handle_result(result: Val) -> i8
    {
        vout!("Result = {:?}\n", result);
        let code: i8 = match result {
            Val::Int(i) => i as i8,
            Val::Void => 0,
            Val::Failure(ref tag, ref msg, ref trace, status) => {
                println!("Failure: {}", tag);
                println!("Message: {}", msg);
                println!("Stack Trace:\n{}", trace);
                status
            }
            res => {
                println!("Result: {}", res);
                -2
            }
        };
        code
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

#[derive(Clone)]
#[derive(Debug)]
pub struct AppCaller
{
    app_send: Sender<AppMsg>,
}

impl AppCaller
{
    pub fn push_call(&self, call: Lri) -> Receiver<Val>
    {
        let (result_send, result_recv) = channel();
        self.app_send
            .send(AppMsg::Spawn(result_send, call, Struple(Vec::new())))
            .unwrap();
        result_recv
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
    use leema::application::Application;
    use leema::loader::Interloader;
    use leema::lri::Lri;
    use leema::lstr::Lstr;
    use leema::program;
    use leema::val::Val;

    use libc::getpid;
    use std::io::{stderr, Write};


    #[test]
    fn test_main_func_finishes()
    {
        let p = unsafe {
            getpid();
        };
        writeln!(stderr(), "test_main_func_finishes {:?}", p).unwrap();
        let input = "func main() -> 3 --".to_string();
        let mut inter = Interloader::new(Lstr::Sref("test.lma"));
        inter.set_mod_txt(Lstr::Sref("test"), input);
        let prog = program::Lib::new(inter);

        let mut app = Application::new(prog);
        let caller = app.caller();
        let recv = caller.push_call(Lri::with_modules(
            Lstr::Sref("test"),
            Lstr::Sref("main"),
        ));
        app.run();

        writeln!(stderr(), "Application::wait_until_done").unwrap();
        let result = app.wait_for_result(recv);
        assert_eq!(Some(Val::Int(3)), result);
    }

}
