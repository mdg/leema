use leema::program;
use leema::io::{Io, IoLoop};
use leema::msg::{AppMsg, WorkerMsg, IoMsg};
use leema::worker::{Worker};
use leema::val::{Val};
use leema::log;

use std::collections::{HashMap, LinkedList};
use std::sync::mpsc::{channel, Sender, Receiver};
use std::thread;
use std::io::{Write};


pub struct Application
{
    prog: program::Lib,
    app_recv: Receiver<AppMsg>,
    app_send: Sender<AppMsg>,
    io_recv: Option<Receiver<IoMsg>>,
    io_send: Sender<IoMsg>,
    worker: HashMap<i64, Sender<WorkerMsg>>,
    calls: LinkedList<(String, String)>,
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
        Application{
            prog: prog,
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

    pub fn push_call(&mut self, module: &str, func: &str)
    {
        self.calls.push_back((String::from(module), String::from(func)));
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
        let handle = thread::spawn(move || {
            let (rcio, core) = Io::new(app_send, io_recv);
            IoLoop::run(core, rcio);
        });
        handle
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
        self.io_send.send(IoMsg::NewWorker(worker_id, worker_send))
            .expect("fail to send worker to io thread");
        handle
    }

    pub fn next_worker_id(&mut self) -> i64
    {
        self.last_worker_id += 1;
        self.last_worker_id
    }

    pub fn wait_for_result(&mut self) -> Option<Val>
    {
        while !self.done {
            self.iterate();
            thread::yield_now();
            // self.done.store(true, Ordering::Relaxed);
        }
        vout!("application done\n");
        self.result.take()
    }

    pub fn iterate(&mut self)
    {
        while let Some((module, call)) = self.calls.pop_front() {
            vout!("application call {}.{}()\n", module, call);
            let w = self.worker.values().next().unwrap();
            w.send(WorkerMsg::Spawn(module, call))
                .expect("fail sending spawn call to worker");
        }

        while let Result::Ok(msg) = self.app_recv.try_recv() {
            self.process_msg(msg);
        }
    }

    pub fn process_msg(&mut self, msg: AppMsg)
    {
        vout!("Received a message! {:?}\n", msg);
        match msg {
            AppMsg::RequestCode(worker_id, frame, module, func) => {
                let code = self.prog.load_code(&module, &func);
                let worker = self.worker.get(&worker_id).unwrap();
                worker.send(
                    WorkerMsg::FoundCode(frame, module, func, code.clone())
                ).expect("fail to send found code to worker");
            }
            AppMsg::MainResult(mv) => {
                self.result = Some(mv.take());
                self.done = true;
            }
            AppMsg::Spawn(_, _) => {
                panic!("whoa a spawn msg sent to Application");
            }
        }
    }

    pub fn take_result(&mut self) -> Option<Val>
    {
        self.result.take()
    }

    pub fn handle_result(result: Option<Val>) -> i8
    {
        vout!("Result = {:?}\n", result);
        if result.is_none() {
            return -4;
        }
        let code: i8 = match result.unwrap() {
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
struct FunctionLib
{
    code: HashMap<String, Code>,
}

struct TypeLoad
{
}

struct RunLoad
{
}
*/

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

enum Ixpr
| Val(Val, Itype)
| Id(String, Itype)
| Call(Ixpr, Vec<Ixpr>, Itype)
| Ixpr(IxprType, Ival)
--
*/


#[cfg(test)]
mod tests {
    use leema::application::{Application};
    use leema::loader::{Interloader};
    use leema::program;
    use leema::val::{Val};

    use std::io::{stderr, Write};
    use libc::{getpid};


#[test]
fn test_main_func_finishes()
{
let p = unsafe { getpid(); };
write!(stderr(), "test_main_func_finishes {:?}\n", p).unwrap();
    let input = "func main() -> 3 --".to_string();
    let mut inter = Interloader::new("test.lma");
    inter.set_mod_txt("test", input);
    let prog = program::Lib::new(inter);

    let mut app = Application::new(prog);
    app.push_call("test", "main");
    app.run();

write!(stderr(), "Application::wait_until_done\n").unwrap();
    let result = app.wait_for_result();
    assert_eq!(Some(Val::Int(3)), result);
}

}
