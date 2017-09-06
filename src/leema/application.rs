use leema::loader::{Interloader};
use leema::program;
use leema::msg::{AppMsg, WorkerMsg};
use leema::worker::{Worker};
use leema::code::{Code};
use leema::val::{Val};
use leema::log;

use std::collections::{HashMap, LinkedList};
use std::sync::mpsc::{channel, Sender, Receiver};
use std::thread;
use std::io::{stderr, Write};


pub struct Application
{
    prog: program::Lib,
    app_recv: Receiver<AppMsg>,
    app_send: Sender<AppMsg>,
    worker: HashMap<i64, Sender<WorkerMsg>>,
    calls: LinkedList<(String, String)>,
    result: Option<Val>,
    done: bool,
    last_worker_id: i64,
}

impl Application
{
    pub fn new(prog: program::Lib) -> Application
    {
        let (tx, rx) = channel();
        Application{
            prog: prog,
            app_recv: rx,
            app_send: tx,
            worker: HashMap::new(),
            calls: LinkedList::new(),
            result: None,
            done: false,
            last_worker_id: 0,
        }
    }

    pub fn push_call(&mut self, module: &str, func: &str)
    {
        self.calls.push_back((String::from(module), String::from(func)));
    }

    pub fn run(&mut self)
    {
        let t1 = self.start_worker();
        let t2 = self.start_worker();
    }

    pub fn next_worker_id(&mut self) -> i64
    {
        self.last_worker_id += 1;
        self.last_worker_id
    }

    fn start_worker(&mut self) -> thread::JoinHandle<()>
    {
        let worker_id = self.next_worker_id();
        let app_send = self.app_send.clone();
        let (worker_send, worker_recv) = channel();
        vout!("start worker {}\n", worker_id);
        let handle = thread::spawn(move || {
            Worker::run(worker_id, app_send, worker_recv);
        });
        self.worker.insert(worker_id, worker_send);
        handle
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
            w.send(WorkerMsg::Spawn(module, call));
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
                );
            }
            AppMsg::MainResult(mv) => {
                self.result = Some(Val::from_msg(mv));
                self.done = true;
            }
            AppMsg::Spawn(module, call) => {
                panic!("whoa a spawn msg sent to Application");
            }
        }
    }

    pub fn take_result(&mut self) -> Option<Val>
    {
        self.result.take()
    }

    // pub fn get_interface_code(module: &str, func: &str, typ: &Type) {}
    // pub fn get_protocol_code(module: &str, func: &str, typ: &Vec<Type>) {}
    /*
    pub fn load_code(&mut self, module: &str, func: &str) -> OpVec
    {
        if self.lib.contains(module, func) {
            return self.lib.get((module, func))
        }
        / *
        let ifunc = self.inter.load_func(module, func);
        let tfunc = self.inter.resolve_types(ifunc);
        let new_code = code::make_ops(tfunc);
        self.lib.insert((module, func), new_code);
        new_code
        * /
        vec![]
    }
    */

    pub fn type_mod(module: &str, func: &str) // -> FuncType
    {
        /*
        imod = interload.load_mod(module);
        ifunc = load_func(imod, func);
        tfunc = self.type_check(ifunc);
        self.ftypes.insert((module, func), tfunc);
        tfunc
        */
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
    use leema::log;
    use leema::application::{Application};
    use leema::loader::{Interloader};
    use leema::module::{ModKey, ModuleInterface, ModuleSource};
    use leema::program;
    use leema::val::{Env, Val};

    use std::thread;
    use std::rc::{Rc};
    use std::io::{stderr, Write};
    use libc::{getpid};


#[test]
fn test_main_func_finishes()
{
let p = unsafe { getpid(); };
write!(stderr(), "test_main_func_finishes {:?}\n", p);
    let input = "func main() -> 3 --".to_string();
    let mut inter = Interloader::new("test.lma");
    inter.set_mod_txt("test", input);
    let prog = program::Lib::new(inter);

    let mut app = Application::new(prog);
    app.push_call("test", "main");
    app.run();

write!(stderr(), "Application::wait_until_done\n");
    let result = app.wait_for_result();
    assert_eq!(Some(Val::Int(3)), result);
}

}
