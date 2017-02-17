
use leema::code::{Code};
use leema::frame::{Frame};
use leema::log;
use leema::msg::{Msg};

use std::collections::{HashMap, LinkedList};
use std::io::{stderr, Write};
use std::sync::mpsc::{channel, Sender, Receiver};
use std::thread;


pub struct Worker
{
    fresh: LinkedList<(Code, Frame)>,
    code: HashMap<String, HashMap<String, Code>>,
    tx: Sender<Msg>,
    rx: Receiver<Msg>,
    worker_id: i64,
    code_request_idx: u64,
    done: bool,
}

impl Worker
{
    pub fn new(wid: i64, send: Sender<Msg>, recv: Receiver<Msg>) -> Worker
    {
        Worker{
            fresh: LinkedList::new(),
            code: HashMap::new(),
            tx: send,
            rx: recv,
            worker_id: wid,
            code_request_idx: 0,
            done: false,
        }
    }

    pub fn run(&mut self)
    {
        while !self.done {
            self.iterate();
            thread::yield_now();
        }
    }

    pub fn iterate(&mut self)
    {
        // vout!("iterate worker {}\n", self.worker_id);
        while let Result::Ok(msg) = self.rx.try_recv() {
            self.process_msg(msg);
        }
    }

    pub fn process_msg(&mut self, msg: Msg)
    {
        vout!("received message: {:?}\n", msg);
        match msg {
            Msg::Call(module, call) => {
                self.create_frame(module, call);
            }
            Msg::RequestCode(module, call) => {
                panic!("Cannot request code from a worker");
            }
        }
    }

    pub fn create_frame(&mut self, module: String, func: String)
    {
        let m = self.code.get(&module);
        let code = if m.is_some() {
            m.unwrap().get(&func)
        } else {
            None
        };
        if code.is_some() {
            vout!("make new frame with {}.{}\n", module, func);
        } else {
            self.tx.send(Msg::RequestCode(module, func));
        }
        //push_frame(get_code(module, func))
    }

    /*
    pub fn get_code(&mut self, module: &str, func: &str)
    {
        c = self.code.find(module, func);
        if c.is_none() {
            i = self.new_code_request();
            c = self.app_channel.push(CodeRequest(i, module, func));
            frame.wait_on_code(i)
        }
        c
    }
    */

    pub fn new_code_request(&mut self) -> u64
    {
        let idx = self.code_request_idx;
        self.code_request_idx += 1;
        idx
    }
}
