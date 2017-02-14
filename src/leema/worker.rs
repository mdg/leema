
use leema::code::{Code};
use leema::frame::{Frame};
use leema::log;
use leema::msg::{Msg};

use std::collections::{HashMap, LinkedList};
use std::io::{stderr, Write};
use std::sync::mpsc::{channel, Sender, Receiver};


pub struct Worker
{
    fresh: LinkedList<(Code, Frame)>,
    code: HashMap<(String, String), Code>,
    app_send: Sender<Msg>,
    worker_id: i64,
    code_request_idx: u64,
    done: bool,
}

impl Worker
{
    pub fn new(wid: i64, app_ch: Sender<Msg>) -> Worker
    {
        Worker{
            fresh: LinkedList::new(),
            code: HashMap::new(),
            app_send: app_ch,
            worker_id: wid,
            code_request_idx: 0,
            done: false,
        }
    }

    pub fn run(&mut self)
    {
        while !self.done {
            self.iterate();
        }
    }

    pub fn iterate(&mut self)
    {
        vout!("iterate worker {}\n", self.worker_id);
    }

    pub fn call_func(&mut self, module: &str, func: &str)
    {
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
