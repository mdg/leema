#[macro_use]
use leema::log;
use leema::val::{Val, Env, FutureVal, Type};
use leema::reg::{Reg, Ireg};
use leema::compile::{StaticSpace};
use leema::code::{self, CodeKey, Code, CodeMap, Op, OpVec, ModSym, RustFunc};
use leema::list;
use std::collections::{HashMap, LinkedList};
use std::sync::{Arc, Mutex, MutexGuard, Condvar};
use std::sync::atomic::{AtomicBool, AtomicIsize, Ordering};
use std::sync::mpsc;
use std::mem;
use std::ops::{Deref};
use std::fmt::{self, Debug};
use std::thread;
use std::time;
use std::io::{stderr, Write};


pub enum Parent
{
    Null,
    Caller(Reg, Code, Box<Frame>),
    Fork(Arc<AtomicBool>, mpsc::Sender<Val>),
    Repl,
    Main,
}

impl Debug for Parent
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self {
            &Parent::Null => write!(f, "Parent::Null"),
            &Parent::Caller(ref dst, ref code, ref pf) => {
                write!(f,
                    "Parent::Caller({:?}, {:?}, {:?})",
                    dst, code, pf
                )
            }
            &Parent::Fork(ref ready, _) => {
                write!(f, "Parent::Fork({:?})", ready)
            }
            &Parent::Repl => write!(f, "Parent::Repl"),
            &Parent::Main => write!(f, "Parent::Main"),
        }
    }
}

#[derive(Debug)]
pub struct Frame
{
    pub parent: Parent,
    pub e: Env,
    pc: i32,
}

impl Frame
{
    pub fn new(par: Parent, env: Env) -> Frame
    {
        Frame{
            parent: par,
            e: env,
            pc: 0,
        }
    }

    pub fn set_parent(&mut self, p: Parent)
    {
        self.parent = p;
    }

    pub fn take_env(&mut self) -> Env
    {
        let mut e = Env::new();
        mem::swap(&mut e, &mut self.e);
        e
    }

    pub fn receive_future(&mut self, r: &Reg) -> Option<Val>
    {
        let fval = self.e.get_reg(&r);
        if !fval.is_future() {
            panic!("This isn't even a future {:?}", fval);
        }
        if !fval.is_future_ready() {
            return None;
        }
        match fval {
            &Val::Future(FutureVal(_, ref amrx)) => {
                let rx = amrx.lock().unwrap();
                Some(rx.recv().unwrap())
            },
            _ => panic!("Not a future val? {:?}", fval),
        }
    }

    /**
     * handy accessor function when calling from rust native functions
     */
    pub fn get_param(&self, p: i8) -> &Val
    {
        self.e.get_reg(&Reg::Param(Ireg::Reg(p)))
    }

    pub fn get_param_mut(&mut self, p: i8) -> &mut Val
    {
        self.e.get_reg_mut(&Reg::Param(Ireg::Reg(p)))
    }
}

/*
impl Debug for Frame
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "Frame({:?}, {:?}, {})",
            self.parent, self.e, self.pc,
        )
    }
}

pub struct IOQueue {
    waiters: LinkedList<Frame>,
}

pub struct IoWorker
{
    queue: LinkedList<Box<Iop>>,
    files: HashMap<u32, File>,
    id: u16,
}
*/

#[derive(Debug)]
pub enum Event
{
    Uneventful,
    Call(Reg, Code, Frame),
    Fork,
    FutureWait(Reg),
    IOWait,
    Complete,
    Failed,
}

pub struct Worker
{
    fresh: LinkedList<(Code, Frame)>,
    futures: LinkedList<(Reg, Code, Frame)>,
    event: Event,
    pub result: Val,
    //io: IOQueue,
    app: Arc<Mutex<Application>>,
    code: CodeMap,
    done: bool,
}

pub struct Application
{
    new_frames: LinkedList<(CodeKey, Frame)>,
    main_frame: Option<Frame>,
    code: CodeMap,
    //done: Arc<(Mutex<bool>, Condvar)>,
    done: AtomicBool,
    // modules
}

impl Application
{
    pub fn new() -> Application
    {
        Application{
            new_frames: LinkedList::new(),
            main_frame: None,
            code: HashMap::new(),
            //done: Arc::new((Mutex::new(false), Condvar::new())),
            done: AtomicBool::new(false),
        }
    }

    pub fn push_new_frame(&mut self, fname: &CodeKey, f: Frame) -> () {
        self.new_frames.push_back((fname.clone(), f))
    }

    pub fn pop_new_frame(&mut self) -> Option<(CodeKey, Frame)> {
        self.new_frames.pop_front()
    }

    pub fn set_main_frame(&mut self, f: Frame) -> () {
        if self.main_frame.is_some() {
            panic!("main_frame is already set");
        }
        self.main_frame = Some(f)
    }

    pub fn wait_until_done(app: &Arc<Mutex<Application>>) -> Option<Frame>
    {
        let mut result = None;
        let mut done = false;
        while !done {
            {
//write!(stderr(), "wait_until_done try_lock\n");
                let mut lock_result = (*app).try_lock();
                if !lock_result.is_err() {
//write!(stderr(), "wait_until_done locked\n");
                    let mut _app = lock_result.unwrap();
                    done = _app.done.load(Ordering::Relaxed);
                    if done {
                        result = _app.take_main_frame();
                    }
                }
            }
//write!(stderr(), "wait_until_done lock released?\n");
            thread::yield_now();
            //thread::sleep(time::Duration::new(1, 0));
        }
        /*
        let &(ref lock, ref cond) = &*self.done;
        let guard = lock.lock().unwrap();
        cond.wait(guard);
        while !self.done.load(Ordering::Relaxed) {
            //println!("still not done");
        }
        */
        result
    }

    pub fn take_main_frame(&mut self) -> Option<Frame> {
        self.main_frame.take()
    }

    pub fn add_app_code(&mut self, ss: &StaticSpace)
    {
        for (name, code) in &ss.lib {
            self.code.insert(name.clone(), code.clone());
        }
        for (name, inter) in &ss.interlib {
            let ops = code::make_ops(inter);
            self.code.insert((*name).clone(), Code::Leema(Arc::new(ops)));
        }
    }

    pub fn add_code(&mut self, key: CodeKey, c: Code) -> Code
    {
        self.code.insert(key.clone(), c);
        self.find_code(&key).unwrap()
    }

    pub fn find_code(&self, key: &CodeKey) -> Option<Code>
    {
        let c = self.code.get(&key);
        if c.is_none() {
            None
        } else {
            Some(c.unwrap().clone())
        }
    }
}

fn execute_const_val(curf: &mut Frame, reg: &Reg, v: &Val)
{
verbose_out!("execute_const_val({:?}, {:?})\n", reg, v);
    curf.e.set_reg(reg, v.clone());
verbose_out!("e: {:?}\n", curf.e);
    curf.pc = curf.pc + 1;
}

fn execute_constructor(curf: &mut Frame, reg: &Reg, typ: &Type)
{
verbose_out!("execute_constructor({:?}, {:?})\n", reg, typ);
    if let &Type::Struct(_, nfields) = typ {
        let mut fields = Vec::with_capacity(nfields as usize);
        fields.resize(nfields as usize, Val::Void);
        curf.e.set_reg(reg, Val::Struct(typ.clone(), fields));
        curf.pc = curf.pc + 1;
    } else {
        panic!("Cannot construct not structure: {:?}", typ);
    }
}

fn execute_copy(curf: &mut Frame, dst: &Reg, src: &Reg) {
    let src_val = curf.e.get_reg(src).clone();
    curf.e.set_reg(dst, src_val);
    curf.pc = curf.pc + 1;
}

/**
 * fork the frame and frame state
 * add it to the fresh queue
 * jump current frame state past the fork block
 */
fn execute_fork(w: &mut Worker, curf: &mut Frame,
    dst: &Reg, freg: &Reg, argreg: &Reg
) {
    println!("execute_fork");

    let fkey = {
        let ref fname_val = curf.e.get_reg(freg);
        match *fname_val {
            &Val::Str(ref name_str) => {
                // load code
                let ck = CodeKey::Name(name_str.clone());
                let code = w.find_code(&ck);
                if code.is_none() {
                    panic!("Can't find function {}()",
                        name_str,
                    );
                }
                ck
            }
            _ => {
                panic!("That's not a function name! {:?}",
                    fname_val,
                );
            }
        }
    };

    // args are empty for a fork
    // create new frame
    let e = Env::new();
    // set current state to called
    let (tx, rx) = mpsc::channel::<Val>();
    let ready = Arc::new(AtomicBool::new(false));
    let newf = Frame{
        parent: Parent::Fork(ready.clone(), tx),
        e: curf.e.clone(),
        pc: 0,
    };
    curf.e.set_reg(dst, Val::future(ready, rx));
    w.add_fork(&fkey, newf);

    curf.pc = curf.pc + 1;
    w.event = Event::Fork;
}

fn execute_jump(curf: &mut Frame, jmp: i16)
{
    curf.pc += jmp as i32;
}

fn execute_jump_if_not(curf: &mut Frame, jmp: i16, reg: &Reg)
{
verbose_out!("execute_jump_if_not({:?},{:?})\n", jmp, reg);
    let test_val = curf.e.get_reg(reg);
    if let &Val::Bool(test) = test_val {
        if test {
            verbose_out!("if test is true");
            curf.pc += 1;
        } else {
            verbose_out!("if test is false");
            curf.pc += jmp as i32;
        }
    } else {
        panic!("can't if check a not bool {:?}", test_val);
    }
}

fn execute_match_pattern(curf: &mut Frame, jmp: i16, patt: &Reg, input: &Reg)
{
    verbose_out!("execute_match_pattern({}, {:?}, {:?})\n", jmp, patt, input);
    let e: &mut Env = &mut curf.e;
    let matches = {
        let pval = e.get_reg(&patt);
        let ival = e.get_reg(&input);
verbose_out!("match input: {:?}={:?}\n", pval, ival);
        Val::pattern_match(pval, ival)
    };
verbose_out!("matches: {:?}\n", matches);
    match matches {
        Some(assignments) => {
            for a in assignments {
                let (dst, v) = a;
                e.set_reg(&dst, v);
            }
            curf.pc += 1;
        }
        Nothing => curf.pc += jmp as i32,
    }
}

fn execute_list_cons(curf: &mut Frame, dst: &Reg, src_reg: &Reg)
{
    let src;
    {
        src = curf.e.get_reg(&src_reg).clone();
    }
    curf.e.set_reg(&dst, list::cons(Val::Void, src));
    curf.pc += 1;
}

fn execute_list_create(curf: &mut Frame, dst: &Reg) {
    curf.e.set_reg(&dst, list::empty());
    curf.pc = curf.pc + 1;
}

fn execute_strcat(w: &mut Worker, curf: &mut Frame, dstreg: &Reg, srcreg: &Reg)
{
    let result;
    {
        let src = curf.e.get_reg(srcreg);
        if src.is_future() {
            // oops, not ready to do this yet, let's bail and wait
            w.event = Event::FutureWait(srcreg.clone());
            return;
        }
        let dst = curf.e.get_reg(dstreg);
        result = Val::Str(Arc::new(format!("{}{}", dst, src)));
    }
    curf.pc = curf.pc + 1;
    curf.e.set_reg(dstreg, result);
}

fn execute_tuple_create(curf: &mut Frame, dst: &Reg, ref sz: i8)
{
    verbose_out!("execute_tuple_create({:?}, {})\n", dst, sz);
    let tupsize: usize = *sz as usize;
    curf.e.set_reg(dst, Val::new_tuple(tupsize));
    curf.pc = curf.pc + 1;
}

fn execute_load_func(curf: &mut Frame, dst: &Reg, ms: &ModSym)
{
    curf.pc = curf.pc + 1;
}

/**
 * get code from func
 * make an Env from the args
 * make a new frame state
 * create a new frame w/ func code and new frame state
 * set curf.flag to Called(new_frame)
 */
fn execute_func_apply(w: &mut Worker, curf: &mut Frame, dst: &Reg, freg: &Reg, argreg: &Reg)
{
    let ref fname_val = curf.e.get_reg(freg);
    match *fname_val {
        &Val::Str(ref name_str) => {
            curf.pc = curf.pc + 1;
            // load code
            let ck = CodeKey::Name(name_str.clone());
            let code = w.find_code(&ck).unwrap().clone();
            // pass in args
            let args = curf.e.get_reg(argreg);
            // create new frame
            let e = Env::with_args(args.clone());
            // set current state to called
            w.event = Event::Call(
                dst.clone(),
                code.clone(),
                Frame::new(Parent::Null, e),
            );
        }
        _ => {
            panic!("That's not a function! {:?}", fname_val);
        }
    }
}


/**
 * main_loop
 *   get fresh/active frame
 *     iterate until !active
 *   push frame
 *   rotate
 */
impl Worker
{
    pub fn new(app: Arc<Mutex<Application>>) -> Worker
    {
        let done = {
verbose_out!("lock app, new worker\n");
            let _app = app.lock().unwrap();
            //_app.done.clone()
        };
        Worker {
            fresh: LinkedList::new(),
            futures: LinkedList::new(),
            event: Event::Uneventful,
            result: Val::Int(0),
            app: app,
            code: HashMap::new(),
            done: false,
        }
    }

    /*
    pub fn push(&mut self, f: Frame) -> () {
        println!("huh, this is called after all");
        self.fresh.push_front(f);
    }
    */

    pub fn find_code(&mut self, name: &CodeKey) -> Option<&Code>
    {
        if self.code.contains_key(name) {
            let mut c: Option<&Code> = self.code.get(name);
            return c;
        }

        let ac = {
verbose_out!("lock app, find_code\n");
            let app = self.app.lock().unwrap();
            //let app = self.app.lock().unwrap() as MutexGuard<'a, Application>;
            //let app = Deref::deref(&self.app.lock().unwrap());
            match app.find_code(name) {
                Some(appc) => {
                    appc.clone()
                }
                None => {
                    panic!("shit, no code for {:?}", name);
                }
            }
        };
        self.code.insert(name.clone(), ac);
        self.code.get(name)
    }

    pub fn execute(&mut self, curf: &mut Frame, ops: &OpVec)
    {
        let op = ops.get(curf.pc as usize).unwrap();
        // println!("exec: {:?}", op);
        match op {
            &Op::ConstVal(ref dst, ref v) => {
                execute_const_val(curf, dst, v);
            }
            &Op::Constructor(ref dst, ref typ) => {
                execute_constructor(curf, dst, typ);
            }
            &Op::Copy(ref dst, ref src) => {
                execute_copy(curf, dst, src);
            }
            &Op::Fork(ref dst, ref freg, ref args) => {
                execute_fork(self, curf, dst, freg, args);
            }
            &Op::Jump(jmp) => {
                execute_jump(curf, jmp);
            }
            &Op::JumpIfNot(jmp, ref reg) => {
                execute_jump_if_not(curf, jmp, reg);
            }
            &Op::MatchPattern(jmp, ref patt, ref input) => {
                execute_match_pattern(curf, jmp, patt, input);
            }
            &Op::ListCons(ref dst, ref src) => {
                execute_list_cons(curf, dst, src);
            }
            &Op::ListCreate(ref dst) => {
                execute_list_create(curf, dst);
            }
            &Op::TupleCreate(ref dst, ref sz) => {
                execute_tuple_create(curf, dst, *sz);
            }
            &Op::StrCat(ref dst, ref src) => {
                execute_strcat(self, curf, dst, src);
            }
            &Op::LoadFunc(ref reg, ref modsym) => {
                execute_load_func(curf, reg, modsym);
            }
            &Op::ApplyFunc(ref dst, ref func, ref args) => {
                execute_func_apply(self, curf, dst, func, args);
            }
            &Op::Return => {
                self.event = Event::Complete;
            }
        }
    }

    fn add_fork(&mut self, key: &CodeKey, newf: Frame)
    {
verbose_out!("lock app, add_fork\n");
        let mut a = self.app.lock().unwrap();
        a.push_new_frame(key, newf);
    }

    fn iterate_leema(&mut self, curf: &mut Frame, ops: &OpVec)
    {
        while let Event::Uneventful = self.event {
            self.execute(curf, ops);
        }
    }

    pub fn take_event(&mut self) -> Event
    {
        let mut e = Event::Uneventful;
        mem::swap(&mut e, &mut self.event);
        e
    }

    pub fn iterate(&mut self, code: Code, mut curf: Frame)
    {
verbose_out!("iterate\n");
        match code {
            Code::Leema(ref ops) => {
                self.iterate_leema(&mut curf, ops);
            }
            Code::Rust(ref rf) => {
                rf(&mut curf);
                self.event = Event::Complete;
            }
            Code::Inter(ref ix) => {
                panic!("cannot execute partial code");
            }
        }
        match self.take_event() {
            Event::Complete => {
                match curf.parent {
                    Parent::Caller(dst, code, mut pf) => {
                        pf.e.set_reg(
                            &dst,
                            curf.e.takeResult(),
                        );
                        self.fresh.push_back((code, *pf));
                    }
                    Parent::Repl => {
verbose_out!("lock app, repl done in iterate\n");
                        let mut _app = self.app.lock().unwrap();
                        _app.set_main_frame(curf);
                    }
                    Parent::Main => {
verbose_out!("finished main func");
                        {
verbose_out!("lock app, main done in iterate\n");
                            let mut _app = self.app.lock().unwrap();
                            _app.set_main_frame(curf);
                            _app.done.store(true, Ordering::Relaxed);
                            self.done = true;
                        }
                        //self.notify_done();
                    }
                    Parent::Fork(mut ready, mut tx) => {
                        println!("finished a fork!");
                        /*
                        // still need to pass the
                        // result from the fork
                        // to the parent
                        */
                        let r = curf.e.takeResult();
                        println!("send({:?})", r);
                        tx.send(r);
                        ready.store(
                            true,
                            Ordering::Relaxed,
                        );
                    }
                    Parent::Null => {
                        // this shouldn't have happened
                    }
                }
            }
            Event::Call(dst, ch_code, mut ch_frame) => {
                ch_frame.parent = Parent::Caller(
                    dst,
                    code,
                    Box::new(curf),
                );
                self.fresh.push_back((ch_code, ch_frame));
            }
            Event::FutureWait(reg) => {
                println!("wait for future {:?}", reg);
                self.futures.push_back((reg, code, curf));
            }
            Event::IOWait => {
                println!("do I/O");
            }
            Event::Fork => {
                self.fresh.push_back((code, curf));
                // end this iteration,
            }
            Event::Failed => {
                panic!("Frame failed");
            }
            Event::Uneventful => {
                panic!("We shouldn't be here with uneventful");
            }
        }
    }

    pub fn rotate(&mut self) -> Option<(Code, Frame)>
    {
verbose_out!("rotate\n");
        self.check_futures();
//println!("worker.app.try_lock()");
        let lock_result = self.app.try_lock();
        if lock_result.is_err() {
verbose_out!("rotate try_lock is_err\n");
            return None;
        }
//println!("lock_result.unwrap()");
        let mut app = lock_result.unwrap();
        match app.pop_new_frame() {
            Some((codekey, new_frame)) => {
                let new_code = app.find_code(&codekey);
                if new_code.is_none() {
                    panic!("can't find new code");
                }
                self.fresh.push_front((
                    new_code.unwrap(),
                    new_frame,
                    ));
            }
            None => {
                // nothing
            }
        }

        if self.fresh.is_empty() {
            return None;
        }
        self.fresh.pop_front()
    }

    pub fn check_futures(&mut self)
    {
        let mut newfutures = LinkedList::new();
        loop {
            let mut ff = self.futures.pop_front();
            if ff.is_none() {
                break;
            }
            match ff.unwrap() {
                (reg, code, mut frame) => {
                    let result = frame.receive_future(&reg);
                    if result.is_some() {
                        verbose_out!("found future ready {:?}\n", result);
                        frame.e.set_reg(&reg, result.unwrap());
                        self.fresh.push_back((code, frame));
                    } else {
                        verbose_out!("future not ready {:?}\n", reg);
                        newfutures.push_back((reg, code, frame));
                    }
                }
            }
        }
    }

    /*
    fn notify_done(&mut self)
    {
        let &(_, ref cond) = &*self.done;
        cond.notify_one();
    }
    */

    pub fn gotowork(&mut self) -> ()
    {
verbose_out!("local gotowork\n");
        while !self.done {
verbose_out!("worker not done\n");
            match self.rotate() {
                None => {
                    thread::yield_now();
                },
                Some((code, curf)) => {
                    self.iterate(code, curf);
                }
            }
        }
    }
}

/*
process_set
process
|  \- base frames
|      |       \- call code
 \--- fork
*/


#[cfg(test)]
mod tests {
    use leema::log;
    use leema::frame::{Application, Frame, Parent, Worker};
    use leema::ast::{Ast};
    use leema::code::{CodeKey};
    use leema::reg::{Reg};
    use leema::val::{Env, Val};
    use leema::prefab;
    use leema::lex::{lex};
    use std::thread;
    use std::sync::{Arc, Mutex};
    use std::io::{stderr, Write};
use libc::{getpid};


#[test]
fn test_main_func_finishes()
{
let p = unsafe { getpid(); };
write!(stderr(), "test_main_func_finishes {:?}\n", p);
    let input = "func main() -> 3 --".to_string();
    let mut ss = prefab::new_staticspace();
    ss.compile(Ast::parse(lex(input)).root());

    let mut app = Application::new();
write!(stderr(), "app.add_app_code\n");
    app.add_app_code(&ss);

    if ss.has_main() {
        let frm = Frame::new(Parent::Main, Env::new());
        app.push_new_frame(&CodeKey::Main, frm);
    }

    let app0 = Arc::new(Mutex::new(app));
    let app1 = app0.clone();
    thread::spawn(move || {
        let mut w0 = Worker::new(app0);
        verbose_out!("w0.gotowork\n");
        w0.gotowork();
    });

write!(stderr(), "Application::wait_until_done\n");
    let result_frame = Application::wait_until_done(&app1);
    assert!(result_frame.is_some());
    assert_eq!(&Val::Int(3), result_frame.unwrap().e.get_reg(&Reg::Result));
}

}
