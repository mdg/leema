use leema::code::Code;
use leema::log;
use leema::lri::Lri;
use leema::lstr::Lstr;
use leema::rsrc::{self, Rsrc, RunQueue};
use leema::val::{Type, Val};

use std::fmt;
use std::io::Write;
use std::net::{IpAddr, SocketAddr};
use std::thread;

use futures::sync::oneshot as futures_oneshot;
use futures::{future, Future};
use hyper::service::service_fn;
use hyper::{Body, Request, Response, Server};

/*
http handle
.closer
--

let sh := hyper_server::bind(function)
- create closer

hyper_server::run(s)
- run
- create thread

hyper_server::close(s)
*/


type BoxFut = Box<
    Future<Item = Response<Body>, Error = futures_oneshot::Canceled> + Send,
>;
type Graceful = Box<Future<Item = (), Error = ()> + Send>;

struct ServerHandle
{
    closer: futures_oneshot::Sender<()>,
}

impl ServerHandle
{
    pub fn new(closer: futures_oneshot::Sender<()>) -> Box<ServerHandle>
    {
        Box::new(ServerHandle { closer })
    }
}

impl Rsrc for ServerHandle
{
    fn get_type(&self) -> Type
    {
        Type::Resource(Lstr::Sref("ServerHandle"))
    }
}

impl fmt::Debug for ServerHandle
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "ServerHandle")
    }
}

pub fn server_run(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    vout!("hyper_server::bind()\n");
    let port = ctx.take_param(0).unwrap().to_int() as u16;
    let func = ctx.take_param(1).unwrap();
    let funcri = match func {
        Val::FuncRef(ref lri, _) => lri.clone(),
        _ => {
            panic!("cannot bind server to a not function: {:?}", func);
        }
    };

    let runq = ctx.clone_run_queue();
    let (closer, close_recv) = futures_oneshot::channel::<()>();
    let handle = ServerHandle::new(closer);

    thread::spawn(move || {
        server_run_on_thread(port, funcri, runq, close_recv);
    });

    rsrc::Event::NewRsrc(handle, None)
}

pub fn server_run_on_thread(
    port: u16,
    func: Lri,
    runq: RunQueue,
    close_recv: futures_oneshot::Receiver<()>,
)
{
    println!("server_run_on_thread({}, {})", port, func);
    let sock_addr = SocketAddr::new(IpAddr::from([0, 0, 0, 0]), port);

    let new_svc = move || {
        let irunq = runq.clone();
        let ifuncri = func.clone();
        service_fn(move |req| {
            handle_request(ifuncri.clone(), req, irunq.clone())
        })
    };

    let server = Server::bind(&sock_addr)
        .serve(new_svc)
        .with_graceful_shutdown(close_recv)
        .map_err(|e| {
            eprintln!("server error: {}", e);
        });
    ::hyper::rt::run(server);
}

pub fn handle_request(func: Lri, req: Request<Body>, caller: RunQueue)
    -> BoxFut
{
    vout!("handle_request({},\n\t{:?})", func, req);
    let response_future = Box::new(
        caller
            .spawn(func)
            .and_then(|v| {
                let msg = format!("{}", v);
                vout!("response msg: {}", msg);
                future::ok(Response::new(Body::from(msg)))
            }).map_err(|e| {
                println!("request error: {:?}", e);
                e
            }),
    );
    response_future
}

pub fn load_rust_func(func_name: &str) -> Option<Code>
{
    match func_name {
        "run" => Some(Code::Iop(server_run, None)),
        _ => None,
    }
}
