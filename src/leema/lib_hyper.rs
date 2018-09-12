
use leema::code::Code;
use leema::log;
use leema::lri::Lri;
use leema::lstr::Lstr;
use leema::rsrc::{self, Rsrc, RunQueue};
use leema::val::{Val, Type};

use std::fmt;
use std::io::Write;
use std::net::{IpAddr, SocketAddr};

use futures::{future, Future};
use futures::sync::oneshot as futures_oneshot;
use hyper::{Body, Request, Response, Server};
use hyper::service::service_fn;

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


type BoxFut = Box<Future<Item = Response<Body>, Error = futures_oneshot::Canceled> + Send>;

struct ServerHandle
{
    server: Option<BoxFut>,
    closer: futures_oneshot::Sender<()>,
}

impl ServerHandle
{
    pub fn new(server: BoxFut, closer: futures_oneshot::Sender<()>) -> Box<ServerHandle>
    {
        Box::new(ServerHandle { server: Some(server), closer })
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
        let server_str = if self.server.is_some() {
            "server"
        } else {
            "empty"
        };
        write!(f, "ServerHandle({})", server_str)
    }
}

pub fn server_bind(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    vout!("hyper_server::bind()\n");
    let sock_addr = {
        let port = ctx.take_param(0).unwrap().to_int() as u16;
        SocketAddr::new(IpAddr::from([0, 0, 0, 0]), port)
    };
    let func = ctx.take_param(1).unwrap();
    let funcri = match func {
        Val::FuncRef(ref lri, _) => lri.clone(),
        _ => {
            panic!("cannot bind server to a not function: {:?}", func);
        }
    };

    let app = ctx.clone_run_queue();
    let new_svc = move || {
        let iapp = app.clone();
        service_fn(move |req| handle_request(funcri, req, iapp.clone()))
    };

    let (closer, close_recv_1) = futures_oneshot::channel();
    let close_recv = close_recv_1.map_err(|e| {
    });

    let server = Server::bind(&sock_addr)
        .serve(new_svc)
        .with_graceful_shutdown(close_recv)
        .map_err(|e| {
            eprintln!("server error: {}", e);
        });
    let handle = ServerHandle::new(Box::new(server), closer);
    rsrc::Event::NewRsrc(handle, None)
}

pub fn server_run(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    let server_handle: ServerHandle = ctx.take_rsrc();
    let server = server_handle.server.take().unwrap();
    let http_result = ::hyper::rt::spawn(server);
println!("hyper finished with: {:?}", http_result);
    rsrc::Event::Result(Val::Int(0), Some(Box::new(server_handle)))
}

pub fn handle_request(
    func: Lri,
    req: Request<Body>,
    caller: RunQueue,
) -> BoxFut
{
    println!("handle_request({},\n\t{:?})", func, req);
    let response_future: BoxFut = Box::new(
        caller
            .spawn(func)
            .and_then(|v| {
                let msg = format!("{}", v);
                println!("response msg: {}", msg);
                // future::ok(Response::new(Body::from(msg)))
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
        "bind" => Some(Code::Iop(server_bind, None)),
        "run" => Some(Code::Iop(server_run, Some(0))),
        _ => None,
    }
}
