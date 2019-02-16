use leema::code::Code;
use leema::frame::FrameTrace;
use leema::lri::Lri;
use leema::lstr::Lstr;
use leema::rsrc::{self, Rsrc, RunQueue};
use leema::struple::Struple;
use leema::val::{Type, Val};

use std::fmt;
use std::net::{IpAddr, SocketAddr};
use std::thread;
use std::time::Instant;

use futures::sync::oneshot as futures_oneshot;
use futures::{future, Future};
use hyper::client::connect::HttpInfo;
use hyper::rt::Stream;
use hyper::service::service_fn;
use hyper::{Body, Client, Method, Request, Response, Server, StatusCode, Uri};
use hyper_rustls::HttpsConnector;

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

const REQUEST_LRI: Lri = Lri {
    modules: Some(Lstr::Sref("hyper_server")),
    localid: Lstr::Sref("Request"),
    params: None,
};


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
        Val::FuncRef(ref lri, _, _) => lri.clone(),
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

    rsrc::Event::NewRsrc(handle)
}

pub fn server_run_on_thread(
    port: u16,
    func: Lri,
    runq: RunQueue,
    close_recv: futures_oneshot::Receiver<()>,
)
{
    println!("server_run({}, {})", port, func);
    let sock_addr = SocketAddr::new(IpAddr::from([0, 0, 0, 0]), port);

    let new_svc = move || {
        let irunq = runq.clone();
        let ifuncri = func.clone();
        service_fn(move |req| handle_request(ifuncri.clone(), req, &irunq))
    };

    let server = Server::bind(&sock_addr)
        .serve(new_svc)
        .with_graceful_shutdown(close_recv)
        .map_err(|e| {
            eprintln!("server error: {}", e);
        });
    ::hyper::rt::run(server);
}

pub fn new_leema_request(req: &Request<Body>) -> Val
{
    let method_str = match *req.method() {
        Method::GET => "GET",
        Method::POST => "POST",
        Method::PUT => "PUT",
        Method::DELETE => "DELETE",
        _ => {
            return Val::failure(
                Val::Hashtag(Lstr::Sref("unsupported_method")),
                Val::Str(Lstr::from(req.method().as_str().to_string())),
                FrameTrace::new_root(),
                0,
            );
        }
    };
    let leema_method = Val::Str(Lstr::Sref(method_str));
    let leema_path = Val::Str(Lstr::from(req.uri().path().to_string()));

    let fields = vec![
        (Some(Lstr::Sref("method")), leema_method),
        (Some(Lstr::Sref("path")), leema_path),
    ];
    Val::Struct(REQUEST_LRI, Struple(fields))
}

pub fn handle_request(
    func: Lri,
    req: Request<Body>,
    caller: &RunQueue,
) -> BoxFut
{
    vout!("handle_request({},\n\t{:?})", func, req);
    let leema_req = new_leema_request(&req);
    let args = Struple(vec![(Some(Lstr::Sref("req")), leema_req)]);
    Box::new(
        caller
            .spawn(func, args)
            .and_then(|v| {
                let msg = format!("{}", v);
                vout!("response msg: {}", msg);
                future::ok(Response::new(Body::from(msg)))
            })
            .or_else(|e| {
                println!("request error: {:?}", e);
                let resp: Response<Body> = Response::builder()
                    .status(StatusCode::INTERNAL_SERVER_ERROR)
                    .body(Body::from("server error\n"))
                    .unwrap();
                future::ok(resp)
            }),
    )
}

pub fn load_rust_func(func_name: &str) -> Option<Code>
{
    match func_name {
        "run" => Some(Code::Iop(server_run, None)),
        _ => None,
    }
}


const CLIENT_RESP_TYPE: Lri = Lri {
    modules: Some(Lstr::Sref("hyper_client")),
    localid: Lstr::Sref("Response"),
    params: None,
};

pub fn new_client_response(code: i64, body: String) -> Val
{
    let fields = Struple::new_indexed(vec![
        Val::Int(code),
        Val::Str(Lstr::from(body)),
    ]);
    Val::Struct(CLIENT_RESP_TYPE, fields)
}

pub fn client_get(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    let url = {
        match ctx.take_param(0).unwrap() {
            Val::Str(url_str) => url_str.parse::<Uri>().unwrap(),
            not_str => {
                /*
                return rsrc::Event::Result(Val::failure(
                    Val::Hashtag(Lstr::from("type_error")),
                    Val::Str(Lstr::from(format!(
                        "url parameter is not a string: {}", not_str
                    ))),
                    None,
                ));
                */
                panic!("url parameter is not a string: {}", not_str);
            }
        }
    };
    let client = Client::new();
    let get_fut = client
        .get(url)
        .and_then(|res| Stream::collect(res.into_body()))
        .map(|chunks| {
            let text_vec: Vec<String> = chunks
                .into_iter()
                .map(|chunk| {
                    String::from_utf8(chunk.into_bytes().as_ref().to_vec())
                        .unwrap()
                })
                .collect();
            let text: String = text_vec.concat();
            rsrc::Event::Result(Val::Str(Lstr::from(text)))
        })
        .map_err(|_err| {
            rsrc::Event::Result(Val::Str(Lstr::Sref("client_failure")))
        });
    rsrc::Event::Future(Box::new(get_fut))
}

pub fn client_post(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    let url = {
        match ctx.take_param(0).unwrap() {
            Val::Str(url_str) => url_str.parse::<Uri>().unwrap(),
            not_str => {
                /*
                return rsrc::Event::Result(Val::failure(
                    Val::Hashtag(Lstr::from("type_error")),
                    Val::Str(Lstr::from(format!(
                        "url parameter is not a string: {}", not_str
                    ))),
                    None,
                ));
                */
                panic!("url parameter is not a string: {}", not_str);
            }
        }
    };
    let reqbody: Lstr = match ctx.take_param(1).unwrap() {
        Val::Str(body) => body.clone(),
        not_str => {
            panic!("url body is not a string: {:?}", not_str);
        }
    };
    let req = Request::post(url)
        .body(Body::from(String::from(&reqbody)))
        .unwrap();
    let tls_conn = HttpsConnector::new(1);
    let client = Client::builder().build(tls_conn);
    let start_instant = Instant::now();
    let post_fut = client
        .request(req)
        .and_then(move |res| {
            let (parts, body) = res.into_parts();
            Stream::collect(body).map(move |chunks| {
                let req_time = start_instant.elapsed();
eprintln!("secs: {}, millis: {}, mics: {}, nanos: {}", req_time.as_secs(), req_time.subsec_millis(), req_time.subsec_micros(), req_time.subsec_nanos());
                (parts, chunks)
            })
        })
        .map(|resp| {
            let text_vec: Vec<String> = resp.1
                .into_iter()
                .map(|chunk| {
                    String::from_utf8(chunk.into_bytes().as_ref().to_vec())
                        .unwrap()
                })
                .collect();
            let parts = resp.0;
            let info = parts.extensions.get::<HttpInfo>();
eprintln!("\nextensions: {:?}", info);
            let text: String = text_vec.concat();
            let resp_val =
                new_client_response(parts.status.as_u16() as i64, text);
            rsrc::Event::Result(resp_val)
        })
        .map_err(|err| {
            eprintln!("http post error: {:?}", err);
            rsrc::Event::Result(Val::Str(Lstr::Sref("client_failure")))
        });
    rsrc::Event::Future(Box::new(post_fut))
}

pub fn load_client_func(func_name: &str) -> Option<Code>
{
    match func_name {
        "get" => Some(Code::Iop(client_get, None)),
        "post" => Some(Code::Iop(client_post, None)),
        _ => None,
    }
}
