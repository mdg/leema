use crate::leema::code::Code;
use crate::leema::frame::FrameTrace;
use crate::leema::lstr::Lstr;
use crate::leema::rsrc::{self, Rsrc, RunQueue};
use crate::leema::struple::StrupleItem;
use crate::leema::val::{Fref, Type, Val};

use std::fmt;
use std::net::{IpAddr, SocketAddr};
use std::thread;
use std::time::{Duration, Instant};

use futures::sync::oneshot as futures_oneshot;
use futures::{future, Future};
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

const REQUEST_TYPE: Type = user_type!("/hyper_server/Request");
const SERVER_HANDLE_TYPE: Type = user_type!("/hyper_server/ServerHandle");

type BoxFut = Box<
    dyn Future<Item = Response<Body>, Error = futures_oneshot::Canceled> + Send,
>;
type Graceful = Box<dyn Future<Item = (), Error = ()> + Send>;

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
        SERVER_HANDLE_TYPE
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
    let call = ctx.take_param(1).unwrap();
    let fref = match call {
        Val::Call(ref f, _) => f.clone(),
        _ => {
            panic!("cannot bind server to a not function: {:?}", call);
        }
    };

    let runq = ctx.clone_run_queue();
    let (closer, close_recv) = futures_oneshot::channel::<()>();
    let handle = ServerHandle::new(closer);

    thread::spawn(move || {
        server_run_on_thread(port, fref, runq, close_recv);
    });

    rsrc::Event::NewRsrc(handle)
}

pub fn server_run_on_thread(
    port: u16,
    func: Fref,
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
        StrupleItem::new(Some(Lstr::Sref("method")), leema_method),
        StrupleItem::new(Some(Lstr::Sref("path")), leema_path),
    ];
    Val::Struct(REQUEST_TYPE, fields)
}

pub fn handle_request(
    func: Fref,
    req: Request<Body>,
    caller: &RunQueue,
) -> BoxFut
{
    vout!("handle_request({},\n\t{:?})", func, req);
    let leema_req = new_leema_request(&req);
    let args = vec![StrupleItem::new(Some(Lstr::Sref("req")), leema_req)];
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


const CLIENT_RESP_TYPE: Type = user_type!("/hyper_client/Response");

pub fn new_client_response(code: i64, req_time: Duration, body: String) -> Val
{
    let sec_mics: i64 = req_time.as_secs() as i64 * 1000000;
    let sub_mics: i64 = req_time.subsec_micros() as i64;
    let mics = sec_mics + sub_mics;

    let fields = vec![
        StrupleItem::new(None, Val::Int(code)),
        StrupleItem::new(None, Val::Str(Lstr::from(body))),
        StrupleItem::new(None, Val::Int(mics)),
    ];
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
                (parts, req_time, chunks)
            })
        })
        .map(|resp| {
            let text_vec: Vec<String> = resp
                .2
                .into_iter()
                .map(|chunk| {
                    String::from_utf8(chunk.into_bytes().as_ref().to_vec())
                        .unwrap()
                })
                .collect();
            let parts = resp.0;
            // use hyper::client::connect::HttpInfo;
            // let info = parts.extensions.get::<HttpInfo>().unwrap();
            // let remote_addr = format!("{}", info.remote_addr());
            // println!("response from: {}", remote_addr);
            let text: String = text_vec.concat();
            let resp_val =
                new_client_response(parts.status.as_u16() as i64, resp.1, text);
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
