use leema::application::{Application, AppCaller};
use leema::lstr::Lstr;
use leema::val::Val;

use futures::Future;
use futures::future;
use futures::sync::oneshot::Canceled;
use hyper::{Body, Request, Response, Server};
use hyper::service::service_fn;


type HttpFut = Future<Item = Response<Body>, Error = Canceled>;
type BoxFut = Box<Future<Item = Response<Body>, Error = Canceled> + Send>;

pub fn run(app: &mut Application) -> Val
{
    let addr = ([0, 0, 0, 0], 3000).into();
    let caller = app.caller();

    let new_svc = move || {
        let icaller = caller.clone();
        service_fn(move |req| {
            handle_request(icaller.clone(), req)
        })
    };

    let server = Server::bind(&addr)
        .serve(new_svc)
        .map_err(|e| eprintln!("server error: {}", e));

    let http_result = ::hyper::rt::run(server);
    println!("hyper finished with: {:?}", http_result);

    app.wait_for_result().unwrap()
}

pub fn handle_request(caller: AppCaller, req: Request<Body>) -> BoxFut
{
    println!("handle_request({:?})", req);
    let response_future: BoxFut = Box::new(
        caller.push_call(
            &Lstr::Sref("http"), &Lstr::Sref("test_handle")
        )
        .and_then(|v| {
            let msg = format!("hello world: {}\n", v);
            // future::ok(Response::new(Body::from(msg)))
            future::ok(Response::new(Body::from(msg)))
        })
    );
    response_future
}

struct LeemaService
{
    app: AppCaller,
}

/*
impl hyper::service::Service for LeemaService
{
    type ReqBody = Body;
    type ResBody = Body;
    type Error = hyper::Error;
    type Future = Future<Item=Response<Body>, Error=hyper::Error>;

    fn call(&mut self, req: Request<Body>) -> Self::Future
    {
        future::ok(Response::new(Body::from("taco service\n")))
    }
}
*/
