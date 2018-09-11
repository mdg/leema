use leema::application::{Application, AppCaller};
// use leema::lstr::Lstr;
use leema::val::Val;

use futures::future;
use hyper::rt::Future;
use hyper::service::service_fn;
use hyper::{self, Body, Request, Response, Server};


type HttpFut = Future<Item = Response<Body>, Error = hyper::Error>;
type BoxFut = Box<Future<Item = Response<Body>, Error = hyper::Error> + Send>;

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

pub fn handle_request(_caller: AppCaller, req: Request<Body>) -> BoxFut
{
    println!("handle_request({:?})", req);
    /*
    let response_future: BoxFut = Box::new(
        caller.push_call(
            &Lstr::Sref("http"), &Lstr::Sref("test_handle")
        )
        .and_then(|v| {
            let msg = format!("hello world: {}\n", v);
            println!("{}", msg);
            // future::ok(Response::new(Body::from(msg)))
            future::ok(Response::new(Body::from(msg)))
        }).into_future()
    );
    */
    Box::new(future::ok(Response::new(Body::from("tacos\n"))))
}

/*
struct LeemaService
{
    app: AppCaller,
}

impl Service for LeemaService
{
}
*/
