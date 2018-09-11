use leema::application::Application;
use leema::val::Val;

use futures::future;
use hyper::rt::Future;
use hyper::service::service_fn;
use hyper::{self, Body, Request, Response, Server};


type BoxFut = Box<Future<Item = Response<Body>, Error = hyper::Error> + Send>;

pub fn run(app: &mut Application) -> Val
{
    let addr = ([0, 0, 0, 0], 3000).into();
    // let caller = app.caller();

    let new_svc = || service_fn(|req| handle_request(req));

    let server = Server::bind(&addr)
        .serve(new_svc)
        .map_err(|e| eprintln!("server error: {}", e));

    let http_result = ::hyper::rt::run(server);
    println!("hyper finished with: {:?}", http_result);

    app.wait_for_result().unwrap()
}

pub fn handle_request(/*caller: AppCaller,*/ _req: Request<Body>) -> BoxFut
{
    /*
    println!("handle_request({:?})", req);
    let result_receiver = caller.push_call("http", "test_handle");
    result_receiver.map(|v| {
        let msg = format!("hello world: {}\n", v);
        println!("{}", msg);
        Box::new(Response::new(Body::from(msg)))
    })
    */
    Box::new(future::ok(Response::new(Body::from("tacos"))))
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
