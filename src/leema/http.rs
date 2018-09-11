use leema::application::Application;
use leema::val::Val;

use hyper::rt::Future;
use hyper::service::service_fn_ok;
use hyper::{Body, Request, Response, Server};


pub fn run(app: &mut Application) -> Val
{
    let addr = ([0, 0, 0, 0], 3000).into();

    let new_svc = || service_fn_ok(handle_request);

    let server = Server::bind(&addr)
        .serve(new_svc)
        .map_err(|e| eprintln!("server error: {}", e));

    let _http_result = ::hyper::rt::run(server);

    app.wait_for_result().unwrap()
}

pub fn handle_request(req: Request<Body>) -> Response<Body>
{
    println!("handle_request({:?})", req);
    Response::new(Body::from("hello world\n"))
}
