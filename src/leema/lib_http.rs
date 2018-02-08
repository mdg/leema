use leema::code::{Code};
use leema::log;
use leema::rsrc::{self, Rsrc};
use leema::val::{Val, Type, LibVal};

use std;
use std::net::{IpAddr, SocketAddr};
use std::rc::{Rc};
use std::str::{FromStr};
use std::sync::{Arc};
use std::io::{self, Write};
use bytes::{BytesMut};
use bytes::buf::{BufMut};

use ::tokio_core::reactor::{Handle};
use ::tokio_io::{AsyncRead};
use futures::{Async, Poll};
use futures::future::{self, Future};
use futures::stream::Stream;
use futures::task;
use hyper;
use hyper::server::{Http, Request, Response, Service};


#[derive(Debug)]
struct LeemaHttp;

impl LibVal for Response
{
    fn get_type(&self) -> Type
    {
        Type::Lib("HttpResponse".to_string())
    }
}

impl Service for LeemaHttp
{
    type Request = Request;
    type Response = Response;
    type Error = hyper::Error;
    // type Response = rsrc::Event;
    // type Error = rsrc::Event;
    type Future = Box<future::Future<Item=Self::Response, Error=Self::Error>>;

    fn call(&self, req: Self::Request) -> Self::Future
    {
        let resp = Response::new()
                .with_body("tacos");
        // .map(|resp| {
        //     rsrc::Event::Success(Val::Lib(Arc::new(resp)), None)
        // });
        Box::new(future::ok(resp))
    }
}

pub fn http_bind(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    vout!("http_bind()\n");
    let sock_addr = {
        let sock_addr_str = ctx.take_param(0).unwrap();
        let port = ctx.take_param(1).unwrap().to_int() as u16;
        SocketAddr::new(
            IpAddr::from_str((sock_addr_str.str())).unwrap(), port
        )
    };

    let handle = ctx.handle().clone();
    let new_service = || {
        Ok(LeemaHttp)
    };
    let serve = Http::new()
        .serve_addr_handle(&sock_addr, &handle, new_service)
        .unwrap()
        .map(|_| {
            vout!("incoming http connection, maybe()\n");
            rsrc::Event::Success(Val::Void, None)
        })
        .map_err(|_| {
            vout!("failed incoming http connection, maybe()\n");
            rsrc::Event::Failure(Val::Void, None)
        });
    vout!("end http_bind\n");
    rsrc::Event::Stream(Box::new(serve))
}



pub fn load_rust_func(func_name: &str) -> Option<Code>
{
    match func_name {
        "bind" => Some(Code::Iop(http_bind, None)),
        _ => None,
    }
}


#[cfg(test)]
mod tests
{
    use leema::io::tests::{exercise_iop_action};
    use leema::udp;
    use leema::val::{Val};

}


