use leema::code::{Code};
use leema::log;
use leema::rsrc::{self, Rsrc};
use leema::val::{Val, Type, LibVal};

use std;
use std::fmt;
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
use hyper::server::{self, Http, Request, Response, Service, Serve, Connection};


impl LibVal for Response
{
    fn get_type(&self) -> Type
    {
        Type::Lib("HttpResponse".to_string())
    }
}

#[derive(Debug)]
struct LeemaHttp;

impl LeemaHttp
{
    pub fn new() -> LeemaHttp
    {
        LeemaHttp
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
println!("LeemaHttp.call({:?})", req);
        let resp = Response::new()
                .with_body("tacos\n");
        // .map(|resp| {
        //     rsrc::Event::Success(Val::Lib(Arc::new(resp)), None)
        // });
        Box::new(future::ok(resp))
    }
}

struct NewLeemaHttp;

impl server::NewService for NewLeemaHttp
{
    type Request = Request;
    type Response = Response;
    type Error = hyper::Error;
    type Instance = LeemaHttp;

    fn new_service(&self) -> Result<Self::Instance, io::Error>
    {
println!("create new service");
        Ok(LeemaHttp)
    }
}

struct HttpServer
{
    serve: Serve<server::AddrIncoming, NewLeemaHttp>,
}

impl LibVal for HttpServer
{
    fn get_type(&self) -> Type
    {
        Type::Struct(Rc::new("Server".to_string()))
    }
}

impl Future for HttpServer
{
    type Item = rsrc::Event;
    type Error = rsrc::Event;

    fn poll(&mut self) -> Poll<Self::Item, Self::Error>
    {
        task::current().notify();
        let mut pollstate = self.serve.poll();
        match &mut pollstate {
            &mut Ok(Async::NotReady) => {
                Ok(Async::NotReady)
            }
            &mut Ok(Async::Ready(ref mut conn)) => {
println!("pollstate ok: {:?}", conn);
                // let next = conn.poll();
                /*
                Ok(Async::Ready(rsrc::Event::Success(
                    Val::new_str("http ok".to_string()),
                    Some(Box::new(conn.unwrap())),
                )))
                */
                let inner_fut = conn.take().unwrap()
                    .map(|op| {
println!("connection is doing something maybe");
                        rsrc::Event::Success(Val::new_str("inner connection win".to_string()), None)
                    })
                    .map_err(|operr| {
println!("connection maybe failed");
                        rsrc::Event::Failure(Val::new_str("inner connection lose".to_string()), None)
                    });
                Ok(Async::Ready(rsrc::Event::Future(
                    Box::new(inner_fut)
                )))
            }
            &mut Err(ref pollstate2) => {
println!("poll err state: {:?}", pollstate2);
                // pollstate.unwrap()
                Err(rsrc::Event::Failure(
                    Val::new_str("http error".to_string()), None
                ))
            }
        }
    }
}

impl fmt::Debug for HttpServer
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "HttpServer")
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
    let handle2 = handle.clone();
    let serve = Http::new()
        .serve_addr_handle(&sock_addr, &handle, NewLeemaHttp)
        .unwrap()
        /*
        .for_each(|conn| {
println!("serve.for_each");
            conn.map(|c1| {
println!("conn.map");
                c1
            })
            .map_err(|c2| {
println!("conn.map_err");
                c2
            });
        });
        */
        // .into_inner();
        /*
        .map(|_| { 8 })
        .map_err(|_| { "tacos" });
        */
        .map(|c| {
println!("new addr handle: {:?}", c);
            c
        })
        .into_inner();
        /*
        .map(|c| {
println!("incoming http connection, maybe? {:?}", c);
c.map(|i| {
    // println!("c.map(i) = {:?}", i);
    i
});
            vout!("incoming http connection, maybe()\n");
            rsrc::Event::Success(Val::Void, None)
        })
        .map_err(|e| {
println!("failed http connection, maybe? {:?}", e);
            vout!("failed incoming http connection, maybe()\n");
            rsrc::Event::Failure(Val::Void, None)
        });
        */
    let future_loop = HttpServer{serve: serve};
    vout!("end http_bind\n");
    rsrc::Event::Future(Box::new(future_loop))
    // rsrc::Event::Stream(Box::new(future_loop))
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


