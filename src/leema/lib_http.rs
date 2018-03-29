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
use hyper::server::{self, Http, Service, Serve};
use hyper::proto::dispatch::{Dispatch};


impl LibVal for server::Request
{
    fn get_type(&self) -> Type
    {
        Type::Lib("Request".to_string())
    }
}

impl LibVal for server::Response
{
    fn get_type(&self) -> Type
    {
        Type::Lib("Response".to_string())
    }
}

struct Transaction
{
    c: server::Connection<hyper::server::addr_stream::AddrStream, LeemaHttp>,
    req: Option<server::Request>,
}

impl Transaction
{
    pub fn new_box(
        c: server::Connection<hyper::server::addr_stream::AddrStream, LeemaHttp>
        ) -> Box<Transaction>
    {
        Box::new(Transaction{
            c: c,
            req: None,
        })
    }
}

impl rsrc::Rsrc for Transaction
{
    fn get_type(&self) -> Type
    {
        Type::Resource(Rc::new("Transaction".to_string()))
    }
}

impl Future for Transaction
{
    type Item = rsrc::Event;
    type Error = rsrc::Event;

    fn poll(&mut self) -> Poll<Self::Item, Self::Error>
    {
println!("Transaction::poll");
        Ok(Async::NotReady)
        /*
        self.c.poll()
            .map(|opaq| {
println!("transaction future map");
                rsrc::Event::Success(Val::Int(0), None)
            })
            .map_err(|e| {
println!("transaction future err");
                rsrc::Event::Success(Val::Int(7), None)
            })
            */
    }
}

impl fmt::Debug for Transaction
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "Transaction")
    }
}

struct Tx2
{
    pub req: server::Request,
}

impl future::Future for Tx2
{
    type Item = server::Response;
    type Error = hyper::Error;

    fn poll(&mut self) -> Poll<Self::Item, Self::Error>
    {
        Ok(Async::NotReady)
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
    type Request = server::Request;
    type Response = server::Response;
    // type Response = rsrc::Event;
    type Error = hyper::Error;
    // type Error = rsrc::Event;
    type Future = Box<future::Future<Item=Self::Response, Error=Self::Error>>;

    fn call(&self, req: Self::Request) -> Self::Future
    {
println!("LeemaHttp.call({:?})", req);
panic!("where am i?");
        let resp = server::Response::new()
                .with_body("tacos\n");
        // .map(|resp| {
        //     rsrc::Event::Success(Val::Lib(Arc::new(resp)), None)
        // });
        // Box::new(future::ok(rsrc::Event::Success(Val::Int(8), None)))
        Box::new(Tx2{req: req})
    }
}

struct NewLeemaHttp;

impl server::NewService for NewLeemaHttp
{
    type Request = server::Request;
    type Response = server::Response;
    // type Response = rsrc::Event;
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

impl rsrc::Rsrc for HttpServer
{
    fn get_type(&self) -> Type
    {
        Type::Resource(Rc::new("Server".to_string()))
    }
}

impl Future for HttpServer
{
    type Item = rsrc::Event;
    type Error = rsrc::Event;

    fn poll(&mut self) -> Poll<Self::Item, Self::Error>
    {
println!("http server future poll");
        task::current().notify();
        let mut pollstate = self.serve.poll();
        match &mut pollstate {
            &mut Ok(Async::NotReady) => {
                Ok(Async::NotReady)
            }
            &mut Ok(Async::Ready(ref mut opt_conn)) => {
println!("pollstate ok: {:?}", opt_conn);
                let conn = opt_conn.take().unwrap();
                /*
                Ok(Async::Ready(rsrc::Event::NewRsrc(
                    Box::new(Conn{c: conn}),
                )))
                */
                Ok(Async::NotReady)
            }
            &mut Err(ref pollstate2) => {
println!("poll err state: {:?}", pollstate2);
                // pollstate.unwrap()
                Err(rsrc::Event::Result(
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
            rsrc::Event::Result(Val::Void, None)
        });
        */
    let future_loop = HttpServer{serve: serve};
    vout!("end http_bind\n");
    rsrc::Event::NewRsrc(Box::new(future_loop), None)
}

pub fn http_accept(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    vout!("http_accept()\n");

    let srv: HttpServer = ctx.take_rsrc();
println!("server: {:?}", srv);

    let fut = srv.serve.into_future()
        .map(|(opt_item, isrv)| {
            let item = opt_item.unwrap();
println!("received a: {:?}", item);
let sp = item.should_poll();
println!("should poll? {}", sp);
            let tx = Transaction::new_box(item);
            let srv_result = Box::new(HttpServer{serve: isrv});
            rsrc::Event::NewRsrc(tx, Some(srv_result))
        })
        .map_err(|(err, isrv)| {
            let srv_result = Box::new(HttpServer{serve: isrv});
            rsrc::Event::Result(Val::Int(0), Some(srv_result))
        });
    rsrc::Event::Future(Box::new(fut))
}

pub fn http_request(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    let tx: Transaction = ctx.take_rsrc();
println!("tx is what? '{:?}'", tx);
    Dispatch::recv_req(tx.c);
    rsrc::Event::Result(Val::Int(0), None)
}

pub fn load_rust_func(func_name: &str) -> Option<Code>
{
    match func_name {
        "bind" => Some(Code::Iop(http_bind, None)),
        "accept" => Some(Code::Iop(http_accept, Some(0))),
        "request" => Some(Code::Iop(http_request, Some(0))),
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

