use crate::leema::code::Code;
use crate::leema::frame::FrameTrace;
use crate::leema::lstr::Lstr;
use crate::leema::rsrc::{self, Rsrc};
use crate::leema::val::{self, Type, Val};

use bytes::BytesMut;
use std;
use std::io;
use std::net::{IpAddr, SocketAddr};
use std::str::FromStr;

use futures::future::Future;
use futures::task;
use futures::{Async, Poll};
use tokio::io::{AsyncRead, AsyncWrite};
use tokio::net::{TcpListener, TcpStream};


const SOCKET_TYPE: Type = user_type!("/tcp/Socket");
const LISTENER_TYPE: Type = user_type!("/tcp/Listener");

impl Rsrc for TcpStream
{
    fn get_type(&self) -> Type
    {
        SOCKET_TYPE
    }
}

impl Rsrc for TcpListener
{
    fn get_type(&self) -> Type
    {
        LISTENER_TYPE
    }
}

struct Acceptor
{
    listener: Option<TcpListener>,
}

impl Future for Acceptor
{
    type Item = (TcpListener, TcpStream, SocketAddr);
    type Error = (TcpListener, std::io::Error);

    fn poll(&mut self) -> Poll<Self::Item, Self::Error>
    {
        let accept_result = { self.listener.as_mut().unwrap().poll_accept() };
        match accept_result {
            Ok(Async::Ready((sock, addr))) => {
                let listener = self.listener.take().unwrap();
                Ok(Async::Ready((listener, sock, addr)))
            }
            Ok(Async::NotReady) => {
                task::current().notify();
                Ok(Async::NotReady)
            }
            Err(e) => {
                panic!("failure accepting new tcp connection: {:?}", e);
            }
        }
    }
}

struct Receiver
{
    sock: Option<TcpStream>,
}

impl Future for Receiver
{
    type Item = (TcpStream, Val);
    type Error = (TcpStream, std::io::Error);

    fn poll(&mut self) -> Poll<Self::Item, Self::Error>
    {
        let mut buf = BytesMut::new();
        let read_result = self.sock.as_ref().unwrap().read_buf(&mut buf);
        match read_result {
            Ok(Async::Ready(_sz)) => {
                let isock = self.sock.take().unwrap();
                let rstr = String::from_utf8(buf.to_vec()).unwrap();
                let rval = Val::Str(Lstr::from(rstr));
                Ok(Async::Ready((isock, rval)))
            }
            Ok(Async::NotReady) => {
                vout!("Receiver NotReady\n");
                Ok(Async::NotReady)
            }
            Err(e) => {
                match e.kind() {
                    io::ErrorKind::WouldBlock => {
                        vout!("Receiver WouldBlock\n");
                        Ok(Async::NotReady)
                    }
                    _ => {
                        let sock = self.sock.take().unwrap();
                        Err((sock, e))
                    }
                }
            }
        }
    }
}

struct Sender
{
    ctx: rsrc::IopCtx,
}

impl Future for Sender
{
    type Item = rsrc::Event;
    type Error = rsrc::Event;

    fn poll(&mut self) -> Poll<rsrc::Event, rsrc::Event>
    {
        let mut sock: TcpStream = self.ctx.take_rsrc();
        let msg = self.ctx.take_param(1).unwrap();
        vout!("tcp::Sender::poll({})\n", msg);

        let write_result = sock.poll_write(msg.str().as_bytes());
        let nbytes = match write_result {
            Ok(Async::Ready(nb)) => nb as i64,
            Ok(Async::NotReady) => {
                self.ctx.init_rsrc(Box::new(sock)).unwrap();
                return Ok(Async::NotReady);
            }
            Err(e) => {
                panic!("failed writing to socket: {:?}", e);
            }
        };

        let result = rsrc::Event::seq(
            rsrc::Event::ReturnRsrc(Box::new(sock)),
            rsrc::Event::Result(Val::Int(nbytes)),
        );
        Ok(Async::Ready(result))
    }
}


pub fn tcp_connect(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    vout!("tcp_connect()\n");
    let sock_addr = {
        let sock_addr_str = ctx.take_param(0).unwrap();
        let port = ctx.take_param(1).unwrap().to_int() as u16;
        SocketAddr::new(IpAddr::from_str(sock_addr_str.str()).unwrap(), port)
    };

    let fut = TcpStream::connect(&sock_addr)
        .map(move |sock| {
            vout!("tcp connected\n");
            rsrc::Event::NewRsrc(Box::new(sock))
        })
        .map_err(move |_| {
            rsrc::Event::Result(Val::failure(
                Val::Hashtag(Lstr::Sref("connection_failure")),
                Val::Str(Lstr::Sref("Failure to connect")),
                FrameTrace::new_root(),
                val::FAILURE_MISSINGDATA,
            ))
        });
    rsrc::Event::Future(Box::new(fut))
}

pub fn tcp_listen(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    vout!("tcp_listen()\n");
    let ip_str = ctx.take_param(0).unwrap();
    let port = ctx.take_param(1).unwrap().to_int() as u16;
    let sock_addr =
        SocketAddr::new(IpAddr::from_str(ip_str.str()).unwrap(), port);
    let listen_result = TcpListener::bind(&sock_addr);
    let listener: TcpListener = listen_result.unwrap();
    rsrc::Event::NewRsrc(Box::new(listener))
}

pub fn tcp_accept(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    vout!("tcp_accept()\n");
    let listener: TcpListener = ctx.take_rsrc();
    let acc = Acceptor {
        listener: Some(listener),
    }
    .map(|(ilistener, sock, _addr)| {
        rsrc::Event::seq(
            rsrc::Event::ReturnRsrc(Box::new(ilistener)),
            rsrc::Event::NewRsrc(Box::new(sock)),
        )
    })
    .map_err(|_| rsrc::Event::Result(Val::Str(Lstr::Sref("accept error"))));
    rsrc::Event::Future(Box::new(acc))
}


/**
 * tcp_recv(sock)
 */
pub fn tcp_recv(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    vout!("tcp_recv()\n");

    let sock: TcpStream = ctx.take_rsrc();
    let fut = Receiver { sock: Some(sock) }
        .map(|(isock, data)| {
            rsrc::Event::seq(
                rsrc::Event::ReturnRsrc(Box::new(isock)),
                rsrc::Event::Result(data),
            )
        })
        .map_err(|(isock, _err)| {
            let errval = Val::Str(Lstr::Sref("recv failure"));
            rsrc::Event::seq(
                rsrc::Event::ReturnRsrc(Box::new(isock)),
                rsrc::Event::Result(errval),
            )
        });
    rsrc::Event::Future(Box::new(fut))
}

pub fn tcp_send(ctx: rsrc::IopCtx) -> rsrc::Event
{
    vout!("tcp_send()\n");
    let fut = Sender { ctx };
    rsrc::Event::Future(Box::new(fut))
}

pub fn load_rust_func(func_name: &str) -> Option<Code>
{
    match func_name {
        "connect" => Some(Code::Iop(tcp_connect, None)),
        "listen" => Some(Code::Iop(tcp_listen, None)),
        "accept" => Some(Code::Iop(tcp_accept, Some(0))),
        "recv" => Some(Code::Iop(tcp_recv, Some(0))),
        "send" => Some(Code::Iop(tcp_send, Some(0))),
        _ => None,
    }
}
