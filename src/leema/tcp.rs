use leema::code::{Code};
use leema::log;
use leema::rsrc::{self, Rsrc};
use leema::val::{Val, Type};

use std;
use std::net::{IpAddr, SocketAddr};
use std::rc::{Rc};
use std::str::{FromStr};
use std::io::{self, Write};
use bytes::{BytesMut};
use bytes::buf::{BufMut};

use ::tokio_core::net::{TcpStream, TcpListener};
use ::tokio_core::reactor::{Handle};
use ::tokio_io::{AsyncRead};
use ::tokio_io::codec::{Framed, Encoder, Decoder};
use futures::{Async, Poll};
use futures::future::{Future};
use futures::sink::{Sink};
use futures::task;


#[derive(Debug)]
struct TcpValCodec
{
}

impl Encoder for TcpValCodec
{
    type Item = Val;
    type Error = Val;

    fn encode(&mut self, item: Val, dst: &mut BytesMut)
        -> Result<(), Self::Error>
    {
        BufMut::put_slice(dst, item.str().as_bytes());
        Ok(())
    }
}

impl Decoder for TcpValCodec
{
    type Item = Val;
    type Error = Val;

    fn decode(&mut self, _src: &mut BytesMut) ->
        Result<Option<Val>, Self::Error>
    {
        Ok(Some(Val::Void))
    }
}

impl Rsrc for Framed<Box<TcpStream>, TcpValCodec>
{
    fn get_type(&self) -> Type
    {
        Type::Resource(Rc::new(String::from("TcpSocket")))
    }
}

impl Rsrc for TcpStream
{
    fn get_type(&self) -> Type
    {
        Type::Resource(Rc::new(String::from("TcpSocket")))
    }
}

impl Rsrc for TcpListener
{
    fn get_type(&self) -> Type
    {
        Type::Resource(Rc::new(String::from("TcpListener")))
    }
}

struct Acceptor
{
    listener: Option<TcpListener>,
    handle: Handle,
}

impl Future for Acceptor
{
    type Item = (TcpListener, TcpStream, SocketAddr);
    type Error = (TcpListener, std::io::Error);

    fn poll(&mut self)
        -> Poll<(TcpListener, TcpStream, SocketAddr),
                (TcpListener, std::io::Error)>
    {
        let accept_result = {
            self.listener.as_mut().unwrap().accept()
        };
        match accept_result {
            Ok((sock, addr)) => {
                let listener = self.listener.take().unwrap();
                Ok(Async::Ready((listener, sock, addr)))
            }
            Err(e) => {
                match e.kind() {
                    io::ErrorKind::WouldBlock => {
                        task::current().notify();
                        Ok(Async::NotReady)
                    }
                    _ => {
                        let listener = self.listener.take().unwrap();
                        Err((listener, e))
                    }
                }
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

    fn poll(&mut self)
        -> Poll<(TcpStream, Val),
                (TcpStream, std::io::Error)>
    {
        let mut buf = BytesMut::new();
        let read_result = self.sock.as_ref().unwrap().read_buf(&mut buf);
        match read_result {
            Ok(Async::Ready(_sz)) => {
                let isock = self.sock.take().unwrap();
                let rstr = String::from_utf8(buf.to_vec()).unwrap();
                let rval = Val::new_str(rstr);
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


pub fn tcp_connect(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    vout!("tcp_connect()\n");
    let sock_addr = {
        let sock_addr_str = ctx.take_param(0).unwrap();
        let port = ctx.take_param(1).unwrap().to_int() as u16;
        SocketAddr::new(
            IpAddr::from_str(sock_addr_str.str()).unwrap(), port
        )
    };

    let handle = ctx.handle().clone();
    let fut =
        TcpStream::connect(&sock_addr, &handle)
        .map(move |sock| {
            vout!("tcp connected");
            let codec = TcpValCodec{};
            let box_sock = Box::new(sock);
            let framed = AsyncRead::framed(box_sock, codec);
            rsrc::Event::NewRsrc(Box::new(framed), None)
        })
        .map_err(move |e| {
            rsrc::Event::Result(
                Val::new_str("Failure to connect".to_string()),
                None,
            )
        });
    rsrc::Event::Future(Box::new(fut))
}

pub fn tcp_listen(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    vout!("tcp_listen()\n");
    let ip_str = ctx.take_param(0).unwrap();
    let port = ctx.take_param(1).unwrap().to_int() as u16;
    let sock_addr = SocketAddr::new(
        IpAddr::from_str(ip_str.str()).unwrap(), port
    );
    let handle = ctx.handle().clone();
    let listen_result = TcpListener::bind(&sock_addr, &handle);
    let listener: TcpListener = listen_result.unwrap();
    rsrc::Event::NewRsrc(Box::new(listener), None)
}

pub fn tcp_accept(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    vout!("tcp_accept()\n");
    let listener: TcpListener = ctx.take_rsrc();
    let acc =
        Acceptor{
            listener: Some(listener),
            handle: ctx.handle().clone(),
        }
        .map(|(ilistener, sock, addr)| {
            rsrc::Event::NewRsrc(Box::new(sock), None)
        })
        .map_err(|e| {
            rsrc::Event::Result(Val::new_str("accept error".to_string()), None)
        });
    rsrc::Event::Future(Box::new(acc))
}


/**
 * tcp_recv(sock)
 */
pub fn tcp_recv(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    vout!("tcp_recv()\n");

    let sock: TcpStream = ctx.take_rsrc();
    let fut =
        Receiver{
            sock: Some(sock),
        }
        .map(|(isock, data)| {
            rsrc::Event::Result(data, Some(Box::new(isock)))
        })
        .map_err(|(isock, err)| {
            let errval = Val::new_str("recv failure".to_string());
            rsrc::Event::Result(errval, Some(Box::new(isock)))
        });
    rsrc::Event::Future(Box::new(fut))
}

pub fn tcp_send(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    vout!("tcp_send()\n");
    let sock: Framed<Box<TcpStream>, TcpValCodec> = ctx.take_rsrc();
    let msg = ctx.take_param(1).unwrap();

    let fut = Box::new(Sink::send(sock, msg)
        .map(|sock2| {
            rsrc::Event::Result(Val::Int(0), Some(Box::new(sock2)))
        })
        .map_err(|e| {
            rsrc::Event::Result(Val::new_str("send failure".to_string()), None)
        }));
    rsrc::Event::Future(fut)
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


#[cfg(test)]
mod tests
{
    use leema::io::tests::{exercise_iop_action};
    use leema::val::{Val};

}

