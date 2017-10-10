use leema::code::{Code, RustFunc};
use leema::log;
use leema::rsrc::{self, Rsrc};
use leema::val::{Val, Type};

use std::net::{IpAddr, SocketAddr};
use std::rc::{Rc};
use std::str::{FromStr};
use std::io::{self, stderr, Write};
use std::os::unix::io::AsRawFd;
use bytes::{BytesMut};
use bytes::buf::{BufMut};

use ::tokio_core::io::{Codec, EasyBuf};
use ::tokio_core::net::{TcpStream, TcpListener};
use ::tokio_core::reactor::{Handle, Remote};
use ::tokio_io::{AsyncRead};
use ::tokio_io::codec::{Framed, Encoder, Decoder};
use futures::future::{Future};
use futures::sink::{Sink};


#[derive(Debug)]
struct TcpValCodec
{
}

impl Codec for TcpValCodec
{
    type In = Val;
    type Out = Val;

    fn decode(&mut self, buf: &mut EasyBuf) -> io::Result<Option<Val>>
    {
        Ok(Some(Val::Void))
    }

    fn encode(&mut self, msg: Val, buf: &mut Vec<u8>) -> io::Result<()>
    {
        Ok(())
    }

    // fn decode_eof(&mut self, buf: &mut EasyBuf) -> Result<
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

    fn decode(&mut self, src: &mut BytesMut) ->
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


pub fn tcp_connect(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    vout!("tcp_connect()\n");
    let sock_addr = {
        let sock_addr_str = ctx.take_param(0).unwrap();
        let port = ctx.take_param(1).unwrap().to_int() as u16;
        SocketAddr::new(
            IpAddr::from_str((sock_addr_str.str())).unwrap(), port
        )
    };

    let handle = ctx.handle().clone();
    let fut =
        TcpStream::connect(&sock_addr, &handle)
        .map(move |sock| {
            let codec = TcpValCodec{};
            let box_sock = Box::new(sock);
            let framed = AsyncRead::framed(box_sock, codec);
            rsrc::Event::NewRsrc(Box::new(framed))
        })
        .map_err(move |e| {
            rsrc::Event::Failure(
                Val::new_str("Failure to connect".to_string()),
                None,
            )
        });
    // let rsrc_id = ctx.new_rsrc(Box::new(rsock));
    // ctx.send_result(Val::ResourceRef(rsrc_id));
    rsrc::Event::Future(Box::new(fut))
}

pub fn tcp_listen(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    vout!("tcp_listen()\n");
    let ip_str = ctx.take_param(0).unwrap();
    let port = ctx.take_param(1).unwrap().to_int() as u16;
    let sock_addr = SocketAddr::new(
        IpAddr::from_str((ip_str.str())).unwrap(), port
    );
    let listen_result = TcpListener::bind(&sock_addr, &ctx.handle());
    let listener: TcpListener = listen_result.unwrap();
    rsrc::Event::NewRsrc(Box::new(listener))
}

pub fn tcp_accept(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    vout!("tcp_accept()\n");
    let listener: TcpListener = ctx.take_rsrc();
    rsrc::Event::Success(Val::Int(0), Some(Box::new(listener)))
}


/**
 * tcp_recv(sock)
 */
pub fn tcp_recv(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    vout!("tcp_recv()\n");

    let mut buffer: Vec<u8> = Vec::with_capacity(2048);
    /*
    let sock: TcpStream = ctx.take_rsrc();
    let fut = sock.recv_dgram(buffer)
        .map(|(isock, ibuf, nbytes, src_addr)| {
            let utf8_result = String::from_utf8(ibuf);
            let result_val = Val::new_str(utf8_result.unwrap());
            let irsrc: Box<Rsrc> = Box::new(isock);
            (result_val, Some(irsrc))
        })
        .map_err(|e| {
            println!("error receiving UdpSocket bytes: {:?}", e);
            Val::new_str("error receiving UdpSocket str".to_string())
        });
    rsrc::Event::Future(Box::new(fut))
    */
    rsrc::Event::Success(Val::Void, None)
}

pub fn tcp_send(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    vout!("tcp_send()\n");
    let sock: Framed<Box<TcpStream>, TcpValCodec> = ctx.take_rsrc();
    let msg = ctx.take_param(1).unwrap();

    let fut = Box::new(Sink::send(sock, msg)
        .map(|sock2| {
            rsrc::Event::Success(Val::Int(0), Some(Box::new(sock2)))
        })
        .map_err(|e| {
            rsrc::Event::Failure(Val::new_str("send failure".to_string()), None)
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
    use leema::udp;
    use leema::val::{self, Val};

#[test]
fn test_udp_socket_creation()
{
    let response = exercise_iop_action(udp::udp_socket, vec![]);
    assert!(response.is_ok());
    let (_fiber_id, rsrc_ref) = response.ok().unwrap();
    assert_eq!(Val::ResourceRef(1), rsrc_ref);
}

}

