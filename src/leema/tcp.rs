use leema::code::{Code, RustFunc};
use leema::log;
use leema::rsrc::{self, Rsrc};
use leema::val::{Val, Type};

use std::net::{IpAddr, SocketAddr};
use std::rc::{Rc};
use std::str::{FromStr};
use std::io::{stderr, Write};
use std::os::unix::io::AsRawFd;

use ::tokio_core::net::{TcpStream};
use ::tokio_core::reactor::{Handle, Remote};
use futures::future::{Future};


impl Rsrc for TcpStream
{
    fn get_type(&self) -> Type
    {
        Type::Resource(Rc::new(String::from("TcpSocket")))
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
            rsrc::Event::NewRsrc(Box::new(sock))
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
    /*
    let sock: UdpSocket = ctx.take_rsrc();
    let dst_ip = ctx.take_param(1).unwrap();
    let dst_port = ctx.take_param(2).unwrap().to_int() as u16;
    let msg = ctx.take_param(3).unwrap().to_string();

    let dst_addr = SocketAddr::new(
        IpAddr::from_str(dst_ip.str()).unwrap(),
        dst_port,
    );
    let fut = Box::new(
        sock.send_dgram(msg, dst_addr)
        .map(move |(sock2, buff)| {
            let sockr: Box<Rsrc> = Box::new(sock2) as Box<Rsrc>;
            (Val::Int(0), Some(sockr))
        })
        .map_err(|e| {
            Val::new_str("send dgram didn't work. socket is gone".to_string())
        })
    );
    rsrc::Event::Future(Box::new(fut))
    */
    rsrc::Event::Success(Val::Void, None)
}

pub fn load_rust_func(func_name: &str) -> Option<Code>
{
    match func_name {
        "connect" => Some(Code::Iop(tcp_connect, None)),
        "tcp_recv" => Some(Code::Iop(tcp_recv, Some(0))),
        "tcp_send" => Some(Code::Iop(tcp_send, Some(0))),
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

