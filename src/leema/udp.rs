use crate::leema::code::Code;
use crate::leema::lstr::Lstr;
use crate::leema::rsrc::{self, Rsrc};
use crate::leema::val::{Type, Val};

use std::net::{IpAddr, SocketAddr};
use std::str::FromStr;

use futures::{Async, Future, Poll};
use tokio::net::UdpSocket;


const SOCKET_TYPE: Type = user_type!("/udp/Socket");

impl Rsrc for UdpSocket
{
    fn get_type(&self) -> Type
    {
        SOCKET_TYPE
    }
}


pub fn udp_socket(_ctx: rsrc::IopCtx) -> rsrc::Event
{
    let sock_addr = SocketAddr::new(IpAddr::from_str("0.0.0.0").unwrap(), 0);
    let rsock = UdpSocket::bind(&sock_addr).unwrap();
    rsrc::Event::NewRsrc(Box::new(rsock))
}

pub fn udp_bind(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    vout!("udp_bind()\n");
    let sock_addr_str = ctx.take_param(0).unwrap();
    let port = ctx.take_param(1).unwrap().to_int() as u16;
    let sock_addr =
        SocketAddr::new(IpAddr::from_str(sock_addr_str.str()).unwrap(), port);
    let rsock = UdpSocket::bind(&sock_addr).unwrap();
    rsrc::Event::NewRsrc(Box::new(rsock))
}

pub fn udp_recv(ctx: rsrc::IopCtx) -> rsrc::Event
{
    vout!("udp_recv");
    rsrc::Event::Future(Box::new(UdpRecv {
        ctx,
        buffer: Some(vec![0; 2048]),
    }))
}

struct UdpRecv
{
    ctx: rsrc::IopCtx,
    buffer: Option<Vec<u8>>,
}

impl Future for UdpRecv
{
    type Item = rsrc::Event;
    type Error = rsrc::Event;

    fn poll(&mut self) -> Poll<rsrc::Event, rsrc::Event>
    {
        vout!("UdpRecv::poll()\n");
        let mut sock: UdpSocket = self.ctx.take_rsrc();
        let result = match sock.poll_recv_from(self.buffer.as_mut().unwrap()) {
            Ok(Async::Ready(ready_result)) => {
                vout!("poll_recv_from ready: {:?}", ready_result);
                ready_result
            }
            Ok(Async::NotReady) => {
                vout!("poll_recv_from notready");
                self.ctx.init_rsrc(Box::new(sock));
                return Ok(Async::NotReady);
            }
            Err(e) => {
                panic!("UdpRecv error: {:?}", e);
            }
        };
        let (nbytes, _addr) = result;
        let mut buf = self.buffer.take().unwrap();
        unsafe {
            buf.set_len(nbytes);
        }
        let utf8 = String::from_utf8(buf).unwrap();
        let str_result = Val::Str(Lstr::from(utf8));
        Ok(Async::Ready(rsrc::Event::seq(
            rsrc::Event::ReturnRsrc(Box::new(sock)),
            rsrc::Event::Result(str_result),
        )))
    }
}

pub fn udp_send(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    let sock: UdpSocket = ctx.take_rsrc();
    let dst_ip = ctx.take_param(1).unwrap();
    let dst_port = ctx.take_param(2).unwrap().to_int() as u16;
    vout!("udp_send({}, {})\n", dst_ip, dst_port);
    let msg = ctx.take_param(3).unwrap().to_string();

    let dst_addr =
        SocketAddr::new(IpAddr::from_str(dst_ip.str()).unwrap(), dst_port);
    let fut = Box::new(
        sock.send_dgram(msg, &dst_addr)
            .map(move |(sock2, _buff)| {
                let sockr: Box<dyn Rsrc> = Box::new(sock2) as Box<dyn Rsrc>;
                rsrc::Event::seq(
                    rsrc::Event::ReturnRsrc(sockr),
                    rsrc::Event::Result(Val::Int(0)),
                )
            })
            .map_err(|_| {
                rsrc::Event::Result(Val::Str(Lstr::Sref(
                    "send dgram didn't work. socket is gone",
                )))
            }),
    );
    rsrc::Event::Future(Box::new(fut))
}

pub fn load_rust_func(func_name: &str) -> Option<Code>
{
    match func_name {
        "bind" => Some(Code::Iop(udp_bind, None)),
        "recv" => Some(Code::Iop(udp_recv, Some(0))),
        "send" => Some(Code::Iop(udp_send, Some(0))),
        "socket" => Some(Code::Iop(udp_socket, None)),
        _ => None,
    }
}


#[cfg(test)]
mod tests
{
    use crate::leema::io::tests::exercise_iop_action;
    use crate::leema::udp;
    use crate::leema::val::Val;

    #[test]
    fn test_udp_socket_creation()
    {
        let response = exercise_iop_action(udp::udp_socket, vec![]);
        assert!(response.is_ok());
        let (_fiber_id, rsrc_ref) = response.ok().unwrap();
        assert_eq!(Val::ResourceRef(10), rsrc_ref);
    }
}
