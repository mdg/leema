use leema::code::Code;
use leema::log;
use leema::lstr::Lstr;
use leema::rsrc::{self, Rsrc};
use leema::val::{Type, Val};

use std::io::Write;
use std::net::{IpAddr, SocketAddr};
use std::str::FromStr;

use futures::future::Future;
use tokio::net::UdpSocket;


impl Rsrc for UdpSocket
{
    fn get_type(&self) -> Type
    {
        Type::Resource(Lstr::Sref("UdpSocket"))
    }
}


pub fn udp_socket(_ctx: rsrc::IopCtx) -> rsrc::Event
{
    let sock_addr = SocketAddr::new(IpAddr::from_str("0.0.0.0").unwrap(), 0);
    let rsock = UdpSocket::bind(&sock_addr).unwrap();
    rsrc::Event::NewRsrc(Box::new(rsock), None)
}

pub fn udp_bind(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    vout!("udp_bind()\n");
    let sock_addr_str = ctx.take_param(0).unwrap();
    let port = ctx.take_param(1).unwrap().to_int() as u16;
    let sock_addr =
        SocketAddr::new(IpAddr::from_str(sock_addr_str.str()).unwrap(), port);
    let rsock = UdpSocket::bind(&sock_addr).unwrap();
    rsrc::Event::NewRsrc(Box::new(rsock), None)
}

/**
 * udp_recv(sock)
 */
pub fn udp_recv(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    vout!("udp_recv()\n");

    let buffer: Vec<u8> = Vec::with_capacity(2048);
    let sock: UdpSocket = ctx.take_rsrc();
    let fut = sock
        .recv_dgram(buffer)
        .map(|(isock, ibuf, _nbytes, _src_addr)| {
            let utf8_result = String::from_utf8(ibuf);
            let result_val = Val::Str(Lstr::from(utf8_result.unwrap()));
            let irsrc: Box<Rsrc> = Box::new(isock);
            rsrc::Event::Result(result_val, Some(irsrc))
        }).map_err(|e| {
            println!("error receiving UdpSocket bytes: {:?}", e);
            rsrc::Event::Result(
                Val::Str(Lstr::Sref("error receiving UdpSocket str")),
                None,
            )
        });
    rsrc::Event::Future(Box::new(fut))
}

pub fn udp_send(mut ctx: rsrc::IopCtx) -> rsrc::Event
{
    vout!("udp_send()\n");
    let sock: UdpSocket = ctx.take_rsrc();
    let dst_ip = ctx.take_param(1).unwrap();
    let dst_port = ctx.take_param(2).unwrap().to_int() as u16;
    let msg = ctx.take_param(3).unwrap().to_string();

    let dst_addr =
        SocketAddr::new(IpAddr::from_str(dst_ip.str()).unwrap(), dst_port);
    let fut = Box::new(
        sock.send_dgram(msg, &dst_addr)
            .map(move |(sock2, _buff)| {
                let sockr: Box<Rsrc> = Box::new(sock2) as Box<Rsrc>;
                rsrc::Event::Result(Val::Int(0), Some(sockr))
            }).map_err(|_| {
                rsrc::Event::Result(
                    Val::Str(Lstr::Sref(
                        "send dgram didn't work. socket is gone",
                    )),
                    None,
                )
            }),
    );
    rsrc::Event::Future(Box::new(fut))
}

pub fn load_rust_func(func_name: &str) -> Option<Code>
{
    match func_name {
        "udp_bind" => Some(Code::Iop(udp_bind, None)),
        "udp_recv" => Some(Code::Iop(udp_recv, Some(0))),
        "udp_send" => Some(Code::Iop(udp_send, Some(0))),
        "udp_socket" => Some(Code::Iop(udp_socket, None)),
        _ => None,
    }
}


#[cfg(test)]
mod tests
{
    use leema::io::tests::exercise_iop_action;
    use leema::udp;
    use leema::val::Val;

    #[test]
    fn test_udp_socket_creation()
    {
        let response = exercise_iop_action(udp::udp_socket, vec![]);
        assert!(response.is_ok());
        let (_fiber_id, rsrc_ref) = response.ok().unwrap();
        assert_eq!(Val::ResourceRef(1), rsrc_ref);
    }

}
