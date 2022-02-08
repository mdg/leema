use crate::leema::code::Code;
use crate::leema::lstr::Lstr;
use crate::leema::rsrc::{self, IopFuture, Rsrc};
use crate::leema::val::{Type, Val};

use std::net::{IpAddr, SocketAddr};
use std::str::FromStr;

use tokio::net::UdpSocket;


const SOCKET_TYPE: Type = user_type!("/udp/Socket");

impl Rsrc for UdpSocket
{
    fn get_type(&self) -> Type
    {
        SOCKET_TYPE
    }
}


pub fn udp_socket(mut ctx: rsrc::IopCtx) -> IopFuture
{
    Box::pin(async move {
        let sock_addr =
            SocketAddr::new(IpAddr::from_str("0.0.0.0").unwrap(), 0);
        let rsock = UdpSocket::bind(&sock_addr).await.unwrap();
        ctx.return_rsrc(Box::new(rsock));
        ctx
    })
}

pub fn udp_bind(mut ctx: rsrc::IopCtx) -> IopFuture
{
    Box::pin(async move {
        vout!("udp_bind()\n");
        let sock_addr_str = ctx.take_param(0).unwrap();
        let port = ctx.take_param(1).unwrap().to_int() as u16;
        let sock_addr = SocketAddr::new(
            IpAddr::from_str(sock_addr_str.str()).unwrap(),
            port,
        );
        let rsock = UdpSocket::bind(&sock_addr).await.unwrap();
        ctx.return_rsrc(Box::new(rsock));
        ctx
    })
}

pub fn udp_recv(mut ctx: rsrc::IopCtx) -> IopFuture
{
    Box::pin(async move {
        vout!("udp_recv");
        let mut buf: Vec<u8> = vec![0; 2048];
        let sock: &mut UdpSocket = ctx.rsrc_mut(0).unwrap();

        let nbytes = match sock.recv(&mut buf).await {
            Ok(result) => {
                vout!("recv ready bytes: {:?}", result);
                result
            }
            Err(e) => {
                panic!("UdpRecv error: {:?}", e);
            }
        };
        unsafe {
            buf.set_len(nbytes);
        }
        let utf8 = String::from_utf8(buf).unwrap();
        ctx.set_result(Val::Str(Lstr::from(utf8)));
        ctx
    })
}

pub fn udp_send(mut ctx: rsrc::IopCtx) -> IopFuture
{
    Box::pin(async move {
        let dst_ip = ctx.take_param(1).unwrap();
        let dst_port = ctx.take_param(2).unwrap().to_int() as u16;
        vout!("udp_send({}, {})\n", dst_ip, dst_port);
        let msg = ctx.take_param(3).unwrap().to_string();

        let dst_addr =
            SocketAddr::new(IpAddr::from_str(dst_ip.str()).unwrap(), dst_port);
        let sock: &mut UdpSocket = ctx.rsrc_mut(0).unwrap();
        let nbytes = sock.send_to(msg.as_bytes(), &dst_addr).await.unwrap();
        ctx.set_result(Val::Int(nbytes as i64));
        ctx
    })
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
