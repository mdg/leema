use crate::leema::code::Code;
use crate::leema::lstr::Lstr;
use crate::leema::rsrc::{self, IopFuture, Rsrc};
use crate::leema::val::{Type, Val};

use std;
use std::net::{IpAddr, SocketAddr};
use std::str::FromStr;

use tokio::io::{AsyncReadExt, AsyncWriteExt};
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

pub fn tcp_connect(mut ctx: rsrc::IopCtx) -> IopFuture
{
    Box::pin(async move {
        vout!("tcp_connect()\n");
        let sock_addr = {
            let sock_addr_str = ctx.take_param(0).unwrap();
            let port = ctx.take_param(1).unwrap().to_int() as u16;
            SocketAddr::new(
                IpAddr::from_str(sock_addr_str.str()).unwrap(),
                port,
            )
        };

        let sock = TcpStream::connect(&sock_addr).await.unwrap();
        ctx.return_rsrc(Box::new(sock));
        ctx
    })
}

pub fn tcp_listen(mut ctx: rsrc::IopCtx) -> IopFuture
{
    Box::pin(async move {
        vout!("tcp_listen()\n");
        let ip_str = ctx.take_param(0).unwrap();
        let port = ctx.take_param(1).unwrap().to_int() as u16;
        let sock_addr =
            SocketAddr::new(IpAddr::from_str(ip_str.str()).unwrap(), port);
        let listen_result = TcpListener::bind(&sock_addr).await;
        let listener: TcpListener = listen_result.unwrap();
        ctx.return_rsrc(Box::new(listener));
        ctx
    })
}

/// really should return the connecting address too
pub fn tcp_accept(mut ctx: rsrc::IopCtx) -> IopFuture
{
    Box::pin(async move {
        vout!("tcp_accept()\n");
        let listener: &mut TcpListener = ctx.rsrc_mut(0).unwrap();
        let (sock, _addr) = listener.accept().await.unwrap();
        ctx.return_rsrc(Box::new(sock));
        // also tho, return addr
        ctx
    })
}

/**
 * tcp_recv(sock)
 */
pub fn tcp_recv(mut ctx: rsrc::IopCtx) -> IopFuture
{
    Box::pin(async move {
        vout!("tcp_recv()\n");

        let sock: &mut TcpStream = ctx.rsrc_mut(0).unwrap();
        let mut result = String::new();
        sock.read_to_string(&mut result).await.unwrap();
        ctx.set_result(Val::Str(Lstr::from(result)));
        ctx
    })
}

pub fn tcp_send(mut ctx: rsrc::IopCtx) -> IopFuture
{
    Box::pin(async move {
        vout!("tcp_send()\n");
        let msg = ctx.take_param(1).unwrap();
        let sock: &mut TcpStream = ctx.rsrc_mut(0).unwrap();
        vout!("tcp::Sender::poll({})\n", msg);

        sock.write_all(msg.str().as_bytes()).await.unwrap();
        ctx
    })
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
