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

use ::tokio_core::reactor::{Handle};
use ::tokio_io::{AsyncRead};
use ::tokio_io::codec::{Framed, Encoder, Decoder};
use futures::{Async, Poll};
use futures::future;
use futures::task;


#[derive(Debug)]
struct HttpService;


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
    rsrc::Event::Success(Val::Void, None)
    // rsrc::Event::Future(Box::new(fut))
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


