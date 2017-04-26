extern crate tokio_core;

use leema::code::{Code, RustIoFunc};
use leema::frame::{Frame};
use leema::val::{Val, LibVal, Type};

use std::net::{IpAddr, SocketAddr};
use std::str::{FromStr};
use std::sync::{Arc};
use self::tokio_core::net::{UdpSocket};
use self::tokio_core::reactor::{Handle};


pub fn udp_bind(fs: &mut Frame, h: &Handle)
{
    let addr_val = fs.e.get_param(0);
    let port_val = fs.e.get_param(1);
    let sock = match (addr_val, port_val) {
        (&Val::Str(ref addr), &Val::Int(big_port)) => {
            let ip = IpAddr::from_str(addr).unwrap();
            let short_port = big_port as u16;
            let sock_addr = SocketAddr::new(ip, short_port);
            let sock = Arc::new(UdpSocket::bind(&sock_addr, h).unwrap());
println!("udp_bind = {:?}", sock);
            Val::Lib(LibVal{
                v: sock,
                t: Type::Lib(String::from("UdpStream")),
            })
        }
        _ => {
            panic!("invalid parameters to udp_bind");
        }
    };
    fs.parent.set_result(sock);
}

pub fn udp_read(fs: &mut Frame, h: &Handle)
{
}

pub fn udp_send(fs: &mut Frame, h: &Handle)
{
    let sock = {
        let mut sock_val = fs.e.get_param_mut(0);
        // let sock_opt: Option<&mut UdpSocket> = sock_val.libval_as();
        let sock_opt: Option<&mut UdpSocket> = sock_val.libval_as_mut();

        match sock_opt {
            Some(ref s) => s.clone(),
            None => {
                panic!("udp_write for not a UdpSocket: {:?}", sock_val);
            }
        }
    };
    println!("sock = {:?}", sock);

    let addr_val = fs.e.get_param(1);
    let port_val = fs.e.get_param(2);
    let text_val = fs.e.get_param(3);
    println!("udp_send({:?}, {:?}, {}, {})",
        sock, addr_val, port_val, text_val);
    match (addr_val, port_val, text_val) {
        (&Val::Str(ref addr), &Val::Int(long_port), &Val::Str(ref output)) => {
            let ip = IpAddr::from_str(addr).unwrap();
            let short_port = long_port as u16;
            let sock_addr = SocketAddr::new(ip, short_port);
            let sent = sock.send_dgram(&**output, sock_addr);
println!("udp_sent = {}", output);
        }
        _ => {
            panic!("invalid parameters to udp_bind");
        }
    }
}

pub fn load_rust_func(func_name: &str) -> Option<Code>
{
    match func_name {
        "udp_bind" => Some(Code::RustIo(udp_bind)),
        "udp_read" => Some(Code::RustIo(udp_read)),
        "udp_send" => Some(Code::RustIo(udp_send)),
        _ => None,
    }
}

