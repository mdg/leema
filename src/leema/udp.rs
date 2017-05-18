use leema::code::{Code, RustFunc};
use leema::frame::{Frame, Event};
use leema::val::{Val, LibVal, Type};

use std::net::{IpAddr, SocketAddr};
use std::str::{FromStr};
use std::sync::{Arc};
use ::tokio_core::net::{UdpSocket};
use ::tokio_core::reactor::{Handle};
use futures::future::{Future};
use std::os::unix::io::AsRawFd;


pub fn udp_bind(fs: &mut Frame) -> Event
{
    /*
    let send_addr = SocketAddr::new(
        IpAddr::from_str("127.0.0.1").unwrap(),
        3999,
    );
    let addr_val = fs.e.get_param(0);
    let port_val = fs.e.get_param(1);
    let sock_val = match (addr_val, port_val) {
        (&Val::Str(ref addr), &Val::Int(big_port)) => {
            let ip = IpAddr::from_str(addr).unwrap();
            let short_port = big_port as u16;
            let sock_addr = SocketAddr::new(ip, short_port);
            let sock = UdpSocket::bind(&sock_addr, h).unwrap();
let sock_fd = sock.as_raw_fd();
println!("sock fd: {}", sock_fd);
            Val::Lib(LibVal{
                v: Arc::new(sock),
                t: Type::Lib(String::from("UdpStream")),
            })
        }
        _ => {
            panic!("invalid parameters to udp_bind");
        }
    };
    // fs.parent.set_result(sock_val);
    fs.parent.set_result(Val::Int(0));
    */
    Event::success()
}

pub fn udp_recv(f: &mut Frame) -> Event
{
    let sock_ref = f.e.get_param(0);
    /*
    let evf = |sock| {
        let buf = String::from("hello");
        sock.recv_dgram(buf)
            .map(|(sock, buf2)| {
                buf2
            });
    };
    let post_ev = |fiber, (_sock, buf)| {
        fiber.head.e.set(Reg::reg(0), buf);
    };
    let ev = Event::ResourceAction(sock_ref, |sock| {
        })
        .and_then(|whatever| {
        });
        */
    Event::success()
}

pub fn udp_send(fs: &mut Frame) -> Event
{
    println!("udp_send");
    let dst = fs.e.get_param(0);
    let sock_ref = fs.e.get_param(1);
    let msg = fs.e.get_param(2);
    /*
    let send_addr = dst.to_str();
    let event = Event::ResourceAction(sock_ref, |sock| {
            sock.send_dgram(msg, send_addr);
        });
            let sent = sock.send_dgram(msg, send_addr)
                .map(move |(s, t)| {
                    ()
                })
                .map_err(|e| {
                    println!("dgram error {:?}", e);
                    ()
                });
            h.spawn(sent);
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
    */
    Event::success()
}

pub fn load_rust_func(func_name: &str) -> Option<Code>
{
    match func_name {
        "udp_bind" => Some(Code::Rust(udp_bind)),
        "udp_recv" => Some(Code::Rust(udp_recv)),
        "udp_send" => Some(Code::Rust(udp_send)),
        _ => None,
    }
}

