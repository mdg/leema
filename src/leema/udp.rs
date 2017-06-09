use leema::code::{Code, RustFunc};
use leema::fiber::{Fiber};
use leema::frame::{Frame, Event};
use leema::log;
use leema::val::{Val, LibVal, Type};

use std::net::{IpAddr, SocketAddr};
use std::str::{FromStr};
use std::sync::{Arc, Mutex, MutexGuard, TryLockError};
use std::io::{stderr, Write};
use std::os::unix::io::AsRawFd;

use ::tokio_core::net::{UdpSocket};
use ::tokio_core::reactor::{Handle, Remote};
use futures::future::{Future};


#[derive(Debug)]
struct UdpSock
{
    handle: Handle,
    remote: Remote,
    socket: Option<UdpSocket>,
    buffer: String,
}

impl LibVal for Mutex<UdpSock>
{
    fn get_type(&self) -> Type
    {
        Type::Lib(String::from("UdpSocket"))
    }
}

pub fn udp_socket(f: &mut Fiber) -> Event
{
    vout!("udp_socket\n");
    let sock_addr = SocketAddr::new(IpAddr::from_str("0.0.0.0").unwrap(), 0);
    let rsock = UdpSocket::bind(&sock_addr, &f.handle).unwrap();
    let lsock = UdpSock{
        handle: f.handle.clone(),
        remote: f.handle.remote().clone(),
        socket: Some(rsock),
        buffer: String::from(""),
    };
    let rval = Val::libval(Mutex::new(lsock));
    f.head.parent.set_result(rval);
    Event::success()
}

pub fn udp_bind(f: &mut Fiber) -> Event
{
println!("udp_bind");
    let addr_val = f.head.e.get_param(0);
    let port_val = f.head.e.get_param(1);
    /*
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
    */
    f.head.parent.set_result(Val::Int(0));
    Event::success()
}

pub fn udp_recv(f: &mut Fiber) -> Event
{
    /*
    let sockr = f.e.get_param(0);
    let mutex_sock: &Mutex<UdpSock> = sockr.libval_as();
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
    f.head.parent.set_result(Val::Int(0));
    Event::success()
}

pub fn udp_send(f: &mut Fiber) -> Event
{
    vout!("udp_send.e = {:?}\n", f.head.e);
    let sockr = f.head.e.get_param(0);
    let dst_ip = f.head.e.get_param(1);
    let dst_port =
        if let &Val::Int(p) = f.head.e.get_param(2) {
            p as i16
        } else {
            panic!("port is not a number");
        };
    let msg =
        if let &Val::Str(ref s) = f.head.e.get_param(3) {
            s.clone()
        } else {
            panic!("msg is not a string");
        };
    let send_addr = SocketAddr::new(
        IpAddr::from_str(dst_ip.str()).unwrap(),
        3999,
    );
    let omsg = (&*msg).clone();

    vout!("udp_send(sockr, '{}', {}, '{}')\n", dst_ip, dst_port, msg);
    vout!("sockr: {:?}\n", sockr);

    let opt_mutex = sockr.libval_as();
    let mutex_sock: &Mutex<UdpSock> = opt_mutex.unwrap();
    match mutex_sock.try_lock() {
        Ok(ref mut guard) => {
            let sock = guard.socket.take().unwrap();
            let fut = sock.send_dgram(omsg, send_addr)
                .map(|(used_sock, buf)| {
                    println!("send_dgram sent");
                    ()
                })
                .map_err(|e| {
                    println!("send_dgram error: {:?}", e);
                    ()
                });
            guard.handle.spawn(fut);
        }
        Err(TryLockError::WouldBlock) => {
            return Event::Uneventful;
        }
        Err(TryLockError::Poisoned(ref p)) => {
            panic!("socket lock is poisoned");
        }
    }
    /*
    let send_addr = SocketAddr::new(
        IpAddr::from_str("127.0.0.1").unwrap(),
        3999,
    );
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
    f.head.parent.set_result(Val::Int(0));
    Event::success()
}

pub fn load_rust_func(func_name: &str) -> Option<Code>
{
    match func_name {
        "udp_bind" => Some(Code::Rust(udp_bind)),
        "udp_recv" => Some(Code::Rust(udp_recv)),
        "udp_send" => Some(Code::Rust(udp_send)),
        "udp_socket" => Some(Code::Rust(udp_socket)),
        _ => None,
    }
}

