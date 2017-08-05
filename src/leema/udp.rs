use leema::code::{Code, RustFunc};
use leema::fiber::{Fiber};
use leema::frame::{Frame, Event};
use leema::log;
use leema::reg::{Reg};
use leema::val::{Val, LibVal, Type};

use std::net::{IpAddr, SocketAddr};
use std::str::{FromStr};
use std::sync::{Arc, Mutex, MutexGuard, TryLockResult, TryLockError};
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

pub fn udp_socket(mut f: Fiber) -> Event
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
    Event::success(f)
}

pub fn udp_bind(mut f: Fiber) -> Event
{
    vout!("udp_bind({:?})", f.head.e);
    /*
    let addr_val = f.head.e.get_param(0);
    let port_num = if let &Val::Int(p) = f.head.e.get_param(1) {
        p
    } else {
        0
    } as u16;

    let sock_addr = SocketAddr::new(
        IpAddr::from_str(addr_val.str()).unwrap(),
        port_num,
    );
    let rsock = UdpSocket::bind(&sock_addr, &f.handle).unwrap();
    let lsock = UdpSock{
        handle: f.handle.clone(),
        remote: f.handle.remote().clone(),
        socket: Some(rsock),
        buffer: String::from(""),
    };
    let rval = Val::libval(Mutex::new(lsock));
    f.head.parent.set_result(rval);
    */
    Event::success(f)
}

pub fn udp_recv(mut f: Fiber) -> Event
{
    vout!("udp_recv({:?})\n", f.head.e);
    /*
    let sockr = f.head.e.get_param(0).clone();
    let dstreg = Reg::local(0);
    f.head.e.set_reg(&dstreg, Val::Buffer(Vec::with_capacity(2048)));
    let result = f.head.e.get_reg_mut(&dstreg);
    let opt_sock = sockr.libval_as();
    if opt_sock.is_none() {
        panic!("socket param is not a UdpSock: {:?}", sockr);
    }
    let mutex_sock: &Mutex<UdpSock> = opt_sock.unwrap();
    match mutex_sock.try_lock() {
        Ok(ref mut guard) => {
            let mut buffer: Vec<u8> = Vec::with_capacity(2048);
            // let mut buffer = String::from("");
            let sock = guard.socket.take().unwrap();
            let sockr2 = sockr.clone();
            // let fut = sock.recv_dgram(result)
            / *
            let fut = sock.recv_dgram(buffer)
                .map(|(isock, ibuf, nbytes, src_addr)| {
                    // sockr.
                    // buf2
                    f.head.e.set_reg(&Reg::local(0), Val::Int(888));
                    ()
                })
                .map_err(|e| {
                    println!("error receiving UdpSock bytes: {:?}", e);
                    ()
                });
                * /
            // guard.handle.spawn(fut);
        }
        Err(TryLockError::WouldBlock) => {
            return Event::Uneventful(f);
        }
        Err(TryLockError::Poisoned(ref p)) => {
            panic!("socket lock is poisoned");
        }
    }
    */
    f.head.parent.set_result(Val::Int(0));
    Event::success(f)
}

pub fn udp_send(mut f: Fiber) -> Event
{
    vout!("udp_send.e = {:?}\n", f.head.e);
    let sockr2;

    let sock_result = {
        let sockr = f.head.e.get_param(0);
        vout!("sockr: {:?}\n", sockr);
        sockr2 = sockr.clone();

        let opt_mutex = sockr.libval_as();
        let mutex_sock: &Mutex<UdpSock> = opt_mutex.unwrap();
        // let sock_lock_r: TryLockResult<MutexGuard> = mutex_sock.try_lock();
        let sock_lock_result = mutex_sock.try_lock();
        sock_lock_result.map(|ref mut guard| {
            (guard.socket.take().unwrap(), guard.handle.clone())
        })
        .map_err(|err| {
            match err {
                TryLockError::WouldBlock => true,
                TryLockError::Poisoned(_) => {
                    panic!("socket mutex is poisoned");
                }
            }
        })
    };

    if sock_result.is_err() {
        return Event::Uneventful(f)
    }
    let (sock, handle) = sock_result.unwrap();

    let fut = {
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

        sock.send_dgram(omsg, send_addr)
    };

    {
        let hfut = fut.map(move |(used_sock, buf)| {
            // put the used sock back in the value
            let msock2: &Mutex<UdpSock> = sockr2.libval_as().unwrap();
            if let Ok(ref mut g) = msock2.lock() {
                g.socket = Some(used_sock);
            }
            vout!("send_dgram sent from fiber: {}\n", f.fiber_id);
            ()
        })
        .map_err(|e| {
            println!("send_dgram error: {:?}", e);
            ()
        });
        handle.spawn(hfut);
    }

    /*
        let sock = guard.socket.take().unwrap();
        let fut = sock.send_dgram(omsg, send_addr)
        guard.handle.spawn(fut);
    f.head.parent.set_result(Val::Int(0));
        */
    Event::IOWait
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

