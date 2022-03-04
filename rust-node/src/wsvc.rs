use std::collections::HashMap;
use std::sync::{mpsc};
use std::thread;
use std::time::Duration;
use crate::network::Network;
use crate::message::{Message, PartyId};
use serde::Serialize;
use ureq;
use rouille;
use threadpool;

fn send<T:Serialize>(url: String, msg: T) {
  loop {
    let response = ureq::post(&url).send_json(&msg);

    match response {
      Ok(r) if r.status() == 200 => {
        return;
      }
      _ => {
        // retry after waiting a couple seconds
        thread::sleep(Duration::from_secs(5));
      }
    }
  }
}

fn output_thread(reciever: mpsc::Receiver<Message>, peers: Vec<String>) {

  let pool = threadpool::Builder::new().build();

  loop {
    let mut sends = Vec::new();
    match reciever.recv() {
      Ok(msg) => {
        for url in &peers {
          let u = url.clone();
          let m = msg.clone();
          let req = pool.execute(|| send(u, m));
          sends.push(req);
        }
      }
      Err(_) => break // TODO: is this ok?
    }

    pool.join();
  }
}

pub struct RestNetwork {
  party_id: PartyId,
  peers: Vec<PartyId>,
  server_thread: thread::JoinHandle<()>,
  client_thread: thread::JoinHandle<()>,
  input: mpsc::Receiver<(PartyId, Message)>,
  output: mpsc::Sender<Message>
}

impl Network for RestNetwork {
    fn me(&self) -> PartyId {
      self.party_id
    }

    fn peers(&self) -> Vec<PartyId> {
      self.peers.clone()
    }

    fn send(&self, message: &Message) -> Result<(), String> {
      self.output.send(message.clone()).map_err(|_| "send failed".to_string())
    }

    fn recv(&self) -> Result<(PartyId, Message), String> {
      self.input.recv().map_err(|_| "recv failed".to_string())
    }

    fn recv_timeout(&self, timeout: std::time::Duration) -> Result<(PartyId, Message), String> {
      self.input.recv_timeout(timeout).map_err(|_| "recv_timeout failed".to_string())
    }
}

fn url_with_from(url: &String, from: PartyId) -> String {
  if url.ends_with("/") {
    format!("{}{}", url, from)
  } else {
    format!("{}/{}", url, from)
  }
}

pub fn start_ws_network(addr: &String, peers: &HashMap<PartyId, String>, my_id: PartyId) -> std::io::Result<RestNetwork> {

  let (input_s, input_r) = mpsc::channel();
  let (output_s, output_r) = mpsc::channel();

  let s_addr = addr.clone();
  let input_s_mutex = std::sync::Mutex::new(input_s);
  let srv =
    std::thread::spawn(|| {
    rouille::start_server(s_addr, move |req| {
      let s = {
        let l = input_s_mutex.lock().unwrap();
        l.clone()
      };

      rouille::router!(req,
        (POST) (/{u:PartyId}) => {
          let msg: Message = rouille::try_or_400!(rouille::input::json_input(req));
          s.send((u, msg)).unwrap();
          rouille::Response::text("hello")
        },
        _ => {
          rouille::Response::empty_404()
        }
      )
    });
  });

  // append our PID to peer URLs
  let c_peers: Vec<String> = peers.values().map(|addr| url_with_from(addr, my_id)).collect();
  // let client = ;
  let client_thread =
    thread::spawn(|| output_thread(output_r, c_peers));

  Ok(RestNetwork {
    party_id: my_id,
    peers: peers.keys().copied().collect(),
    input: input_r,
    output: output_s,
    client_thread: client_thread,
    server_thread: srv,
  })
}