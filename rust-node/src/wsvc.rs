use actix_web::dev::Server;
use actix_web::{web, App, Responder, HttpResponse};
use awc::Client;
use std::collections::HashMap;
use std::sync::{mpsc};
use crate::network::Network;
use crate::message::{Message, PartyId};

use futures::executor::block_on;
use std::thread;
use async_std;

#[derive(Clone)]
struct ServerContext {
  input_sender : mpsc::Sender<(PartyId, Message)>,
}

async fn send(url: String, msg: Message) {
  let client = Client::default();
  loop {
    let response = client.post(&url)
          .send_json(&msg)
          .await;

    match response {
      Ok(r) if r.status().is_success() => return,
      _ => {
        // retry after waiting a couple seconds
        async_std::task::sleep(std::time::Duration::from_secs(10)).await;
      }
    }
  }
}

fn output_thread(reciever: mpsc::Receiver<Message>, peers: Vec<String>) {
  loop {
    let mut sends = Vec::new();
    match reciever.recv() {
      Ok(msg) => {
        for url in &peers {
          let req = send(url.clone(), msg.clone());
          sends.push(req);
        }
      }
      Err(_) => break // TODO: is this ok?
    }

    for s in sends {
      let _ = block_on(s);
    }
  }
}

async fn message(ctx: web::Data<ServerContext>, path: web::Path<PartyId>, msg: web::Json<Message>) -> impl Responder {
  match ctx.get_ref().input_sender.send((path.into_inner(), msg.into_inner())) {
    Ok(_) => HttpResponse::Ok().finish(),
    Err(_) => HttpResponse::InternalServerError().finish(),
  }

}

pub struct RestNetwork {
  party_id: PartyId,
  peers: Vec<PartyId>,
  server: Server,
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

  let srv =
    actix_web::HttpServer::new(move || {
      let srv_ctx = ServerContext {
        input_sender: input_s.clone(),
      };

      App::new()
        .app_data(srv_ctx)
        .route("/{from}", web::get().to(message))
    })
    .bind(addr)?
    .run();

  // append our PID to peer URLs
  let c_peers: Vec<String> = peers.values().map(|addr| url_with_from(addr, my_id)).collect();
  let client = std::thread::spawn(move || output_thread(output_r, c_peers));

  Ok(RestNetwork {
    party_id: my_id,
    peers: peers.keys().copied().collect(),
    input: input_r,
    output: output_s,
    client_thread: client,
    server: srv,
  })
}