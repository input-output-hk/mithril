use std::{net::SocketAddr, sync::mpsc::TryRecvError};

use libp2p::Multiaddr;
use mithril_common::{
    messages::RegisterSignatureMessage,
    test_utils::test_http_server::{test_http_server, TestHttpServer},
    StdResult,
};
use reqwest::StatusCode;
use std::sync::mpsc;
use warp::Filter;

use crate::peer::{Peer, PeerEvent};

pub struct Relay {
    server: TestHttpServer,
    pub peer: Peer,
    pub message_rx: mpsc::Receiver<RegisterSignatureMessage>,
}

impl Relay {
    pub async fn start(topic_name: &str) -> StdResult<Self> {
        let (tx, rx) = mpsc::channel::<RegisterSignatureMessage>();
        let peer = Peer::new(topic_name).start().await?;
        let server = test_http_server(
            warp::path("register-signatures")
                .and(warp::post())
                .and(warp::body::json())
                .map(move |signature_message: RegisterSignatureMessage| {
                    tx.send(signature_message).unwrap();
                    warp::reply::with_status(warp::reply::reply(), StatusCode::CREATED)
                }),
        );

        Ok(Self {
            server,
            peer,
            message_rx: rx,
        })
    }

    pub fn address(&self) -> SocketAddr {
        self.server.address()
    }

    pub fn peer_address(&self) -> Option<Multiaddr> {
        self.peer.addr.to_owned()
    }

    pub async fn tick_peer(&mut self) -> StdResult<Option<PeerEvent>> {
        match self.message_rx.try_recv() {
            Ok(signature_message) => {
                println!(" ");
                println!("************************************************");
                println!("Relay publish signature: {signature_message:#?}");
                println!("************************************************");
                println!(" ");
                self.peer.publish(&signature_message)?;
            }
            Err(TryRecvError::Empty) => {
                //println!("No message available");
            }
            Err(error) => {
                panic!("Queue disconnected: {}", error);
            }
        };

        self.peer.tick_swarm().await
    }
}
