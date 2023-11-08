use std::net::SocketAddr;

use libp2p::Multiaddr;
use mithril_common::{
    messages::RegisterSignatureMessage,
    test_utils::test_http_server::{test_http_server, TestHttpServer},
    StdResult,
};
use reqwest::StatusCode;
use slog_scope::{debug, info};
use tokio::sync::mpsc::{self};
use warp::Filter;

use crate::peer::{Peer, PeerEvent};

pub struct Relay {
    server: TestHttpServer,
    pub peer: Peer,
    pub message_rx: mpsc::UnboundedReceiver<RegisterSignatureMessage>,
}

impl Relay {
    pub async fn start(topic_name: &str) -> StdResult<Self> {
        debug!("Relay: starting...");
        let (tx, rx) = mpsc::unbounded_channel::<RegisterSignatureMessage>();
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
        info!("Relay: listening on"; "address" => format!("{:?}", server.address()));

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

    pub async fn tick(&mut self) -> StdResult<Option<PeerEvent>> {
        tokio::select! {
            message = self.message_rx.recv()  => {
                match message {
                    Some(signature_message) => {
                        info!("Relay: publish signature to p2p network"; "message" => format!("{signature_message:#?}"));
                        self.peer.publish(&signature_message)?;
                        Ok(None)
                    }
                    None => {
                        debug!("Relay: no message available");
                        Ok(None)
                    }
                }
            },
            event =  self.peer.tick_swarm() => {event}
        }
    }
}
