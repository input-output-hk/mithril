use std::{
    net::SocketAddr,
    sync::{Arc, Mutex},
};

use libp2p::Multiaddr;
use mithril_common::{
    messages::RegisterSignatureMessage,
    test_utils::test_http_server::{test_http_server, TestHttpServer},
    StdResult,
};
use reqwest::StatusCode;
use warp::Filter;

use crate::peer::{Peer, PeerEvent};

pub struct Relay {
    server: TestHttpServer,
    pub peer: Arc<Mutex<Peer>>,
}

impl Relay {
    pub async fn start(topic_name: &str) -> StdResult<Self> {
        let peer = Arc::new(Mutex::new(Peer::new(topic_name).start().await?));
        let peer_reference = peer.clone();
        let server = test_http_server(warp::path("register-signatures").and(warp::post()).map(
            move || {
                let mut peer = peer.lock().unwrap();
                peer.publish(&RegisterSignatureMessage::dummy()).unwrap();

                warp::reply::with_status(warp::reply::reply(), StatusCode::CREATED)
            },
        ));

        Ok(Self {
            server,
            peer: peer_reference,
        })
    }

    pub fn address(&self) -> SocketAddr {
        self.server.address()
    }

    pub fn peer_address(&self) -> Option<Multiaddr> {
        let peer = self.peer.lock().unwrap();
        peer.addr.to_owned()
    }

    pub async fn tick_peer(&mut self) -> StdResult<Option<PeerEvent>> {
        let mut peer = self.peer.lock().unwrap();
        peer.tick_swarm().await
    }
}
