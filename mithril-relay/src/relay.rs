use std::{
    net::SocketAddr,
    sync::{Arc, Mutex},
};

use mithril_common::{
    messages::RegisterSignatureMessage,
    test_utils::test_http_server::{test_http_server, TestHttpServer},
    StdResult,
};
use reqwest::StatusCode;
use warp::Filter;

use crate::peer::Peer;

pub struct Relay {
    server: TestHttpServer,
}

impl Relay {
    pub async fn start(topic_name: &str) -> StdResult<Self> {
        let peer = Arc::new(Mutex::new(Peer::new(topic_name).start()?));

        let server = test_http_server(warp::path("register-signatures").and(warp::post()).map(
            move || {
                let mut peer = peer.lock().unwrap();
                peer.publish(&RegisterSignatureMessage::dummy()).unwrap();

                warp::reply::with_status(warp::reply::reply(), StatusCode::CREATED)
            },
        ));

        Ok(Self { server })
    }

    pub fn address(&self) -> SocketAddr {
        self.server.address()
    }
}
