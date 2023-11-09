use std::net::SocketAddr;

use libp2p::Multiaddr;
use mithril_common::{
    messages::RegisterSignatureMessage,
    test_utils::test_http_server::{test_http_server_with_port, TestHttpServer},
    StdResult,
};
use slog_scope::{debug, info};
use tokio::sync::mpsc::{self, unbounded_channel};
use warp::Filter;

use crate::peer::{Peer, PeerEvent};

pub fn aggregator_endpoint() -> String {
    "https://aggregator.testing-preview.api.mithril.network/aggregator".to_string()
}

pub struct Relay {
    server: TestHttpServer,
    pub peer: Peer,
    pub message_rx: mpsc::UnboundedReceiver<RegisterSignatureMessage>,
}

impl Relay {
    pub async fn start(topic_name: &str, address: &Multiaddr) -> StdResult<Self> {
        debug!("Relay: starting...");
        let (tx, rx) = unbounded_channel::<RegisterSignatureMessage>();
        let peer = Peer::new(topic_name, address).start().await?;
        let port = 3132;
        let aggregator_endpoint =
            "https://aggregator.testing-preview.api.mithril.network/aggregator".to_string();
        let server = test_http_server_with_port(
            warp::path("register-signatures")
                .and(warp::post())
                .and(warp::body::json())
                .and(middlewares::with_transmitter(tx))
                .and_then(handlers::register_signatures_handler)
                .or(warp::path("register-signer")
                    .and(warp::post())
                    .and(warp::body::json())
                    .and(middlewares::with_aggregator_endpoint(
                        aggregator_endpoint.clone(),
                    ))
                    .and_then(handlers::register_signer_handler))
                .or(warp::path("epoch-settings")
                    .and(warp::get())
                    .and(middlewares::with_aggregator_endpoint(
                        aggregator_endpoint.clone(),
                    ))
                    .and_then(handlers::epoch_settings_handler))
                .or(warp::path("certificate-pending")
                    .and(warp::get())
                    .and(middlewares::with_aggregator_endpoint(
                        aggregator_endpoint.clone(),
                    ))
                    .and_then(handlers::certificate_pending_handler)),
            port,
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
        self.peer.addr_peer.to_owned()
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

mod middlewares {
    use std::convert::Infallible;
    use tokio::sync::mpsc::UnboundedSender;
    use warp::Filter;

    pub fn with_transmitter<T: Send + Sync>(
        tx: UnboundedSender<T>,
    ) -> impl Filter<Extract = (UnboundedSender<T>,), Error = Infallible> + Clone {
        warp::any().map(move || tx.clone())
    }

    pub fn with_aggregator_endpoint(
        aggregator_endpoint: String,
    ) -> impl Filter<Extract = (String,), Error = Infallible> + Clone {
        warp::any().map(move || aggregator_endpoint.clone())
    }
}

mod handlers {
    use mithril_common::messages::{RegisterSignatureMessage, RegisterSignerMessage};
    use reqwest::{Error, Response, StatusCode};
    use slog_scope::debug;
    use std::convert::Infallible;
    use tokio::sync::mpsc::UnboundedSender;

    pub async fn register_signer_handler(
        register_signer_message: RegisterSignerMessage,
        aggregator_endpoint: String,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("Relay: serve HTTP route /register-signer"; "register_signer_message" => format!("{register_signer_message:#?}"));
        let response = reqwest::Client::new()
            .post(format!("{aggregator_endpoint}/register-signer"))
            .json(&register_signer_message)
            .send()
            .await;
        reply_response(response).await
    }

    pub async fn register_signatures_handler(
        register_signature_message: RegisterSignatureMessage,
        tx: UnboundedSender<RegisterSignatureMessage>,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("Relay: serve HTTP route /register-signatures"; "register_signature_message" => format!("{register_signature_message:#?}"));
        match tx.send(register_signature_message) {
            Ok(_) => Ok(Box::new(warp::reply::with_status(
                "".to_string(),
                StatusCode::CREATED,
            ))),
            Err(err) => Ok(Box::new(warp::reply::with_status(
                format!("{err:?}"),
                StatusCode::INTERNAL_SERVER_ERROR,
            ))),
        }
    }

    pub async fn epoch_settings_handler(
        aggregator_endpoint: String,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("Relay: serve HTTP route /epoch-settings");
        let response = reqwest::Client::new()
            .get(format!("{aggregator_endpoint}/epoch-settings"))
            .send()
            .await;
        reply_response(response).await
    }

    pub async fn certificate_pending_handler(
        aggregator_endpoint: String,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("Relay: serve HTTP route /certificate-pending");
        let response = reqwest::Client::new()
            .get(format!("{aggregator_endpoint}/certificate-pending"))
            .send()
            .await;
        reply_response(response).await
    }

    pub async fn reply_response(
        response: Result<Response, Error>,
    ) -> Result<impl warp::Reply, Infallible> {
        match response {
            Ok(response) => {
                let status = response.status().to_owned();
                let content = response.text().await.as_ref().unwrap().to_owned();
                Ok(Box::new(warp::reply::with_status(content, status)))
            }
            Err(err) => Ok(Box::new(warp::reply::with_status(
                format!("{err:?}"),
                StatusCode::INTERNAL_SERVER_ERROR,
            ))),
        }
    }
}
