use crate::peer::{Peer, PeerEvent};
use libp2p::Multiaddr;
use mithril_common::{
    messages::RegisterSignatureMessage,
    test_utils::test_http_server::{test_http_server_with_port, TestHttpServer},
    StdResult,
};
use slog_scope::{debug, info};
use std::net::SocketAddr;
use tokio::sync::mpsc::{unbounded_channel, UnboundedReceiver, UnboundedSender};
use warp::Filter;

pub struct SignerRelay {
    server: TestHttpServer,
    pub peer: Peer,
    pub signature_rx: UnboundedReceiver<RegisterSignatureMessage>,
}

impl SignerRelay {
    pub async fn start(
        topic_name: &str,
        address: &Multiaddr,
        server_port: &u16,
        aggregator_endpoint: &str,
    ) -> StdResult<Self> {
        debug!("SignerRelay: starting...");
        let (signature_tx, signature_rx) = unbounded_channel::<RegisterSignatureMessage>();
        let peer = Peer::new(topic_name, address).start().await?;
        let server = Self::start_http_server(server_port, aggregator_endpoint, signature_tx).await;
        info!("SignerRelay: listening on"; "address" => format!("{:?}", server.address()));

        Ok(Self {
            server,
            peer,
            signature_rx,
        })
    }

    async fn start_http_server(
        server_port: &u16,
        aggregator_endpoint: &str,
        signature_tx: UnboundedSender<RegisterSignatureMessage>,
    ) -> TestHttpServer {
        test_http_server_with_port(
            warp::path("register-signatures")
                .and(warp::post())
                .and(warp::body::json())
                .and(middlewares::with_transmitter(signature_tx))
                .and_then(handlers::register_signatures_handler)
                .or(warp::path("register-signer")
                    .and(warp::post())
                    .and(warp::body::json())
                    .and(middlewares::with_aggregator_endpoint(
                        aggregator_endpoint.to_string(),
                    ))
                    .and_then(handlers::register_signer_handler))
                .or(warp::path("epoch-settings")
                    .and(warp::get())
                    .and(middlewares::with_aggregator_endpoint(
                        aggregator_endpoint.to_string(),
                    ))
                    .and_then(handlers::epoch_settings_handler))
                .or(warp::path("certificate-pending")
                    .and(warp::get())
                    .and(middlewares::with_aggregator_endpoint(
                        aggregator_endpoint.to_string(),
                    ))
                    .and_then(handlers::certificate_pending_handler)),
            *server_port,
        )
    }

    pub async fn tick(&mut self) -> StdResult<()> {
        tokio::select! {
            message = self.signature_rx.recv()  => {
                match message {
                    Some(signature_message) => {
                        info!("SignerRelay: publish signature to p2p network"; "message" => format!("{signature_message:#?}"));
                        self.peer.publish(&signature_message)?;
                        Ok(())
                    }
                    None => {
                        debug!("SignerRelay: no message available");
                        Ok(())
                    }
                }
            },
            _event =  self.peer.tick_swarm() => {Ok(())}
        }
    }

    pub async fn receive_signature(&mut self) -> Option<RegisterSignatureMessage> {
        self.signature_rx.recv().await
    }

    pub async fn tick_peer(&mut self) -> StdResult<Option<PeerEvent>> {
        self.peer.tick_swarm().await
    }

    pub fn dial_peer(&mut self, addr: Multiaddr) -> StdResult<()> {
        self.peer.dial(addr)
    }

    pub fn address(&self) -> SocketAddr {
        self.server.address()
    }

    pub fn peer_address(&self) -> Option<Multiaddr> {
        self.peer.addr_peer.to_owned()
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
        debug!("SignerRelay: serve HTTP route /register-signer"; "register_signer_message" => format!("{register_signer_message:#?}"));
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
        debug!("SignerRelay: serve HTTP route /register-signatures"; "register_signature_message" => format!("{register_signature_message:#?}"));
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
        debug!("SignerRelay: serve HTTP route /epoch-settings");
        let response = reqwest::Client::new()
            .get(format!("{aggregator_endpoint}/epoch-settings"))
            .send()
            .await;
        reply_response(response).await
    }

    pub async fn certificate_pending_handler(
        aggregator_endpoint: String,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("SignerRelay: serve HTTP route /certificate-pending");
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
                debug!(
                    "SignerRelay: received response with status '{status}' and content {content:?}"
                );

                Ok(Box::new(warp::reply::with_status(content, status)))
            }
            Err(err) => {
                debug!("SignerRelay: received error '{err:?}'");
                Ok(Box::new(warp::reply::with_status(
                    format!("{err:?}"),
                    StatusCode::INTERNAL_SERVER_ERROR,
                )))
            }
        }
    }
}
