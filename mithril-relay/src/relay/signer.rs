use crate::{
    p2p::{Peer, PeerEvent},
    repeater::MessageRepeater,
};
use libp2p::Multiaddr;
use mithril_common::{
    logging::LoggerExtensions,
    messages::{RegisterSignatureMessage, RegisterSignerMessage},
    test_utils::test_http_server::{test_http_server_with_socket_address, TestHttpServer},
    StdResult,
};
use slog::{debug, info, Logger};
use std::{net::SocketAddr, sync::Arc, time::Duration};
use tokio::sync::mpsc::{unbounded_channel, UnboundedReceiver, UnboundedSender};
use warp::Filter;

/// A relay for a Mithril signer
pub struct SignerRelay {
    server: TestHttpServer,
    peer: Peer,
    signature_rx: UnboundedReceiver<RegisterSignatureMessage>,
    signer_rx: UnboundedReceiver<RegisterSignerMessage>,
    signer_repeater: Arc<MessageRepeater<RegisterSignerMessage>>,
    logger: Logger,
}

impl SignerRelay {
    /// Start a relay for a Mithril signer
    pub async fn start(
        address: &Multiaddr,
        server_port: &u16,
        aggregator_endpoint: &str,
        signer_repeater_delay: &Duration,
        logger: &Logger,
    ) -> StdResult<Self> {
        let relay_logger = logger.new_with_component_name::<Self>();
        debug!(relay_logger, "Starting...");
        let (signature_tx, signature_rx) = unbounded_channel::<RegisterSignatureMessage>();
        let (signer_tx, signer_rx) = unbounded_channel::<RegisterSignerMessage>();
        let signer_repeater = Arc::new(MessageRepeater::new(
            signer_tx.clone(),
            signer_repeater_delay.to_owned(),
            logger,
        ));
        let peer = Peer::new(address).start().await?;
        let server = Self::start_http_server(
            server_port,
            aggregator_endpoint,
            signer_tx,
            signature_tx,
            signer_repeater.clone(),
            logger,
        )
        .await;
        info!(relay_logger, "Listening on"; "address" => ?server.address());

        Ok(Self {
            server,
            peer,
            signature_rx,
            signer_rx,
            signer_repeater,
            logger: relay_logger,
        })
    }

    async fn start_http_server(
        server_port: &u16,
        aggregator_endpoint: &str,
        signer_tx: UnboundedSender<RegisterSignerMessage>,
        signature_tx: UnboundedSender<RegisterSignatureMessage>,
        signer_repeater: Arc<MessageRepeater<RegisterSignerMessage>>,
        logger: &Logger,
    ) -> TestHttpServer {
        let server_logger = logger.new_with_name("http_server");
        test_http_server_with_socket_address(
            warp::path::end()
                .and(warp::get())
                .and(middlewares::with_logger(&server_logger))
                .and(middlewares::with_aggregator_endpoint(
                    aggregator_endpoint.to_string(),
                ))
                .and_then(handlers::aggregator_features_handler)
                .or(warp::path("register-signatures")
                    .and(warp::post())
                    .and(warp::body::json())
                    .and(middlewares::with_logger(&server_logger))
                    .and(middlewares::with_transmitter(signature_tx))
                    .and_then(handlers::register_signatures_handler))
                .or(warp::path("register-signer")
                    .and(warp::post())
                    .and(warp::body::json())
                    .and(middlewares::with_logger(&server_logger))
                    .and(middlewares::with_transmitter(signer_tx))
                    .and(middlewares::with_repeater(signer_repeater.clone()))
                    .and_then(handlers::register_signer_handler))
                .or(warp::path("epoch-settings")
                    .and(warp::get())
                    .and(middlewares::with_logger(&server_logger))
                    .and(middlewares::with_aggregator_endpoint(
                        aggregator_endpoint.to_string(),
                    ))
                    .and_then(handlers::epoch_settings_handler))
                .or(warp::path("certificate-pending")
                    .and(warp::get())
                    .and(middlewares::with_logger(&server_logger))
                    .and(middlewares::with_aggregator_endpoint(
                        aggregator_endpoint.to_string(),
                    ))
                    .and_then(handlers::certificate_pending_handler)),
            ([0, 0, 0, 0], *server_port).into(),
        )
    }

    /// Tick the signer relay
    pub async fn tick(&mut self) -> StdResult<()> {
        tokio::select! {
            message = self.signature_rx.recv()  => {
                match message {
                    Some(signature_message) => {
                        info!(self.logger, "Publish signature to p2p network"; "message" => #?signature_message);
                        self.peer.publish_signature(&signature_message)?;
                        Ok(())
                    }
                    None => {
                        debug!(self.logger, "No signature message available");
                        Ok(())
                    }
                }
            },
            message = self.signer_rx.recv()  => {
                match message {
                    Some(signer_message) => {
                        info!(self.logger, "Publish signer-registration to p2p network"; "message" => #?signer_message);
                        self.peer.publish_signer_registration(&signer_message)?;
                        Ok(())
                    }
                    None => {
                        debug!(self.logger, "No signer message available");
                        Ok(())
                    }
                }
            },
            _ = self.signer_repeater.repeat_message() => {Ok(())},
            _event =  self.peer.tick_swarm() => {Ok(())}
        }
    }

    /// Receive signature from the underlying channel
    #[allow(dead_code)]
    pub async fn receive_signature(&mut self) -> Option<RegisterSignatureMessage> {
        self.signature_rx.recv().await
    }

    /// Tick the peer of the signer relay
    pub async fn tick_peer(&mut self) -> StdResult<Option<PeerEvent>> {
        self.peer.tick_swarm().await
    }

    /// Connect to a remote peer
    pub fn dial_peer(&mut self, addr: Multiaddr) -> StdResult<()> {
        self.peer.dial(addr)
    }

    /// Retrieve address on which the HTTP Server is listening
    pub fn address(&self) -> SocketAddr {
        self.server.address()
    }

    /// Retrieve address on which the peer is listening
    pub fn peer_address(&self) -> Option<Multiaddr> {
        self.peer.addr_peer.to_owned()
    }
}

mod middlewares {
    use std::{convert::Infallible, fmt::Debug, sync::Arc};
    use tokio::sync::mpsc::UnboundedSender;
    use warp::Filter;

    use crate::repeater::MessageRepeater;

    pub fn with_logger(
        logger: &slog::Logger,
    ) -> impl Filter<Extract = (slog::Logger,), Error = Infallible> + Clone {
        let logger = logger.clone();
        warp::any().map(move || logger.clone())
    }

    pub fn with_transmitter<T: Send + Sync>(
        tx: UnboundedSender<T>,
    ) -> impl Filter<Extract = (UnboundedSender<T>,), Error = Infallible> + Clone {
        warp::any().map(move || tx.clone())
    }

    pub fn with_repeater<M: Clone + Debug + Sync + Send + 'static>(
        repeater: Arc<MessageRepeater<M>>,
    ) -> impl Filter<Extract = (Arc<MessageRepeater<M>>,), Error = Infallible> + Clone {
        warp::any().map(move || repeater.clone())
    }

    pub fn with_aggregator_endpoint(
        aggregator_endpoint: String,
    ) -> impl Filter<Extract = (String,), Error = Infallible> + Clone {
        warp::any().map(move || aggregator_endpoint.clone())
    }
}

mod handlers {
    use mithril_common::messages::{RegisterSignatureMessage, RegisterSignerMessage};
    use reqwest::{Error, Response};
    use slog::{debug, Logger};
    use std::{convert::Infallible, sync::Arc};
    use tokio::sync::mpsc::UnboundedSender;
    use warp::http::StatusCode;

    use crate::repeater;

    pub async fn aggregator_features_handler(
        logger: Logger,
        aggregator_endpoint: String,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!(logger, "Serve HTTP route /");
        let response = reqwest::Client::new()
            .get(format!("{aggregator_endpoint}/"))
            .send()
            .await;
        reply_response(logger, response).await
    }

    pub async fn register_signer_handler(
        register_signer_message: RegisterSignerMessage,
        logger: Logger,
        tx: UnboundedSender<RegisterSignerMessage>,
        repeater: Arc<repeater::MessageRepeater<RegisterSignerMessage>>,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!(logger, "Serve HTTP route /register-signer"; "register_signer_message" => #?register_signer_message);

        repeater.set_message(register_signer_message.clone()).await;
        match tx.send(register_signer_message) {
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

    pub async fn register_signatures_handler(
        register_signature_message: RegisterSignatureMessage,
        logger: Logger,
        tx: UnboundedSender<RegisterSignatureMessage>,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!(logger, "Serve HTTP route /register-signatures"; "register_signature_message" => #?register_signature_message);
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
        logger: Logger,
        aggregator_endpoint: String,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!(logger, "Serve HTTP route /epoch-settings");
        let response = reqwest::Client::new()
            .get(format!("{aggregator_endpoint}/epoch-settings"))
            .send()
            .await;
        reply_response(logger, response).await
    }

    pub async fn certificate_pending_handler(
        logger: Logger,
        aggregator_endpoint: String,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!(logger, "Serve HTTP route /certificate-pending");
        let response = reqwest::Client::new()
            .get(format!("{aggregator_endpoint}/certificate-pending"))
            .send()
            .await;
        reply_response(logger, response).await
    }

    pub async fn reply_response(
        logger: Logger,
        response: Result<Response, Error>,
    ) -> Result<impl warp::Reply, Infallible> {
        match response {
            Ok(response) => match StatusCode::from_u16(response.status().into()) {
                Ok(status) => match response.text().await {
                    Ok(content) => {
                        debug!(logger, "Received response with status '{status}'"; "content" => &content);

                        Ok(Box::new(warp::reply::with_status(content, status)))
                    }
                    Err(err) => {
                        debug!(logger, "Received error"; "error" => ?err);
                        Ok(Box::new(warp::reply::with_status(
                            format!("{err:?}"),
                            StatusCode::INTERNAL_SERVER_ERROR,
                        )))
                    }
                },
                Err(err) => {
                    debug!(logger, "Failed to parse the returned status"; "error" => ?err);
                    Ok(Box::new(warp::reply::with_status(
                        format!("{err:?}"),
                        StatusCode::INTERNAL_SERVER_ERROR,
                    )))
                }
            },
            Err(err) => {
                debug!(logger, "Received error"; "error" => ?err);
                Ok(Box::new(warp::reply::with_status(
                    format!("{err:?}"),
                    StatusCode::INTERNAL_SERVER_ERROR,
                )))
            }
        }
    }
}
