#[cfg(feature = "future_dmq")]
use std::path::Path;
use std::{net::SocketAddr, sync::Arc, time::Duration};

use anyhow::anyhow;
use clap::ValueEnum;
use libp2p::Multiaddr;
#[cfg(feature = "future_dmq")]
use slog::error;
use slog::{Logger, debug, info};
use strum::Display;
use tokio::sync::mpsc::{UnboundedReceiver, UnboundedSender, unbounded_channel};
#[cfg(feature = "future_dmq")]
use tokio::sync::watch::{self, Receiver, Sender};
use warp::Filter;

#[cfg(feature = "future_dmq")]
use mithril_common::CardanoNetwork;
use mithril_common::{
    StdResult,
    logging::LoggerExtensions,
    messages::{RegisterSignatureMessageHttp, RegisterSignerMessage},
};
#[cfg(feature = "future_dmq")]
use mithril_dmq::{DmqMessage, DmqPublisherServer, DmqPublisherServerPallas};
use mithril_test_http_server::{TestHttpServer, test_http_server_with_socket_address};

use crate::{
    p2p::{Peer, PeerEvent},
    repeater::MessageRepeater,
};

/// Signer relay mode
///
/// The relay mode defines how the relay will behave when it receives a message
#[derive(Debug, Clone, Display, PartialEq, Eq, ValueEnum)]
#[strum(serialize_all = "mixed_case")]
pub enum SignerRelayMode {
    /// Passthrough relay mode
    ///
    /// In this mode, the relay will only call the aggregator with the message received
    Passthrough,
    /// P2P relay mode
    ///
    /// In this mode, the relay will publish the message received to the P2P network
    P2P,
}

struct HTTPServerConfiguration<'a> {
    server_port: &'a u16,
    signer_registration_mode: SignerRelayMode,
    signature_registration_mode: SignerRelayMode,
    aggregator_endpoint: &'a str,
    signer_tx: UnboundedSender<RegisterSignerMessage>,
    signature_tx: UnboundedSender<RegisterSignatureMessageHttp>,
    signer_repeater: Arc<MessageRepeater<RegisterSignerMessage>>,
    logger: &'a Logger,
}

/// A relay for a Mithril signer
pub struct SignerRelay {
    http_server: TestHttpServer,
    peer: Peer,
    signature_http_rx: UnboundedReceiver<RegisterSignatureMessageHttp>,
    #[cfg(feature = "future_dmq")]
    signature_dmq_rx: UnboundedReceiver<DmqMessage>,
    signer_http_rx: UnboundedReceiver<RegisterSignerMessage>,
    signer_repeater: Arc<MessageRepeater<RegisterSignerMessage>>,
    #[cfg(feature = "future_dmq")]
    stop_tx: Sender<()>,
    logger: Logger,
}

impl SignerRelay {
    /// Start a relay for a Mithril signer
    pub async fn start(
        address: &Multiaddr,
        server_port: &u16,
        #[cfg(feature = "future_dmq")] dmq_node_socket_path: &Path,
        #[cfg(feature = "future_dmq")] cardano_network: &CardanoNetwork,
        signer_registration_mode: &SignerRelayMode,
        signature_registration_mode: &SignerRelayMode,
        aggregator_endpoint: &str,
        signer_repeater_delay: &Duration,
        logger: &Logger,
    ) -> StdResult<Self> {
        let relay_logger = logger.new_with_component_name::<Self>();
        debug!(relay_logger, "Starting..."; "signer_registration_mode" => ?signer_registration_mode, "signature_registration_mode" => ?signature_registration_mode);
        let (signature_tx, signature_rx) = unbounded_channel::<RegisterSignatureMessageHttp>();
        let (signer_tx, signer_rx) = unbounded_channel::<RegisterSignerMessage>();
        let signer_repeater = Arc::new(MessageRepeater::new(
            signer_tx.clone(),
            signer_repeater_delay.to_owned(),
            logger,
        ));
        let peer = Peer::new(address).start().await?;
        let http_server = Self::start_http_server(&HTTPServerConfiguration {
            server_port,
            signer_registration_mode: signer_registration_mode.to_owned(),
            signature_registration_mode: signature_registration_mode.to_owned(),
            aggregator_endpoint,
            signer_tx: signer_tx.clone(),
            signature_tx: signature_tx.clone(),
            signer_repeater: signer_repeater.clone(),
            logger: &relay_logger,
        })
        .await;
        info!(relay_logger, "Listening on"; "address" => ?http_server.address());

        #[cfg(feature = "future_dmq")]
        {
            let (stop_tx, stop_rx) = watch::channel(());
            let (signature_dmq_tx, signature_dmq_rx) = unbounded_channel::<DmqMessage>();
            #[cfg(unix)]
            let _dmq_publisher_server = Self::start_dmq_publisher_server(
                dmq_node_socket_path,
                cardano_network,
                signature_dmq_tx,
                stop_rx,
                relay_logger.clone(),
            )
            .await?;

            Ok(Self {
                http_server,
                peer,
                signature_http_rx: signature_rx,
                signature_dmq_rx,
                signer_http_rx: signer_rx,
                signer_repeater,
                stop_tx,
                logger: relay_logger,
            })
        }
        #[cfg(not(feature = "future_dmq"))]
        Ok(Self {
            http_server,
            peer,
            signature_http_rx: signature_rx,
            signer_http_rx: signer_rx,
            signer_repeater,
            logger: relay_logger,
        })
    }

    /// Stop the signer relay
    pub async fn stop(&self) -> StdResult<()> {
        #[cfg(feature = "future_dmq")]
        self.stop_tx
            .send(())
            .map_err(|e| anyhow!("Failed to send stop signal to DMQ publisher server: {e}"))?;

        Ok(())
    }

    #[cfg(feature = "future_dmq")]
    async fn start_dmq_publisher_server(
        socket: &Path,
        cardano_network: &CardanoNetwork,
        signature_dmq_tx: UnboundedSender<DmqMessage>,
        stop_rx: Receiver<()>,
        logger: Logger,
    ) -> StdResult<Arc<DmqPublisherServerPallas>> {
        let dmq_publisher_server = Arc::new(DmqPublisherServerPallas::new(
            socket.to_path_buf(),
            cardano_network.to_owned(),
            stop_rx,
            logger.clone(),
        ));
        dmq_publisher_server.register_transmitter(signature_dmq_tx).await?;
        let dmq_publisher_server_clone = dmq_publisher_server.clone();
        tokio::spawn(async move {
            if let Err(err) = dmq_publisher_server_clone.run().await {
                error!(logger.to_owned(), "DMQ Publisher server failed"; "error" => ?err);
            }
        });

        Ok(dmq_publisher_server)
    }

    async fn start_http_server(configuration: &HTTPServerConfiguration<'_>) -> TestHttpServer {
        let server_logger = configuration.logger.new_with_name("http_server");
        test_http_server_with_socket_address(
            warp::path::end()
                .and(warp::get())
                .and(middlewares::with_logger(&server_logger))
                .and(middlewares::with_aggregator_endpoint(
                    configuration.aggregator_endpoint.to_string(),
                ))
                .and_then(handlers::aggregator_features_handler)
                .or(warp::path("register-signatures")
                    .and(warp::post())
                    .and(warp::body::json())
                    .and(middlewares::with_signer_relay_mode(
                        configuration.signature_registration_mode.clone(),
                    ))
                    .and(middlewares::with_aggregator_endpoint(
                        configuration.aggregator_endpoint.to_string(),
                    ))
                    .and(middlewares::with_logger(&server_logger))
                    .and(middlewares::with_transmitter(
                        configuration.signature_tx.clone(),
                    ))
                    .and_then(handlers::register_signatures_handler))
                .or(warp::path("register-signer")
                    .and(warp::post())
                    .and(warp::body::json())
                    .and(middlewares::with_signer_relay_mode(
                        configuration.signer_registration_mode.clone(),
                    ))
                    .and(middlewares::with_aggregator_endpoint(
                        configuration.aggregator_endpoint.to_string(),
                    ))
                    .and(middlewares::with_logger(&server_logger))
                    .and(middlewares::with_transmitter(
                        configuration.signer_tx.clone(),
                    ))
                    .and(middlewares::with_repeater(
                        configuration.signer_repeater.clone(),
                    ))
                    .and_then(handlers::register_signer_handler))
                .or(warp::path("epoch-settings")
                    .and(warp::get())
                    .and(middlewares::with_logger(&server_logger))
                    .and(middlewares::with_aggregator_endpoint(
                        configuration.aggregator_endpoint.to_string(),
                    ))
                    .and_then(handlers::epoch_settings_handler)),
            ([0, 0, 0, 0], *configuration.server_port).into(),
        )
    }

    fn process_register_signature_http_message(
        &mut self,
        message: Option<RegisterSignatureMessageHttp>,
    ) -> StdResult<()> {
        match message {
            Some(signature_message) => {
                info!(self.logger, "Publish HTTP signature to p2p network"; "message" => #?signature_message);
                self.peer.publish_signature_http(&signature_message)?;

                Ok(())
            }
            None => {
                debug!(self.logger, "No HTTP signature message available");

                Ok(())
            }
        }
    }

    fn process_register_signer_http_message(
        &mut self,
        message: Option<RegisterSignerMessage>,
    ) -> StdResult<()> {
        match message {
            Some(signer_message) => {
                info!(self.logger, "Publish HTTP signer-registration to p2p network"; "message" => #?signer_message);
                self.peer.publish_signer_registration(&signer_message)?;

                Ok(())
            }
            None => {
                debug!(self.logger, "No HTTP signer message available");

                Ok(())
            }
        }
    }

    #[cfg(feature = "future_dmq")]
    fn process_register_signature_dmq_message(
        &mut self,
        message: Option<DmqMessage>,
    ) -> StdResult<()> {
        match message {
            Some(signature_message) => {
                info!(self.logger, "Publish DMQ signature to p2p network"; "message" => #?signature_message);
                self.peer.publish_signature_dmq(&signature_message)?;

                Ok(())
            }
            None => {
                //debug!(self.logger, "No DMQ signature message available");
                Ok(())
            }
        }
    }

    /// Tick the signer relay
    pub async fn tick(&mut self) -> StdResult<()> {
        #[cfg(feature = "future_dmq")]
        tokio::select! {
            message = self.signature_http_rx.recv()  => {
                self.process_register_signature_http_message(message)
            },
            message = self.signer_http_rx.recv()  => {
                self.process_register_signer_http_message(message)
            },
            message = self.signature_dmq_rx.recv()  => {
                self.process_register_signature_dmq_message(message)
            },
            _ = self.signer_repeater.repeat_message() => {Ok(())},
            _event =  self.peer.tick_swarm() => {Ok(())}
        }
        #[cfg(not(feature = "future_dmq"))]
        tokio::select! {
            message = self.signature_http_rx.recv()  => {
                self.process_register_signature_http_message(message)
            },
            message = self.signer_http_rx.recv()  => {
                self.process_register_signer_http_message(message)
            },
            _ = self.signer_repeater.repeat_message() => {Ok(())},
            _event =  self.peer.tick_swarm() => {Ok(())}
        }
    }

    /// Receive signature from the underlying channel
    #[allow(dead_code)]
    pub async fn receive_signature(&mut self) -> Option<RegisterSignatureMessageHttp> {
        self.signature_http_rx.recv().await
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
        self.http_server.address()
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

    use super::SignerRelayMode;

    pub fn with_logger(
        logger: &slog::Logger,
    ) -> impl Filter<Extract = (slog::Logger,), Error = Infallible> + Clone + use<> {
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

    pub fn with_signer_relay_mode(
        signer_relay_mode: SignerRelayMode,
    ) -> impl Filter<Extract = (SignerRelayMode,), Error = Infallible> + Clone {
        warp::any().map(move || signer_relay_mode.clone())
    }
}

mod handlers {
    use mithril_common::messages::{RegisterSignatureMessageHttp, RegisterSignerMessage};
    use reqwest::{Error, Response};
    use slog::{Logger, debug};
    use std::{convert::Infallible, sync::Arc};
    use tokio::sync::mpsc::{UnboundedSender, error::SendError};
    use warp::{http::StatusCode, reply::WithStatus};

    use crate::repeater;

    use super::SignerRelayMode;

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
        signer_relay_mode: SignerRelayMode,
        aggregator_endpoint: String,
        logger: Logger,
        tx: UnboundedSender<RegisterSignerMessage>,
        repeater: Arc<repeater::MessageRepeater<RegisterSignerMessage>>,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!(logger, "Serve HTTP route /register-signer"; "signer_relay_mode" => ?signer_relay_mode, "register_signer_message" => #?register_signer_message,);
        match signer_relay_mode {
            SignerRelayMode::P2P => {
                repeater.set_message(register_signer_message.clone()).await;
                reply_response_from_tx_send_result(tx.send(register_signer_message))
            }
            SignerRelayMode::Passthrough => {
                let response = reqwest::Client::new()
                    .post(format!("{aggregator_endpoint}/register-signer"))
                    .json(&register_signer_message)
                    .send()
                    .await;
                reply_response(logger, response).await
            }
        }
    }

    pub async fn register_signatures_handler(
        register_signature_message: RegisterSignatureMessageHttp,
        signer_relay_mode: SignerRelayMode,
        aggregator_endpoint: String,
        logger: Logger,
        tx: UnboundedSender<RegisterSignatureMessageHttp>,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!(logger, "Serve HTTP route /register-signatures"; "signer_relay_mode" => ?signer_relay_mode, "register_signature_message" => #?register_signature_message);

        match signer_relay_mode {
            SignerRelayMode::P2P => {
                reply_response_from_tx_send_result(tx.send(register_signature_message))
            }
            SignerRelayMode::Passthrough => {
                let response = reqwest::Client::new()
                    .post(format!("{aggregator_endpoint}/register-signatures"))
                    .json(&register_signature_message)
                    .send()
                    .await;
                reply_response(logger, response).await
            }
        }
    }

    fn reply_response_from_tx_send_result<T>(
        result: Result<(), SendError<T>>,
    ) -> Result<Box<WithStatus<String>>, Infallible> {
        match result {
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

    pub async fn reply_response(
        logger: Logger,
        response: Result<Response, Error>,
    ) -> Result<Box<WithStatus<String>>, Infallible> {
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

#[cfg(test)]
mod tests {
    use httpmock::{
        Method::{GET, POST},
        MockServer,
    };
    use mithril_common::test::double::Dummy;
    use tokio::sync::mpsc::error::TryRecvError;

    use crate::{repeater, test_tools::TestLogger};

    use super::*;

    #[tokio::test]
    async fn sends_accept_encoding_header_with_correct_values() {
        let server = MockServer::start();
        let mock = server.mock(|when, then| {
            when.matches(|req| {
                let headers = req.headers.clone().expect("HTTP headers not found");
                let accept_encoding_header = headers
                    .iter()
                    .find(|(name, _values)| name.to_lowercase() == "accept-encoding")
                    .expect("Accept-Encoding header not found");

                let header_value = accept_encoding_header.clone().1;
                ["gzip", "br", "deflate", "zstd"]
                    .iter()
                    .all(|&value| header_value.contains(value))
            });

            then.status(200).body("ok");
        });

        handlers::aggregator_features_handler(TestLogger::stdout(), server.url(""))
            .await
            .unwrap();

        mock.assert();
    }

    #[tokio::test]
    async fn epoch_settings_handler() {
        let test_logger = TestLogger::stdout();
        let server = MockServer::start();
        let mock = server.mock(|when, then| {
            when.method(GET).path("/epoch-settings");
            then.status(201).body("ok");
        });

        handlers::epoch_settings_handler(test_logger, server.url(""))
            .await
            .unwrap();

        mock.assert();
    }

    #[tokio::test]
    async fn register_signer_handler_with_passthrough() {
        let test_logger = TestLogger::stdout();
        let (tx, mut rx) = unbounded_channel::<RegisterSignerMessage>();
        let message = RegisterSignerMessage::dummy();
        let repeater =
            repeater::MessageRepeater::new(tx.clone(), Duration::from_secs(10), &test_logger);
        let server = MockServer::start();
        let mock = server.mock(|when, then| {
            when.method(POST)
                .path("/register-signer")
                .body_contains(serde_json::to_string(&message).unwrap());
            then.status(201).body("ok");
        });

        handlers::register_signer_handler(
            message,
            SignerRelayMode::Passthrough,
            server.url(""),
            test_logger.clone(),
            tx.clone(),
            Arc::new(repeater),
        )
        .await
        .unwrap();

        mock.assert();
        assert_eq!(Err(TryRecvError::Empty), rx.try_recv());
    }

    #[tokio::test]
    async fn register_signer_handler_with_p2p() {
        let test_logger = TestLogger::stdout();
        let (tx, mut rx) = unbounded_channel::<RegisterSignerMessage>();
        let message = RegisterSignerMessage::dummy();
        let repeater =
            repeater::MessageRepeater::new(tx.clone(), Duration::from_secs(10), &test_logger);

        handlers::register_signer_handler(
            message.clone(),
            SignerRelayMode::P2P,
            "unreachable_endpoint".to_string(),
            test_logger.clone(),
            tx.clone(),
            Arc::new(repeater),
        )
        .await
        .unwrap();

        let received_message = rx.recv().await.unwrap();
        assert_eq!(received_message, message);
    }

    #[tokio::test]
    async fn register_signatures_handler_with_passthrough() {
        let test_logger = TestLogger::stdout();
        let (tx, mut rx) = unbounded_channel::<RegisterSignatureMessageHttp>();
        let message = RegisterSignatureMessageHttp::dummy();
        let server = MockServer::start();
        let mock = server.mock(|when, then| {
            when.method(POST)
                .path("/register-signatures")
                .body_contains(serde_json::to_string(&message).unwrap());
            then.status(201).body("ok");
        });

        handlers::register_signatures_handler(
            message,
            SignerRelayMode::Passthrough,
            server.url(""),
            test_logger.clone(),
            tx.clone(),
        )
        .await
        .unwrap();

        mock.assert();
        assert_eq!(Err(TryRecvError::Empty), rx.try_recv());
    }

    #[tokio::test]
    async fn register_signatures_handler_with_p2p() {
        let test_logger = TestLogger::stdout();
        let (tx, mut rx) = unbounded_channel::<RegisterSignatureMessageHttp>();
        let message = RegisterSignatureMessageHttp::dummy();

        handlers::register_signatures_handler(
            message.clone(),
            SignerRelayMode::P2P,
            "unreachable_endpoint".to_string(),
            test_logger.clone(),
            tx.clone(),
        )
        .await
        .unwrap();

        let received_message = rx.recv().await.unwrap();
        assert_eq!(received_message, message);
    }
}
