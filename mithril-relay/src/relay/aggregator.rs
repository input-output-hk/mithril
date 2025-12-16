use std::{path::Path, sync::Arc};

use anyhow::Context;
use anyhow::anyhow;
use libp2p::Multiaddr;
use reqwest::StatusCode;
use slog::{Logger, error, info};
use tokio::sync::{
    mpsc::{UnboundedReceiver, UnboundedSender, unbounded_channel},
    watch::{self, Receiver, Sender},
};

use mithril_common::{
    StdResult,
    logging::LoggerExtensions,
    messages::{RegisterSignatureMessageHttp, RegisterSignerMessage},
};
use mithril_dmq::{DmqConsumerServer, DmqConsumerServerPallas, DmqMessage, DmqNetwork};

use crate::p2p::{BroadcastMessage, Peer, PeerEvent};

/// A relay for a Mithril aggregator
pub struct AggregatorRelay {
    aggregator_endpoint: String,
    peer: Peer,
    signature_dmq_tx: UnboundedSender<DmqMessage>,
    stop_tx: Sender<()>,
    logger: Logger,
}

impl AggregatorRelay {
    /// Start a relay for a Mithril aggregator
    pub async fn start(
        addr: &Multiaddr,
        dmq_node_socket_path: &Path,
        dmq_network: &DmqNetwork,
        aggregator_endpoint: &str,
        logger: &Logger,
    ) -> StdResult<Self> {
        let peer = Peer::new(addr).with_logger(logger).start().await?;
        let logger = logger.new_with_component_name::<Self>();
        {
            let (stop_tx, stop_rx) = watch::channel(());
            let (signature_dmq_tx, signature_dmq_rx) = unbounded_channel::<DmqMessage>();
            #[cfg(unix)]
            let _dmq_consumer_server = Self::start_dmq_consumer_server(
                dmq_node_socket_path,
                dmq_network,
                signature_dmq_rx,
                stop_rx,
                logger.clone(),
            )
            .await?;

            Ok(Self {
                aggregator_endpoint: aggregator_endpoint.to_owned(),
                peer,
                signature_dmq_tx,
                stop_tx,
                logger,
            })
        }
    }

    /// Stop the aggregator relay
    pub async fn stop(&self) -> StdResult<()> {
        self.stop_tx
            .send(())
            .with_context(|| "Failed to send stop signal to DMQ consumer server")?;

        Ok(())
    }

    async fn start_dmq_consumer_server(
        socket: &Path,
        dmq_network: &DmqNetwork,
        signature_dmq_rx: UnboundedReceiver<DmqMessage>,
        stop_rx: Receiver<()>,
        logger: Logger,
    ) -> StdResult<Arc<DmqConsumerServerPallas>> {
        let dmq_consumer_server = Arc::new(DmqConsumerServerPallas::new(
            socket.to_path_buf(),
            dmq_network.to_owned(),
            stop_rx,
            logger.clone(),
        ));
        dmq_consumer_server.register_receiver(signature_dmq_rx).await?;
        let dmq_consumer_server_clone = dmq_consumer_server.clone();
        tokio::spawn(async move {
            if let Err(err) = dmq_consumer_server_clone.run().await {
                error!(logger.to_owned(), "DMQ Consumer server failed"; "error" => ?err);
            }
        });

        Ok(dmq_consumer_server)
    }

    async fn notify_signature_to_aggregator(
        &self,
        signature_message: &RegisterSignatureMessageHttp,
    ) -> StdResult<()> {
        let response = reqwest::Client::new()
            .post(format!("{}/register-signatures", self.aggregator_endpoint))
            .json(signature_message)
            //.header(MITHRIL_API_VERSION_HEADER, "0.1.13") // TODO: retrieve current version
            .send()
            .await;
        match response {
            Ok(response) => match response.status() {
                StatusCode::CREATED | StatusCode::ACCEPTED => {
                    info!(self.logger, "Sent successfully signature message to aggregator"; "signature_message" => #?signature_message);
                    Ok(())
                }
                status => {
                    error!(
                        self.logger,
                        "Post `/register-signatures` should have returned a 201 or 202 status code, got: {status}"
                    );
                    Err(anyhow!(
                        "Post `/register-signatures` should have returned a 201 or 202 status code, got: {status}"
                    ))
                }
            },
            Err(err) => {
                error!(self.logger, "Post `/register-signatures` failed"; "error" => ?err);
                Err(anyhow!(err).context("Post `/register-signatures` failed"))
            }
        }
    }

    async fn notify_signer_to_aggregator(
        &self,
        signer_message: &RegisterSignerMessage,
    ) -> StdResult<()> {
        let response = reqwest::Client::new()
            .post(format!("{}/register-signer", self.aggregator_endpoint))
            .json(signer_message)
            //.header(MITHRIL_API_VERSION_HEADER, "0.1.13") // TODO: retrieve current version
            .send()
            .await;
        match response {
            Ok(response) => match response.status() {
                StatusCode::CREATED => {
                    info!(self.logger, "Sent successfully signer registration message to aggregator"; "signer_message" => #?signer_message);
                    Ok(())
                }
                status => {
                    error!(
                        self.logger,
                        "Post `/register-signer` should have returned a 201 status code, got: {status}"
                    );
                    Err(anyhow!(
                        "Post `/register-signer` should have returned a 201 status code, got: {status}"
                    ))
                }
            },
            Err(err) => {
                error!(self.logger, "Post `/register-signer` failed"; "error" => ?err);
                Err(anyhow!(err).context("Post `/register-signer` failed"))
            }
        }
    }

    /// Tick the aggregator relay
    pub async fn tick(&mut self) -> StdResult<()> {
        if let Some(peer_event) = self.peer.tick_swarm().await? {
            match self.peer.convert_peer_event_to_message(peer_event) {
                Ok(Some(BroadcastMessage::RegisterSignerHttp(signer_message_received))) => {
                    let retry_max = 3;
                    let mut retry_count = 0;
                    while let Err(e) =
                        self.notify_signer_to_aggregator(&signer_message_received).await
                    {
                        retry_count += 1;
                        if retry_count >= retry_max {
                            error!(self.logger, "Failed to send signer registration message to aggregator after {retry_count} attempts"; "signer_message" => #?signer_message_received, "error" => ?e);
                            return Err(e);
                        }
                    }
                }
                Ok(Some(BroadcastMessage::RegisterSignatureHttp(signature_message_received))) => {
                    let retry_max = 3;
                    let mut retry_count = 0;
                    while let Err(e) =
                        self.notify_signature_to_aggregator(&signature_message_received).await
                    {
                        retry_count += 1;
                        if retry_count >= retry_max {
                            error!(self.logger, "Failed to send signature message to aggregator after {retry_count} attempts"; "signature_message" => #?signature_message_received, "error" => ?e);
                            return Err(e);
                        }
                    }
                }
                Ok(Some(BroadcastMessage::RegisterSignatureDmq(signature_message_received))) => {
                    self.signature_dmq_tx.send(signature_message_received).with_context(
                        || "Failed to send signature message to DMQ consumer server",
                    )?;
                }
                Ok(None) => {}
                Err(e) => return Err(e),
            }
        }

        Ok(())
    }

    /// Tick the peer of the aggregator relay
    #[allow(dead_code)]
    pub(crate) async fn tick_peer(&mut self) -> StdResult<Option<PeerEvent>> {
        self.peer.tick_swarm().await
    }

    /// Connect to a remote peer
    pub fn dial_peer(&mut self, addr: Multiaddr) -> StdResult<()> {
        self.peer.dial(addr)
    }

    /// Retrieve address on which the peer is listening
    pub fn peer_address(&self) -> Option<Multiaddr> {
        self.peer.addr_peer.to_owned()
    }
}

#[cfg(test)]
mod tests {
    use httpmock::MockServer;
    use mithril_common::test::double::Dummy;

    use crate::test_tools::TestLogger;

    use super::*;

    #[tokio::test]
    async fn sends_accept_encoding_header_with_correct_values() {
        let server = MockServer::start();
        let mock = server.mock(|when, then| {
            when.is_true(|req| {
                let headers = req.headers();
                let accept_encoding_header = headers
                    .get("accept-encoding")
                    .expect("Accept-Encoding header not found");

                ["gzip", "br", "deflate", "zstd"].iter().all(|&encoding| {
                    accept_encoding_header.to_str().is_ok_and(|h| h.contains(encoding))
                })
            });

            then.status(201).body("ok");
        });
        let addr: Multiaddr = "/ip4/0.0.0.0/tcp/0".parse().unwrap();
        let relay = AggregatorRelay::start(
            &addr,
            Path::new("test"),
            &DmqNetwork::TestNet(123),
            &server.url(""),
            &TestLogger::stdout(),
        )
        .await
        .unwrap();

        relay
            .notify_signature_to_aggregator(&RegisterSignatureMessageHttp::dummy())
            .await
            .expect("Should succeed with Accept-Encoding header");

        mock.assert();
    }
}
