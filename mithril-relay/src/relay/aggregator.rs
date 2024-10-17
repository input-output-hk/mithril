use crate::p2p::{BroadcastMessage, Peer, PeerEvent};
use anyhow::anyhow;
use libp2p::Multiaddr;
use mithril_common::{
    logging::LoggerExtensions,
    messages::{RegisterSignatureMessage, RegisterSignerMessage},
    StdResult,
};
use reqwest::StatusCode;
use slog::{error, info, Logger};

/// A relay for a Mithril aggregator
pub struct AggregatorRelay {
    aggregator_endpoint: String,
    peer: Peer,
    logger: Logger,
}

impl AggregatorRelay {
    /// Start a relay for a Mithril aggregator
    pub async fn start(
        addr: &Multiaddr,
        aggregator_endpoint: &str,
        logger: &Logger,
    ) -> StdResult<Self> {
        Ok(Self {
            aggregator_endpoint: aggregator_endpoint.to_owned(),
            peer: Peer::new(addr).with_logger(logger).start().await?,
            logger: logger.new_with_component_name::<Self>(),
        })
    }

    async fn notify_signature_to_aggregator(
        &self,
        signature_message: &RegisterSignatureMessage,
    ) -> StdResult<()> {
        let response = reqwest::Client::new()
            .post(format!("{}/register-signatures", self.aggregator_endpoint))
            .json(signature_message)
            //.header(MITHRIL_API_VERSION_HEADER, "0.1.13") // TODO: retrieve current version
            .send()
            .await;
        match response {
            Ok(response) => match response.status() {
                StatusCode::CREATED => {
                    info!(self.logger, "Relay aggregator: sent successfully signature message to aggregator"; "signature_message" => #?signature_message);
                    Ok(())
                }
                status => {
                    error!(self.logger, "Relay aggregator: Post `/register-signatures` should have returned a 201 status code, got: {status}");
                    Err(anyhow!("Post `/register-signatures` should have returned a 201 status code, got: {status}"))
                }
            },
            Err(err) => {
                error!(
                    self.logger,
                    "Relay aggregator: Post `/register-signatures` failed: {err:?}"
                );
                Err(anyhow!("Post `/register-signatures` failed: {err:?}"))
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
                    info!(self.logger, "Relay aggregator: sent successfully signer registration message to aggregator"; "signer_message" => #?signer_message);
                    Ok(())
                }
                status => {
                    error!(self.logger, "Relay aggregator: Post `/register-signer` should have returned a 201 status code, got: {status}");
                    Err(anyhow!("Post `/register-signer` should have returned a 201 status code, got: {status}"))
                }
            },
            Err(err) => {
                error!(
                    self.logger,
                    "Relay aggregator: Post `/register-signer` failed: {err:?}"
                );
                Err(anyhow!("Post `/register-signer` failed: {err:?}"))
            }
        }
    }

    /// Tick the aggregator relay
    pub async fn tick(&mut self) -> StdResult<()> {
        if let Some(peer_event) = self.peer.tick_swarm().await? {
            match self.peer.convert_peer_event_to_message(peer_event) {
                Ok(Some(BroadcastMessage::RegisterSigner(signer_message_received))) => {
                    let retry_max = 3;
                    let mut retry_count = 0;
                    while let Err(e) = self
                        .notify_signer_to_aggregator(&signer_message_received)
                        .await
                    {
                        retry_count += 1;
                        if retry_count >= retry_max {
                            error!(self.logger, "Relay aggregator: failed to send signer registration message to aggregator after {retry_count} attempts"; "signer_message" => #?signer_message_received, "error" => ?e);
                            return Err(e);
                        }
                    }
                }
                Ok(Some(BroadcastMessage::RegisterSignature(signature_message_received))) => {
                    let retry_max = 3;
                    let mut retry_count = 0;
                    while let Err(e) = self
                        .notify_signature_to_aggregator(&signature_message_received)
                        .await
                    {
                        retry_count += 1;
                        if retry_count >= retry_max {
                            error!(self.logger, "Relay aggregator: failed to send signature message to aggregator after {retry_count} attempts"; "signature_message" => #?signature_message_received, "error" => ?e);
                            return Err(e);
                        }
                    }
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
