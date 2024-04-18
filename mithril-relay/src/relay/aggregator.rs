use crate::p2p::{BroadcastMessage, Peer, PeerEvent};
use anyhow::anyhow;
use libp2p::Multiaddr;
use mithril_common::{
    messages::{RegisterSignatureMessage, RegisterSignerMessage},
    StdResult,
};
use reqwest::StatusCode;
use slog_scope::{error, info};

/// A relay for a Mithril aggregator
pub struct AggregatorRelay {
    aggregator_endpoint: String,
    peer: Peer,
}

impl AggregatorRelay {
    /// Start a relay for a Mithril aggregator
    pub async fn start(addr: &Multiaddr, aggregator_endpoint: &str) -> StdResult<Self> {
        Ok(Self {
            aggregator_endpoint: aggregator_endpoint.to_owned(),
            peer: Peer::new(addr).start().await?,
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
                    info!("Relay aggregator: sent successfully signature message to aggregator"; "signature_message" => format!("{:#?}", signature_message));
                    Ok(())
                }
                status => {
                    error!("Relay aggregator: Post `/register-signatures` should have returned a 201 status code, got: {status}");
                    Err(anyhow!("Post `/register-signatures` should have returned a 201 status code, got: {status}"))
                }
            },
            Err(err) => {
                error!("Relay aggregator: Post `/register-signatures` failed: {err:?}");
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
                    info!("Relay aggregator: sent successfully signer registration message to aggregator"; "signer_message" => format!("{:#?}", signer_message));
                    Ok(())
                }
                status => {
                    error!("Relay aggregator: Post `/register-signer` should have returned a 201 status code, got: {status}");
                    Err(anyhow!("Post `/register-signer` should have returned a 201 status code, got: {status}"))
                }
            },
            Err(err) => {
                error!("Relay aggregator: Post `/register-signer` failed: {err:?}");
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
                    let notify_aggregator = |message: RegisterSignerMessage| -> StdResult<()> {
                        tokio::runtime::Runtime::new()?
                            .block_on(self.notify_signer_to_aggregator(&message))
                    };
                    if let Err(e) = Self::try_notify_aggregator(
                        retry_max,
                        signer_message_received.clone(),
                        notify_aggregator,
                    )
                    .await
                    {
                        error!("Relay aggregator: failed to send signer registration message to aggregator after {retry_max} attempts"; "signer_message" => format!("{:#?}", signer_message_received), "error" => format!("{e:?}"));
                        return Err(e);
                    }
                }
                Ok(Some(BroadcastMessage::RegisterSignature(signature_message_received))) => {
                    let retry_max = 3;
                    let notify_aggregator = |message: RegisterSignatureMessage| -> StdResult<()> {
                        tokio::runtime::Runtime::new()?
                            .block_on(self.notify_signature_to_aggregator(&message))
                    };
                    if let Err(e) = Self::try_notify_aggregator(
                        retry_max,
                        signature_message_received.clone(),
                        notify_aggregator,
                    )
                    .await
                    {
                        error!("Relay aggregator: failed to send signature message to aggregator after {retry_max} attempts"; "signature_message" => format!("{:#?}", signature_message_received), "error" => format!("{e:?}"));
                        return Err(e);
                    }
                }
                Ok(None) => {}
                Err(e) => return Err(e),
            }
        }

        Ok(())
    }

    async fn try_notify_aggregator<'a, M: Clone, F>(
        retry_max: usize,
        message: M,
        notify_fn: F,
    ) -> StdResult<()>
    where
        F: Fn(M) -> StdResult<()>,
    {
        let mut retry_count = 0;
        while let Err(e) = notify_fn(message.clone()) {
            retry_count += 1;
            if retry_count >= retry_max {
                return Err(e);
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
    use std::cell::RefCell;

    use mithril_common::{messages::RegisterSignerMessage, StdResult};

    use super::*;

    struct Counter {
        count: RefCell<usize>,
    }

    impl Counter {
        fn increment_counter(&self) {
            (*self.count.borrow_mut()) += 1;
        }

        fn get_counter(&self) -> usize {
            *self.count.borrow()
        }
    }

    impl Default for Counter {
        fn default() -> Self {
            Self {
                count: RefCell::new(0),
            }
        }
    }

    #[tokio::test]
    async fn test_try_notify_aggregator_succeeds_with_no_retry() {
        let retry_max = 3;
        let counter = Counter::default();
        let notify_aggregator = |_message: RegisterSignerMessage| -> StdResult<()> {
            counter.increment_counter();
            Ok(())
        };
        let notified = AggregatorRelay::try_notify_aggregator(
            retry_max,
            RegisterSignerMessage::dummy(),
            notify_aggregator,
        )
        .await;

        notified.unwrap();
        assert_eq!(counter.get_counter(), 1);
    }

    #[tokio::test]
    async fn test_try_notify_aggregator_succeeds_after_less_than_max_retry() {
        let retry_max = 3;
        let counter = Counter::default();
        let notify_aggregator = |_message: RegisterSignerMessage| -> StdResult<()> {
            counter.increment_counter();
            if counter.get_counter() <= 2 {
                return Err(anyhow!("notify error"));
            }

            Ok(())
        };
        let notified = AggregatorRelay::try_notify_aggregator(
            retry_max,
            RegisterSignerMessage::dummy(),
            notify_aggregator,
        )
        .await;

        notified.unwrap();
        assert_eq!(counter.get_counter(), 3);
    }

    #[tokio::test]
    async fn test_try_notify_aggregator_fails_after_more_than_max_retry() {
        let retry_max = 3;
        let counter = Counter::default();
        let notify_aggregator = |_message: RegisterSignerMessage| -> StdResult<()> {
            counter.increment_counter();

            Err(anyhow!("notify error"))
        };
        let notified = AggregatorRelay::try_notify_aggregator(
            retry_max,
            RegisterSignerMessage::dummy(),
            notify_aggregator,
        )
        .await;

        notified.expect_err("should have failed");
        assert_eq!(counter.get_counter(), 3);
    }
}
