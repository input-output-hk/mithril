use crate::peer::{Peer, PeerBehaviourEvent, PeerEvent};
use anyhow::anyhow;
use libp2p::{gossipsub, Multiaddr};
use mithril_common::{messages::RegisterSignatureMessage, StdResult};
use reqwest::StatusCode;
use slog_scope::{error, info};

pub struct AggregatorRelay {
    aggregator_endpoint: String,
    pub peer: Peer,
}

impl AggregatorRelay {
    pub async fn start(
        topic_name: &str,
        addr: &Multiaddr,
        aggregator_endpoint: &str,
    ) -> StdResult<Self> {
        Ok(Self {
            aggregator_endpoint: aggregator_endpoint.to_owned(),
            peer: Peer::new(topic_name, addr).start().await?,
        })
    }

    pub fn convert_peer_event_to_signature_message(
        &mut self,
        event: PeerEvent,
    ) -> StdResult<Option<RegisterSignatureMessage>> {
        match event {
            PeerEvent::Behaviour {
                event: PeerBehaviourEvent::Gossipsub(gossipsub::Event::Message { message, .. }),
            } => Ok(Some(serde_json::from_slice(&message.data)?)),
            _ => Ok(None),
        }
    }

    pub async fn notify_signature_to_aggregator(
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
                    info!("Relay aggregator: sent successfully signature message to aggregator");
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

    pub async fn tick(&mut self) -> StdResult<()> {
        if let Some(peer_event) = self.peer.tick_swarm().await? {
            if let Ok(Some(signature_message_received)) =
                self.convert_peer_event_to_signature_message(peer_event)
            {
                self.notify_signature_to_aggregator(&signature_message_received)
                    .await?;
            }
        }

        Ok(())
    }

    pub async fn tick_peer(&mut self) -> StdResult<Option<PeerEvent>> {
        self.peer.tick_swarm().await
    }

    pub fn dial_peer(&mut self, addr: Multiaddr) -> StdResult<()> {
        self.peer.dial(addr)
    }

    pub fn peer_address(&self) -> Option<Multiaddr> {
        self.peer.addr_peer.to_owned()
    }
}
