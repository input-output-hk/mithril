use crate::p2p::{Peer, PeerBehaviourEvent, PeerEvent};
use libp2p::{gossipsub, Multiaddr};
use mithril_common::{messages::RegisterSignatureMessage, StdResult};
use slog_scope::{debug, info};

/// A passive relay
pub struct PassiveRelay {
    /// Relay peer
    // TODO: should be private
    pub peer: Peer,
}

impl PassiveRelay {
    /// Create a passive relay
    /// TODO: should be replaced by Self::start(...)
    pub fn new(addr: &Multiaddr) -> Self {
        Self {
            peer: Peer::new(addr),
        }
    }

    /// Start a passive relay
    pub async fn start(self) -> StdResult<Self> {
        debug!("PassiveRelay: starting...");
        Ok(Self {
            peer: self.peer.start().await?,
        })
    }

    /// Convert event to signature message
    /// TODO: should be removed
    pub fn convert_event(
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

    /// Tick the passive relay
    pub async fn tick(&mut self) -> StdResult<()> {
        if let Some(peer_event) = self.peer.tick_swarm().await? {
            if let Ok(Some(signature_message_received)) = self
                .peer
                .convert_peer_event_to_signature_message(peer_event)
            {
                info!("Relay passive: received signature message from P2P network"; "signature_message" => format!("{:#?}", signature_message_received));
            }
        }

        Ok(())
    }

    /// Tick the peer of the passive relay
    pub async fn tick_peer(&mut self) -> StdResult<Option<PeerEvent>> {
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
