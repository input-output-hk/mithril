use crate::p2p::{BroadcastMessage, Peer, PeerEvent};
use libp2p::Multiaddr;
use mithril_common::StdResult;
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

    /// Convert event to broadcast message
    /// TODO: should be removed
    pub fn convert_peer_event_to_message(
        &mut self,
        event: PeerEvent,
    ) -> StdResult<Option<BroadcastMessage>> {
        self.peer.convert_peer_event_to_message(event)
    }

    /// Tick the passive relay
    pub async fn tick(&mut self) -> StdResult<()> {
        if let Some(peer_event) = self.peer.tick_swarm().await? {
            match self.peer.convert_peer_event_to_message(peer_event) {
                Ok(Some(BroadcastMessage::RegisterSigner(signer_message_received))) => {
                    info!("Relay passive: received signer registration message from P2P network"; "signer_message" => format!("{:#?}", signer_message_received));
                }
                Ok(Some(BroadcastMessage::RegisterSignature(signature_message_received))) => {
                    info!("Relay passive: received signature message from P2P network"; "signature_message" => format!("{:#?}", signature_message_received));
                }
                Ok(None) => {}
                Err(e) => return Err(e),
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
