use crate::p2p::{BroadcastMessage, Peer, PeerEvent};
use libp2p::Multiaddr;
use mithril_common::StdResult;
use mithril_common::logging::LoggerExtensions;
use slog::{Logger, debug, info};

/// A passive relay
pub struct PassiveRelay {
    /// Relay peer
    peer: Peer,
    logger: Logger,
}

impl PassiveRelay {
    /// Start a passive relay
    pub async fn start(addr: &Multiaddr, logger: &Logger) -> StdResult<Self> {
        let relay_logger = logger.new_with_component_name::<Self>();
        debug!(relay_logger, "Starting...");

        Ok(Self {
            peer: Peer::new(addr).with_logger(logger).start().await?,
            logger: relay_logger,
        })
    }

    /// Get the peer of the passive relay
    pub fn peer_mut(&mut self) -> &mut Peer {
        &mut self.peer
    }

    /// Tick the passive relay
    pub async fn tick(&mut self) -> StdResult<()> {
        if let Some(peer_event) = self.peer.tick_swarm().await? {
            match self.peer.convert_peer_event_to_message(peer_event) {
                Ok(Some(BroadcastMessage::RegisterSigner(signer_message_received))) => {
                    info!(self.logger, "Received signer registration message from P2P network"; "signer_message" => #?signer_message_received);
                }
                Ok(Some(BroadcastMessage::RegisterSignature(signature_message_received))) => {
                    info!(self.logger, "Received signature message from P2P network"; "signature_message" => #?signature_message_received);
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
