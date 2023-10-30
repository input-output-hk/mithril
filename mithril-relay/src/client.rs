use libp2p::{gossipsub, Multiaddr};
use mithril_common::{messages::RegisterSignatureMessage, StdResult};

use crate::peer::{Peer, PeerBehaviourEvent, PeerEvent};

pub struct P2PClient {
    pub peer: Peer,
}

impl P2PClient {
    pub fn new(topic_name: &str) -> Self {
        Self {
            peer: Peer::new(topic_name),
        }
    }

    pub fn consume(&mut self, event: PeerEvent) -> StdResult<Option<RegisterSignatureMessage>> {
        match event {
            PeerEvent::Behaviour {
                event: PeerBehaviourEvent::Gossipsub(gossipsub::Event::Message { message, .. }),
            } => Ok(Some(serde_json::from_slice(&message.data)?)),
            _ => Ok(None),
        }
    }

    pub async fn tick_peer(&mut self) -> StdResult<Option<PeerEvent>> {
        self.peer.tick_swarm().await
    }

    pub async fn start(self) -> StdResult<Self> {
        Ok(Self {
            peer: self.peer.start().await?,
        })
    }

    pub fn address(&self) -> Option<Multiaddr> {
        self.peer.addr.to_owned()
    }
}
