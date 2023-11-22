#![allow(missing_docs)]
use anyhow::{anyhow, Context};
use libp2p::{
    core::upgrade::Version,
    futures::StreamExt,
    gossipsub::{self, ValidationMode},
    noise, ping,
    swarm::{self, DialError, NetworkBehaviour},
    tcp, yamux, Multiaddr, PeerId, Swarm, Transport,
};
use mithril_common::{messages::RegisterSignatureMessage, StdResult};
use slog_scope::{debug, info};
use std::{collections::HashMap, time::Duration};

use crate::{p2p::PeerError, MITHRIL_SIGNATURES_TOPIC_NAME};

/// [Peer] custom network behaviour
#[derive(NetworkBehaviour)]
pub struct PeerBehaviour {
    gossipsub: gossipsub::Behaviour,
    ping: ping::Behaviour,
}

/// [Peer] event that is polled from the swarm
#[derive(Debug)]
pub enum PeerEvent {
    /// The peer is listening on an address
    ListeningOnAddr {
        /// Listening multi address
        address: Multiaddr,
    },
    /// The peer established a connection with another peer
    ConnectionEstablished {
        /// Remote peer id
        peer_id: PeerId,
    },
    /// The peer can not connect to another peer
    OutgoingConnectionError {
        /// Remote peer id
        peer_id: Option<PeerId>,
        /// Error that occurred when dialing the remote peer
        error: DialError,
    },
    /// The peer received a behaviour related event
    Behaviour {
        /// The behaviour event the peer received
        event: PeerBehaviourEvent,
    },
}

/// The topic name of a P2P pubsub
pub type TopicName = String;

/// A peer in the P2P network
pub struct Peer {
    topics: HashMap<TopicName, gossipsub::IdentTopic>,
    swarm: Option<Swarm<PeerBehaviour>>,
    addr: Multiaddr,
    /// Multi address on which the peer is listening
    pub addr_peer: Option<Multiaddr>,
}

impl Peer {
    /// Peer factory
    pub fn new(addr: &Multiaddr) -> Self {
        Self {
            topics: Self::build_topics(),
            swarm: None,
            addr: addr.to_owned(),
            addr_peer: None,
        }
    }

    fn build_topics() -> HashMap<TopicName, gossipsub::IdentTopic> {
        HashMap::from([(
            MITHRIL_SIGNATURES_TOPIC_NAME.into(),
            gossipsub::IdentTopic::new(MITHRIL_SIGNATURES_TOPIC_NAME),
        )])
    }

    /// Start the peer
    pub async fn start(mut self) -> StdResult<Self> {
        debug!("Peer: starting...");
        let mut swarm = libp2p::SwarmBuilder::with_new_identity()
            .with_tokio()
            .with_other_transport(|key| {
                let noise_config = noise::Config::new(key).unwrap();
                let yamux_config = yamux::Config::default();
                let base_transport =
                    tcp::tokio::Transport::new(tcp::Config::default().nodelay(true));
                base_transport
                    .upgrade(Version::V1Lazy)
                    .authenticate(noise_config)
                    .multiplex(yamux_config)
            })?
            .with_dns()?
            .with_behaviour(|key| {
                let gossipsub_config = gossipsub::ConfigBuilder::default()
                    .max_transmit_size(262144)
                    .heartbeat_initial_delay(Duration::from_millis(100))
                    .heartbeat_interval(Duration::from_millis(200))
                    .history_length(10)
                    .history_gossip(10)
                    .validation_mode(ValidationMode::Strict)
                    .build()?;
                Ok(PeerBehaviour {
                    gossipsub: gossipsub::Behaviour::new(
                        gossipsub::MessageAuthenticity::Signed(key.clone()),
                        gossipsub_config,
                    )
                    .expect("Valid configuration"),
                    ping: ping::Behaviour::new(ping::Config::new()),
                })
            })?
            .build();

        for topic in self.topics.values() {
            debug!("Peer: subscribing to"; "topic" => format!("{topic:?}"), "local_peer_id" => format!("{:?}", self.local_peer_id()));
            swarm.behaviour_mut().gossipsub.subscribe(topic)?;
        }

        let _listener_id = swarm.listen_on(self.addr.clone())?;
        self.swarm = Some(swarm);

        loop {
            if let Some(PeerEvent::ListeningOnAddr { address }) = self.tick_swarm().await? {
                info!("Peer: listening on"; "address" => format!("{address:?}"), "local_peer_id" => format!("{:?}", self.local_peer_id()));
                self.addr_peer = Some(address);
                break;
            }
        }

        Ok(self)
    }

    /// Tick the peer swarm to receive the next event
    pub async fn tick_swarm(&mut self) -> StdResult<Option<PeerEvent>> {
        debug!("Peer: reading next event"; "local_peer_id" => format!("{:?}", self.local_peer_id()));
        match self
            .swarm
            .as_mut()
            .ok_or(PeerError::UnavailableSwarm())
            .with_context(|| "Can not publish signature without swarm")?
            .next()
            .await
        {
            Some(swarm::SwarmEvent::NewListenAddr { address, .. }) => {
                debug!("Peer: received listening address event"; "address" => format!("{address:?}"), "local_peer_id" => format!("{:?}", self.local_peer_id()));
                Ok(Some(PeerEvent::ListeningOnAddr { address }))
            }
            Some(swarm::SwarmEvent::OutgoingConnectionError { peer_id, error, .. }) => {
                debug!("Peer: received outgoing connection error event"; "error" => format!("{error:#?}"), "remote_peer_id" => format!("{peer_id:?}"), "local_peer_id" => format!("{:?}", self.local_peer_id()));
                Ok(Some(PeerEvent::OutgoingConnectionError { peer_id, error }))
            }
            Some(swarm::SwarmEvent::ConnectionEstablished { peer_id, .. }) => {
                debug!("Peer: received connection established event"; "remote_peer_id" => format!("{peer_id:?}"), "local_peer_id" => format!("{:?}", self.local_peer_id()));
                Ok(Some(PeerEvent::ConnectionEstablished { peer_id }))
            }
            Some(swarm::SwarmEvent::Behaviour(event)) => {
                debug!("Peer: received behaviour event"; "event" => format!("{event:#?}"), "local_peer_id" => format!("{:?}", self.local_peer_id()));
                Ok(Some(PeerEvent::Behaviour { event }))
            }
            Some(event) => {
                debug!("Peer: received other event"; "event" => format!("{event:#?}"), "local_peer_id" => format!("{:?}", self.local_peer_id()));
                Ok(None)
            }
            _ => Ok(None),
        }
    }

    /// Publish a signature on the P2P pubsub
    pub fn publish_signature(
        &mut self,
        message: &RegisterSignatureMessage,
    ) -> StdResult<gossipsub::MessageId> {
        let topic = self
            .topics
            .get(MITHRIL_SIGNATURES_TOPIC_NAME)
            .ok_or(PeerError::MissingTopic())
            .with_context(|| {
                format!(
                    "Can not publish signature on invalid topic: {MITHRIL_SIGNATURES_TOPIC_NAME}"
                )
            })?
            .to_owned();
        let data = serde_json::to_vec(message)
            .with_context(|| "Can not publish signature with invalid format")?;

        let message_id = self
            .swarm
            .as_mut()
            .map(|swarm| swarm.behaviour_mut().gossipsub.publish(topic, data))
            .transpose()
            .with_context(|| "Can not publish signature on P2P pubsub")?
            .ok_or(PeerError::UnavailableSwarm())
            .with_context(|| "Can not publish signature without swarm")?;
        Ok(message_id.to_owned())
    }

    /// Connect to a remote peer
    pub fn dial(&mut self, addr: Multiaddr) -> StdResult<()> {
        debug!("Peer: dialing to"; "address" => format!("{addr:?}"), "local_peer_id" => format!("{:?}", self.local_peer_id()));
        self.swarm
            .as_mut()
            .ok_or(PeerError::UnavailableSwarm())
            .with_context(|| "Can not dial without swarm")?
            .dial(addr)
            .map_err(|e| anyhow!(e))
    }

    /// Get the local peer id (if any)
    pub fn local_peer_id(&self) -> Option<PeerId> {
        self.swarm.as_ref().map(|s| s.local_peer_id().to_owned())
    }
}
