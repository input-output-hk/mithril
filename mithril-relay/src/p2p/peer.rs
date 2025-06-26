#![allow(missing_docs)]
use anyhow::{anyhow, Context};
use libp2p::{
    core::{muxing::StreamMuxerBox, transport::dummy::DummyTransport},
    futures::StreamExt,
    gossipsub::{self, ValidationMode},
    noise, ping,
    swarm::{self, DialError, NetworkBehaviour},
    tls, yamux, Multiaddr, PeerId, Swarm, SwarmBuilder,
};
use mithril_common::{
    logging::LoggerExtensions,
    messages::{RegisterSignatureMessageHttp, RegisterSignerMessage},
    StdResult,
};
use serde::{Deserialize, Serialize};
use slog::{debug, info, Logger};
use std::{collections::HashMap, time::Duration};

use crate::{mithril_p2p_topic, p2p::PeerError};

/// The idle connection timeout for a P2P connection
const P2P_IDLE_CONNECTION_TIMEOUT: Duration = Duration::from_secs(30);

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

/// The broadcast message received from a Gossip sub event
#[derive(Serialize, Deserialize)]
pub enum BroadcastMessage {
    /// A signer registration message received from the Gossip sub
    RegisterSigner(RegisterSignerMessage),

    /// A signature registration message received from the Gossip sub
    RegisterSignature(RegisterSignatureMessageHttp),
}

/// A peer in the P2P network
pub struct Peer {
    topics: HashMap<TopicName, gossipsub::IdentTopic>,
    swarm: Option<Swarm<PeerBehaviour>>,
    addr: Multiaddr,
    /// Multi address on which the peer is listening
    pub addr_peer: Option<Multiaddr>,
    logger: Logger,
}

impl Peer {
    /// Peer factory
    pub fn new(addr: &Multiaddr) -> Self {
        Self {
            topics: Self::build_topics(),
            swarm: None,
            addr: addr.to_owned(),
            addr_peer: None,
            logger: Logger::root(slog::Discard, slog::o!()),
        }
    }

    fn build_topics() -> HashMap<TopicName, gossipsub::IdentTopic> {
        HashMap::from([
            (
                mithril_p2p_topic::SIGNATURES.into(),
                gossipsub::IdentTopic::new(mithril_p2p_topic::SIGNATURES),
            ),
            (
                mithril_p2p_topic::SIGNERS.into(),
                gossipsub::IdentTopic::new(mithril_p2p_topic::SIGNERS),
            ),
        ])
    }

    /// Set the logger for the peer
    pub fn with_logger(mut self, logger: &Logger) -> Self {
        self.logger = logger.new_with_component_name::<Self>();
        self
    }

    /// Start the peer
    pub async fn start(mut self) -> StdResult<Self> {
        debug!(self.logger, "Starting...");
        let mut swarm = SwarmBuilder::with_new_identity()
            .with_tokio()
            .with_tcp(
                Default::default(),
                (tls::Config::new, noise::Config::new),
                yamux::Config::default,
            )?
            .with_quic()
            .with_other_transport(|_key| DummyTransport::<(PeerId, StreamMuxerBox)>::new())?
            .with_dns()?
            .with_websocket(
                (tls::Config::new, noise::Config::new),
                yamux::Config::default,
            )
            .await?
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
            .with_swarm_config(|c| c.with_idle_connection_timeout(P2P_IDLE_CONNECTION_TIMEOUT))
            .build();

        for topic in self.topics.values() {
            debug!(self.logger, "Subscribing to"; "topic" => ?topic, "local_peer_id" => ?self.local_peer_id());
            swarm.behaviour_mut().gossipsub.subscribe(topic)?;
        }

        let _listener_id = swarm.listen_on(self.addr.clone())?;
        self.swarm = Some(swarm);

        loop {
            if let Some(PeerEvent::ListeningOnAddr { address }) = self.tick_swarm().await? {
                info!(self.logger, "Listening on"; "address" => ?address, "local_peer_id" => ?self.local_peer_id());
                self.addr_peer = Some(address);
                break;
            }
        }

        Ok(self)
    }

    /// Convert a peer event to a broadcast message
    pub fn convert_peer_event_to_message(
        &mut self,
        event: PeerEvent,
    ) -> StdResult<Option<BroadcastMessage>> {
        match event {
            PeerEvent::Behaviour {
                event: PeerBehaviourEvent::Gossipsub(gossipsub::Event::Message { message, .. }),
            } => Ok(Some(serde_json::from_slice(&message.data)?)),
            _ => Ok(None),
        }
    }

    /// Tick the peer swarm to receive the next event
    pub async fn tick_swarm(&mut self) -> StdResult<Option<PeerEvent>> {
        debug!(self.logger, "Reading next event"; "local_peer_id" => ?self.local_peer_id());
        match self
            .swarm
            .as_mut()
            .ok_or(PeerError::UnavailableSwarm())
            .with_context(|| "Can not publish signature without swarm")?
            .next()
            .await
        {
            Some(swarm::SwarmEvent::NewListenAddr { address, .. }) => {
                debug!(self.logger, "Received listening address event"; "address" => ?address, "local_peer_id" => ?self.local_peer_id());
                Ok(Some(PeerEvent::ListeningOnAddr { address }))
            }
            Some(swarm::SwarmEvent::OutgoingConnectionError { peer_id, error, .. }) => {
                debug!(self.logger, "Received outgoing connection error event"; "error" => ?error, "remote_peer_id" => ?peer_id, "local_peer_id" => ?self.local_peer_id());
                Ok(Some(PeerEvent::OutgoingConnectionError { peer_id, error }))
            }
            Some(swarm::SwarmEvent::ConnectionEstablished { peer_id, .. }) => {
                debug!(self.logger, "Received connection established event"; "remote_peer_id" => ?peer_id, "local_peer_id" => ?self.local_peer_id());
                Ok(Some(PeerEvent::ConnectionEstablished { peer_id }))
            }
            Some(swarm::SwarmEvent::Behaviour(event)) => {
                debug!(self.logger, "Received behaviour event"; "event" => ?event, "local_peer_id" => ?self.local_peer_id());
                Ok(Some(PeerEvent::Behaviour { event }))
            }
            Some(event) => {
                debug!(self.logger, "Received other event"; "event" => ?event, "local_peer_id" => ?self.local_peer_id());
                Ok(None)
            }
            _ => Ok(None),
        }
    }

    /// Publish a signature on the P2P pubsub
    pub fn publish_signature(
        &mut self,
        message: &RegisterSignatureMessageHttp,
    ) -> StdResult<gossipsub::MessageId> {
        self.publish_broadcast_message(
            &BroadcastMessage::RegisterSignature(message.to_owned()),
            mithril_p2p_topic::SIGNATURES,
        )
    }

    /// Publish a broadcast message on the P2P pubsub
    pub fn publish_broadcast_message(
        &mut self,
        message: &BroadcastMessage,
        topic_name: &str,
    ) -> StdResult<gossipsub::MessageId> {
        let topic = self
            .topics
            .get(topic_name)
            .ok_or(PeerError::MissingTopic())
            .with_context(|| {
                format!("Can not publish broadcast message on invalid topic: {topic_name}")
            })?
            .to_owned();
        let data = serde_json::to_vec(message).with_context(|| {
            format!("Can not publish broadcast message with invalid format on topic {topic_name}")
        })?;

        let message_id = self
            .swarm
            .as_mut()
            .map(|swarm| swarm.behaviour_mut().gossipsub.publish(topic, data))
            .transpose()
            .with_context(|| {
                format!("Can not publish broadcast message on {topic_name} P2P pubsub")
            })?
            .ok_or(PeerError::UnavailableSwarm())
            .with_context(|| {
                format!(
                    "Can not publish broadcast message on {topic_name} P2P pubsub without swarm"
                )
            })?;

        Ok(message_id.to_owned())
    }

    /// Publish a signer registration on the P2P pubsub
    pub fn publish_signer_registration(
        &mut self,
        message: &RegisterSignerMessage,
    ) -> StdResult<gossipsub::MessageId> {
        self.publish_broadcast_message(
            &BroadcastMessage::RegisterSigner(message.to_owned()),
            mithril_p2p_topic::SIGNERS,
        )
    }

    /// Connect to a remote peer
    pub fn dial(&mut self, addr: Multiaddr) -> StdResult<()> {
        debug!(self.logger, "Dialing to"; "address" => ?addr, "local_peer_id" => ?self.local_peer_id());
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
