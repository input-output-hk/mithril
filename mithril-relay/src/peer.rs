use std::time::Duration;

use libp2p::{
    core::{transport::MemoryTransport, upgrade::Version},
    futures::StreamExt,
    gossipsub::{self, ValidationMode},
    noise, ping, swarm, tcp, yamux, Multiaddr, Swarm, Transport,
};
use mithril_common::{messages::RegisterSignatureMessage, StdResult};

// We create a custom network behaviour that combines gossipsub and ping.
#[derive(swarm::NetworkBehaviour)]
pub struct PeerBehaviour {
    gossipsub: gossipsub::Behaviour,
    ping: ping::Behaviour,
}

#[derive(Debug)]
pub enum PeerEvent {
    ListeningOnAddr { address: Multiaddr },
    Behaviour { event: PeerBehaviourEvent },
}

pub struct Peer {
    pub topic: gossipsub::IdentTopic,
    pub swarm: Option<Swarm<PeerBehaviour>>,
    pub addr: Option<Multiaddr>,
}

impl Peer {
    pub fn new(topic_name: &str) -> Self {
        Self {
            topic: gossipsub::IdentTopic::new(topic_name),
            swarm: None,
            addr: None,
        }
    }

    pub fn publish(
        &mut self,
        message: &RegisterSignatureMessage,
    ) -> StdResult<gossipsub::MessageId> {
        let topic = self.topic.clone();
        let data = serde_json::to_vec(message).unwrap();

        let message_id = self
            .swarm
            .as_mut()
            .map(|swarm| swarm.behaviour_mut().gossipsub.publish(topic, data))
            .transpose()?
            .unwrap();
        Ok(message_id.to_owned())
    }

    pub async fn tick_swarm(&mut self) -> StdResult<Option<PeerEvent>> {
        println!(
            ">> [{}] Reading next event",
            self.swarm.as_ref().unwrap().local_peer_id()
        );
        match self.swarm.as_mut().unwrap().next().await {
            Some(swarm::SwarmEvent::NewListenAddr { address, .. }) => {
                println!(
                    ">> [{}] Received listening address event: {address:?}",
                    self.swarm.as_ref().unwrap().local_peer_id()
                );
                Ok(Some(PeerEvent::ListeningOnAddr { address }))
            }
            Some(swarm::SwarmEvent::Behaviour(event)) => {
                println!(
                    ">> [{}] Received behaviour event: {event:?}",
                    self.swarm.as_ref().unwrap().local_peer_id()
                );
                Ok(Some(PeerEvent::Behaviour { event }))
            }
            Some(event) => {
                println!(
                    ">> [{}] Received other event: {event:?}",
                    self.swarm.as_ref().unwrap().local_peer_id()
                );
                Ok(None)
            }
            _ => Ok(None),
        }
    }

    pub async fn start(mut self) -> StdResult<Self> {
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
                /* MemoryTransport::default()
                .upgrade(libp2p::core::upgrade::Version::V1)
                .authenticate(libp2p::noise::Config::new(key).unwrap())
                .multiplex(libp2p::yamux::Config::default()) */
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
        swarm
            .behaviour_mut()
            .gossipsub
            .subscribe(&self.topic)
            .unwrap();

        let addr: Multiaddr = "/ip4/0.0.0.0/tcp/0".parse()?;
        //let addr: Multiaddr = "/memory/0".parse()?;
        let _listener_id = swarm.listen_on(addr.clone()).unwrap();
        self.swarm = Some(swarm);

        loop {
            if let Some(PeerEvent::ListeningOnAddr { address }) = self.tick_swarm().await? {
                println!(
                    ">> [{}] Listening on {:?}",
                    self.swarm.as_ref().unwrap().local_peer_id(),
                    address
                );
                self.addr = Some(address);
                break;
            }
        }

        Ok(self)
    }

    pub fn dial(&mut self, addr: Multiaddr) -> StdResult<()> {
        println!(
            ">> [{}] Dialing {addr:?}",
            self.swarm.as_ref().unwrap().local_peer_id()
        );
        self.swarm.as_mut().unwrap().dial(addr)?;

        Ok(())
    }
}
