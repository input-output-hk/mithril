use libp2p::{
    core::upgrade::Version, futures::StreamExt, gossipsub, noise, ping, swarm, tcp, yamux,
    Multiaddr, Swarm, Transport,
};
use mithril_common::{messages::RegisterSignatureMessage, StdResult};

pub struct P2PClient {
    pub peer: Peer,
}

impl P2PClient {
    pub fn new(topic_name: &str) -> Self {
        Self {
            peer: Peer::new(topic_name),
        }
    }

    pub async fn consume(
        &mut self,
        event: PeerEvent,
    ) -> StdResult<Option<RegisterSignatureMessage>> {
        match event {
            PeerEvent::Behaviour {
                event: PeerBehaviourEvent::Gossipsub(gossipsub::Event::Message { .. }),
            } => Ok(Some(RegisterSignatureMessage::dummy())),
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
}

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
        match self.swarm.as_mut().unwrap().next().await {
            Some(swarm::SwarmEvent::NewListenAddr { address, .. }) => {
                Ok(Some(PeerEvent::ListeningOnAddr { address }))
            }
            Some(swarm::SwarmEvent::Behaviour(event)) => Ok(Some(PeerEvent::Behaviour { event })),
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
            })?
            .with_dns()?
            .with_behaviour(|key| {
                let gossipsub_config = gossipsub::ConfigBuilder::default()
                    .max_transmit_size(262144)
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
        let _listener_id = swarm.listen_on(addr.clone()).unwrap();
        self.swarm = Some(swarm);

        loop {
            if let Some(PeerEvent::ListeningOnAddr { address }) = self.tick_swarm().await? {
                println!(">> Listening on {:?}", address);
                self.addr = Some(address);
                break;
            }
        }

        Ok(self)
    }

    pub fn dial(&mut self, addr: Multiaddr) -> StdResult<()> {
        println!(">> Dialing {addr:?}");
        self.swarm.as_mut().unwrap().dial(addr)?;

        Ok(())
    }
}
