use libp2p::{
    core::transport::MemoryTransport,
    futures::StreamExt,
    gossipsub::{Behaviour, IdentTopic, MessageAuthenticity, MessageId},
    identity,
    swarm::SwarmEvent,
    Multiaddr, Swarm, Transport,
};
use mithril_common::{messages::RegisterSignatureMessage, StdResult};

pub struct P2PClient {
    peer: Peer,
}
impl P2PClient {
    pub fn new(topic_name: &str) -> Self {
        Self {
            peer: Peer::new(topic_name),
        }
    }

    pub async fn consume(&mut self) -> StdResult<Option<RegisterSignatureMessage>> {
        self.peer.consume().await
    }

    pub fn start(self) -> StdResult<Self> {
        Ok(Self {
            peer: self.peer.start()?,
        })
    }
}

pub struct Peer {
    topic: IdentTopic,
    swarm: Option<Swarm<Behaviour>>,
}

impl Peer {
    pub fn new(topic_name: &str) -> Self {
        Self {
            topic: IdentTopic::new(topic_name),
            swarm: None,
        }
    }

    pub fn publish(&mut self, message: &RegisterSignatureMessage) -> StdResult<MessageId> {
        let topic = self.topic.clone();
        let data = serde_json::to_vec(message).unwrap();

        let message_id = self
            .swarm
            .as_mut()
            .map(|swarm| swarm.behaviour_mut().publish(topic, data))
            .transpose()?
            .unwrap();
        Ok(message_id.to_owned())
    }

    pub async fn consume(&mut self) -> StdResult<Option<RegisterSignatureMessage>> {
        match self.swarm.as_mut().unwrap().next().await {
            Some(SwarmEvent::NewListenAddr { address, .. }) => {
                println!("Listening on {address:?}");

                Ok(None)
            }
            Some(SwarmEvent::Behaviour(event)) => {
                println!("{event:?}");
                Ok(Some(RegisterSignatureMessage::dummy()))
            }
            _ => Ok(None),
        }
    }

    pub fn start(mut self) -> StdResult<Self> {
        let local_key = identity::Keypair::generate_ed25519();
        let local_peer_id = local_key.public().to_peer_id();

        // Set up an encrypted TCP Transport over yamux
        // This is test transport (memory).
        let transport = MemoryTransport::default()
            .upgrade(libp2p::core::upgrade::Version::V1)
            .authenticate(libp2p::noise::Config::new(&local_key).unwrap())
            .multiplex(libp2p::yamux::Config::default())
            .boxed();

        // Set the message authenticity - How we expect to publish messages
        // Here we expect the publisher to sign the message with their key.
        let message_authenticity = MessageAuthenticity::Signed(local_key);

        // Create a Swarm to manage peers and events
        let mut swarm = {
            // set default parameters for gossipsub
            let gossipsub_config = libp2p::gossipsub::Config::default();
            // build a gossipsub network behaviour
            let mut gossipsub: libp2p::gossipsub::Behaviour =
                libp2p::gossipsub::Behaviour::new(message_authenticity, gossipsub_config).unwrap();
            // subscribe to the topic
            let _ = gossipsub.subscribe(&self.topic);
            // create the swarm
            libp2p::swarm::SwarmBuilder::with_tokio_executor(transport, gossipsub, local_peer_id)
                .build()
        };

        // Listen on a memory transport.
        let memory: Multiaddr = libp2p::core::multiaddr::Protocol::Memory(0).into();
        let addr = swarm.listen_on(memory).unwrap();
        println!("Listening on {:?}", addr);

        self.swarm = Some(swarm);

        Ok(self)
    }
}
