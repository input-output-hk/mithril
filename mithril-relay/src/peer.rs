use libp2p::{
    core::transport::{ListenerId, MemoryTransport},
    gossipsub::{IdentTopic, MessageAuthenticity},
    identity, Multiaddr, Transport,
};
use mithril_common::{messages::RegisterSignatureMessage, StdResult};

pub struct P2PClient {
    topic: IdentTopic,
}

impl P2PClient {
    pub fn new(topic_name: &str) -> Self {
        Self {
            topic: IdentTopic::new(topic_name),
        }
    }

    pub async fn consume(&self) -> StdResult<RegisterSignatureMessage> {
        todo!("P2PClient::consume")
    }

    pub fn start(&self) -> StdResult<ListenerId> {
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
        let memory: Multiaddr = libp2p::core::multiaddr::Protocol::Memory(10).into();
        let addr = swarm.listen_on(memory).unwrap();
        println!("Listening on {:?}", addr);

        Ok(addr)
    }
}
