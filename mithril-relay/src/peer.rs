use libp2p::{
    futures::StreamExt,
    gossipsub::{Behaviour, IdentTopic, MessageAuthenticity, MessageId},
    identity,
    swarm::SwarmEvent,
    tcp, Multiaddr, Swarm, Transport,
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

    pub async fn consume(&mut self) -> StdResult<Option<RegisterSignatureMessage>> {
        self.peer.consume().await
    }

    pub async fn start(self) -> StdResult<Self> {
        Ok(Self {
            peer: self.peer.start().await?,
        })
    }
}

pub struct Peer {
    pub topic: IdentTopic,
    pub swarm: Option<Swarm<Behaviour>>,
    pub addr: Option<Multiaddr>,
}

impl Peer {
    pub fn new(topic_name: &str) -> Self {
        Self {
            topic: IdentTopic::new(topic_name),
            swarm: None,
            addr: None,
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

    pub async fn start(mut self) -> StdResult<Self> {
        let local_key = identity::Keypair::generate_ed25519();
        let local_peer_id = local_key.public().to_peer_id();

        // Set up an encrypted TCP Transport over yamux
        // This is test transport (memory).
        let transport = tcp::tokio::Transport::new(tcp::Config::default())
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
            let res = gossipsub.subscribe(&self.topic).unwrap();
            print!("Subscription:{res:?}");

            // create the swarm
            libp2p::swarm::SwarmBuilder::with_tokio_executor(transport, gossipsub, local_peer_id)
                .build()
        };

        // Listen on a memory transport.
        let addr: Multiaddr = "/ip4/0.0.0.0/tcp/0".parse()?;
        let _listener_id = swarm.listen_on(addr.clone()).unwrap();

        loop {
            if let Some(SwarmEvent::NewListenAddr { address, .. }) = swarm.next().await {
                println!("Listening on {:?}", address);
                self.addr = Some(address);
                break;
            }
        }

        self.swarm = Some(swarm);

        Ok(self)
    }

    pub fn dial(&mut self, addr: Multiaddr) -> StdResult<()> {
        println!("Dialing {addr:?}");
        self.swarm.as_mut().unwrap().dial(addr)?;

        Ok(())
    }
}
