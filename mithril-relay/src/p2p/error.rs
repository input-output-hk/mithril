use thiserror::Error;

/// [Peer][crate::p2p::Peer] related errors.
#[derive(Debug, Error)]
pub enum PeerError {
    /// Topic does not exist
    #[error("topic does not exist")]
    MissingTopic(),

    /// No available swarm
    #[error("no available swarm")]
    UnavailableSwarm(),
}
