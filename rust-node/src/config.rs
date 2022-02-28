use crate::message;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Clone)]
pub struct Config {
    pub nodes: Vec<NodeConfig>,
    pub parameters: message::Parameters,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct NodeConfig {
    pub id: message::PartyId,
    pub address_local: String,
    pub address_endpoint: String,
    pub stake: u64,
}