use crate::MagicId;
use serde::{Deserialize, Serialize};
use std::fmt::Display;

/// The Cardano Network that is being targeted
#[allow(clippy::enum_variant_names)]
#[derive(Debug, Copy, Clone, PartialEq, Serialize, Deserialize, Hash, Eq, PartialOrd)]
pub enum CardanoNetwork {
    /// The Cardano mainnet
    MainNet,

    /// The Cardano testnet
    TestNet(MagicId),

    /// A Cardano private devnet
    DevNet(MagicId),
}

impl Display for CardanoNetwork {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            CardanoNetwork::MainNet => write!(f, "mainnet"),
            CardanoNetwork::DevNet(_) => write!(f, "devnet"),
            CardanoNetwork::TestNet(_) => write!(f, "testnet"),
        }
    }
}
