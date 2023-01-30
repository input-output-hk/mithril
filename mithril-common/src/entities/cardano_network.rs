use serde::{Deserialize, Serialize};
use std::fmt::Display;
use thiserror::Error;

use crate::MagicId;

const TESTNET_MAGIC_ID: MagicId = 1097911063;
const PREPROD_MAGIC_ID: MagicId = 1;
const PREVIEW_MAGIC_ID: MagicId = 2;

#[derive(Error, Debug)]
pub enum CardanoNetworkError {
    #[error("parse from code error: '{0}'")]
    ParseFromCode(String),
}

/// The Cardano Network that is being targeted
#[allow(clippy::enum_variant_names)]
#[derive(Debug, Copy, Clone, PartialEq, Serialize, Deserialize, Hash, Eq, PartialOrd)]
pub enum CardanoNetwork {
    /// The Cardano mainnet network
    MainNet,

    /// A Cardano test network (testnet, preview, or preprod)
    TestNet(MagicId),

    /// A Cardano private devnet
    DevNet(MagicId),
}

impl CardanoNetwork {
    /// Instantiates a CardanoNetwork from its code and magic id
    pub fn from_code(
        network_code: String,
        network_magic: Option<u64>,
    ) -> Result<CardanoNetwork, CardanoNetworkError> {
        match network_code.to_lowercase().as_str() {
            "mainnet" => Ok(CardanoNetwork::MainNet),
            "testnet" => Ok(CardanoNetwork::TestNet(TESTNET_MAGIC_ID)),
            "preview" => Ok(CardanoNetwork::TestNet(PREVIEW_MAGIC_ID)),
            "preprod" => Ok(CardanoNetwork::TestNet(PREPROD_MAGIC_ID)),
            "private" => {
                if let Some(magic) = network_magic {
                    Ok(CardanoNetwork::TestNet(magic))
                } else {
                    Err(CardanoNetworkError::ParseFromCode(
                        "no NETWORK MAGIC number given for test network".to_string(),
                    ))
                }
            }
            "devnet" => {
                if let Some(magic) = network_magic {
                    Ok(CardanoNetwork::DevNet(magic))
                } else {
                    Err(CardanoNetworkError::ParseFromCode(
                        "no NETWORK MAGIC number given for devnet network".to_string(),
                    ))
                }
            }
            what => Err(CardanoNetworkError::ParseFromCode(format!(
                "could not parse network '{what}', the only recognized networks are: mainnet, devnet, testnet, preview, preprod and private"
            ))),
        }
    }
}

impl Display for CardanoNetwork {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            CardanoNetwork::MainNet => write!(f, "mainnet"),
            CardanoNetwork::DevNet(_) => write!(f, "devnet"),
            CardanoNetwork::TestNet(magic_id) => match magic_id {
                TESTNET_MAGIC_ID => write!(f, "testnet"),
                PREVIEW_MAGIC_ID => write!(f, "preview"),
                PREPROD_MAGIC_ID => write!(f, "preprod"),
                _ => write!(f, "private"),
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cardano_network_from_code() {
        assert_eq!(
            CardanoNetwork::from_code("mainnet".to_string(), None).unwrap(),
            CardanoNetwork::MainNet
        );
        assert_eq!(
            CardanoNetwork::from_code("mainnet".to_string(), Some(123)).unwrap(),
            CardanoNetwork::MainNet
        );
        assert_eq!(
            CardanoNetwork::from_code("preview".to_string(), None).unwrap(),
            CardanoNetwork::TestNet(PREVIEW_MAGIC_ID)
        );
        assert_eq!(
            CardanoNetwork::from_code("preview".to_string(), Some(123)).unwrap(),
            CardanoNetwork::TestNet(PREVIEW_MAGIC_ID)
        );
        assert_eq!(
            CardanoNetwork::from_code("preprod".to_string(), None).unwrap(),
            CardanoNetwork::TestNet(PREPROD_MAGIC_ID)
        );
        assert_eq!(
            CardanoNetwork::from_code("preprod".to_string(), Some(123)).unwrap(),
            CardanoNetwork::TestNet(PREPROD_MAGIC_ID)
        );
        assert_eq!(
            CardanoNetwork::from_code("testnet".to_string(), None).unwrap(),
            CardanoNetwork::TestNet(TESTNET_MAGIC_ID)
        );
        assert_eq!(
            CardanoNetwork::from_code("testnet".to_string(), Some(123)).unwrap(),
            CardanoNetwork::TestNet(TESTNET_MAGIC_ID)
        );
        assert_eq!(
            CardanoNetwork::from_code("private".to_string(), Some(123)).unwrap(),
            CardanoNetwork::TestNet(123)
        );
        assert!(CardanoNetwork::from_code("private".to_string(), None).is_err());
    }
}
