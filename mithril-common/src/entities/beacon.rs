use crate::entities::{Epoch, ImmutableFileNumber};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

/// Beacon represents a point in the Cardano chain at which a Mithril certificate should be produced
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize, Hash)]
pub struct Beacon {
    /// Cardano network
    #[serde(rename = "network")]
    pub network: String,

    /// Cardano chain epoch number
    #[serde(rename = "epoch")]
    pub epoch: Epoch,

    /// Number of the last included immutable files for the digest computation
    #[serde(rename = "immutable_file_number")]
    pub immutable_file_number: ImmutableFileNumber,
}

impl PartialOrd for Beacon {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.network.partial_cmp(&other.network) {
            Some(core::cmp::Ordering::Equal) => {}
            _ord => return None,
        };
        match self.epoch.partial_cmp(&other.epoch) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        self.immutable_file_number
            .partial_cmp(&other.immutable_file_number)
    }
}

impl Beacon {
    /// Beacon factory
    pub fn new(network: String, epoch: u64, immutable_file_number: ImmutableFileNumber) -> Beacon {
        Beacon {
            network,
            epoch: Epoch(epoch),
            immutable_file_number,
        }
    }

    /// Computes the hash of a Beacon
    pub fn compute_hash(&self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(self.network.as_bytes());
        hasher.update(self.epoch.0.to_be_bytes());
        hasher.update(self.immutable_file_number.to_be_bytes());
        hex::encode(hasher.finalize())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cmp::Ordering;

    #[test]
    fn test_beacon_partial_ord_different_network() {
        let beacon1: Beacon = Beacon {
            network: "A".to_string(),
            epoch: Epoch(0),
            immutable_file_number: 0,
        };
        let beacon2: Beacon = Beacon {
            network: "B".to_string(),
            epoch: Epoch(0),
            immutable_file_number: 0,
        };

        assert!(beacon1.partial_cmp(&beacon2).is_none());
    }

    #[test]
    fn test_beacon_partial_ord_equal() {
        let beacon1: Beacon = Beacon {
            network: "A".to_string(),
            epoch: Epoch(0),
            immutable_file_number: 0,
        };

        assert_eq!(Some(Ordering::Equal), beacon1.partial_cmp(&beacon1));
    }

    #[test]
    fn test_beacon_partial_ord_same_epoch_less() {
        let beacon1: Beacon = Beacon {
            network: "A".to_string(),
            epoch: Epoch(0),
            immutable_file_number: 0,
        };
        let beacon2: Beacon = Beacon {
            network: "A".to_string(),
            epoch: Epoch(0),
            immutable_file_number: 1,
        };

        assert_eq!(Some(Ordering::Less), beacon1.partial_cmp(&beacon2));
    }

    #[test]
    fn test_beacon_partial_ord_same_epoch_greater() {
        let beacon1: Beacon = Beacon {
            network: "A".to_string(),
            epoch: Epoch(0),
            immutable_file_number: 1,
        };
        let beacon2: Beacon = Beacon {
            network: "A".to_string(),
            epoch: Epoch(0),
            immutable_file_number: 0,
        };

        assert_eq!(Some(Ordering::Greater), beacon1.partial_cmp(&beacon2));
    }

    #[test]
    fn test_beacon_partial_ord_cmp_epochs_less() {
        let beacon1: Beacon = Beacon {
            network: "A".to_string(),
            epoch: Epoch(0),
            immutable_file_number: 99,
        };
        let beacon2: Beacon = Beacon {
            network: "A".to_string(),
            epoch: Epoch(1),
            immutable_file_number: 99,
        };

        assert_eq!(Some(Ordering::Less), beacon1.partial_cmp(&beacon2));
    }

    #[test]
    fn test_beacon_compute_hash() {
        let hash_expected = "48cbf709b56204d8315aefd3a416b45398094f6fd51785c5b7dcaf7f35aacbfb";

        assert_eq!(
            hash_expected,
            Beacon::new("testnet".to_string(), 10, 100).compute_hash()
        );
        assert_ne!(
            hash_expected,
            Beacon::new("mainnet".to_string(), 10, 100).compute_hash()
        );
        assert_ne!(
            hash_expected,
            Beacon::new("testnet".to_string(), 20, 100).compute_hash()
        );
        assert_ne!(
            hash_expected,
            Beacon::new("testnet".to_string(), 10, 200).compute_hash()
        );
    }
}
