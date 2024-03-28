use crate::entities::{Epoch, ImmutableFileNumber};
use crate::signable_builder::Beacon;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::cmp::Ordering;
use std::fmt::{Display, Formatter};

/// A point in the Cardano chain at which a Mithril certificate of the Cardano Database should be
/// produced.
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize, Hash)]
pub struct CardanoDbBeacon {
    // todo: remove network (we only need it as metadata of the certificates)
    /// Cardano network
    pub network: String,

    /// Cardano chain epoch number
    pub epoch: Epoch,

    /// Number of the last included immutable files for the digest computation
    pub immutable_file_number: ImmutableFileNumber,
}

impl Beacon for CardanoDbBeacon {}

impl PartialOrd for CardanoDbBeacon {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match self.network.partial_cmp(&other.network) {
            Some(Ordering::Equal) => {}
            _ord => return None,
        };
        match self.epoch.partial_cmp(&other.epoch) {
            Some(Ordering::Equal) => {}
            ord => return ord,
        }
        self.immutable_file_number
            .partial_cmp(&other.immutable_file_number)
    }
}

impl Display for CardanoDbBeacon {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "CardanoDbBeacon (network: {}, epoch: {}, immutable_file_number: {})",
            self.network, self.epoch, self.immutable_file_number
        )
    }
}

impl CardanoDbBeacon {
    /// CardanoDbBeacon factory
    pub fn new<T: Into<String>>(
        network: T,
        epoch: u64,
        immutable_file_number: ImmutableFileNumber,
    ) -> CardanoDbBeacon {
        CardanoDbBeacon {
            network: network.into(),
            epoch: Epoch(epoch),
            immutable_file_number,
        }
    }

    /// Computes the hash of a CardanoDbBeacon
    pub fn compute_hash(&self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(self.network.as_bytes());
        hasher.update(self.epoch.to_be_bytes());
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
        let beacon1: CardanoDbBeacon = CardanoDbBeacon {
            network: "A".to_string(),
            epoch: Epoch(0),
            immutable_file_number: 0,
        };
        let beacon2: CardanoDbBeacon = CardanoDbBeacon {
            network: "B".to_string(),
            epoch: Epoch(0),
            immutable_file_number: 0,
        };

        assert!(beacon1.partial_cmp(&beacon2).is_none());
    }

    #[test]
    fn test_beacon_partial_ord_equal() {
        let beacon1: CardanoDbBeacon = CardanoDbBeacon {
            network: "A".to_string(),
            epoch: Epoch(0),
            immutable_file_number: 0,
        };

        assert_eq!(Some(Ordering::Equal), beacon1.partial_cmp(&beacon1));
    }

    #[test]
    fn test_beacon_partial_ord_same_epoch_less() {
        let beacon1: CardanoDbBeacon = CardanoDbBeacon {
            network: "A".to_string(),
            epoch: Epoch(0),
            immutable_file_number: 0,
        };
        let beacon2: CardanoDbBeacon = CardanoDbBeacon {
            network: "A".to_string(),
            epoch: Epoch(0),
            immutable_file_number: 1,
        };

        assert_eq!(Some(Ordering::Less), beacon1.partial_cmp(&beacon2));
    }

    #[test]
    fn test_beacon_partial_ord_same_epoch_greater() {
        let beacon1: CardanoDbBeacon = CardanoDbBeacon {
            network: "A".to_string(),
            epoch: Epoch(0),
            immutable_file_number: 1,
        };
        let beacon2: CardanoDbBeacon = CardanoDbBeacon {
            network: "A".to_string(),
            epoch: Epoch(0),
            immutable_file_number: 0,
        };

        assert_eq!(Some(Ordering::Greater), beacon1.partial_cmp(&beacon2));
    }

    #[test]
    fn test_beacon_partial_ord_cmp_epochs_less() {
        let beacon1: CardanoDbBeacon = CardanoDbBeacon {
            network: "A".to_string(),
            epoch: Epoch(0),
            immutable_file_number: 99,
        };
        let beacon2: CardanoDbBeacon = CardanoDbBeacon {
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
            CardanoDbBeacon::new("testnet".to_string(), 10, 100).compute_hash()
        );
        assert_ne!(
            hash_expected,
            CardanoDbBeacon::new("mainnet".to_string(), 10, 100).compute_hash()
        );
        assert_ne!(
            hash_expected,
            CardanoDbBeacon::new("testnet".to_string(), 20, 100).compute_hash()
        );
        assert_ne!(
            hash_expected,
            CardanoDbBeacon::new("testnet".to_string(), 10, 200).compute_hash()
        );
    }
}
