use crate::entities::{Epoch, ImmutableFileNumber};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::cmp::Ordering;
use std::fmt::{Display, Formatter};
use thiserror::Error;

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

/// A BeaconComparison is the result of the comparison between a beacon and an oldest beacon.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BeaconComparison {
    /// The current beacon has a newer epoch than the older beacon.
    GreaterEpoch,
    /// The current beacon has a newer immutable file number than the older beacon.
    GreaterImmutableFileNumber,
    /// The current beacon has both a newer epoch and newer immutable file number than the older beacon.
    BothGreater,
    /// The current beacon has an equal epoch and immutable file number as the older beacon.
    Equal,
}

impl BeaconComparison {
    /// Returns true if this comparison result isn't equal.
    pub fn is_new_beacon(&self) -> bool {
        matches!(
            self,
            BeaconComparison::GreaterImmutableFileNumber
                | BeaconComparison::GreaterEpoch
                | BeaconComparison::BothGreater
        )
    }

    /// Returns true if this comparison have a greater epoch but an equal immutable file number.
    pub fn is_new_epoch(&self) -> bool {
        matches!(
            self,
            BeaconComparison::GreaterEpoch | BeaconComparison::BothGreater
        )
    }
}

/// [Beacon::compare_to_older] related errors.
#[derive(Error, Debug, PartialEq, Eq)]
pub enum BeaconComparisonError {
    /// Error raised when a comparison between beacons from different networks is made.
    #[error("can't compare: those beacons are issued by different network: {0} != {1}")]
    NetworkNotMatch(String, String),

    /// Error raised the newest beacon has oldest data than the "oldest" beacon (meaning something
    /// wrong is happening).
    #[error(
        "compare failed: the 'oldest' have both a newest epoch and immutable file number than the newest beacon: newest [{0:?}] / oldest [{1:?}]"
    )]
    BeaconOlderThanPreviousBeacon(Beacon, Beacon),
}

impl PartialOrd for Beacon {
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

impl Display for Beacon {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Beacon (network: {}, epoch: {}, immutable_file_number: {})",
            self.network, self.epoch, self.immutable_file_number
        )
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

    /// This method returns a BeaconOrdering between self and the other beacon.
    ///
    /// This method should be called using the newest beacon available as it will fails if
    /// the current beacon have data older than the other beacon.
    pub fn compare_to_older(
        &self,
        other: &Beacon,
    ) -> Result<BeaconComparison, BeaconComparisonError> {
        if self.network != other.network {
            return Err(BeaconComparisonError::NetworkNotMatch(
                self.network.clone(),
                other.network.clone(),
            ));
        }

        match (
            self.epoch.cmp(&other.epoch),
            self.immutable_file_number.cmp(&other.immutable_file_number),
        ) {
            (Ordering::Greater, Ordering::Greater) => Ok(BeaconComparison::BothGreater),
            (Ordering::Greater, Ordering::Equal) => Ok(BeaconComparison::GreaterEpoch),
            (Ordering::Equal, Ordering::Greater) => {
                Ok(BeaconComparison::GreaterImmutableFileNumber)
            }
            (Ordering::Equal, Ordering::Equal) => Ok(BeaconComparison::Equal),
            // Those cases should not be possible
            (Ordering::Less, Ordering::Less) => Err(
                BeaconComparisonError::BeaconOlderThanPreviousBeacon(self.clone(), other.clone()),
            ),
            (Ordering::Less, Ordering::Greater) | (Ordering::Less, Ordering::Equal) => Err(
                BeaconComparisonError::BeaconOlderThanPreviousBeacon(self.clone(), other.clone()),
            ),
            (Ordering::Greater, Ordering::Less) | (Ordering::Equal, Ordering::Less) => Err(
                BeaconComparisonError::BeaconOlderThanPreviousBeacon(self.clone(), other.clone()),
            ),
        }
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
    fn test_beacon_compare_to_older_different_network() {
        let beacon1: Beacon = Beacon::new("A".to_string(), 0, 0);
        let beacon2: Beacon = Beacon::new("B".to_string(), 0, 0);

        assert_eq!(
            BeaconComparisonError::NetworkNotMatch("A".to_string(), "B".to_string()),
            beacon1.compare_to_older(&beacon2).unwrap_err()
        );
    }

    #[test]
    fn test_beacon_compare_to_older_lower_epoch_greater_immutable() {
        // put this case in the doc
        let previous_beacon: Beacon = Beacon::new("A".to_string(), 1, 0);
        let current_beacon: Beacon = Beacon::new("A".to_string(), 0, 1);

        assert_eq!(
            BeaconComparisonError::BeaconOlderThanPreviousBeacon(
                current_beacon.clone(),
                previous_beacon.clone()
            ),
            current_beacon
                .compare_to_older(&previous_beacon)
                .unwrap_err()
        );
    }

    #[test]
    fn test_beacon_compare_to_older_equal() {
        let beacon: Beacon = Beacon::new("A".to_string(), 0, 0);

        assert_eq!(
            Ok(BeaconComparison::Equal),
            beacon.compare_to_older(&beacon)
        );
    }

    #[test]
    fn test_beacon_compare_to_older_same_epoch_less_immutable() {
        let previous_beacon: Beacon = Beacon::new("A".to_string(), 0, 1);
        let current_beacon: Beacon = Beacon::new("A".to_string(), 0, 0);

        assert_eq!(
            BeaconComparisonError::BeaconOlderThanPreviousBeacon(
                current_beacon.clone(),
                previous_beacon.clone()
            ),
            current_beacon
                .compare_to_older(&previous_beacon)
                .unwrap_err()
        );
    }

    #[test]
    fn test_beacon_compare_to_older_same_epoch_greater_immutable() {
        let previous_beacon: Beacon = Beacon::new("A".to_string(), 0, 0);
        let current_beacon: Beacon = Beacon::new("A".to_string(), 0, 1);

        assert_eq!(
            Ok(BeaconComparison::GreaterImmutableFileNumber),
            current_beacon.compare_to_older(&previous_beacon)
        )
    }

    #[test]
    fn test_beacon_compare_to_older_epochs_greater() {
        let previous_beacon: Beacon = Beacon::new("A".to_string(), 0, 99);
        let current_beacon: Beacon = Beacon::new("A".to_string(), 1, 99);

        assert_eq!(
            Ok(BeaconComparison::GreaterEpoch),
            current_beacon.compare_to_older(&previous_beacon)
        );
    }

    #[test]
    fn test_beacon_compare_to_older_epochs_less() {
        let previous_beacon: Beacon = Beacon::new("A".to_string(), 1, 99);
        let current_beacon: Beacon = Beacon::new("A".to_string(), 0, 99);

        assert_eq!(
            BeaconComparisonError::BeaconOlderThanPreviousBeacon(
                current_beacon.clone(),
                previous_beacon.clone()
            ),
            current_beacon
                .compare_to_older(&previous_beacon)
                .unwrap_err()
        );
    }

    #[test]
    fn test_beacon_ordering_is_new_beacon() {
        assert_eq!(
            (true, true, true, false),
            (
                BeaconComparison::GreaterEpoch.is_new_beacon(),
                BeaconComparison::GreaterImmutableFileNumber.is_new_beacon(),
                BeaconComparison::BothGreater.is_new_beacon(),
                BeaconComparison::Equal.is_new_beacon(),
            )
        );
    }

    #[test]
    fn test_beacon_ordering_is_new_epoch() {
        assert_eq!(
            (true, false, true, false),
            (
                BeaconComparison::GreaterEpoch.is_new_epoch(),
                BeaconComparison::GreaterImmutableFileNumber.is_new_epoch(),
                BeaconComparison::BothGreater.is_new_epoch(),
                BeaconComparison::Equal.is_new_epoch(),
            )
        );
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
