use std::cmp::Ordering;
use std::fmt::{Display, Formatter};

use crate::entities::{ChainPoint, Epoch, ImmutableFileNumber};

/// TimePoint aggregates all types of point in the Cardano chain and is used by the state machines
/// for their computations.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TimePoint {
    /// Cardano chain epoch number
    pub epoch: Epoch,

    /// Number of the last immutable files used for the digest computation
    pub immutable_file_number: ImmutableFileNumber,

    /// Chain point
    pub chain_point: ChainPoint,
}

impl TimePoint {
    /// [TimePoint] factory
    pub fn new(
        epoch: u64,
        immutable_file_number: ImmutableFileNumber,
        chain_point: ChainPoint,
    ) -> TimePoint {
        TimePoint {
            epoch: Epoch(epoch),
            immutable_file_number,
            chain_point,
        }
    }

    cfg_test_tools! {
        /// Create a dummy TimePoint
        pub fn dummy() -> Self {
            Self::new(10, 100, ChainPoint::dummy())
        }
    }
}

impl PartialOrd for TimePoint {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TimePoint {
    fn cmp(&self, other: &Self) -> Ordering {
        self.epoch
            .cmp(&other.epoch)
            .then(self.immutable_file_number.cmp(&other.immutable_file_number))
            .then(self.chain_point.cmp(&other.chain_point))
    }
}

impl Display for TimePoint {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "TimePoint (epoch: {}, immutable_file_number: {}, chain_point: {})",
            self.epoch, self.immutable_file_number, self.chain_point
        )
    }
}

#[cfg(test)]
mod tests {
    use std::cmp::Ordering;

    use super::*;

    #[test]
    fn time_point_ord_cmp_epochs_take_precedence_over_other_fields() {
        let time_point1 = TimePoint {
            epoch: Epoch(5),
            immutable_file_number: 0,
            chain_point: ChainPoint {
                slot_number: 10,
                block_number: 20,
                block_hash: "hash1".to_string(),
            },
        };
        let time_point2 = TimePoint {
            epoch: Epoch(0),
            immutable_file_number: 1,
            chain_point: ChainPoint {
                slot_number: 15,
                block_number: 25,
                block_hash: "hash2".to_string(),
            },
        };

        assert_eq!(Ordering::Greater, time_point1.cmp(&time_point2));
    }

    #[test]
    fn time_point_ord_cmp_if_epoch_equals_then_immutable_take_precedence_over_chain_point() {
        let time_point1 = TimePoint {
            epoch: Epoch(0),
            immutable_file_number: 5,
            chain_point: ChainPoint {
                slot_number: 10,
                block_number: 20,
                block_hash: "hash1".to_string(),
            },
        };
        let time_point2 = TimePoint {
            epoch: Epoch(0),
            immutable_file_number: 0,
            chain_point: ChainPoint {
                slot_number: 15,
                block_number: 25,
                block_hash: "hash2".to_string(),
            },
        };

        assert_eq!(Ordering::Greater, time_point1.cmp(&time_point2));
    }

    #[test]
    fn time_point_ord_cmp_if_epoch_and_immutables_equals_then_compare_over_chain_points() {
        let time_point1 = TimePoint {
            epoch: Epoch(0),
            immutable_file_number: 0,
            chain_point: ChainPoint {
                slot_number: 10,
                block_number: 20,
                block_hash: "hash1".to_string(),
            },
        };
        let time_point2 = TimePoint {
            epoch: Epoch(0),
            immutable_file_number: 0,
            chain_point: ChainPoint {
                slot_number: 15,
                block_number: 25,
                block_hash: "hash2".to_string(),
            },
        };

        assert_eq!(Ordering::Less, time_point1.cmp(&time_point2));
    }
}
