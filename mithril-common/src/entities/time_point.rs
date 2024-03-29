use crate::entities::{Epoch, ImmutableFileNumber};
use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::fmt::{Display, Formatter};

/// TimePoint aggregates all types of point in the Cardano chain and is used by the state machines
/// for their computations.
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct TimePoint {
    /// Cardano chain epoch number
    pub epoch: Epoch,

    /// Number of the last immutable files used for the digest computation
    pub immutable_file_number: ImmutableFileNumber,
}

impl TimePoint {
    /// [TimePoint] factory
    pub fn new(epoch: u64, immutable_file_number: ImmutableFileNumber) -> TimePoint {
        TimePoint {
            epoch: Epoch(epoch),
            immutable_file_number,
        }
    }

    cfg_test_tools! {
        /// Create a dummy TimePoint
        pub fn dummy() -> Self {
            Self::new(10, 100)
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
    }
}

impl Display for TimePoint {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "TimePoint (epoch: {}, immutable_file_number: {})",
            self.epoch, self.immutable_file_number
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cmp::Ordering;

    #[test]
    fn time_point_ord_equal() {
        let time_point1 = TimePoint {
            epoch: Epoch(0),
            immutable_file_number: 0,
        };

        assert_eq!(Ordering::Equal, time_point1.cmp(&time_point1));
    }

    #[test]
    fn time_point_ord_same_epoch_less() {
        let time_point1 = TimePoint {
            epoch: Epoch(0),
            immutable_file_number: 0,
        };
        let time_point2 = TimePoint {
            epoch: Epoch(0),
            immutable_file_number: 1,
        };

        assert_eq!(Ordering::Less, time_point1.cmp(&time_point2));
    }

    #[test]
    fn time_point_ord_same_epoch_greater() {
        let time_point1 = TimePoint {
            epoch: Epoch(0),
            immutable_file_number: 1,
        };
        let time_point2 = TimePoint {
            epoch: Epoch(0),
            immutable_file_number: 0,
        };

        assert_eq!(Ordering::Greater, time_point1.cmp(&time_point2));
    }

    #[test]
    fn time_point_ord_cmp_epochs_less() {
        let time_point1 = TimePoint {
            epoch: Epoch(0),
            immutable_file_number: 99,
        };
        let time_point2 = TimePoint {
            epoch: Epoch(1),
            immutable_file_number: 99,
        };

        assert_eq!(Ordering::Less, time_point1.cmp(&time_point2));
    }
}
