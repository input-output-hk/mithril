use crate::entities::{Epoch, ImmutableFileNumber};
use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::fmt::{Display, Formatter};
use thiserror::Error;

/// TimePoint aggregate all types of point in the Cardano chain (aka 'beacons') at which a Mithril
/// certificate should be produced
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct TimePoint {
    /// Cardano chain epoch number
    pub epoch: Epoch,

    /// Number of the last included immutable files for the digest computation
    pub immutable_file_number: ImmutableFileNumber,
}

/// A TimePointComparison is the result of the comparison between a beacon and an oldest beacon.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TimePointComparison {
    /// The current beacon has a newer epoch than the older beacon.
    GreaterEpoch,
    /// The current beacon has a newer immutable file number than the older beacon.
    GreaterImmutableFileNumber,
    /// The current beacon has both a newer epoch and newer immutable file number than the older beacon.
    BothGreater,
    /// The current beacon has an equal epoch and immutable file number as the older beacon.
    Equal,
}

impl TimePointComparison {
    /// Returns true if this comparison result isn't equal.
    pub fn is_new_beacon(&self) -> bool {
        matches!(
            self,
            TimePointComparison::GreaterImmutableFileNumber
                | TimePointComparison::GreaterEpoch
                | TimePointComparison::BothGreater
        )
    }

    /// Returns true if this comparison have a greater epoch but an equal immutable file number.
    pub fn is_new_epoch(&self) -> bool {
        matches!(
            self,
            TimePointComparison::GreaterEpoch | TimePointComparison::BothGreater
        )
    }
}

/// [TimePoint::compare_to_older] related errors.
#[derive(Error, Debug, PartialEq, Eq)]
pub enum TimePointComparisonError {
    /// Error raised the newest beacon has older data than the "oldest" beacon (meaning something
    /// wrong is happening).
    #[error(
    "compare failed: the 'oldest' have both a newest epoch and immutable file number than the newest time point: newest [{0:?}] / oldest [{1:?}]"
    )]
    OlderThanPreviousTimePoint(TimePoint, TimePoint),
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

impl TimePoint {
    /// Beacon factory
    pub fn new(epoch: u64, immutable_file_number: ImmutableFileNumber) -> TimePoint {
        TimePoint {
            epoch: Epoch(epoch),
            immutable_file_number,
        }
    }

    /// This method returns a BeaconOrdering between self and the other beacon.
    ///
    /// This method should be called using the newest beacon available as it will fails if
    /// the current beacon have data older than the other beacon.
    pub fn compare_to_older(
        &self,
        other: &TimePoint,
    ) -> Result<TimePointComparison, TimePointComparisonError> {
        match (
            self.epoch.cmp(&other.epoch),
            self.immutable_file_number.cmp(&other.immutable_file_number),
        ) {
            (Ordering::Greater, Ordering::Greater) => Ok(TimePointComparison::BothGreater),
            (Ordering::Greater, Ordering::Equal) => Ok(TimePointComparison::GreaterEpoch),
            (Ordering::Equal, Ordering::Greater) => {
                Ok(TimePointComparison::GreaterImmutableFileNumber)
            }
            (Ordering::Equal, Ordering::Equal) => Ok(TimePointComparison::Equal),
            // Those cases should not be possible
            (Ordering::Less, Ordering::Less) => Err(
                TimePointComparisonError::OlderThanPreviousTimePoint(self.clone(), other.clone()),
            ),
            (Ordering::Less, Ordering::Greater) | (Ordering::Less, Ordering::Equal) => Err(
                TimePointComparisonError::OlderThanPreviousTimePoint(self.clone(), other.clone()),
            ),
            (Ordering::Greater, Ordering::Less) | (Ordering::Equal, Ordering::Less) => Err(
                TimePointComparisonError::OlderThanPreviousTimePoint(self.clone(), other.clone()),
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cmp::Ordering;

    #[test]
    fn beacon_ord_equal() {
        let beacon1 = TimePoint {
            epoch: Epoch(0),
            immutable_file_number: 0,
        };

        assert_eq!(Ordering::Equal, beacon1.cmp(&beacon1));
    }

    #[test]
    fn beacon_ord_same_epoch_less() {
        let beacon1 = TimePoint {
            epoch: Epoch(0),
            immutable_file_number: 0,
        };
        let beacon2 = TimePoint {
            epoch: Epoch(0),
            immutable_file_number: 1,
        };

        assert_eq!(Ordering::Less, beacon1.cmp(&beacon2));
    }

    #[test]
    fn beacon_ord_same_epoch_greater() {
        let beacon1 = TimePoint {
            epoch: Epoch(0),
            immutable_file_number: 1,
        };
        let beacon2 = TimePoint {
            epoch: Epoch(0),
            immutable_file_number: 0,
        };

        assert_eq!(Ordering::Greater, beacon1.cmp(&beacon2));
    }

    #[test]
    fn beacon_ord_cmp_epochs_less() {
        let beacon1 = TimePoint {
            epoch: Epoch(0),
            immutable_file_number: 99,
        };
        let beacon2 = TimePoint {
            epoch: Epoch(1),
            immutable_file_number: 99,
        };

        assert_eq!(Ordering::Less, beacon1.cmp(&beacon2));
    }

    #[test]
    fn time_point_compare_to_older_lower_epoch_greater_immutable() {
        let previous_beacon = TimePoint::new(1, 0);
        let current_beacon = TimePoint::new(0, 1);

        assert_eq!(
            TimePointComparisonError::OlderThanPreviousTimePoint(
                current_beacon.clone(),
                previous_beacon.clone()
            ),
            current_beacon
                .compare_to_older(&previous_beacon)
                .unwrap_err()
        );
    }

    #[test]
    fn time_point_compare_to_older_equal() {
        let beacon = TimePoint::new(0, 0);

        assert_eq!(
            Ok(TimePointComparison::Equal),
            beacon.compare_to_older(&beacon)
        );
    }

    #[test]
    fn time_point_compare_to_older_same_epoch_less_immutable() {
        let previous_beacon = TimePoint::new(0, 1);
        let current_beacon = TimePoint::new(0, 0);

        assert_eq!(
            TimePointComparisonError::OlderThanPreviousTimePoint(
                current_beacon.clone(),
                previous_beacon.clone()
            ),
            current_beacon
                .compare_to_older(&previous_beacon)
                .unwrap_err()
        );
    }

    #[test]
    fn time_point_compare_to_older_same_epoch_greater_immutable() {
        let previous_beacon = TimePoint::new(0, 0);
        let current_beacon = TimePoint::new(0, 1);

        assert_eq!(
            Ok(TimePointComparison::GreaterImmutableFileNumber),
            current_beacon.compare_to_older(&previous_beacon)
        )
    }

    #[test]
    fn time_point_compare_to_older_epochs_greater() {
        let previous_beacon = TimePoint::new(0, 99);
        let current_beacon = TimePoint::new(1, 99);

        assert_eq!(
            Ok(TimePointComparison::GreaterEpoch),
            current_beacon.compare_to_older(&previous_beacon)
        );
    }

    #[test]
    fn time_point_compare_to_older_lower_epochs() {
        let previous_beacon = TimePoint::new(1, 99);
        let current_beacon = TimePoint::new(0, 99);

        assert_eq!(
            TimePointComparisonError::OlderThanPreviousTimePoint(
                current_beacon.clone(),
                previous_beacon.clone()
            ),
            current_beacon
                .compare_to_older(&previous_beacon)
                .unwrap_err()
        );
    }

    #[test]
    fn time_point_ordering_is_new_beacon() {
        assert_eq!(
            (true, true, true, false),
            (
                TimePointComparison::GreaterEpoch.is_new_beacon(),
                TimePointComparison::GreaterImmutableFileNumber.is_new_beacon(),
                TimePointComparison::BothGreater.is_new_beacon(),
                TimePointComparison::Equal.is_new_beacon(),
            )
        );
    }

    #[test]
    fn time_point_ordering_is_new_epoch() {
        assert_eq!(
            (true, false, true, false),
            (
                TimePointComparison::GreaterEpoch.is_new_epoch(),
                TimePointComparison::GreaterImmutableFileNumber.is_new_epoch(),
                TimePointComparison::BothGreater.is_new_epoch(),
                TimePointComparison::Equal.is_new_epoch(),
            )
        );
    }
}
