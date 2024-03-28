use crate::entities::{Epoch, ImmutableFileNumber};
use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::fmt::{Display, Formatter};
use thiserror::Error;

/// TimePoint aggregates all types of point in the Cardano chain and is used by the state machines
/// for their computations.
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct TimePoint {
    /// Cardano chain epoch number
    pub epoch: Epoch,

    /// Number of the last immutable files used for the digest computation
    pub immutable_file_number: ImmutableFileNumber,
}

/// A TimePointComparison is the result of the comparison between a time point and an older time point.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TimePointComparison {
    /// The current time point has a newer epoch than the older time point.
    GreaterEpoch,
    /// The current time point has a newer immutable file number than the older time point.
    GreaterImmutableFileNumber,
    /// The current time point has both a newer epoch and newer immutable file number than the older time point.
    BothGreater,
    /// The current time point has an equal epoch and immutable file number as the older time point.
    Equal,
}

impl TimePointComparison {
    /// Returns true if this comparison result isn't equal.
    pub fn is_newer(&self) -> bool {
        matches!(
            self,
            TimePointComparison::GreaterImmutableFileNumber
                | TimePointComparison::GreaterEpoch
                | TimePointComparison::BothGreater
        )
    }

    /// Returns true if this comparison have a greater epoch.
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
    /// Error raised the newest time point has older data than the "oldest" time point (meaning something
    /// wrong is happening).
    #[error(
    "compare failed: the 'oldest' has both a newest epoch and immutable file number than the newest time point: newest [{0:?}] / oldest [{1:?}]"
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

    /// This method returns a [TimePointComparison] between self and the other time point.
    ///
    /// This method should be called using the newest time point available as it will fail if
    /// the current time point have data older than the other time point.
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

    #[test]
    fn time_point_compare_to_older_lower_epoch_greater_immutable() {
        let previous_time_point = TimePoint::new(1, 0);
        let current_time_point = TimePoint::new(0, 1);

        assert_eq!(
            TimePointComparisonError::OlderThanPreviousTimePoint(
                current_time_point.clone(),
                previous_time_point.clone()
            ),
            current_time_point
                .compare_to_older(&previous_time_point)
                .unwrap_err()
        );
    }

    #[test]
    fn time_point_compare_to_older_equal() {
        let time_point = TimePoint::new(0, 0);

        assert_eq!(
            Ok(TimePointComparison::Equal),
            time_point.compare_to_older(&time_point)
        );
    }

    #[test]
    fn time_point_compare_to_older_same_epoch_less_immutable() {
        let previous_time_point = TimePoint::new(0, 1);
        let current_time_point = TimePoint::new(0, 0);

        assert_eq!(
            TimePointComparisonError::OlderThanPreviousTimePoint(
                current_time_point.clone(),
                previous_time_point.clone()
            ),
            current_time_point
                .compare_to_older(&previous_time_point)
                .unwrap_err()
        );
    }

    #[test]
    fn time_point_compare_to_older_same_epoch_greater_immutable() {
        let previous_time_point = TimePoint::new(0, 0);
        let current_time_point = TimePoint::new(0, 1);

        assert_eq!(
            Ok(TimePointComparison::GreaterImmutableFileNumber),
            current_time_point.compare_to_older(&previous_time_point)
        )
    }

    #[test]
    fn time_point_compare_to_older_epochs_greater() {
        let previous_time_point = TimePoint::new(0, 99);
        let current_time_point = TimePoint::new(1, 99);

        assert_eq!(
            Ok(TimePointComparison::GreaterEpoch),
            current_time_point.compare_to_older(&previous_time_point)
        );
    }

    #[test]
    fn time_point_compare_to_older_lower_epochs() {
        let previous_time_point = TimePoint::new(1, 99);
        let current_time_point = TimePoint::new(0, 99);

        assert_eq!(
            TimePointComparisonError::OlderThanPreviousTimePoint(
                current_time_point.clone(),
                previous_time_point.clone()
            ),
            current_time_point
                .compare_to_older(&previous_time_point)
                .unwrap_err()
        );
    }

    #[test]
    fn time_point_comparison_is_newer() {
        assert!(TimePointComparison::GreaterEpoch.is_newer());
        assert!(TimePointComparison::GreaterImmutableFileNumber.is_newer());
        assert!(TimePointComparison::BothGreater.is_newer());
        assert!(!TimePointComparison::Equal.is_newer());
    }

    #[test]
    fn time_point_comparison_is_new_epoch() {
        assert!(TimePointComparison::GreaterEpoch.is_newer());
        assert!(!TimePointComparison::GreaterImmutableFileNumber.is_newer());
        assert!(TimePointComparison::BothGreater.is_newer());
        assert!(!TimePointComparison::Equal.is_newer());
    }
}
