//! Helpers when working with parameters from either the query string or the path of an HTTP request

use std::ops::Deref;

use mithril_common::StdResult;
use mithril_common::entities::{Epoch, EpochSpecifier};

use crate::dependency_injection::EpochServiceWrapper;

/// An epoch parsed from a http request parameter
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExpandedEpoch {
    /// Epoch was explicitly provided as a number (e.g., "123")
    Parsed(Epoch),
    /// Epoch was provided as "latest" (e.g., "latest")
    Latest(Epoch),
    /// Epoch was provided as "latest-{offset}" (e.g., "latest-100")
    LatestMinusOffset(Epoch, u64),
}

impl Deref for ExpandedEpoch {
    type Target = Epoch;

    fn deref(&self) -> &Self::Target {
        match self {
            ExpandedEpoch::Parsed(epoch)
            | ExpandedEpoch::Latest(epoch)
            | ExpandedEpoch::LatestMinusOffset(epoch, _) => epoch,
        }
    }
}

impl ExpandedEpoch {
    /// Returns true if this epoch was expanded from 'latest-{offset}' and the offset
    /// is greater than the given value.
    pub fn has_offset_greater_than(&self, value: u64) -> bool {
        match self {
            ExpandedEpoch::Parsed(_) | ExpandedEpoch::Latest(_) => false,
            ExpandedEpoch::LatestMinusOffset(_, offset) => offset > &value,
        }
    }

    /// Apply an additional negative offset to the epoch, but only if it was derived from 'latest'.
    pub fn apply_offset_for_latest(&self, additional_offset: u64) -> Epoch {
        match self {
            ExpandedEpoch::Parsed(epoch) => *epoch,
            ExpandedEpoch::Latest(epoch) | ExpandedEpoch::LatestMinusOffset(epoch, ..) => {
                Epoch(epoch.saturating_sub(additional_offset))
            }
        }
    }
}

/// Parse the string into an [Epoch] if it's a number, or if it's 'latest{-{offset}}' expand
/// into the actual epoch minus the optional given offset.
///
/// Return the expanded epoch and the eventual offset that was applied
pub async fn expand_epoch(
    epoch_str: &str,
    epoch_service: EpochServiceWrapper,
) -> StdResult<ExpandedEpoch> {
    let epoch_str = epoch_str.to_lowercase();
    match EpochSpecifier::parse(&epoch_str)? {
        EpochSpecifier::Number(epoch) => Ok(ExpandedEpoch::Parsed(epoch)),
        EpochSpecifier::Latest => epoch_service
            .read()
            .await
            .epoch_of_current_data()
            .map(ExpandedEpoch::Latest),
        EpochSpecifier::LatestMinusOffset(offset) => {
            epoch_service.read().await.epoch_of_current_data().map(|epoch| {
                ExpandedEpoch::LatestMinusOffset(Epoch(epoch.saturating_sub(offset)), offset)
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;
    use tokio::sync::RwLock;

    use crate::services::FakeEpochServiceBuilder;

    use super::*;

    fn fake_epoch_service(returned_epoch: Epoch) -> EpochServiceWrapper {
        Arc::new(RwLock::new(
            FakeEpochServiceBuilder::dummy(returned_epoch).build(),
        ))
    }

    #[tokio::test]
    async fn use_given_epoch_if_valid_number() {
        let epoch_service = fake_epoch_service(Epoch(300));
        let expanded_epoch = expand_epoch("456", epoch_service).await.unwrap();

        assert_eq!(expanded_epoch, ExpandedEpoch::Parsed(Epoch(456)));
    }

    #[tokio::test]
    async fn use_epoch_service_current_epoch_if_latest() {
        let epoch_service = fake_epoch_service(Epoch(89));
        let expanded_epoch = expand_epoch("latest", epoch_service).await.unwrap();

        assert_eq!(expanded_epoch, ExpandedEpoch::Latest(Epoch(89)));
    }

    #[tokio::test]
    async fn use_offset_epoch_service_current_epoch_if_latest_minus_a_number() {
        let epoch_service = fake_epoch_service(Epoch(89));
        let expanded_epoch = expand_epoch("latest-13", epoch_service.clone()).await.unwrap();
        assert_eq!(
            expanded_epoch,
            ExpandedEpoch::LatestMinusOffset(Epoch(76), 13)
        );

        let expanded_epoch = expand_epoch("latest-0", epoch_service).await.unwrap();
        assert_eq!(
            expanded_epoch,
            ExpandedEpoch::LatestMinusOffset(Epoch(89), 0)
        );
    }

    #[tokio::test]
    async fn error_if_given_epoch_is_not_a_number_nor_latest_nor_latest_minus_a_number() {
        let epoch_service = fake_epoch_service(Epoch(78));
        for invalid_epoch in [
            "invalid",
            "latest-",
            "latest+",
            "latest+0",
            "latest+293",
            "latest-4.5",
            "latest+2.9",
            "latest-invalid",
        ] {
            expand_epoch(invalid_epoch, epoch_service.clone()).await.expect_err(
                "Should fail if epoch is not a number nor 'latest' nor 'latest-{offset}'",
            );
        }
    }

    #[tokio::test]
    async fn dont_overflow_if_epoch_minus_offset_is_negative() {
        let epoch_service = fake_epoch_service(Epoch(89));
        let expanded_epoch = expand_epoch(&format!("latest-{}", u64::MAX), epoch_service)
            .await
            .unwrap();

        assert_eq!(
            expanded_epoch,
            ExpandedEpoch::LatestMinusOffset(Epoch(0), u64::MAX)
        );
    }

    #[test]
    fn test_expanded_epoch_is_parsed_offset_greater_than() {
        assert!(!ExpandedEpoch::Parsed(Epoch(90)).has_offset_greater_than(15));
        assert!(!ExpandedEpoch::Latest(Epoch(90)).has_offset_greater_than(15));
        assert!(!ExpandedEpoch::LatestMinusOffset(Epoch(90), 10).has_offset_greater_than(10));
        assert!(ExpandedEpoch::LatestMinusOffset(Epoch(90), 11).has_offset_greater_than(10));
    }

    #[test]
    fn test_expanded_epoch_apply_additional_offset() {
        assert_eq!(
            ExpandedEpoch::Parsed(Epoch(90)).apply_offset_for_latest(15),
            Epoch(90)
        );
        assert_eq!(
            ExpandedEpoch::Latest(Epoch(90)).apply_offset_for_latest(15),
            Epoch(75)
        );
        assert_eq!(
            ExpandedEpoch::LatestMinusOffset(Epoch(90), 13).apply_offset_for_latest(15),
            Epoch(75)
        );
        assert_eq!(
            ExpandedEpoch::Latest(Epoch(90)).apply_offset_for_latest(u64::MAX),
            Epoch(0)
        );
    }
}
