use anyhow::Context;
use std::collections::BTreeMap;
use std::sync::Arc;
use tokio::sync::RwLock;

use mithril_common::StdResult;
use mithril_common::entities::Epoch;

use crate::interface::MithrilNetworkConfigurationProvider;
use crate::model::{MithrilNetworkConfiguration, MithrilNetworkConfigurationForEpoch};

/// A fake [MithrilNetworkConfigurationProvider] that return [MithrilNetworkConfiguration] based
/// on configurable markers.
#[derive(Clone, Default)]
pub struct FakeMithrilNetworkConfigurationProviderWithEpochMarkers {
    markers: Arc<RwLock<BTreeMap<Epoch, MithrilNetworkConfigurationForEpoch>>>,
}

impl FakeMithrilNetworkConfigurationProviderWithEpochMarkers {
    /// Create a new [FakeMithrilNetworkConfigurationProviderWithEpochMarkers] with the given markers.
    pub fn new(markers: BTreeMap<Epoch, MithrilNetworkConfigurationForEpoch>) -> Self {
        Self {
            markers: Arc::new(RwLock::new(markers)),
        }
    }

    /// Insert a marker for the given epoch.
    ///
    /// If a marker already exists for the given epoch, it will be replaced.
    pub async fn insert_marker(&self, epoch: Epoch, marker: MithrilNetworkConfigurationForEpoch) {
        let mut markers = self.markers.write().await;
        markers.insert(epoch, marker);
    }

    /// Get the marker that applies for the given epoch.
    ///
    /// If no applicable marker is found, it returns None.
    pub async fn get_marker_at_or_before(
        &self,
        epoch: Epoch,
    ) -> Option<MithrilNetworkConfigurationForEpoch> {
        let markers = self.markers.read().await;
        markers.range(..=epoch).next_back().map(|(_, marker)| marker.clone())
    }
}

impl<const N: usize> From<[(Epoch, MithrilNetworkConfigurationForEpoch); N]>
    for FakeMithrilNetworkConfigurationProviderWithEpochMarkers
{
    fn from(value: [(Epoch, MithrilNetworkConfigurationForEpoch); N]) -> Self {
        Self::new(BTreeMap::from(value))
    }
}

#[async_trait::async_trait]
impl MithrilNetworkConfigurationProvider
    for FakeMithrilNetworkConfigurationProviderWithEpochMarkers
{
    async fn get_network_configuration(
        &self,
        epoch: Epoch,
    ) -> StdResult<MithrilNetworkConfiguration> {
        let aggregation_epoch =
            epoch.offset_to_signer_retrieval_epoch().with_context(|| {
                format!("MithrilNetworkConfigurationProvider could not compute aggregation epoch from epoch: {epoch}")
            })?;
        let next_aggregation_epoch = epoch.offset_to_next_signer_retrieval_epoch();
        let registration_epoch = epoch.offset_to_next_signer_retrieval_epoch().next();

        let marker_for_aggregation = self
            .get_marker_at_or_before(aggregation_epoch)
            .await
            .with_context(|| {
                format!("missing marker for aggregation epoch: {aggregation_epoch}")
            })?;
        let marker_for_next_aggregation = self
            .get_marker_at_or_before(next_aggregation_epoch)
            .await
            .with_context(|| {
                format!("missing marker for next aggregation epoch: {next_aggregation_epoch}")
            })?;
        let marker_for_registration = self
            .get_marker_at_or_before(registration_epoch)
            .await
            .with_context(|| {
                format!("missing marker for registration epoch: {registration_epoch}")
            })?;

        Ok(MithrilNetworkConfiguration {
            epoch,
            configuration_for_aggregation: marker_for_aggregation,
            configuration_for_next_aggregation: marker_for_next_aggregation,
            configuration_for_registration: marker_for_registration,
        })
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::{ProtocolParameters, SignedEntityTypeDiscriminants};
    use mithril_common::test::double::Dummy;

    use crate::model::SignedEntityTypeConfiguration;

    use super::*;

    fn fake_config_for_epoch(epoch: Epoch) -> MithrilNetworkConfigurationForEpoch {
        MithrilNetworkConfigurationForEpoch {
            protocol_parameters: ProtocolParameters::new(*epoch, *epoch, 0.1),
            enabled_signed_entity_types: SignedEntityTypeDiscriminants::all(),
            signed_entity_types_config: SignedEntityTypeConfiguration::dummy(),
        }
    }

    #[derive(Debug)]
    struct TestCase {
        requested_epoch: Epoch,
        expected_marker_for_aggregation_epoch: Epoch,
        expected_marker_for_next_aggregation_epoch: Epoch,
        expected_marker_for_registration_epoch: Epoch,
    }

    macro_rules! test_case {
        (
            requested: $requested_epoch:expr,
            aggregation: $expected_marker_for_aggregation_epoch:expr,
            next_aggregation: $expected_marker_for_next_aggregation_epoch:expr,
            registration: $expected_marker_for_registration_epoch:expr
        ) => {
            TestCase {
                requested_epoch: Epoch($requested_epoch),
                expected_marker_for_aggregation_epoch: Epoch(
                    $expected_marker_for_aggregation_epoch,
                ),
                expected_marker_for_next_aggregation_epoch: Epoch(
                    $expected_marker_for_next_aggregation_epoch,
                ),
                expected_marker_for_registration_epoch: Epoch(
                    $expected_marker_for_registration_epoch,
                ),
            }
        };
    }

    fn test_markers() -> BTreeMap<Epoch, MithrilNetworkConfigurationForEpoch> {
        BTreeMap::from([
            (Epoch(2), fake_config_for_epoch(Epoch(2))),
            (Epoch(6), fake_config_for_epoch(Epoch(6))),
            (Epoch(10), fake_config_for_epoch(Epoch(10))),
        ])
    }

    /// Test cases based on the [test_markers] above.
    ///
    /// Each test case is defined by a requested epoch and the expected markers for the aggregation,
    /// next aggregation, and registration epochs.
    fn test_cases() -> Vec<TestCase> {
        vec![
            test_case!(requested: 3, aggregation: 2, next_aggregation: 2, registration: 2),
            test_case!(requested: 5, aggregation: 2, next_aggregation: 2, registration: 6),
            test_case!(requested: 6, aggregation: 2, next_aggregation: 6, registration: 6),
            test_case!(requested: 7, aggregation: 6, next_aggregation: 6, registration: 6),
            test_case!(requested: 9, aggregation: 6, next_aggregation: 6, registration: 10),
            test_case!(requested: 10, aggregation: 6, next_aggregation: 10, registration: 10),
            test_case!(requested: 11, aggregation: 10, next_aggregation: 10, registration: 10),
            test_case!(requested: 12, aggregation: 10, next_aggregation: 10, registration: 10),
        ]
    }

    #[tokio::test]
    async fn get_network_configuration_based_on_test_markers() {
        for test_case in test_cases() {
            let provider =
                FakeMithrilNetworkConfigurationProviderWithEpochMarkers::new(test_markers());
            let config = provider
                .get_network_configuration(test_case.requested_epoch)
                .await
                .unwrap();

            assert_eq!(
                test_case.requested_epoch, config.epoch,
                "test_case: {test_case:?}"
            );
            assert_eq!(
                fake_config_for_epoch(test_case.expected_marker_for_aggregation_epoch),
                config.configuration_for_aggregation,
                "test_case: {test_case:?}"
            );
            assert_eq!(
                fake_config_for_epoch(test_case.expected_marker_for_next_aggregation_epoch),
                config.configuration_for_next_aggregation,
                "test_case: {test_case:?}"
            );
            assert_eq!(
                fake_config_for_epoch(test_case.expected_marker_for_registration_epoch),
                config.configuration_for_registration,
                "test_case: {test_case:?}"
            );
        }
    }

    #[tokio::test]
    async fn returns_error_when_no_marker_exists_before_configuration_epoch() {
        let provider = FakeMithrilNetworkConfigurationProviderWithEpochMarkers::from([(
            Epoch(3),
            fake_config_for_epoch(Epoch(3)),
        )]);

        // Reminder: The provider resolves markers for the window [epoch - 1 .. epoch + 1].
        for epoch in 0..4 {
            let result = provider.get_network_configuration(Epoch(epoch)).await;
            assert!(result.is_err(), "epoch: {epoch}");
        }
    }
}
