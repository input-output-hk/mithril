use std::collections::HashMap;

use mithril_metric::{build_metrics_service, MetricsServiceExporter};

use mithril_metric::metric::{MetricCollector, MetricCounter};

build_metrics_service!(
    MetricsService,
    certificate_detail_total_served_since_startup:MetricCounter(
        "mithril_aggregator_certificate_detail_total_served_since_startup",
        "Number of certificate details served since startup on a Mithril aggregator node"
    ),
    artifact_detail_cardano_db_total_served_since_startup:MetricCounter(
        "mithril_aggregator_artifact_detail_cardano_db_total_served_since_startup",
        "Number of Cardano db artifact details served since startup on a Mithril aggregator node"
    ),
    cardano_db_total_restoration_since_startup:MetricCounter(
        "mithril_aggregator_cardano_db_total_restoration_since_startup",
        "Number of Cardano db restorations since startup on a Mithril aggregator node"
    ),
    artifact_detail_mithril_stake_distribution_total_served_since_startup:MetricCounter(
        "mithril_aggregator_artifact_detail_mithril_stake_distribution_total_served_since_startup",
        "Number of Mithril stake distribution artifact details served since startup on a Mithril aggregator node"
    ),
    artifact_detail_cardano_stake_distribution_total_served_since_startup:MetricCounter(
        "mithril_aggregator_artifact_detail_cardano_stake_distribution_total_served_since_startup",
        "Number of Cardano stake distribution artifact details served since startup on a Mithril aggregator node"
    ),
    artifact_detail_cardano_transaction_total_served_since_startup:MetricCounter(
        "mithril_aggregator_artifact_detail_cardano_transaction_total_served_since_startup",
        "Number of Cardano transaction artifact details served since startup on a Mithril aggregator node"
    ),
    proof_cardano_transaction_total_proofs_served_since_startup:MetricCounter(
        "mithril_aggregator_proof_cardano_transaction_total_proofs_served_since_startup",
        "Number of Cardano transaction proofs served since startup on a Mithril aggregator node"
    ),
    proof_cardano_transaction_total_transactions_served_since_startup:MetricCounter(
        "mithril_aggregator_proof_cardano_transaction_total_transactions_served_since_startup",
        "Number of Cardano transaction hashes requested for proof since startup on a Mithril aggregator node"
    ),
    signer_registration_total_received_since_startup:MetricCounter(
        "mithril_aggregator_signer_registration_total_received_since_startup",
        "Number of signer registrations received since startup on a Mithril aggregator node"
    ),
    signature_registration_total_received_since_startup:MetricCounter(
        "mithril_aggregator_signature_registration_total_received_since_startup",
        "Number of signature registrations received since startup on a Mithril aggregator node"
    ),
    certificate_total_produced_since_startup:MetricCounter(
        "mithril_aggregator_certificate_total_produced_since_startup",
        "Number of certificates produced since startup on a Mithril aggregator node"
    ),
    artifact_cardano_db_total_produced_since_startup:MetricCounter(
        "mithril_aggregator_artifact_cardano_db_total_produced_since_startup",
        "Number of Cardano db artifacts produced since startup on a Mithril aggregator node"
    ),
    artifact_mithril_stake_distribution_total_produced_since_startup:MetricCounter(
        "mithril_aggregator_artifact_mithril_stake_distribution_total_produced_since_startup",
        "Number of Mithril stake distribution artifacts produced since startup on a Mithril aggregator node"
    ),
    artifact_cardano_stake_distribution_total_produced_since_startup:MetricCounter(
        "mithril_aggregator_artifact_cardano_stake_distribution_total_produced_since_startup",
        "Number of Cardano stake distribution artifacts produced since startup on a Mithril aggregator node"
    ),
    artifact_cardano_transaction_total_produced_since_startup:MetricCounter(
        "mithril_aggregator_artifact_cardano_transaction_total_produced_since_startup",
        "Number of Cardano transaction artifacts produced since startup on a Mithril aggregator node"
    ),
    artifact_cardano_database_total_produced_since_startup:MetricCounter(
        "mithril_aggregator_artifact_cardano_database_total_produced_since_startup",
        "Number of Cardano database artifacts produced since startup on a Mithril aggregator node"
    ),
    runtime_cycle_success_since_startup:MetricCounter(
        "mithril_aggregator_runtime_cycle_success_since_startup",
        "Number of successful runtime cycles since startup on a Mithril aggregator"
    ),
    runtime_cycle_total_since_startup:MetricCounter(
        "mithril_aggregator_runtime_cycle_total_since_startup",
        "Number of runtime cycles since startup on a Mithril aggregator"
    )

);

impl MetricsService {
    /// Export metrics in map.
    // `get metric` returns a list of Metrics for CounterVec purposes for example.
    // We therefore add up the values ​​even though we will always only have one value with our Counter type metrics.
    pub fn export_metrics_map(&self) -> HashMap<String, u32> {
        self.registry
            .gather()
            .iter()
            .map(|metric_family| {
                (
                    metric_family.get_name().to_string(),
                    metric_family
                        .get_metric()
                        .iter()
                        .map(|m| m.get_counter().get_value() as u32)
                        .sum(),
                )
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use crate::test_tools::TestLogger;

    use super::*;

    #[test]
    fn should_export_counter_metrics_in_a_map() {
        let metrics_service = MetricsService::new(TestLogger::stdout()).unwrap();
        let metric_a = metrics_service.get_runtime_cycle_total_since_startup();
        let metric_b = metrics_service.get_certificate_total_produced_since_startup();
        metric_a.increment_by(5);
        metric_b.increment_by(12);

        let export = metrics_service.export_metrics_map();
        assert_eq!(5, export[&metric_a.name()]);
        assert_eq!(12, export[&metric_b.name()]);
    }

    #[test]
    fn should_export_several_times_and_counter_return_values_since_start() {
        let metrics_service = MetricsService::new(TestLogger::stdout()).unwrap();
        let metric_a = metrics_service.get_runtime_cycle_total_since_startup();
        metric_a.increment_by(5);

        let export = metrics_service.export_metrics_map();
        assert_eq!(5, export[&metric_a.name()]);

        metric_a.increment();
        let export = metrics_service.export_metrics_map();
        assert_eq!(6, export[&metric_a.name()]);
    }

    #[test]
    fn should_export_counter_even_the_value_is_0() {
        let metrics_service = MetricsService::new(TestLogger::stdout()).unwrap();
        let metric_a = metrics_service.get_runtime_cycle_total_since_startup();

        let export = metrics_service.export_metrics_map();
        assert_eq!(0, export[&metric_a.name()]);
    }

    #[test]
    fn metric_service_should_only_contain_counters_as_export_metrics_map_does_not_yet_support_other_types(
    ) {
        let metrics_service = MetricsService::new(TestLogger::stdout()).unwrap();

        for metric_family in metrics_service.registry.gather() {
            for metric in metric_family.get_metric() {
                assert!(metric.has_counter());
            }
        }
    }
}
