use std::collections::HashMap;

use mithril_metric::{build_metrics_service, MetricCounterWithLabels, MetricsServiceExporter};

use mithril_metric::metric::{MetricCollector, MetricCounter};
use prometheus::proto::{LabelPair, Metric, MetricFamily};

// Those are three differents dimensions, they use the same value to simplify usage in Grafana
static CLIENT_ORIGIN_TAG_LABEL: &str = "origin_tag";
static SIGNER_REGISTRATION_ORIGIN_TAG_LABEL: &str = "origin_tag";
static SIGNER_SIGNATURE_ORIGIN_TAG_LABEL: &str = "origin_tag";
static CLIENT_TYPE_LABEL: &str = "client_type";

build_metrics_service!(
    MetricsService,

    certificate_detail_total_served_since_startup:MetricCounterWithLabels(
        "certificate_detail_total_served_since_startup",
        "Number of certificate details served since startup on a Mithril aggregator node",
        &[CLIENT_ORIGIN_TAG_LABEL, CLIENT_TYPE_LABEL]
    ),
    artifact_detail_cardano_immutable_files_full_total_served_since_startup:MetricCounterWithLabels(
        "mithril_aggregator_artifact_detail_cardano_db_total_served_since_startup",
        "Number of Cardano immutable files full artifact details served since startup on a Mithril aggregator node",
        &[CLIENT_ORIGIN_TAG_LABEL]
    ),
    cardano_immutable_files_full_total_restoration_since_startup:MetricCounterWithLabels(
        "mithril_aggregator_cardano_db_total_restoration_since_startup",
        "Number of Cardano immutable files full restorations since startup on a Mithril aggregator node",
        &[CLIENT_ORIGIN_TAG_LABEL]
    ),
    cardano_database_immutable_files_restored_since_startup:MetricCounterWithLabels(
        "mithril_aggregator_cardano_db_immutable_files_restored_since_startup",
        "Number of Cardano immutable files restored since startup on a Mithril aggregator node",
        &[CLIENT_ORIGIN_TAG_LABEL]
    ),
    cardano_database_ancillary_files_restored_since_startup:MetricCounterWithLabels(
        "mithril_aggregator_cardano_db_ancillary_files_restored_since_startup",
        "Number of Cardano ancillary files restored since startup on a Mithril aggregator node",
        &[CLIENT_ORIGIN_TAG_LABEL]
    ),
    cardano_database_complete_restoration_since_startup:MetricCounterWithLabels(
        "mithril_aggregator_cardano_db_complete_restoration_since_startup",
        "Number of complete Cardano database restoration since startup on a Mithril aggregator node",
        &[CLIENT_ORIGIN_TAG_LABEL]
    ),
    cardano_database_partial_restoration_since_startup:MetricCounterWithLabels(
        "mithril_aggregator_cardano_db_partial_restoration_since_startup",
        "Number of partial Cardano database restoration since startup on a Mithril aggregator node",
        &[CLIENT_ORIGIN_TAG_LABEL]
    ),
    artifact_detail_cardano_database_total_served_since_startup:MetricCounterWithLabels(
        "mithril_aggregator_artifact_detail_cardano_database_total_served_since_startup",
        "Number of Cardano database artifact details served since startup on a Mithril aggregator node",
        &[CLIENT_ORIGIN_TAG_LABEL]
    ),
    artifact_detail_mithril_stake_distribution_total_served_since_startup:MetricCounterWithLabels(
        "mithril_aggregator_artifact_detail_mithril_stake_distribution_total_served_since_startup",
        "Number of Mithril stake distribution artifact details served since startup on a Mithril aggregator node",
        &[CLIENT_ORIGIN_TAG_LABEL]
    ),
    artifact_detail_cardano_stake_distribution_total_served_since_startup:MetricCounterWithLabels(
        "mithril_aggregator_artifact_detail_cardano_stake_distribution_total_served_since_startup",
        "Number of Cardano stake distribution artifact details served since startup on a Mithril aggregator node",
        &[CLIENT_ORIGIN_TAG_LABEL]
    ),
    artifact_detail_cardano_transaction_total_served_since_startup:MetricCounterWithLabels(
        "mithril_aggregator_artifact_detail_cardano_transaction_total_served_since_startup",
        "Number of Cardano transaction artifact details served since startup on a Mithril aggregator node",
        &[CLIENT_ORIGIN_TAG_LABEL]
    ),
    proof_cardano_transaction_total_proofs_served_since_startup:MetricCounterWithLabels(
        "mithril_aggregator_proof_cardano_transaction_total_proofs_served_since_startup",
        "Number of Cardano transaction proofs served since startup on a Mithril aggregator node",
        &[CLIENT_ORIGIN_TAG_LABEL]
    ),
    proof_cardano_transaction_total_transactions_served_since_startup:MetricCounterWithLabels(
        "mithril_aggregator_proof_cardano_transaction_total_transactions_served_since_startup",
        "Number of Cardano transaction hashes requested for proof since startup on a Mithril aggregator node",
        &[CLIENT_ORIGIN_TAG_LABEL]
    ),
    signer_registration_total_received_since_startup:MetricCounterWithLabels(
        "mithril_aggregator_signer_registration_total_received_since_startup",
        "Number of signer registrations received since startup on a Mithril aggregator node",
        &[SIGNER_REGISTRATION_ORIGIN_TAG_LABEL]
    ),
    signature_registration_total_received_since_startup:MetricCounterWithLabels(
        "mithril_aggregator_signature_registration_total_received_since_startup",
        "Number of signature registrations received since startup on a Mithril aggregator node",
        &[SIGNER_SIGNATURE_ORIGIN_TAG_LABEL]
    ),
    certificate_total_produced_since_startup:MetricCounter(
        "mithril_aggregator_certificate_total_produced_since_startup",
        "Number of certificates produced since startup on a Mithril aggregator node"
    ),
    artifact_cardano_immutable_files_full_total_produced_since_startup:MetricCounter(
        "mithril_aggregator_artifact_cardano_db_total_produced_since_startup",
        "Number of Cardano immutable files full artifacts produced since startup on a Mithril aggregator node"
    ),
    artifact_cardano_database_total_produced_since_startup:MetricCounter(
        "mithril_aggregator_artifact_cardano_database_total_produced_since_startup",
        "Number of Cardano database artifacts produced since startup on a Mithril aggregator node"
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
    pub fn export_metrics_map(&self) -> HashMap<String, HashMap<String, u32>> {
        self.registry
            .gather()
            .iter()
            .map(|metric_family| {
                (
                    metric_family.name().to_string(),
                    self.build_metric_map_per_label(metric_family),
                )
            })
            .collect()
    }

    fn build_label_key(&self, labels: &[LabelPair]) -> String {
        labels
            .iter()
            .map(|p| p.value())
            .collect::<Vec<_>>()
            .join(",")
    }

    fn build_metric_map_per_label(&self, metric_family: &MetricFamily) -> HashMap<String, u32> {
        metric_family
            .get_metric()
            .iter()
            .map(|m| {
                println!(
                    "VVVVVV Exporting metric: {} with labels: {:?}, builded label key: {}",
                    m.get_counter().as_ref().unwrap().value(),
                    m.get_label(),
                    m.get_counter().as_ref().unwrap_or_default().value()
                );
                (
                    self.build_label_key(m.get_label()),
                    m.get_counter().as_ref().unwrap_or_default().value() as u32,
                )
            })
            .collect()
    }

    pub fn export_metrics_map2(&self) -> HashMap<String, HashMap<String, MetricMap>> {
        self.registry
            .gather()
            .iter()
            .map(|metric_family| {
                (
                    metric_family.name().to_string(),
                    self.build_metric_map(metric_family),
                )
            })
            .collect()
    }

    fn build_metric_map(&self, metric_family: &MetricFamily) -> HashMap<String, MetricMap> {
        metric_family
            .get_metric()
            .iter()
            .map(|m| {
                println!(
                    "VVVVVV Exporting metric: {} with labels: {:?}, builded label key: {}",
                    m.get_counter().as_ref().unwrap().value(),
                    m.get_label(),
                    self.build_label_key(m.get_label()),
                );
                (
                    self.build_label_key(m.get_label()),
                    MetricMap::new(m.clone()),
                )
            })
            .collect()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MetricMap {
    //key (labels) and value
    pub key_value: HashMap<String, String>,
    // metric counter
    pub counter: u32,
}

impl MetricMap {
    pub fn new(metric: Metric) -> Self {
        Self {
            key_value: metric
                .get_label()
                .iter()
                .map(|label| (label.name().to_string(), label.value().to_string()))
                .collect(),
            counter: metric.get_counter().as_ref().unwrap_or_default().value() as u32,
        }
    }
    fn new(key_value: HashMap<String, String>, counter: u32) -> Self {
        Self { key_value, counter }
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
        assert_eq!(5, export[&metric_a.name()][""]);
        assert_eq!(12, export[&metric_b.name()][""]);
    }

    #[test]
    fn should_export_counter_metrics_with_label_in_a_map() {
        let metrics_service = MetricsService::new(TestLogger::stdout()).unwrap();
        let metric_a = metrics_service.get_certificate_detail_total_served_since_startup();
        metric_a.increment_by(&["TOKEN_A", "TOKEN_B"], 5);
        metric_a.increment_by(&["TOKEN_1", "TOKEN_2"], 12);

        let export = metrics_service.export_metrics_map();
        assert_eq!(5, export[&metric_a.name()]["TOKEN_B,TOKEN_A"]);
        assert_eq!(12, export[&metric_a.name()]["TOKEN_2,TOKEN_1"]);
    }

    #[test]
    fn should_export_counter_metrics_with_label_in_a_map2() {
        let metrics_service = MetricsService::new(TestLogger::stdout()).unwrap();
        let metric_a = metrics_service.get_certificate_detail_total_served_since_startup();
        metric_a.increment_by(&["A", "B"], 5);
        metric_a.increment_by(&["1", "2"], 12);

        let export = metrics_service.export_metrics_map2();

        let metric_a_token_a_b = export[&metric_a.name()]["B,A"].clone();
        let metric_a_token_1_2 = export[&metric_a.name()]["2,1"].clone();

        assert_eq!(5, metric_a_token_a_b.to_owned().counter);
        assert_eq!("A", metric_a_token_a_b.to_owned().key_value["origin_tag"]);
        assert_eq!("B", metric_a_token_a_b.to_owned().key_value["client_type"]);

        assert_eq!(12, metric_a_token_1_2.to_owned().counter);
        assert_eq!("1", metric_a_token_1_2.to_owned().key_value["origin_tag"]);
        assert_eq!("2", metric_a_token_1_2.to_owned().key_value["client_type"]);
    }

    #[test]
    fn should_export_several_times_and_counter_return_values_since_start() {
        let metrics_service = MetricsService::new(TestLogger::stdout()).unwrap();
        let metric_a = metrics_service.get_runtime_cycle_total_since_startup();
        metric_a.increment_by(5);

        let export = metrics_service.export_metrics_map();
        assert_eq!(5, export[&metric_a.name()][""]);

        metric_a.increment();
        let export = metrics_service.export_metrics_map();
        assert_eq!(6, export[&metric_a.name()][""]);
    }

    #[test]
    fn should_export_counter_even_the_value_is_0() {
        let metrics_service = MetricsService::new(TestLogger::stdout()).unwrap();
        let metric_a = metrics_service.get_runtime_cycle_total_since_startup();

        let export = metrics_service.export_metrics_map();
        assert_eq!(0, export[&metric_a.name()][""]);
    }

    #[test]
    fn metric_service_should_only_contain_counters_as_export_metrics_map_does_not_yet_support_other_types(
    ) {
        let metrics_service = MetricsService::new(TestLogger::stdout()).unwrap();

        for metric_family in metrics_service.registry.gather() {
            for metric in metric_family.get_metric() {
                assert!(metric.get_counter().is_some());
            }
        }
    }
}
