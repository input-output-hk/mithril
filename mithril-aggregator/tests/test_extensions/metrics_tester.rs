use mithril_aggregator::MetricsService;
use mithril_metric::MetricCollector;
use std::sync::Arc;

pub struct ExpectedMetrics {
    certificate_total: Option<u32>,
    artifact_cardano_immutable_files_full_total: Option<u32>,
    artifact_cardano_database_total: Option<u32>,
    artifact_mithril_stake_distribution_total: Option<u32>,
    artifact_cardano_stake_distribution_total: Option<u32>,
    artifact_cardano_transaction_total: Option<u32>,
    runtime_cycle_success: Option<u32>,
    runtime_cycle_total: Option<u32>,
}

impl ExpectedMetrics {
    pub fn new() -> Self {
        Self {
            certificate_total: None,
            artifact_cardano_immutable_files_full_total: None,
            artifact_cardano_database_total: None,
            artifact_mithril_stake_distribution_total: None,
            artifact_cardano_stake_distribution_total: None,
            artifact_cardano_transaction_total: None,
            runtime_cycle_success: None,
            runtime_cycle_total: None,
        }
    }

    pub fn certificate_total(mut self, value: u32) -> Self {
        self.certificate_total = Some(value);

        self
    }

    pub fn artifact_cardano_immutable_files_full_total(mut self, value: u32) -> Self {
        self.artifact_cardano_immutable_files_full_total = Some(value);

        self
    }

    pub fn artifact_cardano_database_total(mut self, value: u32) -> Self {
        self.artifact_cardano_database_total = Some(value);

        self
    }

    pub fn artifact_mithril_stake_distribution_total(mut self, value: u32) -> Self {
        self.artifact_mithril_stake_distribution_total = Some(value);

        self
    }

    pub fn artifact_cardano_stake_distribution_total(mut self, value: u32) -> Self {
        self.artifact_cardano_stake_distribution_total = Some(value);

        self
    }

    pub fn artifact_cardano_transaction_total(mut self, value: u32) -> Self {
        self.artifact_cardano_transaction_total = Some(value);

        self
    }

    pub fn runtime_cycle_success(mut self, value: u32) -> Self {
        self.runtime_cycle_success = Some(value);

        self
    }

    pub fn runtime_cycle_total(mut self, value: u32) -> Self {
        self.runtime_cycle_total = Some(value);

        self
    }
}

pub struct MetricsVerifier {
    metrics_service: Arc<MetricsService>,
}

impl MetricsVerifier {
    pub fn new(metrics_service: Arc<MetricsService>) -> Self {
        Self { metrics_service }
    }

    pub fn verify(&self, expected_metrics: ExpectedMetrics) {
        macro_rules! verify_metric {
            ($expected_metric:expr, $metric:expr) => {
                if let Some(expected) = $expected_metric {
                    assert_eq!(
                        expected,
                        $metric.get(),
                        "Expected '{}' to be '{}' but got '{}'",
                        $metric.name(),
                        expected,
                        $metric.get()
                    );
                }
            };
        }

        verify_metric!(
            expected_metrics.certificate_total,
            self.metrics_service
                .get_certificate_total_produced_since_startup()
        );

        verify_metric!(
            expected_metrics.artifact_cardano_immutable_files_full_total,
            self.metrics_service
                .get_artifact_cardano_immutable_files_full_total_produced_since_startup()
        );

        verify_metric!(
            expected_metrics.artifact_cardano_database_total,
            self.metrics_service
                .get_artifact_cardano_database_total_produced_since_startup()
        );

        verify_metric!(
            expected_metrics.artifact_mithril_stake_distribution_total,
            self.metrics_service
                .get_artifact_mithril_stake_distribution_total_produced_since_startup()
        );

        verify_metric!(
            expected_metrics.artifact_cardano_stake_distribution_total,
            self.metrics_service
                .get_artifact_cardano_stake_distribution_total_produced_since_startup()
        );

        verify_metric!(
            expected_metrics.artifact_cardano_transaction_total,
            self.metrics_service
                .get_artifact_cardano_transaction_total_produced_since_startup()
        );

        verify_metric!(
            expected_metrics.runtime_cycle_success,
            self.metrics_service
                .get_runtime_cycle_success_since_startup()
        );

        verify_metric!(
            expected_metrics.runtime_cycle_total,
            self.metrics_service.get_runtime_cycle_total_since_startup()
        );
    }
}
