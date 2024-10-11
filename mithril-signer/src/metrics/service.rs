use mithril_metric::{build_metrics_service, metrics_tools, MetricsServiceTrait};
use prometheus::Registry;
use slog::Logger;

use mithril_common::{entities::Epoch, StdResult};

use mithril_metric::commons::{CounterValue, MetricCounter, MetricGauge, MithrilMetric};

/// Build the metrics service with the required metrics.
build_metrics_service!(
    MetricsService,
    "mithril_signer",
    signer_registration_success_since_startup_counter: MetricCounter(
        "Number of successful signer registrations since startup on a Mithril signer node"
    ),
    signer_registration_total_since_startup_counter: MetricCounter(
        "Number of signer registrations since startup on a Mithril signer node"
    ),
    signer_registration_success_last_epoch_gauge: MetricGauge(
        "Latest epoch at which signer successfully registered on a Mithril signer node"
    ),
    signature_registration_success_since_startup_counter: MetricCounter(
        "Number of successful signature registrations since startup on a Mithril signer node"
    ),
    signature_registration_total_since_startup_counter: MetricCounter(
        "Number of signature registrations since startup on a Mithril signer node"
    ),
    signature_registration_success_last_epoch_gauge: MetricGauge(
        "Latest epoch at which signature successfully registered on a Mithril signer node"
    ),
    // Runtime cycle metrics
    runtime_cycle_success_since_startup_counter: MetricCounter(
        "Number of successful runtime cycles since startup on a Mithril signer node"
    ),
    runtime_cycle_total_since_startup_counter: MetricCounter(
        "Number of runtime cycles since startup on a Mithril signer node"
    )

);

// TODO these functions are kept while changes are made for all uses
impl MetricsService {
    /// Increment the `signer_registration_success_since_startup` counter.
    pub fn signer_registration_success_since_startup_counter_increment(&self) {
        self.get_signer_registration_success_since_startup_counter()
            .record();
    }

    /// Get the `signer_registration_success_since_startup` counter.
    pub fn signer_registration_success_since_startup_counter_get(&self) -> CounterValue {
        self.get_signer_registration_success_since_startup_counter()
            .get()
    }

    /// Increment the `signer_registration_total_since_startup` counter.
    pub fn signer_registration_total_since_startup_counter_increment(&self) {
        self.get_signer_registration_total_since_startup_counter()
            .record();
    }

    /// Get the `signer_registration_total_since_startup` counter.
    pub fn signer_registration_total_since_startup_counter_get(&self) -> CounterValue {
        self.get_signer_registration_total_since_startup_counter()
            .get()
    }

    /// Set the `signer_registration_success_last_epoch` gauge value.
    pub fn signer_registration_success_last_epoch_gauge_set(&self, value: Epoch) {
        self.get_signer_registration_success_last_epoch_gauge()
            .record(value);
    }

    /// Get the `signer_registration_success_last_epoch` gauge value.
    pub fn signer_registration_success_last_epoch_gauge_get(&self) -> Epoch {
        self.get_signer_registration_success_last_epoch_gauge()
            .get()
    }

    /// Increment the `signature_registration_success_since_startup` counter.
    pub fn signature_registration_success_since_startup_counter_increment(&self) {
        self.get_signature_registration_success_since_startup_counter()
            .record();
    }

    /// Get the `signature_registration_success_since_startup` counter.
    pub fn signature_registration_success_since_startup_counter_get(&self) -> CounterValue {
        self.get_signature_registration_success_since_startup_counter()
            .get()
    }

    /// Increment the `signature_registration_total_since_startup` counter.
    pub fn signature_registration_total_since_startup_counter_increment(&self) {
        self.get_signature_registration_total_since_startup_counter()
            .record();
    }

    /// Get the `signature_registration_total_since_startup` counter.
    pub fn signature_registration_total_since_startup_counter_get(&self) -> CounterValue {
        self.get_signature_registration_total_since_startup_counter()
            .get()
    }

    /// Set the `signature_registration_success_last_epoch` gauge value.
    pub fn signature_registration_success_last_epoch_gauge_set(&self, value: Epoch) {
        self.get_signature_registration_success_last_epoch_gauge()
            .record(value);
    }

    /// Get the `signature_registration_success_last_epoch` gauge value.
    pub fn signature_registration_success_last_epoch_gauge_get(&self) -> Epoch {
        self.get_signature_registration_success_last_epoch_gauge()
            .get()
    }

    /// Increment the `runtime_cycle_total_since_startup` counter.
    pub fn runtime_cycle_total_since_startup_counter_increment(&self) {
        self.get_runtime_cycle_total_since_startup_counter()
            .record();
    }

    /// Get the `runtime_cycle_total_since_startup` counter.
    pub fn runtime_cycle_total_since_startup_counter_get(&self) -> CounterValue {
        self.get_runtime_cycle_total_since_startup_counter().get()
    }

    /// Increment the `runtime_cycle_success_since_startup` counter.
    pub fn runtime_cycle_success_since_startup_counter_increment(&self) {
        self.get_runtime_cycle_success_since_startup_counter()
            .record();
    }

    /// Get the `runtime_cycle_success_since_startup` counter.
    pub fn runtime_cycle_success_since_startup_counter_get(&self) -> CounterValue {
        self.get_runtime_cycle_success_since_startup_counter().get()
    }
}

#[cfg(test)]
mod tests {
    use prometheus_parse::Value;
    use std::collections::BTreeMap;

    use crate::test_tools::TestLogger;

    use super::*;

    fn parse_metrics(raw_metrics: &str) -> StdResult<BTreeMap<String, Value>> {
        Ok(
            prometheus_parse::Scrape::parse(raw_metrics.lines().map(|s| Ok(s.to_owned())))?
                .samples
                .into_iter()
                .map(|s| (s.metric, s.value))
                .collect::<BTreeMap<_, _>>(),
        )
    }

    #[test]
    fn test_export_metrics() {
        let metrics_service = MetricsService::new(TestLogger::stdout()).unwrap();
        let exported_metrics = metrics_service.export_metrics().unwrap();

        let parsed_metrics = parse_metrics(&exported_metrics).unwrap();

        let parsed_metrics_expected = BTreeMap::from([
            (
                metrics_service
                    .get_runtime_cycle_success_since_startup_counter()
                    .name(),
                Value::Counter(0.0),
            ),
            (
                metrics_service
                    .get_runtime_cycle_total_since_startup_counter()
                    .name(),
                Value::Counter(0.0),
            ),
            (
                metrics_service
                    .get_signature_registration_success_last_epoch_gauge()
                    .name(),
                Value::Gauge(0.0),
            ),
            (
                metrics_service
                    .get_signature_registration_success_since_startup_counter()
                    .name(),
                Value::Counter(0.0),
            ),
            (
                metrics_service
                    .get_signature_registration_total_since_startup_counter()
                    .name(),
                Value::Counter(0.0),
            ),
            (
                metrics_service
                    .get_signer_registration_success_last_epoch_gauge()
                    .name(),
                Value::Gauge(0.0),
            ),
            (
                metrics_service
                    .get_signer_registration_success_since_startup_counter()
                    .name(),
                Value::Counter(0.0),
            ),
            (
                metrics_service
                    .get_signer_registration_total_since_startup_counter()
                    .name(),
                Value::Counter(0.0),
            ),
        ]);
        assert_eq!(parsed_metrics_expected, parsed_metrics);
    }

    #[test]
    fn test_signer_registration_success_since_startup_counter_increment() {
        let metrics_service = MetricsService::new(TestLogger::stdout()).unwrap();
        assert_eq!(
            0,
            metrics_service.signer_registration_success_since_startup_counter_get(),
        );

        metrics_service.signer_registration_success_since_startup_counter_increment();
        assert_eq!(
            1,
            metrics_service.signer_registration_success_since_startup_counter_get(),
        );
    }

    #[test]
    fn test_signer_registration_total_since_startup_counter_increment() {
        let metrics_service = MetricsService::new(TestLogger::stdout()).unwrap();
        assert_eq!(
            0,
            metrics_service.signer_registration_total_since_startup_counter_get(),
        );

        metrics_service.signer_registration_total_since_startup_counter_increment();
        assert_eq!(
            1,
            metrics_service.signer_registration_total_since_startup_counter_get(),
        );
    }

    #[test]
    fn test_signer_registration_success_last_epoch_gauge_set() {
        let metrics_service = MetricsService::new(TestLogger::stdout()).unwrap();
        assert_eq!(
            Epoch(0),
            metrics_service.signer_registration_success_last_epoch_gauge_get(),
        );

        metrics_service.signer_registration_success_last_epoch_gauge_set(Epoch(123));
        assert_eq!(
            Epoch(123),
            metrics_service.signer_registration_success_last_epoch_gauge_get(),
        );
    }

    #[test]
    fn test_signature_registration_success_since_startup_counter_increment() {
        let metrics_service = MetricsService::new(TestLogger::stdout()).unwrap();
        assert_eq!(
            0,
            metrics_service.signature_registration_success_since_startup_counter_get(),
        );

        metrics_service.signature_registration_success_since_startup_counter_increment();
        assert_eq!(
            1,
            metrics_service.signature_registration_success_since_startup_counter_get(),
        );
    }

    #[test]
    fn test_signature_registration_total_since_startup_counter_increment() {
        let metrics_service = MetricsService::new(TestLogger::stdout()).unwrap();
        assert_eq!(
            0,
            metrics_service.signature_registration_total_since_startup_counter_get(),
        );

        metrics_service.signature_registration_total_since_startup_counter_increment();
        assert_eq!(
            1,
            metrics_service.signature_registration_total_since_startup_counter_get(),
        );
    }

    #[test]
    fn test_signature_registration_success_last_epoch_gauge_set() {
        let metrics_service = MetricsService::new(TestLogger::stdout()).unwrap();
        assert_eq!(
            Epoch(0),
            metrics_service.signature_registration_success_last_epoch_gauge_get(),
        );

        metrics_service.signature_registration_success_last_epoch_gauge_set(Epoch(123));
        assert_eq!(
            Epoch(123),
            metrics_service.signature_registration_success_last_epoch_gauge_get(),
        );
    }

    #[test]
    fn test_runtime_cycle_success_since_startup_counter_increment() {
        let metrics_service = MetricsService::new(TestLogger::stdout()).unwrap();
        assert_eq!(
            0,
            metrics_service.runtime_cycle_success_since_startup_counter_get(),
        );

        metrics_service.runtime_cycle_success_since_startup_counter_increment();
        assert_eq!(
            1,
            metrics_service.runtime_cycle_success_since_startup_counter_get(),
        );
    }

    #[test]
    fn test_runtime_cycle_total_since_startup_counter_increment() {
        let metrics_service = MetricsService::new(TestLogger::stdout()).unwrap();
        assert_eq!(
            0,
            metrics_service.runtime_cycle_total_since_startup_counter_get(),
        );

        metrics_service.runtime_cycle_total_since_startup_counter_increment();
        assert_eq!(
            1,
            metrics_service.runtime_cycle_total_since_startup_counter_get(),
        );
    }
}
