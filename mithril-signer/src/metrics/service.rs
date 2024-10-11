use mithril_metric::MetricsServiceTrait;
use prometheus::Registry;
use slog::Logger;

use mithril_common::logging::LoggerExtensions;
use mithril_common::{entities::Epoch, StdResult};

use mithril_metric::commons::metrics_tools;
use mithril_metric::commons::{CounterValue, MetricCounter, MetricGauge, MithrilMetric};

/// Metrics service which is responsible for recording and exposing metrics.
pub struct MetricsService {
    registry: Registry,
    signer_registration_success_since_startup_counter: MetricCounter,
    signer_registration_total_since_startup_counter: MetricCounter,
    signer_registration_success_last_epoch_gauge: MetricGauge,
    signature_registration_success_since_startup_counter: MetricCounter,
    signature_registration_total_since_startup_counter: MetricCounter,
    signature_registration_success_last_epoch_gauge: MetricGauge,
    runtime_cycle_success_since_startup_counter: MetricCounter,
    runtime_cycle_total_since_startup_counter: MetricCounter,
}

impl MetricsServiceTrait for MetricsService {
    /// Export the metrics as a string with the Open Metrics standard format.
    /// These metrics can be exposed on an HTTP server.
    fn export_metrics(&self) -> StdResult<String> {
        metrics_tools::export_metrics(&self.registry)
    }
}

impl MetricsService {
    /// Create a new `MetricsService` instance.
    pub fn new(logger: Logger) -> StdResult<Self> {
        let logger = logger.new_with_component_name::<Self>();

        let registry = Registry::new();

        fn register<T: MithrilMetric>(registry: &Registry, metric: T) -> StdResult<T> {
            registry.register(metric.collector())?;
            Ok(metric)
        }

        let signer_registration_success_since_startup_counter = register(
            &registry,
            MetricCounter::new(
                logger.clone(),
                "mithril_signer_signer_registration_success_since_startup",
                "Number of successful signer registrations since startup on a Mithril signer node",
            )?,
        )?;

        let signer_registration_total_since_startup_counter = register(
            &registry,
            MetricCounter::new(
                logger.clone(),
                "mithril_signer_signer_registration_total_since_startup",
                "Number of signer registrations since startup on a Mithril signer node",
            )?,
        )?;

        let signer_registration_success_last_epoch_gauge = register(
            &registry,
            MetricGauge::new(
                logger.clone(),
                "mithril_signer_signer_registration_success_last_epoch",
                "Latest epoch at which signer successfully registered on a Mithril signer node",
            )?,
        )?;
        // Signature registration metrics

        let signature_registration_success_since_startup_counter = register(
            &registry,
            MetricCounter::new(
                logger.clone(),
                "mithril_signer_signature_registration_success_since_startup",
                "Number of successful signature registrations since startup on a Mithril signer node",
            )?,
        )?;

        let signature_registration_total_since_startup_counter = register(
            &registry,
            MetricCounter::new(
                logger.clone(),
                "mithril_signer_signature_registration_total_since_startup",
                "Number of signature registrations since startup on a Mithril signer node",
            )?,
        )?;

        let signature_registration_success_last_epoch_gauge = register(
            &registry,
            MetricGauge::new(
                logger.clone(),
                "mithril_signer_signature_registration_success_last_epoch",
                "Latest epoch at which signature successfully registered on a Mithril signer node",
            )?,
        )?;

        // Runtime cycle metrics
        let runtime_cycle_success_since_startup_counter = register(
            &registry,
            MetricCounter::new(
                logger.clone(),
                "mithril_signer_runtime_cycle_success_since_startup",
                "Number of successful runtime cycles since startup on a Mithril signer node",
            )?,
        )?;
        let runtime_cycle_total_since_startup_counter = register(
            &registry,
            MetricCounter::new(
                logger.clone(),
                "mithril_signer_runtime_cycle_total_since_startup",
                "Number of runtime cycles since startup on a Mithril signer node",
            )?,
        )?;

        Ok(Self {
            registry,
            signer_registration_success_since_startup_counter,
            signer_registration_total_since_startup_counter,
            signer_registration_success_last_epoch_gauge,
            signature_registration_success_since_startup_counter,
            signature_registration_total_since_startup_counter,
            signature_registration_success_last_epoch_gauge,
            runtime_cycle_success_since_startup_counter,
            runtime_cycle_total_since_startup_counter,
        })
    }

    /// Increment the `signer_registration_success_since_startup` counter.
    pub fn signer_registration_success_since_startup_counter_increment(&self) {
        self.signer_registration_success_since_startup_counter
            .record();
    }

    /// Get the `signer_registration_success_since_startup` counter.
    pub fn signer_registration_success_since_startup_counter_get(&self) -> CounterValue {
        self.signer_registration_success_since_startup_counter.get()
    }

    /// Increment the `signer_registration_total_since_startup` counter.
    pub fn signer_registration_total_since_startup_counter_increment(&self) {
        self.signer_registration_total_since_startup_counter
            .record();
    }

    /// Get the `signer_registration_total_since_startup` counter.
    pub fn signer_registration_total_since_startup_counter_get(&self) -> CounterValue {
        self.signer_registration_total_since_startup_counter.get()
    }

    /// Set the `signer_registration_success_last_epoch` gauge value.
    pub fn signer_registration_success_last_epoch_gauge_set(&self, value: Epoch) {
        self.signer_registration_success_last_epoch_gauge
            .record(value);
    }

    /// Get the `signer_registration_success_last_epoch` gauge value.
    pub fn signer_registration_success_last_epoch_gauge_get(&self) -> Epoch {
        self.signer_registration_success_last_epoch_gauge.get()
    }

    /// Increment the `signature_registration_success_since_startup` counter.
    pub fn signature_registration_success_since_startup_counter_increment(&self) {
        self.signature_registration_success_since_startup_counter
            .record();
    }

    /// Get the `signature_registration_success_since_startup` counter.
    pub fn signature_registration_success_since_startup_counter_get(&self) -> CounterValue {
        self.signature_registration_success_since_startup_counter
            .get()
    }

    /// Increment the `signature_registration_total_since_startup` counter.
    pub fn signature_registration_total_since_startup_counter_increment(&self) {
        self.signature_registration_total_since_startup_counter
            .record();
    }

    /// Get the `signature_registration_total_since_startup` counter.
    pub fn signature_registration_total_since_startup_counter_get(&self) -> CounterValue {
        self.signature_registration_total_since_startup_counter
            .get()
    }

    /// Set the `signature_registration_success_last_epoch` gauge value.
    pub fn signature_registration_success_last_epoch_gauge_set(&self, value: Epoch) {
        self.signature_registration_success_last_epoch_gauge
            .record(value);
    }

    /// Get the `signature_registration_success_last_epoch` gauge value.
    pub fn signature_registration_success_last_epoch_gauge_get(&self) -> Epoch {
        self.signature_registration_success_last_epoch_gauge.get()
    }

    /// Increment the `runtime_cycle_total_since_startup` counter.
    pub fn runtime_cycle_total_since_startup_counter_increment(&self) {
        self.runtime_cycle_total_since_startup_counter.record();
    }

    /// Get the `runtime_cycle_total_since_startup` counter.
    pub fn runtime_cycle_total_since_startup_counter_get(&self) -> CounterValue {
        self.runtime_cycle_total_since_startup_counter.get()
    }

    /// Increment the `runtime_cycle_success_since_startup` counter.
    pub fn runtime_cycle_success_since_startup_counter_increment(&self) {
        self.runtime_cycle_success_since_startup_counter.record();
    }

    /// Get the `runtime_cycle_success_since_startup` counter.
    pub fn runtime_cycle_success_since_startup_counter_get(&self) -> CounterValue {
        self.runtime_cycle_success_since_startup_counter.get()
    }

    /// Get the `signer_registration_success_since_startup_counter` counter.
    pub fn get_signer_registration_success_since_startup_counter(&self) -> &MetricCounter {
        &self.signer_registration_success_since_startup_counter
    }

    /// Get the `signer_registration_total_since_startup_counter` counter.
    pub fn get_signer_registration_total_since_startup_counter(&self) -> &MetricCounter {
        &self.signer_registration_total_since_startup_counter
    }

    /// Get the `signer_registration_success_last_epoch_gauge` counter.
    pub fn get_signer_registration_success_last_epoch_gauge(&self) -> &MetricGauge {
        &self.signer_registration_success_last_epoch_gauge
    }

    /// Get the `signature_registration_success_since_startup_counter` counter.
    pub fn get_signature_registration_success_since_startup_counter(&self) -> &MetricCounter {
        &self.signature_registration_success_since_startup_counter
    }

    /// Get the `signature_registration_total_since_startup_counter` counter.
    pub fn get_signature_registration_total_since_startup_counter(&self) -> &MetricCounter {
        &self.signature_registration_total_since_startup_counter
    }

    /// Get the `signature_registration_success_last_epoch_gauge` counter.
    pub fn get_signature_registration_success_last_epoch_gauge(&self) -> &MetricGauge {
        &self.signature_registration_success_last_epoch_gauge
    }

    /// Get the `runtime_cycle_success_since_startup_counter` counter.
    pub fn get_runtime_cycle_success_since_startup_counter(&self) -> &MetricCounter {
        &self.runtime_cycle_success_since_startup_counter
    }

    /// Get the `runtime_cycle_total_since_startup_counter` counter.
    pub fn get_runtime_cycle_total_since_startup_counter(&self) -> &MetricCounter {
        &self.runtime_cycle_total_since_startup_counter
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
