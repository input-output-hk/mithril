use prometheus::{core::Collector, Counter, Encoder, Gauge, Opts, Registry, TextEncoder};
use slog::{debug, Logger};

use mithril_common::logging::LoggerExtensions;
use mithril_common::{entities::Epoch, StdResult};

use super::{
    RUNTIME_CYCLE_SUCCESS_SINCE_STARTUP_METRIC_HELP,
    RUNTIME_CYCLE_SUCCESS_SINCE_STARTUP_METRIC_NAME, RUNTIME_CYCLE_TOTAL_SINCE_STARTUP_METRIC_HELP,
    RUNTIME_CYCLE_TOTAL_SINCE_STARTUP_METRIC_NAME,
    SIGNATURE_REGISTRATION_SUCCESS_LAST_EPOCH_METRIC_HELP,
    SIGNATURE_REGISTRATION_SUCCESS_LAST_EPOCH_METRIC_NAME,
    SIGNATURE_REGISTRATION_SUCCESS_SINCE_STARTUP_METRIC_HELP,
    SIGNATURE_REGISTRATION_SUCCESS_SINCE_STARTUP_METRIC_NAME,
    SIGNATURE_REGISTRATION_TOTAL_SINCE_STARTUP_METRIC_HELP,
    SIGNATURE_REGISTRATION_TOTAL_SINCE_STARTUP_METRIC_NAME,
    SIGNER_REGISTRATION_SUCCESS_LAST_EPOCH_METRIC_HELP,
    SIGNER_REGISTRATION_SUCCESS_LAST_EPOCH_METRIC_NAME,
    SIGNER_REGISTRATION_SUCCESS_SINCE_STARTUP_METRIC_HELP,
    SIGNER_REGISTRATION_SUCCESS_SINCE_STARTUP_METRIC_NAME,
    SIGNER_REGISTRATION_TOTAL_SINCE_STARTUP_METRIC_HELP,
    SIGNER_REGISTRATION_TOTAL_SINCE_STARTUP_METRIC_NAME,
};

/// Type alias for a metric name.
pub type MetricName = str;

/// Type alias for a counter value.
type CounterValue = u32;

/// Mithril metric
pub trait MithrilMetric {
    /// Metric name
    fn name(&self) -> String;

    /// Wrapped prometheus collector
    fn collector(&self) -> Box<dyn Collector>;
}

pub struct MetricCounter {
    name: String,
    logger: Logger,
    counter: Box<Counter>,
}

impl MetricCounter {
    fn new(logger: Logger, name: &str, help: &str) -> StdResult<Self> {
        let counter = MetricCounter::create_metric_counter(name, help)?;
        Ok(Self {
            logger,
            name: name.to_string(),
            counter: Box::new(counter),
        })
    }

    fn record(&self) {
        debug!(self.logger, "incrementing '{}' counter", self.name);
        self.counter.inc();
    }

    fn get(&self) -> CounterValue {
        self.counter.get().round() as CounterValue
    }

    fn create_metric_counter(name: &MetricName, help: &str) -> StdResult<Counter> {
        let counter_opts = Opts::new(name, help);
        let counter = Counter::with_opts(counter_opts)?;

        Ok(counter)
    }
}

impl MithrilMetric for MetricCounter {
    fn collector(&self) -> Box<dyn Collector> {
        self.counter.clone()
    }

    fn name(&self) -> String {
        self.name.clone()
    }
}

pub struct MetricGauge {
    name: String,
    logger: Logger,
    gauge: Box<Gauge>,
}

impl MetricGauge {
    fn new(logger: Logger, name: &str, help: &str) -> StdResult<Self> {
        let gauge = MetricGauge::create_metric_gauge(name, help)?;
        Ok(Self {
            logger,
            name: name.to_string(),
            gauge: Box::new(gauge),
        })
    }

    fn record(&self, epoch: Epoch) {
        debug!(
            self.logger,
            "set '{}' gauge value to {}", self.name, epoch.0
        );
        self.gauge.set(epoch.0 as f64);
    }

    fn get(&self) -> Epoch {
        Epoch(self.gauge.get().round() as u64)
    }

    fn create_metric_gauge(name: &MetricName, help: &str) -> StdResult<Gauge> {
        let gauge_opts = Opts::new(name, help);
        let gauge = Gauge::with_opts(gauge_opts)?;

        Ok(gauge)
    }
}
impl MithrilMetric for MetricGauge {
    fn collector(&self) -> Box<dyn Collector> {
        self.gauge.clone()
    }
    fn name(&self) -> String {
        self.name.clone()
    }
}

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
                SIGNER_REGISTRATION_SUCCESS_SINCE_STARTUP_METRIC_NAME,
                SIGNER_REGISTRATION_SUCCESS_SINCE_STARTUP_METRIC_HELP,
            )?,
        )?;

        let signer_registration_total_since_startup_counter = register(
            &registry,
            MetricCounter::new(
                logger.clone(),
                SIGNER_REGISTRATION_TOTAL_SINCE_STARTUP_METRIC_NAME,
                SIGNER_REGISTRATION_TOTAL_SINCE_STARTUP_METRIC_HELP,
            )?,
        )?;

        let signer_registration_success_last_epoch_gauge = register(
            &registry,
            MetricGauge::new(
                logger.clone(),
                SIGNER_REGISTRATION_SUCCESS_LAST_EPOCH_METRIC_NAME,
                SIGNER_REGISTRATION_SUCCESS_LAST_EPOCH_METRIC_HELP,
            )?,
        )?;
        // Signature registration metrics

        let signature_registration_success_since_startup_counter = register(
            &registry,
            MetricCounter::new(
                logger.clone(),
                SIGNATURE_REGISTRATION_SUCCESS_SINCE_STARTUP_METRIC_NAME,
                SIGNATURE_REGISTRATION_SUCCESS_SINCE_STARTUP_METRIC_HELP,
            )?,
        )?;

        let signature_registration_total_since_startup_counter = register(
            &registry,
            MetricCounter::new(
                logger.clone(),
                SIGNATURE_REGISTRATION_TOTAL_SINCE_STARTUP_METRIC_NAME,
                SIGNATURE_REGISTRATION_TOTAL_SINCE_STARTUP_METRIC_HELP,
            )?,
        )?;

        let signature_registration_success_last_epoch_gauge = register(
            &registry,
            MetricGauge::new(
                logger.clone(),
                SIGNATURE_REGISTRATION_SUCCESS_LAST_EPOCH_METRIC_NAME,
                SIGNATURE_REGISTRATION_SUCCESS_LAST_EPOCH_METRIC_HELP,
            )?,
        )?;

        // Runtime cycle metrics
        let runtime_cycle_success_since_startup_counter = register(
            &registry,
            MetricCounter::new(
                logger.clone(),
                RUNTIME_CYCLE_SUCCESS_SINCE_STARTUP_METRIC_NAME,
                RUNTIME_CYCLE_SUCCESS_SINCE_STARTUP_METRIC_HELP,
            )?,
        )?;
        let runtime_cycle_total_since_startup_counter = register(
            &registry,
            MetricCounter::new(
                logger.clone(),
                RUNTIME_CYCLE_TOTAL_SINCE_STARTUP_METRIC_NAME,
                RUNTIME_CYCLE_TOTAL_SINCE_STARTUP_METRIC_HELP,
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

    /// Export the metrics as a string with the Open Metrics standard format.
    /// These metrics can be exposed on an HTTP server.
    pub fn export_metrics(&self) -> StdResult<String> {
        let mut buffer = vec![];
        let encoder = TextEncoder::new();
        let metric_families = self.registry.gather();
        encoder.encode(&metric_families, &mut buffer)?;

        Ok(String::from_utf8(buffer)?)
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
                RUNTIME_CYCLE_SUCCESS_SINCE_STARTUP_METRIC_NAME.to_string(),
                Value::Counter(0.0),
            ),
            (
                RUNTIME_CYCLE_TOTAL_SINCE_STARTUP_METRIC_NAME.to_string(),
                Value::Counter(0.0),
            ),
            (
                SIGNATURE_REGISTRATION_SUCCESS_LAST_EPOCH_METRIC_NAME.to_string(),
                Value::Gauge(0.0),
            ),
            (
                SIGNATURE_REGISTRATION_SUCCESS_SINCE_STARTUP_METRIC_NAME.to_string(),
                Value::Counter(0.0),
            ),
            (
                SIGNATURE_REGISTRATION_TOTAL_SINCE_STARTUP_METRIC_NAME.to_string(),
                Value::Counter(0.0),
            ),
            (
                SIGNER_REGISTRATION_SUCCESS_LAST_EPOCH_METRIC_NAME.to_string(),
                Value::Gauge(0.0),
            ),
            (
                SIGNER_REGISTRATION_SUCCESS_SINCE_STARTUP_METRIC_NAME.to_string(),
                Value::Counter(0.0),
            ),
            (
                SIGNER_REGISTRATION_TOTAL_SINCE_STARTUP_METRIC_NAME.to_string(),
                Value::Counter(0.0),
            ),
        ]);
        assert_eq!(parsed_metrics_expected, parsed_metrics);
    }

    #[test]
    fn test_retrieve_metric_by_name() {
        let metrics_service = MetricsService::new(TestLogger::stdout()).unwrap();
        let name = metrics_service
            .runtime_cycle_success_since_startup_counter
            .name();
        assert_eq!(name, RUNTIME_CYCLE_SUCCESS_SINCE_STARTUP_METRIC_NAME);

        let name = metrics_service
            .signature_registration_success_last_epoch_gauge
            .name();
        assert_eq!(name, SIGNATURE_REGISTRATION_SUCCESS_LAST_EPOCH_METRIC_NAME);
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
