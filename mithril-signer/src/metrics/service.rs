use mithril_common::{entities::Epoch, StdResult};
use prometheus::{Counter, Encoder, Gauge, Opts, Registry, TextEncoder};
use slog_scope::debug;

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

/// Metrics service which is responsible for recording and exposing metrics.
pub struct MetricsService {
    registry: Registry,
    signer_registration_success_since_startup_counter: Box<Counter>,
    signer_registration_total_since_startup_counter: Box<Counter>,
    signer_registration_success_last_epoch_gauge: Box<Gauge>,
    signature_registration_success_since_startup_counter: Box<Counter>,
    signature_registration_total_since_startup_counter: Box<Counter>,
    signature_registration_success_last_epoch_gauge: Box<Gauge>,
    runtime_cycle_success_since_startup_counter: Box<Counter>,
    runtime_cycle_total_since_startup_counter: Box<Counter>,
}

impl MetricsService {
    /// Create a new `MetricsService` instance.
    pub fn new() -> StdResult<Self> {
        let registry = Registry::new();

        // Signer registration metrics
        let signer_registration_success_since_startup_counter =
            Box::new(Self::create_metric_counter(
                SIGNER_REGISTRATION_SUCCESS_SINCE_STARTUP_METRIC_NAME,
                SIGNER_REGISTRATION_SUCCESS_SINCE_STARTUP_METRIC_HELP,
            )?);
        registry.register(signer_registration_success_since_startup_counter.clone())?;

        let signer_registration_total_since_startup_counter =
            Box::new(Self::create_metric_counter(
                SIGNER_REGISTRATION_TOTAL_SINCE_STARTUP_METRIC_NAME,
                SIGNER_REGISTRATION_TOTAL_SINCE_STARTUP_METRIC_HELP,
            )?);
        registry.register(signer_registration_total_since_startup_counter.clone())?;

        let signer_registration_success_last_epoch_gauge = Box::new(Self::create_metric_gauge(
            SIGNER_REGISTRATION_SUCCESS_LAST_EPOCH_METRIC_NAME,
            SIGNER_REGISTRATION_SUCCESS_LAST_EPOCH_METRIC_HELP,
        )?);
        registry.register(signer_registration_success_last_epoch_gauge.clone())?;

        // Signature registration metrics
        let signature_registration_success_since_startup_counter =
            Box::new(Self::create_metric_counter(
                SIGNATURE_REGISTRATION_SUCCESS_SINCE_STARTUP_METRIC_NAME,
                SIGNATURE_REGISTRATION_SUCCESS_SINCE_STARTUP_METRIC_HELP,
            )?);
        registry.register(signature_registration_success_since_startup_counter.clone())?;

        let signature_registration_total_since_startup_counter =
            Box::new(Self::create_metric_counter(
                SIGNATURE_REGISTRATION_TOTAL_SINCE_STARTUP_METRIC_NAME,
                SIGNATURE_REGISTRATION_TOTAL_SINCE_STARTUP_METRIC_HELP,
            )?);
        registry.register(signature_registration_total_since_startup_counter.clone())?;

        let signature_registration_success_last_epoch_gauge = Box::new(Self::create_metric_gauge(
            SIGNATURE_REGISTRATION_SUCCESS_LAST_EPOCH_METRIC_NAME,
            SIGNATURE_REGISTRATION_SUCCESS_LAST_EPOCH_METRIC_HELP,
        )?);
        registry.register(signature_registration_success_last_epoch_gauge.clone())?;

        // Runtime cycle metrics
        let runtime_cycle_success_since_startup_counter = Box::new(Self::create_metric_counter(
            RUNTIME_CYCLE_SUCCESS_SINCE_STARTUP_METRIC_NAME,
            RUNTIME_CYCLE_SUCCESS_SINCE_STARTUP_METRIC_HELP,
        )?);
        registry.register(runtime_cycle_success_since_startup_counter.clone())?;

        let runtime_cycle_total_since_startup_counter = Box::new(Self::create_metric_counter(
            RUNTIME_CYCLE_TOTAL_SINCE_STARTUP_METRIC_NAME,
            RUNTIME_CYCLE_TOTAL_SINCE_STARTUP_METRIC_HELP,
        )?);
        registry.register(runtime_cycle_total_since_startup_counter.clone())?;

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

    fn create_metric_counter(name: &MetricName, help: &str) -> StdResult<Counter> {
        let counter_opts = Opts::new(name, help);
        let counter = Counter::with_opts(counter_opts)?;

        Ok(counter)
    }

    fn create_metric_gauge(name: &MetricName, help: &str) -> StdResult<Gauge> {
        let gauge_opts = Opts::new(name, help);
        let gauge = Gauge::with_opts(gauge_opts)?;

        Ok(gauge)
    }

    /// Export the metrics as a string with the Open Metrics standard format.
    /// These metrics can be exposed on a HTTP server.
    pub fn export_metrics(&self) -> StdResult<String> {
        let mut buffer = vec![];
        let encoder = TextEncoder::new();
        let metric_families = self.registry.gather();
        encoder.encode(&metric_families, &mut buffer).unwrap();

        Ok(String::from_utf8(buffer)?)
    }

    /// Increment the `signer_registration_success_since_startup` counter.
    pub fn signer_registration_success_since_startup_counter_increment(&self) {
        debug!("MetricsService: incrementing 'signer_registration_success_since_startup' counter");
        self.signer_registration_success_since_startup_counter.inc();
    }

    /// Get the `signer_registration_success_since_startup` counter.
    pub fn signer_registration_success_since_startup_counter_get(&self) -> CounterValue {
        self.signer_registration_success_since_startup_counter
            .get()
            .round() as CounterValue
    }

    /// Increment the `signer_registration_total_since_startup` counter.
    pub fn signer_registration_total_since_startup_counter_increment(&self) {
        debug!("MetricsService: incrementing 'signer_registration_total_since_startup' counter");
        self.signer_registration_total_since_startup_counter.inc();
    }

    /// Get the `signer_registration_total_since_startup` counter.
    pub fn signer_registration_total_since_startup_counter_get(&self) -> CounterValue {
        self.signer_registration_total_since_startup_counter
            .get()
            .round() as CounterValue
    }

    /// Set the `signer_registration_success_last_epoch` gauge value.
    pub fn signer_registration_success_last_epoch_gauge_set(&self, value: Epoch) {
        debug!("MetricsService: set 'signer_registration_success_last_epoch_set' gauge value to {value}");
        self.signer_registration_success_last_epoch_gauge
            .set(value.0 as f64);
    }

    /// Get the `signer_registration_success_last_epoch` gauge value.
    pub fn signer_registration_success_last_epoch_gauge_get(&self) -> Epoch {
        Epoch(
            self.signer_registration_success_last_epoch_gauge
                .get()
                .round() as u64,
        )
    }

    /// Increment the `signature_registration_success_since_startup` counter.
    pub fn signature_registration_success_since_startup_counter_increment(&self) {
        debug!(
            "MetricsService: incrementing 'signature_registration_success_since_startup' counter"
        );
        self.signature_registration_success_since_startup_counter
            .inc();
    }

    /// Get the `signature_registration_success_since_startup` counter.
    pub fn signature_registration_success_since_startup_counter_get(&self) -> CounterValue {
        self.signature_registration_success_since_startup_counter
            .get()
            .round() as CounterValue
    }

    /// Increment the `signature_registration_total_since_startup` counter.
    pub fn signature_registration_total_since_startup_counter_increment(&self) {
        debug!("MetricsService: incrementing 'signature_registration_total_since_startup' counter");
        self.signature_registration_total_since_startup_counter
            .inc();
    }

    /// Get the `signature_registration_total_since_startup` counter.
    pub fn signature_registration_total_since_startup_counter_get(&self) -> CounterValue {
        self.signature_registration_total_since_startup_counter
            .get()
            .round() as CounterValue
    }

    /// Set the `signature_registration_success_last_epoch` gauge value.
    pub fn signature_registration_success_last_epoch_gauge_set(&self, value: Epoch) {
        debug!("MetricsService: set 'signature_registration_success_last_epoch_set' gauge value to {value}");
        self.signature_registration_success_last_epoch_gauge
            .set(value.0 as f64);
    }

    /// Get the `signature_registration_success_last_epoch` gauge value.
    pub fn signature_registration_success_last_epoch_gauge_get(&self) -> Epoch {
        Epoch(
            self.signature_registration_success_last_epoch_gauge
                .get()
                .round() as u64,
        )
    }

    /// Increment the `runtime_cycle_total_since_startup` counter.
    pub fn runtime_cycle_total_since_startup_counter_increment(&self) {
        debug!("MetricsService: incrementing 'runtime_cycle_total_since_startup' counter");
        self.runtime_cycle_total_since_startup_counter.inc();
    }

    /// Get the `runtime_cycle_total_since_startup` counter.
    pub fn runtime_cycle_total_since_startup_counter_get(&self) -> CounterValue {
        self.runtime_cycle_total_since_startup_counter.get().round() as CounterValue
    }

    /// Increment the `runtime_cycle_success_since_startup` counter.
    pub fn runtime_cycle_success_since_startup_counter_increment(&self) {
        debug!("MetricsService: incrementing 'runtime_cycle_success_since_startup' counter");
        self.runtime_cycle_success_since_startup_counter.inc();
    }

    /// Get the `runtime_cycle_success_since_startup` counter.
    pub fn runtime_cycle_success_since_startup_counter_get(&self) -> CounterValue {
        self.runtime_cycle_success_since_startup_counter
            .get()
            .round() as CounterValue
    }
}

#[cfg(test)]
mod tests {
    use prometheus_parse::Value;
    use std::collections::BTreeMap;

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
        let metrics_service = MetricsService::new().unwrap();
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
    fn test_signer_registration_success_since_startup_counter_increment() {
        let metrics_service = MetricsService::new().unwrap();
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
        let metrics_service = MetricsService::new().unwrap();
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
        let metrics_service = MetricsService::new().unwrap();
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
        let metrics_service = MetricsService::new().unwrap();
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
        let metrics_service = MetricsService::new().unwrap();
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
        let metrics_service = MetricsService::new().unwrap();
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
        let metrics_service = MetricsService::new().unwrap();
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
        let metrics_service = MetricsService::new().unwrap();
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
