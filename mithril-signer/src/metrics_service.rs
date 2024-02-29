use mithril_common::StdResult;
use prometheus::{Counter, Encoder, Opts, Registry, TextEncoder};
use slog_scope::debug;

/// Type alias for a metric name.
pub type MetricName = str;

/// Metrics service which is responsible for recording and exposing metrics.
pub struct MetricsService {
    registry: Registry,
    signer_registration_success_since_startup_counter: Box<Counter>,
    signer_registration_total_since_startup_counter: Box<Counter>,
    signature_registration_success_since_startup_counter: Box<Counter>,
    signature_registration_total_since_startup_counter: Box<Counter>,
    runtime_cycle_success_since_startup_counter: Box<Counter>,
    runtime_cycle_total_since_startup_counter: Box<Counter>,
}

impl MetricsService {
    /// Create a new `MetricsService` instance.
    pub fn new() -> StdResult<Self> {
        let registry = Registry::new();

        let signer_registration_success_since_startup_counter =
            Box::new(Self::create_metric_counter(
                "signer_registration_success_since_startup",
                "Number of successful signer registrations since startup",
            )?);
        registry.register(signer_registration_success_since_startup_counter.clone())?;

        let signer_registration_total_since_startup_counter =
            Box::new(Self::create_metric_counter(
                "signer_registration_total_since_startup",
                "Number of signer registrations since startup",
            )?);
        registry.register(signer_registration_total_since_startup_counter.clone())?;

        let signature_registration_success_since_startup_counter =
            Box::new(Self::create_metric_counter(
                "signature_registration_success_since_startup",
                "Number of successful signature registrations since startup",
            )?);
        registry.register(signature_registration_success_since_startup_counter.clone())?;

        let signature_registration_total_since_startup_counter =
            Box::new(Self::create_metric_counter(
                "signature_registration_total_since_startup",
                "Number of signature registrations since startup",
            )?);
        registry.register(signature_registration_total_since_startup_counter.clone())?;

        let runtime_cycle_success_since_startup_counter = Box::new(Self::create_metric_counter(
            "runtime_cycle_success_since_startup",
            "Number of successful runtime cycles since startup",
        )?);
        registry.register(runtime_cycle_success_since_startup_counter.clone())?;

        let runtime_cycle_total_since_startup_counter = Box::new(Self::create_metric_counter(
            "runtime_cycle_total_since_startup",
            "Number of runtime cycles since startup",
        )?);
        registry.register(runtime_cycle_total_since_startup_counter.clone())?;

        Ok(Self {
            registry,
            signer_registration_success_since_startup_counter,
            signer_registration_total_since_startup_counter,
            signature_registration_success_since_startup_counter,
            signature_registration_total_since_startup_counter,
            runtime_cycle_success_since_startup_counter,
            runtime_cycle_total_since_startup_counter,
        })
    }

    fn create_metric_counter(name: &MetricName, help: &str) -> StdResult<Counter> {
        let counter_opts = Opts::new(name, help);
        let counter = Counter::with_opts(counter_opts)?;

        Ok(counter)
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
    pub fn signer_registration_success_since_startup_counter_get(&self) -> u32 {
        self.signer_registration_success_since_startup_counter
            .get()
            .round() as u32
    }

    /// Increment the `signer_registration_total_since_startup` counter.
    pub fn signer_registration_total_since_startup_counter_increment(&self) {
        debug!("MetricsService: incrementing 'signer_registration_total_since_startup' counter");
        self.signer_registration_total_since_startup_counter.inc();
    }

    /// Get the `signer_registration_total_since_startup` counter.
    pub fn signer_registration_total_since_startup_counter_get(&self) -> u32 {
        self.signer_registration_total_since_startup_counter
            .get()
            .round() as u32
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
    pub fn signature_registration_success_since_startup_counter_get(&self) -> u32 {
        self.signature_registration_success_since_startup_counter
            .get()
            .round() as u32
    }

    /// Increment the `signature_registration_total_since_startup` counter.
    pub fn signature_registration_total_since_startup_counter_increment(&self) {
        debug!("MetricsService: incrementing 'signature_registration_total_since_startup' counter");
        self.signature_registration_total_since_startup_counter
            .inc();
    }

    /// Get the `signature_registration_total_since_startup` counter.
    pub fn signature_registration_total_since_startup_counter_get(&self) -> u32 {
        self.signature_registration_total_since_startup_counter
            .get()
            .round() as u32
    }

    /// Increment the `runtime_cycle_total_since_startup` counter.
    pub fn runtime_cycle_total_since_startup_counter_increment(&self) {
        debug!("MetricsService: incrementing 'runtime_cycle_total_since_startup' counter");
        self.runtime_cycle_total_since_startup_counter.inc();
    }

    /// Get the `runtime_cycle_total_since_startup` counter.
    pub fn runtime_cycle_total_since_startup_counter_get(&self) -> u32 {
        self.runtime_cycle_total_since_startup_counter.get().round() as u32
    }

    /// Increment the `runtime_cycle_success_since_startup` counter.
    pub fn runtime_cycle_success_since_startup_counter_increment(&self) {
        debug!("MetricsService: incrementing 'runtime_cycle_success_since_startup' counter");
        self.runtime_cycle_success_since_startup_counter.inc();
    }

    /// Get the `runtime_cycle_success_since_startup` counter.
    pub fn runtime_cycle_success_since_startup_counter_get(&self) -> u32 {
        self.runtime_cycle_success_since_startup_counter
            .get()
            .round() as u32
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_signer_registration_success_since_startup_counter_increment() {
        let metrics_service = MetricsService::new().unwrap();

        metrics_service.signer_registration_success_since_startup_counter_increment();
        assert_eq!(
            1,
            metrics_service.signer_registration_success_since_startup_counter_get(),
        );
    }

    #[test]
    fn test_signer_registration_total_since_startup_counter_increment() {
        let metrics_service = MetricsService::new().unwrap();

        metrics_service.signer_registration_total_since_startup_counter_increment();
        assert_eq!(
            1,
            metrics_service.signer_registration_total_since_startup_counter_get(),
        );
    }

    #[test]
    fn test_signature_registration_success_since_startup_counter_increment() {
        let metrics_service = MetricsService::new().unwrap();

        metrics_service.signature_registration_success_since_startup_counter_increment();
        assert_eq!(
            1,
            metrics_service.signature_registration_success_since_startup_counter_get(),
        );
    }

    #[test]
    fn test_signature_registration_total_since_startup_counter_increment() {
        let metrics_service = MetricsService::new().unwrap();

        metrics_service.signature_registration_total_since_startup_counter_increment();
        assert_eq!(
            1,
            metrics_service.signature_registration_total_since_startup_counter_get(),
        );
    }

    #[test]
    fn test_runtime_cycle_success_since_startup_counter_increment() {
        let metrics_service = MetricsService::new().unwrap();

        metrics_service.runtime_cycle_success_since_startup_counter_increment();
        assert_eq!(
            1,
            metrics_service.runtime_cycle_success_since_startup_counter_get(),
        );
    }

    #[test]
    fn test_runtime_cycle_total_since_startup_counter_increment() {
        let metrics_service = MetricsService::new().unwrap();

        metrics_service.runtime_cycle_total_since_startup_counter_increment();
        assert_eq!(
            1,
            metrics_service.runtime_cycle_total_since_startup_counter_get(),
        );
    }
}
