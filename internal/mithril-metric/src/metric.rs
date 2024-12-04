//! This module contains wrapper to prometheus metrics for use in a metrics service.

use prometheus::{core::Collector, Counter, CounterVec, Gauge, Opts};
use slog::{debug, Logger};

use mithril_common::StdResult;

/// Type alias for a metric name.
pub type MetricName = str;

/// Type alias for a counter value.
pub type CounterValue = u32;

/// Metric collector
pub trait MetricCollector {
    /// Metric name
    fn name(&self) -> String;

    /// Wrapped prometheus collector
    fn collector(&self) -> Box<dyn Collector>;
}

/// Metric counter
pub struct MetricCounter {
    name: String,
    logger: Logger,
    counter: Box<Counter>,
}

impl MetricCounter {
    /// Create a new metric counter.
    pub fn new(logger: Logger, name: &str, help: &str) -> StdResult<Self> {
        let counter = MetricCounter::create_metric_counter(name, help)?;
        Ok(Self {
            logger,
            name: name.to_string(),
            counter: Box::new(counter),
        })
    }

    /// Increment the counter.
    pub fn increment(&self) {
        debug!(self.logger, "Incrementing '{}' counter", self.name);
        self.counter.inc();
    }

    /// Increment the counter by a value.
    pub fn increment_by(&self, value: CounterValue) {
        debug!(
            self.logger,
            "Incrementing '{}' counter by {}", self.name, value
        );
        self.counter.inc_by(value as f64);
    }

    /// Get the counter value.
    pub fn get(&self) -> CounterValue {
        self.counter.get().round() as CounterValue
    }

    fn create_metric_counter(name: &MetricName, help: &str) -> StdResult<Counter> {
        let counter_opts = Opts::new(name, help);
        let counter = Counter::with_opts(counter_opts)?;

        Ok(counter)
    }
}

impl MetricCollector for MetricCounter {
    fn collector(&self) -> Box<dyn Collector> {
        self.counter.clone()
    }

    fn name(&self) -> String {
        self.name.clone()
    }
}

/// Metric counter vec
pub struct MetricCounterVec {
    name: String,
    logger: Logger,
    counter: Box<CounterVec>,
}

impl MetricCounterVec {
    /// Create a new metric counter.
    pub fn new(logger: Logger, name: &str, help: &str, labels: &[&str]) -> StdResult<Self> {
        let counter = MetricCounterVec::create_metric_counter_vec(name, help, labels)?;
        Ok(Self {
            logger,
            name: name.to_string(),
            counter: Box::new(counter),
        })
    }

    /// Increment the counter.
    pub fn increment(&self, labels: &[&str]) {
        debug!(self.logger, "Incrementing '{}' counter", self.name);
        self.counter.with_label_values(labels).inc();
    }

    /// Increment the counter by a value.
    pub fn increment_by(&self, labels: &[&str], value: CounterValue) {
        debug!(
            self.logger,
            "Incrementing '{}' counter by {}", self.name, value
        );
        self.counter.with_label_values(labels).inc_by(value as f64);
    }

    /// Get the counter value.
    pub fn get(&self, labels: &[&str]) -> CounterValue {
        self.counter.with_label_values(labels).get().round() as CounterValue
    }

    fn create_metric_counter_vec(
        name: &MetricName,
        help: &str,
        labels: &[&str],
    ) -> StdResult<CounterVec> {
        let counter_opts = Opts::new(name, help);
        let counter = CounterVec::new(counter_opts, labels)?;
        Ok(counter)
    }
}

impl MetricCollector for MetricCounterVec {
    fn collector(&self) -> Box<dyn Collector> {
        self.counter.clone()
    }

    fn name(&self) -> String {
        self.name.clone()
    }
}

/// Metric gauge
pub struct MetricGauge {
    name: String,
    logger: Logger,
    gauge: Box<Gauge>,
}

impl MetricGauge {
    /// Create a new metric gauge.
    pub fn new(logger: Logger, name: &str, help: &str) -> StdResult<Self> {
        let gauge = MetricGauge::create_metric_gauge(name, help)?;
        Ok(Self {
            logger,
            name: name.to_string(),
            gauge: Box::new(gauge),
        })
    }

    /// Record a value in the gauge.
    pub fn record<T: Into<f64>>(&self, value: T) {
        let value = value.into();
        debug!(self.logger, "Set '{}' gauge value to {}", self.name, value);
        self.gauge.set(value);
    }

    /// Get the gauge value.
    pub fn get(&self) -> f64 {
        self.gauge.get()
    }

    fn create_metric_gauge(name: &MetricName, help: &str) -> StdResult<Gauge> {
        let gauge_opts = Opts::new(name, help);
        let gauge = Gauge::with_opts(gauge_opts)?;

        Ok(gauge)
    }
}
impl MetricCollector for MetricGauge {
    fn collector(&self) -> Box<dyn Collector> {
        self.gauge.clone()
    }
    fn name(&self) -> String {
        self.name.clone()
    }
}

#[cfg(test)]
mod tests {
    use crate::helper::test_tools::TestLogger;

    use super::*;

    #[test]
    fn test_metric_counter_can_be_incremented() {
        let metric =
            MetricCounter::new(TestLogger::stdout(), "test_counter", "test counter help").unwrap();
        assert_eq!(metric.name(), "test_counter");
        assert_eq!(metric.get(), 0);

        metric.increment();
        assert_eq!(metric.get(), 1);
    }

    #[test]
    fn test_metric_gauge_can_be_recorded() {
        let metric =
            MetricGauge::new(TestLogger::stdout(), "test_gauge", "test gauge help").unwrap();
        assert_eq!(metric.name(), "test_gauge");
        assert_eq!(metric.get(), 0.0);

        metric.record(12.3);
        assert_eq!(metric.get(), 12.3);
    }

    #[test]
    fn test_metric_counter_can_be_incremented_more_than_one() {
        let metric =
            MetricCounter::new(TestLogger::stdout(), "test_counter", "test counter help").unwrap();

        metric.increment_by(37);
        assert_eq!(metric.get(), 37);
    }

    #[test]
    fn test_metric_counter_vec_can_be_recorded() {
        let metric = MetricCounterVec::new(
            TestLogger::stdout(),
            "test_counter_vec",
            "test counter vec help",
            &["label_1", "label_2"],
        )
        .unwrap();

        metric.increment(&["A", "200"]);
        metric.increment_by(&["B", "100"], 22);
        assert_eq!(metric.get(&["A", "200"]), 1);
        assert_eq!(metric.get(&["B", "100"]), 22);
    }
}
