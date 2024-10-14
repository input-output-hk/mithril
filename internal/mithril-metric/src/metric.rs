use prometheus::{core::Collector, Counter, Gauge, Opts};
use slog::{debug, Logger};

use mithril_common::{entities::Epoch, StdResult};

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
    pub fn new(logger: Logger, name: &str, help: &str) -> StdResult<Self> {
        let counter = MetricCounter::create_metric_counter(name, help)?;
        Ok(Self {
            logger,
            name: name.to_string(),
            counter: Box::new(counter),
        })
    }

    pub fn increment(&self) {
        debug!(self.logger, "incrementing '{}' counter", self.name);
        self.counter.inc();
    }

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

/// Metric gauge
pub struct MetricGauge {
    name: String,
    logger: Logger,
    gauge: Box<Gauge>,
}

impl MetricGauge {
    pub fn new(logger: Logger, name: &str, help: &str) -> StdResult<Self> {
        let gauge = MetricGauge::create_metric_gauge(name, help)?;
        Ok(Self {
            logger,
            name: name.to_string(),
            gauge: Box::new(gauge),
        })
    }

    pub fn record(&self, epoch: Epoch) {
        debug!(
            self.logger,
            "set '{}' gauge value to {}", self.name, epoch.0
        );
        self.gauge.set(epoch.0 as f64);
    }

    pub fn get(&self) -> Epoch {
        Epoch(self.gauge.get().round() as u64)
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
    fn test_metric_gauge_can_be_set() {
        let metric =
            MetricGauge::new(TestLogger::stdout(), "test_gauge", "test gauge help").unwrap();
        assert_eq!(metric.name(), "test_gauge");
        assert_eq!(metric.get(), Epoch(0));

        metric.record(Epoch(12));
        assert_eq!(metric.get(), Epoch(12));
    }
}
