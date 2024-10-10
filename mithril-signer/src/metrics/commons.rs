use prometheus::{core::Collector, Counter, Gauge, Opts};
use slog::{debug, Logger};

use mithril_common::{entities::Epoch, StdResult};

/// Type alias for a metric name.
pub type MetricName = str;

/// Type alias for a counter value.
pub type CounterValue = u32;

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
    pub fn new(logger: Logger, name: &str, help: &str) -> StdResult<Self> {
        let counter = MetricCounter::create_metric_counter(name, help)?;
        Ok(Self {
            logger,
            name: name.to_string(),
            counter: Box::new(counter),
        })
    }

    pub fn record(&self) {
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
impl MithrilMetric for MetricGauge {
    fn collector(&self) -> Box<dyn Collector> {
        self.gauge.clone()
    }
    fn name(&self) -> String {
        self.name.clone()
    }
}
