use prometheus::{core::Collector, Counter, Gauge, Opts};
use slog::{debug, Logger};

use mithril_common::{entities::Epoch, StdResult};

/// Type alias for a metric name.
pub type MetricName = str;

/// Type alias for a counter value.
pub type CounterValue = u32;

/// Create a MetricService.
///
/// To build the service you need to provide the structure name and a list of metrics.
/// Each metrics is defined by an attribute name, a type, a metric name and a help message.
/// When the metric name is not provided the attribute name will be used.
/// We can specify a prefix that will be added to all metric names.
///
/// The attribute name will be used to create a getter method for the metric.
///
/// Crate that use this macro should have `paste` as dependency.
///
/// build_metrics_service!(
///     MetricsService,
///     counter_example: MetricCounter(
///         "custom_counter_example_name",
///         "Example of a counter metric"
///     ),
///     gauge_example: MetricGauge(
///         "custom_gauge_example_name",
///         "Example of a gauge metric"
///     )
/// );
///
/// build_metrics_service!(
///     MetricsService,
///     counter_example: MetricCounter("Example of a counter metric"),
///     gauge_example: MetricGauge("Example of a gauge metric")
/// );
///
/// build_metrics_service!(
///     MetricsService,
///     "metric_prefix",
///     counter_example: MetricCounter("Example of a counter metric"),
///     gauge_example: MetricGauge("Example of a gauge metric")
/// );
///
/// let service = MetricsServiceExampleBuildWithMacro::new(TestLogger::stdout()).unwrap();
/// service.get_counter_example().record();
/// service.get_gauge_example().record(Epoch(12));
#[macro_export]
macro_rules! build_metrics_service {
    ($service:ident, $($metric_attribute:ident:$metric_type:ident($help:literal)),*) => {
        build_metrics_service!($service, "", $($metric_attribute:$metric_type("", $help)),*);
    };
    ($service:ident, $prefix:literal, $($metric_attribute:ident:$metric_type:ident($help:literal)),*) => {
        build_metrics_service!($service, $prefix, $($metric_attribute:$metric_type("", $help)),*);
    };
    ($service:ident, $($metric_attribute:ident:$metric_type:ident($name:literal, $help:literal)),*) => {
        build_metrics_service!($service, "", $($metric_attribute:$metric_type($name, $help)),*);
    };
    ($service:ident, $prefix:literal, $($metric_attribute:ident:$metric_type:ident($name:literal, $help:literal)),*) => {
        paste::item! {
            /// Metrics service which is responsible for recording and exposing metrics.
            pub struct $service {
                registry: Registry,
                $(
                    $metric_attribute: $metric_type,
                )*
            }

            impl $service {
                pub fn new(logger: Logger) -> StdResult<Self> {

                    let registry = Registry::new();

                    let prefix = if $prefix.is_empty() {
                        "".to_string()
                    } else {
                        format!("{}_", $prefix)
                    };

                    $(
                        let metric_name = if $name.is_empty() {
                            stringify!($metric_attribute)
                        } else {
                            $name
                        };
                        let $metric_attribute = $metric_type::new(
                            logger.clone(),
                            &format!("{}{}", prefix, metric_name),
                            $help,
                        )?;
                        registry.register($metric_attribute.collector())?;
                    )*

                    Ok(Self {
                        registry,
                        $(
                            $metric_attribute,
                        )*
                    })
                }
                $(
                    /// Get the `$metric_attribute` counter.
                    pub fn [<get_ $metric_attribute>](&self) -> &$metric_type {
                        &self.$metric_attribute
                    }
                )*
            }

            impl MetricsServiceTrait for $service {
                fn export_metrics(&self) -> StdResult<String> {
                    metrics_tools::export_metrics(&self.registry)
                }
            }

        }
    };
}

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

pub mod metrics_tools {

    use mithril_common::StdResult;
    use prometheus::TextEncoder;

    pub fn export_metrics(registry: &prometheus::Registry) -> StdResult<String> {
        let encoder = TextEncoder::new();
        let metric_families = registry.gather();
        // TODO check encode_utf8 do the same as encode and remove the commented code
        // let mut buffer = vec![];
        // encoder.encode(&metric_families, &mut buffer)?;
        // Ok(String::from_utf8(buffer)?)

        let mut buffer = String::new();
        encoder.encode_utf8(&metric_families, &mut buffer)?;
        Ok(buffer)
    }
}

#[cfg(test)]
pub mod test_tools {
    use std::{io, sync::Arc};

    use slog::{Drain, Logger};
    use slog_async::Async;
    use slog_term::{CompactFormat, PlainDecorator};
    pub struct TestLogger;

    impl TestLogger {
        fn from_writer<W: io::Write + Send + 'static>(writer: W) -> Logger {
            let decorator = PlainDecorator::new(writer);
            let drain = CompactFormat::new(decorator).build().fuse();
            let drain = Async::new(drain).build().fuse();
            Logger::root(Arc::new(drain), slog::o!())
        }

        pub fn stdout() -> Logger {
            Self::from_writer(slog_term::TestStdoutWriter)
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use crate::MetricsServiceTrait;

    use super::*;
    use prometheus::Registry;
    use prometheus_parse::Value;
    use test_tools::TestLogger;

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
    fn test_metric_counter_can_be_incremented() {
        let metric =
            MetricCounter::new(TestLogger::stdout(), "test_counter", "test counter help").unwrap();
        assert_eq!(metric.name(), "test_counter");
        assert_eq!(metric.get(), 0);

        metric.record();
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

    mod tools {
        use super::*;
        use prometheus::Registry;

        use mithril_common::entities::Epoch;

        #[test]
        fn test_export_metrics() {
            let counter_metric =
                MetricCounter::new(TestLogger::stdout(), "test_counter", "test counter help")
                    .unwrap();
            counter_metric.record();

            let gauge_metric =
                MetricGauge::new(TestLogger::stdout(), "test_gauge", "test gauge help").unwrap();
            gauge_metric.record(Epoch(12));

            let registry = Registry::new();
            registry.register(counter_metric.collector()).unwrap();
            registry.register(gauge_metric.collector()).unwrap();

            let exported_metrics = metrics_tools::export_metrics(&registry).unwrap();

            let parsed_metrics = parse_metrics(&exported_metrics).unwrap();

            let parsed_metrics_expected = BTreeMap::from([
                (counter_metric.name(), Value::Counter(1.0)),
                (gauge_metric.name(), Value::Gauge(12.0)),
            ]);

            assert_eq!(parsed_metrics_expected, parsed_metrics);
        }
    }

    pub struct MetricsServiceExample {
        registry: Registry,
        counter_example: MetricCounter,
        gauge_example: MetricGauge,
    }

    impl MetricsServiceExample {
        pub fn new(logger: Logger) -> StdResult<Self> {
            let registry = Registry::new();

            let counter_example = MetricCounter::new(
                logger.clone(),
                "counter_example",
                "Example of a counter metric",
            )?;
            registry.register(counter_example.collector())?;

            let gauge_example =
                MetricGauge::new(logger.clone(), "gauge_example", "Example of a gauge metric")?;
            registry.register(gauge_example.collector())?;

            Ok(Self {
                registry,
                counter_example,
                gauge_example,
            })
        }

        /// Get the `counter_example` counter.
        pub fn get_counter_example(&self) -> &MetricCounter {
            &self.counter_example
        }

        /// Get the `gauge_example` counter.
        pub fn get_gauge_example(&self) -> &MetricGauge {
            &self.gauge_example
        }
    }

    impl MetricsServiceTrait for MetricsServiceExample {
        fn export_metrics(&self) -> StdResult<String> {
            metrics_tools::export_metrics(&self.registry)
        }
    }

    #[test]
    fn test_service_creation() {
        let service = MetricsServiceExample::new(TestLogger::stdout()).unwrap();
        service.get_counter_example().record();
        service.get_counter_example().record();
        service.get_gauge_example().record(Epoch(12));

        assert_eq!(2, service.get_counter_example().get());
        assert_eq!(Epoch(12), service.get_gauge_example().get());
    }

    build_metrics_service!(
        MetricsServiceExampleBuildWithMacro,
        counter_example: MetricCounter(
            "custom_counter_example_name",
            "Example of a counter metric"
        ),
        gauge_example: MetricGauge(
            "custom_gauge_example_name",
            "Example of a gauge metric"
        )
    );

    #[test]
    fn test_service_creation_using_build_metrics_service_macro() {
        let service = MetricsServiceExampleBuildWithMacro::new(TestLogger::stdout()).unwrap();
        service.get_counter_example().record();
        service.get_counter_example().record();
        service.get_gauge_example().record(Epoch(12));

        assert_eq!(2, service.get_counter_example().get());
        assert_eq!(Epoch(12), service.get_gauge_example().get());
    }

    #[test]
    fn test_build_metrics_service_named_metrics_with_attribute_name() {
        let service = MetricsServiceExampleBuildWithMacro::new(TestLogger::stdout()).unwrap();
        assert_eq!(
            "custom_counter_example_name",
            service.get_counter_example().name()
        );
        assert_eq!(
            "custom_gauge_example_name",
            service.get_gauge_example().name()
        );
    }

    build_metrics_service!(
        MetricsServiceExampleBuildWithMacroWithoutMetricName,
        counter_example: MetricCounter("Example of a counter metric"),
        gauge_example: MetricGauge("Example of a gauge metric")
    );

    #[test]
    fn test_build_metrics_service_macro_without_metric_name() {
        let service = MetricsServiceExampleBuildWithMacro::new(TestLogger::stdout()).unwrap();
        service.get_counter_example().record();
        service.get_counter_example().record();
        service.get_gauge_example().record(Epoch(12));

        assert_eq!(2, service.get_counter_example().get());
        assert_eq!(Epoch(12), service.get_gauge_example().get());
    }

    #[test]
    fn test_build_metrics_service_named_metrics_without_attribute_name() {
        let service =
            MetricsServiceExampleBuildWithMacroWithoutMetricName::new(TestLogger::stdout())
                .unwrap();
        assert_eq!("counter_example", service.get_counter_example().name());
        assert_eq!("gauge_example", service.get_gauge_example().name());
    }

    build_metrics_service!(
        MetricsServiceExampleBuildWithMacroUsingPrefix,
        "metric_prefix",
        counter_example: MetricCounter("Example of a counter metric"),
        gauge_example: MetricGauge("Example of a gauge metric")
    );

    #[test]
    fn test_build_metrics_service_named_metrics_with_attribute_name_and_prefix() {
        let service =
            MetricsServiceExampleBuildWithMacroUsingPrefix::new(TestLogger::stdout()).unwrap();
        assert_eq!(
            "metric_prefix_counter_example",
            service.get_counter_example().name()
        );
        assert_eq!(
            "metric_prefix_gauge_example",
            service.get_gauge_example().name()
        );
    }

    #[test]
    fn test_build_metrics_service_implement_export() {
        let service =
            MetricsServiceExampleBuildWithMacroUsingPrefix::new(TestLogger::stdout()).unwrap();
        service.get_gauge_example().record(Epoch(12));

        let export = service.export_metrics().unwrap();
        let metrics = parse_metrics(&export).unwrap();

        assert_eq!(
            Value::Gauge(12.0),
            metrics
                .get(&service.get_gauge_example().name())
                .unwrap()
                .clone()
        );
    }
}
