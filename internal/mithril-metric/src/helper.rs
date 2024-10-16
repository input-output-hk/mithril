//! Helper to create a metric service.

/// Create a MetricService.
///
/// To build the service you need to provide the structure name and a list of metrics.
/// Each metrics is defined by an attribute name, a type, a metric name and a help message.
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
/// let service = MetricsService::new(TestLogger::stdout()).unwrap();
/// service.get_counter_example().record();
/// service.get_gauge_example().record(Epoch(12));
#[macro_export]
macro_rules! build_metrics_service {
    ($service:ident, $($metric_attribute:ident:$metric_type:ident($name:literal, $help:literal)),*) => {
        paste::item! {
            /// Metrics service which is responsible for recording and exposing metrics.
            pub struct $service {
                registry: prometheus::Registry,
                $(
                    $metric_attribute: $metric_type,
                )*
            }

            impl $service {
                /// Create a new MetricsService instance.
                pub fn new(logger: slog::Logger) -> mithril_common::StdResult<Self> {

                    let registry = prometheus::Registry::new();

                    $(
                        let $metric_attribute = $metric_type::new(
                            logger.clone(),
                            $name,
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

            impl MetricsServiceExporter for $service {
                fn export_metrics(&self) -> mithril_common::StdResult<String> {
                    Ok(prometheus::TextEncoder::new().encode_to_string(&self.registry.gather())?)
                }
            }

        }
    };
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

    use crate::{MetricCollector, MetricCounter, MetricGauge, MetricsServiceExporter};

    use super::*;
    use mithril_common::{entities::Epoch, StdResult};
    use prometheus::{Registry, TextEncoder};
    use prometheus_parse::Value;
    use slog::Logger;
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

    impl MetricsServiceExporter for MetricsServiceExample {
        fn export_metrics(&self) -> StdResult<String> {
            Ok(TextEncoder::new().encode_to_string(&self.registry.gather())?)
        }
    }

    #[test]
    fn test_service_creation() {
        let service = MetricsServiceExample::new(TestLogger::stdout()).unwrap();
        service.get_counter_example().increment();
        service.get_counter_example().increment();
        service.get_gauge_example().record(Epoch(12));

        assert_eq!(2, service.get_counter_example().get());
        assert_eq!(Epoch(12), Epoch(service.get_gauge_example().get() as u64));
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
        service.get_counter_example().increment();
        service.get_counter_example().increment();
        service.get_gauge_example().record(Epoch(12));

        assert_eq!(2, service.get_counter_example().get());
        assert_eq!(Epoch(12), Epoch(service.get_gauge_example().get() as u64));
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

    #[test]
    fn test_build_metrics_service_provide_a_functional_export_metrics_function() {
        let service = MetricsServiceExampleBuildWithMacro::new(TestLogger::stdout()).unwrap();

        service.counter_example.increment();
        service.gauge_example.record(Epoch(12));

        let exported_metrics = service.export_metrics().unwrap();

        let parsed_metrics = parse_metrics(&exported_metrics).unwrap();

        let parsed_metrics_expected = BTreeMap::from([
            (service.counter_example.name(), Value::Counter(1.0)),
            (service.gauge_example.name(), Value::Gauge(12.0)),
        ]);

        assert_eq!(parsed_metrics_expected, parsed_metrics);
    }
}
