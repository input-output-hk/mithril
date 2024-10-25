use std::{collections::HashMap, sync::Arc, time::Duration};

use crate::{
    event_store::{EventMessage, TransmitterService},
    MetricsService,
};
use mithril_common::logging::LoggerExtensions;
use slog::{info, Logger};

/// Reporter of metrics about the usage of the application.
pub struct UsageReporter {
    transmitter_service: Arc<TransmitterService<EventMessage>>,
    metric_service: Arc<MetricsService>,
    last_metrics: HashMap<String, u32>,
    logger: Logger,
}

impl UsageReporter {
    /// Create a new UsageReporter.
    pub fn new(
        transmitter_service: Arc<TransmitterService<EventMessage>>,
        metric_service: Arc<MetricsService>,
        logger: Logger,
    ) -> Self {
        Self {
            transmitter_service,
            metric_service,
            last_metrics: HashMap::new(),
            logger: logger.new_with_component_name::<Self>(),
        }
    }

    fn metrics_delta(
        metrics_before: &HashMap<String, u32>,
        metrics_after: &HashMap<String, u32>,
    ) -> HashMap<String, u32> {
        metrics_after
            .iter()
            .map(|(name, value)| {
                let last_value = metrics_before.get(name).unwrap_or(&0);
                (name.clone(), value - last_value)
            })
            .filter(|(_name, value)| *value > 0)
            .collect()
    }

    fn send_metrics(&mut self) -> () {
        let metrics = self.metric_service.export_metrics_map();
        let delta = UsageReporter::metrics_delta(&self.last_metrics, &metrics);
        self.last_metrics = metrics;

        for (name, value) in delta {
            let _result = self.transmitter_service.send_event_message::<u32>(
                "Metrics",
                &name,
                &value,
                vec![],
            );
        }
    }

    /// Start a loop that call [run][Self::send_metrics] at the given time interval.
    pub async fn run_forever(&mut self, run_interval: Duration) {
        let mut interval = tokio::time::interval(run_interval);

        loop {
            interval.tick().await;
            self.send_metrics();

            info!(
                self.logger,
                "Metrics sent, Sleeping for {} seconds",
                run_interval.as_secs()
            );
        }
    }
}

#[cfg(test)]
mod tests {
    use mithril_metric::MetricCollector;

    use super::UsageReporter;
    use super::*;
    use crate::test_tools::TestLogger;

    #[tokio::test]
    async fn when_no_metrics_no_message_sent() {
        let (tx, mut rx) = tokio::sync::mpsc::unbounded_channel::<EventMessage>();
        let metric_service = Arc::new(MetricsService::new(TestLogger::stdout()).unwrap());
        let transmitter_service = Arc::new(TransmitterService::new(tx, TestLogger::stdout()));
        let mut usage_reporter = UsageReporter::new(
            transmitter_service.clone(),
            metric_service.clone(),
            TestLogger::stdout(),
        );

        usage_reporter.send_metrics();

        let received_messages = received_messages(&mut rx);
        assert_eq!(0, received_messages.len());
    }

    #[tokio::test]
    async fn send_one_message_for_each_non_zero_value_metrics() {
        let (tx, mut rx) = tokio::sync::mpsc::unbounded_channel::<EventMessage>();
        let metric_service = Arc::new(MetricsService::new(TestLogger::stdout()).unwrap());
        let transmitter_service = Arc::new(TransmitterService::new(tx, TestLogger::stdout()));
        let mut usage_reporter = UsageReporter::new(
            transmitter_service.clone(),
            metric_service.clone(),
            TestLogger::stdout(),
        );

        let metric_1 = metric_service.get_certificate_total_produced_since_startup();
        let metric_2 = metric_service.get_certificate_detail_total_served_since_startup();
        metric_1.increment();
        metric_2.increment();

        usage_reporter.send_metrics();

        let mut received_messages: Vec<EventMessage> = Vec::new();
        received_messages.push(rx.try_recv().unwrap());
        received_messages.push(rx.try_recv().unwrap());
        assert!(rx.try_recv().is_err());

        assert!(received_messages.contains(&EventMessage::new(
            "Metrics",
            &metric_1.name(),
            serde_json::json!(1)
        )));
        assert!(received_messages.contains(&EventMessage::new(
            "Metrics",
            &metric_2.name(),
            serde_json::json!(1)
        )));
    }

    #[tokio::test]
    async fn resend_only_delta_since_last_send() {
        let (tx, mut rx) = tokio::sync::mpsc::unbounded_channel::<EventMessage>();
        let metric_service = Arc::new(MetricsService::new(TestLogger::stdout()).unwrap());
        let transmitter_service = Arc::new(TransmitterService::new(tx, TestLogger::stdout()));
        let mut usage_reporter = UsageReporter::new(
            transmitter_service.clone(),
            metric_service.clone(),
            TestLogger::stdout(),
        );

        let metric_1 = metric_service.get_certificate_total_produced_since_startup();
        let metric_2 = metric_service.get_certificate_detail_total_served_since_startup();

        {
            metric_1.increment_by(12);
            metric_2.increment_by(5);
            usage_reporter.send_metrics();

            assert!(rx.try_recv().is_ok());
            assert!(rx.try_recv().is_ok());
            assert!(rx.try_recv().is_err());
        }
        {
            metric_2.increment_by(20);
            metric_2.increment_by(33);
            usage_reporter.send_metrics();

            let message = rx.try_recv().unwrap();
            assert!(rx.try_recv().is_err());

            assert_eq!(
                EventMessage::new("Metrics", &metric_2.name(), serde_json::json!(53)),
                message
            );
        }
        {
            metric_2.increment_by(15);
            usage_reporter.send_metrics();

            let message = rx.try_recv().unwrap();
            assert!(rx.try_recv().is_err());

            assert_eq!(
                EventMessage::new("Metrics", &metric_2.name(), serde_json::json!(15)),
                message
            );
        }
    }

    mod metric_delta {
        fn build_hashmap(values: &[(&str, u32)]) -> HashMap<String, u32> {
            let mut metrics = HashMap::new();
            for (name, value) in values {
                metrics.insert(name.to_string(), *value);
            }
            metrics
        }
        use super::*;

        #[test]
        fn should_not_contain_metric_that_not_change() {
            let metrics_before = build_hashmap(&[("a", 1)]);

            let metrics_after = build_hashmap(&[("a", 1)]);

            let expected = build_hashmap(&[]);

            assert_eq!(
                expected,
                UsageReporter::metrics_delta(&metrics_before, &metrics_after)
            );
        }

        #[test]
        fn should_contain_the_difference_of_a_value_increase() {
            let metrics_before = build_hashmap(&[("a", 1)]);

            let metrics_after = build_hashmap(&[("a", 5)]);

            let expected = build_hashmap(&[("a", 4)]);

            assert_eq!(
                expected,
                UsageReporter::metrics_delta(&metrics_before, &metrics_after)
            );
        }

        #[test]
        fn should_contains_new_value_of_a_metric_not_present_before() {
            let metrics_before = build_hashmap(&[]);

            let metrics_after = build_hashmap(&[("a", 5)]);

            let expected = build_hashmap(&[("a", 5)]);

            assert_eq!(
                expected,
                UsageReporter::metrics_delta(&metrics_before, &metrics_after)
            );
        }
    }
}

// TODO What should we do if the value decrease ?
// TODO What are the source, action, json content ?
// TODO In which module usage_reporter should be ? event_store ? service ?
