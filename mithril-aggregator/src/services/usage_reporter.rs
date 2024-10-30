use std::{collections::HashMap, sync::Arc, time::Duration};

use crate::{
    event_store::{EventMessage, TransmitterService},
    MetricsService,
};
use chrono::{DateTime, Utc};
use mithril_common::logging::LoggerExtensions;
use serde::{Deserialize, Serialize};
use slog::{info, Logger};

/// Message sent to the event store to report a metric value.
#[derive(Serialize, Deserialize)]
struct MetricEventMessage {
    counter: i64,
    duration: Duration,
    date: DateTime<Utc>,
}
/// Reporter of usage metrics of the application.
pub struct UsageReporter {
    transmitter_service: Arc<TransmitterService<EventMessage>>,
    metrics_service: Arc<MetricsService>,
    last_reported_metrics: HashMap<String, u32>,
    logger: Logger,
}

impl UsageReporter {
    /// Create a new UsageReporter.
    pub fn new(
        transmitter_service: Arc<TransmitterService<EventMessage>>,
        metrics_service: Arc<MetricsService>,
        logger: Logger,
    ) -> Self {
        Self {
            transmitter_service,
            metrics_service,
            last_reported_metrics: HashMap::new(),
            logger: logger.new_with_component_name::<Self>(),
        }
    }

    fn compute_metrics_delta(
        metrics_before: &HashMap<String, u32>,
        metrics_after: &HashMap<String, u32>,
    ) -> HashMap<String, i64> {
        metrics_after
            .iter()
            .map(|(name, value)| {
                let last_value = metrics_before.get(name).unwrap_or(&0);
                let delta: i64 = (*value as i64) - (*last_value as i64);
                (name.clone(), delta)
            })
            .filter(|(_name, value)| *value != 0)
            .collect()
    }

    fn send_metrics(&mut self, duration: &Duration) {
        let metrics = self.metrics_service.export_metrics_map();
        let delta = Self::compute_metrics_delta(&self.last_reported_metrics, &metrics);
        let date = Utc::now();

        self.last_reported_metrics = metrics;

        for (name, value) in delta {
            let _result = self
                .transmitter_service
                .send_event_message::<MetricEventMessage>(
                    "Metrics",
                    &name,
                    &MetricEventMessage {
                        counter: value,
                        duration: *duration,
                        date,
                    },
                    vec![],
                );
        }
    }

    /// Start a loop that send event about metrics at the given time interval.
    pub async fn run_forever(&mut self, run_interval: Duration) {
        let mut interval = tokio::time::interval(run_interval);

        loop {
            interval.tick().await;
            self.send_metrics(&run_interval);

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
    use tokio::sync::mpsc::UnboundedReceiver;

    use super::UsageReporter;
    use super::*;
    use crate::test_tools::TestLogger;

    fn build_usage_reporter() -> (
        UsageReporter,
        Arc<MetricsService>,
        UnboundedReceiver<EventMessage>,
    ) {
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<EventMessage>();
        let metrics_service = Arc::new(MetricsService::new(TestLogger::stdout()).unwrap());
        let transmitter_service = Arc::new(TransmitterService::new(tx, TestLogger::stdout()));
        let usage_reporter = UsageReporter::new(
            transmitter_service.clone(),
            metrics_service.clone(),
            TestLogger::stdout(),
        );
        (usage_reporter, metrics_service, rx)
    }

    fn received_messages(
        rx: &mut tokio::sync::mpsc::UnboundedReceiver<EventMessage>,
    ) -> Vec<EventMessage> {
        let mut received_messages: Vec<EventMessage> = Vec::new();
        while let Ok(message) = rx.try_recv() {
            received_messages.push(message);
        }
        received_messages
    }

    fn extract_metric_value(message: &EventMessage) -> (String, i64) {
        let metric_delta: MetricEventMessage =
            serde_json::from_value(message.content.clone()).unwrap();
        (message.action.clone(), metric_delta.counter)
    }

    #[test]
    fn when_no_metrics_no_message_sent() {
        let (mut usage_reporter, _metrics_service, mut rx) = build_usage_reporter();

        usage_reporter.send_metrics(&Duration::from_secs(10));

        let received_messages = received_messages(&mut rx);
        assert_eq!(0, received_messages.len());
    }

    #[test]
    fn verify_event_content_on_a_metric() {
        let (mut usage_reporter, metrics_service, mut rx) = build_usage_reporter();

        let metric = metrics_service.get_certificate_total_produced_since_startup();
        metric.increment_by(3);
        usage_reporter.send_metrics(&Duration::from_secs(10));

        let received_messages = received_messages(&mut rx);
        let message = &received_messages[0];
        assert_eq!(message.source, "Metrics");
        assert_eq!(message.action, metric.name());
        let message_content: MetricEventMessage =
            serde_json::from_value(message.content.clone()).unwrap();
        assert_eq!(3, message_content.counter);
        assert_eq!(Duration::from_secs(10), message_content.duration);
    }

    #[test]
    fn send_one_message_for_each_non_zero_value_metrics() {
        let (mut usage_reporter, metrics_service, mut rx) = build_usage_reporter();

        let metric_1 = metrics_service.get_certificate_total_produced_since_startup();
        let metric_2 = metrics_service.get_certificate_detail_total_served_since_startup();
        metric_1.increment();
        metric_2.increment();

        usage_reporter.send_metrics(&Duration::from_secs(10));

        let received_messages = received_messages(&mut rx);
        let metric_values: Vec<(String, i64)> =
            received_messages.iter().map(extract_metric_value).collect();

        assert!(metric_values.contains(&(metric_1.name(), 1)));
        assert!(metric_values.contains(&(metric_2.name(), 1)));
    }

    #[test]
    fn resend_only_delta_since_last_send() {
        let (mut usage_reporter, metrics_service, mut rx) = build_usage_reporter();

        let metric_1 = metrics_service.get_certificate_total_produced_since_startup();
        let metric_2 = metrics_service.get_certificate_detail_total_served_since_startup();

        {
            metric_1.increment_by(12);
            metric_2.increment_by(5);
            usage_reporter.send_metrics(&Duration::from_secs(10));

            let received_messages = received_messages(&mut rx);
            assert_eq!(2, received_messages.len());
        }
        {
            metric_2.increment_by(20);
            metric_2.increment_by(33);
            usage_reporter.send_metrics(&Duration::from_secs(10));

            let received_messages = received_messages(&mut rx);
            assert_eq!(1, received_messages.len());

            assert_eq!(
                (metric_2.name(), 53),
                extract_metric_value(&received_messages[0])
            );
        }
        {
            metric_2.increment_by(15);
            usage_reporter.send_metrics(&Duration::from_secs(10));

            let received_messages = received_messages(&mut rx);
            assert_eq!(1, received_messages.len());

            assert_eq!(
                (metric_2.name(), 15),
                extract_metric_value(&received_messages[0])
            );
        }
    }

    mod metric_delta {
        fn build_hashmap<T: Copy>(values: &[(&str, T)]) -> HashMap<String, T> {
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
                UsageReporter::compute_metrics_delta(&metrics_before, &metrics_after)
            );
        }

        #[test]
        fn should_contain_the_difference_of_an_increased_metric() {
            let metrics_before = build_hashmap(&[("a", 1)]);

            let metrics_after = build_hashmap(&[("a", 5)]);

            let expected = build_hashmap(&[("a", 4)]);

            assert_eq!(
                expected,
                UsageReporter::compute_metrics_delta(&metrics_before, &metrics_after)
            );
        }

        #[test]
        fn should_contain_the_difference_of_a_decreased_metric() {
            let metrics_before = build_hashmap(&[("a", 5)]);

            let metrics_after = build_hashmap(&[("a", 2)]);

            let expected = build_hashmap(&[("a", -3)]);

            assert_eq!(
                expected,
                UsageReporter::compute_metrics_delta(&metrics_before, &metrics_after)
            );
        }

        #[test]
        fn should_contain_new_value_of_a_metric_not_present_before() {
            let metrics_before = build_hashmap(&[]);

            let metrics_after = build_hashmap(&[("a", 5)]);

            let expected = build_hashmap(&[("a", 5)]);

            assert_eq!(
                expected,
                UsageReporter::compute_metrics_delta(&metrics_before, &metrics_after)
            );
        }

        #[test]
        fn should_not_panic_with_a_large_delta() {
            let metrics_at_0 = build_hashmap(&[("a", 0)]);
            let metrics_at_max = build_hashmap(&[("a", u32::MAX)]);

            assert_eq!(
                build_hashmap(&[("a", u32::MAX as i64)]),
                UsageReporter::compute_metrics_delta(&metrics_at_0, &metrics_at_max)
            );
            assert_eq!(
                build_hashmap(&[("a", -(u32::MAX as i64))]),
                UsageReporter::compute_metrics_delta(&metrics_at_max, &metrics_at_0)
            );
        }
    }
}
