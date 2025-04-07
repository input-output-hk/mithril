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
    /// Name of the metric.
    name: String,
    /// Value of the metric.
    value: i64,
    /// Period of time during which the metric was collected.
    period: Duration,
    /// Identify the origin of the metric.
    origin: String,
    /// Date at which the metric was collected.
    date: DateTime<Utc>,
}
/// Reporter of usage metrics of the application.
pub struct UsageReporter {
    transmitter_service: Arc<TransmitterService<EventMessage>>,
    metrics_service: Arc<MetricsService>,
    last_reported_metrics: HashMap<String, HashMap<String, u32>>,
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
        metrics_before: Option<&HashMap<String, u32>>,
        metrics_after: &HashMap<String, u32>,
    ) -> HashMap<String, i64> {
        metrics_after
            .iter()
            .map(|(name, value)| {
                let last_value = metrics_before.and_then(|m| m.get(name)).unwrap_or(&0);
                let delta: i64 = (*value as i64) - (*last_value as i64);
                (name.clone(), delta)
            })
            .filter(|(_name, value)| *value != 0)
            .collect()
    }

    fn compute_metrics_delta_with_label(
        metrics_before: &HashMap<String, HashMap<String, u32>>,
        metrics_after: &HashMap<String, HashMap<String, u32>>,
    ) -> HashMap<String, HashMap<String, i64>> {
        metrics_after
            .iter()
            .map(|(name, label_values)| {
                (
                    name.clone(),
                    UsageReporter::compute_metrics_delta(metrics_before.get(name), label_values),
                )
            })
            .filter(|(_name, value)| !value.is_empty())
            .collect()
    }

    fn send_metrics(&mut self, duration: &Duration) {
        let metrics = self.metrics_service.export_metrics_map();
        let delta = Self::compute_metrics_delta_with_label(&self.last_reported_metrics, &metrics);
        let date = Utc::now();

        self.last_reported_metrics = metrics;

        for (name, origin_values) in delta {
            for (origin, value) in origin_values {
                let message = Self::create_metrics_event_message(
                    name.clone(),
                    value,
                    *duration,
                    origin,
                    date,
                );
                self.transmitter_service.send(message);
            }
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

    /// Create a new EventMessage for a metrics.
    pub fn create_metrics_event_message(
        name: String,
        value: i64,
        period: Duration,
        origin: String,
        date: DateTime<Utc>,
    ) -> EventMessage {
        EventMessage::new(
            "Metrics",
            &name.clone(),
            &MetricEventMessage {
                name,
                value,
                period,
                origin,
                date,
            },
            vec![],
        )
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
        (message.action.clone(), metric_delta.value)
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
        assert_eq!(3, message_content.value);
        assert_eq!(Duration::from_secs(10), message_content.period);
        assert_eq!("".to_string(), message_content.origin);
    }

    #[test]
    fn verify_event_content_on_a_metric_with_label() {
        let (mut usage_reporter, metrics_service, mut rx) = build_usage_reporter();

        let metric = metrics_service.get_certificate_detail_total_served_since_startup();
        metric.increment_by(&["ORIGIN_A"], 3);
        usage_reporter.send_metrics(&Duration::from_secs(10));

        let received_messages = received_messages(&mut rx);
        let message = &received_messages[0];
        assert_eq!(message.source, "Metrics");
        assert_eq!(message.action, metric.name());
        let message_content: MetricEventMessage =
            serde_json::from_value(message.content.clone()).unwrap();
        assert_eq!(3, message_content.value);
        assert_eq!(Duration::from_secs(10), message_content.period);
        assert_eq!("ORIGIN_A", message_content.origin);
    }

    #[test]
    fn send_one_message_per_origin() {
        let (mut usage_reporter, metrics_service, mut rx) = build_usage_reporter();

        let metric = metrics_service.get_certificate_detail_total_served_since_startup();
        metric.increment_by(&["ORIGIN_A"], 3);
        metric.increment_by(&["ORIGIN_B"], 7);
        usage_reporter.send_metrics(&Duration::from_secs(10));

        let received_messages = received_messages(&mut rx);
        assert_eq!(2, received_messages.len());
        let received_messages: HashMap<_, _> = received_messages
            .iter()
            .map(|message| {
                let event: MetricEventMessage =
                    serde_json::from_value(message.content.clone()).unwrap();
                (event.origin, event.value)
            })
            .collect();
        assert_eq!(Some(&3), received_messages.get("ORIGIN_A"));
        assert_eq!(Some(&7), received_messages.get("ORIGIN_B"));
    }

    #[test]
    fn send_one_message_for_each_non_zero_value_metrics() {
        let (mut usage_reporter, metrics_service, mut rx) = build_usage_reporter();

        let metric_1 = metrics_service.get_certificate_total_produced_since_startup();
        let metric_2 = metrics_service.get_certificate_detail_total_served_since_startup();
        metric_1.increment();
        metric_2.increment(&["ORIGIN"]);

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
            metric_2.increment_by(&["ORIGIN"], 5);
            usage_reporter.send_metrics(&Duration::from_secs(10));

            let received_messages = received_messages(&mut rx);
            assert_eq!(2, received_messages.len());
        }
        {
            metric_2.increment_by(&["ORIGIN"], 20);
            metric_2.increment_by(&["ORIGIN"], 33);
            usage_reporter.send_metrics(&Duration::from_secs(10));

            let received_messages = received_messages(&mut rx);
            assert_eq!(1, received_messages.len());

            assert_eq!(
                (metric_2.name(), 53),
                extract_metric_value(&received_messages[0])
            );
        }
        {
            metric_2.increment_by(&["ORIGIN"], 15);
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
        use super::*;

        fn build_hashmap<T: Copy>(
            values: &[(&str, &[(&str, T)])],
        ) -> HashMap<String, HashMap<String, T>> {
            values
                .iter()
                .map(|(name, origin_values)| {
                    let value_per_label: HashMap<String, T> = origin_values
                        .iter()
                        .map(|(origin, value)| (origin.to_string(), *value))
                        .collect();
                    (name.to_string(), value_per_label)
                })
                .collect()
        }

        #[test]
        fn should_not_contain_metric_that_not_change() {
            let metrics_before = build_hashmap(&[("metric_a", &[("origin_1", 1)])]);

            let metrics_after = build_hashmap(&[("metric_a", &[("origin_1", 1)])]);

            let expected = build_hashmap(&[]);

            assert_eq!(
                expected,
                UsageReporter::compute_metrics_delta_with_label(&metrics_before, &metrics_after)
            );
        }

        #[test]
        fn should_contain_the_difference_of_an_increased_metric() {
            let metrics_before = build_hashmap(&[("metric_a", &[("origin_1", 1)])]);

            let metrics_after = build_hashmap(&[("metric_a", &[("origin_1", 5)])]);

            let expected = build_hashmap(&[("metric_a", &[("origin_1", 4)])]);

            assert_eq!(
                expected,
                UsageReporter::compute_metrics_delta_with_label(&metrics_before, &metrics_after)
            );
        }

        #[test]
        fn should_contain_the_difference_of_a_decreased_metric() {
            let metrics_before = build_hashmap(&[("metric_a", &[("origin_1", 5)])]);

            let metrics_after = build_hashmap(&[("metric_a", &[("origin_1", 2)])]);

            let expected = build_hashmap(&[("metric_a", &[("origin_1", -3)])]);

            assert_eq!(
                expected,
                UsageReporter::compute_metrics_delta_with_label(&metrics_before, &metrics_after)
            );
        }

        #[test]
        fn should_contain_new_value_of_a_metric_not_present_before() {
            let metrics_before = build_hashmap(&[]);

            let metrics_after = build_hashmap(&[("metric_a", &[("origin_1", 5)])]);

            let expected = build_hashmap(&[("metric_a", &[("origin_1", 5)])]);

            assert_eq!(
                expected,
                UsageReporter::compute_metrics_delta_with_label(&metrics_before, &metrics_after)
            );
        }

        #[test]
        fn should_not_panic_with_a_large_delta() {
            let metrics_at_0 = build_hashmap(&[("metric_a", &[("origin_1", 0)])]);
            let metrics_at_max = build_hashmap(&[("metric_a", &[("origin_1", u32::MAX)])]);

            assert_eq!(
                build_hashmap(&[("metric_a", &[("origin_1", u32::MAX as i64)])]),
                UsageReporter::compute_metrics_delta_with_label(&metrics_at_0, &metrics_at_max)
            );
            assert_eq!(
                build_hashmap(&[("metric_a", &[("origin_1", -(u32::MAX as i64))])]),
                UsageReporter::compute_metrics_delta_with_label(&metrics_at_max, &metrics_at_0)
            );
        }

        #[test]
        fn should_contain_only_the_difference_for_origin_on_which_value_change() {
            let metrics_before = build_hashmap(&[(
                "metric_a",
                &[("origin_1", 1), ("origin_2", 3), ("origin_3", 7)],
            )]);

            let metrics_after = build_hashmap(&[(
                "metric_a",
                &[("origin_1", 5), ("origin_2", 3), ("origin_3", 9)],
            )]);

            let expected = build_hashmap(&[("metric_a", &[("origin_1", 4), ("origin_3", 2)])]);

            assert_eq!(
                expected,
                UsageReporter::compute_metrics_delta_with_label(&metrics_before, &metrics_after)
            );
        }
    }
}
