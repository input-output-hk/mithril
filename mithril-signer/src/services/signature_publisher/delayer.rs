use std::{sync::Arc, time::Duration};

use mithril_common::{
    entities::{ProtocolMessage, SignedEntityType, SingleSignature},
    logging::LoggerExtensions,
    StdResult,
};
use slog::{error, Logger};

use super::SignaturePublisher;

/// A decorator of [SignaturePublisher] that publishes right away on a first publisher
/// and with a delay on the second publisher.
pub struct SignaturePublisherDelayer {
    first_publisher: Arc<dyn SignaturePublisher>,
    second_publisher: Arc<dyn SignaturePublisher>,
    delay_between_publish: Duration,
    logger: Logger,
}

impl SignaturePublisherDelayer {
    /// Creates a new [SignaturePublisherDelayer]
    pub fn new(
        first_publisher: Arc<dyn SignaturePublisher>,
        second_publisher: Arc<dyn SignaturePublisher>,
        delay_between_publish: Duration,
        logger: Logger,
    ) -> Self {
        Self {
            first_publisher,
            second_publisher,
            delay_between_publish,
            logger: logger.new_with_component_name::<Self>(),
        }
    }
}

#[async_trait::async_trait]
impl SignaturePublisher for SignaturePublisherDelayer {
    async fn publish(
        &self,
        signed_entity_type: &SignedEntityType,
        signature: &SingleSignature,
        protocol_message: &ProtocolMessage,
    ) -> StdResult<()> {
        if let Err(e) = self
            .first_publisher
            .publish(signed_entity_type, signature, protocol_message)
            .await
        {
            error!(
                self.logger,
                "Delayer failed to publish first signature";
                "error" => ?e
            );
        }

        tokio::time::sleep(self.delay_between_publish).await;

        if let Err(e) = self
            .second_publisher
            .publish(signed_entity_type, signature, protocol_message)
            .await
        {
            error!(
                self.logger,
                "Delayer failed to publish second signature";
                "error" => ?e
            );
            return Err(e);
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::{entities::Epoch, test_utils::fake_data};

    use crate::{services::MockSignaturePublisher, test_tools::TestLogger};

    use super::*;

    #[tokio::test]
    async fn should_call_both_publishers_when_first_succeeds() {
        let mut first_publisher = MockSignaturePublisher::new();
        first_publisher.expect_publish().once().returning(|_, _, _| Ok(()));

        let mut second_publisher = MockSignaturePublisher::new();
        second_publisher.expect_publish().once().returning(|_, _, _| Ok(()));

        let delayer = SignaturePublisherDelayer::new(
            Arc::new(first_publisher),
            Arc::new(second_publisher),
            Duration::from_millis(0),
            TestLogger::stdout(),
        );

        delayer
            .publish(
                &SignedEntityType::MithrilStakeDistribution(Epoch(1)),
                &fake_data::single_signature(vec![1]),
                &ProtocolMessage::default(),
            )
            .await
            .unwrap();
    }

    #[tokio::test]
    async fn should_call_second_publisher_even_if_first_fails_and_log_error() {
        let (logger, log_inspector) = TestLogger::memory();
        let mut first_publisher = MockSignaturePublisher::new();
        first_publisher
            .expect_publish()
            .once()
            .returning(|_, _, _| Err(anyhow::anyhow!("first publisher failure")));

        let mut second_publisher = MockSignaturePublisher::new();
        second_publisher.expect_publish().once().returning(|_, _, _| Ok(()));

        let delayer = SignaturePublisherDelayer::new(
            Arc::new(first_publisher),
            Arc::new(second_publisher),
            Duration::from_millis(0),
            logger,
        );

        delayer
            .publish(
                &SignedEntityType::MithrilStakeDistribution(Epoch(1)),
                &fake_data::single_signature(vec![1]),
                &ProtocolMessage::default(),
            )
            .await
            .unwrap();

        assert!(log_inspector.contains_log("Delayer failed to publish first signature"));
        assert!(log_inspector.contains_log("first publisher failure"));
    }

    #[tokio::test]
    async fn should_return_and_log_error_if_second_publisher_fails() {
        let (logger, log_inspector) = TestLogger::memory();
        let mut first_publisher = MockSignaturePublisher::new();
        first_publisher.expect_publish().once().returning(|_, _, _| Ok(()));

        let mut second_publisher = MockSignaturePublisher::new();
        second_publisher
            .expect_publish()
            .once()
            .returning(|_, _, _| Err(anyhow::anyhow!("second publisher failure")));

        let delayer = SignaturePublisherDelayer::new(
            Arc::new(first_publisher),
            Arc::new(second_publisher),
            Duration::from_millis(0),
            logger,
        );

        delayer
            .publish(
                &SignedEntityType::MithrilStakeDistribution(Epoch(1)),
                &fake_data::single_signature(vec![1]),
                &ProtocolMessage::default(),
            )
            .await
            .expect_err("Expected error when delayed publisher failed");

        assert!(log_inspector.contains_log("Delayer failed to publish second signature"));
        assert!(log_inspector.contains_log("second publisher failure"));
    }

    #[tokio::test]
    async fn should_wait_before_calling_second_publisher() {
        let mut first_publisher = MockSignaturePublisher::new();
        first_publisher.expect_publish().once().returning(|_, _, _| Ok(()));

        let mut second_publisher = MockSignaturePublisher::new();
        second_publisher.expect_publish().once().returning(|_, _, _| Ok(()));

        let delay = Duration::from_millis(50);
        let delayer = SignaturePublisherDelayer::new(
            Arc::new(first_publisher),
            Arc::new(second_publisher),
            delay,
            TestLogger::stdout(),
        );

        let start_time = std::time::Instant::now();
        delayer
            .publish(
                &SignedEntityType::MithrilStakeDistribution(Epoch(1)),
                &fake_data::single_signature(vec![1]),
                &ProtocolMessage::default(),
            )
            .await
            .unwrap();

        let elapsed_time = start_time.elapsed();
        assert!(
            elapsed_time >= delay,
            "Expected at least {delay:?} time elapsed, but got {elapsed_time:?}"
        );
        assert!(
            elapsed_time < delay * 2,
            "Expected less than {:?} time elapsed, but got {:?}",
            delay * 2,
            elapsed_time
        );
    }
}
