use std::{sync::Arc, time::Duration};

use mithril_common::{
    StdResult,
    entities::{ProtocolMessage, SignedEntityType, SingleSignature},
};

use super::SignaturePublisher;

/// Policy for retrying signature publishing
#[derive(Debug, PartialEq, Clone)]
pub struct SignaturePublishRetryPolicy {
    /// Number of attempts to publish a signature
    pub attempts: u8,
    /// Delay between two attempts
    pub delay_between_attempts: Duration,
}

impl SignaturePublishRetryPolicy {
    /// Create a policy that never retries
    pub fn never() -> Self {
        Self {
            attempts: 1,
            delay_between_attempts: Duration::from_secs(0),
        }
    }
}

impl Default for SignaturePublishRetryPolicy {
    /// Create a default retry policy
    fn default() -> Self {
        Self {
            attempts: 3,
            delay_between_attempts: Duration::from_secs(5),
        }
    }
}

/// A decorator of [SignaturePublisher] that retries the publishing of signatures in case of failure
pub struct SignaturePublisherRetrier {
    publisher: Arc<dyn SignaturePublisher>,
    retry_policy: SignaturePublishRetryPolicy,
}

impl SignaturePublisherRetrier {
    /// Creates a new [SignaturePublisherRetrier]
    pub fn new(
        publisher: Arc<dyn SignaturePublisher>,
        retry_policy: SignaturePublishRetryPolicy,
    ) -> Self {
        Self {
            publisher,
            retry_policy,
        }
    }
}

#[async_trait::async_trait]
impl SignaturePublisher for SignaturePublisherRetrier {
    async fn publish(
        &self,
        signed_entity_type: &SignedEntityType,
        signature: &SingleSignature,
        protocol_message: &ProtocolMessage,
    ) -> StdResult<()> {
        let mut nb_attempts = 0;
        loop {
            nb_attempts += 1;

            match self
                .publisher
                .publish(signed_entity_type, signature, protocol_message)
                .await
            {
                Ok(_) => return Ok(()),
                Err(e) if nb_attempts >= self.retry_policy.attempts => {
                    return Err(anyhow::anyhow!(e)
                        .context(format!("Publish failed after {nb_attempts} attempts")));
                }
                _ => tokio::time::sleep(self.retry_policy.delay_between_attempts).await,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::{entities::Epoch, test::double::fake_data};

    use super::*;
    use crate::services::MockSignaturePublisher;

    #[tokio::test]
    async fn should_call_publish_once_when_no_retry_policy() {
        let retry_policy = SignaturePublishRetryPolicy::never();

        let mut publisher = MockSignaturePublisher::new();
        publisher.expect_publish().once().returning(|_, _, _| Ok(()));

        let retrier = SignaturePublisherRetrier::new(Arc::new(publisher), retry_policy);

        retrier
            .publish(
                &SignedEntityType::MithrilStakeDistribution(Epoch(1)),
                &fake_data::single_signature(vec![1]),
                &ProtocolMessage::default(),
            )
            .await
            .unwrap();
    }

    #[tokio::test]
    async fn should_not_retry_when_publish_fails_and_retry_policy_is_never() {
        let retry_policy = SignaturePublishRetryPolicy::never();

        let mut publisher = MockSignaturePublisher::new();
        publisher
            .expect_publish()
            .once()
            .returning(|_, _, _| Err(anyhow::anyhow!("error while publishing")));

        let retrier = SignaturePublisherRetrier::new(Arc::new(publisher), retry_policy);

        retrier
            .publish(
                &SignedEntityType::MithrilStakeDistribution(Epoch(1)),
                &fake_data::single_signature(vec![1]),
                &ProtocolMessage::default(),
            )
            .await
            .expect_err("An error should be returned");
    }

    #[tokio::test]
    async fn should_retry_once_and_succeed_on_second_attempt() {
        let retry_policy = SignaturePublishRetryPolicy {
            attempts: 2,
            delay_between_attempts: Duration::from_secs(0),
        };

        let mut publisher = MockSignaturePublisher::new();
        publisher
            .expect_publish()
            .times(1)
            .return_once(|_, _, _| Err(anyhow::anyhow!("error")));
        publisher.expect_publish().times(1).return_once(|_, _, _| Ok(()));

        let retrier = SignaturePublisherRetrier::new(Arc::new(publisher), retry_policy);

        retrier
            .publish(
                &SignedEntityType::MithrilStakeDistribution(Epoch(1)),
                &fake_data::single_signature(vec![1]),
                &ProtocolMessage::default(),
            )
            .await
            .unwrap();
    }

    #[tokio::test]
    async fn should_retry_and_return_error_after_max_attempts() {
        let retry_policy = SignaturePublishRetryPolicy {
            attempts: 2,
            delay_between_attempts: Duration::from_secs(0),
        };

        let mut publisher = MockSignaturePublisher::new();
        publisher
            .expect_publish()
            .times(2)
            .returning(|_, _, _| Err(anyhow::anyhow!("error")));

        let retrier = SignaturePublisherRetrier::new(Arc::new(publisher), retry_policy);

        retrier
            .publish(
                &SignedEntityType::MithrilStakeDistribution(Epoch(1)),
                &fake_data::single_signature(vec![1]),
                &ProtocolMessage::default(),
            )
            .await
            .expect_err("An error should be returned after max attempts");
    }

    #[tokio::test]
    async fn should_wait_between_retries_according_to_policy() {
        let delay_between_attempts = Duration::from_millis(50);
        let retry_policy = SignaturePublishRetryPolicy {
            attempts: 2,
            delay_between_attempts,
        };

        let mut publisher = MockSignaturePublisher::new();
        publisher
            .expect_publish()
            .once()
            .return_once(|_, _, _| Err(anyhow::anyhow!("error")));
        publisher.expect_publish().once().return_once(|_, _, _| Ok(()));

        let retrier = SignaturePublisherRetrier::new(Arc::new(publisher), retry_policy);

        let start_time = std::time::Instant::now();
        retrier
            .publish(
                &SignedEntityType::MithrilStakeDistribution(Epoch(1)),
                &fake_data::single_signature(vec![1]),
                &ProtocolMessage::default(),
            )
            .await
            .unwrap();

        let elapsed_time = start_time.elapsed();
        assert!(
            elapsed_time >= delay_between_attempts,
            "Expected at least {delay_between_attempts:?} time elapsed, but got {elapsed_time:?}"
        );
        assert!(
            elapsed_time < delay_between_attempts * 2,
            "Expected less than {:?} time elapsed, but got {:?}",
            delay_between_attempts * 2,
            elapsed_time
        );
    }
}
