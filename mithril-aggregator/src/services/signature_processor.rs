use std::sync::Arc;

use slog::{error, warn, Logger};

use mithril_common::{logging::LoggerExtensions, StdResult};
use tokio::{select, sync::watch::Receiver};

use super::{CertifierService, SignatureConsumer};

/// A signature processor which receives signature and processes them.
#[cfg_attr(test, mockall::automock)]
#[async_trait::async_trait]
pub trait SignatureProcessor: Sync + Send {
    /// Processes the signatures received from the consumer.
    async fn process_signatures(&self) -> StdResult<()>;

    /// Starts the processor, which will run indefinitely, processing signatures as they arrive.
    async fn run(&self) -> StdResult<()>;
}

/// A sequential signature processor receives messages and processes them sequentially
pub struct SequentialSignatureProcessor {
    consumer: Arc<dyn SignatureConsumer>,
    certifier: Arc<dyn CertifierService>,
    stop_rx: Receiver<()>,
    logger: Logger,
}

impl SequentialSignatureProcessor {
    /// Creates a new `SignatureProcessor` instance.
    pub fn new(
        consumer: Arc<dyn SignatureConsumer>,
        certifier: Arc<dyn CertifierService>,
        stop_rx: Receiver<()>,
        logger: Logger,
    ) -> Self {
        Self {
            consumer,
            certifier,
            stop_rx,
            logger: logger.new_with_component_name::<Self>(),
        }
    }
}

#[async_trait::async_trait]
impl SignatureProcessor for SequentialSignatureProcessor {
    async fn process_signatures(&self) -> StdResult<()> {
        match self.consumer.get_signatures().await {
            Ok(signatures) => {
                for (signature, signed_entity_type) in signatures {
                    if let Err(e) = self
                        .certifier
                        .register_single_signature(&signed_entity_type, &signature)
                        .await
                    {
                        error!(self.logger, "Error dispatching single signature"; "error" => ?e);
                    }
                }
            }
            Err(e) => {
                error!(self.logger, "Error consuming single signatures"; "error" => ?e);
            }
        }

        Ok(())
    }

    async fn run(&self) -> StdResult<()> {
        loop {
            let mut stop_rx = self.stop_rx.clone();
            select! {
                _ = stop_rx.changed() => {
                    warn!(self.logger, "Stopping signature processor...");

                    return Ok(());
                }
                _ = self.process_signatures() => {}
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use anyhow::anyhow;
    use mithril_common::{
        entities::{Epoch, SignedEntityType},
        test_utils::fake_data,
    };
    use mockall::predicate::eq;
    use tokio::{
        sync::watch::channel,
        time::{sleep, Duration},
    };

    use crate::{
        services::{
            FakeSignatureConsumer, MockCertifierService, MockSignatureConsumer,
            SignatureRegistrationStatus,
        },
        test_tools::TestLogger,
    };

    use super::*;

    #[tokio::test]
    async fn processor_process_signatures_succeeds() {
        let logger = TestLogger::stdout();
        let mock_consumer = {
            let mut mock_consumer = MockSignatureConsumer::new();
            mock_consumer
                .expect_get_signatures()
                .returning(|| {
                    Ok(vec![
                        (
                            fake_data::single_signature(vec![1, 2, 3]),
                            SignedEntityType::MithrilStakeDistribution(Epoch(1)),
                        ),
                        (
                            fake_data::single_signature(vec![4, 5, 6]),
                            SignedEntityType::MithrilStakeDistribution(Epoch(2)),
                        ),
                    ])
                })
                .times(1);
            mock_consumer
        };
        let mock_certifier = {
            let mut mock_certifier = MockCertifierService::new();
            mock_certifier
                .expect_register_single_signature()
                .with(
                    eq(SignedEntityType::MithrilStakeDistribution(Epoch(1))),
                    eq(fake_data::single_signature(vec![1, 2, 3])),
                )
                .returning(|_, _| Ok(SignatureRegistrationStatus::Registered))
                .times(1);
            mock_certifier
                .expect_register_single_signature()
                .with(
                    eq(SignedEntityType::MithrilStakeDistribution(Epoch(2))),
                    eq(fake_data::single_signature(vec![4, 5, 6])),
                )
                .returning(|_, _| Ok(SignatureRegistrationStatus::Registered))
                .times(1);

            mock_certifier
        };
        let (_stop_tx, stop_rx) = channel(());
        let processor = SequentialSignatureProcessor::new(
            Arc::new(mock_consumer),
            Arc::new(mock_certifier),
            stop_rx,
            logger,
        );

        processor
            .process_signatures()
            .await
            .expect("Failed to process signatures");
    }

    #[tokio::test]
    async fn processor_run_succeeds() {
        let logger = TestLogger::stdout();
        let fake_consumer = FakeSignatureConsumer::new(vec![
            Err(anyhow!("Error consuming signatures")),
            Ok(vec![(
                fake_data::single_signature(vec![1, 2, 3]),
                SignedEntityType::MithrilStakeDistribution(Epoch(1)),
            )]),
        ]);
        let mock_certifier = {
            let mut mock_certifier = MockCertifierService::new();
            mock_certifier
                .expect_register_single_signature()
                .with(
                    eq(SignedEntityType::MithrilStakeDistribution(Epoch(1))),
                    eq(fake_data::single_signature(vec![1, 2, 3])),
                )
                .returning(|_, _| Ok(SignatureRegistrationStatus::Registered))
                .times(1);

            mock_certifier
        };
        let (stop_tx, stop_rx) = channel(());
        let processor = SequentialSignatureProcessor::new(
            Arc::new(fake_consumer),
            Arc::new(mock_certifier),
            stop_rx,
            logger,
        );

        tokio::select!(
            _res =  processor.run() => {},
            _res = sleep(Duration::from_millis(10)) => {
                println!("Stopping signature processor...");
                stop_tx.send(()).unwrap();
            },
        );
    }
}
