use std::{sync::Arc, time::Duration};

use anyhow::anyhow;
use slog::{Logger, error, trace, warn};

use mithril_common::{
    StdResult,
    entities::{SingleSignature, SingleSignatureAuthenticationStatus},
    logging::LoggerExtensions,
};
use tokio::{select, sync::watch::Receiver};

use crate::MetricsService;

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
    metrics_service: Arc<MetricsService>,
    wait_delay_on_error: Duration,
    logger: Logger,
}

impl SequentialSignatureProcessor {
    /// Delay to wait between two signature processing attempts after an error
    pub const DEFAULT_WAIT_DELAY_ON_ERROR: Duration = Duration::from_secs(10);

    /// Creates a new `SignatureProcessor` instance.
    pub fn new(
        consumer: Arc<dyn SignatureConsumer>,
        certifier: Arc<dyn CertifierService>,
        stop_rx: Receiver<()>,
        metrics_service: Arc<MetricsService>,
        wait_delay_on_error_in_seconds: Duration,
        logger: Logger,
    ) -> Self {
        Self {
            consumer,
            certifier,
            stop_rx,
            metrics_service,
            wait_delay_on_error: wait_delay_on_error_in_seconds,
            logger: logger.new_with_component_name::<Self>(),
        }
    }

    /// Authenticates a single signature
    ///
    /// This is always the case with single signatures received from the DMQ network.
    fn authenticate_signature(&self, signature: &mut SingleSignature) {
        signature.authentication_status = SingleSignatureAuthenticationStatus::Authenticated;
    }
}

#[async_trait::async_trait]
impl SignatureProcessor for SequentialSignatureProcessor {
    async fn process_signatures(&self) -> StdResult<()> {
        let origin_network = self.consumer.get_origin_tag();

        let mut total_import_errors = 0;
        match self.consumer.get_signatures().await {
            Ok(signatures) => {
                let number_of_signatures = signatures.len() as u32;
                trace!(self.logger, "Received {} signatures", number_of_signatures);

                self.metrics_service
                    .get_signature_registration_total_received_since_startup()
                    .increment_by(&[&origin_network], number_of_signatures);

                for (mut signature, signed_entity_type) in signatures {
                    self.authenticate_signature(&mut signature);
                    match self
                        .certifier
                        .register_single_signature(&signed_entity_type, &signature)
                        .await
                    {
                        Ok(_registration_status) => {
                            self.metrics_service
                                .get_signature_registration_total_successful_since_startup()
                                .increment(&[&origin_network]);
                        }
                        Err(e) => {
                            total_import_errors += 1;
                            error!(
                                self.logger, "Error dispatching single signature";
                                "full_payload" => #?signature, "error" => ?e
                            );
                        }
                    }
                }
            }
            Err(e) => {
                error!(self.logger, "Error consuming single signatures"; "error" => ?e);
                total_import_errors += 1;
            }
        }

        if total_import_errors > 0 {
            error!(
                self.logger,
                "Total import errors while processing signatures: {}", total_import_errors
            );
            return Err(anyhow!(
                "Total import errors while processing signatures: {}",
                total_import_errors
            ));
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
                res = self.process_signatures() => {
                    if let Err(e) = res {
                        error!(self.logger, "Error processing signatures"; "error" => ?e);
                        error!(self.logger, "Sleep for {} seconds", self.wait_delay_on_error.as_secs());
                        tokio::time::sleep(self.wait_delay_on_error).await;
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use anyhow::anyhow;
    use mockall::predicate::eq;
    use tokio::{
        sync::watch::channel,
        time::{Duration, sleep},
    };

    use mithril_common::{
        entities::{Epoch, SignedEntityType},
        test::{double::fake_data, mock_extensions::MockBuilder},
    };

    use crate::services::{
        FakeSignatureConsumer, MockCertifierService, MockSignatureConsumer,
        SignatureRegistrationStatus,
    };
    use crate::test::TestLogger;

    use super::*;

    #[tokio::test]
    async fn processor_process_signatures_succeeds() {
        let logger = TestLogger::stdout();
        let single_signatures = vec![
            (
                fake_data::single_signature(vec![1, 2, 3]),
                SignedEntityType::MithrilStakeDistribution(Epoch(1)),
            ),
            (
                fake_data::single_signature(vec![4, 5, 6]),
                SignedEntityType::MithrilStakeDistribution(Epoch(2)),
            ),
        ];
        let mock_consumer = MockBuilder::<MockSignatureConsumer>::configure(|mock| {
            mock.expect_get_signatures()
                .returning(move || Ok(single_signatures.clone()))
                .times(1);
            mock.expect_get_origin_tag()
                .returning(|| "whatever".to_string())
                .times(1);
        });
        let mock_certifier = MockBuilder::<MockCertifierService>::configure(|mock| {
            mock.expect_register_single_signature()
                .with(
                    eq(SignedEntityType::MithrilStakeDistribution(Epoch(1))),
                    eq(SingleSignature {
                        authentication_status: SingleSignatureAuthenticationStatus::Authenticated,
                        ..fake_data::single_signature(vec![1, 2, 3])
                    }),
                )
                .returning(|_, single_signature| {
                    assert_eq!(
                        single_signature.authentication_status,
                        SingleSignatureAuthenticationStatus::Authenticated
                    );
                    Ok(SignatureRegistrationStatus::Registered)
                })
                .times(1);
            mock.expect_register_single_signature()
                .with(
                    eq(SignedEntityType::MithrilStakeDistribution(Epoch(2))),
                    eq(SingleSignature {
                        authentication_status: SingleSignatureAuthenticationStatus::Authenticated,
                        ..fake_data::single_signature(vec![4, 5, 6])
                    }),
                )
                .returning(|_, _| Ok(SignatureRegistrationStatus::Registered))
                .times(1);
        });
        let (_stop_tx, stop_rx) = channel(());
        let processor = SequentialSignatureProcessor::new(
            mock_consumer,
            mock_certifier,
            stop_rx,
            Arc::new(MetricsService::new(TestLogger::stdout()).unwrap()),
            Duration::from_millis(10),
            logger,
        );

        processor
            .process_signatures()
            .await
            .expect("Failed to process signatures");
    }

    #[tokio::test]
    async fn processor_process_signatures_send_total_received_and_successful_statistics_if_successful()
     {
        let logger = TestLogger::stdout();
        let fake_consumer = FakeSignatureConsumer::new(vec![Ok(vec![(
            fake_data::single_signature(vec![1, 2, 3]),
            SignedEntityType::MithrilStakeDistribution(Epoch(1)),
        )])]);
        let network_origin = fake_consumer.get_origin_tag();

        let mock_certifier = MockBuilder::<MockCertifierService>::configure(|mock| {
            mock.expect_register_single_signature()
                .returning(|_, _| Ok(SignatureRegistrationStatus::Registered));
        });
        let (_stop_tx, stop_rx) = channel(());
        let metrics_service = Arc::new(MetricsService::new(logger.clone()).unwrap());
        let processor = SequentialSignatureProcessor::new(
            Arc::new(fake_consumer),
            mock_certifier,
            stop_rx,
            metrics_service.clone(),
            Duration::from_millis(10),
            logger,
        );

        let initial_received_counter_value = metrics_service
            .get_signature_registration_total_received_since_startup()
            .get(&[&network_origin]);
        let initial_successful_counter_value = metrics_service
            .get_signature_registration_total_successful_since_startup()
            .get(&[&network_origin]);

        processor.process_signatures().await.unwrap();

        assert_eq!(
            initial_received_counter_value + 1,
            metrics_service
                .get_signature_registration_total_received_since_startup()
                .get(&[&network_origin])
        );
        assert_eq!(
            initial_successful_counter_value + 1,
            metrics_service
                .get_signature_registration_total_successful_since_startup()
                .get(&[&network_origin])
        );
    }

    #[tokio::test]
    async fn processor_process_signatures_send_only_total_received_statistic_if_failure() {
        let logger = TestLogger::stdout();
        let fake_consumer = FakeSignatureConsumer::new(vec![Ok(vec![(
            fake_data::single_signature(vec![1, 2, 3]),
            SignedEntityType::MithrilStakeDistribution(Epoch(1)),
        )])]);
        let network_origin = fake_consumer.get_origin_tag();

        let mock_certifier = MockBuilder::<MockCertifierService>::configure(|mock| {
            mock.expect_register_single_signature()
                .returning(|_, _| Err(anyhow!("Error registering signature")));
        });
        let (_stop_tx, stop_rx) = channel(());
        let metrics_service = Arc::new(MetricsService::new(logger.clone()).unwrap());
        let processor = SequentialSignatureProcessor::new(
            Arc::new(fake_consumer),
            mock_certifier,
            stop_rx,
            metrics_service.clone(),
            Duration::from_millis(10),
            logger,
        );

        let initial_received_counter_value = metrics_service
            .get_signature_registration_total_received_since_startup()
            .get(&[&network_origin]);
        let initial_successful_counter_value = metrics_service
            .get_signature_registration_total_successful_since_startup()
            .get(&[&network_origin]);

        processor.process_signatures().await.expect_err("Should have failed");

        assert_eq!(
            initial_received_counter_value + 1,
            metrics_service
                .get_signature_registration_total_received_since_startup()
                .get(&[&network_origin])
        );
        assert_eq!(
            initial_successful_counter_value,
            metrics_service
                .get_signature_registration_total_successful_since_startup()
                .get(&[&network_origin])
        );
    }

    #[tokio::test]
    async fn processor_run_succeeds_even_if_processing_signatures_fails() {
        let logger = TestLogger::stdout();
        let fake_consumer = FakeSignatureConsumer::new(vec![
            Err(anyhow!("Error consuming signatures")),
            Ok(vec![(
                fake_data::single_signature(vec![1, 2, 3]),
                SignedEntityType::MithrilStakeDistribution(Epoch(1)),
            )]),
        ]);
        let mock_certifier = MockBuilder::<MockCertifierService>::configure(|mock| {
            mock.expect_register_single_signature()
                .with(
                    eq(SignedEntityType::MithrilStakeDistribution(Epoch(1))),
                    eq(SingleSignature {
                        authentication_status: SingleSignatureAuthenticationStatus::Authenticated,
                        ..fake_data::single_signature(vec![1, 2, 3])
                    }),
                )
                .returning(|_, _| Ok(SignatureRegistrationStatus::Registered))
                .times(1);
        });
        let (stop_tx, stop_rx) = channel(());
        let metrics_service = MetricsService::new(logger.clone()).unwrap();
        let processor = SequentialSignatureProcessor::new(
            Arc::new(fake_consumer),
            mock_certifier,
            stop_rx,
            Arc::new(metrics_service),
            Duration::from_millis(1),
            logger,
        );

        select!(
            _res =  processor.run() => {},
            _res = sleep(Duration::from_millis(500)) => {
                println!("Stopping signature processor...");
                stop_tx.send(()).unwrap();
            },
        );
    }

    #[tokio::test]
    async fn processor_run_waits_before_resuming_if_processing_signatures_fails() {
        let logger = TestLogger::stdout();
        let fake_consumer = FakeSignatureConsumer::new(vec![
            Err(anyhow!("Error consuming signatures")),
            Ok(vec![(
                fake_data::single_signature(vec![1, 2, 3]),
                SignedEntityType::MithrilStakeDistribution(Epoch(1)),
            )]),
        ]);
        let mock_certifier = MockBuilder::<MockCertifierService>::configure(|mock| {
            mock.expect_register_single_signature().never();
        });
        let (stop_tx, stop_rx) = channel(());
        let metrics_service = MetricsService::new(logger.clone()).unwrap();
        let processor = SequentialSignatureProcessor::new(
            Arc::new(fake_consumer),
            mock_certifier,
            stop_rx,
            Arc::new(metrics_service),
            Duration::from_millis(1000),
            logger,
        );

        select!(
            _res =  processor.run() => {},
            _res = sleep(Duration::from_millis(500)) => {
                println!("Stopping signature processor...");
                stop_tx.send(()).unwrap();
            },
        );
    }
}
