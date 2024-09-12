use async_trait::async_trait;
use slog::{debug, trace, warn, Logger};
use std::sync::Arc;

use mithril_common::entities::{
    Certificate, Epoch, ProtocolMessage, SignedEntityType, SignedEntityTypeDiscriminants,
    SingleSignatures,
};
use mithril_common::StdResult;

use crate::entities::OpenMessage;
use crate::services::{
    BufferedSingleSignatureStore, CertifierService, CertifierServiceError, RegistrationStatus,
};
use crate::MultiSigner;

/// A decorator of [CertifierService] that buffers that can buffer registration of single signatures
/// when the open message is not yet created.
///
/// When an open message is created, buffered single signatures for the open message type are
/// registered.
pub struct BufferedCertifierService {
    certifier_service: Arc<dyn CertifierService>,
    multi_signer: Arc<dyn MultiSigner>,
    buffered_single_signature_store: Arc<dyn BufferedSingleSignatureStore>,
    logger: Logger,
}

impl BufferedCertifierService {
    /// Create a new instance of `BufferedCertifierService`.
    pub fn new(
        certifier_service: Arc<dyn CertifierService>,
        multi_signer: Arc<dyn MultiSigner>,
        buffered_single_signature_store: Arc<dyn BufferedSingleSignatureStore>,
        logger: Logger,
    ) -> Self {
        Self {
            certifier_service,
            multi_signer,
            buffered_single_signature_store,
            logger,
        }
    }

    async fn try_register_buffered_signatures_to_current_open_message(
        &self,
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<()> {
        let discriminant: SignedEntityTypeDiscriminants = signed_entity_type.into();
        let buffered_signatures = self
            .buffered_single_signature_store
            .get_buffered_signatures(discriminant)
            .await?;
        let mut signatures_to_remove = vec![];

        for signature in buffered_signatures {
            match self
                .certifier_service
                .register_single_signature(signed_entity_type, &signature)
                .await
            {
                Ok(..) => {
                    signatures_to_remove.push(signature);
                }
                Err(error) => match error.downcast_ref::<CertifierServiceError>() {
                    Some(CertifierServiceError::InvalidSingleSignature(..)) => {
                        trace!(self.logger, "Skipping invalid signature for signed entity '{signed_entity_type:?}'";
                            "party_id" => &signature.party_id,
                            "error" => ?error,
                        );
                    }
                    _ => {
                        anyhow::bail!(error);
                    }
                },
            }
        }

        self.buffered_single_signature_store
            .remove_buffered_signatures(discriminant, signatures_to_remove)
            .await?;

        Ok(())
    }
}

#[async_trait]
impl CertifierService for BufferedCertifierService {
    async fn inform_epoch(&self, epoch: Epoch) -> StdResult<()> {
        self.certifier_service.inform_epoch(epoch).await
    }

    async fn register_single_signature(
        &self,
        signed_entity_type: &SignedEntityType,
        signature: &SingleSignatures,
    ) -> StdResult<RegistrationStatus> {
        match self
            .certifier_service
            .register_single_signature(signed_entity_type, signature)
            .await
        {
            Ok(res) => Ok(res),
            Err(error) => match error.downcast_ref::<CertifierServiceError>() {
                Some(CertifierServiceError::NotFound(..)) => {
                    match &signature.signed_message {
                        // Only buffer signatures that have a signed message so we can validate them
                        Some(signed_message) => {
                            debug!(
                                self.logger,
                                "No OpenMessage available for signed entity - Buffering single signature";
                                "signed_entity_type" => ?signed_entity_type,
                                "party_id" => &signature.party_id
                            );

                            if let Err(error) = self
                                .multi_signer
                                .verify_single_signature(signed_message, signature)
                                .await
                            {
                                // Signers may detect epoch changes before the aggregator and send
                                // new signatures using the next epoch stake distribution
                                debug!(
                                    self.logger,
                                    "Signature is invalid for current epoch stake distribution, trying next epoch";
                                    "validation_error" => ?error
                                );

                                self.multi_signer
                                    .verify_single_signature_for_next_epoch(
                                        signed_message,
                                        signature,
                                    )
                                    .await
                                    .map_err(|err| {
                                        CertifierServiceError::InvalidSingleSignature(
                                            signed_entity_type.clone(),
                                            err,
                                        )
                                    })?;
                            }

                            self.buffered_single_signature_store
                                .buffer_signature(signed_entity_type.into(), signature)
                                .await?;

                            Ok(RegistrationStatus::Buffered)
                        }
                        None => Err(error),
                    }
                }
                _ => Err(error),
            },
        }
    }

    async fn create_open_message(
        &self,
        signed_entity_type: &SignedEntityType,
        protocol_message: &ProtocolMessage,
    ) -> StdResult<OpenMessage> {
        // IMPORTANT: this method should not fail if the open message creation succeeds.
        // else:
        // 1 - state machine won't create a pending certificate for the signed entity type
        // 2 - Without a pending certificate, the signers won't send their signatures
        // 3 - state machine will retry the transition to signing and, since an open message was
        // opened for the signed entity type, it will try the next on the list.
        // 4 - since the state machine never was in signing it will never try to aggregate
        // signatures for the signed entity type

        let creation_result = self
            .certifier_service
            .create_open_message(signed_entity_type, protocol_message)
            .await;

        if creation_result.is_ok() {
            if let Err(error) = self
                .try_register_buffered_signatures_to_current_open_message(signed_entity_type)
                .await
            {
                warn!(self.logger, "Failed to register buffered signatures to the new open message";
                    "signed_entity_type" => ?signed_entity_type,
                    "error" => ?error
                );
            }
        }

        creation_result
    }

    async fn get_open_message(
        &self,
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<Option<OpenMessage>> {
        self.certifier_service
            .get_open_message(signed_entity_type)
            .await
    }

    async fn mark_open_message_if_expired(
        &self,
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<Option<OpenMessage>> {
        self.certifier_service
            .mark_open_message_if_expired(signed_entity_type)
            .await
    }

    async fn create_certificate(
        &self,
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<Option<Certificate>> {
        self.certifier_service
            .create_certificate(signed_entity_type)
            .await
    }

    async fn get_certificate_by_hash(&self, hash: &str) -> StdResult<Option<Certificate>> {
        self.certifier_service.get_certificate_by_hash(hash).await
    }

    async fn get_latest_certificates(&self, last_n: usize) -> StdResult<Vec<Certificate>> {
        self.certifier_service.get_latest_certificates(last_n).await
    }

    async fn verify_certificate_chain(&self, epoch: Epoch) -> StdResult<()> {
        self.certifier_service.verify_certificate_chain(epoch).await
    }
}

#[cfg(test)]
mod tests {
    use anyhow::anyhow;
    use mockall::predicate::eq;
    use std::collections::BTreeMap;

    use mithril_common::test_utils::fake_data;

    use crate::multi_signer::MockMultiSigner;
    use crate::services::{
        CertifierServiceError, MockBufferedSingleSignatureStore, MockCertifierService,
    };
    use crate::test_tools::TestLogger;
    use crate::InMemoryBufferedSingleSignatureStore;

    use super::*;

    fn mock_certifier(
        certifier_mock_config: impl FnOnce(&mut MockCertifierService),
    ) -> Arc<MockCertifierService> {
        let mut certifier = MockCertifierService::new();
        certifier_mock_config(&mut certifier);
        Arc::new(certifier)
    }

    fn mock_multi_signer(
        multi_signer_mock_config: impl FnOnce(&mut MockMultiSigner),
    ) -> Arc<MockMultiSigner> {
        let mut multi_signer = MockMultiSigner::new();
        multi_signer_mock_config(&mut multi_signer);
        Arc::new(multi_signer)
    }

    fn mock_store<F>(store_mock_config: F) -> Arc<MockBufferedSingleSignatureStore>
    where
        F: FnOnce(&mut MockBufferedSingleSignatureStore),
    {
        let mut store = MockBufferedSingleSignatureStore::new();
        store_mock_config(&mut store);
        Arc::new(store)
    }

    /// Run a scenario where we try to register a signature (using a fixed signed entity type).
    ///
    /// Return the registration result and the list of buffered signatures after the registration.
    async fn run_register_signature_scenario(
        decorated_certifier_mock_config: impl FnOnce(&mut MockCertifierService),
        multi_signer_mock_config: impl FnOnce(&mut MockMultiSigner),
        signature_to_register: &SingleSignatures,
    ) -> (StdResult<RegistrationStatus>, Vec<SingleSignatures>) {
        let store = Arc::new(InMemoryBufferedSingleSignatureStore::default());
        let certifier = BufferedCertifierService::new(
            mock_certifier(decorated_certifier_mock_config),
            mock_multi_signer(multi_signer_mock_config),
            store.clone(),
            TestLogger::stdout(),
        );

        let registration_result = certifier
            .register_single_signature(
                &SignedEntityType::MithrilStakeDistribution(Epoch(5)),
                signature_to_register,
            )
            .await;

        let buffered_signatures = store
            .get_buffered_signatures(SignedEntityTypeDiscriminants::MithrilStakeDistribution)
            .await
            .unwrap();

        (registration_result, buffered_signatures)
    }

    #[tokio::test]
    async fn when_registering_single_signature_dont_buffer_signature_if_decorated_certifier_succeed(
    ) {
        let signature = SingleSignatures::fake_with_signed_message("party_1", "a message");
        let (registration_result, buffered_signatures_after_registration) =
            run_register_signature_scenario(
                |mock_certifier| {
                    mock_certifier
                        .expect_register_single_signature()
                        .returning(|_, _| Ok(RegistrationStatus::Registered));
                },
                |_mock_multi_signer| {},
                &signature,
            )
            .await;

        let status = registration_result.expect("Registration should have succeed");
        assert_eq!(status, RegistrationStatus::Registered);
        assert_eq!(
            buffered_signatures_after_registration,
            Vec::<SingleSignatures>::new()
        );
    }

    mod when_registering_single_signature_if_decorated_certifier_as_no_opened_message {
        use super::*;

        #[tokio::test]
        async fn buffer_signature_with_valid_signed_message_for_current_epoch() {
            let signature = SingleSignatures::fake_with_signed_message("party_1", "a message");
            let (registration_result, buffered_signatures_after_registration) =
                run_register_signature_scenario(
                    |mock_certifier| {
                        mock_certifier
                            .expect_register_single_signature()
                            .returning(|_, _| {
                                Err(CertifierServiceError::NotFound(
                                    SignedEntityType::MithrilStakeDistribution(Epoch(5)),
                                )
                                .into())
                            });
                    },
                    |mock_multi_signer| {
                        mock_multi_signer
                            .expect_verify_single_signature()
                            .returning(|_, _| Ok(()));
                    },
                    &signature,
                )
                .await;

            let status = registration_result.expect("Registration should have succeed");
            assert_eq!(status, RegistrationStatus::Buffered);
            assert_eq!(buffered_signatures_after_registration, vec![signature]);
        }

        #[tokio::test]
        async fn buffer_signature_with_valid_signed_message_for_next_epoch() {
            let signature = SingleSignatures::fake_with_signed_message("party_1", "a message");
            let (registration_result, buffered_signatures_after_registration) =
                run_register_signature_scenario(
                    |mock_certifier| {
                        mock_certifier
                            .expect_register_single_signature()
                            .returning(|_, _| {
                                Err(CertifierServiceError::NotFound(
                                    SignedEntityType::MithrilStakeDistribution(Epoch(5)),
                                )
                                .into())
                            });
                    },
                    |mock_multi_signer| {
                        mock_multi_signer
                            .expect_verify_single_signature()
                            .returning(|_, _| Err(anyhow!("Invalid signature")));
                        mock_multi_signer
                            .expect_verify_single_signature_for_next_epoch()
                            .returning(|_, _| Ok(()));
                    },
                    &signature,
                )
                .await;

            let status = registration_result.expect("Registration should have succeed");
            assert_eq!(status, RegistrationStatus::Buffered);
            assert_eq!(buffered_signatures_after_registration, vec![signature]);
        }

        #[tokio::test]
        async fn dont_buffer_signature_with_invalid_signed_message() {
            let signature = SingleSignatures::fake_with_signed_message("party_1", "a message");
            let (registration_result, buffered_signatures_after_registration) =
                run_register_signature_scenario(
                    |mock_certifier| {
                        mock_certifier
                            .expect_register_single_signature()
                            .returning(|_, _| {
                                Err(CertifierServiceError::NotFound(
                                    SignedEntityType::MithrilStakeDistribution(Epoch(5)),
                                )
                                .into())
                            });
                    },
                    |mock_multi_signer| {
                        mock_multi_signer
                            .expect_verify_single_signature()
                            .returning(|_, _| Err(anyhow!("Invalid signature for current epoch")));
                        mock_multi_signer
                            .expect_verify_single_signature_for_next_epoch()
                            .returning(|_, _| Err(anyhow!("Invalid signature for next epoch")));
                    },
                    &signature,
                )
                .await;

            registration_result.expect_err("Registration should have failed");
            assert_eq!(
                buffered_signatures_after_registration,
                Vec::<SingleSignatures>::new()
            );
        }

        #[tokio::test]
        async fn dont_buffer_signature_without_signed_message() {
            let signature = SingleSignatures {
                signed_message: None,
                ..SingleSignatures::fake_with_signed_message("party_1", "a message")
            };
            let (registration_result, buffered_signatures_after_registration) =
                run_register_signature_scenario(
                    |mock_certifier| {
                        mock_certifier
                            .expect_register_single_signature()
                            .returning(|_, _| {
                                Err(CertifierServiceError::NotFound(
                                    SignedEntityType::MithrilStakeDistribution(Epoch(5)),
                                )
                                .into())
                            });
                    },
                    |_mock_multi_signer| {},
                    &signature,
                )
                .await;

            registration_result.expect_err("Registration should have failed");
            assert_eq!(
                buffered_signatures_after_registration,
                Vec::<SingleSignatures>::new()
            );
        }
    }

    #[tokio::test]
    async fn buffered_signatures_are_moved_to_newly_opened_message() {
        let store = Arc::new(InMemoryBufferedSingleSignatureStore::with_data(
            BTreeMap::from([
                (
                    SignedEntityTypeDiscriminants::MithrilStakeDistribution,
                    vec![
                        fake_data::single_signatures(vec![1]),
                        fake_data::single_signatures(vec![2]),
                    ],
                ),
                (
                    SignedEntityTypeDiscriminants::CardanoTransactions,
                    vec![fake_data::single_signatures(vec![10])],
                ),
            ]),
        ));
        let certifier = BufferedCertifierService::new(
            mock_certifier(|mock| {
                mock.expect_create_open_message()
                    .returning(|_, _| Ok(OpenMessage::dummy()));

                // Those configuration Asserts that the buffered signatures are registered
                mock.expect_register_single_signature()
                    .with(
                        eq(SignedEntityType::MithrilStakeDistribution(Epoch(5))),
                        eq(fake_data::single_signatures(vec![1])),
                    )
                    .once()
                    .returning(|_, _| Ok(RegistrationStatus::Registered));
                mock.expect_register_single_signature()
                    .with(
                        eq(SignedEntityType::MithrilStakeDistribution(Epoch(5))),
                        eq(fake_data::single_signatures(vec![2])),
                    )
                    .once()
                    .returning(|_, _| Ok(RegistrationStatus::Registered));
            }),
            mock_multi_signer(|_| {}),
            store.clone(),
            TestLogger::stdout(),
        );

        certifier
            .create_open_message(
                &SignedEntityType::MithrilStakeDistribution(Epoch(5)),
                &ProtocolMessage::new(),
            )
            .await
            .unwrap();

        let remaining_sigs = store
            .get_buffered_signatures(SignedEntityTypeDiscriminants::MithrilStakeDistribution)
            .await
            .unwrap();
        assert!(remaining_sigs.is_empty());
    }

    mod when_failing_to_transfer_buffered_signature_to_new_open_message {
        use mockall::predicate::always;

        use super::*;

        async fn run_scenario(
            certifier_mock_config: impl FnOnce(&mut MockCertifierService),
            store_mock_config: impl FnOnce(&mut MockBufferedSingleSignatureStore),
        ) {
            let store = mock_store(store_mock_config);
            let certifier = BufferedCertifierService::new(
                mock_certifier(certifier_mock_config),
                mock_multi_signer(|_| {}),
                store,
                TestLogger::stdout(),
            );

            certifier
                .create_open_message(
                    &SignedEntityType::MithrilStakeDistribution(Epoch(5)),
                    &ProtocolMessage::new(),
                )
                .await
                .expect("Transferring buffered signatures to new open message should not fail");
        }

        #[tokio::test]
        async fn skip_invalid_signatures() {
            run_scenario(
                |mock| {
                    mock.expect_create_open_message()
                        .returning(|_, _| Ok(OpenMessage::dummy()));

                    mock.expect_register_single_signature()
                        .with(always(), eq(fake_data::single_signatures(vec![1])))
                        .returning(|_, _| Ok(RegistrationStatus::Registered))
                        .once();
                    mock.expect_register_single_signature()
                        .with(always(), eq(fake_data::single_signatures(vec![2])))
                        .returning(|_, _| {
                            Err(CertifierServiceError::InvalidSingleSignature(
                                OpenMessage::dummy().signed_entity_type,
                                anyhow!("Invalid signature"),
                            )
                            .into())
                        })
                        .once();
                    mock.expect_register_single_signature()
                        .with(always(), eq(fake_data::single_signatures(vec![3])))
                        .returning(|_, _| Ok(RegistrationStatus::Registered))
                        .once();
                },
                |mock| {
                    mock.expect_get_buffered_signatures().returning(|_| {
                        Ok(vec![
                            fake_data::single_signatures(vec![1]),
                            fake_data::single_signatures(vec![2]),
                            fake_data::single_signatures(vec![3]),
                        ])
                    });
                    mock.expect_remove_buffered_signatures()
                        // Only non-skipped signatures should be removed
                        .withf(|_, sig_to_remove| sig_to_remove.len() == 2)
                        .returning(|_, _| Ok(()));
                },
            )
            .await;
        }

        #[tokio::test]
        async fn do_not_return_an_error_if_getting_buffer_signatures_fail() {
            run_scenario(
                |mock| {
                    mock.expect_create_open_message()
                        .returning(|_, _| Ok(OpenMessage::dummy()));
                    mock.expect_register_single_signature()
                        .returning(|_, _| Ok(RegistrationStatus::Registered));
                },
                |mock| {
                    mock.expect_get_buffered_signatures()
                        .returning(|_| Err(anyhow!("get_buffered_signatures error")));
                },
            )
            .await;
        }

        #[tokio::test]
        async fn do_not_return_an_error_if_getting_registering_signature_fail() {
            run_scenario(
                |mock| {
                    mock.expect_create_open_message()
                        .returning(|_, _| Ok(OpenMessage::dummy()));
                    mock.expect_register_single_signature()
                        .returning(|_, _| Err(anyhow!("register_single_signature error")));
                },
                |mock| {
                    mock.expect_get_buffered_signatures()
                        .returning(|_| Ok(vec![fake_data::single_signatures(vec![1])]));
                },
            )
            .await;
        }

        #[tokio::test]
        async fn do_not_return_an_error_if_removing_buffered_signatures_fail() {
            run_scenario(
                |mock| {
                    mock.expect_create_open_message()
                        .returning(|_, _| Ok(OpenMessage::dummy()));
                    mock.expect_register_single_signature()
                        .returning(|_, _| Ok(RegistrationStatus::Registered));
                },
                |mock| {
                    mock.expect_get_buffered_signatures()
                        .returning(|_| Ok(vec![fake_data::single_signatures(vec![1])]));
                    mock.expect_remove_buffered_signatures()
                        .returning(|_, _| Err(anyhow!("remove_buffered_signatures error")));
                },
            )
            .await;
        }
    }
}
