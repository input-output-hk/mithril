use slog::{debug, Logger};
use std::sync::Arc;

use mithril_common::entities::{SingleSignatureAuthenticationStatus, SingleSignatures};
use mithril_common::logging::LoggerExtensions;
use mithril_common::StdResult;

use crate::MultiSigner;

/// Authenticates single signatures against a signed message.
pub struct SingleSignatureAuthenticator {
    multi_signer: Arc<dyn MultiSigner>,
    logger: Logger,
}

impl SingleSignatureAuthenticator {
    /// Creates a new `SingleSignatureAuthenticator`.
    pub fn new(multi_signer: Arc<dyn MultiSigner>, logger: Logger) -> Self {
        Self {
            multi_signer,
            logger: logger.new_with_component_name::<Self>(),
        }
    }

    /// Authenticates a single signature against a signed message.
    pub async fn authenticate(
        &self,
        single_signature: &mut SingleSignatures,
        signed_message: &str,
    ) -> StdResult<()> {
        let is_authenticated = match self
            .multi_signer
            .verify_single_signature(signed_message, single_signature)
            .await
        {
            Ok(()) => {
                debug!(
                    self.logger,
                    "Single signature party authenticated for current stake distribution";
                    "party_id" => &single_signature.party_id,
                );
                true
            }
            Err(_error) => {
                // Signers may detect epoch changes before the aggregator and send
                // new signatures using the next epoch stake distribution
                if self
                    .multi_signer
                    .verify_single_signature_for_next_stake_distribution(
                        signed_message,
                        single_signature,
                    )
                    .await
                    .is_ok()
                {
                    debug!(
                        self.logger,
                        "Single signature party authenticated for next stake distribution";
                        "party_id" => &single_signature.party_id,
                    );
                    true
                } else {
                    debug!(
                        self.logger,
                        "Single signature party not authenticated";
                        "party_id" => &single_signature.party_id,
                    );
                    false
                }
            }
        };

        single_signature.authentication_status = if is_authenticated {
            SingleSignatureAuthenticationStatus::Authenticated
        } else {
            SingleSignatureAuthenticationStatus::Unauthenticated
        };

        Ok(())
    }
}

#[cfg(test)]
impl SingleSignatureAuthenticator {
    pub(crate) fn new_that_authenticate_everything() -> Self {
        let mut multi_signer = crate::multi_signer::MockMultiSigner::new();
        multi_signer
            .expect_verify_single_signature()
            .returning(|_, _| Ok(()));
        multi_signer
            .expect_verify_single_signature_for_next_stake_distribution()
            .returning(|_, _| Ok(()));

        Self {
            multi_signer: Arc::new(multi_signer),
            logger: crate::test_tools::TestLogger::stdout(),
        }
    }

    pub(crate) fn new_that_reject_everything() -> Self {
        let mut multi_signer = crate::multi_signer::MockMultiSigner::new();
        multi_signer
            .expect_verify_single_signature()
            .returning(|_, _| Err(anyhow::anyhow!("error")));
        multi_signer
            .expect_verify_single_signature_for_next_stake_distribution()
            .returning(|_, _| Err(anyhow::anyhow!("error")));

        Self {
            multi_signer: Arc::new(multi_signer),
            logger: crate::test_tools::TestLogger::stdout(),
        }
    }
}

#[cfg(test)]
mod tests {
    use anyhow::anyhow;

    use crate::multi_signer::MockMultiSigner;
    use crate::test_tools::TestLogger;

    use super::*;

    fn mock_multi_signer(
        multi_signer_mock_config: impl FnOnce(&mut MockMultiSigner),
    ) -> Arc<MockMultiSigner> {
        let mut multi_signer = MockMultiSigner::new();
        multi_signer_mock_config(&mut multi_signer);
        Arc::new(multi_signer)
    }

    #[tokio::test]
    async fn single_signature_against_valid_signed_message_for_current_stake_distribution_is_authenticated(
    ) {
        let signed_message = "signed_message".to_string();
        let mut single_signature = SingleSignatures {
            authentication_status: SingleSignatureAuthenticationStatus::Unauthenticated,
            ..SingleSignatures::fake("party_id", &signed_message)
        };

        let authenticator = SingleSignatureAuthenticator::new(
            mock_multi_signer(|mock_config| {
                mock_config
                    .expect_verify_single_signature()
                    .returning(|_, _| Ok(()));
            }),
            TestLogger::stdout(),
        );

        authenticator
            .authenticate(&mut single_signature, &signed_message)
            .await
            .unwrap();

        assert_eq!(
            single_signature.authentication_status,
            SingleSignatureAuthenticationStatus::Authenticated
        );
    }

    #[tokio::test]
    async fn single_signature_against_valid_signed_message_for_next_stake_distribution_is_authenticated(
    ) {
        let signed_message = "signed_message".to_string();
        let mut single_signature = SingleSignatures {
            authentication_status: SingleSignatureAuthenticationStatus::Unauthenticated,
            ..SingleSignatures::fake("party_id", &signed_message)
        };

        let authenticator = SingleSignatureAuthenticator::new(
            mock_multi_signer(|mock_config| {
                mock_config
                    .expect_verify_single_signature()
                    .returning(|_, _| Err(anyhow!("error")));
                mock_config
                    .expect_verify_single_signature_for_next_stake_distribution()
                    .returning(|_, _| Ok(()));
            }),
            TestLogger::stdout(),
        );

        authenticator
            .authenticate(&mut single_signature, &signed_message)
            .await
            .unwrap();

        assert_eq!(
            single_signature.authentication_status,
            SingleSignatureAuthenticationStatus::Authenticated
        );
    }

    #[tokio::test]
    async fn single_signature_against_invalid_signed_message_for_current_and_next_stake_distribution_is_not_authenticated(
    ) {
        let signed_message = "signed_message".to_string();
        let mut single_signature = SingleSignatures {
            authentication_status: SingleSignatureAuthenticationStatus::Unauthenticated,
            ..SingleSignatures::fake("party_id", &signed_message)
        };

        let authenticator = SingleSignatureAuthenticator::new(
            mock_multi_signer(|mock_config| {
                mock_config
                    .expect_verify_single_signature()
                    .returning(|_, _| Err(anyhow!("verify_single_signature error")));
                mock_config
                    .expect_verify_single_signature_for_next_stake_distribution()
                    .returning(|_, _| {
                        Err(anyhow!(
                            "verify_single_signature_for_next_stake_distribution error"
                        ))
                    });
            }),
            TestLogger::stdout(),
        );

        authenticator
            .authenticate(&mut single_signature, &signed_message)
            .await
            .unwrap();

        assert_eq!(
            single_signature.authentication_status,
            SingleSignatureAuthenticationStatus::Unauthenticated
        );
    }

    #[tokio::test]
    async fn single_signature_previously_authenticated_but_fail_new_authentication_is_now_unauthenticated(
    ) {
        let signed_message = "signed_message".to_string();
        let mut single_signature = SingleSignatures {
            authentication_status: SingleSignatureAuthenticationStatus::Authenticated,
            ..SingleSignatures::fake("party_id", &signed_message)
        };

        let authenticator = SingleSignatureAuthenticator::new(
            mock_multi_signer(|mock_config| {
                mock_config
                    .expect_verify_single_signature()
                    .returning(|_, _| Err(anyhow!("verify_single_signature error")));
                mock_config
                    .expect_verify_single_signature_for_next_stake_distribution()
                    .returning(|_, _| {
                        Err(anyhow!(
                            "verify_single_signature_for_next_stake_distribution error"
                        ))
                    });
            }),
            TestLogger::stdout(),
        );

        authenticator
            .authenticate(&mut single_signature, &signed_message)
            .await
            .unwrap();

        assert_eq!(
            single_signature.authentication_status,
            SingleSignatureAuthenticationStatus::Unauthenticated
        );
    }
}
