use async_trait::async_trait;
use std::{
    path::{Path, PathBuf},
    sync::Arc,
};
use thiserror::Error;

use mithril_common::{
    certificate_chain::CertificateVerifier,
    crypto_helper::{
        key_decode_hex, key_encode_hex, ProtocolAggregateVerificationKey, ProtocolGenesisVerifier,
    },
    entities::{MithrilStakeDistribution, ProtocolMessagePartKey},
    messages::MithrilStakeDistributionListItemMessage,
    protocol::SignerBuilder,
    StdError, StdResult,
};

use crate::aggregator_client::{CertificateClient, MithrilStakeDistributionClient};

/// Errors related to the StakeDistributionService
#[derive(Debug, Error)]
pub enum MithrilStakeDistributionServiceError {
    /// When certificate cannot be verified
    #[error("Could not verify the Mithril stake distribution multisignature, hash='{hash}', certificate hash='{certificate_hash}. {context}")]
    CouldNotVerifyStakeDistribution {
        /// StakeDistribution identifier
        hash: String,

        /// Associated certificate
        certificate_hash: String,

        /// Context
        context: String,
    },
    /// Associated certificate not found
    #[error("Could not get associated certificate '{0}'.")]
    CertificateNotFound(String),

    /// The configuration has invalid or missing parameters
    #[error("Missing or invalid parameters: {context}. Error: {error}")]
    InvalidParameters {
        /// Error context
        context: String,

        /// Eventual nested error
        error: StdError,
    },

    /// Could not find the given stake distribution
    #[error("Could not find stake distribution associated to hash '{0}'.")]
    CouldNotFindStakeDistribution(String),
}
/// Definition of the service responsible of Mithril Stake Distribution.
#[async_trait]
pub trait MithrilStakeDistributionService {
    /// Return a list of the certified Mithril stake distributions
    async fn list(&self) -> StdResult<Vec<MithrilStakeDistributionListItemMessage>>;

    /// Download and verify the specified stake distribution
    async fn download(
        &self,
        hash: &str,
        dirpath: &Path,
        genesis_verification_key: &str,
    ) -> StdResult<PathBuf>;
}

/// Service responsible of the MithrilStakeDistribution operations.
pub struct AppMithrilStakeDistributionService {
    /// Aggreggator client for MithrilStakeDistribution
    stake_distribution_client: Arc<MithrilStakeDistributionClient>,
    certificate_client: Arc<CertificateClient>,
    certificate_verifier: Arc<dyn CertificateVerifier>,
}

impl AppMithrilStakeDistributionService {
    /// Constructor
    pub fn new(
        stake_distribution_client: Arc<MithrilStakeDistributionClient>,
        certificate_client: Arc<CertificateClient>,
        certificate_verifier: Arc<dyn CertificateVerifier>,
    ) -> Self {
        Self {
            stake_distribution_client,
            certificate_client,
            certificate_verifier,
        }
    }

    async fn compute_avk_from_mithril_stake_distribution(
        &self,
        stake_distribution: &MithrilStakeDistribution,
    ) -> StdResult<ProtocolAggregateVerificationKey> {
        let signer_builder = SignerBuilder::new(
            &stake_distribution.signers_with_stake,
            &stake_distribution.protocol_parameters,
        )?;

        Ok(signer_builder
            .build_multi_signer()
            .compute_aggregate_verification_key())
    }
}

#[async_trait]
impl MithrilStakeDistributionService for AppMithrilStakeDistributionService {
    async fn list(&self) -> StdResult<Vec<MithrilStakeDistributionListItemMessage>> {
        self.stake_distribution_client.list().await
    }

    async fn download(
        &self,
        hash: &str,
        dirpath: &Path,
        genesis_verification_key: &str,
    ) -> StdResult<PathBuf> {
        let stake_distribution_entity = self
            .stake_distribution_client
            .get(hash)
            .await?
            .ok_or_else(|| {
                MithrilStakeDistributionServiceError::CouldNotFindStakeDistribution(hash.to_owned())
            })?;

        let certificate = self
            .certificate_client
            .get(&stake_distribution_entity.certificate_id)
            .await?
            .ok_or_else(|| {
                MithrilStakeDistributionServiceError::CertificateNotFound(
                    stake_distribution_entity.certificate_id.clone(),
                )
            })?;

        let genesis_verification_key = key_decode_hex(&genesis_verification_key.to_string())
            .map_err(
                |e| MithrilStakeDistributionServiceError::InvalidParameters {
                    context: format!(
                        "Invalid genesis verification key '{genesis_verification_key}'"
                    ),
                    error: e.into(),
                },
            )?;
        self.certificate_verifier
            .verify_certificate_chain(
                certificate.clone(),
                self.certificate_client.clone(),
                &ProtocolGenesisVerifier::from_verification_key(genesis_verification_key),
            )
            .await?;

        let avk = key_encode_hex(
            self.compute_avk_from_mithril_stake_distribution(&stake_distribution_entity.artifact)
                .await?,
        )?;
        let mut protocol_message = certificate.protocol_message.clone();
        protocol_message
            .set_message_part(ProtocolMessagePartKey::NextAggregateVerificationKey, avk);

        if !self
            .certificate_verifier
            .verify_protocol_message(&protocol_message, &certificate)
        {
            return Err(
                MithrilStakeDistributionServiceError::CouldNotVerifyStakeDistribution {
                    hash: hash.to_owned(),
                    certificate_hash: certificate.hash,
                    context: "Verification failed: messages do not match.".to_string(),
                }
                .into(),
            );
        }

        if !dirpath.is_dir() {
            std::fs::create_dir_all(dirpath)?;
        }
        let filepath = PathBuf::new()
            .join(dirpath)
            .join(format!("mithril_stake_distribution-{hash}.json"));
        std::fs::write(
            &filepath,
            serde_json::to_string(&stake_distribution_entity.artifact)?,
        )?;

        Ok(filepath)
    }
}

#[cfg(test)]
mod tests {
    use chrono::{DateTime, Utc};
    use mithril_common::{
        certificate_chain::MithrilCertificateVerifier,
        crypto_helper::ProtocolGenesisSigner,
        entities::{Epoch, SignerWithStake},
        messages::{
            CertificateMessage, MithrilStakeDistributionListMessage,
            MithrilStakeDistributionMessage,
        },
        test_utils::{fake_data, MithrilFixtureBuilder},
    };

    use crate::{
        aggregator_client::MockAggregatorHTTPClient, services::MockCertificateVerifierImpl,
    };

    use super::*;

    fn get_stake_distribution_list_message() -> MithrilStakeDistributionListMessage {
        vec![MithrilStakeDistributionListItemMessage::dummy()]
    }

    fn get_stake_distribution_message(
        signers_with_stake: &[SignerWithStake],
    ) -> MithrilStakeDistributionMessage {
        MithrilStakeDistributionMessage {
            epoch: Epoch(1),
            signers_with_stake: signers_with_stake.to_owned(),
            hash: "hash-123".to_string(),
            certificate_hash: "certificate-hash-123".to_string(),
            created_at: DateTime::<Utc>::default(),
            protocol_parameters: fake_data::protocol_parameters(),
        }
    }

    /// Instantiate a Genesis Signer and its associated Verifier
    pub fn setup_genesis() -> (ProtocolGenesisSigner, ProtocolGenesisVerifier) {
        let genesis_signer = ProtocolGenesisSigner::create_deterministic_genesis_signer();
        let genesis_verifier = genesis_signer.create_genesis_verifier();
        (genesis_signer, genesis_verifier)
    }

    #[tokio::test]
    async fn list_ok() {
        let mut http_client = MockAggregatorHTTPClient::new();
        http_client.expect_get_content().returning(|_| {
            Ok(serde_json::to_string(&get_stake_distribution_list_message()).unwrap())
        });
        let http_client = Arc::new(http_client);
        let service = AppMithrilStakeDistributionService::new(
            Arc::new(MithrilStakeDistributionClient::new(http_client.clone())),
            Arc::new(CertificateClient::new(http_client.clone())),
            Arc::new(MithrilCertificateVerifier::new(slog_scope::logger())),
        );
        let list = service.list().await.unwrap();

        assert_eq!(1, list.len());
    }

    #[tokio::test]
    async fn download_ok() {
        let signers_with_stake = MithrilFixtureBuilder::default()
            .with_signers(2)
            .build()
            .signers_with_stake();
        let stake_distribution_message = get_stake_distribution_message(&signers_with_stake);
        let mut http_client = MockAggregatorHTTPClient::new();
        http_client
            .expect_get_content()
            .returning(move |_| Ok(serde_json::to_string(&stake_distribution_message).unwrap()))
            .times(1);
        http_client
            .expect_get_content()
            .returning(|_| {
                let mut message = CertificateMessage::dummy();
                message.signed_message = message.protocol_message.compute_hash();
                let message = serde_json::to_string(&message).unwrap();

                Ok(message)
            })
            .times(1);
        let http_client = Arc::new(http_client);
        let mut certificate_verifier = MockCertificateVerifierImpl::new();
        certificate_verifier
            .expect_verify_certificate_chain()
            .returning(|_, _, _| Ok(()))
            .times(1);
        certificate_verifier
            .expect_verify_protocol_message()
            .returning(|_, _| true)
            .times(1);
        let service = AppMithrilStakeDistributionService::new(
            Arc::new(MithrilStakeDistributionClient::new(http_client.clone())),
            Arc::new(CertificateClient::new(http_client.clone())),
            Arc::new(certificate_verifier),
        );

        let dirpath = std::env::temp_dir().join("test_download_ok");
        let (_, genesis_verifier) = setup_genesis();
        let genesis_verification_key = genesis_verifier.to_verification_key();
        let filepath = service
            .download(
                "hash-123",
                &dirpath,
                &key_encode_hex(genesis_verification_key).unwrap(),
            )
            .await
            .unwrap();

        assert!(filepath.exists());
    }

    #[tokio::test]
    async fn download_ko() {
        let signers_with_stake = MithrilFixtureBuilder::default()
            .with_signers(2)
            .build()
            .signers_with_stake();
        let stake_distribution_message = get_stake_distribution_message(&signers_with_stake);
        let mut http_client = MockAggregatorHTTPClient::new();
        http_client
            .expect_get_content()
            .returning(move |_| Ok(serde_json::to_string(&stake_distribution_message).unwrap()))
            .times(1);
        http_client
            .expect_get_content()
            .returning(|_| {
                let mut message = CertificateMessage::dummy();
                message.signed_message = message.protocol_message.compute_hash();
                let message = serde_json::to_string(&message).unwrap();

                Ok(message)
            })
            .times(1);
        let http_client = Arc::new(http_client);
        let mut certificate_verifier = MockCertificateVerifierImpl::new();
        certificate_verifier
            .expect_verify_certificate_chain()
            .returning(|_, _, _| Ok(()))
            .times(1);
        certificate_verifier
            .expect_verify_protocol_message()
            .returning(|_, _| false)
            .times(1);
        let service = AppMithrilStakeDistributionService::new(
            Arc::new(MithrilStakeDistributionClient::new(http_client.clone())),
            Arc::new(CertificateClient::new(http_client.clone())),
            Arc::new(certificate_verifier),
        );

        let dirpath = std::env::temp_dir().join("test_download_ko");
        let (_, genesis_verifier) = setup_genesis();
        let genesis_verification_key = genesis_verifier.to_verification_key();
        let _error = service
            .download(
                "hash-123",
                &dirpath,
                &key_encode_hex(genesis_verification_key).unwrap(),
            )
            .await
            .unwrap_err();
    }
}
