use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use async_trait::async_trait;
use thiserror::Error;

use mithril_common::{
    certificate_chain::CertificateVerifier,
    crypto_helper::{
        key_decode_hex, key_encode_hex, ProtocolClerk, ProtocolGenesisVerifier,
        ProtocolKeyRegistration, ProtocolParameters, ProtocolStakeDistribution,
    },
    entities::{ProtocolMessagePartKey, SignerWithStake},
    messages::MithrilStakeDistributionListItemMessage,
    StdError, StdResult,
};

use crate::aggregator_client::{CertificateClient, MithrilStakeDistributionClient};

/// Errors related to the StakeDistributionService
#[derive(Debug, Error)]
pub enum MithrilStakeDistributionServiceError {
    /// When certificate cannot be verified
    #[error("Could not verify the Mithril stake distribution multisignature, hash='{hash}', certificate hash='{certificate_hash}")]
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
/// Definition of the service responsible of [MithrilStakeDistribution].
#[async_trait]
pub trait StakeDistributionService {
    /// Return a list of the certified Mithril stake distributions
    async fn list(&self) -> StdResult<Vec<MithrilStakeDistributionListItemMessage>>;

    /// Download and verify the specified stake distribution
    async fn verify(
        &self,
        hash: &str,
        dirpath: &Path,
        genesis_verification_key: &str,
    ) -> StdResult<PathBuf>;
}

/// Service responsible of the MithrilStakeDistribution operations.
pub struct MithrilStakeDistributionService {
    /// Aggreggator client for MithrilStakeDistribution
    stake_distribution_client: Arc<MithrilStakeDistributionClient>,
    certificate_client: Arc<CertificateClient>,
    certificate_verifier: Arc<dyn CertificateVerifier>,
}

impl MithrilStakeDistributionService {
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

    // This method is shamefully copied from the Aggregator's multisigner. This
    // WILL be refactored in a hopefully clean API in Common lib.
    async fn create_clerk(
        &self,
        signers_with_stake: &[SignerWithStake],
        protocol_parameters: &ProtocolParameters,
    ) -> StdResult<Option<ProtocolClerk>> {
        let stake_distribution = signers_with_stake
            .iter()
            .map(|s| s.into())
            .collect::<ProtocolStakeDistribution>();
        let mut key_registration = ProtocolKeyRegistration::init(&stake_distribution);
        let mut total_signers = 0;

        for signer in signers_with_stake {
            let operational_certificate = match &signer.operational_certificate {
                Some(operational_certificate) => key_decode_hex(operational_certificate)?,
                _ => None,
            };
            let verification_key = key_decode_hex(&signer.verification_key)?;
            let kes_signature = match &signer.verification_key_signature {
                Some(verification_key_signature) => {
                    Some(key_decode_hex(verification_key_signature)?)
                }
                _ => None,
            };
            let kes_period = signer.kes_period;
            key_registration.register(
                Some(signer.party_id.to_owned()),
                operational_certificate,
                kes_signature,
                kes_period,
                verification_key,
            )?;
            total_signers += 1;
        }

        match total_signers {
            0 => Ok(None),
            _ => {
                let closed_registration = key_registration.close();
                Ok(Some(ProtocolClerk::from_registration(
                    protocol_parameters,
                    &closed_registration,
                )))
            }
        }
    }
}

#[async_trait]
impl StakeDistributionService for MithrilStakeDistributionService {
    async fn list(&self) -> StdResult<Vec<MithrilStakeDistributionListItemMessage>> {
        self.stake_distribution_client.list().await
    }

    async fn verify(
        &self,
        hash: &str,
        dirpath: &Path,
        genesis_verification_key: &str,
    ) -> StdResult<PathBuf> {
        // 1 - retrieve stake distribution
        let stake_distribution =
            self.stake_distribution_client
                .get(hash)
                .await?
                .ok_or_else(|| {
                    MithrilStakeDistributionServiceError::CouldNotFindStakeDistribution(
                        hash.to_owned(),
                    )
                })?;

        // 2 retrieve certificate
        let certificate = self
            .certificate_client
            .get(&stake_distribution.certificate_hash)
            .await?
            .ok_or_else(|| {
                MithrilStakeDistributionServiceError::CertificateNotFound(
                    stake_distribution.certificate_hash.clone(),
                )
            })?;

        // 3 get and check genesis verification key
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

        // 4 Compute and check protocol message
        let clerk = self
            .create_clerk(
                &stake_distribution.signers_with_stake,
                &certificate.metadata.protocol_parameters.clone().into(),
            )
            .await?
            .ok_or_else(|| {
                MithrilStakeDistributionServiceError::CouldNotVerifyStakeDistribution {
                    hash: hash.to_owned(),
                    certificate_hash: certificate.hash.clone(),
                    context: "Cannot verify an empty stake distribution".to_string(),
                }
            })?;
        let avk = key_encode_hex(clerk.compute_avk())?;
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

        // 5 save the JSON file
        if !dirpath.is_dir() {
            std::fs::create_dir_all(dirpath)?;
        }
        let filepath = PathBuf::new()
            .join(dirpath)
            .join("mithril_stake_distribution-{hash}.json");
        std::fs::write(&filepath, serde_json::to_string(&stake_distribution)?)?;

        Ok(filepath)
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::{
        certificate_chain::MithrilCertificateVerifier,
        crypto_helper::ProtocolGenesisSigner,
        entities::Epoch,
        messages::{
            CertificateMessage, MithrilStakeDistributionListMessage,
            MithrilStakeDistributionMessage,
        },
        test_utils::MithrilFixtureBuilder,
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
        let service = MithrilStakeDistributionService::new(
            Arc::new(MithrilStakeDistributionClient::new(http_client.clone())),
            Arc::new(CertificateClient::new(http_client.clone())),
            Arc::new(MithrilCertificateVerifier::new(slog_scope::logger())),
        );
        let list = service.list().await.unwrap();

        assert_eq!(1, list.len());
    }

    #[tokio::test]
    async fn verify_ok() {
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
        let service = MithrilStakeDistributionService::new(
            Arc::new(MithrilStakeDistributionClient::new(http_client.clone())),
            Arc::new(CertificateClient::new(http_client.clone())),
            Arc::new(certificate_verifier),
        );

        let dirpath = std::env::temp_dir().join("test_verify_ok");
        let (_, genesis_verifier) = setup_genesis();
        let genesis_verification_key = genesis_verifier.to_verification_key();
        let filepath = service
            .verify(
                "hash-123",
                &dirpath,
                &key_encode_hex(genesis_verification_key).unwrap(),
            )
            .await
            .unwrap();
    }
}
