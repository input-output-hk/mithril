use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use async_trait::async_trait;
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
use thiserror::Error;

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
        let stake_distribution =
            self.stake_distribution_client
                .get(hash)
                .await?
                .ok_or_else(|| {
                    MithrilStakeDistributionServiceError::CouldNotFindStakeDistribution(
                        hash.to_owned(),
                    )
                })?;
        let certificate = self
            .certificate_client
            .get(&stake_distribution.certificate_hash)
            .await?
            .ok_or_else(|| {
                MithrilStakeDistributionServiceError::CertificateNotFound(
                    stake_distribution.certificate_hash.clone(),
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
        let genesis_verifier =
            ProtocolGenesisVerifier::from_verification_key(genesis_verification_key);
        self.certificate_verifier
            .verify_certificate_chain(
                certificate.clone(),
                self.certificate_client.clone(),
                &genesis_verifier,
            )
            .await?;
        let clerk = self
            .create_clerk(
                &stake_distribution.signers_with_stake,
                &certificate.metadata.protocol_parameters.into(),
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

        if protocol_message.compute_hash() != certificate.signed_message {
            return Err(
                MithrilStakeDistributionServiceError::CouldNotVerifyStakeDistribution {
                    hash: hash.to_owned(),
                    certificate_hash: certificate.hash,
                    context: "Verification failed: messages do not match.".to_string(),
                }
                .into(),
            );
        }
        let filepath = PathBuf::new()
            .join(dirpath)
            .join("mithril_stake_distribution-{hash}.json");
        std::fs::write(&filepath, serde_json::to_string(&stake_distribution)?)?;

        Ok(filepath)
    }
}
