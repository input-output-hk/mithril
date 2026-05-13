use anyhow::Context;
use std::sync::Arc;

use mithril_common::certificate_chain::{CertificateVerifier, MithrilCertificateVerifier};
use mithril_common::crypto_helper::{
    GenesisEd25519Signer, GenesisEd25519VerificationKey, GenesisEd25519Verifier,
};
#[cfg(feature = "future_snark")]
use mithril_common::crypto_helper::{
    GenesisSchnorrSigner, GenesisSchnorrVerifier, GenesisVerificationKeyBundle,
};

use crate::database::repository::{BufferedSingleSignatureRepository, SingleSignatureRepository};
use crate::dependency_injection::{DependenciesBuilder, DependenciesBuilderError, Result};
use crate::get_dependency;
use crate::services::{
    BufferedCertifierService, CertificateChainSynchronizer, CertifierService,
    MithrilCertificateChainSynchronizer, MithrilCertificateChainSynchronizerNoop,
    MithrilCertifierService, MithrilSignerRegistrationFollower, SignerSynchronizer,
};
use crate::{
    ExecutionEnvironment, MithrilSignerRegistrationLeader, MithrilSignerRegistrationVerifier,
    MultiSigner, MultiSignerImpl, SignerRegisterer, SignerRegistrationRoundOpener,
    SignerRegistrationVerifier, SingleSignatureAuthenticator,
};

/// Retrieve the mithril_signer_registration according to the configuration.
/// The macro is used because we are not able to return different kind of trait using a function.
macro_rules! get_mithril_signer_registration {
    ($self:ident) => {{
        if $self.configuration.is_follower_aggregator() {
            $self.get_mithril_signer_registration_follower().await?
        } else {
            $self.get_mithril_signer_registration_leader().await?
        }
    }};
}

impl DependenciesBuilder {
    /// Create [CertifierService] service
    pub async fn build_certifier_service(&mut self) -> Result<Arc<dyn CertifierService>> {
        let cardano_network = self.configuration.get_network().with_context(
            || "Dependencies Builder can not get Cardano network while building the chain observer",
        )?;
        let sqlite_connection = self.get_sqlite_connection().await?;
        let open_message_repository = self.get_open_message_repository().await?;
        let single_signature_repository =
            Arc::new(SingleSignatureRepository::new(sqlite_connection.clone()));
        let certificate_repository = self.get_certificate_repository().await?;
        let certificate_verifier = self.get_certificate_verifier().await?;
        let genesis_verifier = self.get_genesis_verifier().await?;
        let multi_signer = self.get_multi_signer().await?;
        let epoch_service = self.get_epoch_service().await?;
        let logger = self.root_logger();

        let certifier = Arc::new(MithrilCertifierService::new(
            cardano_network,
            open_message_repository,
            single_signature_repository,
            certificate_repository,
            certificate_verifier,
            genesis_verifier,
            multi_signer,
            epoch_service,
            logger,
        ));

        Ok(Arc::new(BufferedCertifierService::new(
            certifier,
            Arc::new(BufferedSingleSignatureRepository::new(sqlite_connection)),
            self.root_logger(),
        )))
    }

    /// [CertifierService] service
    pub async fn get_certifier_service(&mut self) -> Result<Arc<dyn CertifierService>> {
        get_dependency!(self.certifier_service)
    }

    async fn build_multi_signer(&mut self) -> Result<Arc<dyn MultiSigner>> {
        let multi_signer = MultiSignerImpl::new(
            self.configuration.aggregate_signature_type(),
            self.get_epoch_service().await?,
            self.root_logger(),
        );

        Ok(Arc::new(multi_signer))
    }

    /// Get a configured multi signer
    pub async fn get_multi_signer(&mut self) -> Result<Arc<dyn MultiSigner>> {
        get_dependency!(self.multi_signer)
    }

    async fn build_certificate_chain_synchronizer(
        &mut self,
    ) -> Result<Arc<dyn CertificateChainSynchronizer>> {
        let synchronizer: Arc<dyn CertificateChainSynchronizer> =
            if self.configuration.is_follower_aggregator() {
                let leader_aggregator_client = self.get_leader_aggregator_client().await?;
                let verifier = Arc::new(MithrilCertificateVerifier::new(
                    self.root_logger(),
                    leader_aggregator_client.clone(),
                ));

                Arc::new(MithrilCertificateChainSynchronizer::new(
                    leader_aggregator_client,
                    self.get_certificate_repository().await?,
                    verifier,
                    self.get_genesis_verifier().await?,
                    self.get_open_message_repository().await?,
                    self.get_epoch_settings_store().await?,
                    self.root_logger(),
                ))
            } else {
                Arc::new(MithrilCertificateChainSynchronizerNoop)
            };

        Ok(synchronizer)
    }

    /// [CertificateChainSynchronizer] service
    pub async fn get_certificate_chain_synchronizer(
        &mut self,
    ) -> Result<Arc<dyn CertificateChainSynchronizer>> {
        get_dependency!(self.certificate_chain_synchronizer)
    }

    async fn build_certificate_verifier(&mut self) -> Result<Arc<dyn CertificateVerifier>> {
        let verifier = Arc::new(MithrilCertificateVerifier::new(
            self.root_logger(),
            self.get_certificate_repository().await?,
        ));

        Ok(verifier)
    }

    /// [CertificateVerifier] service.
    pub async fn get_certificate_verifier(&mut self) -> Result<Arc<dyn CertificateVerifier>> {
        get_dependency!(self.certificate_verifier)
    }

    async fn build_genesis_verifier(&mut self) -> Result<Arc<GenesisEd25519Verifier>> {
        let genesis_verifier: GenesisEd25519Verifier = match self.configuration.environment() {
            ExecutionEnvironment::Production => {
                let concatenation_verification_key =
                    self.parse_concatenation_genesis_verification_key()?;
                GenesisEd25519Verifier::from_verification_key(concatenation_verification_key)
            }
            _ => GenesisEd25519Signer::create_deterministic_signer().create_verifier(),
        };

        Ok(Arc::new(genesis_verifier))
    }

    fn parse_concatenation_genesis_verification_key(
        &self,
    ) -> Result<GenesisEd25519VerificationKey> {
        let raw_verification_key = self.configuration.genesis_verification_key();
        #[cfg(feature = "future_snark")]
        {
            let bundle = GenesisVerificationKeyBundle::try_from_hex_or_legacy(
                &raw_verification_key,
            )
            .map_err(|e| DependenciesBuilderError::Initialization {
                message: format!(
                    "Could not decode genesis verification key bundle: '{raw_verification_key}'"
                ),
                error: Some(e),
            })?;
            Ok(bundle.ed25519)
        }
        #[cfg(not(feature = "future_snark"))]
        {
            GenesisEd25519VerificationKey::from_json_hex(&raw_verification_key)
                .map_err(|e| DependenciesBuilderError::Initialization {
                    message: format!(
                        "Could not decode hex key to build genesis verifier: '{raw_verification_key}'"
                    ),
                    error: Some(e),
                })
        }
    }

    /// Return a [GenesisEd25519Verifier]
    pub async fn get_genesis_verifier(&mut self) -> Result<Arc<GenesisEd25519Verifier>> {
        get_dependency!(self.genesis_verifier)
    }

    /// Build the optional SNARK-friendly genesis verifier.
    ///
    /// Returns `Some` when the configured verification key is a dual signing bundle, `None` when
    /// it is the legacy single-Ed25519 file. In non-production environments a fresh SNARK
    /// verifier is generated on demand to keep dev/test parity with production.
    #[cfg(feature = "future_snark")]
    async fn build_genesis_schnorr_verifier(
        &mut self,
    ) -> Result<Option<Arc<GenesisSchnorrVerifier>>> {
        let snark_verifier = match self.configuration.environment() {
            ExecutionEnvironment::Production => {
                let raw_verification_key = self.configuration.genesis_verification_key();
                let bundle = GenesisVerificationKeyBundle::try_from_hex_or_legacy(
                    &raw_verification_key,
                )
                .map_err(|e| DependenciesBuilderError::Initialization {
                    message: format!(
                        "Could not decode genesis verification key bundle: '{raw_verification_key}'"
                    ),
                    error: Some(e),
                })?;
                bundle
                    .schnorr
                    .map(|key| Arc::new(GenesisSchnorrVerifier::from_verification_key(key)))
            }
            _ => Some(Arc::new(
                GenesisSchnorrSigner::create_non_deterministic_signer().create_verifier(),
            )),
        };

        Ok(snark_verifier)
    }

    /// Return the optional [GenesisSchnorrVerifier], `None` when only the legacy
    /// single-Ed25519 verification key is configured.
    #[cfg(feature = "future_snark")]
    pub async fn get_genesis_schnorr_verifier(
        &mut self,
    ) -> Result<Option<Arc<GenesisSchnorrVerifier>>> {
        get_dependency!(self.genesis_schnorr_verifier)
    }

    /// Return a [MithrilSignerRegistrationLeader] service
    async fn build_mithril_signer_registration_leader(
        &mut self,
    ) -> Result<Arc<MithrilSignerRegistrationLeader>> {
        let registerer = MithrilSignerRegistrationLeader::new(
            self.get_verification_key_store().await?,
            self.get_signer_store().await?,
            self.get_signer_registration_verifier().await?,
        );

        Ok(Arc::new(registerer))
    }

    /// Return a [MithrilSignerRegistrationLeader]
    pub async fn get_mithril_signer_registration_leader(
        &mut self,
    ) -> Result<Arc<MithrilSignerRegistrationLeader>> {
        get_dependency!(self.mithril_signer_registration_leader)
    }

    /// Return a [MithrilSignerRegistrationFollower] service
    async fn build_mithril_signer_registration_follower(
        &mut self,
    ) -> Result<Arc<MithrilSignerRegistrationFollower>> {
        let registerer = MithrilSignerRegistrationFollower::new(
            self.get_epoch_service().await?,
            self.get_verification_key_store().await?,
            self.get_signer_store().await?,
            self.get_signer_registration_verifier().await?,
            self.get_leader_aggregator_client().await?,
            self.get_stake_store().await?,
        );

        Ok(Arc::new(registerer))
    }

    /// Return a [MithrilSignerRegistrationFollower]
    pub async fn get_mithril_signer_registration_follower(
        &mut self,
    ) -> Result<Arc<MithrilSignerRegistrationFollower>> {
        get_dependency!(self.mithril_signer_registration_follower)
    }

    /// Return a [SignerRegisterer]
    pub async fn get_signer_registerer(&mut self) -> Result<Arc<dyn SignerRegisterer>> {
        get_dependency!(self.signer_registerer = get_mithril_signer_registration!(self))
    }

    /// Return a [SignerSynchronizer]
    pub async fn get_signer_synchronizer(&mut self) -> Result<Arc<dyn SignerSynchronizer>> {
        get_dependency!(self.signer_synchronizer = get_mithril_signer_registration!(self))
    }

    async fn build_signer_registration_verifier(
        &mut self,
    ) -> Result<Arc<dyn SignerRegistrationVerifier>> {
        let registerer = MithrilSignerRegistrationVerifier::new(self.get_chain_observer().await?);

        Ok(Arc::new(registerer))
    }

    /// Return a [SignerRegistrationVerifier]
    pub async fn get_signer_registration_verifier(
        &mut self,
    ) -> Result<Arc<dyn SignerRegistrationVerifier>> {
        get_dependency!(self.signer_registration_verifier)
    }

    /// Return a [SignerRegistrationRoundOpener]
    pub async fn get_signer_registration_round_opener(
        &mut self,
    ) -> Result<Arc<dyn SignerRegistrationRoundOpener>> {
        get_dependency!(
            self.signer_registration_round_opener = get_mithril_signer_registration!(self)
        )
    }

    async fn build_single_signature_authenticator(
        &mut self,
    ) -> Result<Arc<SingleSignatureAuthenticator>> {
        let authenticator =
            SingleSignatureAuthenticator::new(self.get_multi_signer().await?, self.root_logger());

        Ok(Arc::new(authenticator))
    }

    /// [SingleSignatureAuthenticator] service
    pub async fn get_single_signature_authenticator(
        &mut self,
    ) -> Result<Arc<SingleSignatureAuthenticator>> {
        get_dependency!(self.single_signature_authenticator)
    }
}

#[cfg(all(test, feature = "future_snark"))]
mod tests {
    use mithril_common::crypto_helper::{GenesisSchnorrSigner, ProtocolKey};
    use mithril_common::temp_dir;

    use crate::configuration::ServeCommandConfiguration;

    use super::*;

    fn dual_bundle_hex() -> (String, GenesisSchnorrVerifier) {
        let ed25519_signer = GenesisEd25519Signer::create_deterministic_signer();
        let schnorr_signer = GenesisSchnorrSigner::create_non_deterministic_signer();
        let verification_key_bundle = GenesisVerificationKeyBundle::new(
            ed25519_signer.verification_key(),
            schnorr_signer.verification_key(),
        );
        let hex_string = ProtocolKey::new(verification_key_bundle).to_bytes_hex().unwrap();
        (hex_string, schnorr_signer.create_verifier())
    }

    fn production_config_with_genesis_key(
        genesis_verification_key: String,
    ) -> ServeCommandConfiguration {
        ServeCommandConfiguration {
            environment: ExecutionEnvironment::Production,
            genesis_verification_key,
            ..ServeCommandConfiguration::new_sample(temp_dir!())
        }
    }

    #[tokio::test]
    async fn build_genesis_verifier_accepts_a_legacy_single_ed25519_key_in_production() {
        let mut dep_builder = {
            let config = ServeCommandConfiguration {
                environment: ExecutionEnvironment::Production,
                ..ServeCommandConfiguration::new_sample(temp_dir!())
            };
            DependenciesBuilder::new_with_stdout_logger(Arc::new(config))
        };

        dep_builder
            .build_genesis_verifier()
            .await
            .expect("legacy single-Ed25519 verification key must be accepted in production");
    }

    #[tokio::test]
    async fn build_genesis_verifier_accepts_a_dual_bundle_key_in_production() {
        let (bundle_hex, _) = dual_bundle_hex();
        let mut dep_builder = DependenciesBuilder::new_with_stdout_logger(Arc::new(
            production_config_with_genesis_key(bundle_hex),
        ));

        dep_builder
            .build_genesis_verifier()
            .await
            .expect("dual verification key bundle must be accepted in production");
    }

    #[tokio::test]
    async fn build_genesis_schnorr_verifier_returns_none_for_a_legacy_key_in_production() {
        let mut dep_builder = {
            let config = ServeCommandConfiguration {
                environment: ExecutionEnvironment::Production,
                ..ServeCommandConfiguration::new_sample(temp_dir!())
            };
            DependenciesBuilder::new_with_stdout_logger(Arc::new(config))
        };

        let snark_verifier = dep_builder.build_genesis_schnorr_verifier().await.unwrap();

        assert!(
            snark_verifier.is_none(),
            "legacy single-Ed25519 verification key must yield no SNARK verifier"
        );
    }

    #[tokio::test]
    async fn build_genesis_schnorr_verifier_returns_some_for_a_dual_bundle_in_production() {
        let (bundle_hex, expected_snark_verifier) = dual_bundle_hex();
        let mut dep_builder = DependenciesBuilder::new_with_stdout_logger(Arc::new(
            production_config_with_genesis_key(bundle_hex),
        ));

        let snark_verifier = dep_builder
            .build_genesis_schnorr_verifier()
            .await
            .unwrap()
            .expect("dual bundle must yield a SNARK verifier");

        assert_eq!(
            snark_verifier.to_verification_key().to_bytes(),
            expected_snark_verifier.to_verification_key().to_bytes(),
        );
    }

    #[tokio::test]
    async fn build_genesis_schnorr_verifier_returns_some_in_non_production_environment() {
        let mut dep_builder = {
            let config = ServeCommandConfiguration::new_sample(temp_dir!());
            DependenciesBuilder::new_with_stdout_logger(Arc::new(config))
        };

        let snark_verifier = dep_builder.build_genesis_schnorr_verifier().await.unwrap();

        assert!(
            snark_verifier.is_some(),
            "non-production environment must auto-generate a SNARK verifier"
        );
    }
}
