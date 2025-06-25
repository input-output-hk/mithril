use anyhow::Context;
use std::sync::Arc;

use mithril_common::certificate_chain::{CertificateVerifier, MithrilCertificateVerifier};
use mithril_common::crypto_helper::{
    ProtocolGenesisSigner, ProtocolGenesisVerificationKey, ProtocolGenesisVerifier,
};

use crate::database::repository::{BufferedSingleSignatureRepository, SingleSignatureRepository};
use crate::dependency_injection::{DependenciesBuilder, DependenciesBuilderError, Result};
use crate::get_dependency;
use crate::services::{
    BufferedCertifierService, CertifierService, MithrilCertifierService,
    MithrilSignerRegistrationFollower, SignerSynchronizer,
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
        let multi_signer =
            MultiSignerImpl::new(self.get_epoch_service().await?, self.root_logger());

        Ok(Arc::new(multi_signer))
    }

    /// Get a configured multi signer
    pub async fn get_multi_signer(&mut self) -> Result<Arc<dyn MultiSigner>> {
        get_dependency!(self.multi_signer)
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

    async fn build_genesis_verifier(&mut self) -> Result<Arc<ProtocolGenesisVerifier>> {
        let genesis_verifier: ProtocolGenesisVerifier = match self.configuration.environment() {
            ExecutionEnvironment::Production => ProtocolGenesisVerifier::from_verification_key(
                ProtocolGenesisVerificationKey::from_json_hex(
                    &self.configuration.genesis_verification_key(),
                )
                .map_err(|e| DependenciesBuilderError::Initialization {
                    message: format!(
                        "Could not decode hex key to build genesis verifier: '{}'",
                        self.configuration.genesis_verification_key()
                    ),
                    error: Some(e),
                })?,
            ),
            _ => ProtocolGenesisSigner::create_deterministic_signer().create_verifier(),
        };

        Ok(Arc::new(genesis_verifier))
    }

    /// Return a [ProtocolGenesisVerifier]
    pub async fn get_genesis_verifier(&mut self) -> Result<Arc<ProtocolGenesisVerifier>> {
        get_dependency!(self.genesis_verifier)
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
