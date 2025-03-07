use anyhow::Context;
use std::sync::Arc;

use mithril_common::certificate_chain::{CertificateVerifier, MithrilCertificateVerifier};
use mithril_common::crypto_helper::{
    ProtocolGenesisSigner, ProtocolGenesisVerificationKey, ProtocolGenesisVerifier,
};

use crate::database::repository::{BufferedSingleSignatureRepository, SingleSignatureRepository};
use crate::dependency_injection::{DependenciesBuilder, DependenciesBuilderError, Result};
use crate::services::{
    BufferedCertifierService, CertifierService, MithrilCertifierService,
    MithrilSignerRegistrationSlave, SignerSynchronizer,
};
use crate::{
    ExecutionEnvironment, MithrilSignerRegistrationMaster, MithrilSignerRegistrationVerifier,
    MultiSigner, MultiSignerImpl, SignerRegisterer, SignerRegistrationRoundOpener,
    SignerRegistrationVerifier, SingleSignatureAuthenticator,
};

impl DependenciesBuilder {
    /// Create [CertifierService] service
    pub async fn build_certifier_service(&mut self) -> Result<Arc<dyn CertifierService>> {
        let cardano_network = self.configuration.get_network().with_context(|| {
            "Dependencies Builder can not get Cardano network while building the chain observer"
        })?;
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
        if self.certifier_service.is_none() {
            self.certifier_service = Some(self.build_certifier_service().await?);
        }

        Ok(self.certifier_service.as_ref().cloned().unwrap())
    }

    async fn build_multi_signer(&mut self) -> Result<Arc<dyn MultiSigner>> {
        let multi_signer =
            MultiSignerImpl::new(self.get_epoch_service().await?, self.root_logger());

        Ok(Arc::new(multi_signer))
    }

    /// Get a configured multi signer
    pub async fn get_multi_signer(&mut self) -> Result<Arc<dyn MultiSigner>> {
        if self.multi_signer.is_none() {
            self.multi_signer = Some(self.build_multi_signer().await?);
        }

        Ok(self.multi_signer.as_ref().cloned().unwrap())
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
        if self.certificate_verifier.is_none() {
            self.certificate_verifier = Some(self.build_certificate_verifier().await?);
        }

        Ok(self.certificate_verifier.as_ref().cloned().unwrap())
    }

    async fn build_genesis_verifier(&mut self) -> Result<Arc<ProtocolGenesisVerifier>> {
        let genesis_verifier: ProtocolGenesisVerifier = match self.configuration.environment {
            ExecutionEnvironment::Production => ProtocolGenesisVerifier::from_verification_key(
                ProtocolGenesisVerificationKey::from_json_hex(
                    &self.configuration.genesis_verification_key,
                )
                .map_err(|e| DependenciesBuilderError::Initialization {
                    message: format!(
                        "Could not decode hex key to build genesis verifier: '{}'",
                        self.configuration.genesis_verification_key
                    ),
                    error: Some(e),
                })?,
            ),
            _ => ProtocolGenesisSigner::create_deterministic_genesis_signer()
                .create_genesis_verifier(),
        };

        Ok(Arc::new(genesis_verifier))
    }

    /// Return a [ProtocolGenesisVerifier]
    pub async fn get_genesis_verifier(&mut self) -> Result<Arc<ProtocolGenesisVerifier>> {
        if self.genesis_verifier.is_none() {
            self.genesis_verifier = Some(self.build_genesis_verifier().await?);
        }

        Ok(self.genesis_verifier.as_ref().cloned().unwrap())
    }

    /// Return a [MithrilSignerRegistrationMaster] service
    async fn build_mithril_signer_registration_master(
        &mut self,
    ) -> Result<Arc<MithrilSignerRegistrationMaster>> {
        let registerer = MithrilSignerRegistrationMaster::new(
            self.get_verification_key_store().await?,
            self.get_signer_store().await?,
            self.get_signer_registration_verifier().await?,
            self.configuration.safe_epoch_retention_limit(),
        );

        Ok(Arc::new(registerer))
    }

    /// Return a [MithrilSignerRegistrationMaster]
    pub async fn get_mithril_signer_registration_master(
        &mut self,
    ) -> Result<Arc<MithrilSignerRegistrationMaster>> {
        if self.mithril_signer_registration_master.is_none() {
            self.mithril_signer_registration_master =
                Some(self.build_mithril_signer_registration_master().await?);
        }

        Ok(self
            .mithril_signer_registration_master
            .as_ref()
            .cloned()
            .unwrap())
    }

    /// Return a [MithrilSignerRegistrationSlave] service
    async fn build_mithril_signer_registration_slave(
        &mut self,
    ) -> Result<Arc<MithrilSignerRegistrationSlave>> {
        let registerer = MithrilSignerRegistrationSlave::new(
            self.get_verification_key_store().await?,
            self.get_signer_store().await?,
            self.get_signer_registration_verifier().await?,
            self.get_master_aggregator_client().await?,
            self.get_stake_store().await?,
            self.configuration.safe_epoch_retention_limit(),
        );

        Ok(Arc::new(registerer))
    }

    /// Return a [MithrilSignerRegistrationSlave]
    pub async fn get_mithril_signer_registration_slave(
        &mut self,
    ) -> Result<Arc<MithrilSignerRegistrationSlave>> {
        if self.mithril_signer_registration_slave.is_none() {
            self.mithril_signer_registration_slave =
                Some(self.build_mithril_signer_registration_slave().await?);
        }

        Ok(self
            .mithril_signer_registration_slave
            .as_ref()
            .cloned()
            .unwrap())
    }

    /// Return a [SignerRegisterer]
    pub async fn get_signer_registerer(&mut self) -> Result<Arc<dyn SignerRegisterer>> {
        if self.signer_registerer.is_none() {
            self.signer_registerer = Some(if self.configuration.is_slave_aggregator() {
                self.get_mithril_signer_registration_slave().await?
            } else {
                self.get_mithril_signer_registration_master().await?
            });
        }

        Ok(self.signer_registerer.as_ref().cloned().unwrap())
    }

    /// Return a [SignerSynchronizer]
    pub async fn get_signer_synchronizer(&mut self) -> Result<Arc<dyn SignerSynchronizer>> {
        if self.signer_synchronizer.is_none() {
            self.signer_synchronizer = Some(if self.configuration.is_slave_aggregator() {
                self.get_mithril_signer_registration_slave().await?
            } else {
                self.get_mithril_signer_registration_master().await?
            });
        }

        Ok(self.signer_synchronizer.as_ref().cloned().unwrap())
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
        if self.signer_registration_verifier.is_none() {
            self.signer_registration_verifier =
                Some(self.build_signer_registration_verifier().await?);
        }

        Ok(self.signer_registration_verifier.as_ref().cloned().unwrap())
    }

    /// Return a [SignerRegistrationRoundOpener]
    pub async fn get_signer_registration_round_opener(
        &mut self,
    ) -> Result<Arc<dyn SignerRegistrationRoundOpener>> {
        if self.signer_registration_round_opener.is_none() {
            if self.configuration.is_slave_aggregator() {
                self.signer_registration_round_opener =
                    Some(self.get_mithril_signer_registration_slave().await?);
            } else {
                self.signer_registration_round_opener =
                    Some(self.get_mithril_signer_registration_master().await?);
            }
        }

        Ok(self
            .signer_registration_round_opener
            .as_ref()
            .cloned()
            .unwrap())
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
        if self.single_signer_authenticator.is_none() {
            self.single_signer_authenticator =
                Some(self.build_single_signature_authenticator().await?);
        }

        Ok(self.single_signer_authenticator.as_ref().cloned().unwrap())
    }
}
