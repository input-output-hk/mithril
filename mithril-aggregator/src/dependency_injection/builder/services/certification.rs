//! Services that handle certificates production and validation.

use anyhow::Context;
use std::sync::Arc;

use mithril_common::certificate_chain::{CertificateVerifier, MithrilCertificateVerifier};
use mithril_common::crypto_helper::{
    ProtocolGenesisSigner, ProtocolGenesisVerificationKey, ProtocolGenesisVerifier,
};

use crate::database::repository::{BufferedSingleSignatureRepository, SingleSignatureRepository};
use crate::dependency_injection::{DependenciesBuilder, DependenciesBuilderError};
use crate::services::{BufferedCertifierService, CertifierService, MithrilCertifierService};
use crate::{ExecutionEnvironment, MultiSigner, MultiSignerImpl};

impl DependenciesBuilder {
    /// Create [CertifierService] service
    pub async fn build_certifier_service(
        &mut self,
    ) -> crate::dependency_injection::Result<Arc<dyn CertifierService>> {
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
    pub async fn get_certifier_service(
        &mut self,
    ) -> crate::dependency_injection::Result<Arc<dyn CertifierService>> {
        if self.certifier_service.is_none() {
            self.certifier_service = Some(self.build_certifier_service().await?);
        }

        Ok(self.certifier_service.as_ref().cloned().unwrap())
    }

    async fn build_multi_signer(
        &mut self,
    ) -> crate::dependency_injection::Result<Arc<dyn MultiSigner>> {
        let multi_signer =
            MultiSignerImpl::new(self.get_epoch_service().await?, self.root_logger());

        Ok(Arc::new(multi_signer))
    }

    /// Get the [MultiSigner] instance
    pub async fn get_multi_signer(
        &mut self,
    ) -> crate::dependency_injection::Result<Arc<dyn MultiSigner>> {
        if self.multi_signer.is_none() {
            self.multi_signer = Some(self.build_multi_signer().await?);
        }

        Ok(self.multi_signer.as_ref().cloned().unwrap())
    }

    async fn build_certificate_verifier(
        &mut self,
    ) -> crate::dependency_injection::Result<Arc<dyn CertificateVerifier>> {
        let verifier = Arc::new(MithrilCertificateVerifier::new(
            self.root_logger(),
            self.get_certificate_repository().await?,
        ));

        Ok(verifier)
    }

    /// Get the [CertificateVerifier] instance
    pub async fn get_certificate_verifier(
        &mut self,
    ) -> crate::dependency_injection::Result<Arc<dyn CertificateVerifier>> {
        if self.certificate_verifier.is_none() {
            self.certificate_verifier = Some(self.build_certificate_verifier().await?);
        }

        Ok(self.certificate_verifier.as_ref().cloned().unwrap())
    }

    async fn build_genesis_verifier(
        &mut self,
    ) -> crate::dependency_injection::Result<Arc<ProtocolGenesisVerifier>> {
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

    /// Get the [ProtocolGenesisVerifier] instance
    pub async fn get_genesis_verifier(
        &mut self,
    ) -> crate::dependency_injection::Result<Arc<ProtocolGenesisVerifier>> {
        if self.genesis_verifier.is_none() {
            self.genesis_verifier = Some(self.build_genesis_verifier().await?);
        }

        Ok(self.genesis_verifier.as_ref().cloned().unwrap())
    }
}
