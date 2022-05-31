use slog_scope::{error, info};
use std::collections::HashMap;
use std::time::Duration;
use thiserror::Error;
use tokio::time::sleep;

use crate::certificate_handler::CertificateHandlerError;
use crate::single_signer::SingleSignerError;
use mithril_common::crypto_helper::{key_encode_hex, Bytes};
use mithril_common::digesters::{Digester, DigesterError};
use mithril_common::entities::{self, Beacon, CertificatePending, SignerWithStake};
use mithril_common::fake_data;

use super::certificate_handler::CertificateHandler;
use super::single_signer::SingleSigner;

pub struct Runtime {
    certificate_handler: Box<dyn CertificateHandler>,
    single_signer: Box<dyn SingleSigner>,
    digester: Box<dyn Digester>,
    current_beacon: Option<Beacon>,
}

#[derive(Error, Debug)]
pub enum RuntimeError {
    #[error("single signatures computation failed: `{0}`")]
    SingleSignaturesComputeFailed(#[from] SingleSignerError),
    #[error("could not retrieve pending certificate: `{0}`")]
    RetrievePendingCertificateFailed(#[from] CertificateHandlerError),
    #[error("could not retrieve protocol initializer")]
    RetrieveProtocolInitializerFailed(),
    #[error("register signer failed: `{0}`")]
    RegisterSignerFailed(String),
    #[error("codec error:`{0}`")]
    Codec(String),
    #[error("digest computation failed: `{0}`")]
    Digester(#[from] DigesterError),
}

impl Runtime {
    pub fn new(
        certificate_handler: Box<dyn CertificateHandler>,
        single_signer: Box<dyn SingleSigner>,
        digester: Box<dyn Digester>,
    ) -> Self {
        Self {
            certificate_handler,
            single_signer,
            digester,
            current_beacon: None,
        }
    }

    pub async fn infinite_loop(&mut self, loop_interval: u64) {
        loop {
            if let Err(e) = self.run().await {
                error!("{:?}", e)
            }

            info!("Sleeping for {}", loop_interval);
            sleep(Duration::from_millis(loop_interval)).await;
        }
    }

    pub async fn run(&mut self) -> Result<(), RuntimeError> {
        if let Some(pending_certificate) = self
            .certificate_handler
            .retrieve_pending_certificate()
            .await?
        {
            self.register_to_aggregator_if_needed().await?;

            if self.should_register_signature(&pending_certificate.beacon) {
                let message = self.digester.compute_digest()?;
                info!("Signing digest"; "digester_result" => #?message);
                self.register_signature(message.digest.into_bytes(), pending_certificate)
                    .await?;
            }
        }

        Ok(())
    }

    fn should_register_signature(&self, new_beacon: &Beacon) -> bool {
        match &self.current_beacon {
            None => {
                info!("Unknown beacon, signatures will be registered ...");
                true
            }
            Some(beacon) => {
                if beacon != new_beacon {
                    info!("The beacon changed, signatures will be registered ...");
                    true
                } else {
                    info!("Signatures already registered for this beacon");
                    false
                }
            }
        }
    }

    async fn register_to_aggregator_if_needed(&mut self) -> Result<(), RuntimeError> {
        let must_register_to_aggregator = !self.single_signer.get_is_registered();
        if !must_register_to_aggregator {
            return Ok(());
        }

        if let Some(protocol_initializer) = self.single_signer.get_protocol_initializer() {
            let verification_key = protocol_initializer.verification_key();
            let verification_key = key_encode_hex(verification_key).map_err(RuntimeError::Codec)?;
            let signer = entities::Signer::new(self.single_signer.get_party_id(), verification_key);
            self.certificate_handler
                .register_signer(&signer)
                .await
                .map_err(|e| RuntimeError::RegisterSignerFailed(e.to_string()))?;
            self.single_signer
                .update_is_registered(true)
                .map_err(|e| RuntimeError::RegisterSignerFailed(e.to_string()))?;
        }

        Ok(())
    }

    async fn register_signature(
        &mut self,
        message: Bytes,
        pending_certificate: CertificatePending,
    ) -> Result<(), RuntimeError> {
        let verification_keys = pending_certificate
            .signers
            .iter()
            .map(|signer| (signer.party_id, signer.verification_key.as_str()))
            .collect::<HashMap<u64, &str>>();

        let stake_distribution = fake_data::signers_with_stakes(5);
        let stake_distribution_extended = stake_distribution
            .into_iter()
            .map(|signer| {
                let verification_key = match verification_keys.get(&signer.party_id) {
                    Some(verification_key_found) => *verification_key_found,
                    None => "",
                };
                SignerWithStake::new(signer.party_id, verification_key.to_string(), signer.stake)
            })
            .collect::<Vec<SignerWithStake>>();

        let signatures = self.single_signer.compute_single_signatures(
            message,
            stake_distribution_extended,
            &pending_certificate.protocol_parameters,
        )?;

        if !signatures.is_empty() {
            let _ = self
                .certificate_handler
                .register_signatures(&signatures)
                .await;
        }
        self.current_beacon = Some(pending_certificate.beacon);

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::super::certificate_handler::{CertificateHandlerError, MockCertificateHandler};
    use super::super::single_signer::{MockSingleSigner, SingleSignerError};
    use super::*;

    use mithril_common::crypto_helper::tests_setup::*;
    use mithril_common::digesters::{Digester, DigesterError, DigesterResult};
    use mithril_common::fake_data;
    use mockall::mock;

    mock! {
        pub DigesterImpl { }
        impl Digester for DigesterImpl {
            fn compute_digest(&self) -> Result<DigesterResult, DigesterError>;
        }
    }

    #[tokio::test]
    async fn signer_doesnt_sign_when_there_is_no_pending_certificate() {
        let current_signer = &setup_signers(1)[0];
        let party_id = current_signer.clone().0;
        let protocol_initializer = current_signer.4.clone();
        let mut mock_certificate_handler = MockCertificateHandler::new();
        let mut mock_single_signer = MockSingleSigner::new();
        let mock_digester = MockDigesterImpl::new();
        mock_certificate_handler
            .expect_retrieve_pending_certificate()
            .return_once(|| Ok(None));
        mock_certificate_handler
            .expect_register_signer()
            .return_once(|_| Ok(()));
        mock_single_signer
            .expect_compute_single_signatures()
            .never();
        mock_single_signer
            .expect_get_party_id()
            .return_once(move || party_id);
        mock_single_signer
            .expect_get_protocol_initializer()
            .return_once(move || Some(protocol_initializer));
        mock_single_signer
            .expect_get_is_registered()
            .return_once(|| false);

        let mut signer = Runtime::new(
            Box::new(mock_certificate_handler),
            Box::new(mock_single_signer),
            Box::new(mock_digester),
        );
        assert!(signer.run().await.is_ok());
    }

    #[tokio::test]
    async fn signer_fails_when_pending_certificate_fails() {
        let mut mock_certificate_handler = MockCertificateHandler::new();
        let mut mock_single_signer = MockSingleSigner::new();
        let mock_digester = MockDigesterImpl::new();
        mock_certificate_handler
            .expect_retrieve_pending_certificate()
            .return_once(|| {
                Err(CertificateHandlerError::RemoteServerTechnical(
                    "An Error".to_string(),
                ))
            });
        mock_single_signer
            .expect_get_protocol_initializer()
            .return_once(move || None);

        let mut signer = Runtime::new(
            Box::new(mock_certificate_handler),
            Box::new(mock_single_signer),
            Box::new(mock_digester),
        );
        assert_eq!(
            RuntimeError::RetrievePendingCertificateFailed(
                CertificateHandlerError::RemoteServerTechnical("An Error".to_string())
            )
            .to_string(),
            signer.run().await.unwrap_err().to_string()
        );
    }

    #[tokio::test]
    async fn signer_sign_when_triggered_by_pending_certificate() {
        let current_signer = &setup_signers(1)[0];
        let party_id = current_signer.clone().0;
        let protocol_initializer = current_signer.4.clone();
        let mut mock_certificate_handler = MockCertificateHandler::new();
        let mut mock_single_signer = MockSingleSigner::new();
        let mut mock_digester = MockDigesterImpl::new();
        let pending_certificate = fake_data::certificate_pending();
        mock_certificate_handler
            .expect_retrieve_pending_certificate()
            .returning(|| Ok(None))
            .once();
        mock_certificate_handler
            .expect_retrieve_pending_certificate()
            .return_once(|| Ok(Some(pending_certificate)));
        mock_certificate_handler
            .expect_register_signer()
            .returning(|_| Ok(()))
            .times(1);
        mock_certificate_handler
            .expect_register_signatures()
            .return_once(|_| Ok(()));
        mock_single_signer
            .expect_compute_single_signatures()
            .return_once(|_, _, _| Ok(fake_data::single_signatures(2)));
        mock_single_signer
            .expect_get_party_id()
            .return_once(move || party_id);
        mock_single_signer
            .expect_get_protocol_initializer()
            .return_once(move || Some(protocol_initializer));
        mock_single_signer
            .expect_get_is_registered()
            .return_once(|| false);
        mock_single_signer
            .expect_update_is_registered()
            .return_once(move |_| Ok(()));
        mock_digester
            .expect_compute_digest()
            .return_once(|| Ok(fake_data::digester_result("digest")));

        let mut signer = Runtime::new(
            Box::new(mock_certificate_handler),
            Box::new(mock_single_signer),
            Box::new(mock_digester),
        );
        assert!(signer.run().await.is_ok());
        assert!(signer.run().await.is_ok());
    }

    #[tokio::test]
    async fn signer_sign_only_once_if_pending_certificate_has_not_changed() {
        let current_signer = &setup_signers(1)[0];
        let party_id = current_signer.clone().0;
        let protocol_initializer = current_signer.4.clone();
        let mut mock_certificate_handler = MockCertificateHandler::new();
        let mut mock_single_signer = MockSingleSigner::new();
        let mut mock_digester = MockDigesterImpl::new();
        let pending_certificate = fake_data::certificate_pending();
        mock_certificate_handler
            .expect_retrieve_pending_certificate()
            .returning(move || Ok(Some(pending_certificate.clone())))
            .times(2);
        mock_certificate_handler
            .expect_register_signatures()
            .return_once(|_| Ok(()));
        mock_certificate_handler
            .expect_register_signer()
            .returning(|_| Ok(()))
            .times(1);
        mock_single_signer
            .expect_compute_single_signatures()
            .return_once(|_, _, _| Ok(fake_data::single_signatures(2)));
        mock_single_signer
            .expect_get_party_id()
            .returning(move || party_id)
            .once();
        mock_single_signer
            .expect_get_is_registered()
            .returning(|| false)
            .once();
        mock_single_signer
            .expect_get_is_registered()
            .returning(|| true)
            .once();
        mock_single_signer
            .expect_update_is_registered()
            .return_once(move |_| Ok(()));
        mock_single_signer
            .expect_get_protocol_initializer()
            .return_once(move || Some(protocol_initializer.clone()));
        mock_digester
            .expect_compute_digest()
            .return_once(|| Ok(fake_data::digester_result("digest")));

        let mut signer = Runtime::new(
            Box::new(mock_certificate_handler),
            Box::new(mock_single_signer),
            Box::new(mock_digester),
        );
        assert!(signer.run().await.is_ok());
        assert!(signer.run().await.is_ok());
    }

    #[tokio::test]
    async fn signer_does_not_send_signatures_if_none_are_computed() {
        let current_signer = &setup_signers(1)[0];
        let party_id = current_signer.clone().0;
        let protocol_initializer = current_signer.4.clone();
        let mut mock_certificate_handler = MockCertificateHandler::new();
        let mut mock_single_signer = MockSingleSigner::new();
        let mut mock_digester = MockDigesterImpl::new();
        let pending_certificate = fake_data::certificate_pending();
        mock_certificate_handler
            .expect_retrieve_pending_certificate()
            .return_once(|| Ok(Some(pending_certificate)));
        mock_certificate_handler
            .expect_register_signatures()
            .never();
        mock_certificate_handler
            .expect_register_signer()
            .return_once(|_| Ok(()));
        mock_single_signer
            .expect_compute_single_signatures()
            .return_once(|_, _, _| Ok(fake_data::single_signatures(0)));
        mock_single_signer
            .expect_get_party_id()
            .return_once(move || party_id);
        mock_single_signer
            .expect_get_protocol_initializer()
            .return_once(move || Some(protocol_initializer));
        mock_single_signer
            .expect_get_is_registered()
            .return_once(|| false);
        mock_single_signer
            .expect_update_is_registered()
            .return_once(move |_| Ok(()));
        mock_digester
            .expect_compute_digest()
            .return_once(|| Ok(fake_data::digester_result("digest")));

        let mut signer = Runtime::new(
            Box::new(mock_certificate_handler),
            Box::new(mock_single_signer),
            Box::new(mock_digester),
        );
        assert!(signer.run().await.is_ok());
    }

    #[tokio::test]
    async fn signer_fails_if_signature_computation_fails() {
        let mut mock_certificate_handler = MockCertificateHandler::new();
        let mut mock_single_signer = MockSingleSigner::new();
        let mut mock_digester = MockDigesterImpl::new();
        let pending_certificate = fake_data::certificate_pending();
        mock_certificate_handler
            .expect_retrieve_pending_certificate()
            .return_once(|| Ok(Some(pending_certificate)));
        mock_single_signer
            .expect_compute_single_signatures()
            .return_once(|_, _, _| Err(SingleSignerError::UnregisteredVerificationKey()));
        mock_single_signer
            .expect_get_is_registered()
            .return_once(|| false);
        mock_single_signer
            .expect_get_protocol_initializer()
            .return_once(move || None);
        mock_digester
            .expect_compute_digest()
            .return_once(|| Ok(fake_data::digester_result("digest")));

        let mut signer = Runtime::new(
            Box::new(mock_certificate_handler),
            Box::new(mock_single_signer),
            Box::new(mock_digester),
        );
        assert_eq!(
            RuntimeError::SingleSignaturesComputeFailed(
                SingleSignerError::UnregisteredVerificationKey()
            )
            .to_string(),
            signer.run().await.unwrap_err().to_string()
        );
    }

    #[tokio::test]
    async fn signer_fails_when_register_signer_fails() {
        let current_signer = &setup_signers(1)[0];
        let party_id = current_signer.clone().0;
        let protocol_initializer = current_signer.4.clone();
        let pending_certificate = fake_data::certificate_pending();
        let mut mock_certificate_handler = MockCertificateHandler::new();
        let mut mock_single_signer = MockSingleSigner::new();
        let mock_digester = MockDigesterImpl::new();
        mock_certificate_handler
            .expect_retrieve_pending_certificate()
            .return_once(|| Ok(Some(pending_certificate)));
        mock_certificate_handler
            .expect_register_signer()
            .return_once(|_| {
                Err(CertificateHandlerError::RemoteServerLogical(
                    "an error occurred".to_string(),
                ))
            });
        mock_single_signer
            .expect_compute_single_signatures()
            .never();
        mock_single_signer
            .expect_get_party_id()
            .return_once(move || party_id);
        mock_single_signer
            .expect_get_protocol_initializer()
            .return_once(move || Some(protocol_initializer));
        mock_single_signer
            .expect_get_is_registered()
            .return_once(|| false);

        let mut signer = Runtime::new(
            Box::new(mock_certificate_handler),
            Box::new(mock_single_signer),
            Box::new(mock_digester),
        );
        assert_eq!(
            RuntimeError::RegisterSignerFailed(
                CertificateHandlerError::RemoteServerLogical("an error occurred".to_string())
                    .to_string()
            )
            .to_string(),
            signer.run().await.unwrap_err().to_string()
        );
    }

    #[tokio::test]
    async fn signer_fails_if_digest_computation_fails() {
        let mut mock_certificate_handler = MockCertificateHandler::new();
        let mut mock_single_signer = MockSingleSigner::new();
        let mut mock_digester = MockDigesterImpl::new();
        let pending_certificate = fake_data::certificate_pending();
        mock_certificate_handler
            .expect_retrieve_pending_certificate()
            .return_once(|| Ok(Some(pending_certificate)));
        mock_single_signer
            .expect_compute_single_signatures()
            .never();
        mock_single_signer
            .expect_get_is_registered()
            .return_once(|| false);
        mock_single_signer
            .expect_get_protocol_initializer()
            .return_once(move || None);
        mock_digester
            .expect_compute_digest()
            .return_once(|| Err(DigesterError::NotEnoughImmutable()));

        let mut signer = Runtime::new(
            Box::new(mock_certificate_handler),
            Box::new(mock_single_signer),
            Box::new(mock_digester),
        );
        assert_eq!(
            RuntimeError::Digester(DigesterError::NotEnoughImmutable()).to_string(),
            signer.run().await.unwrap_err().to_string()
        );
    }
}
