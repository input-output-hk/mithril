use thiserror::Error;

use mithril_common::entities::Beacon;
use mithril_common::fake_data;

use super::certificate_handler::CertificateHandler;
use super::single_signer::SingleSigner;

pub struct Signer {
    certificate_handler: Box<dyn CertificateHandler>,
    single_signer: Box<dyn SingleSigner>,
    current_beacon: Option<Beacon>,
}

#[derive(Error, Debug, PartialEq)]
pub enum SignerError {
    #[error("single signatures computation failed: `{0}`")]
    SingleSignaturesComputeFailed(String),
    #[error("could not retrieve pending certificate: `{0}`")]
    RetrievePendingCertificateFailed(String),
}

impl Signer {
    pub fn new(
        certificate_handler: Box<dyn CertificateHandler>,
        single_signer: Box<dyn SingleSigner>,
    ) -> Self {
        Self {
            certificate_handler,
            single_signer,
            current_beacon: None,
        }
    }

    pub async fn run(&mut self) -> Result<(), SignerError> {
        if let Some(pending_certificate) = self
            .certificate_handler
            .retrieve_pending_certificate()
            .await
            .map_err(|e| SignerError::RetrievePendingCertificateFailed(e.to_string()))?
        {
            let must_register_signature = match &self.current_beacon {
                None => {
                    self.current_beacon = Some(pending_certificate.beacon);
                    true
                }
                Some(beacon) => beacon != &pending_certificate.beacon,
            };

            if must_register_signature {
                let message = fake_data::digest();
                let stake_distribution = fake_data::signers_with_stakes(5);
                let signatures = self
                    .single_signer
                    .compute_single_signatures(
                        message,
                        stake_distribution,
                        &pending_certificate.protocol_parameters,
                    )
                    .map_err(|e| SignerError::SingleSignaturesComputeFailed(e.to_string()))?;
                if !signatures.is_empty() {
                    let _ = self
                        .certificate_handler
                        .register_signatures(&signatures)
                        .await;
                }
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::super::certificate_handler::{CertificateHandlerError, MockCertificateHandler};
    use super::super::single_signer::{MockSingleSigner, SingleSignerError};
    use super::*;
    use mithril_common::fake_data;

    #[tokio::test]
    async fn signer_doesnt_sign_when_there_is_no_pending_certificate() {
        let mut mock_certificate_handler = MockCertificateHandler::new();
        let mut mock_single_signer = MockSingleSigner::new();
        mock_certificate_handler
            .expect_retrieve_pending_certificate()
            .return_once(|| Ok(None));
        mock_single_signer
            .expect_compute_single_signatures()
            .never();

        let mut signer = Signer::new(
            Box::new(mock_certificate_handler),
            Box::new(mock_single_signer),
        );
        assert!(signer.run().await.is_ok());
    }

    #[tokio::test]
    async fn signer_fails_when_pending_certificate_fails() {
        let mut mock_certificate_handler = MockCertificateHandler::new();
        mock_certificate_handler
            .expect_retrieve_pending_certificate()
            .return_once(|| {
                Err(CertificateHandlerError::RemoteServerTechnical(
                    "An Error".to_string(),
                ))
            });

        let mut signer = Signer::new(
            Box::new(mock_certificate_handler),
            Box::new(MockSingleSigner::new()),
        );
        assert_eq!(
            SignerError::RetrievePendingCertificateFailed(
                CertificateHandlerError::RemoteServerTechnical("An Error".to_string()).to_string()
            ),
            signer.run().await.unwrap_err()
        );
    }

    #[tokio::test]
    async fn signer_sign_when_triggered_by_pending_certificate() {
        let mut mock_certificate_handler = MockCertificateHandler::new();
        let mut mock_single_signer = MockSingleSigner::new();
        let pending_certificate = fake_data::certificate_pending();
        mock_certificate_handler
            .expect_retrieve_pending_certificate()
            .returning(|| Ok(None))
            .once();
        mock_certificate_handler
            .expect_retrieve_pending_certificate()
            .return_once(|| Ok(Some(pending_certificate)));
        mock_certificate_handler
            .expect_register_signatures()
            .return_once(|_| Ok(()));
        mock_single_signer
            .expect_compute_single_signatures()
            .return_once(|_, _, _| Ok(fake_data::single_signatures(2)));

        let mut signer = Signer::new(
            Box::new(mock_certificate_handler),
            Box::new(mock_single_signer),
        );
        assert!(signer.run().await.is_ok());
        assert!(signer.run().await.is_ok());
    }

    #[tokio::test]
    async fn signer_sign_only_once_if_pending_certificate_has_not_changed() {
        let mut mock_certificate_handler = MockCertificateHandler::new();
        let mut mock_single_signer = MockSingleSigner::new();
        let pending_certificate = fake_data::certificate_pending();
        mock_certificate_handler
            .expect_retrieve_pending_certificate()
            .returning(move || Ok(Some(pending_certificate.clone())))
            .times(2);
        mock_certificate_handler
            .expect_register_signatures()
            .return_once(|_| Ok(()));
        mock_single_signer
            .expect_compute_single_signatures()
            .return_once(|_, _, _| Ok(fake_data::single_signatures(2)));

        let mut signer = Signer::new(
            Box::new(mock_certificate_handler),
            Box::new(mock_single_signer),
        );
        assert!(signer.run().await.is_ok());
        assert!(signer.run().await.is_ok());
    }

    #[tokio::test]
    async fn signer_does_not_send_signatures_if_none_are_computed() {
        let mut mock_certificate_handler = MockCertificateHandler::new();
        let mut mock_single_signer = MockSingleSigner::new();
        let pending_certificate = fake_data::certificate_pending();
        mock_certificate_handler
            .expect_retrieve_pending_certificate()
            .return_once(|| Ok(Some(pending_certificate)));
        mock_certificate_handler
            .expect_register_signatures()
            .never();
        mock_single_signer
            .expect_compute_single_signatures()
            .return_once(|_, _, _| Ok(fake_data::single_signatures(0)));

        let mut signer = Signer::new(
            Box::new(mock_certificate_handler),
            Box::new(mock_single_signer),
        );
        assert!(signer.run().await.is_ok());
    }

    #[tokio::test]
    async fn signer_fails_if_signature_computation_fails() {
        let mut mock_certificate_handler = MockCertificateHandler::new();
        let mut mock_single_signer = MockSingleSigner::new();
        let pending_certificate = fake_data::certificate_pending();
        mock_certificate_handler
            .expect_retrieve_pending_certificate()
            .return_once(|| Ok(Some(pending_certificate)));
        mock_single_signer
            .expect_compute_single_signatures()
            .return_once(|_, _, _| Err(SingleSignerError::UnregisteredVerificationKey()));

        let mut signer = Signer::new(
            Box::new(mock_certificate_handler),
            Box::new(mock_single_signer),
        );
        assert_eq!(
            SignerError::SingleSignaturesComputeFailed(
                SingleSignerError::UnregisteredVerificationKey().to_string()
            ),
            signer.run().await.unwrap_err()
        );
    }
}
