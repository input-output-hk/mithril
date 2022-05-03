use thiserror::Error;

use mithril_aggregator::entities::Beacon;
use mithril_aggregator::fake_data;

use crate::certificate_handler::CertificateHandler;
use crate::single_signer::SingleSigner;

struct Signer {
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
    fn new(
        _party_id: &str,
        _signing_key: &str,
        certificate_handler: Box<dyn CertificateHandler>,
        single_signer: Box<dyn SingleSigner>,
    ) -> Self {
        Self {
            certificate_handler,
            single_signer,
            current_beacon: None,
        }
    }

    fn run(&mut self) -> Result<(), SignerError> {
        if let Some(pending_certificate) = self
            .certificate_handler
            .retrieve_pending_certificate()
            .map_err(SignerError::RetrievePendingCertificateFailed)?
        {
            let must_register_signature = match &self.current_beacon {
                None => {
                    self.current_beacon = Some(pending_certificate.beacon);
                    true
                }
                Some(beacon) => beacon != &pending_certificate.beacon,
            };

            if must_register_signature {
                let message = "message".as_bytes().to_vec();
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
                    let _ = self.certificate_handler.register_signatures("");
                }
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::certificate_handler::MockCertificateHandler;
    use crate::single_signer::{MockSingleSigner, SingleSignerError};
    use mithril_aggregator::fake_data;

    #[test]
    fn signer_doesnt_sign_when_there_is_no_pending_certificate() {
        let party_id = "";
        let signing_key = "";
        let mut mock_certificate_handler = MockCertificateHandler::new();
        let mut mock_single_signer = MockSingleSigner::new();
        mock_certificate_handler
            .expect_retrieve_pending_certificate()
            .return_const(Ok(None))
            .once();
        mock_single_signer
            .expect_compute_single_signatures()
            .never();

        let mut signer = Signer::new(
            party_id,
            signing_key,
            Box::new(mock_certificate_handler),
            Box::new(mock_single_signer),
        );
        assert!(signer.run().is_ok());
    }

    #[test]
    fn signer_fails_when_pending_certificate_fails() {
        let party_id = "";
        let signing_key = "";
        let mut mock_certificate_handler = MockCertificateHandler::new();
        mock_certificate_handler
            .expect_retrieve_pending_certificate()
            .return_const(Err("An Error".to_string()))
            .once();

        let mut signer = Signer::new(
            party_id,
            signing_key,
            Box::new(mock_certificate_handler),
            Box::new(MockSingleSigner::new()),
        );
        assert_eq!(
            SignerError::RetrievePendingCertificateFailed("An Error".to_string()),
            signer.run().unwrap_err()
        );
    }

    #[test]
    fn signer_sign_when_triggered_by_pending_certificate() {
        let party_id = "";
        let signing_key = "";
        let mut mock_certificate_handler = MockCertificateHandler::new();
        let mut mock_single_signer = MockSingleSigner::new();
        let pending_certificate = fake_data::certificate_pending();
        mock_certificate_handler
            .expect_retrieve_pending_certificate()
            .return_const(Ok(None))
            .once();
        mock_certificate_handler
            .expect_retrieve_pending_certificate()
            .return_const(Ok(Some(pending_certificate)))
            .once();
        mock_certificate_handler
            .expect_register_signatures()
            .return_const(Ok(()))
            .once();
        mock_single_signer
            .expect_compute_single_signatures()
            .return_once(|_, _, _| Ok(fake_data::single_signatures(2)));

        let mut signer = Signer::new(
            party_id,
            signing_key,
            Box::new(mock_certificate_handler),
            Box::new(mock_single_signer),
        );
        assert!(signer.run().is_ok());
        assert!(signer.run().is_ok());
    }

    #[test]
    fn signer_sign_only_once_if_pending_certificate_has_not_changed() {
        let party_id = "";
        let signing_key = "";
        let mut mock_certificate_handler = MockCertificateHandler::new();
        let mut mock_single_signer = MockSingleSigner::new();
        let pending_certificate = fake_data::certificate_pending();
        mock_certificate_handler
            .expect_retrieve_pending_certificate()
            .return_const(Ok(Some(pending_certificate)))
            .times(2);
        mock_certificate_handler
            .expect_register_signatures()
            .return_const(Ok(()))
            .once();
        mock_single_signer
            .expect_compute_single_signatures()
            .return_once(|_, _, _| Ok(fake_data::single_signatures(2)));

        let mut signer = Signer::new(
            party_id,
            signing_key,
            Box::new(mock_certificate_handler),
            Box::new(mock_single_signer),
        );
        assert!(signer.run().is_ok());
        assert!(signer.run().is_ok());
    }

    #[test]
    fn signer_does_not_send_signatures_if_none_are_computed() {
        let party_id = "";
        let signing_key = "";
        let mut mock_certificate_handler = MockCertificateHandler::new();
        let mut mock_single_signer = MockSingleSigner::new();
        let pending_certificate = fake_data::certificate_pending();
        mock_certificate_handler
            .expect_retrieve_pending_certificate()
            .return_const(Ok(Some(pending_certificate)))
            .once();
        mock_certificate_handler
            .expect_register_signatures()
            .never();
        mock_single_signer
            .expect_compute_single_signatures()
            .return_once(|_, _, _| Ok(fake_data::single_signatures(0)));

        let mut signer = Signer::new(
            party_id,
            signing_key,
            Box::new(mock_certificate_handler),
            Box::new(mock_single_signer),
        );
        assert!(signer.run().is_ok());
    }

    #[test]
    fn signer_fails_if_signature_computation_fails() {
        let party_id = "";
        let signing_key = "";
        let mut mock_certificate_handler = MockCertificateHandler::new();
        let mut mock_single_signer = MockSingleSigner::new();
        let pending_certificate = fake_data::certificate_pending();
        mock_certificate_handler
            .expect_retrieve_pending_certificate()
            .return_const(Ok(Some(pending_certificate)))
            .once();
        mock_single_signer
            .expect_compute_single_signatures()
            .return_once(|_, _, _| Err(SingleSignerError::UnregisteredVerificationKey()));

        let mut signer = Signer::new(
            party_id,
            signing_key,
            Box::new(mock_certificate_handler),
            Box::new(mock_single_signer),
        );
        assert_eq!(
            SignerError::SingleSignaturesComputeFailed(
                SingleSignerError::UnregisteredVerificationKey().to_string()
            ),
            signer.run().unwrap_err()
        );
    }
}
