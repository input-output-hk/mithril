use mithril_aggregator::entities::{Beacon, CertificatePending};

#[cfg(test)]
use mockall::automock;

#[cfg_attr(test, automock)]
pub trait CertificateHandler {
    fn retrieve_pending_certificate(&self) -> Result<Option<CertificatePending>, String>;

    fn register_signatures(&self, signature: &str) -> Result<(), String>;
}

struct Signer {
    certificate_handler: Box<dyn CertificateHandler>,
    current_beacon: Option<Beacon>,
}

impl Signer {
    fn new(
        _party_id: &str,
        _signing_key: &str,
        certificate_handler: Box<dyn CertificateHandler>,
    ) -> Self {
        Self {
            certificate_handler,
            current_beacon: None,
        }
    }

    fn run(&mut self) {
        if let Some(pending_certificate) = self
            .certificate_handler
            .retrieve_pending_certificate()
            .unwrap()
        {
            let must_register_signature = match &self.current_beacon {
                None => {
                    self.current_beacon = Some(pending_certificate.beacon);
                    true
                }
                Some(beacon) => beacon != &pending_certificate.beacon,
            };

            if must_register_signature {
                let _ = self.certificate_handler.register_signatures("");
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use mithril_aggregator::fake_data;

    #[test]
    fn signer_doesnt_sign_when_there_is_no_pending_certificate() {
        let party_id = "";
        let signing_key = "";
        let mut mock_certificate_handler = MockCertificateHandler::new();
        mock_certificate_handler
            .expect_retrieve_pending_certificate()
            .return_const(Ok(None))
            .once();

        let mut signer = Signer::new(party_id, signing_key, Box::new(mock_certificate_handler));
        signer.run();
    }

    #[test]
    fn signer_sign_when_triggered_by_pending_certificate() {
        let party_id = "";
        let signing_key = "";
        let mut mock_certificate_handler = MockCertificateHandler::new();
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

        let mut signer = Signer::new(party_id, signing_key, Box::new(mock_certificate_handler));
        signer.run();
        signer.run();
    }

    #[test]
    fn signer_sign_only_once_if_pending_certificate_has_not_changed() {
        let party_id = "";
        let signing_key = "";
        let mut mock_certificate_handler = MockCertificateHandler::new();
        let pending_certificate = fake_data::certificate_pending();
        mock_certificate_handler
            .expect_retrieve_pending_certificate()
            .return_const(Ok(Some(pending_certificate)))
            .times(2);
        mock_certificate_handler
            .expect_register_signatures()
            .return_const(Ok(()))
            .once();

        let mut signer = Signer::new(party_id, signing_key, Box::new(mock_certificate_handler));
        signer.run();
        signer.run();
    }
}
