use mithril_aggregator::entities::CertificatePending;

#[cfg(test)]
use mockall::automock;

#[cfg_attr(test, automock)]
pub trait CertificateHandler {
    fn retrieve_pending_certificate(&self) -> Result<Option<CertificatePending>, String>;

    fn register_signatures(&self, signature: &str) -> Result<(), String>;
}

struct Signer {
    certificate_handler: Box<dyn CertificateHandler>,
}

impl Signer {
    fn new(
        _party_id: &str,
        _signing_key: &str,
        certificate_handler: Box<dyn CertificateHandler>,
    ) -> Self {
        Self {
            certificate_handler,
        }
    }

    fn run(&self) {
        if let Some(_certificate) = self
            .certificate_handler
            .retrieve_pending_certificate()
            .unwrap()
        {
            self.certificate_handler.register_signatures("");
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use mithril_aggregator::fake_data;

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

        let signer = Signer::new(party_id, signing_key, Box::new(mock_certificate_handler));
        signer.run();
        signer.run();
    }
}
