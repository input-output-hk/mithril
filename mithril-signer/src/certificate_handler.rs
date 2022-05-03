use mithril_aggregator::entities::CertificatePending;
#[cfg(test)]
use mockall::automock;

#[cfg_attr(test, automock)]
pub trait CertificateHandler {
    fn retrieve_pending_certificate(&self) -> Result<Option<CertificatePending>, String>;

    fn register_signatures(&self, signature: &str) -> Result<(), String>;
}
