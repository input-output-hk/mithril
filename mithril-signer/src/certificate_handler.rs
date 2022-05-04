use async_trait::async_trait;
use mithril_aggregator::entities::CertificatePending;
#[cfg(test)]
use mockall::automock;

#[cfg_attr(test, automock)]
#[async_trait]
pub trait CertificateHandler {
    async fn retrieve_pending_certificate(&self) -> Result<Option<CertificatePending>, String>;

    async fn register_signatures(&self, signature: &str) -> Result<(), String>;
}

pub struct CertificateHandlerNoOp {}

#[async_trait]
impl CertificateHandler for CertificateHandlerNoOp {
    async fn retrieve_pending_certificate(&self) -> Result<Option<CertificatePending>, String> {
        unimplemented!()
    }

    async fn register_signatures(&self, _signature: &str) -> Result<(), String> {
        unimplemented!()
    }
}
