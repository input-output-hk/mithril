use crate::aggregator_client::AggregatorClient;
use mithril_common::certificate_chain::CertificateVerifier;
use reqwest::IntoUrl;
use std::sync::Arc;

pub struct ClientBuilder {}

impl ClientBuilder {
    // note: easy alternative to `new().with_aggregator_client(..)`
    pub fn aggregator<T: IntoUrl>(url: T) -> ClientBuilder {
        todo!()
    }

    // note: easy alternative to `with_certificate_verifier(..)`
    fn genesis_verification_key(&mut self, genesis_verification_key: &str) -> &mut ClientBuilder {
        todo!()
    }

    pub fn new() -> ClientBuilder {
        todo!()
    }

    fn with_aggregator_client(
        &mut self,
        aggregator_client: Arc<dyn AggregatorClient>,
    ) -> &mut ClientBuilder {
        todo!()
    }

    fn with_certificate_verifier(
        &mut self,
        certificate_verifier: Arc<dyn CertificateVerifier>,
    ) -> &mut ClientBuilder {
        todo!()
    }
}
