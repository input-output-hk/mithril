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

#[cfg(test)]
mod tests {
    use super::*;
    use flate2::write::GzEncoder;
    use flate2::Compression;
    use httpmock::prelude::*;
    use serde_json::json;

    use mithril_aggregator::fake_data;

    fn setup_test() -> (MockServer, Config) {
        let server = MockServer::start();
        let config = Config {
            network: "testnet".to_string(),
            aggregator_endpoint: server.url(""),
        };
        (server, config)
    }

    #[tokio::test]
    async fn can_retrieve_pending_certificate_from_server() {
        // let (server, config) = setup_test();
        // let snapshots_expected = fake_data::snapshots(5);
        // let _snapshots_mock = server.mock(|when, then| {
        //     when.path("/snapshots");
        //     then.status(200).body(json!(snapshots_expected).to_string());
        // });
        // let aggregator_client =
        //     AggregatorHTTPClient::new(config.network, config.aggregator_endpoint);
        // let snapshots = aggregator_client.list_snapshots().await;
        // snapshots.as_ref().expect("unexpected error");
        // assert_eq!(snapshots.unwrap(), snapshots_expected);
        assert!(false);
    }
}
