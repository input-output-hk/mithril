mod extensions;

use crate::extensions::fake::FakeAggregator;
use mithril_client::{aggregator_client::AggregatorRequest, ClientBuilder};

#[tokio::test]
async fn certificate_get_list() {
    let certificate_hash_list = vec!["certificate-123".to_string(), "certificate-456".to_string()];
    let genesis_verification_key =
        mithril_common::test_utils::fake_keys::genesis_verification_key()[0];
    let fake_aggregator = FakeAggregator::new();
    let test_http_server = fake_aggregator.spawn_with_certificate(&certificate_hash_list);
    let client = ClientBuilder::aggregator(&test_http_server.url(), genesis_verification_key)
        .build()
        .expect("Should be able to create a Client");

    let certificates = client
        .certificate()
        .list()
        .await
        .expect("List Certificate should not fail");
    assert_eq!(
        fake_aggregator.get_last_call().await,
        Some(format!("/{}", AggregatorRequest::ListCertificates.route()))
    );

    let mut hashes: Vec<String> = certificates.into_iter().map(|c| c.hash).collect();
    hashes.sort();

    assert_eq!(certificate_hash_list, hashes);
}
