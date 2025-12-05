// TODO: Remove this allow once migration from deprecated AggregatorClient types is complete
#![allow(deprecated)]

mod extensions;

use mithril_client::{AggregatorDiscoveryType, ClientBuilder, GenesisVerificationKey};
use mithril_common::test::double::fake_keys;

use crate::extensions::fake_aggregator::FakeAggregator;

#[tokio::test]
async fn certificate_get_list() {
    let certificate_hash_list = vec!["certificate-123".to_string(), "certificate-456".to_string()];
    let genesis_verification_key = fake_keys::genesis_verification_key()[0];
    let fake_aggregator = FakeAggregator::spawn_with_certificate(&certificate_hash_list);
    let client = ClientBuilder::new(AggregatorDiscoveryType::Url(
        fake_aggregator.server_root_url(),
    ))
    .set_genesis_verification_key(GenesisVerificationKey::JsonHex(
        genesis_verification_key.to_string(),
    ))
    .build()
    .expect("Should be able to create a Client");

    let certificates = client
        .certificate()
        .list()
        .await
        .expect("List Certificate should not fail");
    assert_eq!(
        fake_aggregator.get_last_call().await,
        Some("/certificates".to_string())
    );

    let mut hashes: Vec<String> = certificates.into_iter().map(|c| c.hash).collect();
    hashes.sort();

    assert_eq!(certificate_hash_list, hashes);
}
