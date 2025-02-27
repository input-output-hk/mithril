mod extensions;

use std::sync::Arc;

use mithril_client::{
    aggregator_client::AggregatorRequest, feedback::SlogFeedbackReceiver, ClientBuilder,
};
use mithril_common::digesters::DummyCardanoDbBuilder;

use crate::extensions::fake::{FakeAggregator, FakeCertificateVerifier};

#[tokio::test]
async fn cardano_db_snapshot_list_get_download_verify() {
    let work_dir = extensions::get_test_dir("cardano_db_snapshot_list_get_download_verify");
    let genesis_verification_key =
        mithril_common::test_utils::fake_keys::genesis_verification_key()[0];
    let snapshot_hash = "cardano_db_snapshot_hash";
    let certificate_hash = "certificate_hash";
    let cardano_db = DummyCardanoDbBuilder::new("cardano_db_snapshot_list_get_download_verify_db")
        .with_immutables(&[1, 2, 3])
        .append_immutable_trio()
        .build();
    let fake_aggregator = FakeAggregator::new();
    let test_http_server = fake_aggregator
        .spawn_with_cardano_db_snapshot(snapshot_hash, certificate_hash, &cardano_db, &work_dir)
        .await;
    let client = ClientBuilder::aggregator(&test_http_server.url(), genesis_verification_key)
        .with_certificate_verifier(FakeCertificateVerifier::build_that_validate_any_certificate())
        .add_feedback_receiver(Arc::new(SlogFeedbackReceiver::new(
            extensions::test_logger(),
        )))
        .build()
        .expect("Should be able to create a Client");

    let cardano_db_snapshots = client
        .cardano_database()
        .list()
        .await
        .expect("List CardanoDatabaseSnapshot should not fail");
    assert_eq!(
        fake_aggregator.get_last_call().await,
        Some(format!(
            "/{}",
            AggregatorRequest::ListCardanoDatabaseSnapshots.route()
        ))
    );

    let last_hash = cardano_db_snapshots.first().unwrap().hash.as_ref();

    let _cardano_db_snapshot = client
        .cardano_database()
        .get(last_hash)
        .await
        .expect("Get CardanoDatabaseSnapshot should not fail ")
        .unwrap_or_else(|| panic!("A CardanoDatabaseSnapshot should exist for hash '{last_hash}'"));
    assert_eq!(
        fake_aggregator.get_last_call().await,
        Some(format!(
            "/{}",
            AggregatorRequest::GetCardanoDatabaseSnapshot {
                hash: (last_hash.to_string())
            }
            .route()
        ))
    );
}
