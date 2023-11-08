mod extensions;

use crate::extensions::fake::{FakeAggregator, FakeCertificateVerifier};
use mithril_client::feedback::SlogFeedbackReceiver;
use mithril_client::{ClientBuilder, MessageBuilder};
use mithril_common::digesters::DummyImmutablesDbBuilder;
use std::sync::Arc;

#[tokio::test]
async fn snapshot_list_get_show_download_verify() {
    let work_dir = extensions::get_test_dir("snapshot_list_get_show_download_verify");
    let genesis_verification_key =
        mithril_common::test_utils::fake_keys::genesis_verification_key()[0];
    let digest = "snapshot_digest";
    let certificate_hash = "certificate_hash";
    let immutable_db = DummyImmutablesDbBuilder::new("snapshot_list_get_show_download_verify_db")
        .with_immutables(&[1, 2, 3])
        .append_immutable_trio()
        .build();
    let fake_aggregator =
        FakeAggregator::spawn_with_snapshot(digest, certificate_hash, &immutable_db, &work_dir)
            .await;
    let client = ClientBuilder::aggregator(&fake_aggregator.url(), genesis_verification_key)
        .with_certificate_verifier(FakeCertificateVerifier::build_that_validate_any_certificate())
        .add_feedback_receiver(Arc::new(SlogFeedbackReceiver::new(
            extensions::test_logger(),
        )))
        .build()
        .expect("Should be able to create a Client");

    let snapshots = client
        .snapshot()
        .list()
        .await
        .expect("List MithrilStakeDistribution should not fail");

    let last_digest = snapshots.first().unwrap().digest.as_ref();

    let snapshot = client
        .snapshot()
        .get(last_digest)
        .await
        .expect("Get Snapshot should not fail ")
        .unwrap_or_else(|| panic!("A Snapshot should exist for hash '{last_digest}'"));

    let unpacked_dir = work_dir.join("unpack");
    std::fs::create_dir(&unpacked_dir).unwrap();

    client
        .snapshot()
        .download_unpack(&snapshot, &unpacked_dir)
        .await
        .expect("download/unpack snapshot should not fail");

    let certificate = client
        .certificate()
        .verify_chain(&snapshot.certificate_hash)
        .await
        .expect("Validating the chain should not fail");

    let message = MessageBuilder::new()
        .compute_snapshot_message(&certificate, &unpacked_dir)
        .await
        .expect("Computing snapshot message should not fail");

    assert!(
        certificate.match_message(&message),
        "Certificate and message did not match:\ncertificate_message: '{}'\n computed_message: '{}'",
        certificate.signed_message,
        message.compute_hash()
    );
}
