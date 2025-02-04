mod extensions;

use crate::extensions::fake::{FakeAggregator, FakeCertificateVerifier};
use mithril_client::aggregator_client::AggregatorRequest;
use mithril_client::cardano_database_client::{DownloadUnpackOptions, ImmutableFileRange};
use mithril_client::feedback::SlogFeedbackReceiver;
use mithril_client::{ClientBuilder, MessageBuilder};
use mithril_common::digesters::DummyCardanoDbBuilder;
use std::sync::Arc;

#[tokio::test]
async fn cardano_database_list_get_show_download_verify() {
    let work_dir = extensions::get_test_dir("cardano_database_list_get_show_download_verify");
    let genesis_verification_key =
        mithril_common::test_utils::fake_keys::genesis_verification_key()[0];
    let hash = "hash";
    let certificate_hash = "certificate_hash";
    let cardano_db = DummyCardanoDbBuilder::new("cardano_database_list_get_show_download_verify")
        .with_immutables(&[1, 2, 3, 4, 5, 6])
        .append_immutable_trio()
        .build();
    let fake_aggregator = FakeAggregator::new();
    let test_http_server = fake_aggregator
        .spawn_with_snapshot(hash, certificate_hash, &cardano_db, &work_dir)
        .await;
    let client = ClientBuilder::aggregator(&test_http_server.url(), genesis_verification_key)
        .with_certificate_verifier(FakeCertificateVerifier::build_that_validate_any_certificate())
        .add_feedback_receiver(Arc::new(SlogFeedbackReceiver::new(
            extensions::test_logger(),
        )))
        .build()
        .expect("Should be able to create a Client");

    let cardano_database_snapshots = client
        .cardano_database()
        .list()
        .await
        .expect("List Cardano database snapshots should not fail");
    assert_eq!(
        fake_aggregator.get_last_call().await,
        Some(format!(
            "/{}",
            AggregatorRequest::ListCardanoDatabaseSnapshots.route()
        ))
    );

    let latest_hash = cardano_database_snapshots.first().unwrap().hash.as_ref();

    let cardano_database_snapshot = client
        .cardano_database()
        .get(latest_hash)
        .await
        .expect("Get Snapshot should not fail ")
        .unwrap_or_else(|| panic!("A Snapshot should exist for hash '{latest_hash}'"));
    assert_eq!(
        fake_aggregator.get_last_call().await,
        Some(format!(
            "/{}",
            AggregatorRequest::GetSnapshot {
                digest: (latest_hash.to_string())
            }
            .route()
        ))
    );

    let unpacked_dir = work_dir.join("unpack");
    std::fs::create_dir(&unpacked_dir).unwrap();

    let certificate = client
        .certificate()
        .verify_chain(&cardano_database_snapshot.certificate_hash)
        .await
        .expect("Validating the chain should not fail");
    assert_eq!(
        fake_aggregator.get_last_call().await,
        Some(format!(
            "/{}",
            AggregatorRequest::GetCertificate {
                hash: (cardano_database_snapshot.certificate_hash.clone())
            }
            .route()
        ))
    );

    let immutable_file_range = ImmutableFileRange::Range(2, 4);
    let download_unpack_options = DownloadUnpackOptions {
        allow_override: false,
        include_ancillary: true,
    };
    client
        .cardano_database()
        .download_unpack(
            &cardano_database_snapshot,
            &immutable_file_range,
            &unpacked_dir,
            download_unpack_options,
        )
        .await
        .expect("download/unpack Cardano database should not fail");
    assert_eq!(
        fake_aggregator.get_last_call().await,
        Some(format!(
            "/{}/download",
            AggregatorRequest::GetCardanoDatabaseSnapshot {
                hash: (cardano_database_snapshot.hash.clone())
            }
            .route()
        ))
    );

    /* client
        .cardano_database()
        .add_statistics(&cardano_database_snapshot)
        .await
        .expect("add_statistics should not fail");
    assert_eq!(
        fake_aggregator.get_last_call().await,
        Some(format!(
            "/{}",
            AggregatorRequest::IncrementSnapshotStatistic {
                snapshot: "whatever".to_string()
            }
            .route()
        ))
    ); */

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
