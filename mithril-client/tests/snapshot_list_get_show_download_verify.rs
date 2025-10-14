// TODO: Remove this allow once migration from deprecated AggregatorClient types is complete
#![allow(deprecated)]

mod extensions;

use std::sync::Arc;

use mithril_cardano_node_internal_database::test::DummyCardanoDbBuilder;
use mithril_client::aggregator_client::AggregatorRequest;
use mithril_client::feedback::SlogFeedbackReceiver;
use mithril_client::{ClientBuilder, MessageBuilder};
use mithril_common::crypto_helper::ManifestSigner;
use mithril_common::test::double::fake_keys;

use crate::extensions::fake_aggregator::{FakeAggregator, FakeCertificateVerifier};

#[tokio::test]
async fn snapshot_list_get_show_download_verify() {
    let work_dir = extensions::get_test_dir("snapshot_list_get_show_download_verify");
    let genesis_verification_key = fake_keys::genesis_verification_key()[0];
    let (ancillary_manifest_signing_key, ancillary_manifest_signer_verification_key) = {
        let signer = ManifestSigner::create_deterministic_signer();
        (signer.secret_key(), signer.verification_key())
    };
    let digest = "snapshot_digest";
    let certificate_hash = "certificate_hash";
    let cardano_db = DummyCardanoDbBuilder::new("snapshot_list_get_show_download_verify_db")
        .with_immutables(&[1, 2, 3])
        .append_immutable_trio()
        .with_legacy_ledger_snapshots(&[506, 562])
        .build();
    let fake_aggregator = FakeAggregator::spawn_with_snapshot(
        digest,
        certificate_hash,
        &cardano_db,
        &work_dir,
        ancillary_manifest_signing_key,
    )
    .await;
    let client =
        ClientBuilder::aggregator(&fake_aggregator.server_root_url(), genesis_verification_key)
            .set_ancillary_verification_key(
                ancillary_manifest_signer_verification_key.to_json_hex().unwrap(),
            )
            .with_certificate_verifier(
                FakeCertificateVerifier::build_that_validate_any_certificate(),
            )
            .add_feedback_receiver(Arc::new(SlogFeedbackReceiver::new(
                extensions::test_logger(),
            )))
            .with_logger(extensions::test_logger())
            .build()
            .expect("Should be able to create a Client");

    let snapshots = client
        .cardano_database()
        .list()
        .await
        .expect("List MithrilStakeDistribution should not fail");
    assert_eq!(
        fake_aggregator.get_last_call().await,
        Some(format!("/{}", AggregatorRequest::ListSnapshots.route()))
    );

    let last_digest = snapshots.first().unwrap().digest.as_ref();

    let snapshot = client
        .cardano_database()
        .get(last_digest)
        .await
        .expect("Get Snapshot should not fail ")
        .unwrap_or_else(|| panic!("A Snapshot should exist for hash '{last_digest}'"));
    assert_eq!(
        fake_aggregator.get_last_call().await,
        Some(format!(
            "/{}",
            AggregatorRequest::GetSnapshot {
                digest: (last_digest.to_string())
            }
            .route()
        ))
    );

    let unpacked_dir = work_dir.join("unpack");
    std::fs::create_dir(&unpacked_dir).unwrap();

    let certificate = client
        .certificate()
        .verify_chain(&snapshot.certificate_hash)
        .await
        .expect("Validating the chain should not fail");
    assert_eq!(
        fake_aggregator.get_last_call().await,
        Some(format!(
            "/{}",
            AggregatorRequest::GetCertificate {
                hash: (snapshot.certificate_hash.clone())
            }
            .route()
        ))
    );

    client
        .cardano_database()
        .download_unpack_full(&snapshot, &unpacked_dir)
        .await
        .expect("download/unpack snapshot should not fail");

    client
        .cardano_database()
        .add_statistics(&snapshot)
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
    );

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
