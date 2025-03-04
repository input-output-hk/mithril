mod extensions;

use std::sync::Arc;

use mithril_client::{
    aggregator_client::AggregatorRequest,
    cardano_database_client::{DownloadUnpackOptions, ImmutableFileRange},
    feedback::SlogFeedbackReceiver,
    ClientBuilder, MessageBuilder,
};
use mithril_common::digesters::{CardanoImmutableDigester, DummyCardanoDbBuilder};

use crate::extensions::fake::{FakeAggregator, FakeCertificateVerifier};

#[tokio::test]
async fn cardano_db_snapshot_list_get_download_verify() {
    let work_dir = extensions::get_test_dir("cardano_db_snapshot_list_get_download_verify");
    let genesis_verification_key =
        mithril_common::test_utils::fake_keys::genesis_verification_key()[0];
    let cardano_db_snapshot_hash = "cardano_db_snapshot_hash";
    let certificate_hash = "certificate_hash";
    let cardano_db = DummyCardanoDbBuilder::new("cardano_db_snapshot_list_get_download_verify_db")
        .with_immutables(&[1, 2, 3, 4])
        .append_immutable_trio()
        .with_ledger_files(&["blocks-0.dat", "blocks-1.dat"])
        .with_volatile_files(&["437", "537", "637"])
        .build();
    let digester =
        CardanoImmutableDigester::new("whatever".to_string(), None, extensions::test_logger());
    let range = 1..=4;
    let fake_aggregator = FakeAggregator::new();
    let test_http_server = fake_aggregator
        .spawn_with_cardano_db_snapshot(
            cardano_db_snapshot_hash,
            certificate_hash,
            &cardano_db,
            &work_dir,
            digester,
            range,
        )
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

    let last_hash = cardano_db_snapshots.first().unwrap().hash.as_ref();

    let cardano_db_snapshot = client
        .cardano_database()
        .get(last_hash)
        .await
        .expect("Get CardanoDatabaseSnapshot should not fail ")
        .unwrap_or_else(|| panic!("A CardanoDatabaseSnapshot should exist for hash '{last_hash}'"));

    let certificate = client
        .certificate()
        .verify_chain(&cardano_db_snapshot.certificate_hash)
        .await
        .expect("Validating the chain should not fail");

    let unpacked_dir = work_dir.join("unpack");
    std::fs::create_dir(&unpacked_dir).unwrap();
    let immutable_file_range = ImmutableFileRange::From(2);
    let download_unpack_options = DownloadUnpackOptions {
        include_ancillary: true,
        ..DownloadUnpackOptions::default()
    };

    client
        .cardano_database()
        .download_unpack(
            &cardano_db_snapshot,
            &immutable_file_range,
            &unpacked_dir,
            download_unpack_options,
        )
        .await
        .expect("download/unpack cardano db snapshot should not fail");

    let full_restoration = immutable_file_range == ImmutableFileRange::Full;
    let include_ancillary = download_unpack_options.include_ancillary;
    let number_of_immutable_files_restored =
        immutable_file_range.length(cardano_db_snapshot.beacon.immutable_file_number);
    client
        .cardano_database()
        .add_statistics(
            full_restoration,
            include_ancillary,
            number_of_immutable_files_restored,
        )
        .await
        .expect("add_statistics should not fail");
    let last_api_calls = fake_aggregator.get_latest_calls(3).await;
    assert!(last_api_calls.contains(&format!(
        "/{}",
        AggregatorRequest::IncrementCardanoDatabaseAncillaryStatistic.route()
    )));
    assert!(last_api_calls.contains(&format!(
        "/{}",
        AggregatorRequest::IncrementCardanoDatabasePartialRestorationStatistic.route()
    )));
    assert!(last_api_calls.contains(&format!(
        "/{}",
        AggregatorRequest::IncrementCardanoDatabaseImmutablesRestoredStatistic {
            number_of_immutables: number_of_immutable_files_restored.to_string()
        }
        .route()
    )));

    let merkle_proof = client
        .cardano_database()
        .compute_merkle_proof(
            &certificate,
            &cardano_db_snapshot,
            &immutable_file_range,
            &unpacked_dir,
        )
        .await
        .expect("Computing merkle proof should not fail");
    merkle_proof.verify().expect("Merkle proof should be valid");

    let message = MessageBuilder::new()
        .compute_cardano_database_message(&certificate, &merkle_proof)
        .await
        .expect("Computing cardano database snapshot message should not fail");

    assert!(
            certificate.match_message(&message),
            "Certificate and message did not match:\ncertificate_message: '{}'\n computed_message: '{}'",
            certificate.signed_message,
            message.compute_hash()
    );
}
