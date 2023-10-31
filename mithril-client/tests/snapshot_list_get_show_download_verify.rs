mod extensions;

use crate::extensions::mock;
use mithril_client::client::ClientBuilder;
use mithril_client::message::Message;
use mithril_client::{
    MithrilStakeDistribution, MithrilStakeDistributionListItem, Snapshot, SnapshotListItem,
};
use mithril_common::digesters::DummyImmutablesDbBuilder;
use mithril_common::messages::CertificateMessage;
use mithril_common::test_utils::test_http_server::{test_http_server, TestHttpServer};
use std::path::Path;
use std::sync::Arc;
use warp::Filter;

async fn spawn_fake_aggregator(
    snapshot_digest: &str,
    certificate_hash: &str,
    immutable_db: &Path,
) -> TestHttpServer {
    let snapshot = Snapshot {
        digest: snapshot_digest.to_string(),
        certificate_hash: certificate_hash.to_string(),
        ..Snapshot::dummy()
    };
    let snapshot_json = serde_json::to_string(&snapshot).unwrap();
    let snapshot_list_json = serde_json::to_string(&vec![
        SnapshotListItem {
            digest: snapshot_digest.to_string(),
            certificate_hash: certificate_hash.to_string(),
            ..SnapshotListItem::dummy()
        },
        SnapshotListItem::dummy(),
    ])
    .unwrap();

    let message = Message::compute_snapshot_message(&snapshot, immutable_db)
        .await
        .expect("Computing snapshot message should not fail");

    let certificate_json = serde_json::to_string(&CertificateMessage {
        hash: certificate_hash.to_string(),
        signed_message: message.compute_hash(),
        ..CertificateMessage::dummy()
    })
    .unwrap();

    test_http_server(
        warp::path!("artifact" / "snapshots")
            .map(move || snapshot_list_json.clone())
            .or(warp::path!("artifact" / "snapshot" / String)
                .map(move |_hash| snapshot_json.clone()))
            .or(warp::path!("certificate" / String).map(move |_hash| certificate_json.clone())),
    )
}

#[tokio::test]
async fn snapshot_list_get_show_download_verify() {
    let genesis_verification_key =
        mithril_common::test_utils::fake_keys::genesis_verification_key()[0];
    let digest = "snapshot_digest";
    let certificate_hash = "certificate_hash";
    let immutable_db = DummyImmutablesDbBuilder::new("snapshot_list_get_show_download_verify")
        .with_immutables(&[1, 2, 3])
        .build();
    let fake_aggregator = spawn_fake_aggregator(digest, certificate_hash, &immutable_db.dir).await;
    let mut mock_verifier = mock::MockCertificateVerifierImpl::new();
    mock_verifier
        .expect_verify_certificate_chain()
        .returning(|_, _| Ok(()))
        .once();
    let client = ClientBuilder::aggregator(&fake_aggregator.url(), genesis_verification_key)
        .with_certificate_verifier(Arc::new(mock_verifier))
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

    let certificate = client
        .certificate()
        .verify_chain(&snapshot.certificate_hash)
        .await
        .expect("Validating the chain should not fail");
    let unpacked_dir = &immutable_db.dir;

    let message = Message::compute_snapshot_message(&snapshot, unpacked_dir)
        .await
        .expect("Computing snapshot message should not fail");

    assert!(
        certificate.match_message(&message),
        "Certificate and message did not match:\ncertificate_message: '{}'\n computed_message: '{}'",
        certificate.signed_message,
        message.compute_hash()
    );
}
