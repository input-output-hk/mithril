use mithril_client::certificate_client::CertificateVerifier;
use mithril_client::{
    MessageBuilder, MithrilCertificateListItem, MithrilStakeDistribution,
    MithrilStakeDistributionListItem, Snapshot, SnapshotListItem,
};
use mithril_common::digesters::DummyImmutableDb;
use mithril_common::entities::{Beacon, CompressionAlgorithm};
use mithril_common::messages::CertificateMessage;
use mithril_common::test_utils::fake_data;
use mithril_common::test_utils::test_http_server::{test_http_server, TestHttpServer};
use std::fs::File;
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};
use warp::Filter;

use crate::extensions::mock;

pub struct FakeCertificateVerifier;

impl FakeCertificateVerifier {
    pub fn build_that_validate_any_certificate() -> Arc<dyn CertificateVerifier> {
        let mut mock_verifier = mock::MockCertificateVerifierImpl::new();
        mock_verifier.expect_verify_chain().returning(|_| Ok(()));

        Arc::new(mock_verifier)
    }
}

pub struct FakeAggregator;

impl FakeAggregator {
    pub fn spawn_with_mithril_stake_distribution(
        msd_hash: &str,
        certificate_hash: &str,
    ) -> TestHttpServer {
        let mithril_stake_distribution = MithrilStakeDistribution {
            hash: msd_hash.to_string(),
            certificate_hash: certificate_hash.to_string(),
            ..MithrilStakeDistribution::dummy()
        };
        let mithril_stake_distribution_json =
            serde_json::to_string(&mithril_stake_distribution).unwrap();
        let mithril_stake_distribution_list_json = serde_json::to_string(&vec![
            MithrilStakeDistributionListItem {
                hash: msd_hash.to_string(),
                certificate_hash: certificate_hash.to_string(),
                ..MithrilStakeDistributionListItem::dummy()
            },
            MithrilStakeDistributionListItem::dummy(),
        ])
        .unwrap();

        let message = MessageBuilder::new()
            .compute_mithril_stake_distribution_message(&mithril_stake_distribution)
            .expect("Computing msd message should not fail");

        let certificate_json = serde_json::to_string(&CertificateMessage {
            hash: certificate_hash.to_string(),
            signed_message: message.compute_hash(),
            ..CertificateMessage::dummy()
        })
        .unwrap();

        test_http_server(
            warp::path!("artifact" / "mithril-stake-distributions")
                .map(move || mithril_stake_distribution_list_json.clone())
                .or(
                    warp::path!("artifact" / "mithril-stake-distribution" / String)
                        .map(move |_hash| mithril_stake_distribution_json.clone()),
                )
                .or(warp::path!("certificate" / String).map(move |_hash| certificate_json.clone())),
        )
    }

    pub async fn spawn_with_snapshot(
        snapshot_digest: &str,
        certificate_hash: &str,
        immutable_db: &DummyImmutableDb,
        work_dir: &Path,
    ) -> TestHttpServer {
        let beacon = Beacon {
            immutable_file_number: immutable_db.last_immutable_number().unwrap(),
            ..fake_data::beacon()
        };

        // Ugly horror needed to update the snapshot location after the server is started, server
        // which need said snapshot to start and run in another thread.
        // The RwLock is needed to mutate the value across threads and the Arc to transfer it
        // to the server thread while keeping an access on the main thread.
        let snapshot = Arc::new(RwLock::new(Snapshot {
            digest: snapshot_digest.to_string(),
            certificate_hash: certificate_hash.to_string(),
            beacon: beacon.clone(),
            compression_algorithm: Some(CompressionAlgorithm::Zstandard),
            ..Snapshot::dummy()
        }));
        let snapshot_clone = snapshot.clone();

        let snapshot_list_json = serde_json::to_string(&vec![
            SnapshotListItem {
                digest: snapshot_digest.to_string(),
                certificate_hash: certificate_hash.to_string(),
                beacon: beacon.clone(),
                compression_algorithm: Some(CompressionAlgorithm::Zstandard),
                ..SnapshotListItem::dummy()
            },
            SnapshotListItem::dummy(),
        ])
        .unwrap();

        let mut certificate = CertificateMessage {
            hash: certificate_hash.to_string(),
            beacon,
            ..CertificateMessage::dummy()
        };
        certificate.signed_message = MessageBuilder::new()
            .compute_snapshot_message(&certificate, &immutable_db.dir)
            .await
            .expect("Computing snapshot message should not fail")
            .compute_hash();
        let certificate_json = serde_json::to_string(&certificate).unwrap();

        let snapshot_archive_path = build_fake_zstd_snapshot(immutable_db, work_dir);

        let server = test_http_server(
            warp::path!("artifact" / "snapshots")
                .map(move || snapshot_list_json.clone())
                .or(
                    warp::path!("artifact" / "snapshot" / String).map(move |_digest| {
                        let data = snapshot_clone.read().unwrap();
                        serde_json::to_string(&data.clone()).unwrap()
                    }),
                )
                .or(warp::path!("artifact" / "snapshot" / String / "download")
                    .and(warp::fs::file(snapshot_archive_path))
                    .map(|_digest, reply: warp::fs::File| {
                        let filepath = reply.path().to_path_buf();
                        Box::new(warp::reply::with_header(
                            reply,
                            "Content-Disposition",
                            format!(
                                "attachment; filename=\"{}\"",
                                filepath.file_name().unwrap().to_str().unwrap()
                            ),
                        )) as Box<dyn warp::Reply>
                    }))
                .or(warp::path!("certificate" / String).map(move |_hash| certificate_json.clone())),
        );

        update_snapshot_location(&server.url(), snapshot_digest, snapshot);

        server
    }

    pub fn spawn_with_certificate(certificate_hash_list: &[String]) -> TestHttpServer {
        let certificate_json = serde_json::to_string(&CertificateMessage {
            hash: certificate_hash_list[0].to_string(),
            ..CertificateMessage::dummy()
        })
        .unwrap();
        let certificate_list_json = serde_json::to_string(
            &certificate_hash_list
                .iter()
                .map(|hash| MithrilCertificateListItem {
                    hash: hash.clone(),
                    ..MithrilCertificateListItem::dummy()
                })
                .collect::<Vec<_>>(),
        )
        .unwrap();

        test_http_server(
            warp::path!("certificates")
                .map(move || certificate_list_json.clone())
                .or(warp::path!("certificate" / String).map(move |_hash| certificate_json.clone())),
        )
    }
}

/// Compress the given db into an zstd archive in the given target directory.
///
/// return the path to the compressed archive.
pub fn build_fake_zstd_snapshot(immutable_db: &DummyImmutableDb, target_dir: &Path) -> PathBuf {
    let snapshot_name = format!(
        "db-i{}.{}",
        immutable_db.immutables_files.len(),
        CompressionAlgorithm::Zstandard.tar_file_extension()
    );
    let target_file = target_dir.join(snapshot_name);
    let tar_file = File::create(&target_file).unwrap();
    let enc = zstd::Encoder::new(tar_file, 3).unwrap();
    let mut tar = tar::Builder::new(enc);

    tar.append_dir_all(".", immutable_db.dir.parent().unwrap())
        .unwrap();

    let zstd = tar.into_inner().unwrap();
    zstd.finish().unwrap();

    target_file
}

fn update_snapshot_location(
    aggregator_url: &str,
    snapshot_digest: &str,
    snapshot: Arc<RwLock<Snapshot>>,
) {
    let snapshot_location =
        format!("{aggregator_url}/artifact/snapshot/{snapshot_digest}/download",);
    let mut snapshot_to_update = snapshot.write().unwrap();
    *snapshot_to_update = Snapshot {
        locations: vec![snapshot_location],
        ..snapshot_to_update.clone()
    };
}
