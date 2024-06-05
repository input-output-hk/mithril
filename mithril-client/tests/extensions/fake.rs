use super::routes;
use crate::extensions::mock;
use mithril_client::certificate_client::CertificateVerifier;
use mithril_client::{
    MessageBuilder, MithrilCertificate, MithrilCertificateListItem, MithrilStakeDistribution,
    MithrilStakeDistributionListItem,
};
use mithril_common::test_utils::test_http_server::{test_http_server, TestHttpServer};
use std::convert::Infallible;
use std::sync::Arc;
use tokio::sync::Mutex;
use warp::filters::path::FullPath;
use warp::Filter;

pub type FakeAggregatorCalls = Arc<Mutex<Vec<String>>>;

pub struct FakeCertificateVerifier;

impl FakeCertificateVerifier {
    pub fn build_that_validate_any_certificate() -> Arc<dyn CertificateVerifier> {
        let mut mock_verifier = mock::MockCertificateVerifierImpl::new();
        mock_verifier.expect_verify_chain().returning(|_| Ok(()));

        Arc::new(mock_verifier)
    }
}

pub struct FakeAggregator {
    calls: FakeAggregatorCalls,
}

impl FakeAggregator {
    pub fn new() -> Self {
        FakeAggregator {
            calls: Arc::new(Mutex::new(vec![])),
        }
    }

    async fn get_calls(&self) -> Vec<String> {
        let calls = self.calls.lock().await;

        calls.clone()
    }

    pub async fn get_last_call(&self) -> Option<String> {
        self.get_calls().await.last().cloned()
    }

    pub async fn store_call_and_return_value(
        full_path: FullPath,
        calls: FakeAggregatorCalls,
        returned_value: String,
    ) -> Result<impl warp::Reply, Infallible> {
        let mut call_list = calls.lock().await;
        call_list.push(full_path.as_str().to_string());

        Ok(returned_value)
    }

    pub async fn store_call_with_query_and_return_value(
        full_path: FullPath,
        query: String,
        calls: FakeAggregatorCalls,
        returned_value: String,
    ) -> Result<impl warp::Reply, Infallible> {
        let mut call_list = calls.lock().await;
        call_list.push(format!("{}?{}", full_path.as_str(), query));

        Ok(returned_value)
    }

    pub fn spawn_with_mithril_stake_distribution(
        &self,
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

        let certificate_json = serde_json::to_string(&MithrilCertificate {
            hash: certificate_hash.to_string(),
            signed_message: message.compute_hash(),
            ..MithrilCertificate::dummy()
        })
        .unwrap();

        test_http_server(
            routes::mithril_stake_distribution::routes(
                self.calls.clone(),
                mithril_stake_distribution_list_json,
                mithril_stake_distribution_json,
            )
            .or(routes::certificate::routes(
                self.calls.clone(),
                None,
                certificate_json,
            )),
        )
    }

    pub fn spawn_with_certificate(&self, certificate_hash_list: &[String]) -> TestHttpServer {
        let certificate_json = serde_json::to_string(&MithrilCertificate {
            hash: certificate_hash_list[0].to_string(),
            ..MithrilCertificate::dummy()
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

        test_http_server(routes::certificate::routes(
            self.calls.clone(),
            Some(certificate_list_json),
            certificate_json,
        ))
    }
}

#[cfg(feature = "unstable")]
mod proof {
    use super::*;
    use mithril_client::common::ProtocolMessagePartKey;
    use mithril_client::{CardanoTransactionsProofs, CardanoTransactionsSetProof};
    use mithril_common::crypto_helper::{MKProof, ProtocolMkProof};

    impl FakeAggregator {
        pub fn spawn_with_transactions_proofs(
            &self,
            tx_hashes: &[&str],
            certificate_hash: &str,
        ) -> TestHttpServer {
            let proof = MKProof::from_leaves(tx_hashes).unwrap();

            let proofs_json = serde_json::to_string(&CardanoTransactionsProofs {
                certificate_hash: certificate_hash.to_string(),
                certified_transactions: vec![CardanoTransactionsSetProof {
                    transactions_hashes: tx_hashes.iter().map(|h| h.to_string()).collect(),
                    proof: ProtocolMkProof::new(proof.to_owned().into())
                        .to_json_hex()
                        .unwrap(),
                }],
                non_certified_transactions: vec![],
                latest_block_number: 9999,
            })
            .unwrap();

            let certificate = {
                let mut cert = MithrilCertificate {
                    hash: certificate_hash.to_string(),
                    ..MithrilCertificate::dummy()
                };
                cert.protocol_message.set_message_part(
                    ProtocolMessagePartKey::CardanoTransactionsMerkleRoot,
                    proof.root().to_hex(),
                );
                cert.protocol_message
                    .set_message_part(ProtocolMessagePartKey::LatestBlockNumber, 9999.to_string());
                cert.signed_message = cert.protocol_message.compute_hash();
                cert
            };
            let certificate_json = serde_json::to_string(&certificate).unwrap();

            test_http_server(routes::proof::routes(self.calls.clone(), proofs_json).or(
                routes::certificate::routes(self.calls.clone(), None, certificate_json),
            ))
        }
    }
}

#[cfg(feature = "fs")]
mod file {
    use super::*;
    use mithril_client::{MessageBuilder, Snapshot, SnapshotListItem};
    use mithril_common::digesters::DummyImmutableDb;
    use mithril_common::entities::{CardanoDbBeacon, CompressionAlgorithm, SignedEntityType};
    use mithril_common::test_utils::fake_data;
    use mithril_common::test_utils::test_http_server::{test_http_server, TestHttpServer};
    use std::path::{Path, PathBuf};
    use std::sync::{Arc, RwLock};
    use warp::Filter;

    impl FakeAggregator {
        #[cfg(feature = "fs")]
        pub async fn spawn_with_snapshot(
            &self,
            snapshot_digest: &str,
            certificate_hash: &str,
            immutable_db: &DummyImmutableDb,
            work_dir: &Path,
        ) -> TestHttpServer {
            let beacon = CardanoDbBeacon {
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

            let mut certificate = MithrilCertificate {
                hash: certificate_hash.to_string(),
                epoch: beacon.epoch,
                signed_entity_type: SignedEntityType::CardanoImmutableFilesFull(beacon),
                ..MithrilCertificate::dummy()
            };
            certificate.signed_message = MessageBuilder::new()
                .compute_snapshot_message(&certificate, &immutable_db.dir)
                .await
                .expect("Computing snapshot message should not fail")
                .compute_hash();
            let certificate_json = serde_json::to_string(&certificate).unwrap();

            let routes =
                routes::snapshot::routes(self.calls.clone(), snapshot_list_json, snapshot_clone)
                    .or(routes::certificate::routes(
                        self.calls.clone(),
                        None,
                        certificate_json,
                    ))
                    .or(routes::statistics::routes(self.calls.clone()));

            let snapshot_archive_path = build_fake_zstd_snapshot(immutable_db, work_dir);

            let routes = routes.or(routes::snapshot::download(
                self.calls.clone(),
                snapshot_archive_path,
            ));
            let server = test_http_server(routes);

            update_snapshot_location(&server.url(), snapshot_digest, snapshot);

            server
        }
    }

    /// Compress the given db into an zstd archive in the given target directory.
    ///
    /// return the path to the compressed archive.
    pub fn build_fake_zstd_snapshot(immutable_db: &DummyImmutableDb, target_dir: &Path) -> PathBuf {
        use std::fs::File;

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
}
