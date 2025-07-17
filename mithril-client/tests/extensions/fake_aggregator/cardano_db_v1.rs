use std::path::Path;
use std::sync::Arc;

use tokio::sync::RwLock;

use mithril_cardano_node_internal_database::test::DummyCardanoDb;
use mithril_client::{MessageBuilder, MithrilCertificate, Snapshot, SnapshotListItem};
use mithril_common::crypto_helper::ManifestVerifierSecretKey;
use mithril_common::entities::{CardanoDbBeacon, CompressionAlgorithm, SignedEntityType};
use mithril_common::test_utils::{double::Dummy, fake_data};

use crate::extensions::{routes, snapshot_archives};

use super::FakeAggregator;

impl FakeAggregator {
    pub async fn spawn_with_snapshot(
        snapshot_digest: &str,
        certificate_hash: &str,
        cardano_db: &DummyCardanoDb,
        work_dir: &Path,
        ancillary_manifest_signing_key: ManifestVerifierSecretKey,
    ) -> Self {
        let beacon = CardanoDbBeacon {
            immutable_file_number: cardano_db.last_immutable_number().unwrap(),
            ..fake_data::beacon()
        };

        let snapshot = Arc::new(RwLock::new(Snapshot {
            digest: snapshot_digest.to_string(),
            certificate_hash: certificate_hash.to_string(),
            beacon: beacon.clone(),
            compression_algorithm: CompressionAlgorithm::Zstandard,
            ..Snapshot::dummy()
        }));
        let snapshot_clone = snapshot.clone();

        let snapshot_list = vec![
            SnapshotListItem {
                digest: snapshot_digest.to_string(),
                certificate_hash: certificate_hash.to_string(),
                beacon: beacon.clone(),
                compression_algorithm: CompressionAlgorithm::Zstandard,
                ..SnapshotListItem::dummy()
            },
            SnapshotListItem::dummy(),
        ];

        let mut certificate = MithrilCertificate {
            hash: certificate_hash.to_string(),
            epoch: beacon.epoch,
            signed_entity_type: SignedEntityType::CardanoImmutableFilesFull(beacon.clone()),
            ..MithrilCertificate::dummy()
        };
        certificate.signed_message = MessageBuilder::new()
            .compute_snapshot_message(&certificate, cardano_db.get_immutable_dir())
            .await
            .expect("Computing snapshot message should not fail")
            .compute_hash();

        let snapshots_dir = snapshot_archives::build_cardano_db_v1_snapshot_archives(
            cardano_db,
            work_dir,
            ancillary_manifest_signing_key,
        )
        .await;

        let routes = routes::snapshot::routes(snapshot_list, snapshot_clone, &snapshots_dir)
            .merge(routes::certificate::routes(Vec::new(), certificate))
            .merge(routes::statistics::routes());
        let fake_aggregator = Self::spawn_test_server_on_random_port(routes);
        fake_aggregator.update_snapshot_location(snapshot).await;

        fake_aggregator
    }

    async fn update_snapshot_location(&self, snapshot: Arc<RwLock<Snapshot>>) {
        let mut snapshot_to_update = snapshot.write().await;
        *snapshot_to_update = Snapshot {
            locations: vec![self.server_url("/snapshot_download/completed_immutables.tar.zst")],
            ancillary_locations: Some(vec![
                self.server_url("/snapshot_download/ancillary.tar.zst"),
            ]),
            ..snapshot_to_update.clone()
        };
    }
}
