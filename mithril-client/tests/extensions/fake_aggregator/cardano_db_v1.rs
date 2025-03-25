use std::path::Path;
use std::sync::{Arc, RwLock};
use warp::Filter;

use mithril_client::{MessageBuilder, MithrilCertificate, Snapshot, SnapshotListItem};
use mithril_common::digesters::DummyCardanoDb;
use mithril_common::entities::{CardanoDbBeacon, CompressionAlgorithm, SignedEntityType};
use mithril_common::test_utils::fake_data;
use mithril_common::test_utils::test_http_server::{test_http_server, TestHttpServer};

use crate::extensions::{routes, snapshot_archives};

use super::FakeAggregator;

impl FakeAggregator {
    pub async fn spawn_with_snapshot(
        &self,
        snapshot_digest: &str,
        certificate_hash: &str,
        cardano_db: &DummyCardanoDb,
        work_dir: &Path,
    ) -> TestHttpServer {
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

        let snapshot_list_json = serde_json::to_string(&vec![
            SnapshotListItem {
                digest: snapshot_digest.to_string(),
                certificate_hash: certificate_hash.to_string(),
                beacon: beacon.clone(),
                compression_algorithm: CompressionAlgorithm::Zstandard,
                ..SnapshotListItem::dummy()
            },
            SnapshotListItem::dummy(),
        ])
        .unwrap();

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
        let certificate_json = serde_json::to_string(&certificate).unwrap();

        let routes =
            routes::snapshot::routes(self.calls.clone(), snapshot_list_json, snapshot_clone)
                .or(routes::certificate::routes(
                    self.calls.clone(),
                    None,
                    certificate_json,
                ))
                .or(routes::statistics::routes(self.calls.clone()));

        let cardano_db_archives =
            snapshot_archives::build_cardano_db_v1_snapshot_archives(cardano_db, work_dir);

        let routes = routes.or(routes::snapshot::download(
            self.calls.clone(),
            cardano_db_archives,
        ));
        let server = test_http_server(routes);

        update_snapshot_location(&server.url(), snapshot);

        server
    }
}

fn update_snapshot_location(aggregator_url: &str, snapshot: Arc<RwLock<Snapshot>>) {
    let mut snapshot_to_update = snapshot.write().unwrap();
    *snapshot_to_update = Snapshot {
        locations: vec![format!(
            "{aggregator_url}/snapshot_download/completed_immutables.tar.zst"
        )],
        ancillary_locations: Some(vec![format!(
            "{aggregator_url}/snapshot_download/ancillary.tar.zst"
        )]),
        ..snapshot_to_update.clone()
    };
}
