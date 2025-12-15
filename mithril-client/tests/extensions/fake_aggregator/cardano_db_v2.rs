use std::ops::RangeInclusive;
use std::path::Path;
use std::sync::Arc;

use mithril_cardano_node_internal_database::digesters::{
    CardanoImmutableDigester, ImmutableDigester,
};
use mithril_cardano_node_internal_database::test::DummyCardanoDb;
use mithril_client::common::{
    AncillaryLocation, AncillaryMessagePart, CardanoDbBeacon, CompressionAlgorithm, DigestLocation,
    DigestsMessagePart, ImmutableFileNumber, ImmutablesLocation, ImmutablesMessagePart,
    MultiFilesUri, ProtocolMessagePartKey, TemplateUri,
};
use mithril_client::{
    CardanoDatabaseSnapshot, CardanoDatabaseSnapshotListItem, MithrilCertificate,
};
use mithril_common::crypto_helper::ManifestVerifierSecretKey;
use mithril_common::test::double::{Dummy, fake_data};
use tokio::sync::RwLock;

use super::FakeAggregator;
use crate::extensions::{routes, snapshot_archives};

pub struct CardanoDatabaseSnapshotV2Fixture<'a> {
    pub snapshot_hash: &'a str,
    pub certificate_hash: &'a str,
    pub range: RangeInclusive<ImmutableFileNumber>,
    pub ancillary_manifest_signing_key: ManifestVerifierSecretKey,
}

impl FakeAggregator {
    pub async fn spawn_with_cardano_db_snapshot(
        fixture: CardanoDatabaseSnapshotV2Fixture<'_>,
        cardano_db: &DummyCardanoDb,
        work_dir: &Path,
        digester: CardanoImmutableDigester,
    ) -> Self {
        let beacon = CardanoDbBeacon {
            immutable_file_number: cardano_db.last_immutable_number().unwrap(),
            ..fake_data::beacon()
        };

        let cardano_db_snapshot = Arc::new(RwLock::new(CardanoDatabaseSnapshot {
            hash: fixture.certificate_hash.to_string(),
            certificate_hash: fixture.certificate_hash.to_string(),
            beacon: beacon.clone(),
            ..CardanoDatabaseSnapshot::dummy()
        }));
        let cardano_db_snapshot_clone = cardano_db_snapshot.clone();

        let cardano_db_snapshot_list = vec![
            CardanoDatabaseSnapshotListItem {
                hash: fixture.snapshot_hash.to_string(),
                certificate_hash: fixture.certificate_hash.to_string(),
                beacon: beacon.clone(),
                ..CardanoDatabaseSnapshotListItem::dummy()
            },
            CardanoDatabaseSnapshotListItem::dummy(),
        ];

        let certificate = {
            let mut cert = MithrilCertificate::dummy();
            let merkle_tree = digester
                .compute_merkle_tree(cardano_db.get_immutable_dir(), &beacon)
                .await
                .unwrap();
            let merkle_root = merkle_tree.compute_root().unwrap().to_hex();

            cert.protocol_message.set_message_part(
                ProtocolMessagePartKey::CardanoDatabaseMerkleRoot,
                merkle_root,
            );
            cert.signed_message = cert.protocol_message.compute_hash();

            cert
        };

        let computed_immutables_digests = digester
            .compute_digests_for_range(cardano_db.get_immutable_dir(), &fixture.range)
            .await
            .unwrap();
        let cardano_db_snapshot_archives_path =
            snapshot_archives::build_cardano_db_v2_snapshot_archives(
                cardano_db,
                work_dir,
                computed_immutables_digests,
                fixture.ancillary_manifest_signing_key,
            )
            .await;

        let routes = routes::cardano_db_snapshot::routes(
            cardano_db_snapshot_list,
            cardano_db_snapshot_clone,
            &cardano_db_snapshot_archives_path,
        )
        .merge(routes::certificate::routes(Vec::new(), certificate))
        .merge(routes::statistics::routes());

        let fake_aggregator = Self::spawn_test_server_on_random_port(routes);
        fake_aggregator
            .update_cardano_db_snapshot_locations(cardano_db_snapshot)
            .await;

        fake_aggregator
    }

    async fn update_cardano_db_snapshot_locations(
        &self,
        cardano_db_snapshot: Arc<RwLock<CardanoDatabaseSnapshot>>,
    ) {
        let base_url = self.server_url("/cardano-database-download");
        let immutables_locations = vec![ImmutablesLocation::CloudStorage {
            uri: MultiFilesUri::Template(TemplateUri(format!(
                "{base_url}/{{immutable_file_number}}.tar.zst"
            ))),
            compression_algorithm: Some(CompressionAlgorithm::Zstandard),
        }];
        let ancillary_locations = vec![AncillaryLocation::CloudStorage {
            uri: format!("{base_url}/ancillary.tar.zst"),
            compression_algorithm: Some(CompressionAlgorithm::Zstandard),
        }];
        let digest_locations = vec![DigestLocation::CloudStorage {
            uri: format!("{base_url}/digests.json"),
            compression_algorithm: None,
        }];

        let mut cardano_db_snapshot_to_update = cardano_db_snapshot.write().await;
        *cardano_db_snapshot_to_update = CardanoDatabaseSnapshot {
            immutables: ImmutablesMessagePart {
                locations: immutables_locations,
                ..cardano_db_snapshot_to_update.immutables.clone()
            },
            ancillary: AncillaryMessagePart {
                locations: ancillary_locations,
                ..cardano_db_snapshot_to_update.ancillary.clone()
            },
            digests: DigestsMessagePart {
                locations: digest_locations,
                ..cardano_db_snapshot_to_update.digests.clone()
            },
            ..cardano_db_snapshot_to_update.clone()
        };
    }
}
