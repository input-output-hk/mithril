use mithril_client::message::MessageBuilder;
use mithril_client::{
    MithrilStakeDistribution, MithrilStakeDistributionListItem, Snapshot, SnapshotListItem,
};
use mithril_common::certificate_chain::CertificateVerifier;
use mithril_common::digesters::DummyImmutableDb;
use mithril_common::entities::Beacon;
use mithril_common::messages::CertificateMessage;
use mithril_common::test_utils::fake_data;
use mithril_common::test_utils::test_http_server::{test_http_server, TestHttpServer};
use std::sync::Arc;
use warp::Filter;

use crate::extensions::mock;

pub struct FakeCertificateVerifier;

impl FakeCertificateVerifier {
    pub fn build_that_validate_any_certificate() -> Arc<dyn CertificateVerifier> {
        let mut mock_verifier = mock::MockCertificateVerifierImpl::new();
        mock_verifier
            .expect_verify_certificate_chain()
            .returning(|_, _| Ok(()));
        mock_verifier
            .expect_verify_certificate()
            .returning(|_, _| Ok(None));

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
    ) -> TestHttpServer {
        let beacon = Beacon {
            immutable_file_number: immutable_db.last_immutable_number().unwrap(),
            ..fake_data::beacon()
        };

        let snapshot_json = serde_json::to_string(&Snapshot {
            digest: snapshot_digest.to_string(),
            certificate_hash: certificate_hash.to_string(),
            beacon: beacon.clone(),
            ..Snapshot::dummy()
        })
        .unwrap();
        let snapshot_list_json = serde_json::to_string(&vec![
            SnapshotListItem {
                digest: snapshot_digest.to_string(),
                certificate_hash: certificate_hash.to_string(),
                beacon: beacon.clone(),
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
            .compute_snapshot_message(&certificate.clone().try_into().unwrap(), &immutable_db.dir)
            .await
            .expect("Computing snapshot message should not fail")
            .compute_hash();
        let certificate_json = serde_json::to_string(&certificate).unwrap();

        test_http_server(
            warp::path!("artifact" / "snapshots")
                .map(move || snapshot_list_json.clone())
                .or(warp::path!("artifact" / "snapshot" / String)
                    .map(move |_hash| snapshot_json.clone()))
                .or(warp::path!("certificate" / String).map(move |_hash| certificate_json.clone())),
        )
    }
}
