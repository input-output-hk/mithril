mod extensions;

use crate::extensions::mock;
use mithril_client::client::ClientBuilder;
use mithril_client::message::MessageBuilder;
use mithril_client::{MithrilStakeDistribution, MithrilStakeDistributionListItem};
use mithril_common::messages::CertificateMessage;
use mithril_common::test_utils::test_http_server::{test_http_server, TestHttpServer};
use std::sync::Arc;
use warp::Filter;

fn spawn_fake_aggregator(msd_hash: &str, certificate_hash: &str) -> TestHttpServer {
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

#[tokio::test]
async fn mithril_stake_distribution_list_get_show_verify() {
    let genesis_verification_key =
        mithril_common::test_utils::fake_keys::genesis_verification_key()[0];
    let msd_hash = "msd_hash";
    let certificate_hash = "certificate_hash";
    let fake_aggregator = spawn_fake_aggregator(msd_hash, certificate_hash);
    let mut mock_verifier = mock::MockCertificateVerifierImpl::new();
    mock_verifier
        .expect_verify_certificate_chain()
        .returning(|_, _| Ok(()))
        .once();
    let client = ClientBuilder::aggregator(&fake_aggregator.url(), genesis_verification_key)
        .with_certificate_verifier(Arc::new(mock_verifier))
        .build()
        .expect("Should be able to create a Client");
    let mithril_stake_distribution_client = client.mithril_stake_distribution();

    let mithril_stake_distributions = mithril_stake_distribution_client
        .list()
        .await
        .expect("List MithrilStakeDistribution should not fail");

    let last_hash = mithril_stake_distributions.first().unwrap().hash.as_ref();

    let mithril_stake_distribution = mithril_stake_distribution_client
        .get(last_hash)
        .await
        .expect("Get MithrilStakeDistribution should not fail ")
        .unwrap_or_else(|| {
            panic!("A MithrilStakeDistribution should exist for hash '{last_hash}'")
        });

    let certificate = client
        .certificate()
        .verify_chain(&mithril_stake_distribution.certificate_hash)
        .await
        .expect("Validating the chain should not fail");

    let message = MessageBuilder::new()
        .compute_mithril_stake_distribution_message(&mithril_stake_distribution)
        .expect("Computing msd message should not fail");

    assert!(
        certificate.match_message(&message),
        "Certificate and message did not match:\ncertificate_message: '{}'\n computed_message: '{}'",
        certificate.signed_message,
        message.compute_hash()
    );
}
