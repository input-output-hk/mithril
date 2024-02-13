mod extensions;

use crate::extensions::fake::{FakeAggregator, FakeCertificateVerifier};
use mithril_client::{aggregator_client::AggregatorRequest, ClientBuilder, MessageBuilder};

#[tokio::test]
async fn mithril_stake_distribution_list_get_show_verify() {
    let genesis_verification_key =
        mithril_common::test_utils::fake_keys::genesis_verification_key()[0];
    let msd_hash = "msd_hash";
    let certificate_hash = "certificate_hash";
    let fake_aggregator = FakeAggregator::new();
    let test_http_server =
        fake_aggregator.spawn_with_mithril_stake_distribution(msd_hash, certificate_hash);
    let client = ClientBuilder::aggregator(&test_http_server.url(), genesis_verification_key)
        .with_certificate_verifier(FakeCertificateVerifier::build_that_validate_any_certificate())
        .build()
        .expect("Should be able to create a Client");
    let mithril_stake_distribution_client = client.mithril_stake_distribution();

    let mithril_stake_distributions = mithril_stake_distribution_client
        .list()
        .await
        .expect("List MithrilStakeDistribution should not fail");
    assert_eq!(
        fake_aggregator.get_last_call().await,
        Some(format!(
            "/{}",
            AggregatorRequest::ListMithrilStakeDistributions.route()
        ))
    );

    let last_hash = mithril_stake_distributions.first().unwrap().hash.as_ref();

    let mithril_stake_distribution = mithril_stake_distribution_client
        .get(last_hash)
        .await
        .expect("Get MithrilStakeDistribution should not fail ")
        .unwrap_or_else(|| {
            panic!("A MithrilStakeDistribution should exist for hash '{last_hash}'")
        });
    assert_eq!(
        fake_aggregator.get_last_call().await,
        Some(format!(
            "/{}",
            AggregatorRequest::GetMithrilStakeDistribution {
                hash: (last_hash).to_string()
            }
            .route()
        ))
    );

    let certificate = client
        .certificate()
        .verify_chain(&mithril_stake_distribution.certificate_hash)
        .await
        .expect("Validating the chain should not fail");
    assert_eq!(
        fake_aggregator.get_last_call().await,
        Some(format!(
            "/{}",
            AggregatorRequest::GetCertificate {
                hash: mithril_stake_distribution.certificate_hash.clone()
            }
            .route()
        ))
    );

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
