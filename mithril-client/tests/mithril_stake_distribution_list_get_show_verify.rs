// TODO: Remove this allow once migration from deprecated AggregatorClient types is complete
#![allow(deprecated)]

mod extensions;

use mithril_client::{
    AggregatorDiscoveryType, ClientBuilder, GenesisVerificationKey, MessageBuilder,
};
use mithril_common::test::double::fake_keys;

use crate::extensions::fake_aggregator::{FakeAggregator, FakeCertificateVerifier};

#[tokio::test]
async fn mithril_stake_distribution_list_get_show_verify() {
    let genesis_verification_key = fake_keys::genesis_verification_key()[0];
    let msd_hash = "msd_hash";
    let certificate_hash = "certificate_hash";
    let fake_aggregator =
        FakeAggregator::spawn_with_mithril_stake_distribution(msd_hash, certificate_hash);
    let client = ClientBuilder::new(AggregatorDiscoveryType::Url(
        fake_aggregator.server_root_url(),
    ))
    .set_genesis_verification_key(GenesisVerificationKey::JsonHex(
        genesis_verification_key.to_string(),
    ))
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
        Some("/artifact/mithril-stake-distributions".to_string())
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
        Some(format!("/artifact/mithril-stake-distribution/{last_hash}",))
    );

    let certificate = client
        .certificate()
        .verify_chain(&mithril_stake_distribution.certificate_hash)
        .await
        .expect("Validating the chain should not fail");
    assert_eq!(
        fake_aggregator.get_last_call().await,
        Some(format!(
            "/certificate/{}",
            mithril_stake_distribution.certificate_hash
        ))
    );

    let message = MessageBuilder::new()
        .compute_mithril_stake_distribution_message(&certificate, &mithril_stake_distribution)
        .expect("Computing msd message should not fail");

    assert!(
        certificate.match_message(&message),
        "Certificate and message did not match:\ncertificate_message: '{}'\n computed_message: '{}'",
        certificate.signed_message,
        message.compute_hash()
    );
}
