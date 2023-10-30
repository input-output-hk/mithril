use mithril_client::client::ClientBuilder;
use mithril_client::message::Message;

#[tokio::test]
async fn mithril_stake_distribution_list_get_show_verify() {
    let genesis_verification_key =
        mithril_common::test_utils::fake_keys::genesis_verification_key()[0];
    let client = ClientBuilder::aggregator("https://example.rs", genesis_verification_key)
        .build()
        .expect("Should be able to create a Client");
    let mithril_stake_distribution_client = client.mithril_stake_distribution();

    let mithril_stake_distributions = mithril_stake_distribution_client
        .list()
        .await
        .expect("List MithrilStakeDistribution should not fail");

    let last_hash = mithril_stake_distributions.last().unwrap().hash.as_ref();

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

    let message = Message::compute_mithril_stake_distribution_message(&mithril_stake_distribution);

    assert!(certificate.match_message(&message));
}
