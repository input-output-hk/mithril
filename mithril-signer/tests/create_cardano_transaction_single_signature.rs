mod test_extensions;

use mithril_common::{
    crypto_helper::tests_setup,
    entities::{ChainPoint, Epoch, SignedEntityTypeDiscriminants, TimePoint},
    test_utils::MithrilFixtureBuilder,
};

use test_extensions::StateMachineTester;

#[rustfmt::skip]
#[tokio::test]
async fn test_create_cardano_transaction_single_signature() {
    let protocol_parameters = tests_setup::setup_protocol_parameters();
    let fixture = MithrilFixtureBuilder::default()
        .with_signers(10)
        .with_protocol_parameters(protocol_parameters.into())
        .build();
    let signers_with_stake = fixture.signers_with_stake();
    let initial_time_point = TimePoint {
        epoch: Epoch(1),
        immutable_file_number: 1,
        chain_point: ChainPoint {
            slot_number: 1,
            // Note: the starting block number must be greater than the cardano_transactions_signing_config.step
            // so first block range root computation is not on block 0.
            block_number: 100,
            block_hash: "block_hash-100".to_string(),
        },
    };
    let mut tester = StateMachineTester::init(&signers_with_stake, initial_time_point)
        .await
        .expect("state machine tester init should not fail");
    let total_signer_registrations_expected = 3;
    let total_signature_registrations_expected = 2;

    tester
        .comment("state machine starts in Init and transit to Unregistered state.")
        .is_init().await.unwrap()
        .aggregator_send_signed_entity(SignedEntityTypeDiscriminants::CardanoTransactions).await
        .cycle_unregistered().await.unwrap()

        .comment("getting an epoch settings changes the state → Registered")
        .aggregator_send_epoch_settings().await
        .cycle_registered().await.unwrap()
        .register_signers(&signers_with_stake[..2]).await.unwrap()
        .check_protocol_initializer(Epoch(2)).await.unwrap()
        .check_stake_store(Epoch(2)).await.unwrap()

        .comment("waiting 2 epoch for the registration to be effective")
        .increase_epoch(2).await.unwrap()
        .cycle_unregistered().await.unwrap()
        .cycle_registered().await.unwrap()

        .increase_epoch(3).await.unwrap()
        .cycle_unregistered().await.unwrap()

        .comment("creating a new certificate pending with a cardano transaction signed entity → Registered")
        .increase_block_number(70, 170).await.unwrap()
        .cycle_registered().await.unwrap()

        .comment("signer can now create a single signature → Signed")
        .cycle_signed().await.unwrap()

        .comment("more cycles do not change the state = Signed")
        .cycle_signed().await.unwrap()
        .cycle_signed().await.unwrap()

        .comment("new blocks means a new signature with the same stake distribution → Signed")
        .increase_block_number(125, 295).await.unwrap()
        .cardano_chain_send_rollback(230).await.unwrap()
        .cycle_registered().await.unwrap()
        .cycle_signed().await.unwrap()

        .comment("metrics should be correctly computed")
        .check_metrics(total_signer_registrations_expected,total_signature_registrations_expected).await.unwrap()
        ;
}
