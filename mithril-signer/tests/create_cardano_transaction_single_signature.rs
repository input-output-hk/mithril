mod test_extensions;

use mithril_common::{
    entities::{
        BlockNumber, CardanoDbBeacon, ChainPoint, Epoch,
        SignedEntityType::{
            CardanoImmutableFilesFull, CardanoTransactions, MithrilStakeDistribution,
        },
        SignedEntityTypeDiscriminants, SlotNumber, TimePoint,
    },
    test::{builder::MithrilFixtureBuilder, crypto_helper},
};
use test_extensions::StateMachineTester;

#[rustfmt::skip]
#[tokio::test]
async fn test_create_cardano_transaction_single_signature() {
    let protocol_parameters = crypto_helper::setup_protocol_parameters();
    let fixture = MithrilFixtureBuilder::default()
        .with_signers(10)
        .with_protocol_parameters(protocol_parameters.into())
        .build();
    let signers_with_stake = fixture.signers_with_stake();
    let initial_time_point = TimePoint {
        epoch: Epoch(1),
        immutable_file_number: 1,
        chain_point: ChainPoint {
            slot_number: SlotNumber(10),
            // Note: the starting block number must be greater than the cardano_transactions_signing_config.step
            // so first block range root computation is not on block 0.
            block_number: BlockNumber(100),
            block_hash: "block_hash-100".to_string(),
        },
    };
    let mut tester = StateMachineTester::init(&signers_with_stake, initial_time_point)
        .await
        .expect("state machine tester init should not fail");
    let total_signer_registrations_expected = 3;
    let total_signature_registrations_expected = 4;

    tester
        .comment("state machine starts in Init and transit to Unregistered state.")
        .is_init().await.unwrap()
        .aggregator_allow_signed_entities(&[
            SignedEntityTypeDiscriminants::CardanoTransactions, 
            SignedEntityTypeDiscriminants::CardanoImmutableFilesFull
        ]).await
        .cycle_unregistered().await.unwrap()

        .comment("getting an epoch settings changes the state → RegisteredNotAbleToSign")
        .aggregator_send_epoch_settings().await
        .cycle_registered_not_able_to_sign().await.unwrap()
        .register_signers(&signers_with_stake[..2]).await.unwrap()
        .check_protocol_initializer(Epoch(2)).await.unwrap()
        .check_stake_store(Epoch(2)).await.unwrap()

        .comment("waiting 2 epoch for the registration to be effective")
        .increase_epoch(2).await.unwrap()
        .cycle_unregistered().await.unwrap()
        .cycle_registered_not_able_to_sign().await.unwrap()

        .increase_epoch(3).await.unwrap()
        .cycle_unregistered().await.unwrap()

        .comment("signer can now create a single signature → ReadyToSign")
        .cycle_ready_to_sign_without_signature_registration().await.unwrap()
        
        .comment("signer signs a single signature for MithrilStakeDistribution = ReadyToSign")
        .cycle_ready_to_sign_with_signature_registration(MithrilStakeDistribution(Epoch(3))).await.unwrap()
        
        .comment("signer signs a single signature for CardanoImmutableFilesFull = ReadyToSign")
        .cycle_ready_to_sign_with_signature_registration(CardanoImmutableFilesFull(CardanoDbBeacon::new(3, 1))).await.unwrap()
        
        .comment("signer signs a single signature for CardanoTransactions = ReadyToSign")
        .cycle_ready_to_sign_with_signature_registration(CardanoTransactions(Epoch(3), BlockNumber(89))).await.unwrap()

        .comment("more cycles do not change the state = ReadyToSign")
        .cycle_ready_to_sign_without_signature_registration().await.unwrap()
        .cycle_ready_to_sign_without_signature_registration().await.unwrap()

        .comment("new blocks means a new signature with the same stake distribution → ReadyToSign")
        .increase_block_number_and_slot_number(125, SlotNumber(135), BlockNumber(225)).await.unwrap()
        .cardano_chain_send_rollback(SlotNumber(135), BlockNumber(160)).await.unwrap()
        .cycle_ready_to_sign_with_signature_registration(CardanoTransactions(Epoch(3), BlockNumber(149))).await.unwrap()

        .comment("metrics should be correctly computed")
        .check_metrics(total_signer_registrations_expected,total_signature_registrations_expected).await.unwrap()
        ;
}
