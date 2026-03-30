mod test_extensions;

use mithril_common::{
    current_function,
    entities::{
        BlockNumber, BlockNumberOffset, CardanoBlocksTransactionsSigningConfig, ChainPoint, Epoch,
        SignedEntityType::{CardanoBlocksTransactions, MithrilStakeDistribution},
        SignedEntityTypeDiscriminants, SlotNumber, TimePoint,
    },
    test::{builder::MithrilFixtureBuilder, crypto_helper},
};

use test_extensions::{StateMachineTester, get_test_dir};

#[rustfmt::skip]
#[tokio::test]
async fn test_create_cardano_blocks_transactions_single_signature() {
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
            // Note: the starting block number must be greater than the cardano_blocks_transactions_signing_config.step
            // so first block range root computation is not on block 0.
            block_number: BlockNumber(100),
            block_hash: "block_hash-100".to_string(),
        },
    };
    let mut tester =
        StateMachineTester::init(&get_test_dir(current_function!()), &signers_with_stake, initial_time_point)
            .await.expect("state machine tester init should not fail");
    let total_signer_registrations_expected = 3;
    let total_signature_registrations_expected = 3;

    tester
        .comment("state machine starts in Init and transit to Unregistered state.")
        .is_init().await.unwrap()
        .change_network_configuration_for_aggregation(|conf| {
            conf.signed_entity_types_config.cardano_blocks_transactions = Some(CardanoBlocksTransactionsSigningConfig {
                security_parameter: BlockNumberOffset(0),
                step: BlockNumber(5),
            });
        }).await
        .aggregator_allow_signed_entities(&[
            SignedEntityTypeDiscriminants::CardanoBlocksTransactions,
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

        .comment("signer signs a single signature for CardanoTransactions = ReadyToSign")
        .comment("Signing up to a partial block range - BlockNumber 99 (in range 90..105)")
        .cycle_ready_to_sign_with_signature_registration(CardanoBlocksTransactions(Epoch(3), BlockNumber(99), BlockNumberOffset(0))).await.unwrap()

        .comment("more cycles do not change the state = ReadyToSign")
        .cycle_ready_to_sign_without_signature_registration().await.unwrap()
        .cycle_ready_to_sign_without_signature_registration().await.unwrap()

        .comment("new blocks means a new signature with the same stake distribution → ReadyToSign")
        .increase_block_number_and_slot_number(125, SlotNumber(135), BlockNumber(225)).await.unwrap()
        .cardano_chain_send_rollback(SlotNumber(135), BlockNumber(150)).await.unwrap()
        .comment("Signing up to a complete block range - BlockNumber 149")
        .cycle_ready_to_sign_with_signature_registration(CardanoBlocksTransactions(Epoch(3), BlockNumber(149), BlockNumberOffset(0))).await.unwrap()

        .comment("metrics should be correctly computed")
        .check_metrics(total_signer_registrations_expected, total_signature_registrations_expected).await.unwrap()
        ;
}
