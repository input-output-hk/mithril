mod test_extensions;

use mithril_common::{
    current_function,
    entities::{
        BlockNumber, BlockNumberOffset, CardanoBlocksTransactionsSigningConfig,
        CardanoTransactionsSigningConfig, ChainPoint, Epoch,
        SignedEntityType::MithrilStakeDistribution, SignedEntityTypeDiscriminants, SlotNumber,
        TimePoint,
    },
    test::{
        builder::MithrilFixtureBuilder, double::fake_data,
        entities_extensions::SignedEntityTypeDiscriminantsTestExtension,
    },
};
use mithril_protocol_config::model::{
    MithrilNetworkConfigurationForEpoch, SignedEntityTypeConfiguration,
};

use test_extensions::{StateMachineTester, get_test_dir};

#[rustfmt::skip]
#[tokio::test]
async fn can_cycle_to_ready_to_sign_with_all_signed_entities_enabled() {
    let protocol_parameters = fake_data::protocol_parameters();
    let fixture = MithrilFixtureBuilder::default()
        .with_signers(10)
        .with_protocol_parameters(protocol_parameters.clone())
        .build();
    let signers_with_stake = fixture.signers_with_stake();
    let initial_time_point = TimePoint {
        epoch: Epoch(1),
        immutable_file_number: 1,
        chain_point: ChainPoint {
            slot_number: SlotNumber(1),
            block_number: BlockNumber(100),
            block_hash: "block_hash-100".to_string(),
        },
    };
    let mut tester = StateMachineTester::init(
        &get_test_dir(current_function!()),
        &signers_with_stake,
        initial_time_point,
    )
    .await
    .expect("state machine tester init should not fail");

    tester
        .comment("state machine starts in Init and transit to Unregistered state.")
        .is_init().await.unwrap()
        .set_network_configuration_marker(
            Epoch(0),
            MithrilNetworkConfigurationForEpoch {
                protocol_parameters,
                enabled_signed_entity_types: SignedEntityTypeDiscriminants::all_with_unstable(),
                signed_entity_types_config: SignedEntityTypeConfiguration {
                    cardano_transactions: Some(CardanoTransactionsSigningConfig {
                        security_parameter: BlockNumberOffset(0),
                        step: BlockNumber(30),
                    }),
                    cardano_blocks_transactions: Some(CardanoBlocksTransactionsSigningConfig {
                        security_parameter: BlockNumberOffset(0),
                        step: BlockNumber(30),
                    }),
                },
            },
        ).await

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

        .comment("signer derive the timepoint to all signed entities, including unstable, and is able to\
                  do a single signature for MithrilStakeDistribution = ReadyToSign")
        .cycle_ready_to_sign_with_signature_registration(MithrilStakeDistribution(Epoch(3))).await.unwrap();
}
