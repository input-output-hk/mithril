mod test_extensions;

use mithril_common::entities::SignedEntityType::{
    CardanoImmutableFilesFull, CardanoStakeDistribution, MithrilStakeDistribution,
};
use mithril_common::entities::{CardanoDbBeacon, SignedEntityTypeDiscriminants};
use mithril_common::{
    entities::{BlockNumber, ChainPoint, Epoch, SlotNumber, TimePoint},
    test::{builder::MithrilFixtureBuilder, crypto_helper},
};
use test_extensions::StateMachineTester;

#[rustfmt::skip]
#[tokio::test]
async fn test_create_immutable_files_full_single_signature() {
    let protocol_parameters = crypto_helper::setup_protocol_parameters();
    let fixture = MithrilFixtureBuilder::default().with_signers(10).with_protocol_parameters(protocol_parameters.into()).build();
    let signers_with_stake = fixture.signers_with_stake();
    let initial_time_point = TimePoint {
        epoch: Epoch(1),
        immutable_file_number: 1,
        chain_point: ChainPoint {
            slot_number: SlotNumber(100),
            block_number: BlockNumber(100),
            block_hash: "block_hash-100".to_string(),
        },
    };
    let mut tester = StateMachineTester::init(&signers_with_stake, initial_time_point).await.expect("state machine tester init should not fail");
    let total_signer_registrations_expected = 4;
    let total_signature_registrations_expected = 5;

    tester
        .comment("state machine starts in Init and transit to Unregistered state.")
        .is_init().await.unwrap()
        .aggregator_allow_signed_entities(
            &[
                SignedEntityTypeDiscriminants::CardanoImmutableFilesFull,
                SignedEntityTypeDiscriminants::MithrilStakeDistribution,
                SignedEntityTypeDiscriminants::CardanoStakeDistribution,
            ]).await
        .cycle_unregistered().await.unwrap()
        .cycle_unregistered().await.unwrap()
        .check_era_checker_last_updated_at(Epoch(1)).await.unwrap()

        .comment("increasing immutable files does not change the state = Unregistered")
        .increase_immutable(1, 2).await.unwrap()
        .cycle_unregistered().await.unwrap()

        .comment("changing the epoch does not change the state = Unregistered")
        .increase_epoch(2).await.unwrap()
        .cycle_unregistered().await.unwrap()
        .check_era_checker_last_updated_at(Epoch(2)).await.unwrap()

        .comment("getting an epoch settings changes the state → RegisteredNotAbleToSign")
        .aggregator_send_epoch_settings().await
        .cycle_registered_not_able_to_sign().await.unwrap()
        .register_signers(&signers_with_stake[..2]).await.unwrap()
        .check_protocol_initializer(Epoch(3)).await.unwrap()
        .check_stake_store(Epoch(3)).await.unwrap()

        .comment("more cycles does not change the state = RegisteredNotAbleToSign")
        .cycle_registered_not_able_to_sign().await.unwrap()

        .comment("changing immutable does not change the state = RegisteredNotAbleToSign")
        .increase_immutable(1, 3).await.unwrap()
        .cycle_registered_not_able_to_sign().await.unwrap()

        .comment("changing Epoch changes the state → Unregistered")
        .increase_epoch(3).await.unwrap()
        .cycle_unregistered().await.unwrap()
        .check_era_checker_last_updated_at(Epoch(3)).await.unwrap()

        .comment("registration made at the previous epoch will only be available next epoch → RegisteredNotAbleToSign")
        .cycle_registered_not_able_to_sign().await.unwrap()
        .check_protocol_initializer(Epoch(4)).await.unwrap()
        .check_stake_store(Epoch(4)).await.unwrap()

        .comment("more cycles do not change the state → RegisteredNotAbleToSign")
        .cycle_registered_not_able_to_sign().await.unwrap()
        .cycle_registered_not_able_to_sign().await.unwrap()

        .comment("increment immutable, the state does not change = RegisteredNotAbleToSign")
        .increase_immutable(5, 8).await.unwrap()
        .cycle_registered_not_able_to_sign().await.unwrap()

        .comment("changing epoch changes the state → Unregistered")
        .increase_epoch(4).await.unwrap()
        .cycle_unregistered().await.unwrap()
        .check_era_checker_last_updated_at(Epoch(4)).await.unwrap()

        .comment("signer can now create a single signature → ReadyToSign")
        .cycle_ready_to_sign_without_signature_registration().await.unwrap()
        .check_protocol_initializer(Epoch(4)).await.unwrap()
        
        .comment("signer signs a single signature for MithrilStakeDistribution = ReadyToSign")
        .cycle_ready_to_sign_with_signature_registration(MithrilStakeDistribution(Epoch(4))).await.unwrap()

        .comment("signer signs a single signature for CardanoStakeDistribution = ReadyToSign")
        .cycle_ready_to_sign_with_signature_registration(CardanoStakeDistribution(Epoch(3))).await.unwrap()

        .comment("signer signs a single signature for CardanoImmutableFilesFull = ReadyToSign")
        .cycle_ready_to_sign_with_signature_registration(CardanoImmutableFilesFull(CardanoDbBeacon::new(4, 8))).await.unwrap()

        .comment("more cycles do not change the state = ReadyToSign")
        .cycle_ready_to_sign_without_signature_registration().await.unwrap()

        .comment("new immutable means a new signature with the same stake distribution → ReadyToSign")
        .increase_immutable(1, 9).await.unwrap()
        .cycle_ready_to_sign_with_signature_registration(CardanoImmutableFilesFull(CardanoDbBeacon::new(4, 9))).await.unwrap()

        .comment("changing epoch changes the state → Unregistered")
        .increase_epoch(5).await.unwrap()
        .cycle_unregistered().await.unwrap()
        .check_era_checker_last_updated_at(Epoch(5)).await.unwrap()
        .comment("signer should be able to create a single signature → ReadyToSign")

        .check_total_signature_registrations_metrics(4).unwrap()
        .cycle_ready_to_sign_without_signature_registration().await.unwrap()
        .cycle_ready_to_sign_with_signature_registration(MithrilStakeDistribution(Epoch(5))).await.unwrap()
        .check_protocol_initializer(Epoch(5)).await.unwrap()

        .comment("metrics should be correctly computed")
        .check_metrics(total_signer_registrations_expected, total_signature_registrations_expected).await.unwrap()
        ;
}
