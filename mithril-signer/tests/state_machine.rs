mod test_extensions;

use mithril_common::{
    crypto_helper::tests_setup, entities::Epoch, test_utils::MithrilFixtureBuilder,
};

use test_extensions::StateMachineTester;

#[rustfmt::skip]
#[tokio::test]
async fn test_create_single_signature() {

    let protocol_parameters = tests_setup::setup_protocol_parameters();
    let fixture = MithrilFixtureBuilder::default().with_signers(10).with_protocol_parameters(protocol_parameters.into()).build();
    let signers_with_stake = fixture.signers_with_stake();
    let mut tester = StateMachineTester::init(&signers_with_stake).await.expect("state machine tester init should not fail");

    tester
        .comment("state machine starts and remains in Unregistered state until a epoch settings is got")
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

        .comment("getting an epoch settings changes the state → Registered")
        .aggregator_send_epoch_settings().await
        .cycle_registered().await.unwrap()
        .register_signers(&signers_with_stake[..2]).await.unwrap()
        .check_protocol_initializer(Epoch(3)).await.unwrap()
        .check_stake_store(Epoch(3)).await.unwrap()

        .comment("more cycles does not change the state = Registered")
        .cycle_registered().await.unwrap()

        .comment("changing immutable does not change the state = Registered")
        .increase_immutable(1, 3).await.unwrap()
        .cycle_registered().await.unwrap()

        .comment("changing Epoch changes the state → Unregistered")
        .increase_epoch(3).await.unwrap()
        .cycle_unregistered().await.unwrap()
        .check_era_checker_last_updated_at(Epoch(3)).await.unwrap()

        .comment("creating a new certificate pending with new signers and new beacon → Registered")
        .cycle_registered().await.unwrap()
        .check_protocol_initializer(Epoch(4)).await.unwrap()
        .check_stake_store(Epoch(4)).await.unwrap()

        .comment("more cycles do not change the state → Registered")
        .cycle_registered().await.unwrap()
        .cycle_registered().await.unwrap()

        .comment("increment immutable, the state does not change = Registered")
        .increase_immutable(5, 8).await.unwrap()
        .cycle_registered().await.unwrap()

        .comment("changing epoch changes the state → Unregistered")
        .increase_epoch(4).await.unwrap()
        .cycle_unregistered().await.unwrap()
        .check_era_checker_last_updated_at(Epoch(4)).await.unwrap()

        .comment("creating a new certificate pending with new signers and new beacon → Registered")
        .cycle_registered().await.unwrap()
        .check_protocol_initializer(Epoch(4)).await.unwrap()

        .comment("signer can now create a single signature → Signed")
        .cycle_signed().await.unwrap()

        .comment("more cycles do not change the state = Signed")
        .cycle_signed().await.unwrap()
        .cycle_signed().await.unwrap()

        .comment("new immutable means a new signature with the same stake distribution → Signed")
        .increase_immutable(1, 9).await.unwrap()
        .cycle_registered().await.unwrap()
        .cycle_signed().await.unwrap()

        .comment("changing epoch changes the state → Unregistered")
        .increase_epoch(5).await.unwrap()
        .cycle_unregistered().await.unwrap()
        .check_era_checker_last_updated_at(Epoch(5)).await.unwrap()
        .cycle_registered().await.unwrap()
        .check_protocol_initializer(Epoch(5)).await.unwrap()

        .comment("signer should be able to create a single signature → Signed")
        .cycle_signed().await.unwrap()
        ;
}
