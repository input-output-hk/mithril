mod test_extensions;

use mithril_common::{
    crypto_helper::tests_setup,
    entities::{ChainPoint, Epoch, TimePoint},
    era::{EraMarker, SupportedEra},
    test_utils::MithrilFixtureBuilder,
};

use test_extensions::StateMachineTester;

#[rustfmt::skip]
#[tokio::test]
async fn era_fail_at_startup() {
    let protocol_parameters = tests_setup::setup_protocol_parameters();
    let fixture = MithrilFixtureBuilder::default().with_signers(10).with_protocol_parameters(protocol_parameters.into()).build();
    let signers_with_stake = fixture.signers_with_stake();
    let initial_time_point = TimePoint {
        epoch: Epoch(1),
        immutable_file_number: 1,
        chain_point: ChainPoint {
            slot_number: 1,
            block_number: 100,
            block_hash: "block_hash-100".to_string(),
        },
    };
    let mut tester = StateMachineTester::init(&signers_with_stake, initial_time_point)
        .await.expect("state machine tester init should not fail");
    tester.set_era_markers(vec![EraMarker::new("whatever", Some(Epoch(0)))]);

    tester
        .comment("TEST: state machine fails starting when current Era is not supported.")
        .is_init().await.unwrap()
        .cycle_unregistered().await
        .expect_err("The state machine must fail at startup.");

    tester
        .comment("TEST: state machine fails when transiting to a new unsupported Era.")
        .set_era_markers(vec![
            EraMarker::new(&SupportedEra::dummy().to_string(), Some(Epoch(0))),
            EraMarker::new("unsupported", Some(Epoch(4))),
            ])
        .is_init().await.unwrap()
        .cycle_unregistered().await.unwrap()
        .check_era_checker_last_updated_at(Epoch(1)).await.unwrap()
        .comment("Init Era checking went well, now let's go to usual business…")
        .increase_epoch(2).await.unwrap()
        .cycle_unregistered().await.unwrap()
        .check_era_checker_last_updated_at(Epoch(2)).await.unwrap()
        .aggregator_send_epoch_settings().await
        .cycle_registered().await.unwrap()
        .increase_epoch(3).await.unwrap()
        .cycle_unregistered().await.unwrap()
        .check_era_checker_last_updated_at(Epoch(3)).await.unwrap()
        .aggregator_send_epoch_settings().await
        .cycle_registered().await.unwrap()
        .increase_epoch(4).await.unwrap()
        .comment("Reaching unsupported Era Epoch")
        .cycle_unregistered().await
        .expect_err("The state machine must fail because of unsupported Era.");
}
