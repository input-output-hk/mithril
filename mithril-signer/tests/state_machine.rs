mod test_extensions;

use mithril_common::entities::Epoch;

use test_extensions::StateMachineTester;

#[tokio::test]
async fn test_create_single_signature() {
    let mut tester = StateMachineTester::init().await;

    tester
        .comment("state machine starts and remains in Unregistered state until a certificate pending is got")
        .cycle_unregistered().await.unwrap()
        .cycle_unregistered().await.unwrap()

        .comment("increasing immutable files does not change the state = Unregistered")
        .increase_immutable(1, 2).await.unwrap()
        .cycle_unregistered().await.unwrap()

        .comment("changing the epoch does not change the state = Unregistered")
        .increase_epoch(2).await.unwrap()
        .cycle_unregistered().await.unwrap()

        .comment("getting a certificate pending changes the state → Registered")
        .register_signers(2).await.unwrap()
        .cycle_registered().await.unwrap()
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
        .cycle_registered().await.unwrap()
        .check_protocol_initializer(Epoch(5)).await.unwrap()

        .comment("signer should be able to create a single signature → Signed")
        .cycle_signed().await.unwrap()
        ;
}
