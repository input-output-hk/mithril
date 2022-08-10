mod init;

use mithril_common::crypto_helper::tests_setup;

#[tokio::test]
async fn create_certificate() {
    // initialization
    let mut tester = init::RuntimeTester::build().await;
    let deps = tester.deps.clone();

    // create signers & declare stake distribution
    let signers = tests_setup::setup_signers(2);
    let signers_with_stake = signers.clone().into_iter().map(|s| s.into()).collect();
    tester.chain_observer.set_signers(signers_with_stake).await;

    // start the runtime state machine
    tester.cycle("signing").await;
    tester.cycle("signing").await;

    // register signers
    tester.register_signers(&signers).await.unwrap();
    tester.cycle("signing").await;

    // change the immutable number to alter the beacon
    tester.increase_immutable_number().await.unwrap();
    tester.cycle("idle").await;
    tester.cycle("signing").await;
    tester.cycle("signing").await;

    // change the EPOCH 2 times to get the first valid stake distribution

    // first EPOCH change
    let _epoch = tester
        .chain_observer
        .next_epoch()
        .await
        .expect("we should get a new epoch");
    tester.cycle("idle").await;
    tester.cycle("signing").await;
    tester.cycle("signing").await;

    // second EPOCH change
    let _epoch = tester
        .chain_observer
        .next_epoch()
        .await
        .expect("we should get a new epoch");
    tester.cycle("idle").await;
    tester.cycle("signing").await;
    tester.cycle("signing").await;

    // signers send their single signature
    tester.send_single_signatures(&signers).await.unwrap();

    // The state machine should issue a multisignature
    tester.cycle("idle").await;
    let last_certificates = deps
        .certificate_store
        .get_list(5)
        .await
        .expect("Querying certificate store should not fail");

    assert_eq!(1, last_certificates.len());
    tester.cycle("idle").await;
}
