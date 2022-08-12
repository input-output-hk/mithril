mod init;

use mithril_aggregator::VerificationKeyStorer;
use mithril_common::crypto_helper::tests_setup;
use mithril_common::entities::SignerWithStake;

#[tokio::test]
async fn create_certificate_chain() {
    // initialization
    let mut tester = init::RuntimeTester::build().await;

    // create signers & declare stake distribution
    let signers = tests_setup::setup_signers(5);
    let mut signers_with_stake: Vec<SignerWithStake> =
        signers.clone().into_iter().map(|s| s.into()).collect();
    tester
        .chain_observer
        .set_signers(signers_with_stake.clone())
        .await;

    // start the runtime state machine
    tester.cycle("signing").await;
    tester.register_signers(&signers).await.unwrap();
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
    tester.register_signers(&signers).await.unwrap();

    // second EPOCH change
    let _epoch = tester
        .chain_observer
        .next_epoch()
        .await
        .expect("we should get a new epoch");
    tester.cycle("idle").await;
    tester.cycle("signing").await;
    tester.register_signers(&signers).await.unwrap();

    // signers send their single signature
    tester.send_single_signatures(&signers).await.unwrap();

    // The state machine should issue a multisignature
    tester.cycle("idle").await;
    let last_certificates = tester.get_last_certificates(5).await.unwrap();
    assert_eq!(1, last_certificates.len());
    tester.cycle("idle").await;

    // Increase immutable number to do a second certificate for this epoch
    tester.increase_immutable_number().await.unwrap();
    tester.cycle("signing").await;
    tester.send_single_signatures(&signers).await.unwrap();
    tester.cycle("idle").await;
    let last_certificates = tester.get_last_certificates(5).await.unwrap();
    assert_eq!(2, last_certificates.len());
    assert_eq!(
        (
            last_certificates[0].beacon.immutable_file_number,
            last_certificates[0].beacon.epoch,
        ),
        (
            last_certificates[1].beacon.immutable_file_number + 1,
            last_certificates[1].beacon.epoch,
        ),
        "Only the immutable_file_number should have changed"
    );
    assert_eq!(
        &last_certificates[0].previous_hash, &last_certificates[1].hash,
        "A new certificate on the same epoch should be linked to the first certificate of the current epoch"
    );

    // Change stake distribution
    for (i, signer) in signers_with_stake.iter_mut().enumerate() {
        signer.stake += (i * 1000) as u64;
    }
    let new_signers = tester.update_stake_distribution(signers_with_stake).await;

    // Increase epoch, triggering stake distribution update
    let new_epoch = tester
        .chain_observer
        .next_epoch()
        .await
        .expect("a new epoch should have been issued");
    tester.cycle("signing").await;

    let next_epoch_verification_keys = tester
        .deps
        .verification_key_store
        .get_verification_keys(new_epoch + 1)
        .await
        .expect("get_verification_keys should not fail");
    assert_eq!(
        None, next_epoch_verification_keys,
        "After a new epoch no signers should be registered for the next epoch"
    );
    tester.register_signers(&new_signers).await.unwrap();
    tester.send_single_signatures(&signers).await.unwrap();
    tester.cycle("idle").await;

    let last_certificates = tester.get_last_certificates(5).await.unwrap();
    assert_eq!(3, last_certificates.len());
    assert_eq!(
        (
            last_certificates[0].beacon.immutable_file_number,
            last_certificates[0].beacon.epoch,
        ),
        (
            last_certificates[1].beacon.immutable_file_number,
            last_certificates[1].beacon.epoch + 1,
        ),
        "Only the epoch should have changed"
    );
    assert_eq!(
        &last_certificates[0].previous_hash, &last_certificates[2].hash,
        "The new epoch certificate should be linked to the first certificate of the previous epoch"
    );
    assert_eq!(
        &last_certificates[0].metadata.get_stake_distribution(),
        &last_certificates[2].metadata.get_stake_distribution(),
        "The stake distribution update should only be take in account in the next epoch, stake_distribution: \n{:?}",
        &last_certificates[0].metadata.get_stake_distribution(),
    );

    // Increase epoch & immutable, stake distribution updated previously should be used now
    // todo: why do I need a third epoch to use the new stake distribution ???
    tester
        .chain_observer
        .next_epoch()
        .await
        .expect("a new epoch should have been issued");
    tester.increase_immutable_number().await.unwrap();
    tester.cycle("signing").await;
    tester.send_single_signatures(&signers).await.unwrap();
    tester.cycle("idle").await;

    tester
        .chain_observer
        .next_epoch()
        .await
        .expect("a new epoch should have been issued");
    tester.increase_immutable_number().await.unwrap();
    tester.cycle("signing").await;
    tester.send_single_signatures(&new_signers).await.unwrap();
    tester.cycle("idle").await;
    let last_certificates = tester.get_last_certificates(5).await.unwrap();

    assert_eq!(5, last_certificates.len());
    assert_eq!(
        (
            last_certificates[0].beacon.immutable_file_number,
            last_certificates[0].beacon.epoch,
        ),
        (
            last_certificates[1].beacon.immutable_file_number + 1,
            last_certificates[1].beacon.epoch + 1,
        ),
        "Both the epoch & immutable file number should have changed"
    );
    assert_eq!(
        &last_certificates[0].previous_hash, &last_certificates[1].hash,
        "The new epoch certificate should be linked to the first certificate of the previous epoch"
    );
    assert_ne!(
        &last_certificates[0].metadata.get_stake_distribution(),
        &last_certificates[2].metadata.get_stake_distribution(),
        "The stake distribution update should have been applied for this epoch, stake_distribution: \n{:?}",
        &last_certificates[0].metadata.get_stake_distribution(),
    );
}
